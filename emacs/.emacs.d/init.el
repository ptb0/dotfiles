(defconst mac-p (eq system-type 'darwin))
(setenv "PATH" (getenv "PATH") ; make sure emacs can access tex
	":/opt/homebrew/bin")

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; put all emacs generated mods in this file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

;; fonts
(let ((mono-spaced-font "Monaco")
      (proportionately-spaced-font "Helvetica"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 120)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; speed
(setq inhibit-compacting-font-caches t)

;; copied from motform .emacs.d
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))    ; pretty
(set-language-environment      "UTF-8")
(prefer-coding-system          'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system   'utf-8)
(set-default-coding-systems    'utf-8)   ; pretty
(set-file-name-coding-system   'utf-8)
(set-keyboard-coding-system    'utf-8)
(set-selection-coding-system   'utf-8)
(set-terminal-coding-system    'utf-8)
(setq locale-coding-system     'utf-8)   ; please

;; setup emacs straight.el and use-package
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Dont show warnings when installing packages
(add-to-list 'display-buffer-alist
	     '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
	       (display-buffer-no-window)
	       (allow-no-window . t)))

;;;;;;;;;;;;;;;;;;;;;;
;;; emacs internals
(use-package emacs
  :ensure nil
  :init
  (setq enable-recursive-minibuffers t)
  (setq backup-by-copying t)
  (setq sentence-end-double-space nil)
  (setq show-trailing-whitespace t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq indent-tabs-mode nil)
  (setq mac-right-option-modifier "none")

  ;; hide commands in m-x which dont work in current mode
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  :config
  (electric-pair-mode 1)
  :custom
  (tab-always-indent 'complete) ; corfu
  (text-mode-ispell-word-completion nil)
  (fill-column 80))

(use-package modus-themes
  :straight t
  :ensure t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'modus-vivendi :no-confirm-loading))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  (dired-mode . hl-line-mode)
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t))

(use-package delsel ; delete selected text when typing
  :ensure nil
  :hook (after-init . delete-selection-mode))


;;;;;;;;;;;;;;;;;
;;; misc tools
(use-package pdf-tools
  :straight t
  :ensure t
  :pin manual
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-width)
  (pdf-tools-install :no-query))


;;;;;;;;;;;;;;;;;;;;;;;
;;; completion & lsp
(use-package eglot
  :ensure nil
  :hook
  (c++-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (c-or-c++-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++ c-mode) "clangd")))

(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))

(use-package corfu
  :straight t
  :ensure t
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :hook ((prog-mode . corfu-mode)
	 (shell-mode . corfu-mode)
	 (eshell-mode .corfu-mode))
  :init
  (global-corfu-mode))

(use-package cape
  :ensure t
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package prog-mode
  :hook ((prog-mode . flyspell-prog-mode)
	 (prog-mode . completion-preview-mode)))

(use-package flyspell
  :custom
  (setq ispell-program-name "aspell"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; programming languages
(use-package tex-mode
  :ensure nil
  :config
  (tex-run-command "xetex"))

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

(use-package sly
  :straight t
  :ensure t
  :custom
  (inferior-lisp-program "sbcl")
  (sly-command-switch-to-existing-lisp 'always))

(use-package haskell-mode
  :straight t)

(use-package tidal
  :straight t
  :config
  (setq tidal-boot-script-path "~/.cabal/share/aarch64-osx-ghc-9.12.2-ea3d/tidal-1.10.1")
  (setq tidal-interpreter "/Users/ptb/.ghcup/bin/ghci"))

;;; helm
;; (use-package helm
;;   :straight t
;;   :config
;;   (helm-mode 1))

(use-package vertico
  :straight t
  :init
  (vertico-mode))
