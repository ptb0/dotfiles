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

;(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; speed
(setq inhibit-compacting-font-caches t)


;; byte compiler warnings
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;; locale
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(set-language-environment      "UTF-8")
(prefer-coding-system          'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system   'utf-8)
(set-default-coding-systems    'utf-8)
(set-file-name-coding-system   'utf-8)
(set-keyboard-coding-system    'utf-8)
(set-selection-coding-system   'utf-8)
(set-terminal-coding-system    'utf-8)
(setq locale-coding-system     'utf-8)

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
  (text-mode-ispell-word-completion nil))

(use-package package
  :init
  (package-initialize)
  :config
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(use-package ef-themes
  :ensure t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)
  (modus-themes-load-theme 'ef-dream))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  (dired-mode . hl-line-mode)
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t))

;; (use-package delsel ; delete selected text when typing
;;   :ensure nil
;;   :hook (after-init . delete-selection-mode))


;;;;;;;;;;;;;;;;;;;;;;;
;;; completion & lsp
(use-package eglot
  :ensure nil
  :hook
  (prog-mode . eglot-ensure)
  (typst-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++ c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '((typst-ts-mode) . ,(eglot-alternatives
							    `(typst-ts-lsp-download-path
							      "tinymist"
							      "typst-lsp")))))

(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))

(use-package corfu
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
  :ensure nil
  :hook ((prog-mode . flyspell-prog-mode)
	 (prog-mode . completion-preview-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; programming languages
;; (use-package tex-mode
;;   :ensure nil
;;   :config
;;   (tex-run-command "xetex"))

(use-package nix-mode
  :ensure nil
  :mode "\\.nix\\'")

(use-package sly
  :ensure t
  :custom
  (inferior-lisp-program "sbcl")
  (sly-command-switch-to-existing-lisp 'always))

(use-package haskell-mode :ensure t)

(use-package typst-ts-mode :ensure t)

;;; helm
;; (use-package helm
;;   :straight t
;;   :config
;;   (helm-mode 1))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t))

(use-package magit :ensure t)

(use-package mmix-mode
  :vc (:url "https://github.com/ppareit/mmix-mode"))
