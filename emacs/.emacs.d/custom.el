;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cape corfu ef-themes haskell-mode magit marginalia mmix-mode
	  nerd-icons-completion nerd-icons-corfu nerd-icons-dired
	  orderless sly typst-ts-mode vertico))
 '(package-vc-selected-packages '((mmix-mode :url "https://github.com/ppareit/mmix-mode")))
 '(tex-dvi-view-command
   '(cond ((eq window-system 'x) "xdvi") ((eq window-system 'w32) "yap")
	  ((eq window-system 'ns) "open") (t "dvi2tty * | cat -s")))
 '(tex-print-file-extension ".pdf")
 '(tex-run-command "xetex"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
