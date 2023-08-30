;;; custom.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Cambiaghi Luca

;; Author: Cambiaghi Luca <cambiaghiluca@CPH-9NV33VTVT>
;; Keywords:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-electric-left-right-brace t)
 '(Man-notify-method 'aggressive t)
 '(TeX-auto-save t)
 '(TeX-electric-math '("$" . "$"))
 '(TeX-electric-sub-and-superscript t)
 '(TeX-parse-self t)
 '(bookmark-save-flag 1)
 '(completion-category-overrides '((file (styles partial-completion))))
 '(completion-cycle-threshold 3)
 '(completion-styles '(orderless basic))
 '(completions-detailed t)
 '(corfu-auto t)
 '(corfu-auto-delay 0.0)
 '(corfu-auto-prefix 2)
 '(corfu-cycle t)
 '(corfu-echo-documentation 0.25 t)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t nil nil "Customized with use-package dired")
 '(ediff-window-setup-function 'ediff-setup-windows-plain t)
 '(eglot-autoshutdown t t)
 '(eshell-scroll-to-bottom-on-input 'this nil nil "Customized with use-package eshell")
 '(evil-respect-visual-line-mode t)
 '(evil-undo-system 'undo-redo)
 '(evil-want-C-h-delete t)
 '(evil-want-C-i-jump nil nil nil "Customized with use-package evil")
 '(evil-want-integration t)
 '(evil-want-keybinding nil)
 '(fast-but-imprecise-scrolling t)
 '(global-auto-revert-non-file-buffers t)
 '(ibuffer-movement-cycle nil)
 '(ibuffer-old-time 24)
 '(kill-do-not-save-duplicates t)
 '(load-prefer-newer t t)
 '(marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil) t)
 '(markdown-enable-html t)
 '(markdown-enable-math t)
 '(mouse-wheel-progressive-speed nil)
 '(org-agenda-files nil)
 '(org-fold-catch-invisible-edits 'error nil nil "Customized with use-package org")
 '(org-hide-emphasis-markers t nil nil "Customized with use-package org")
 '(org-link-descriptive t nil nil "Customized with use-package org")
 '(org-mouse-1-follows-link t)
 '(org-return-follows-link t)
 '(package-archive-priorities
   '(("gnu" . 99)
     ("nongnu" . 80)
     ("stable" . 70)
     ("melpa" . 0)))
 '(package-selected-packages
   '(pandoc-mode markdown-mode ibuffer-project aggressive-indent editorconfig combobulate treesit-auto org-appear denote evil-collection evil tabspaces helpful elisp-demos all-the-icons vertico orderless marginalia embark-consult embark corfu-terminal corfu consult cape))
 '(package-vc-selected-packages
   '((chatgpt-shell :vc-backend Git :url "https://github.com/xenodium/chatgpt-shell")
     (jupyter :vc-backend Git :url "https://github.com/nnicandro/emacs-jupyter")
     (xwwp-full :vc-backend Git :url "https://github.com/kchanqvq/xwwp")
     (org-remoteimg :vc-backend Git :url "https://github.com/gaoDean/org-remoteimg")
     (copilot :vc-backend Git :url "https://github.com/zerolfx/copilot.el")
     (evil-iedit-state :vc-backend Git :url "https://github.com/kassick/evil-iedit-state")
     (evil-org-mode :vc-backend Git :url "https://github.com/hlissner/evil-org-mode")
     (sideline-flymake :vc-backend Git :url "https://github.com/emacs-sideline/sideline-flymake")
     (vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package")))
 '(reftex-plug-into-AUCTeX t)
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle))
           nil t)))
 '(scroll-conservatively 101)
 '(scroll-margin 0)
 '(scroll-preserve-screen-position t)
 '(switch-to-buffer-in-dedicated-window 'pop)
 '(switch-to-buffer-obey-display-actions t)
 '(tabspaces-mode t)
 '(vertico-cycle t)
 '(xref-show-definitions-function 'xref-show-definitions-completing-read))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
