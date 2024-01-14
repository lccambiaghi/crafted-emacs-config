;;; init.el -*- lexical-binding: t; -*-

;; Set up custom.el file
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (and custom-file (file-exists-p custom-file))
;;   (load custom-file nil :nomessage))

;; Bootstrap crafted-emacs in init.el
;; (load "~/git/crafted-emacs/modules/crafted-init-config.el")

;; (require 'crafted-defaults-config)

;; install crafted-emacs-packages
;; (require 'crafted-completion-packages)
;; (require 'crafted-ui-packages)
;; (require 'crafted-evil-packages)
;; (setq package-selected-packages
;;       (remove 'evil-nerd-commenter package-selected-packages))
;; (remove-hook 'package-selected-packages 'aggressive-indent)
;; (require 'crafted-org-packages)
;; (require 'crafted-ide-packages)
;; (require 'crafted-writing-packages)
;; (package-install-selected-packages :noconfirm)

(load "~/.config/emacs/config.el")

;; load crafted-emacs configurations
;; (require 'crafted-completion-config)
;; (require 'crafted-ui-config)
;; (require 'crafted-evil-config)
;; (require 'crafted-org-config)
;; (require 'crafted-ide-config)
;; (require 'crafted-writing-config)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-fold-catch-invisible-edits 'error nil nil "Customized with use-package org")
 '(package-selected-packages
   '(mojo-hl mojo-mode winum dockerfile-mode ox-slack ox-gfm terraform-mode ellama weblorg templatel perspective-tabs project-tab-groups dape))
 '(package-vc-selected-packages
   '((mojo-hl :vc-backend Git :url "https://github.com/andcarnivorous/mojo-hl")
     (mojo-mode :vc-backend Git :url "https://github.com/andcarnivorous/mojo-hl")
     (perspective-tabs :vc-backend Git :url "https://git.sr.ht/~woozong/perspective-tabs")
     (dape :vc-backend Git :url "https://github.com/svaante/dape")))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
	   (lambda nil
	     (org-babel-tangle))
	   nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
