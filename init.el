;;; init.el -*- lexical-binding: t; -*-

;; Set up custom.el file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Bootstrap crafted-emacs in init.el
(load "~/git/crafted-emacs/modules/crafted-init-config.el")

(require 'crafted-defaults-config)

;; install crafted-emacs-packages
(require 'crafted-completion-packages)
(require 'crafted-ui-packages)
(require 'crafted-evil-packages)
(setq package-selected-packages
      (remove 'evil-nerd-commenter package-selected-packages))
(require 'crafted-org-packages)
(require 'crafted-ide-packages)
(require 'crafted-writing-packages)
(package-install-selected-packages :noconfirm)

(load "~/.config/emacs/config.el")

;; load crafted-emacs configurations
(require 'crafted-completion-config)
(require 'crafted-ui-config)
(require 'crafted-evil-config)
(require 'crafted-org-config)
(require 'crafted-ide-config)
(require 'crafted-writing-config)
