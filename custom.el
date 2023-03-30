;;; custom.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Cambiaghi Luca

;; Author: Cambiaghi Luca <cambiaghiluca@CPH-9NV33VTVT>
;; Keywords:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "CPH-9NV33VTVT")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(evil-want-C-i-jump t nil nil "Customized with use-package evil")
 '(org-fold-catch-invisible-edits 'smart nil nil "Customized with use-package org")
 '(org-hide-emphasis-markers t nil nil "Customized with use-package org")
 '(package-selected-packages
   '(copilot copilot aide weblorg clojure emacs-htmlize emacs-htmlize clojure-mode weblorg templatel hexrgb hexrgb org-html-themify xwwp-full evil-snipe evil-iedit-state chatgpt-shell chatgpt-shell openai codegpt codegpt chatgpt-arcana xwwp-full xwwp-full denote-menu hl-todo consult-notes no-littering denote emacsql-sqlite-module emacsql-sqlite-builtin org-roam all-the-icons-completion xwwp csv-mode highlight-indent-guides persistent-scratch emacs-zmq emacs-zmq jupyter dap-mode flymake-ruff lsp-pyright lsp-ui lsp-mode direnv vc-use-package pet envrc inheritenv org-modern nix-mode hydra diff-hl git-timemachine magit pyvenv vterm-toggle vterm evil-goggles eros numpydoc transpose-frame doom-modeline kind-icon rainbow-delimiters dired-subtree dired-hide-dotfiles all-the-icons-dired dired evil-org-mode evil-org-mode evil-cleverparens evil-surround corfu-terminal evil-collection elisp-demos cape all-the-icons org-appear evil-nerd-commenter embark-consult exec-path-from-shell orderless helpful marginalia vertico keycast))
 '(package-vc-selected-packages
   '((copilot :vc-backend Git :url "https://github.com/zerolfx/copilot.el")
     (chatgpt-shell :vc-backend Git :url "https://github.com/xenodium/chatgpt-shell")))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle))
           nil t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (progn
               (lc/org-add-ids-to-headlines-in-file)
               (lc/tangle-config)))
           nil t)))
 '(tabspaces-include-buffers '("*scratch*") nil nil "Customized with use-package tabspaces")
 '(tabspaces-remove-to-default t nil nil "Customized with use-package tabspaces")
 '(tabspaces-use-filtered-buffers-as-default t nil nil "Customized with use-package tabspaces"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:font "fira code 18"))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(fixed-pitch ((t (:inherit (default)))))
 '(fixed-pitch-serif ((t (:inherit (default)))))
 '(variable-pitch ((t (:font "Sans Serif 18")))))
