;;; lc-osx.el --- osx specific config -*- lexical-binding: t -*-

(use-package exec-path-from-shell
  :ensure t
  :hook
        (emacs-startup . (lambda ()
                     (setq exec-path-from-shell-arguments '("-l")) ; removed the -i for faster startup
                     (exec-path-from-shell-initialize)))
  )

;;; Package:
(provide 'lc-osx)
;;; lc-osx.el ends here
