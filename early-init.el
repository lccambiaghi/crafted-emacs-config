;;;; early-init.el --- early initialization  -*- lexical-binding: t; -*-

;;; Commentary:

;; Code to setup `package.el' during `early-init.el'

;;; Code:

;; package archives
;; (load "~/git/crafted-emacs/modules/crafted-early-init-config.el")
(require 'package)
(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defun crafted-package-initialize ()
  "Initialize the package system.
Run this in the `before-init-hook'"
  (package-initialize)
  (require 'seq)
  (when (seq-empty-p package-archive-contents)
    (progn
      (message "crafted-package-config: package archives empty, initalizing")
      (package-refresh-contents))))
(add-hook 'before-init-hook #'crafted-package-initialize)

;;; Garbage collection
;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  ;; NOTE the method for setting the eln-cache directory depends on the emacs version
  (when (fboundp 'startup-redirect-eln-cache)
    (if (version< emacs-version "29")
        (add-to-list 'native-comp-eln-load-path (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))
      (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))))

  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))


;;; UI configuration
;; Remove some unneeded UI elements (the user can turn back on anything they wish)
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mouse-color . "white") default-frame-alist)
(setq ring-bell-function 'ignore)

(provide 'early-init)
;;; early-init.el ends here
