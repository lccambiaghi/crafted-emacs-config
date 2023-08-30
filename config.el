(use-package emacs
  :init
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-compute-statistics t)
  (setq use-package-expand-minimally t))

(use-package emacs
  :init
  (defvar lc/config-directory "~/.config/emacs/")
  (defvar lc/use-xwidget-browser nil)
  (defvar lc/light-theme 'modus-operandi)
  (defvar lc/dark-theme 'modus-vivendi)
  ;; fix void-variable in some packages e.g. helpful
  (defvar read-symbol-positions-list nil)
  (defvar lc/use-lambda-line nil)
  (defvar lc/use-lambda-theme nil)
  (defvar lc/copilot-enabled nil)
  (defvar lc/beorg-folder "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/beorg/")
  )

(use-package emacs
  :hook
  (org-mode . (lambda ()
                (add-hook 'after-save-hook #'org-babel-tangle-config)))
  :init
  (defun org-babel-tangle-config ()
    (interactive)
    (when (string-equal (buffer-file-name)
                        (expand-file-name "readme.org" lc/config-directory))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle)))))

(use-package helpful
  :config
  (defun helpful--autoloaded-p (sym buf)
    "Return non-nil if function SYM is autoloaded."
    (-when-let (file-name (buffer-file-name buf))
      (setq file-name (s-chop-suffix ".gz" file-name))
      (condition-case nil
          (help-fns--autoloaded-p sym file-name)
                                        ; new in Emacs 29.0.50
                                        ; see https://github.com/Wilfred/helpful/pull/283
        (error (help-fns--autoloaded-p sym))))))

(use-package emacs
  :init
  (fset 'yes-or-no-p 'y-or-n-p))

(use-package emacs
  :init
  (electric-indent-mode -1))

(use-package bookmark
  :ensure nil
  :bind
  ("<leader>rr" . 'bookmark-set)
  ("<leader>rd" . 'bookmark-delete)
  ("<leader>r1" . (lambda () (interactive) (bookmark-set "1")))
  ("<leader>r2" . (lambda () (interactive) (bookmark-set "2")))
  ("<leader>r3" . (lambda () (interactive) (bookmark-set "3")))
  ("<leader>r4" . (lambda () (interactive) (bookmark-set "4")))
  ("s-1" . (lambda () (interactive) (bookmark-jump "1")))
  ("s-2" . (lambda () (interactive) (bookmark-jump "2")))
  ("s-3" . (lambda () (interactive) (bookmark-jump "3")))
  ("s-4" . (lambda () (interactive) (bookmark-jump "4"))))

(use-package emacs
  :bind
  ("<leader>wo" . 'doom/window-enlargen)
  :init
  (defun doom/window-enlargen (&optional arg)
    "Enlargen the current window to focus on this one. Does not close other
windows (unlike `doom/window-maximize-buffer'). Activate again to undo."
    (interactive "P")
    (let ((param 'doom--enlargen-last-wconf))
      (cl-destructuring-bind (window . wconf)
          (or (frame-parameter nil param)
              (cons nil nil))
        (set-frame-parameter
         nil param
         (if (and (equal window (selected-window))
                  (not arg)
                  wconf)
             (ignore
              (let ((source-window (selected-window)))
                (set-window-configuration wconf)
                (when (window-live-p source-window)
                  (select-window source-window))))
           (prog1 (cons (selected-window) (or wconf (current-window-configuration)))
             (let* ((window (selected-window))
                    (dedicated-p (window-dedicated-p window))
                    (preserved-p (window-parameter window 'window-preserved-size))
                    (ignore-window-parameters t)
                    (window-resize-pixelwise nil)
                    (frame-resize-pixelwise nil))
               (unwind-protect
                   (progn
                     (when dedicated-p
                       (set-window-dedicated-p window nil))
                     (when preserved-p
                       (set-window-parameter window 'window-preserved-size nil))
                     (maximize-window window))
                 (set-window-dedicated-p window dedicated-p)
                 (when preserved-p
                   (set-window-parameter window 'window-preserved-size preserved-p))
                 (add-hook 'doom-switch-window-hook #'doom--enlargened-forget-last-wconf-h)))))))))
  )

(use-package emacs
  :bind
  ("<leader>sw" . (lambda () (interactive)
                    (tabspaces-switch-or-create-workspace "web")
                    (lc/open-url "google.com")))
  ("<leader>su" . (lambda () (interactive) (call-interactively 'lc/open-url)))
  :init
  (setq lc/xwidget-webkit-last-session-buffer nil)
  (defun lc/open-url-other-window (url &optional new-session)
    (let ((orig-last-session-buffer (if (boundp 'xwidget-webkit-last-session-buffer)
                                        xwidget-webkit-last-session-buffer
                                      nil)))
      (setq xwidget-webkit-last-session-buffer lc/xwidget-webkit-last-session-buffer)
      (save-window-excursion
        (xwidget-webkit-browse-url url new-session))
      (pop-to-buffer xwidget-webkit-last-session-buffer)
      (setq lc/xwidget-webkit-last-session-buffer xwidget-webkit-last-session-buffer)
      (setq xwidget-webkit-last-session-buffer orig-last-session-buffer)))
  (defun lc/open-url (url &optional other-window new-session)
    (interactive
     (list
      (read-string  "Enter URL or keywords: " nil 'eww-prompt-history "")))
    (if other-window
        (lc/open-url-other-window url new-session)
      (xwidget-webkit-browse-url url new-session))))

(use-package emacs
  :bind
  ("<leader>sc" . 'github-code-search)
  :init
  (defun github-code-search ()
    "Search code on github for a given language."
    (interactive)
    (let* ((language (completing-read
                      "Language: "
                      '("Emacs+Lisp" "Python"  "Clojure" "R")))
           (code
            (thread-last
              (read-string "Code: ") (replace-regexp-in-string " " "+")))
           (url (concat "https://github.com/search?l=" language "&type=code&q=" code)))
      (if lc/use-xwidget-browser
          (lc/open-url-other-window url)
        (browse-url url)))))

(use-package emacs
  :bind
  ("<leader>sg" . 'google-search)
  :init
  (defun google-search-str (str)
    (let* ((keywords (replace-regexp-in-string " " "+" str))
          (url (concat "https://www.google.com/search?q=" keywords)))
      (if lc/use-xwidget-browser
          (lc/open-url-other-window url)
        (browse-url url))))
  (defun google-search ()
    "Google search region, if active, or ask for search string."
    (interactive)
    (if (region-active-p)
        (google-search-str
         (buffer-substring-no-properties (region-beginning) (region-end)))
      (google-search-str (read-from-minibuffer "Search: ")))))

(use-package no-littering
  :init
  (require 'recentf)
  (require 'no-littering)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package emacs
  :init
  (setq display-buffer-alist
        `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*") (0+ not-newline))
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 0.33)
           (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))
  )

(use-package emacs
  :init
  (unless (and (fboundp 'server-running-p) (server-running-p))
    (server-start)))

(use-package emacs
  :init
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))
  (require 'vc-use-package))

(use-package emacs
  :init
  (setq-default fill-column 82)
  )

(use-package emacs
  :init
  (add-hook 'window-setup-hook 'toggle-frame-maximized t))

(use-package emacs
  :if (not lc/use-lambda-theme)
  :init
  (require-theme 'modus-themes)

  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-org-blocks 'greyscale ; {nil,'greyscale,'rainbow}
        ;; modus-themes-variable-pitch-ui t
        ;; modus-themes-mixed-fonts t
        modus-themes-headings (quote ((1 . (variable-pitch 1.4))
                                      (2 . (variable-pitch 1.25))
                                      (3 . (variable-pitch 1.1))
                                      (t . (monochrome))))
        modus-themes-bold-constructs t)

  ;; define some palette overrides
  (defun lc/override-modus-themes-colors ()
    (setq modus-themes-operandi-color-overrides
          '((bg-main . "#fefcf4")
            (bg-dim . "#faf6ef")
            (bg-alt . "#f7efe5")
            (bg-hl-line . "#f4f0e3")
            (bg-active . "#e8dfd1")
            (bg-inactive . "#f6ece5")
            (bg-region . "#c6bab1")
            (bg-header . "#ede3e0")
            (bg-tab-bar . "#dcd3d3")
            (bg-tab-active . "#fdf6eb")
            (bg-tab-inactive . "#c8bab8")
            (fg-unfocused ."#55556f")))
    (setq modus-themes-vivendi-color-overrides
          '((bg-main . "#100b17")
            (bg-dim . "#161129")
            (bg-alt . "#181732")
            (bg-hl-line . "#191628")
            (bg-active . "#282e46")
            (bg-inactive . "#1a1e39")
            (bg-region . "#393a53")
            (bg-header . "#202037")
            (bg-tab-bar . "#262b41")
            (bg-tab-active . "#120f18")
            (bg-tab-inactive . "#3a3a5a")
            (fg-unfocused . "#9a9aab"))))

  (lc/override-modus-themes-colors)
  (setq lc/light-theme 'modus-operandi)
  (setq lc/dark-theme 'modus-vivendi)
  )

(use-package emacs
  :init
  (defcustom lc/default-font-family "fira code" 
    "Default font family"
    :type 'string
    :group 'lc)

  (defcustom lc/variable-pitch-font-family "Sans Serif" ;; "cantarell" ;; 
    "Variable pitch font family"
    :type 'string
    :group 'lc)
  
  (defcustom lc/laptop-font-size 150
    ;; (if lc/is-windows 100 150)
    "Font size used for laptop"
    :type 'int
    :group 'lc)
  
  (defcustom lc/monitor-font-size
    120
    "Font size used for laptop"
    :type 'int
    :group 'lc)

  (defcustom lc/theme nil
    "Current theme (light or dark)"
    :type 'symbol
    :options '(light dark)
    :group 'lc)
  
  (defun lc/get-font-size ()
    "font size is calculated according to the size of the primary screen"
    (let* (;; (command "xrandr | awk '/primary/{print sqrt( ($(nf-2)/10)^2 + ($nf/10)^2 )/2.54}'")
           (command "osascript -e 'tell application \"finder\" to get bounds of window of desktop' | cut -d',' -f3")
           (screen-width (string-to-number (shell-command-to-string command))))  ;;<
      (if (> screen-width 2560) lc/monitor-font-size lc/laptop-font-size))) 
  (defun lc/set-font-size ()
    (interactive)
    ;; Main typeface
    (set-face-attribute 'default nil :family lc/default-font-family :height (lc/get-font-size))
    ;; Set the fixed pitch face (monospace)
    (set-face-attribute 'fixed-pitch nil :family lc/default-font-family)
    ;; Set the variable pitch face
    (set-face-attribute 'variable-pitch nil :family lc/variable-pitch-font-family)
    ;; modeline
    (set-face-attribute 'mode-line nil :family lc/default-font-family :height (lc/get-font-size))
    (set-face-attribute 'mode-line-inactive nil :family lc/default-font-family :height (lc/get-font-size))
    )
  (add-hook 'after-init-hook #'lc/set-font-size)
  )

(use-package all-the-icons
  :config
  (add-to-list 'all-the-icons-extension-icon-alist
               '("eld" all-the-icons-fileicon "elisp" :face all-the-icons-purple))
  (add-to-list 'all-the-icons-extension-icon-alist
               '("edn" all-the-icons-fileicon "elisp" :face all-the-icons-purple))
  (add-to-list 'all-the-icons-extension-icon-alist
               '("lock" all-the-icons-fileicon "nix" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-extension-icon-alist
               '("toml" all-the-icons-alltheicon "python" :face all-the-icons-dblue))
)

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook
  (dired-mode . (lambda () (interactive)
                  (unless (file-remote-p default-directory)
                    (all-the-icons-dired-mode)))))

(use-package dashboard
  :bind
  ("<leader>TAB H" . 'lc/go-to-dashboard)
  :preface
  (defun linum-mode (_) )
  (defun lc/go-to-dashboard ()
    (interactive)
    (kill-matching-buffers dashboard-buffer-name nil 'no-ask)
    (tabspaces-switch-or-create-workspace "Home")
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name))
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items
        '((agenda . 10)
          ;; (bookmarks . 5)
          (projects . 10)
          ;; (recents  . 5)
          ;; (registers . 5)
          ))
  (setq dashboard-set-footer nil)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-projects-backend 'project-el)
  ;; (setq dashboard-match-agenda-entry nil)
  (dashboard-setup-startup-hook))

(use-package doom-modeline
  :if (not lc/use-lambda-line)
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-env-enable-python nil)
  (doom-modeline-height 15)
  (doom-modeline-workspace-name nil)
  ;; (doom-modeline-project-detection 'projectile)
  ;; (doom-modeline-buffer-file-name-style 'relative-to-project)
  )

(use-package lambda-line
  :after (all-the-icons)
  :if lc/use-lambda-line
  :vc (:fetcher "github" :repo "lambda-emacs/lambda-line")
  :custom
  (lambda-line-icon-time t) ;; requires ClockFace font (see below)
  (lambda-line-clockface-update-fontset "ClockFaceRect") ;; set clock icon
  (lambda-line-position 'bottom) ;; Set position of status-line
  (lambda-line-abbrev t) ;; abbreviate major modes
  (lambda-line-hspace "  ")  ;; add some cushion
  (lambda-line-prefix t) ;; use a prefix symbol
  (lambda-line-prefix-padding nil) ;; no extra space for prefix
  (lambda-line-status-invert nil)  ;; no invert colors
  (lambda-line-gui-ro-symbol  " ⨂") ;; symbols
  (lambda-line-gui-mod-symbol " ⬤")
  (lambda-line-gui-rw-symbol  " ◯")
  (lambda-line-space-top +.50)  ;; padding on top and bottom of line
  (lambda-line-space-bottom -.50)
  (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  :init
  ;; activate lambda-line
  (lambda-line-mode)
  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_")))
  (customize-set-variable 'flymake-mode-line-counter-format '("" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter ""))
  (customize-set-variable 'flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters))
  (lambda-line-visual-bell-config)
  )

(use-package hl-todo
  :hook
  ((prog-mode org-mode) . lc/hl-todo-init)
  :preface
  (defun lc/hl-todo-init ()
    (setq-local hl-todo-keyword-faces '(("HOLD" . "#d0bf8f")
                                        ("MAYBE" . "#d0bf8f")
                                        ("TODO" . "#cc9393")
                                        ("NEXT" . "#dca3a3")
                                        ("THEM" . "#dc8cc3")
                                        ("PROG" . "#7cb8bb")
                                        ("KILL" . "#5f7f5f")
                                        ("DONE" . "#afd8af")
                                        ("FIXME" . "#cc9393")))
    (hl-todo-mode)))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  ;; refresh kind icon cache to match theme
  (add-hook 'modus-themes-after-load-theme-hook
            #'(lambda () (interactive) (kind-icon-reset-cache)))
)

(use-package centered-cursor-mode
  :bind
  ("<leader>t=" . (lambda () (interactive) (centered-cursor-mode 'toggle))))

(use-package sideline-flymake
  :vc (:fetcher "github" :repo "emacs-sideline/sideline-flymake")
  :hook (flymake-mode . sideline-mode)
  :init
  ;; (setq sideline-display-backend-name t)
  (setq sideline-flymake-display-mode 'line)
  (setq sideline-backends-right '(sideline-flymake)))

;; (use-package sideline-lsp
;;   :vc (:fetcher "github" :repo "emacs-sideline/sideline-lsp")
;;   :hook (lsp-mode . sideline-mode)
;;   :init
;;   (add-to-list 'sideline-backends-right 'sideline-lsp)
;;   (setq sideline-lsp-update-mode 'line)
;; )

(use-package lambda-themes
  :if (and lc/use-lambda-theme (display-graphic-p))
  :vc (:fetcher "github" :repo "lambda-emacs/lambda-themes")
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t)
  :init
  ;; load preferred theme
  (setq lc/light-theme 'lambda-light)
  (setq lc/dark-theme 'lambda-dark))

(use-package emacs
  :bind
  ("<leader>tt" . 'lc/light-dark-theme-toggle)
  :init
  ;; first turn off the deeper-blue theme
  (disable-theme 'deeper-blue)
  ;; poor man's way of checking the hour when emacs is started
  (if (and (< (string-to-number (format-time-string "%H")) ;; >
              19)
           (not (< (string-to-number (format-time-string "%H")) 6 ;; >
                   )))
      ;; light theme
      (load-theme lc/light-theme :no-confim)
    ;; dark theme
    (load-theme lc/dark-theme :no-confim))

  (defun lc/light-dark-theme-toggle ()
    (interactive)
    (if (eq (car custom-enabled-themes) lc/dark-theme)
        ;; set light-theme
        (progn (disable-theme lc/dark-theme) (load-theme lc/light-theme :no-confirm))
      ;; set dark-theme
      (progn (disable-theme lc/light-theme) (load-theme lc/dark-theme :no-confirm))))
  )

(use-package evil
  :hook
  (edebug-mode . (lambda () (require 'evil-collection-edebug) (evil-normalize-keymaps)))
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump t)
  (evil-lookup-func #'helpful-at-point)
  (evil-want-Y-yank-to-eol t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-auto-indent nil)
  :config
  ;; set leader key in normal state
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'insert (kbd "C-SPC"))
  (evil-set-leader 'visual (kbd "SPC"))
  ;; set local leader
  (evil-set-leader 'normal "," t)
  (evil-set-leader 'insert (kbd "C-,") t)
  (evil-set-leader 'visual "," t)
  ;; ESC key
  (define-key evil-insert-state-map (kbd "ESC") 'evil-normal-state)
  ;; set up motion keys
  (define-key evil-motion-state-map "_" 'evil-end-of-line)
  (define-key evil-motion-state-map "0" 'evil-beginning-of-line)
  (define-key evil-motion-state-map "gD" 'xref-find-references)
  ;; unbind C-p so consult can use it
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil))
  )

(use-package evil-collection
  :hook
  (helpful-mode . (lambda () (evil-collection-init '(dired help))))
  )

(use-package evil
  :config
  (defcustom evil-extra-operator-eval-modes-alist
    '((emacs-lisp-mode eros-eval-region)
      (org-mode eros-eval-region)
      ;; (scheme-mode geiser-eval-region)
      (clojure-mode cider-eval-region)
      ;; (jupyter-repl-interaction-mode jupyter-eval-line-or-region)
      ;; (python-ts-mode jupyter-eval-region)
      (python-ts-mode lc/eval-in-jupyter-repl)
      ;; (python-mode python-shell-send-region) ;; when executing in org-src-edit mode
      )
    "Alist used to determine evil-operator-eval's behaviour.
Each element of this alist should be of this form:
 (MAJOR-MODE EVAL-FUNC [ARGS...])
MAJOR-MODE denotes the major mode of buffer. EVAL-FUNC should be a function
with at least 2 arguments: the region beginning and the region end. ARGS will
be passed to EVAL-FUNC as its rest arguments"
    :type '(alist :key-type symbol)
    :group 'evil-extra-operator)
  (evil-define-operator evil-operator-eval (beg end)
    "Evil operator for evaluating code."
    :move-point nil
    (interactive "<r>")
    (let* (;; (mode (if (and (eq major-mode 'org-mode) (org-in-src-block-p))
           ;;           (intern (car (org-babel-get-src-block-info)))
           ;;         major-mode))
           (mode major-mode)
           (ele (assoc mode evil-extra-operator-eval-modes-alist))
           (f-a (cdr-safe ele))
           (func (car-safe f-a))
           (args (cdr-safe f-a)))
      (unless (fboundp func)
        (message "eval operator function not defined for current major mode"))
      ;; (save-mark-and-excursion (apply func beg end args))
      (apply func beg end args)
      (goto-char end)))

  (define-key evil-motion-state-map "gr" 'evil-operator-eval)
  )

(use-package evil-org-mode
  :vc (:fetcher "github" :repo "hlissner/evil-org-mode")
  :bind
  ([remap evil-org-org-insert-heading-respect-content-below] . +org/insert-item-below) ;; "<C-return>"
  ([remap evil-org-org-insert-todo-heading-respect-content-below] . +org/insert-item-above) ;; "<C-S-return>"
  (:map org-mode-map
        ("RET" . 'org-open-at-point))
  :hook
  (org-mode . lc/init-evil-org-mode)
  :preface
  (defun lc/init-evil-org-mode ()
    (require 'evil-org)
    (evil-normalize-keymaps)
    (evil-org-set-key-theme '(textobjects))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)
    (evil-org-mode)
    ;; disable annoying insert-new-line-and-indent behavior
    ;; (define-key evil-org-mode-map (kbd "<normal-state>o") nil)
    )
  (defun +org--insert-item (direction)
    (let ((context (org-element-lineage
                    (org-element-context)
                    '(table table-row headline inlinetask item plain-list)
                    t)))
      (pcase (org-element-type context)
        ;; Add a new list item (carrying over checkboxes if necessary)
        ((or `item `plain-list)
         ;; Position determines where org-insert-todo-heading and org-insert-item
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (org-end-of-item)
           (backward-char))
         (org-insert-item (org-element-property :checkbox context))
         ;; Handle edge case where current item is empty and bottom of list is
         ;; flush against a new heading.
         (when (and (eq direction 'below)
                    (eq (org-element-property :contents-begin context)
                        (org-element-property :contents-end context)))
           (org-end-of-item)
           (org-end-of-line)))

        ;; Add a new table row
        ((or `table `table-row)
         (pcase direction
           ('below (save-excursion (org-table-insert-row t))
                   (org-table-next-row))
           ('above (save-excursion (org-shiftmetadown))
                   (+org/table-previous-row))))

        ;; Otherwise, add a new heading, carrying over any todo state, if
        ;; necessary.
        (_
         (let ((level (or (org-current-level) 1)))
           ;; I intentionally avoid `org-insert-heading' and the like because they
           ;; impose unpredictable whitespace rules depending on the cursor
           ;; position. It's simpler to express this command's responsibility at a
           ;; lower level than work around all the quirks in org's API.
           (pcase direction
             (`below
              (let (org-insert-heading-respect-content)
                (goto-char (line-end-position))
                (org-end-of-subtree)
                (insert "\n" (make-string level ?*) " ")))
             (`above
              (org-back-to-heading)
              (insert (make-string level ?*) " ")
              (save-excursion (insert "\n"))))
           (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                       (todo-type    (org-element-property :todo-type context)))
             (org-todo
              (cond ((eq todo-type 'done)
                     ;; Doesn't make sense to create more "DONE" headings
                     (car (+org-get-todo-keywords-for todo-keyword)))
                    (todo-keyword)
                    ('todo)))))))

      (when (org-invisible-p)
        (org-show-hidden-entry))
      (when (and (bound-and-true-p evil-local-mode)
                 (not (evil-emacs-state-p)))
        (evil-insert 1))))

  (defun +org/insert-item-below (count)
    "Inserts a new heading, table cell or item below the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'below)))

  (defun +org/insert-item-above (count)
    "Inserts a new heading, table cell or item above the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'above))))

(use-package evil-commentary
  :hook (evil-mode))

(use-package evil-surround
  :init
  (with-eval-after-load 'evil
    (global-evil-surround-mode)))

(use-package evil-goggles
  :after evil
  :custom
  (evil-goggles-duration 0.1)
  :config
  (push '(evil-operator-eval
          :face evil-goggles-yank-face
          :switch evil-goggles-enable-yank
          :advice evil-goggles--generic-async-advice)
        evil-goggles--commands)
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-cleverparens
  :after (evil)
  :hook
  (emacs-lisp-mode . lc/init-cleverparens)
  :init
  (defun lc/init-cleverparens ()
    (require 'evil-cleverparens-util)
    (evil-define-text-object evil-cp-a-defun (count &optional beg end type)
      "An outer text object for a top level sexp (defun)."
      (if (evil-cp--inside-form-p)
          (let ((bounds (evil-cp--top-level-bounds)))
            (evil-range (car bounds) (cdr bounds) 'inclusive :expanded t))
        (error "Not inside a sexp.")))

    (evil-define-text-object evil-cp-inner-defun (count &optional beg end type)
      "An inner text object for a top level sexp (defun)."
      (if (evil-cp--inside-form-p)
          (let ((bounds (evil-cp--top-level-bounds)))
            (evil-range (1+ (car bounds)) (1- (cdr bounds)) 'inclusive :expanded t))
        (error "Not inside a sexp.")))

    (define-key evil-outer-text-objects-map "f" #'evil-cp-a-defun)
    (define-key evil-inner-text-objects-map "f" #'evil-cp-inner-defun)
    ))

(use-package evil
  :config
  (defgroup evil-textobj-entire nil
    "Text object entire buffer for Evil"
    :prefix "evil-textobj-entire-"
    :group 'evil)

  (defcustom evil-textobj-entire-key "g"
    "Key for evil-inner-entire"
    :type 'string
    :group 'evil-textobj-entire)

  (evil-define-text-object evil-entire-entire-buffer (count &optional beg end type)
    "Select entire buffer"
    (evil-range (point-min) (point-max)))

  (define-key evil-outer-text-objects-map evil-textobj-entire-key 'evil-entire-entire-buffer)
  (define-key evil-inner-text-objects-map evil-textobj-entire-key 'evil-entire-entire-buffer)
  )

(use-package evil-iedit-state
  :vc (:fetcher github :repo "kassick/evil-iedit-state")
  :bind
  ("<leader>se" . 'iedit-mode))

(use-package evil-snipe
  :hook
  (evil-local-mode . evil-snipe-local-mode)
  :custom
  (evil-snipe-spillover-scope 'whole-visible)
  :config
  (defun evil-snipe--collect-keys (&optional count forward-p)
    (let ((echo-keystrokes 0) ; don't mess with the prompt, Emacs
          (count (or count 1))
          (i evil-snipe--match-count)
          keys)
      (unless forward-p
        (setq count (- count)))
      (unwind-protect
          (catch 'abort
            (while (> i 0)
              (let* ((prompt (format "%d>%s" i (mapconcat #'char-to-string keys "")))
                     (key (evil-read-key (if evil-snipe-show-prompt prompt))))
                (cond
                 ;; TAB adds more characters if `evil-snipe-tab-increment'
                 ((and evil-snipe-tab-increment (eq key ?\t))  ;; TAB
                  (cl-incf i))
                 ;; Enter starts search with current chars
                 ((memq key '(?\r ?\n))  ;; RET
                  (throw 'abort (if (= i evil-snipe--match-count) 'repeat keys)))
                 ;; Abort
                 ((eq key ?\e)  ;; ESC
                  (evil-snipe--cleanup)
                  (throw 'abort 'abort))
                 (t ; Otherwise, process key
                  (cond ((eq key ?\d)  ; DEL (backspace) deletes a character
                         (cl-incf i)
                         (if (<= (length keys) 1)
                             (progn (evil-snipe--cleanup)
                                    (throw 'abort 'abort))
                           (nbutlast keys)))
                        (t ;; Otherwise add it
                         (setq keys (append keys (list key)))
                         (cl-decf i)))
                  (when evil-snipe-enable-incremental-highlight
                    (evil-snipe--cleanup)
                    (evil-snipe--highlight-all count keys)
                    (add-hook 'pre-command-hook #'evil-snipe--cleanup))))))
            keys))))
  )

(use-package request
:commands request)

(use-package emacs
  :bind
  ("<leader>cb" . 'lc/gpt-complete-buffer-and-insert)
;;  (:map evil-visual-state-map
;;        ("<leader>cr" . 'lc/gpt-complete-region-and-insert)
;;        ("<leader>cp" . 'lc/gpt-complete-with-prompt-prefix-and-insert))
  :init
  (setq lc/gpt-api-key-getter (lambda () (auth-source-pick-first-password :host "chat.openai.com")))
  ;; (setq lc/gpt-model 'gpt-3.5-turbo-0301)
  ;; (setq lc/gpt-model 'gpt-4-0314)
  (setq lc/gpt-model 'gpt-4-0613)
  ;; (setq lc/gpt-model 'gpt-3.5-turbo-16k)
  (setq lc/chat-model t)
  (setq lc/gpt-max-output-tokens 2000)
  (setq lc/gpt-temperature 0.1)
  (setq lc/gpt-top-p 0.1)
  (setq lc/gpt-frequency-penalty 0)
  (setq lc/gpt-presence-penalty 0)
  (setq lc/gpt-prompt-prefix-alist
        '(("describe" . "Describe the following code.")
          ("pytest" . "Write a unit test for the following function using pytest.")
          ("docstring" . "Write a docstring for the following function.")
          ))

  (defun lc/gpt-complete-str (api-key prompt-or-messages)
    "Return the prompt answer from OpenAI API."
    (let* ((result nil)
           (auth-value (format "Bearer %s" api-key))
           (url (if lc/chat-model "https://api.openai.com/v1/chat/completions" "https://api.openai.com/v1/completions"))
           (prompt-key (if lc/chat-model "messages" "prompt")))
      (request
        url
        :type "POST"
        :data (json-encode `((,prompt-key . ,prompt-or-messages)
                             ("model"  . ,lc/gpt-model)
                             ("temperature" . ,lc/gpt-temperature)
                             ;; ("max_tokens" . ,lc/gpt-max-output-tokens)
                             ;; ("frequency_penalty" . ,lc/gpt-frequency-penalty)
                             ;; ("presence_penalty" . ,lc/gpt-presence-penalty)
                             ;; ("top_p" . ,lc/gpt-top-p)
                             ))
        :headers `(("Authorization" . ,auth-value) ("Content-Type" . "application/json"))
        :sync t
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (setq result (if lc/chat-model
                                     (alist-get 'content (alist-get 'message (elt (alist-get 'choices data) 0)))
                                   (alist-get 'text (elt (alist-get 'choices data) 0))))))
        :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                              (message "Got error: %S" error-thrown))))
      result))

  (defun lc/gpt-complete-and-insert (prompt)
    (let* ((result (lc/gpt-complete-str (funcall lc/gpt-api-key-getter) prompt)))
      (goto-char (point-max))
      (if result
          (progn (insert "\n" result) (fill-paragraph))
        (message "Empty result"))))

  (defun lc/gpt-complete-with-prompt-prefix-and-insert (start end)
    (interactive "r")
    (let* ((offset 100)
           (action (completing-read
                    "Select completion action: "
                    (lambda (string predicate action)
                      (if (eq action 'metadata)
                          `(metadata
                            (display-sort-function . ,#'identity)
                            (annotation-function
                             . ,(lambda (cand)
                                  (concat (propertize " " 'display `((space :align-to (- right ,offset))))
                                          (cdr (assoc cand lc/gpt-prompt-prefix-alist))))))
                        (complete-with-action action lc/gpt-prompt-prefix-alist string predicate)))
                    nil t))
           (instruction (cdr (assoc action lc/gpt-prompt-prefix-alist)))
           (text (string-trim (buffer-substring start end)))
           (prompt (concat instruction "\n" text))
           (messages `[(("role"    . "user") ("content" . ,prompt))]))
      (lc/gpt-complete-and-insert (if lc/chat-model messages prompt))))

  (defun lc/gpt-complete-region-and-insert (start end)
    "Send the region to OpenAI and insert the result to the end of buffer. "
    (interactive "r")
    (let* ((prompt (buffer-substring-no-properties start end))
           (messages `[(("role"    . "user") ("content" . ,prompt))]))
      (lc/gpt-complete-and-insert (if lc/chat-model messages prompt))))

  (defun lc/gpt-complete-buffer-and-insert ()
    "Send the ENTIRE buffer, up to max tokens, to OpenAI and insert the result to the end of buffer."
    (interactive)
    (let ((prompt (buffer-substring-no-properties (point-min) (point-max))))
      (lc/gpt-complete-and-insert prompt)))

  :config
  (require 'request))

(use-package emacs
  :bind
  ("<leader>sa" . 'search-with-alpaca)
  :init
  (defcustom alpaca-binary "~/git/alpaca.cpp/chat"
    "Path to the alpaca.cpp compiled binary")

  (defcustom alpaca-model-path "~/git/alpaca.cpp/ggml-alpaca-7b-q4.bin"
    "path to alpaca-models")

  (defcustom alpaca-args '("-t" "8")
    "Arguments to pass to alpaca")

  (defvar swl-current-process-buffer nil "Mini-buffer currently being used to display the process.")

  (defvar swl-query-hist nil)

  (defvar-local swl-process-state nil "State of the swl buffer.")
  (defvar-local swl-process-start-point nil "Point at which query starts.")
  (defvar-local swl-process-cmd nil "Original query command running in the buffer.")

  (defvar swl-result-mode-map
    (let ((map (make-sparse-keymap)))
      (suppress-keymap map)
      (define-key map "q" 'swl-quit)
      (define-key map "r" 'swl-restart-query)
      map))

  (defun swl-make-buffer-command (command &optional full)
    "Return a string that invokes alpaca.cpp with a query COMMAND and model MODEL."
    (format "%s -p \"%s\" --model %s %s"
              alpaca-binary
              command
              alpaca-model-path
              (string-join alpaca-args " ")))

  (defun swl-clear-running-process ()
    (when (and swl-current-process-buffer (buffer-live-p swl-current-process-buffer))
      (let  ((process (get-buffer-process swl-current-process-buffer)))
        (if process (kill-process process)))
      (with-current-buffer swl-current-process-buffer
        (erase-buffer))))

  (defun swl-quit ()
    (interactive)
    (swl-clear-running-process)
    (when swl-current-process-buffer
      (with-current-buffer swl-current-process-buffer
        (setq swl-process-cmd nil)
        (setq swl-process-state nil)))
    (quit-window))

  (defun swl-run-query (cmd)
    (if swl-current-process-buffer
        (swl-clear-running-process))
    (with-current-buffer swl-current-process-buffer
      (setq swl-process-state nil)
      (setq swl-process-cmd cmd)
      (goto-char 0)
      (insert "CMD: ") (insert cmd) (insert "\n")
      (insert "Press r to re-run query and q to quit.\n")
      (goto-char (point-max))
      (setq swl-process-start-point (make-marker))
      (move-marker swl-process-start-point (point))
      (use-local-map swl-result-mode-map))
    (let (proc)
      (setq proc (start-process-shell-command
                  "swl-search-process"
                  swl-current-process-buffer
                  cmd))
      (when (and proc (processp proc))
        (set-process-filter proc #'swl-filter))))

  (defun swl-restart-query ()
    (interactive)
    (swl-clear-running-process)
    (when (and swl-current-process-buffer (buffer-live-p swl-current-process-buffer))
      (with-current-buffer swl-current-process-buffer
        (when swl-process-cmd
          (swl-run-query swl-process-cmd)))))

  (defun swl-filter (process event)
    (when (and swl-current-process-buffer (buffer-live-p swl-current-process-buffer) (process-live-p process))
      (with-current-buffer swl-current-process-buffer
        (save-excursion
          (goto-char (process-mark process))
          (insert event)
          (set-marker (process-mark process) (point))
          (unless swl-process-state
            (goto-char (point-min))
            (setq swl-process-state
                  (search-forward-regexp "sampling parameters:.*\n" nil t))
            (if swl-process-state
                (delete-region (marker-position swl-process-start-point) swl-process-state)))))))

  (defun search-with-alpaca (query &optional model-size model-type)
    (interactive
     (list
      (completing-read "" swl-query-hist nil nil nil 'swl-query-hist)))
    (unless (and swl-current-process-buffer (buffer-live-p swl-current-process-buffer))
      (setq swl-current-process-buffer (get-buffer-create "*swl-process-buffer*")))
    (let (cmd)
      (setq cmd (swl-make-buffer-command query))
      (swl-run-query cmd)
      (display-buffer-at-bottom swl-current-process-buffer '(previous-window))
      (pop-to-buffer swl-current-process-buffer)))
  )

(use-package chatgpt-shell
  :vc (:fetcher "github" :repo "xenodium/chatgpt-shell")
  :bind
  ("s-g" . 'chatgpt-shell)
  ("s-d" . 'dall-e-shell)
  ("<leader>op" . lc/open-prompts-file)
  :custom
  (chatgpt-shell-chatgpt-streaming t)
  (chatgpt-shell-chatgpt-model-version lc/gpt-model)
  (chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "chat.openai.com")))
  :preface
  (defun lc/open-prompts-file ()
    (interactive)
    (unless (member '(chatgpt-shell . t) org-babel-load-languages)
      (require 'ob-chatgpt-shell)
      (require 'ob-dall-e-shell)
      (ob-dall-e-shell-setup)
      (ob-chatgpt-shell-setup))
    (find-file-other-window lc/gpt-prompts-file)))

(use-package copilot
  :vc (:fetcher "github" :repo "zerolfx/copilot.el")
  :hook
  (prog-mode . (lambda () (when lc/copilot-enabled (copilot-mode))))
  :custom
  (copilot-idle-delay 0)
  :bind
  ("<leader>cc" . 'lc/toggle-copilot-mode)
  (:map copilot-completion-map
        ("<right>" . 'copilot-accept-completion)
        ("S-TAB" . 'copilot-accept-completion-by-word)
        ("S-<tab>" . 'copilot-accept-completion-by-word)
        ("C-g" . #'copilot-clear-overlay)
        ("C-n" . #'copilot-next-completion)
        ("C-p" . #'copilot-previous-completion))
  :init
  (defun lc/toggle-copilot-mode ()
    (interactive)
    (setq lc/copilot-enabled (not lc/copilot-enabled))
    (message (if lc/copilot-enabled "Enabled copilot-mode" "Disabled copilot-mode"))
    (copilot-mode (if lc/copilot-enabled 1 -1)))
  )

(use-package magit
  :bind
  (("<leader>gb" . 'magit-blame)
   ("<leader>gg" . 'magit-status)
   ("<leader>gG" . 'magit-status-here)
   ("<leader>gl" . 'magit-log)
   (:map magit-status-mode-map
         ("SPC" . evil-send-leader)
         ("TAB" . 'magit-section-toggle)
         ("ESC" . 'transient-quit-one))
   (:map magit-stash-mode-map
         ("TAB" . 'magit-section-toggle)
         ("ESC" . 'transient-quit-one))
   (:map magit-revision-mode-map
         ("TAB" . 'magit-section-toggle)
         ("ESC" . 'transient-quit-one))
   (:map magit-process-mode-map
         ("TAB" . 'magit-section-toggle)
         ("ESC" . 'transient-quit-one))
   (:map magit-diff-mode-map
         ("TAB" . 'magit-section-toggle)
         ("ESC" . 'transient-quit-one))
   (:map magit-mode-map
         ("<normal-state> zz" . 'evil-scroll-line-to-center)
         ("<visual-state> zz" . 'evil-scroll-line-to-center)))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-log-arguments '("--graph" "--decorate" "--color"))
  (git-commit-fill-column 72)
  :config
  (setq magit-buffer-name-format (concat "*" magit-buffer-name-format "*"))
  ;; adding autostash suffix to magit-pull
  (transient-append-suffix 'magit-pull "-A"
    '("-A" "Autostash" "--autostash")))

(use-package git-timemachine
  :hook
  (git-time-machine-mode . evil-normalize-keymaps)
  :custom
  (git-timemachine-show-minibuffer-details t)
  :bind
  (("<leader>gt" . 'git-timemachine-toggle)
   (:map git-timemachine-mode-map
         ("C-k" . 'git-timemachine-show-previous-revision)
         ("C-j" . 'git-timemachine-show-next-revision)
         ("q" . 'git-timemachine-quit))))

(use-package diff-hl
  :bind
  ("<leader>gn" . 'diff-hl-next-hunk)
  ("<leader>gp" . 'diff-hl-previous-hunk)
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)
   (prog-mode org-mode))
  :custom
  (diff-hl-draw-borders nil)
  ;; (setq diff-hl-global-modes '(not org-mode))
  ;; (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
  ;; (setq diff-hl-global-modes (not '(image-mode org-mode)))
  )

(use-package hydra
  :bind
  ("<leader>gm" . 'smerge-hydra/body)
  :hook
  (magit-diff-visit-file . (lambda () (when smerge-mode (smerge-hydra/body))))
  :config
  (defhydra smerge-hydra
    ;; Disable `smerge-mode' when quitting hydra if no merge conflicts remain.
    (:hint nil :pre (smerge-mode 1) :post (smerge-auto-leave))
    "
                                                    ╭────────┐
  Movement   Keep           Diff              Other │ smerge │
  ╭─────────────────────────────────────────────────┴────────╯
     ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
     ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
     ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
     ^_j_ ↓^     [_a_] all        [_H_] hightlight
     ^_C-j_^     [_RET_] current  [_E_] ediff             ╭──────────
     ^_G_^                                            │ [_q_] quit"
    ("g" (progn (goto-char (point-min)) (smerge-next)))
    ("G" (progn (goto-char (point-max)) (smerge-prev)))
    ("C-j" smerge-next)
    ("C-k" smerge-prev)
    ("j" next-line)
    ("k" previous-line)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("H" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("R" smerge-kill-current)
    ("q" nil :color blue)))

(use-package org
  :ensure nil
  :bind
  (:map org-mode-map
        ("<localleader>a" . 'org-archive-subtree)
        ("<localleader>i" . 'org-insert-structure-template)
        ("<localleader>ll" . 'org-insert-link)
        ("<localleader>ls" . 'org-store-link)
        ("<localleader>L" . (lambda () (interactive) (org-latex-preview)))
        ("<localleader>n" . 'org-toggle-narrow-to-subtree)
        ("<localleader>p" . 'org-priority)
        ("<localleader>r" . 'org-refile)
        ("<localleader>t" . 'org-todo)
        ("<localleader>x" . 'org-toggle-checkbox)
        ("<normal-state><localleader>el" . 'eros-eval-last-sexp)
        ("<visual-state><localleader>e" . 'eros-eval-last-sexp)
        ("<localleader>E" . 'org-export-dispatch)
        ("TAB" . nil)
        )
  :custom
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  ;; (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-catch-invisible-edits 'error)
  (org-link-descriptive nil)
  (org-pretty-entities t)
  (org-ellipsis "…")
  (org-insert-heading-respect-content t)
  (org-image-actual-width nil)
  :preface
  (defun +org-cycle-only-current-subtree-h (&optional arg)
    "Toggle the local fold at the point, and no deeper."
    (interactive "P")
    (unless (eq this-command 'org-shifttab)
      (save-excursion
        (org-beginning-of-line)
        (let (invisible-p)
          (when (and (org-at-heading-p)
                     (or org-cycle-open-archived-trees
                         (not (member org-archive-tag (org-get-tags))))
                     (or (not arg)
                         (setq invisible-p (outline-invisible-p (line-end-position)))))
            (unless invisible-p
              (setq org-cycle-subtree-status 'subtree))
            (org-cycle-internal-local)
            t)))))
  :config
  (with-eval-after-load 'org-appear
    (setq org-appear-autolinks t))
  (plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :scale 1.4)
  ;; Only fold the current tree, rather than recursively
  (add-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)
  )

(use-package org
  :preface
  :config
  (require 'org-id)
  (setq org-id-track-globally nil)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  )

(use-package org
  :bind
  ("<leader>oa" . 'org-agenda-list)
  ("<leader>oA" . 'org-agenda)
  ("<leader>ot" . 'org-todo-list)
  ("<leader>oC" . 'org-capture)
  ("<leader>on" . (lambda () (interactive) (org-agenda nil "n")))
  ("<leader>oi" . (lambda () (interactive) (find-file (concat lc/beorg-folder "inbox.org"))))
  ("<leader>ow" . (lambda () (interactive) (find-file (concat lc/beorg-folder "workflow.org"))))
  :custom
  (org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "HOLD(h)" "DONE(d!/!)"))))
  (org-directory lc/beorg-folder)
  (org-agenda-custom-commands
   '(("n" "Next Tasks"
      ((todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))))
     ("w" "Work Tasks" tags-todo "+work")))
  (org-capture-templates
   `(("i" "Inbox" entry
      (file+headline "inbox.org" "Inbox")
      ,(concat "* %^{Title}\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:\n\n"
               "%i%l"))
     ("w" "Work" entry
      (file+headline "inbox.org" "Work")
      ,(concat "* TODO [#A] %^{Title} :@work:\n"
               "SCHEDULED: %^t\n"
               ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
               "%i%?"))))
  :init
  (when (string-equal system-type "darwin")
    (setq org-agenda-files (mapcar (lambda (f) (concat lc/beorg-folder f)) '("inbox.org" "20230623T103529--birthdays__life.org"))))
  )

(use-package org
  :bind
  (:map org-mode-map
        ("<localleader>." . 'org-edit-special)
        ("<localleader>," . 'org-ctrl-c-ctrl-c)
        ("<localleader>-" . 'org-babel-demarcate-block)
        ("<localleader>z" . 'org-babel-hide-result-toggle))
  (:map org-src-mode-map
        ("<localleader>." . 'org-edit-src-exit))
  :custom
  (org-confirm-babel-evaluate nil)
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-src-window-setup 'current-window)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     ;; (clojure . t)
     (shell . t)))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

;; (use-package ob-async
;;   :hook (org-load . (lambda () (require 'ob-async)))
;;   :custom
;;   (ob-async-no-async-languages-alist '("jupyter-python" "jupyter-R" "jupyter-julia")))

;; add frame borders to show code block "line"
(use-package emacs
  :init
  ;; (modify-all-frames-parameters
  ;;  '((right-divider-width . 10)
  ;;    (internal-border-width . 10)))
  (defun lc/hide-divider-and-fringe ()
    (dolist (face '(window-divider
                    window-divider-first-pixel
                    window-divider-last-pixel))
      (face-spec-reset-face face)
      (set-face-foreground face (face-attribute 'default :background)))
    (set-face-background 'fringe (face-attribute 'default :background)))
  ;; call it once at init time
  (lc/hide-divider-and-fringe)
  ;; call it every time the theme changes
  (advice-add 'lc/light-dark-theme-toggle
              :after (lambda () (interactive) (lc/hide-divider-and-fringe))))

(use-package org-modern
  :custom
  (org-modern-block-fringe 10)
  (org-use-sub-superscripts nil)
  (org-pretty-entities-include-sub-superscripts nil)
  :hook
  (org-mode . org-modern-mode))

(use-package org-fragtog
  :hook
  (org-mode))

(use-package org-remoteimg
  :after (org)
  :hook
  (org-mode . (lambda () (require 'org-remoteimg)))
  :vc (:fetcher "github" :repo "gaoDean/org-remoteimg")
  :init
  (setq org-display-remote-inline-images 'cache)
  )

(use-package org
  :config
  (setq lc/org-view-html-tmp-dir "/tmp/org-html-preview/")

  (use-package f)

  (defun lc/org-view-html ()
    (interactive)
    (let ((elem (org-element-at-point))
          (temp-file-path (concat lc/org-view-html-tmp-dir (number-to-string (random (expt 2 32))) ".html")))
      (cond
       ((not (eq 'export-block (car elem)))
        (message "Not in an export block!"))
       ((not (string-equal (plist-get (car (cdr elem)) :type) "HTML"))
        (message "Export block is not HTML!"))
       (t (progn
            (f-mkdir lc/org-view-html-tmp-dir)
            (f-write (plist-get (car (cdr elem)) :value) 'utf-8 temp-file-path)
            (start-process "org-html-preview" nil "xdg-open" temp-file-path))))))
  )

(use-package consult
  :bind
  ("<leader>bb" . 'consult-buffer)
  ("<leader>fr" . 'consult-recent-file)
  ("<leader>rr" . 'consult-bookmark)
  ("<leader>so" . 'consult-outline)
  ("<leader>ss" . 'consult-line)
  ("<leader>sS" . 'lc/search-symbol-at-point)
  ("<leader>sp" . 'consult-ripgrep)
  ("<leader>sd" . 'lc/consult-ripgrep-at-point)
  ("C-p" . 'consult-yank-pop)
  ("<insert-state>C-p" . 'consult-yank-pop)
  ("M-p" . 'consult-toggle-preview)
  :preface
  (defun lc/search-symbol-at-point ()
    "Performs a search in the current buffer for thing at point."
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (defun lc/consult-ripgrep-at-point (&optional dir initial)
    (interactive
     (list
      (read-directory-name "Directory:")
      (when-let ((s (symbol-at-point)))
        (symbol-name s))))
    (consult-ripgrep dir initial))
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; my/command-wrapping-consult    ;; disable auto previews inside my command
   :preview-key '(:debounce 1 any) ;; Option 1: Delay preview
   ;; :preview-key "M-."            ;; Option 2: Manual preview
   ))

(use-package consult-notes
  :bind
  ("<leader>sn" . 'consult-notes)
  ("<leader>sN" . 'consult-notes-search-in-all-notes)
  :config
  (when (locate-library "denote")
    (consult-notes-denote-mode)))

(use-package corfu
  :hook
  (prog-mode . corfu-mode)
  :bind
  (:map corfu-map
        ("<insert-state><escape>" . 'corfu-quit))
  ("<leader>tc" . (lambda () (interactive) (corfu-mode)))
  :custom
  (corfu-min-width 80)
  ;; Always have the same width
  (corfu-max-width corfu-min-width)
  ;; (corfu-auto nil)
  (tab-always-indent 'complete)
  :config
  ;; (corfu-popupinfo-mode -1)
  (global-corfu-mode -1)
  )

(use-package embark
  :bind
  ("C-l" . 'embark-act)
  ;; (:keymaps 'embark-file-map
  ;;           ;; "o" 'find-file-other-window
  ;;           "x" 'lc/dired-open-externally)
  )

(use-package denote
  :hook
  (dired-mode . denote-dired-mode)
  :bind
  ("<leader>nn" . (lambda () (interactive)
                    (tabspaces-switch-or-create-workspace "denote") (call-interactively 'denote-open-or-create)))
  ("<leader>nk" . 'denote-keywords-add)
  ("<leader>nK" . 'denote-keywords-remove)
  ("<leader>nr" . 'denote-rename-file)
  ("<leader>nl" . 'denote-link)
  ("<leader>nb" . 'denote-link-backlinks)
  ("<leader>nj" . 'lc/denote-journal)
  :custom
  (denote-known-keywords '())
  :preface
  (defun lc/denote-journal ()
    "Create an entry tagged 'journal' with the date as its title."
    (interactive)
    (denote
     (format-time-string "%A %e %B %Y") ; format like Tuesday 14 June 2022
     '("journal"))) ; multiple keywords are a list of strings: '("one" "two")
  :init
  (setq denote-directory "~/OneDrive - The Boston Consulting Group, Inc/Documents/denote")
  (setq lc/gpt-prompts-file (concat denote-directory "/20230330T145824--useful-gpt-prompts__llm_org.org"))
  )

(use-package denote-menu
  :commands
  (list-denotes)
  :bind
  ("<leader>nm" . (lambda () (interactive)
                    (tabspaces-switch-or-create-workspace "denote") (list-denotes)))
  ("<leader>nf" . 'denote-menu-filter-by-keyword)
  ("<leader>nF" . 'denote-menu-clear-filters)
  ("<leader>nE" . 'denote-menu-export-to-dired))

(use-package dired
  :ensure nil
  :bind
  ("<leader>fd" . 'dired)
  ("<leader>fj" . 'dired-jump)
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-listing-switches "-lah")
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t)
  :init
  (defun lc/open-in-finder ()
    (interactive)
    (let ((fn (dired-get-file-for-visit)))
      (start-process "open-directory" nil "open" "-R" fn)))
  (defun lc/dired-open-externally ()
    (interactive)
    (let ((fn (dired-get-file-for-visit)))
      (start-process "open-external" nil "open" fn)))
  :config
  (define-key dired-mode-map (kbd "<normal-state>i") nil)
  (define-key dired-mode-map (kbd "<normal-state>X") nil)
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'dired-mode-map
      "X" 'lc/dired-open-externally
      "F" 'lc/open-in-finder))
  )

(use-package dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :config
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'dired-mode-map
      "H" 'dired-hide-dotfiles-mode)))

(use-package dired-subtree
  :init
  (advice-add 'dired-subtree-toggle
              :after (lambda () (interactive)
                       (when all-the-icons-dired-mode (revert-buffer))))
  :config
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'dired-mode-map
      "i" 'dired-subtree-toggle)))

(use-package eros
  :commands
  (eros-eval-region eros-eval-last-sexp)
  :hook
  (emacs-lisp-mode org-mode lisp-interaction-mode)
  :preface
  (defun eros-eval-region (start end)
    (interactive "r")
    (eros--eval-overlay
     (string-trim
      (with-output-to-string
        (eval-region start end standard-output)))
     (max (point) (mark))))
  )

(use-package jinx
  :hook (org-mode . jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

(use-package flymake
  :ensure nil
  :bind
  ("<leader>!j" . 'flymake-goto-next-error)
  ("<leader>!k" . 'flymake-goto-prev-error)
  :hook
  (python-ts-mode emacs-lisp-mode)
  :custom
  (flymake-fringe-indicator-position 'right-fringe))

(use-package hydra
  :after evil
  :bind
  ("<leader>ww" . 'evil-windows-hydra/body)
  :config
  (defhydra evil-windows-hydra (:hint nil
                                      ;; :pre (smerge-mode 1)
                                      ;; :post (smerge-auto-leave)
                                      )
    "
 [_h_] ⇢⇠ decrease width [_l_] ⇠⇢ increase width
 [_j_] decrease height [_k_] increase height
│ [_q_] quit"
    ("h" evil-window-decrease-width)
    ("l" evil-window-increase-width)
    ("j" evil-window-decrease-height)
    ("k" evil-window-increase-height)
    ("q" nil :color blue)))

(use-package persistent-scratch
  :bind
  ("<leader>bs" . (lambda ()
                    "Load persistent-scratch if not already loaded"
                    (interactive)
                    (progn
                      (unless (boundp 'persistent-scratch-mode)
                        (require 'persistent-scratch))
                      (pop-to-buffer "*scratch*"))))
  :custom
  (persistent-scratch-autosave-interval 60)
  :config
  (persistent-scratch-setup-default))

(use-package project
  :ensure nil
  :bind
  ("<leader>pf" . 'project-find-file)
  :init
  (setq project-vc-extra-root-markers '("pyrpoject.toml" ".project"))
  (setq project-vc-ignores '(".idea" ".vscode" ".direnv"))
  )

(use-package rainbow-delimiters
  :hook
  (emacs-lisp-mode clojure-mode))

(use-package tempel
  :custom
  (tempel-trigger-prefix "<")
  (tempel-path (concat lc/config-directory "/templates.eld"))
  :hook
  ((prog-mode org-mode) . tempel-setup-capf)
  :preface
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  ;; :init
  ;; (tempel-key "C-c t f" fun emacs-lisp-mode-map)
  )

(use-package transpose-frame
  :bind
  ("<leader>wt" . 'transpose-frame)
  ;; flip
  ("<leader>wf" . 'rotate-frame))

(use-package tabspaces
  :commands
  (tabspaces-switch-or-create-workspace)
  :bind
  ("<leader>pp" . 'tabspaces-open-or-create-project-and-workspace)
  ;; add new project to list
  ("<leader>TAB n" . 'tabspaces-project-switch-project-open-file)
  ("<leader>TAB TAB" . 'tabspaces-switch-or-create-workspace)
  ("<leader>TAB d" . 'tabspaces-kill-buffers-close-workspace)
  ("<leader>TAB h" . 'tab-bar-switch-to-prev-tab)
  ("<leader>TAB l" . 'tab-bar-switch-to-next-tab)
  ("<leader>oc" . (lambda () (interactive)
                         (tabspaces-switch-or-create-workspace "config")
                         (find-file (concat lc/config-directory "/readme.org"))))
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  ;; (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  ;; (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  :preface
  (defun lc/setup-tabspaces ()
    "Set up tabspace at startup."
    ;; Add *Messages* and *splash* to Tab \`Home\'
    (tabspaces-mode 1)
    (progn
      (tab-bar-rename-tab "Home")
      (when (get-buffer "*Messages*")
        (set-frame-parameter nil
                             'buffer-list
                             (cons (get-buffer "*Messages*") (frame-parameter nil 'buffer-list))))))
  :config
  (lc/setup-tabspaces)
  ;; Filter Buffers for Consult-Buffer
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(use-package emacs
  :ensure nil
  :custom
  (treesit-font-lock-level 3)
  :init
  (require 'treesit)
  )

(use-package treemacs
  :bind
  ("<leader>tp" . 'treemacs)
  (:map evil-treemacs-state-map
        ("SPC" . evil-send-leader)
        ("<leader>SPC" . execute-extended-command)
        ("<leader>wl" . windmove-right)
        )
  :config
  (use-package treemacs-evil :demand t)
  (use-package treemacs-tab-bar
    :demand t
    :config
    (treemacs-set-scope-type 'Tabs))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t)))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package vertico
  :bind
  (:map vertico-map
        ("C-j" . 'vertico-next)
        ;; ("C-k" . 'vertico-next)
        ;; ("C-j" . 'vertico-previous)
        ("C-k" . 'vertico-previous)
        )
  ;; :config
  ;; (vertico-reverse-mode)
  :custom
  (vertico-resize t)
  )

(use-package vterm
  :bind
  (:map vterm-mode-map
        ("<insert-state> M-l" . 'vterm-send-right)
        ("<insert-state> M-h" . 'vterm-send-left))
  :custom
  ;; (vterm-shell (executable-find "fish"))
  (vterm-shell (executable-find "zsh"))
  (vterm-max-scrollback 10000))

(use-package vterm-toggle
  :bind
  ("<leader>pt" . 'vterm-toggle)
  ("<leader>'" . 'vterm-toggle)
  :custom
  (vterm-toggle-scope 'project))

(use-package xwwp-full
  :vc (:fetcher "github" :repo "kchanqvq/xwwp")
  :bind
  (:map xwidget-webkit-mode-map
        ("<localleader>b" .  'xwidget-webkit-back)
        ("<localleader>j" .  'xwwp-ace-toggle)
        ("<localleader>o" .  'xwwp-section)
        ("<localleader>h" .  'xwwp-history-show))
  :custom
  (xwwp-follow-link-completion-system 'default)
  :config
  (require 'cl))

(use-package lsp-mode
  :commands
  (lsp-deferred)
  :hook
  (lsp-mode . (lambda ()
                (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))
  ;; :general
  ;; (lc/local-leader-keys
  ;;   :states 'normal
  ;;   :keymaps 'lsp-mode-map
  ;;   "i" '(:ignore t :which-key "import")
  ;;   "i o" '(lsp-organize-imports :wk "optimize")
  ;;   "l" '(:keymap lsp-command-map :wk "lsp")
  ;;   "a" '(lsp-execute-code-action :wk "code action")
  ;;   "r" '(lsp-rename :wk "rename"))
  :custom
  (lsp-restart 'ignore)
  (lsp-eldoc-enable-hover nil)
  (lsp-signature-auto-activate nil)
  ;; (lsp-eldoc-render-all nil)
  (lsp-enable-file-watchers nil)
  (lsp-keep-workspace-alive nil)
  (lsp-auto-execute-action nil)
  (lsp-before-save-edits nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-diagnostics-provider :none)
  :config
  (add-to-list 'lsp-language-id-configuration '(python-ts-mode . "python"))
  )

(use-package lsp-ui
  :bind
  ("<localleader>g" . 'lsp-ui-doc-glance)
  :custom
  (lsp-ui-doc-show-with-cursor nil) ;; change this to t for auto doc
  (lsp-ui-doc-delay 2)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-sideline-enable nil)
  ;; (lsp-ui-sideline-show-diagnostics nil)
  ;; (lsp-ui-sideline-show-hover nil)
  )

(use-package clojure-mode
  :mode "\\.clj$"
  :init
  (setq clojure-align-forms-automatically t)
	)

(use-package clojure-mode
  :after (lsp-mode)
  :hook
  ((clojure-mode clojurescript-mode)
   . (lambda ()
       (setq-local lsp-enable-indentation nil ; cider indentation
                   lsp-enable-completion-at-point nil ; cider completion
                   )
       (lsp-deferred)))
  )

(use-package cider
  :hook ((cider-repl-mode . evil-normalize-keymaps)
         (cider-mode . (lambda ()
                         (setq-local evil-lookup-func #'cider-doc)))
         (cider-mode . eldoc-mode))
  :bind
  (:map clojure-mode-map
        ("<localleader>c" . 'cider-connect-clj)
        ("<localleader>j" . 'cider-jack-in)
        ("<localleader>J" . 'cider-jack-in-cljs)
        ("<localleader>dd" . 'cider-debug-defun-at-point)
        ("<localleader>eb" . 'cider-eval-buffer)
        ("<localleader>el" . 'cider-eval-last-sexp)
        ("<localleader>eL" . 'cider-pprint-eval-last-sexp-to-comment)
        ("<localleader>ed" . 'cider-eval-defun-at-point)
        ("<localleader>D" . 'cider-pprint-eval-defun-to-comment)
        ("<localleader>h" . 'cider-clojuredocs-web)
        ("<visual-state><localleader>e" . 'cider-eval-region)
        ;; ("K" . 'cider-doc)
        )
  :init
  (setq nrepl-hide-special-buffers t)
  (setq nrepl-sync-request-timeout nil)
  (setq cider-repl-display-help-banner nil)
  )

(use-package org
  :config
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider)
  )

;; keep the file indented
(use-package aggressive-indent
  :hook (clojure-mode emacs-lisp-mode))

(use-package nix-mode
:mode "\\.nix\\'")

(use-package csv-mode
  :hook (csv-mode . lc/init-csv-mode)
  :bind
  (:map csv-mode-map
        ("<localleader>a" . 'csv-align-fields)
        ("<localleader>A" . 'lc/csv-align-visible)
        ("<localleader>i" . 'lc/init-csv-mode)
        ("<localleader>u" . 'csv-unalign-fields)
        ("<localleader>s" . 'csv-sort-fields)
        ("<localleader>;" . 'lc/set-csv-semicolon-separator)
        ("<localleader>," . 'lc/reset-csv-separators))
  :init
  (defun lc/csv-align-visible (&optional arg)
    "Align visible fields"
    (interactive "P")
    (csv-align-fields nil (window-start) (window-end)))
  (defun lc/set-csv-semicolon-separator ()
    (interactive)
    (customize-set-variable 'csv-separators '(";")))
  (defun lc/reset-csv-separators ()
    (interactive)
    (customize-set-variable 'csv-separators lc/default-csv-separators))
  (defun lc/init-csv-mode ()
    (interactive)
    (lc/set-csv-separators)
    (lc/csv-highlight)
    (call-interactively 'csv-align-fields))
  :config
  (require 'cl)
  (require 'color)
  (defun lc/set-csv-separators ()
    (interactive)
    (let* ((n-commas (count-matches "," (point-at-bol) (point-at-eol)))
           (n-semicolons (count-matches ";" (point-at-bol) (point-at-eol))))
      (if ( ; <
           > n-commas n-semicolons)
          (customize-set-variable 'csv-separators '("," "	"))
        (customize-set-variable 'csv-separators '(";" "	")))))
  (defun lc/csv-highlight ()
    (interactive)
    (font-lock-mode 1)
    (let* ((separator (string-to-char (car csv-separators)))
           (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
           (colors (loop for i from 0 to 1.0 by (/ 2.0 n)
                         collect (apply #'color-rgb-to-hex
                                        (color-hsl-to-rgb i 0.3 0.5)))))
      (cl-loop for i from 2 to n by 2
            for c in colors
            for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
            do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))
  )

(use-package envrc
  :hook
  (python-ts-mode org-jupyter-mode))

(use-package lsp-pyright
  :hook
  (python-ts-mode . (lambda ()
                        (require 'lsp-pyright)
                        (lc/init-pyright)
                        (lsp-deferred)
                        (lsp-diagnostics-mode -1)
                        ))
  :custom
  (lsp-pyright-typechecking-mode "basic")
  (python-indent-guess-indent-offset nil)
  :preface
  (defun lc/init-pyright ()
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection (lambda ()
                                              (cons (lsp-package-path 'pyright)
                                                    lsp-pyright-langserver-command-args)))
      ;; NOTE: only change is here, adding python-ts-mode
      :major-modes '(python-mode python-ts-mode)
      :server-id 'pyright
      :multi-root lsp-pyright-multi-root
      :priority 3
      :initialized-fn (lambda (workspace)
                        (with-lsp-workspace workspace
                          ;; we send empty settings initially, LSP server will ask for the
                          ;; configuration of each workspace folder later separately
                          (lsp--set-configuration
                           (make-hash-table :test 'equal))))
      :download-server-fn (lambda (_client callback error-callback _update?)
                            (lsp-package-ensure 'pyright callback error-callback))
      :notification-handlers (lsp-ht ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
                                     ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
                                     ("pyright/endProgress" 'lsp-pyright--end-progress-callback))))
    ))

(use-package dap-mode
  :hook
  (;; (dap-mode . corfu-mode)
   (dap-terminated . lc/hide-debug-windows)
   ;; (dap-stopped  . lc/switch-to-output-buf)
   ;; (dap-session-created . (lambda (_arg) (projectile-save-project-buffers)))
   (dap-ui-repl-mode . (lambda () (setq-local truncate-lines t))))
  :bind
  (:map lsp-mode-map
        ("<localleader>dd" . 'dap-debug)
        ("<localleader>db" . 'dap-breakpoint-toggle)
        ;; "d B" '(dap-ui-breakpoints-list :wk "breakpoint list")
        ("<localleader>dc" . 'dap-continue)
        ("<localleader>dn" . 'dap-next)
        ("<localleader>de" . 'dap-eval-thing-at-point)
        ("<localleader>di" . 'dap-step-in)
        ("<localleader>dl" . 'dap-debug-last)
        ("<localleader>dq" . 'dap-disconnect)
        ("<localleader>dr" . 'dap-ui-repl)
        ("<localleader>di" . 'lc/dap-inspect-df)
        ;; templates
        ("<localleader>dt" . (lambda () (interactive) (dap-debug dap-test-args)))
        ("<localleader>ds" . (lambda () (interactive) (dap-debug dap-script-args))))
  (:map dap-ui-repl-mode-map
        ("<insert-state><up>" . 'comint-previous-input))
  :preface
  (defun lc/dap-inspect-df (dataframe)
    "Save the df to csv and open the file with csv-mode"
    (interactive (list (read-from-minibuffer "DataFrame: " (evil-find-symbol nil))))
    (dap-eval (format  "%s.to_csv('%s', index=False)" dataframe lc/dap-temp-dataframe-path))
    (sleep-for 1)
    (find-file-other-window lc/dap-temp-dataframe-path))
  ;; hide stdout window  when done
  (defun lc/hide-debug-windows (session)
    "Hide debug windows when all debug sessions are dead."
    (unless (-filter 'dap--session-running (dap--get-sessions))
      (lc/kill-output-buffer)))
  (defun lc/dap-python--executable-find (orig-fun &rest args)
    (executable-find "python"))
  (defun lc/kill-output-buffer ()
    "Go to output buffer."
    (interactive)
    (let ((win (display-buffer-in-side-window
                (dap--debug-session-output-buffer (dap--cur-session-or-die))
                `((side . bottom) (slot . 5) (window-width . 0.20)))))
      (delete-window win)))
  (defun lc/window-resize-to-percentage (percentage)
    (interactive)
    (window-resize nil (- (truncate (* percentage (frame-height))) (window-height))))
  (defun lc/reset-dap-windows ()
    (interactive)
    ;; display sessions and repl
    (seq-doseq (feature-start-stop dap-auto-configure-features)
      (when-let
          (start-stop (alist-get feature-start-stop
                                 ;; <
                                 dap-features->windows))
        (funcall (car start-stop))))
    ;; display output buffer
    (save-excursion (dap-go-to-output-buffer t))
    ;; resize window
    (save-window-excursion
      ;; switch to main window
      (winum-select-window-1)
      (lc/window-resize-to-percentage 0.66)))
  (defun lc/switch-to-output-buf ()
    (when-let* ((splits (split-string buffer-file-name "/"))
                (bla (car (last splits))))
      (when (string-equal bla "pytest.py")
        (switch-to-buffer (dap--debug-session-output-buffer (dap--cur-session-or-die))))))
  :init
  (setq lc/dap-temp-dataframe-buffer  "*inspect-df*")
  (setq lc/dap-temp-dataframe-path "~/tmp-inspect-df.csv")
  ;; prevent minibuffer prompt about reloading from disk
  (setq revert-without-query '("~/tmp-inspect-df.csv"))
  ;; (setq dap-auto-configure-features '(locals repl))
  (setq dap-auto-configure-features '(sessions repl))
  (setq dap-python-debugger 'debugpy)
  ;; show stdout
  (setq dap-auto-show-output t)
  (setq dap-output-window-min-height 10)
  (setq dap-output-window-max-height 200)
  (setq dap-overlays-use-overlays nil)
  :config
  ;; configure windows
  (require 'dap-ui)
  (setq dap-ui-buffer-configurations
        '(("*dap-ui-sessions*"
           (side . bottom) (slot . 1) (window-height . 0.33))
          ("*debug-window*"
           (side . bottom) (slot . 2) (window-height . 0.33))
          ("*dap-ui-repl*"
           (side . bottom) (slot . 3) (window-height . 0.33))))
  (dap-ui-mode 1)
  ;; python virtualenv
  (require 'dap-python)
  (advice-add 'dap-python--pyenv-executable-find :around #'lc/dap-python--executable-find)
  ;; debug templates
  (defvar dap-script-args (list :type "python"
                                :args []
                                :cwd "${workspaceFolder}"
                                :justMyCode :json-false
                                :request "launch"
                                :debugger 'debugpy
                                :name "dap-debug-script"))
  (defvar dap-test-args (list :type "python-test-at-point"
                              :args ""
                              :justMyCode :json-false
                              ;; :cwd "${workspaceFolder}"
                              :request "launch"
                              :module "pytest"
                              :debugger 'debugpy
                              :name "dap-debug-test-at-point"))
  (defvar eco-cold-start (list
                          :name "mill"
                          :type "python"
                          :request "launch"
                          :program (expand-file-name "~/git/ran_optimization/scripts_smart_sleep_orchestration/find_cold_start_smart_sleep_thresholds.py")
                          ;; :env '(("NO_JSON_LOG" . "true"))
                          ;; :args ["-m" "mill" "--config" "user_luca"]
                          ))

  (dap-register-debug-template "dap-debug-script" dap-script-args)
  (dap-register-debug-template "dap-debug-test-at-point" dap-test-args)
  (dap-register-debug-template "eco-cold-start" eco-cold-start)
  )

(use-package flymake-ruff
  :hook
  (python-ts-mode . flymake-ruff-load))

(use-package emacs
  :init
  ;; (add-to-list 'native-comp-bootstrap-deny-list ".*jupyter.*")
  ;; (add-to-list 'native-comp-jit-compilation-deny-list ".*jupyter.*")
)

(use-package jupyter
  :vc (:fetcher "github" :repo "nnicandro/emacs-jupyter")
  :bind
  (:map python-ts-mode-map
        ("<localleader>ee" . 'jupyter-eval-line-or-region)
        ;; ("<visual-state><localleader>e" . 'jupyter-eval-line-or-region)
        ("<localleader>ed" . 'jupyter-eval-defun)
        ("<localleader>eb" . 'jupyter-eval-buffer)
        ;; ("<localleader>eb" . (lambda () (interactive) (lc/jupyter-eval-buffer)))
        ("<localleader>er" . 'jupyter-eval-remove-overlays)
        ("<localleader>kd" . 'lc/kill-repl-kernel)
        ("<localleader>ki" . 'jupyter-org-interrupt-kernel)
        ("<localleader>kr" . 'jupyter-repl-restart-kernel)
        ("<localleader>J" . 'lc/jupyter-repl)
        )
  (:map org-mode-map
        ("<localleader>=" . (lambda () (interactive) (jupyter-org-insert-src-block t nil)))
        ("<localleader>m" . 'jupyter-org-merge-blocks)
        ("<localleader>+" . 'jupyter-org-insert-src-block)
        ("<localleader>?" . 'jupyter-inspect-at-point)
        ("<localleader>c" . 'jupyter-org-clear-all-results)
        ("<localleader>x" . 'jupyter-org-kill-block-and-results))
  :hook
  (jupyter-repl-persistent-mode .   (lambda () (setq-local evil-lookup-func #'jupyter-inspect-at-point)))
  (jupyter-repl-interaction-mode .   (lambda () (setq-local evil-lookup-func #'jupyter-inspect-at-point)))
  (jupyter-repl-persistent-mode . (lambda ()  ;; we activate org-interaction-mode ourselves
                                    (when (derived-mode-p 'org-mode) (jupyter-org-interaction-mode))))
  :custom
  (jupyter-repl-prompt-margin-width 4)
  (jupyter-eval-use-overlays nil)
  :preface
  (defun jupyter-command-venv (&rest args)
    "This overrides jupyter-command to use the virtualenv's jupyter"
    (let ((jupyter-executable (executable-find "jupyter")))
      (with-temp-buffer
        (when (zerop (apply #'process-file jupyter-executable nil t nil args))
          (string-trim-right (buffer-string))))))
  (defun lc/jupyter-eval-buffer ()
    "Send the contents of BUFFER using `jupyter-current-client'."
    (interactive)
    (jupyter-eval-string (jupyter-load-file-code (buffer-file-name))))
  (defun lc/jupyter-repl ()
    "If a buffer is already associated with a jupyter buffer, then pop to it. Otherwise start a jupyter kernel."
    (interactive)
    (if (bound-and-true-p jupyter-current-client)
        (jupyter-repl-pop-to-buffer)
      (call-interactively 'jupyter-repl-associate-buffer)))
  (defun lc/kill-repl-kernel ()
    "Kill repl buffer associated with current jupyter kernel"
    (interactive)
    (if jupyter-current-client
        (jupyter-with-repl-buffer jupyter-current-client
          (kill-buffer (current-buffer)))
      (error "Buffer not associated with a REPL, see `jupyter-repl-associate-buffer'")))
  (defun lc/jupyter-toggle-overlays ()
    (interactive)
    (if jupyter-eval-use-overlays
        (setq jupyter-eval-use-overlays nil)
      (setq jupyter-eval-use-overlays t)))
  (defun lc/jupyter-inspect-df (dataframe)
    "Save the df to csv and open the file with csv-mode"
    (interactive (list (read-from-minibuffer "DataFrame: " (evil-find-symbol nil))))
    ;; (jupyter-eval (format
    ;;                "%s.reset_index().assign(index='').rename(columns={'index': ''}).to_csv('%s', index=False, sep='|')"
    ;;                dataframe lc/jupyter-temp-dataframe-path))
    ;; (org-table-align)
    (jupyter-eval (format  "%s.to_csv('%s', index=False)" dataframe lc/jupyter-temp-dataframe-path))
    (find-file-other-window lc/jupyter-temp-dataframe-path))
  (defun lc/eval-in-jupyter-repl (start end)
    "Send region to Jupyter REPL and return to original buffer."
    (interactive "P")
    (let* ((current-buffer (current-buffer))
           (region (when (use-region-p)
                     (car (region-bounds))))
           (start (car region))
           (end (cdr region))
           (content (buffer-substring
                     (or start (line-beginning-position))
                     (or end (line-end-position)))))
      (jupyter-with-repl-buffer jupyter-current-client
        (goto-char (point-max))
        (insert content)
        (jupyter-repl-ret))))
  :init
  ;; TODO refactor to avoid duplication of dap code
  (setq lc/jupyter-temp-dataframe-buffer  "*inspect-df*")
  (setq lc/jupyter-temp-dataframe-path "~/tmp-inspect-df.csv")
  (advice-add 'jupyter-command :override #'jupyter-command-venv)
  ;; stop spawning output buffer
  (add-to-list 'display-buffer-alist
               '("\\*jupyter-output\\*\\|\\*jupyter-error\\*"
                 (cons 'display-buffer-no-window
                       '((allow-no-window . t)))))
  (add-to-list
   'display-buffer-alist
   '("^\\*jupyter-repl" ; *
     (display-buffer-below-selected)
     (window-width . 0.33)))
  )

(use-package emacs
  :hook
  ((org-jupyter-mode . (lambda ()
                         ;; otherwise jupyter-associate-buffer will fail
                         (setq major-mode-remap-alist '())
                         (setq-local evil-lookup-func #'jupyter-inspect-at-point)
                         ;; disable annoying insert-new-line-and-indent behavior
                         (setq-local indent-line-function 'lc/no-indent)
                         ))
   ;; (org-jupyter-mode . 'org-redisplay-inline-images)
   (org-mode . (lambda () (when (lc/is-jupyter-org-buffer?) (org-jupyter-mode)))))
  :bind
  (:map org-jupyter-mode-map
        ("<localleader>kd" . 'lc/kill-repl-kernel)
        ("<localleader>ki" . 'jupyter-org-interrupt-kernel)
        ("<localleader>kr" . 'jupyter-repl-restart-kernel)
        )
  :init
  (defun lc/no-indent () 'noindent)
  (defun lc/is-jupyter-org-buffer? ()
    (with-current-buffer (buffer-name)
      (goto-char (point-min))
      (re-search-forward "begin_src jupyter-" 10000 t)))
  (defun lc/org-cycle-or-py-complete (orig-fun &rest args)
    "If in a jupyter-python code block, call py-indent-or-complete, otherwise use org-cycle"
    (if (and (org-in-src-block-p)
             (eq (intern (org-element-property :language (org-element-at-point))) 'jupyter-python))
        (lc/py-indent-or-complete)
      (apply orig-fun args)))
  (defun lc/py-indent-or-complete ()
    (interactive "*")
    (window-configuration-to-register py--windows-config-register)
    (cond ((use-region-p)
           (py-indent-region (region-beginning) (region-end)))
          ((or (bolp)
               (member (char-before) (list 9 10 12 13 32 ?:  ;; {[(
                                           ?\) ?\] ?\}))
               ;; (not (looking-at "[ \t]*$"))
               )
           (py-indent-line))
          ((comint-check-proc (current-buffer))
           (ignore-errors (completion-at-point)))
          (t
           (completion-at-point))))
  (define-minor-mode org-jupyter-mode
    "Minor mode which is active when an org file has the string begin_src jupyter-python
    in the first few hundred rows"
    :keymap (let ((map (make-sparse-keymap)))
              map)))

(use-package scimax-jupyter
  :load-path "~/git/scimax"
  :bind
  (:map org-jupyter-mode-map
        ("<localleader>?" . 'scimax-jupyter-org-hydra/body)
        ("<localleader>K" . (lambda () (interactive)
                              (scimax-jupyter-org-kill-kernel)
                              (setq header-line-format nil)
                              (message "killer jupyter kernel"))))
  :hook
  (envrc-mode . lc/init-jupyter)
  :preface
  (defun lc/init-jupyter ()
    (interactive)
    ;; only try to load in org-mode
    (unless (member '(jupyter . t) org-babel-load-languages)
        ;; only load if jupyter is available
        (if (executable-find "jupyter")
            (let ((warning-minimum-level :error))
              (require 'scimax-jupyter))
          (message "could not initialize scimax-jupyter!"))))
  :init
  ;; (cl-defmethod jupyter-org--insert-result (_req context result)
  ;;   (let ((str
  ;;          (org-element-interpret-data
  ;;           (jupyter-org--wrap-result-maybe
  ;;            context (if (jupyter-org--stream-result-p result)
  ;;                        (thread-last result
  ;;                                     jupyter-org-strip-last-newline
  ;;                                     jupyter-org-scalar)
  ;;                      result)))))
  ;;     (if (< (length str) 100000)  ;; >
  ;;         (insert str)
  ;;       (insert (format ": Result was too long! Length was %d" (length str)))))
  ;;   (when (/= (point) (line-beginning-position))
  ;;     (insert "\n")))
  (defun scimax-jupyter-get-session ()
    "Get the session name in the current buffer."
    (let ((lang (car (org-babel-get-src-block-info))))
      (or
       (cdr
        (assoc :session
               ;; NOTE: replaced 'cadr' with 'car' here
               (car (org-babel-params-from-properties lang))))
       (cdr (assoc :session
                   org-babel-default-header-args:jupyter-python)))))

  )

(use-package stan-mode
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  :config
  ;; The officially recommended offset is 2.
  (setq stan-indentation-offset 2))

(use-package rjsx-mode
  :hook (rjsx-mode . rainbow-delimiters-mode)
  )

(use-package typescript-mode
  :mode "\.ts\'"
  :hook (typescript-mode . rainbow-delimiters-mode)
  :hook (typescript-mode . lsp-deferred)
  ;; :hook (typescript-tsx-mode . rainbow-delimiters-mode)
  :hook (typescript-mode . (lambda () (lsp-deferred) (lsp-diagnostics-mode -1)))
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TypeScript[tsx]")
  (add-to-list 'auto-mode-alist '("\.tsx\'" . typescript-tsx-mode))
  (add-hook 'typescript-tsx-mode-hook
            (lambda () (tree-sitter-mode) (tree-sitter-hl-mode)))
  :config (setq typescript-indent-level 2)
)

(use-package flymake-eslint
  :hook
  (typescript-mode . (lambda ()
                       (flymake-eslint-enable)
                       (setq-local flymake-eslint-project-root (locate-dominating-file buffer-file-name "tsconfig.json"))))
  )

(use-package evil
  :config
  (evil-define-key 'normal 'global
    (kbd "<leader>SPC") 'execute-extended-command
    (kbd "<leader>R") 'restart-emacs
    (kbd "<leader>;") 'eval-expression
    ;; previous buffer
    (kbd "<leader>`") '(lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) 1)))
    (kbd "<leader>bd")  'kill-current-buffer
    (kbd "<leader>br")  'revert-buffer
    ;; delete file
    (kbd "<leader>fD")  '(lambda () (interactive) (delete-file (buffer-file-name)))
    (kbd "<leader>ff")  'find-file
    (kbd "<leader>fR")  'rename-visited-file
    (kbd "<leader>fs")  'save-buffer
    (kbd "<leader>he")  'view-echo-area-messages
    (kbd "<leader>hf")  'describe-function
    (kbd "<leader>hk")  'describe-key
    (kbd "<leader>hK")  'describe-keymap
    (kbd "<leader>hl")  'view-lossage
    (kbd "<leader>hL")  'find-library
    (kbd "<leader>hp")  'describe-package
    (kbd "<leader>hv")  'describe-variable
    ;; lisp
    (kbd "<leader>lr")  'raise-sexp
    (kbd "<leader>lb")  'sp-forward-barf-sexp
    (kbd "<leader>ls")  'sp-forward-slurp-sexp
    (kbd "<leader>td")  'toggle-debug-on-error
    (kbd "<leader>tf")  'auto-fill-mode
    (kbd "<leader>tl")  'display-line-numbers-mode
    ;; toggle wrapped lines
    (kbd "<leader>tw")  '(lambda () (interactive) (toggle-truncate-lines))
    (kbd "<leader>tm")  'toggle-frame-maximized
    (kbd "<leader>u")  'universal-argument
    (kbd "<leader>wd")  'delete-window
    (kbd "<leader>wh")  'windmove-left
    (kbd "<leader>wl")  'windmove-right
    (kbd "<leader>wk")  'windmove-up
    (kbd "<leader>wj")  'windmove-down
    (kbd "<leader>wm")  'delete-other-windows
    (kbd "<leader>wr")  'winner-redo
    (kbd "<leader>ws")  'evil-window-split
    (kbd "<leader>wu")  'winner-undo
    (kbd "<leader>wv")  'evil-window-vsplit
    (kbd "<leader>w=")  'balance-windows-area)
  (define-key emacs-lisp-mode-map (kbd "<normal-state> gr") nil)
  (evil-define-key 'insert 'global
    (kbd "<leader>SPC") 'execute-extended-command
    (kbd "M-<tab>") 'complete-symbol)
  (evil-define-key 'visual 'global
    (kbd "<leader>SPC") 'execute-extended-command)
  ;; dired
  (evil-define-key 'normal dired-mode-map
    (kbd "SPC") 'evil-send-leader
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-find-file)
  (evil-define-key 'normal help-mode-map
    (kbd "SPC") 'evil-send-leader)
  )
