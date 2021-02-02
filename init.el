;;;* Garbage collection
(setq gc-cons-threshold (* 100 1024 1024))
(defun cr-set-gc () (setq gc-cons-threshold (* 5 1024 1024)))
(add-hook 'after-init-hook 'cr-set-gc)

;;;* Path
(defconst cr-user-emacs-directory-lisp (expand-file-name "lisp" user-emacs-directory)
  "My Emacs configuration base directory.")

(let ((default-directory cr-user-emacs-directory-lisp))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;;* Modules
(when init-file-debug (require 'cr-debug))
(require 'cr-private-vars nil 'noerror)

;;;* Bootstrap `straight' and `use-package'
(custom-set-variables
 '(load-prefer-newer t)
 '(package-enable-at-startup nil)
 '(use-package-always-defer t)
 '(use-package-always-ensure nil)
 '(use-package-hook-name-suffix nil)
 '(use-package-enable-imenu-support t)
 '(use-package-expand-minimally t)
 '(straight-use-package-by-default t)
 '(straight-cache-autoloads t)
 '(straight-current-profile nil)
 '(straight-vc-git-default-clone-depth 'full))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(eval-when-compile
  (straight-use-package 'use-package)
  (require 'use-package))

;;;* Early packages
(straight-use-package 'org)

(use-package exec-path-from-shell
  :demand
  :custom (exec-path-from-shell-variables '("PATH" "MANPATH"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package diminish :demand)

(use-package no-littering
  :demand
  :custom
  (custom-file (no-littering-expand-var-file-name "custom.el"))
  (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;;;* Better defaults
(prefer-coding-system 'utf-8)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq default-frame-alist '((menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . nil)
                            (left-fringe . 8)
                            (right-fringe . 8)
                            (fullscreen . maximized)))

(setq-default fill-column 70
              indicate-empty-lines nil
              indicate-buffer-boundaries nil
              indent-tabs-mode nil
              tab-width 4)

(setq default-directory "~/"
      initial-major-mode 'lisp-interaction-mode
      initial-scratch-message nil
      inhibit-startup-screen t
      disabled-command-function nil
      ring-bell-function 'ignore
      sentence-end-double-space nil
      vc-follow-symlinks t
      create-lockfiles nil
      uniquify-buffer-name-style 'forward
      use-dialog-box nil)

(setq find-file-visit-truename t
      confirm-kill-emacs 'y-or-n-p
      save-abbrevs 'silently
      auto-save-default t
      auto-save-timeout 600
      auto-save-interval 300
      delete-auto-save-files t
      version-control t
      backup-by-copying t
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 5)

(setq select-enable-clipboard t
      select-enable-primary nil)

(setq scroll-margin 2
      scroll-step 1
      scroll-conservatively 101
      scroll-preserve-screen-position t
      mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed t
      next-screen-context-lines 5)

(when (eq system-type 'darwin)
  ;; Modifier keys behaviors on MacOS (mac/ns prefix depending on Emacs provisioner)
  (setq mac-option-modifier 'meta
        ns-alternate-modifier 'meta
        mac-command-modifier 'none
        ns-command-modifier 'none
        ;; Pass the right Alt/Option key to the OS which allows the
        ;; insertion of special characters.
        mac-right-option-modifier 'none
        ns-right-alternate-modifier 'none))

;;;* Personal prefix maps keybinds
(define-prefix-command 'cr-app-map)
(define-prefix-command 'cr-buffer-map)
(define-prefix-command 'cr-emacs-map)
(define-prefix-command 'cr-file-map)
(define-prefix-command 'cr-git-map)
(define-prefix-command 'cr-grammar-map)
(define-prefix-command 'cr-notes-map)
(define-prefix-command 'cr-search-map)
(define-prefix-command 'cr-text-map)
(define-prefix-command 'cr-toggle-map)
(define-prefix-command 'cr-spell-map)

(bind-keys ("C-c w" . cr-app-map)
           ("C-c b" . cr-buffer-map)
           ("C-c e" . cr-emacs-map)
           ("C-c f" . cr-file-map)
           ("C-c g" . cr-git-map)
           ("C-c n" . cr-notes-map)
           ("C-c s" . cr-search-map)
           ("C-c t" . cr-toggle-map)
           ("C-c v" . cr-grammar-map)
           ("C-c x" . cr-text-map)
           ("C-c z" . cr-spell-map))

;;;* Packages
(use-package ace-link
  :config (ace-link-setup-default)
  :bind ("M-g o" . ace-link))

(use-package ag)

(use-package align
  :straight nil
  :commands (align align-regexp)
  :preface
  (defun align-code (beg end &optional arg)
    (interactive "rP")
    (if (null arg)
        (align beg end)
      (let ((end-mark (copy-marker end)))
        (indent-region beg end-mark nil)
        (align beg end-mark))))
  :bind (:map cr-text-map
              ("a" . align-code)
              ("A" . align-regexp)))

(use-package async
  :after dired
  :init (dired-async-mode 1))

(use-package auth-source-pass
  :hook (after-init-hook . auth-source-pass-enable))

(use-package autorevert
  :straight nil
  :diminish (auto-revert-mode global-auto-revert-mode)
  :defer 2
  :custom
  (auto-revert-verbose t)
  (auto-revert-use-notify t)
  (revert-without-query (list "."))
  (auto-revert-stop-on-user-input nil)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-remote-files nil)
  :config
  ;; Prevent showing buffer reversion message while working in the minibuffer
  ;; Source: https://emacs.stackexchange.com/questions/46690
  (advice-add
   'auto-revert-handler
   :around (lambda (orig-fun &rest args)
             (let ((auto-revert-verbose (not (minibufferp (window-buffer)))))
               (apply orig-fun args))))

  (global-auto-revert-mode 1)
  :bind ((:map cr-buffer-map ("g". revert-buffer))
         (:map cr-toggle-map
               ("a" . auto-revert-mode)
               ("A" . global-auto-revert-mode))))

(use-package avy
  :config
  :custom
  (avy-keys '(?a ?s ?d ?f ?g ?h ?k ?l))
  (avy-timeout-seconds .3)
  (avy-all-windows t)
  (avy-all-windows-alt nil)
  :bind* ("C-'" . avy-goto-char-timer))

(use-package battery
  :straight nil
  :commands (battery display-battery-mode)
  :custom (battery-mode-line-limit 85)
  ;; :hook (after-init-hook . display-battery-mode)
  :bind (:map cr-toggle-map
              ("b" . display-battery-mode)))

(use-package bindings
  :straight nil
  :bind ("M-[" . mode-line-other-buffer))

(use-package browse-url
  :config
  (setq browse-url-browser-function
        '(("\\(youtube\\.com\\)\\|\\(youtu\\.be\\)" . browse-url-youtube-mpv)
          ("." . browse-url-default-browser)))

  (defun browse-url-youtube-mpv (url &rest e)
    "Use `mpv' with `youtube-dl' to open URLs, asking for desired quality
Documentation: https://github.com/ytdl-org/youtube-dl#format-selection"
    (let ((quality-arg "")
          (quality-val (completing-read "Max height resolution (0 for unlimited): "
                                        '("0" "480" "720" "1080") nil nil)))
      (setq quality-val (string-to-number quality-val))
      (message "Opening %s with height≤%s with mpv..." url quality-val)
      (when (< 0 quality-val)
        (setq quality-arg (format "--ytdl-format=bestvideo[height<=?%s]+bestaudio" quality-val)))
      (start-process "mpv" nil "mpv" quality-arg url)))

  :bind ("M-g w" . browse-url-at-point))

(use-package calc
  :straight nil
  :bind (:map cr-app-map ("c" . calc)))

(use-package company
  :commands (company-mode company-indent-or-complete-common)
  :demand
  :diminish
  :config
  (setq company-minimum-prefix-length 3
        company-idle-delay 0.2
        company-backends '(company-capf
                           (company-dabbrev-code company-gtags company-etags company-keywords)
                           company-files
                           company-dabbrev))
  (global-company-mode 1)
  :bind (("M-/"   . company-complete)
         ("C-c y" . company-yasnippet)
         (:map company-active-map ("M-/" . company-other-backend))
         (:map cr-toggle-map ("c" . company-mode))))

(use-package compile
  :straight nil
  :bind ("C-c m" . compile))

(use-package counsel
  :after ivy
  :demand
  :diminish
  :config
  (setq counsel-grep-base-command "grep -i -E -n -e %s %s"
        ivy-initial-inputs-alist nil) ;; `counsel' overwrites this setting even if customized

  (counsel-mode 1)
  :bind (("C-c SPC" . counsel-mark-ring)
         ("C-c j"   . counsel-git-grep)
         ("C-c k"   . counsel-rg)
         ("C-c o"   . counsel-outline)
         ("C-c i"   . counsel-imenu)
         ([remap jump-to-register]   . counsel-register)
         (:map cr-toggle-map
               ("T" . counsel-load-theme))
         (:map cr-search-map
               ("f" . counsel-file-jump)
               ("l" . counsel-locate)
               ("r" . counsel-recoll)
               ("z" . counsel-fzf))
         (:map cr-git-map
               ("l" . counsel-git-log))))

(use-package doom-themes
  :demand
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (defvar cr-theme-light 'doom-one-light)
  (defvar cr-theme-dark 'doom-one)
  (defvar cr-theme-default cr-theme-light)
  (defvar cr-theme-pair `(,cr-theme-light ,cr-theme-dark))

  (load-theme cr-theme-default t)
  (doom-themes-org-config)

  (defun cr-theme-toggle ()
    "Toggle between 2 themes defined in `cr-theme-pair'"
    (interactive)
    (let ((new-theme (car (remove (car custom-enabled-themes) cr-theme-pair))))
      (mapcar #'disable-theme custom-enabled-themes)
      (load-theme new-theme t)))

  :bind (:map cr-toggle-map ("t" . cr-theme-toggle)))

(use-package cr-counsel-terms
  :straight nil
  :commands (cr-counsel-vterm cr-counsel-eshell)
  :bind (:map cr-app-map
              ("e" . cr-counsel-eshell)
              ("t" . cr-counsel-vterm)))

(use-package cr-functions
  :straight nil
  :bind (([remap kill-region]    . cr-backward-kill-word-or-region)
         ([remap comment-dwim]   . cr-comment-or-uncomment-line-or-region)
         ([remap fill-paragraph] . cr-fill-or-unfill-paragraph)
         ("M-[" . cr-switch-to-last-buffer)
         (:map cr-buffer-map
               ("n" . cr-new-empty-buffer)
               ("r" . cr-rename-buffer)
               ("x" . cr-goto-scratch))
         (:map cr-file-map
               ("c" . cr-copy-this-file)
               ("D" . cr-delete-current-buffer-file)
               ("r" . cr-rename-current-buffer-file)
               ("y" . cr-yank-filename)
               ("Y" . cr-yank-filename-fullpath))
         (:map cr-text-map
               ("DEL" . cr-flush-blank-lines)
               ("c"   . cr-collapse-blank-lines)
               ("u"   . cr-uniquify-lines)
               ("+"   . cr-increment-number-at-point)
               ("-"   . cr-decrement-number-at-point))
         (:map cr-emacs-map
               ("e" . cr-edit-emacs-init-config)
               ("q" . cr-emacs-quit)
               ("r" . cr-reload-emacs)
               ("t" . cr-test-emacs-config))
         (:map cr-search-map
               ("h" . cr-github-search)
               ("w" . cr-duckduckgo-search))))

(use-package cr-immortal-buffers
  :straight nil
  :demand
  :custom (cr--immortal-buffers-list '("*scratch*" "*Messages")))

(use-package cr-open-external
  :straight nil
  :bind (:map cr-file-map ("e" . cr-open-file-or-directory-in-external-app)))

(use-package cr-org-gtd
  :straight nil
  :demand
  :after org)

(use-package cr-sudo-utils
  :straight nil
  :bind (:map cr-file-map
              ("u" . doom/sudo-this-file)
              ("U" . doom/sudo-save-buffer)))

(use-package css-mode
  :mode "\\.css\\'")

(use-package csv-mode
  :init
  (setq csv-separators '("," ";" "|" " "))
  (add-hook 'csv-mode-hook 'csv-header-line)
  :mode "\\.csv\\'")

(use-package deft
  :disabled
  :custom
  (deft-recursive nil)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/zet/")
  (deft-archive-directory "archives/")
  :bind (:map cr-notes-map ("d" . deft)))

(use-package diff
  :bind (:map cr-file-map ("d" . diff-buffer-with-file)))

(use-package dired
  :straight nil
  :custom
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-listing-switches "-lahv --group-directories-first") ;; requires ls from coreutils on MacOS
  :config
  (put 'dired-find-alternate-file 'disabled nil)

  (defvar cr-dired-sort-base "-lahv")
  (defun cr-dired-sort-by-group ()
    (interactive)
    (dired-sort-other (concat cr-dired-sort-base " --group-directories-first")))
  (defun cr-dired-sort-by-name ()
    (interactive)
    (dired-sort-other (concat cr-dired-sort-base "")))
  (defun cr-dired-sort-by-size ()
    (interactive)
    (dired-sort-other (concat cr-dired-sort-base " -S")))
  (defun cr-dired-sort-by-time ()
    (interactive)
    (dired-sort-other (concat cr-dired-sort-base " -t")))

  (define-prefix-command 'cr-dired-sort-map)
  (define-key cr-dired-sort-map (kbd "g") 'cr-dired-sort-by-group)
  (define-key cr-dired-sort-map (kbd "n") 'cr-dired-sort-by-name)
  (define-key cr-dired-sort-map (kbd "t") 'cr-dired-sort-by-time)
  (define-key cr-dired-sort-map (kbd "s") 'cr-dired-sort-by-size)

  (define-key dired-mode-map (kbd "[") 'dired-up-directory)
  (define-key dired-mode-map (kbd "e") 'ediff-files)
  (define-key dired-mode-map (kbd "s") 'cr-dired-sort-map)

  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'hl-line-mode)
  :bind (:map dired-mode-map ("K" . dired-kill-subdir)))

(use-package dired-git-info
  :after dired
  :bind (:map dired-mode-map (")" . dired-git-info-mode)))

(use-package dired-narrow
  :disabled
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow-regexp)))

(use-package dired-rsync
  :after dired
  :bind (:map dired-mode-map ("r" . dired-rsync)))

(use-package dired-subtree
  :disabled
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

(use-package dired-x
  :straight nil
  :demand
  :after dired)

(use-package display-line-numbers
  :bind (:map cr-toggle-map ("l" . display-line-numbers-mode)))

(use-package docker
  :commands docker
  :bind (:map cr-app-map ("d" . docker)))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'"))

(use-package doom-modeline
  :straight doom-modeline
  :straight all-the-icons
  :custom
  (doom-modeline-vcs-max-length 18)
  (doom-modeline-icon t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-project-detection 'projectile)
  :hook (after-init-hook . doom-modeline-mode))

(use-package ediff
  :commands (ediff ediff-buffers ediff-files magit-ediff-dwim)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)

  (defvar cr--ediff-saved-window-configuration nil
    "Window configuration to restore after exiting an Ediff session.")

  (defun cr--ediff-save-window-config ()
    "Save current window configuration.
For ediff hooks usage"
    (setq cr--ediff-saved-window-configuration (current-window-configuration)))

  (defun cr--ediff-restore-window-config ()
    "Restore previous window configuration.
For ediff hooks usage"
    (when cr--ediff-saved-window-configuration
      (set-window-configuration cr--ediff-saved-window-configuration)))

  (defun cr--ediff-prepare-buffer ()
    (when (memq major-mode '(org-mode))
      (outline-show-all)))

  :hook ((ediff-prepare-buffer-hook . cr--ediff-prepare-buffer)
         (ediff-before-setup-hook   . cr--ediff-save-window-config)
         (ediff-quit-hook           . cr--ediff-restore-window-config)
         (ediff-suspend-hook        . cr--ediff-restore-window-config))
  :bind (("C-c w d" . ediff-buffers)
         ("C-c w D" . ediff-show-registry)))

(use-package eldoc
  :straight nil
  :diminish
  :commands (eldoc-mode global-eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.5)
  (global-eldoc-mode -1)
  :hook ((emacs-lisp-mode-hook c-mode-common) . eldoc-mode))

(use-package electric-pair
  :straight nil
  :bind (:map cr-toggle-map ("e" . electric-pair-local-mode)))

(use-package elfeed
  :commands elfeed
  :init
  (require 'cr-private-feeds nil 'noerror)
  :custom
  (elfeed-search-filter "@2-weeks-ago +unread ")
  (elfeed-search-date-format '("%m-%d" 5 :left))
  :config
  (defun cr-elfeed-show-settings ()
    (setq-local shr-width fill-column)
    (setq-local shr-max-image-proportion 0.7)
    (setq-local line-spacing 0.2))

  (defun elfeed-entry-other-window ()
    "In Elfeed search, display the entry at point in another window."
    (interactive)
    (split-window-sensibly (selected-window))
    (switch-to-buffer-other-window "*elfeed-search*")
    (call-interactively #'elfeed-search-show-entry)
    (switch-to-buffer-other-window "*elfeed-search*")
    (forward-line))
  :hook (elfeed-show-mode-hook . cr-elfeed-show-settings)
  :bind ((:map cr-app-map ("f" . elfeed))
         (:map elfeed-search-mode-map
               ("a" . elfeed-search-show-entry)
               ("o" . elfeed-entry-other-window))
         (:map elfeed-show-mode-map
               ("n" . next-line)
               ("p" . previous-line)
               ("[" . elfeed-show-prev)
               ("]" . elfeed-show-next))))

(use-package elisp-mode
  :straight nil
  :commands emacs-lisp-mode
  :config
  (defun cr-emacs-lisp-settings ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (add-hook 'emacs-lisp-mode-hook 'cr-emacs-lisp-settings))

(use-package eshell
  :straight nil
  :commands (eshell eshell-command)
  :custom
  (eshell-ls-use-colors t)
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-visual-commands '("crontab" "tmux" "htop" "tail" "vi" "screen" "top" "less" "more"))
  (eshell-modules-list '(eshell-alias
                         ;; eshell-banner
                         eshell-basic
                         eshell-cmpl
                         eshell-dirs
                         eshell-glob
                         eshell-hist
                         eshell-ls
                         eshell-pred
                         eshell-prompt
                         eshell-script
                         eshell-smart
                         eshell-term
                         eshell-tramp
                         eshell-unix))
  :config
  (defalias 'v 'eshell-exec-visual)

  (defun cr-eshell-settings ()
    (company-mode -1)
    (setenv "PAGER""cat")
    (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
    (when (featurep 'counsel)
      (define-key eshell-mode-map (kbd "C-r") 'counsel-esh-history)))

  (add-hook 'eshell-mode-hook 'cr-eshell-settings))

(use-package eww
  :bind (:map cr-app-map ("w" . eww)))

(use-package face-remap
  :diminish buffer-face-mode
  :config
  (custom-theme-set-faces
   'user
   '(org-block                 ((t (:inherit fixed-pitch))))
   '(org-checkbox              ((t (:inherit fixed-pitch))))
   '(org-code                  ((t (:inherit (shadow fixed-pitch)))))
   '(org-date                  ((t (:inherit fixed-pitch))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-formula               ((t (:inherit fixed-pitch))))
   '(org-indent                ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link                  ((t (:inherit (fixed-pitch) :underline t))))
   '(org-meta-line             ((t (:inherit fixed-pitch))))
   '(org-property-value        ((t (:inherit fixed-pitch))))
   '(org-special-keyword       ((t (:inherit fixed-pitch))))
   '(org-table                 ((t (:inherit fixed-pitch))))
   '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold))))
   '(org-todo                  ((t (:inherit fixed-pitch))))
   '(org-verbatim              ((t (:inherit fixed-pitch)))))
  :bind (:map cr-toggle-map ("p" . variable-pitch-mode)))

(use-package ffap
  :straight nil
  :bind ("M-g f" . find-file-at-point))

(use-package flycheck
  :commands (flycheck-mode global-flycheck-mode)
  :custom
  (flycheck-idle-change-delay 0.5)
  (lycheck-standard-error-navigation t)
  (flycheck-indication-mode 'left-fringe)
  (flycheck-global-modes t)
  :hook (prog-mode-hook . flycheck-mode)
  :bind (:map cr-toggle-map
              ("f" . flycheck-mode)
              ("F" . global-flycheck-mode)))

(use-package flycheck-popup-tip
  :disabled
  :after flycheck
  :config (setq flycheck-pos-tip-display-errors-tty-function #'flycheck-popup-tip-show-popup)
  :hook (flycheck-mode-hook . flycheck-popup-tip-mode))

(use-package flyspell
  :custom
  (ispell-silently-savep t)
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil)
  :config
  (defun cr-ispell-set-FR ()
    (interactive)
    (ispell-change-dictionary "francais"))

  (defun cr-ispell-set-EN ()
    (interactive)
    (ispell-change-dictionary "english"))

  (defun cr-save-word-to-pdict ()
    "Save word at point to the personal dictionary"
    (interactive)
    (let ((current-location (point))
          (word (flyspell-get-word)))
      (when (consp word)
        (flyspell-do-correct 'save nil (car word)
                             current-location (cadr word) (caddr word)
                             current-location))))

  :hook ((org-mode-hook . flyspell-mode)
         (prog-mode-hook . flyspell-prog-mode))
  :bind ((:map cr-toggle-map ("z" . flyspell-mode))
         (:map cr-spell-map
               ("b" . flyspell-buffer)
               ("d" . ispell-change-dictionary)
               ("e" . cr-ispell-set-EN)
               ("f" . cr-ispell-set-FR)
               ("r" . flyspell-region)
               ("s" . cr-save-word-to-pdict)
               ("z" . flyspell-correct-wrapper))))

(use-package flyspell-correct-ivy
  :after (flyspell ivy)
  :config (setq flyspell-correct-interface 'flyspell-correct-ivy))

(use-package follow
  :bind (:map cr-toggle-map ("=" . follow-delete-other-windows-and-split)))

(use-package forge
  :after magit
  :commands forge-pull-notifications)

(use-package frame
  :straight nil
  :bind (:map cr-toggle-map ("RET" . toggle-frame-fullscreen)))

(use-package go-mode
  :straight t
  :straight company-go
  :straight go-eldoc
  :straight gorepl-mode
  :mode "\\.go\\'"
  :config
  (defun cr-go-mode-settings ()
    "Basic go-mode settings to be used when lsp-mode is not desired."
    (go-eldoc-setup)
    (eldoc-mode 1)
    (set (make-local-variable 'company-backends) '(company-go))
    (set (make-local-variable 'compile-command) "go build -v")
    (company-mode 1))
  :hook (go-mode-hook . cr-go-mode-settings))

(use-package git-gutter
  :diminish
  :commands git-gutter-mode
  :config
  (setq git-gutter:visual-line t
        git-gutter:update-interval 0
        git-gutter:window-width 1
        git-gutter:hide-gutter t)
  :bind ((:map cr-toggle-map
               ("g" . git-gutter-mode))
         (:map cr-git-map
               ("[" . git-gutter:previous-hunk)
               ("]" . git-gutter:next-hunk)
               ("r" . git-gutter:revert-hunk)
               ("s" . git-gutter:stage-hunk)
               ("SPC" . git-gutter:mark-hunk))))

(use-package helpful
  :after counsel
  :commands (helpful-callable
             helpful-variable)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind ("C-h k" . helpful-key))

(use-package hl-line
  :straight nil
  :bind (:map cr-toggle-map
              ("h" . hl-line-mode)
              ("H" . global-hl-line-mode)))

(use-package ibuffer
  :straight nil
  :config (add-hook 'ibuffer-mode-hook 'hl-line-mode)
  :bind (("C-x C-b" . ibuffer)
         (:map ibuffer-mode-map
               ("M-o")
               ("a" . ibuffer-visit-buffer))))

(use-package iedit
  :commands iedit-mode
  :bind ("M-i" . iedit-mode))

(use-package indent
  :straight nil
  :bind (:map indent-rigidly-map
              (">" . indent-rigidly-right)
              ("<" . indent-rigidly-left)
              ("C->" . indent-rigidly-right-to-tab-stop)
              ("C-<" . indent-rigidly-left-to-tab-stop)))

(use-package ivy
  :demand
  :diminish
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-magic-tilde t)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'full)
  (ivy-wrap nil)
  :config
  ;; Temporary fix, waiting for
  ;; https://github.com/abo-abo/swiper/issues/2681 to be resolved
  (with-eval-after-load 'grep
    (define-key ivy-occur-grep-mode-map (kbd "n") 'next-error)
    (define-key ivy-occur-grep-mode-map (kbd "p") 'previous-error))
  :bind ("C-c r" . ivy-resume))

(use-package ivy-pass
  :bind ("C-c q" . ivy-pass)
  :hook (after-init-hook . ivy-mode))

(use-package ivy-rich
  :after ivy
  :defer 2
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-parse-remote-buffer nil
        ivy-rich-parse-remote-file-path nil)
  (ivy-rich-mode 1))

(use-package ivy-xref
  :demand
  :after ivy
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package kubernetes
  :if (executable-find "kubectl")
  :bind (:map cr-app-map ("k" . kubernetes-overview)))

(use-package langtool
  :custom
  (langtool-default-language 'auto)
  (langtool-disabled-rules '("DASH_RULE" "WHITESPACE_RULE" "EN_UNPAIRED_BRACKETS"
                             "COMMA_PARENTHESIS_WHITESPACE" "EN_QUOTES"
                             "MORFOLOGIK_RULE_EN_GB" "MORFOLOGIK_RULE_US"))
  :bind (:map cr-grammar-map
              ("v" . langtool-check)
              ("b" . langtool-correct-buffer)
              ("d" . langtool-switch-default-language)
              ("n" . langtool-goto-next-error)
              ("p" . langtool-goto-previous-error)
              ("q" . langtool-check-done)))

(use-package langtool
  :if (eq system-name "t460")
  :custom (langtool-language-tool-jar "~/opt/languagetool.org/languagetool-commandline.jar"))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-snippet t)
  :hook (lsp-mode-hook . lsp-enable-which-key-integration)
  :custom (lsp-keymap-prefix "C-c u")
  :bind-keymap ("C-c u" . lsp-command-map)
  :bind (:map cr-toggle-map ("u" . lsp)))

(use-package lsp-mode
  :if (executable-find "clang")
  :hook (c-mode-common-hook . lsp-deferred))

(use-package lsp-mode
  :if (executable-find "gopls")
  :hook (go-mode-hook . lsp-deferred))

(use-package magit
  :defer 10
  :commands magit-status
  :custom
  (magit-auto-revert-immediately t)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk nil)
  :bind (:map cr-git-map
              ("b" . magit-branch-checkout)
              ("B" . magit-blame)
              ("c" . magit-clone)
              ("g" . magit-status)
              ("p" . magit-pull-from-upstream)))

(use-package markdown-mode
  :commands markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'"))

(use-package modus-operandi-theme
  :custom
  (modus-operandi-theme-scale-headings nil)
  (modus-operandi-theme-org-blocks 'rainbow)
  (modus-operandi-theme-slanted-constructs t)
  (modus-operandi-theme-bold-constructs t))

(use-package modus-vivendi-theme
  :custom
  (modus-vivendi-theme-scale-headings nil)
  (modus-vivendi-theme-org-blocks 'rainbow)
  (modus-vivendi-theme-slanted-constructs t)
  (modus-vivendi-theme-bold-constructs t))

(use-package multiple-cursors
  :bind (("C-c C-SPC"     . mc/edit-lines)
         ("C-c C-a"       . mc/mark-all-like-this-dwim)
         ("C-c C-A"       . mc/mark-all-dwim)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-M->"         . mc/unmark-previous-like-this)
         ("C-M-<"         . mc/unmark-next-like-this)
         ("C-c C->"       . mc/mark-all-like-this)
         ("<C-M-mouse-1>" . mc/add-cursor-on-click)))

(use-package nov
  :commands nov-mode
  :custom (nov-text-width fill-column)
  :config
  (defun cr-nov-settings ()
    (setq-local left-margin-width 2)
    (setq-local line-spacing 0.2))
  (add-hook 'nov-mode-hook 'cr-nov-settings)
  :mode ("\\.epub\\'" . nov-mode))

(use-package olivetti
  :bind (:map cr-toggle-map ("o" . olivetti-mode)))

(use-package org
  :commands (org-agenda org-capture)
  :config
  (setq org-startup-folded t
        org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
        org-ctrl-k-protect-subtree t
        org-ellipsis " ▼"
        org-fontify-done-headline t
        org-hide-emphasis-markers t
        org-link-file-path-type 'adaptive
        org-hide-leading-stars t
        org-use-tag-inheritance t
        org-indent-indentation-per-level 1
        org-deadline-warning-days 14)

  (setq calendar-weekend-days '(6 0)
        calendar-week-start-day 1)

  (when (require 'french-holidays nil 'noerror)
    (setq calendar-holidays holiday-french-holidays))

  (when (version< org-version "9.2")
    (setq org-structure-template-alist nil))

  (setq org-use-speed-commands t)
  (add-to-list 'org-speed-commands-user '("N" call-interactively 'org-metadown))
  (add-to-list 'org-speed-commands-user '("P" call-interactively 'org-metaup))
  (add-to-list 'org-speed-commands-user '("d" call-interactively 'org-deadline))
  (add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
  (add-to-list 'org-speed-commands-user '("q" call-interactively 'org-set-tags-command))
  (add-to-list 'org-speed-commands-user '("w" call-interactively 'cr-org-refile-in-current-buffer))
  (add-to-list 'org-speed-commands-user '("W" call-interactively 'cr-org-refile-in-any-buffer))

  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (sql . t)
     (C . t)))

  (setq org-babel-default-header-args:sh
        '((:results  . "output")))

  (require 'org-agenda)
  (setq org-agenda-window-setup 'current-window
        org-agenda-follow-indirect nil
        org-agenda-show-outline-path nil
        org-agenda-restore-windows-after-quit t
        org-agenda-log-mode-items '(closed clock state)
        org-agenda-todo-ignore-scheduled 'future
        org-agenda-tags-todo-honor-ignore-options t
        org-agenda-skip-scheduled-if-done nil
        org-agenda-skip-deadline-if-done nil
        org-agenda-skip-deadline-prewarning-if-scheduled nil)

  (setq org-clock-idle-time nil
        org-clock-continuously nil
        org-clock-persist t
        org-clock-in-switch-to-state nil
        org-clock-in-resume nil
        org-clock-report-include-clocking-task t
        org-show-notification-handler 'message)

  (org-clock-persistence-insinuate)

  (setq org-log-done 'time
        org-log-into-drawer t
        org-log-state-notes-insert-after-drawers nil)

  (setq org-refile-allow-creating-parent-nodes 'confirm
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path 'file)

  (defun cr-org-refile-in-current-buffer ()
    (interactive)
    (let ((org-refile-targets '((nil :maxlevel . 5))))
      (org-refile)))

  (defun cr-org-refile-in-any-buffer ()
    (interactive)
    (let ((org-refile-targets '((cr--org-buffer-list :maxlevel . 5))))
      (org-refile)))

  (defun cr--org-buffer-list ()
    (delq nil
          (mapcar (lambda (buffer)
                    (buffer-file-name buffer))
                  (org-buffer-list 'files t))))

  (defun cr-org-pretty-symbols ()
    ;; (push '(":PROPERTIES:"     . ?:) prettify-symbols-alist)
    (push '("#+begin_src"      . ?↦) prettify-symbols-alist)
    (push '("#+BEGIN_SRC"      . ?↦) prettify-symbols-alist)
    (push '("#+end_src"        . ?⇤) prettify-symbols-alist)
    (push '("#+END_SRC"        . ?⇤) prettify-symbols-alist)
    ;; (push '("#+RESULTS:"       . ?↩) prettify-symbols-alist)
    (push '("#+begin_quote"    . ?↦) prettify-symbols-alist)
    (push '("#+BEGIN_QUOTE"    . ?↦) prettify-symbols-alist)
    (push '("#+end_quote"      . ?⇤) prettify-symbols-alist)
    (push '("#+END_QUOTE"      . ?⇤) prettify-symbols-alist)
    (push '("#+begin_example"  . ?↦) prettify-symbols-alist)
    (push '("#+BEGIN_EXAMPLE"  . ?↦) prettify-symbols-alist)
    (push '("#+end_example"    . ?⇤) prettify-symbols-alist)
    (push '("#+END_EXAMPLE"    . ?⇤) prettify-symbols-alist)
    (prettify-symbols-mode t))

  (when (featurep 'diminish)
    (eval-after-load 'org-indent '(diminish 'org-indent-mode)))

  (add-hook 'org-mode-hook 'cr-org-settings)
  (defun cr-org-settings ()
    (setq-local delete-trailing-lines nil)
    (setq-local indicate-empty-lines nil)
    (setq-local indicate-buffer-boundaries nil)
    (cr-org-pretty-symbols)
    (auto-fill-mode -1)
    (org-indent-mode 1))

  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)))

(use-package org-cliplink
  :after org
  :commands (org-cliplink org-cliplink-capture)
  :bind (:map org-mode-map ("C-c C-S-L" . org-cliplink)))

(use-package org-noter
  :after org
  :commands org-noter
  :custom
  (org-noter-default-notes-file-names '("main.org" "notes.org"))
  (org-noter-notes-search-path (list org-directory))
  (org-noter-always-create-frame nil)
  :bind ("C-c w n" . org-noter))

(use-package org-roam
  :custom
  (org-roam-directory "~/zet/")
  (org-roam-db-gc-threshold (* 50 1024 1024))
  :bind (:map cr-notes-map
              ("." . org-roam-jump-to-index)
              ("b" . org-roam-switch-to-buffer)
              ("c" . org-roam-capture)
              ("l" . org-roam)
              ("f" . org-roam-find-file)
              ("g" . org-roam-graph)
              ("i" . org-roam-insert)
              ("I" . org-roam-insert-immediate)))

(use-package org-superstar
  :after org
  :custom (org-superstar-prettify-item-bullets nil)
  :hook (org-mode-hook . org-superstar-mode))

(use-package org-ql
  :bind (:map cr-search-map ("o" . org-ql-search)))

(use-package ox-reveal
  :demand
  :after org
  :custom (org-reveal-root "~/js/reveal.js"))

(use-package pass
  :custom (pass-show-keybindings nil)
  :bind ("C-c Q" . pass))

(use-package password-cache
  :straight nil
  :demand
  :custom
  (password-cache 5)
  (password-cache-expiry 600))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-view-use-scaling t)
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'pdf-view-mode-hook 'pdf-annot-minor-mode)
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "d") 'pdf-annot-delete))

(use-package php-mode)

(use-package proced
  :straight nil
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 5)
  :bind (:map cr-app-map ("x" . proced)))

(use-package prog-mode
  :straight nil
  :config
  (defun cr-prog-mode-settings ()
    (setq-local indicate-empty-lines t))
  (add-hook 'prog-mode-hook 'cr-prog-mode-settings))

(use-package projectile
  :diminish
  :defer 10
  :commands (projectile-find-file
             projectile-ripgrep
             projectile-switch-project)
  :custom
  (projectile-indexing-method 'alien)
  (projectile-enable-caching nil)
  (projectile-verbose t)
  (projectile-switch-project-action (lambda ()
                                      (dired (projectile-project-root))))
  :config
  (projectile-mode 1)
  (defun cr-projectile-refresh ()
    (interactive)
    (projectile-cleanup-known-projects)
    (projectile-discover-projects-in-search-path)
    (message "Projectile refresh: done"))
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map ("." . cr-projectile-refresh)))

(use-package recentf
  :straight nil
  :demand
  :commands recentf-mode
  :custom
  (recentf-max-saved-items 50)
  (recentf-keep '(file-remote-p file-readable-p))
  :config
  (add-to-list 'recentf-exclude "COMMIT_MSG")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG")
  (when (featurep 'no-littering)
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(use-package replace
  :straight nil
  :bind ((:map occur-mode-map
               ("n" . next-line)
               ("p" . previous-line))
         (:map cr-text-map
               ("f" . flush-lines)
               ("k" . keep-lines)
               ("r" . query-replace)
               ("R" . query-replace-regexp))))

(use-package ripgrep
  :bind (:map cr-search-map ("g" . ripgrep-regexp)))

(use-package rust-mode)

(use-package saveplace
  :straight nil
  :hook (after-init-hook . save-place-mode))

(use-package simple
  :straight nil
  :diminish visual-line-mode
  :custom
  (delete-trailing-lines t)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  :bind (([remap just-one-space]  . cycle-spacing)
         ([remap zap-to-char]     . zap-up-to-char)
         ([remap upcase-word]     . upcase-dwim)
         ([remap downcase-word]   . downcase-dwim)
         ([remap capitalize-word] . capitalize-dwim)
         (:map cr-emacs-map ("x" . list-processes))
         (:map cr-text-map ("d"  . delete-trailing-whitespace))
         (:map cr-toggle-map
               ("v" . visual-line-mode)
               ("V" . toggle-truncate-lines)))
  :hook (org-mode-hook . visual-line-mode))

(use-package smartparens
  :commands (smartparens-mode smartparens-scrict-mode)
  :diminish
  :config
  (require 'smartparens-config)
  (defun cr-smartparens-settings ()
    (electric-pair-mode -1)
    (show-smartparens-mode 1))
  :hook ((prog-mode-hook               . smartparens-mode)
         (smartparens-mode-hook        . cr-smartparens-settings)
         (smartparens-strict-mode-hook . cr-smartparens-settings))
  :bind ((:map smartparens-mode-map
               ([remap forward-sexp]   . sp-forward-sexp)
               ([remap backward-sexp]  . sp-backward-sexp)
               ([remap mark-sexp]      . sp-mark-sexp)
               ("C-M-n"           . sp-next-sexp)
               ("C-M-p"           . sp-previous-sexp)
               ("C-M-a"           . sp-beginning-of-sexp)
               ("C-M-e"           . sp-end-of-sexp)
               ("C-M-u"           . sp-backward-up-sexp)
               ("C-M-d"           . sp-down-sexp)
               ("C-k"             . sp-kill-hybrid-sexp)
               ("C-M-k"           . sp-kill-sexp)
               ("C-M-w"           . sp-copy-sexp)
               ("C-M-t"           . sp-transpose-sexp)
               ("C-x C-t"         . sp-transpose-hybrid-sexp)
               ("C-}"             . sp-forward-slurp-sexp)
               ("C-\{"            . sp-forward-barf-sexp)
               ("C-M-{"           . sp-backward-slurp-sexp)
               ("C-M-\}"          . sp-backward-barf-sexp)
               ("M-D"             . sp-splice-sexp)
               ("M-<backspace>"   . sp-unwrap-sexp)
               ("C-M-<backspace>" . sp-splice-sexp-killing-around)
               ("M-F"             . sp-forward-symbol)
               ("M-B"             . sp-backward-symbol)
               ("C-M-;"           . sp-comment)
               ("C-("             . sp-rewrap-sexp)
               ("C-\""            . sp-change-inner))
         (:map cr-toggle-map
               ("s" . smartparens-mode)
               ("S" . smartparens-strict-mode))))

(use-package sort
  :bind (:map cr-text-map ("s" . sort-lines)))

(use-package sql
  :config
  (defun cr-isql-config ()
    (setq-local truncate-lines t))
  (add-hook 'sql-interactive-mode-hook 'cr-isql-config)
  :bind ((:map cr-app-map ("s" . sql-connect))))

(use-package ssh-config-mode)

(use-package swiper
  :after counsel
  :bind (:map cr-search-map
              ("a" . swiper-all)
              ("s" . swiper-isearch)
              ("." . swiper-isearch-thing-at-point)))

(use-package terraform-mode
  :mode "\.tf\\'")

(use-package time
  :straight nil
  :commands (display-time-world display-time-mode)
  :custom
  (display-time-24hr-format t)
  (display-time-default-load-average nil)
  (display-time-world-list '(("America/Los_Angeles" "Seattle")
                             ("America/New_York" "New York")
                             ("Europe/London" "London")
                             ("Europe/Paris" "Paris")
                             ("Asia/Kolkata" "Calcutta")
                             ("Asia/Shanghai" "Beijing")
                             ("Asia/Tokyo" "Tokyo")))
  :config
  (defun cr-display-time-world ()
    (interactive)
    (display-time-world)
    (switch-to-buffer-other-window "*wclock*")
    (search-forward "Paris")
    (move-beginning-of-line nil)
    (hl-line-mode 1))
  ;; :hook (after-init-hook . display-time-mode)
  :bind ((:map cr-toggle-map
               ("." . display-time-mode))
         (:map cr-app-map
               ("." . cr-display-time-world))
         (:map display-time-world-mode-map
               ("n" . next-line)
               ("p" . previous-line))))

(use-package vterm
  :commands vterm
  :init (setq vterm-always-compile-module t)
  :custom (vterm-max-scrollback (* 20 1000))
  :config
  (defun cr-vterm-yank-pop ()
    "Call my version of vterm-yank-pop and insert into vterm.
Source: https://github.com/rlister/emacs.d/blob/master/lisp/vterm-cfg.el"
    (interactive)
    (let ((inhibit-read-only t))
      (vterm-send-string (counsel-yank-pop))))

  (defun cr-vterm-settings ()
    (setq-local global-hl-line-mode nil))
  :hook (vterm-mode-hook . cr-vterm-settings)
  :bind (:map vterm-mode-map ("M-y" . cr-vterm-yank-pop)))

(use-package wdired
  :after dired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-allow-to-redirect-links t)
  (wdired-create-parent-directories t))

(use-package wgrep)

(use-package window
  :straight nil
  :bind ("M-o" . other-window))

(use-package winner
  :straight nil
  :hook (after-init-hook . winner-mode)
  :bind (("C-x u" . winner-undo)
         ("C-x U" . winner-redo)))

(use-package which-key
  :commands which-key-mode
  :defer 2
  :diminish
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.2)
  (which-key-allow-imprecise-window-fit t)
  :config (which-key-mode 1)
  :bind (:map cr-toggle-map ("?" . which-key-mode)))

(use-package whitespace
  :bind (:map cr-toggle-map ("SPC" . whitespace-mode)))

(use-package woman
  :bind (:map cr-app-map ("m" . woman)))

(use-package writeroom-mode
  :custom
  (writeroom-restore-window-config t)
  (writeroom-fringes-outside-margins t)
  (writeroom-width (+ fill-column 10))
  (writeroom-major-modes-exceptions nil)
  (writeroom-major-modes '(text-mode org-mode elfeed-show-mode))
  :config
  (advice-add 'text-scale-adjust :after
              #'visual-fill-column-adjust)
  :bind ((:map writeroom-mode-map
               ("C-M-<" . writeroom-decrease-width)
               ("C-M->" . writeroom-increase-width)
               ("C-M-=" . writeroom-adjust-width))
         (:map cr-toggle-map
               ("w" . writeroom-mode)
               ("W" . global-writeroom-mode))))

(use-package ws-butler
  :diminish
  :hook (prog-mode-hook . ws-butler-mode))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'"))

(use-package yasnippet
  :commands (yas-expand company-yasnippet)
  :defer 2
  :diminish yas-minor-mode
  :config
  (setq yas-verbosity 2)
  (yas-global-mode 1))
