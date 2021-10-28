;;; System tuning
(setq gc-cons-threshold (* 20 1024 1024)
      read-process-output-max (* 3 1024 1024))
(defun native-comp-available-p () 'nil) ;; temp fix for org-roam

;;; Variables and constants
(setq default-directory "~/")
(defconst cr-data-dir "~/nextcloud")
(defconst cr-org-dir (expand-file-name "org" cr-data-dir))
(defconst cr-zet-dir (expand-file-name "zet" cr-org-dir))
(defconst cr-bibliography
  (directory-files (expand-file-name "bibliography" cr-org-dir) t ".*.bib"))
(defconst cr-library (expand-file-name "library" cr-org-dir))
(defconst system-is-osx-p (eq system-type 'darwin))
(defconst system-is-linux-p (eq system-type 'gnu/linux))
(defconst system-is-windows-p (eq system-type 'windows-nt))

;;; Path
(defconst cr-user-emacs-directory-lisp
  (expand-file-name "lisp" user-emacs-directory)
  "My Emacs configuration base directory.")

(let ((default-directory cr-user-emacs-directory-lisp))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; Modules
(when init-file-debug (require 'cr-debug nil 'noerror))
(require 'cr-private-vars nil 'noerror)

;;; Bootstrap `straight' and `use-package'
(custom-set-variables
 '(load-prefer-newer t)
 '(package-enable-at-startup nil)
 '(use-package-always-defer t)
 '(use-package-always-ensure nil)
 '(use-package-enable-imenu-support t)
 '(use-package-expand-minimally t)
 '(straight-use-package-by-default t)
 '(straight-cache-autoloads t)
 '(straight-current-profile nil)
 '(straight-vc-git-default-clone-depth 1)
 '(straight-check-for-modifications '(check-on-save find-when-checking)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
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

;;; Early packages
(straight-use-package 'org)

(use-package exec-path-from-shell
  :if (or (daemonp) (display-graphic-p))
  :hook (after-init . exec-path-from-shell-initialize))

(use-package diminish :demand)

(use-package no-littering
  :demand
  :custom
  (custom-file (no-littering-expand-var-file-name "custom.el"))
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;;; Better defaults
(prefer-coding-system 'utf-8)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default fill-column 80
              indicate-empty-lines nil
              indicate-buffer-boundaries nil
              indent-tabs-mode nil
              tab-width 4)

(customize-set-variable
 'default-frame-alist '((menu-bar-lines . 0)
                        (tool-bar-lines . 0)
                        (vertical-scroll-bars . nil)
                        (left-fringe . 8)
                        (right-fringe . 8)))

(custom-set-variables
 '(disabled-command-function nil)
 '(ring-bell-function 'ignore)
 '(sentence-end-double-space nil)
 '(vc-follow-symlinks t)
 '(create-lockfiles nil)
 '(uniquify-buffer-name-style 'forward)
 '(use-dialog-box nil)
 '(frame-title-format "Emacs")
 '(enable-local-variables nil)
 '(visible-bell nil))

(custom-set-variables
 '(find-file-visit-truename t)
 '(confirm-kill-emacs 'y-or-n-p)
 '(save-abbrevs 'silently)
 '(auto-save-default t)
 '(auto-save-timeout 600)
 '(auto-save-interval 300)
 '(delete-auto-save-files t)
 '(version-control t)
 '(backup-by-copying t)
 '(delete-old-versions t)
 '(kept-old-versions 2)
 '(kept-new-versions 5))

(custom-set-variables
 '(select-enable-clipboard t)
 '(select-enable-primary nil))

(custom-set-variables
 '(scroll-margin 2)
 '(scroll-step 1)
 '(scroll-conservatively 101)
 '(scroll-preserve-screen-position t)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1)))
 '(mouse-wheel-progressive-speed t)
 '(next-screen-context-lines 5))

(when system-is-osx-p
  (custom-set-variables
   '(ns-use-proxy-icon nil)
   '(ns-use-native-fullscreen t)
   '(mac-option-modifier 'meta)
   '(ns-alternate-modifier 'meta)
   '(mac-command-modifier 'none)
   '(ns-command-modifier 'none)
   '(mac-right-option-modifier 'none)
   '(ns-right-alternate-modifier 'none)))

;;; Personal prefix maps key bindings
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

;;; Packages
(use-package ag :if (executable-find "ag"))

(use-package align
  :straight (:type built-in)
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
  :diminish dired-async-mode
  :hook (dired-mode . dired-async-mode))

(use-package auth-source-pass
  :if (executable-find "pass")
  :straight (:type built-in)
  :hook (after-init . auth-source-pass-enable))

(use-package autorevert
  :straight (:type built-in)
  :diminish (auto-revert-mode global-auto-revert-mode)
  :custom
  (auto-revert-avoid-polling t)
  (revert-without-query (list "."))
  :hook (after-init . global-auto-revert-mode)
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

(use-package bibtex
  :config
  (defun cr-bibtex-settings ()
    ;; Fix for bibtex-mode initialization
    ;; https://emacs.stackexchange.com/questions/46691/initialization-of-bibtex-package
    (bibtex-set-dialect 'biblatex))
  :hook (bibtex-mode . cr-bibtex-settings))

(use-package bibtex-actions
  :straight bibtex-actions
  :straight bibtex-completion
  :straight citeproc
  :after (:any org markdown-mode latex-mode python-mode rst-mode)
  :demand
  :custom
  (bibtex-completion-bibliography cr-bibliography)
  (bibtex-actions-bibliography cr-bibliography)
  (bibtex-actions-library-paths (list cr-library))
  (bibtex-actions-notes-paths (list cr-zet-dir))
  (bibtex-actions-default-action 'bibtex-actions-open-notes)
  (bibtex-actions-file-note-org-include '(org-id org-roam-ref))
  (org-cite-global-bibliography cr-bibliography)
  (org-cite-insert-processor 'oc-bibtex-actions)
  (org-cite-follow-processor 'oc-bibtex-actions)
  (org-cite-activate-processor 'oc-bibtex-actions)
  :config
  (with-eval-after-load 'org
    (require 'oc)
    (require 'oc-bibtex-actions))
  (setf (alist-get'org-mode bibtex-completion-format-citation-functions)
        'bibtex-completion-format-citation-org-cite)
  :bind (:map cr-notes-map ("c" . bibtex-actions-insert-citation)))

(use-package browse-url
  :straight (:type built-in)
  :custom
  (browse-url-browser-function
   '(("\\(youtube\\.com/watch\\)\\|\\(youtu\\.be/watch\\)" . browse-url-youtube-mpv)
     ("." . browse-url-default-browser)))
  :config
  (defun browse-url-youtube-mpv (url &rest e)
    "Use `mpv' with `youtube-dl' to open URLs, asking for desired quality
Documentation: https://github.com/ytdl-org/youtube-dl#format-selection"
    (let ((quality-arg "")
          (quality-val (completing-read "Max height resolution (0 for unlimited): "
                                        '("480" "720" "1080" "0") nil nil)))
      (setq quality-val (string-to-number quality-val))
      (message "Opening %s with height≤%s with mpv..." url quality-val)
      (when (< 0 quality-val)
        (setq quality-arg
              (format "--ytdl-format=bestvideo[height<=?%s]+bestaudio" quality-val)))
      (start-process "mpv" nil "mpv" quality-arg url)))
  :bind ("M-g w" . browse-url-at-point))

(use-package calc
  :straight (:type built-in)
  :bind (:map cr-app-map ("c" . calc)))

(use-package calendar
  :straight (:type built-in)
  :custom
  (calendar-weekend-days '(6 0))
  (calendar-week-start-day 1)
  :bind (:map cr-app-map ("!" . calendar)))

(use-package company
  :diminish
  :custom
  (company-idle-delay 0.5)
  (company-minimu-prefix-length 3)
  (company-backends
   '(company-capf
     (company-dabbrev-code company-gtags company-etags company-keywords)
     company-files
     company-dabbrev))
  :hook (after-init . global-company-mode)
  :bind (("M-/"   . company-complete)
         ("C-c y" . company-yasnippet)
         (:map company-active-map ("M-/" . company-other-backend))
         (:map cr-toggle-map ("c" . company-mode))))

(use-package company-prescient
  :after (:all company prescient)
  :hook (company-mode . company-prescient-mode))

(use-package compile
  :straight (:type built-in)
  :bind (:map cr-file-map ("m" . compile)))

(use-package consult
  :custom
  (consult-locate-args "locate --ignore-case --regex")
  (consult-project-root-function #'vc-root-dir)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any))
  :bind (([remap apropos-command] . consult-apropos)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-goto-line)
         ([remap jump-to-register] . consult-register)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ("C-c i" . consult-imenu)
         ("C-c I" . consult-imenu-multi)
         ("C-c j" . consult-git-grep)
         ("C-c k" . consult-ripgrep)
         ("C-c o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g M" . consult-global-mark)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s O" . consult-multi-occur)
         (:map cr-app-map ("m" . consult-man))
         (:map cr-search-map (("f" . consult-find)
                              ("l" . consult-locate)))
         (:map cr-text-map (("f" . consult-focus-lines)
                            ("k" . consult-keep-lines)))
         (:map cr-toggle-map ("T" . consult-theme))))

(use-package consult
  :after org
  :bind (:map org-mode-map ("C-c o" . consult-org-heading)))

(use-package consult-recoll
  :bind (:map cr-search-map ("r" . consult-recoll)))

(use-package cr-focus-mode
  :straight olivetti
  :bind* ("M-O" . cr-focus-mode))

(use-package cr-functions
  :straight nil
  :demand
  :bind (([remap kill-region] . cr-backward-kill-word-or-region)
         ("M-]" . cr-switch-to-last-buffer)
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
               ("H" . cr-grep-app-search)
               ("w" . cr-duckduckgo-search))))

(use-package cr-hide-mode-line-mode
  :straight nil)

(use-package cr-open-external
  :straight nil
  :bind ((:map cr-file-map
               ("x" . cr-open-file-or-directory-in-external-app))
         (:map dired-mode-map
               ("M-RET" . cr-open-file-or-directory-in-external-app))))

(use-package cr-org-gtd
  :straight nil
  :after org
  :demand)

(use-package cr-themes
  :straight nil
  :demand
  :custom
  (cr-themes-light 'modus-operandi)
  (cr-themes-dark 'modus-vivendi)
  (cr-themes-default cr-themes-light)
  :config
  (use-package modus-themes
    :init
    (custom-set-variables
     '(modus-themes-scale-headings t)
     '(modus-themes-slanted-constructs t)
     '(modus-themes-bold-constructs t)
     '(modus-themes-org-blocks 'tinted-background))
    (modus-themes-load-themes))

  (load-theme cr-themes-default t)
  :bind (:map cr-toggle-map ("t" . cr-themes-toggle)))

(use-package css-mode :mode "\\.css\\'")

(use-package csv-mode
  :mode "\\.csv\\'"
  :custom (csv-separators '("," ";" "|" " "))
  :config (add-hook 'csv-mode-hook 'csv-header-line))

(use-package deft
  :custom
  (deft-directory cr-org-dir)
  (deft-recursive t)
  (deft-extensions '("org" "md" "txt"))
  (deft-default-extension "org")
  (deft-use-filename-as-title t)
  :bind (:map cr-notes-map ("d" . deft)))

(use-package dictionary
  :bind (:map cr-search-map ("d" . dictionary-search)))

(use-package diff
  :straight (:type built-in)
  :bind (:map cr-file-map ("d" . diff-buffer-with-file)))

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (delete-by-moving-to-trash t)
  (dired-listing-switches "-lahv")
  (dired-guess-shell-alist-user '(("" "xdg-open")))
  :config
  (put 'dired-find-alternate-file 'disabled nil)

  (defvar cr-dired-sort-base "-lahv")
  (defun cr-dired-sort-by-dir ()
    (interactive)
    ;; Note: default ls from MacOS does not have this option (use ls from coreutils)
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
  (define-key cr-dired-sort-map (kbd "d") 'cr-dired-sort-by-dir)
  (define-key cr-dired-sort-map (kbd "n") 'cr-dired-sort-by-name)
  (define-key cr-dired-sort-map (kbd "t") 'cr-dired-sort-by-time)
  (define-key cr-dired-sort-map (kbd "s") 'cr-dired-sort-by-size)

  (define-key dired-mode-map (kbd "s") 'cr-dired-sort-map)
  (define-key dired-mode-map (kbd "[") 'dired-up-directory)
  (define-key dired-mode-map (kbd "e") 'ediff-files))

(use-package dired-x
  :straight (:type built-in)
  :after dired
  :demand)

(use-package display-line-numbers
  :straight (:type built-in)
  :bind (:map cr-toggle-map ("l" . display-line-numbers-mode)))

(use-package dockerfile-mode :mode ("Dockerfile\\'"))

(use-package doom-sudo-utils
  :straight nil
  :bind (:map cr-file-map
              ("u" . doom/sudo-this-file)
              ("U" . doom/sudo-save-buffer)))

(use-package ediff
  :straight (:type built-in)
  :commands (ediff ediff-buffers ediff-files magit-ediff-dwim)
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  :config
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'outline-show-all))
  (with-eval-after-load 'org
    (add-hook 'ediff-prepare-buffer-hook #'org-show-all))
  :bind (:map cr-buffer-map
              ("d" . ediff-buffers)
              ("D" . ediff-show-registry)))

(use-package eldoc
  :straight (:type built-in)
  :diminish)

(use-package elfeed
  :init
  (require 'cr-private-feeds nil 'noerror)
  :custom
  (elfeed-search-filter "@2-weeks-ago +unread ")
  (elfeed-search-date-format '("%m-%d" 5 :left))
  (elfeed-search-title-max-width 100)
  (elfeed-show-entry-switch 'display-buffer)
  :config
  (defun cr-elfeed-show-settings ()
    (setq-local shr-width fill-column
                shr-max-image-proportion 0.7
                line-spacing 0.2))
  :hook (elfeed-show-mode . cr-elfeed-show-settings)
  :bind ((:map cr-app-map ("f" . elfeed))
         (:map elfeed-search-mode-map ("a" . elfeed-search-show-entry))
         (:map elfeed-show-mode-map
               ("n" . next-line)
               ("p" . previous-line))))

(use-package elisp-mode
  :straight (:type built-in)
  :config
  (defun cr-emacs-lisp-settings ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (add-hook 'emacs-lisp-mode-hook 'cr-emacs-lisp-settings))

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult)
  :demand)

(use-package eshell
  :straight (:type built-in)
  :custom
  (eshell-ls-use-colors t)
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  (eshell-destroy-buffer-when-process-dies nil)
  (eshell-visual-commands
   '("crontab" "tmux" "htop" "tail" "vi" "screen" "top" "less" "more"))
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
  (defun cr-eshell-settings ()
    (company-mode -1)
    (hl-line-mode -1)
    (setenv "PAGER""cat")
    (with-eval-after-load 'counsel
      (define-key eshell-mode-map (kbd "M-r") 'counsel-esh-history)))

  (add-hook 'eshell-mode-hook 'cr-eshell-settings)
  :bind (:map cr-app-map ("e" . eshell)))

(use-package eww
  :straight (:type built-in)
  :bind (:map cr-app-map ("w" . eww)))

(use-package face-remap
  :straight (:type built-in)
  :diminish buffer-face-mode
  :config
  (custom-theme-set-faces
   'user
   '(org-block                 ((t (:inherit fixed-pitch))))
   '(org-block-begin-line      ((t (:inherit fixed-pitch))))
   '(org-block-end-line        ((t (:inherit fixed-pitch))))
   '(org-checkbox              ((t (:inherit fixed-pitch))))
   '(org-code                  ((t (:inherit (shadow fixed-pitch)))))
   '(org-date                  ((t (:inherit fixed-pitch))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-done                  ((t (:inherit fixed-pitch))))
   '(org-formula               ((t (:inherit fixed-pitch))))
   '(org-indent                ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link                  ((t (:inherit (fixed-pitch) :underline t))))
   '(org-meta-line             ((t (:inherit fixed-pitch))))
   '(org-property-value        ((t (:inherit fixed-pitch))))
   '(org-special-keyword       ((t (:inherit fixed-pitch))))
   '(org-table                 ((t (:inherit fixed-pitch))))
   '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold))))
   '(org-todo                  ((t (:inherit fixed-pitch))))
   '(org-verbatim              ((t (:inherit fixed-pitch))))

   '(markdown-code-face               ((t (:inherit fixed-pitch))))
   '(markdown-html-attr-name-face     ((t (:inherit fixed-pitch))))
   '(markdown-html-attr-value-face    ((t (:inherit fixed-pitch))))
   '(markdown-html-entity-face        ((t (:inherit fixed-pitch))))
   '(markdown-html-tag-delimiter-face ((t (:inherit fixed-pitch))))
   '(markdown-html-tag-name-face      ((t (:inherit fixed-pitch))))
   '(markdown-inline-code-face        ((t (:inherit fixed-pitch))))
   '(markdown-language-info-face      ((t (:inherit fixed-pitch))))
   '(markdown-language-keyword-face   ((t (:inherit fixed-pitch))))
   '(markdown-pre-face                ((t (:inherit fixed-pitch))))
   '(markdown-table-face              ((t (:inherit fixed-pitch)))))

  (defun variable-pitch-settings ()
    (if buffer-face-mode
        (setq-local company-idle-delay nil
                    cursor-type 'bar)
      (progn
        (kill-local-variable 'company-idle-delay)
        (kill-local-variable 'cursor-type))))

  (add-hook 'buffer-face-mode-hook 'variable-pitch-settings)

  (defun variable-pitch-mode-avoid-org-agenda-files ()
    "Enable `variable-pitch-mode' in Org buffers, but not in the
main agenda files, which contains many tags. I want those to
remain in fixed pitch for the tags to be aligned."
    (when buffer-file-name
      (unless (or (and (member buffer-file-name (org-agenda-files))
                       (not (string-match ".*journal.*.org$" buffer-file-name)))
                  (string-match ".*.org_archive$" buffer-file-name))
        (variable-pitch-mode 1))))

  :hook
  (markdown-mode . variable-pitch-mode)
  (org-mode . variable-pitch-mode-avoid-org-agenda-files)
  :bind (:map cr-toggle-map ("p" . variable-pitch-mode)))

(use-package ffap
  :straight (:type built-in)
  :bind ("M-g f" . find-file-at-point))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :bind (:map cr-toggle-map
              ("f" . flycheck-mode)
              ("F" . global-flycheck-mode)))

(use-package flyspell
  :diminish
  :custom
  (ispell-silently-savep t)
  (ispell-personal-dictionary "~/nextcloud/resources/dictionary")
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil)
  :config
  (defun cr-save-word-to-pdict ()
    "Save word at point to the personal dictionary"
    (interactive)
    (let ((current-location (point))
          (word (flyspell-get-word)))
      (when (consp word)
        (flyspell-do-correct 'save nil (car word)
                             current-location (cadr word) (caddr word)
                             current-location))))

  :hook ((org-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :bind ((:map flyspell-mode-map ("C-." . nil))
         (:map cr-toggle-map ("z" . flyspell-mode))
         (:map cr-spell-map
               ("." . flyspell-auto-correct-word)
               ("b" . flyspell-buffer)
               ("d" . ispell-change-dictionary)
               ("r" . flyspell-region)
               ("s" . cr-save-word-to-pdict)
               ("z" . flyspell-correct-wrapper))))

(use-package flyspell
  :if (executable-find "hunspell")
  :init
  (defvar cr-ispell-lang1 "english")
  (defvar cr-ispell-lang2 "francais")
  (defvar cr-ispell-lang-multi (concat cr-ispell-lang1 "," cr-ispell-lang2))
  :custom (ispell-program-name "hunspell")
  :config
  (setq ispell-dictionary cr-ispell-lang-multi)
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic cr-ispell-lang-multi)

  (defun cr-ispell-set-lang1 ()
    (interactive)
    (ispell-change-dictionary cr-ispell-lang1))

  (defun cr-ispell-set-lang2 ()
    (interactive)
    (ispell-change-dictionary cr-ispell-lang2))

  (defun cr-ispell-set-MULTI ()
    (interactive)
    (ispell-hunspell-add-multi-dic cr-ispell-lang-multi)
    (ispell-change-dictionary cr-ispell-lang-multi))

  :bind (:map cr-spell-map
              ("1" . cr-ispell-set-lang1)
              ("2" . cr-ispell-set-lang2)
              ("m" . cr-ispell-set-MULTI)))

(use-package follow
  :straight (:type built-in)
  :bind (:map cr-toggle-map ("=" . follow-delete-other-windows-and-split)))

(use-package forge
  :disabled
  :after magit
  :commands forge-pull-notifications)

(use-package go-mode :mode "\\.go\\'")

(use-package git-gutter
  :diminish
  :bind ((:map cr-toggle-map
               ("g" . git-gutter-mode))
         (:map cr-git-map
               ("[" . git-gutter:previous-hunk)
               ("]" . git-gutter:next-hunk)
               ("r" . git-gutter:revert-hunk)
               ("s" . git-gutter:stage-hunk)
               ("SPC" . git-gutter:mark-hunk))))

(use-package git-link
  :bind (:map cr-git-map ("y" . git-link)))

(use-package git-modes)

(use-package git-timemachine
  :bind (:map cr-git-map ("t" . git-timemachine)))

(use-package hl-line
  :straight (:type built-in)
  :hook ((text-mode
          prog-mode
          dired-mode
          elfeed-show-mode
          occur-mode
          org-agenda-mode
          display-time-world-mode
          profiler-report-mode) . hl-line-mode)
  :bind (:map cr-toggle-map
              ("h" . hl-line-mode)
              ("H" . global-hl-line-mode)))

(use-package htmlize)

(use-package ibuffer
  :straight (:type built-in)
  :config (add-hook 'ibuffer-mode-hook 'hl-line-mode)
  :bind (("C-x C-b" . ibuffer)
         (:map ibuffer-mode-map
               ("M-o")
               ("a" . ibuffer-visit-buffer))))

(use-package iedit
  :bind ("M-i" . iedit-mode))

(use-package indent
  :straight (:type built-in)
  :bind (:map indent-rigidly-map
              (">" . indent-rigidly-right)
              ("<" . indent-rigidly-left)
              ("C->" . indent-rigidly-right-to-tab-stop)
              ("C-<" . indent-rigidly-left-to-tab-stop)))

(use-package kubernetes
  :if (executable-find "kubectl")
  :bind (:map cr-app-map ("k" . kubernetes-overview)))

(use-package langtool
  :custom
  (langtool-language-tool-jar
   (shell-command-to-string
    "echo -n $(locate languagetool-commandline.jar | sort -r | head -1)"))
  (langtool-default-language 'auto)
  (langtool-disabled-rules '("DASH_RULE" "WHITESPACE_RULE" "EN_UNPAIRED_BRACKETS"
                             "COMMA_PARENTHESIS_WHITESPACE" "EN_QUOTES"
                             "MORFOLOGIK_RULE_EN_GB" "MORFOLOGIK_RULE_US"))
  :bind (:map cr-grammar-map
              ("v" . langtool-check)
              ("c" . langtool-correct-buffer)
              ("l" . langtool-switch-default-language)
              ("n" . langtool-goto-next-error)
              ("p" . langtool-goto-previous-error)
              ("q" . langtool-check-done)))

(use-package lsp-mode
  :custom
  (lsp-auto-configure t)
  (lsp-auto-guess-root t)
  (lsp-keymap-prefix "C-c u")
  (lsp-idle-delay 0.5)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :bind (:map cr-toggle-map ("u" . lsp)))

(use-package lsp-mode
  :if (executable-find "clang")
  :custom (lsp-clients-clangd-args '("--header-insertion=never"))
  :hook (c-mode-common . lsp-deferred))

(use-package lsp-mode
  :if (executable-find "gopls")
  :hook (go-mode . lsp-deferred))

(use-package lsp-mode
  :if (executable-find "npm")
  :hook ((dockerfile-mode
          html-mode
          php-mode
          sh-mode) . lsp-deferred))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-imenu-enable t)
  (lsp-ui-peek-enable nil)
  (lsp-ui-sideline-enable nil))

(use-package magit
  :bind (:map cr-git-map
              ("b" . magit-branch-checkout)
              ("B" . magit-blame)
              ("c" . magit-clone)
              ("g" . magit-status)
              ("p" . magit-pull-from-upstream)))

(use-package man
  :bind (:map cr-app-map ("m" . man)))

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :custom (markdown-fontify-code-blocks-natively t))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package nginx-mode)

(use-package nov
  :custom (nov-text-width 70)
  :config
  (defun cr-nov-settings ()
    (face-remap-add-relative 'variable-pitch
                             :family "Georgia"
                             :height 1.0)
    (setq-local line-spacing 0.2)
    (hl-line-mode 1))
  (add-hook 'nov-mode-hook 'cr-nov-settings)
  :mode ("\\.\\(epub\\|mobi\\)\\'" . nov-mode)
  :bind (:map nov-mode-map
              ("n" . next-line)
              ("p" . previous-line)))

(use-package ob-async
  :after org
  :demand)

(use-package olivetti
  :diminish
  :custom
  (olivetti-body-width (+ fill-column 30))
  (olivetti-minimum-body-width fill-column)
  :bind (:map cr-toggle-map ("O" . olivetti-mode)))

(use-package org
  :custom
  (org-adapt-indentation nil)
  (org-agenda-follow-indirect nil)
  (org-agenda-log-mode-items '(closed clock))
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-show-outline-path nil)
  (org-agenda-skip-deadline-if-done nil)
  (org-agenda-skip-deadline-prewarning-if-scheduled nil)
  (org-agenda-skip-scheduled-if-done nil)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-todo-ignore-scheduled 'future)
  (org-agenda-window-setup 'current-window)
  (org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
  (org-catch-invisible-edits 'show)
  (org-clock-continuously nil)
  (org-clock-idle-time nil)
  (org-clock-in-resume nil)
  (org-clock-in-switch-to-state nil)
  (org-clock-persist t)
  (org-clock-report-include-clocking-task t)
  (org-clock-sound t)
  (org-confirm-babel-evaluate nil)
  (org-ctrl-k-protect-subtree t)
  (org-deadline-warning-days 14)
  (org-directory cr-org-dir)
  (org-ellipsis " ▼")
  (org-fontify-done-headline t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-imenu-depth 3)
  (org-link-file-path-type 'adaptive)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-outline-path 'file)
  (org-show-notification-handler 'message)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-startup-folded nil)
  (org-use-speed-commands t)
  :config
  (with-eval-after-load 'ox (require 'ox-md nil 'noerror))

  (setq system-time-locale "C")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (C . t)))

  (setq org-babel-default-header-args '((:results  . "replace output")))

  (org-clock-persistence-insinuate)

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

  (add-to-list 'org-speed-commands '("w" call-interactively 'cr-org-refile-in-current-buffer))
  (add-to-list 'org-speed-commands '("W" call-interactively 'cr-org-refile-in-any-buffer))

  (defun cr-org-pretty-symbols ()
    (push '("#+begin_src"      . ?↦) prettify-symbols-alist)
    (push '("#+end_src"        . ?⇤) prettify-symbols-alist)
    (push '("#+begin_quote"    . ?↦) prettify-symbols-alist)
    (push '("#+end_quote"      . ?⇤) prettify-symbols-alist)
    (push '("#+begin_example"  . ?↦) prettify-symbols-alist)
    (push '("#+end_example"    . ?⇤) prettify-symbols-alist)
    (prettify-symbols-mode 1))

  (add-hook 'org-mode-hook 'cr-org-pretty-symbols)

  :mode ("\\.org_archive\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c C-x C-o" . org-clock-out)
         ("C-c C-x C-x" . org-clock-in-last)
         ("C-c C-x C-j" . org-clock-goto)
         ("C-c C-x C-q" . org-clock-cancel)))

(use-package org-cliplink
  :bind (:map cr-notes-map ("l" . org-cliplink)))

(use-package org-download
  :after org
  :custom
  (org-download-method 'directory) ;; org-download-delete does not work with 'attach
  (org-download-image-dir "images")
  (org-download-image-org-width 400)
  (org-download-heading-lvl nil)
  :bind (:map cr-notes-map
              ("y" . org-download-yank)
              ("Y" . org-download-screenshot))
  :hook ((org-mode dired-mode) . org-download-enable))

(use-package org-indent
  :straight (:type built-in)
  :diminish
  :custom (org-indent-indentation-per-level 1)
  :hook (org-mode . org-indent-mode))

(use-package org-noter
  :after (:any org pdf-view)
  :custom
  (org-noter-notes-search-path (list (expand-file-name "notes/" org-directory)))
  (org-noter-default-notes-file-names '("notes.org" "main.org"))
  (org-noter-auto-save-last-location t)
  (org-noter-always-create-frame nil)
  (org-noter-kill-frame-at-session-end nil)
  :bind ("C-c w n" . org-noter))

(use-package org-roam
  :if (executable-find "sqlite3")
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename (expand-file-name "zet/" org-directory)))
  (org-roam-db-gc-threshold (* 100 1024 1024))
  (org-id-link-to-org-use-id t)
  :config (org-roam-db-autosync-enable)
  :bind (:map cr-notes-map
              ("b" . org-roam-buffer)
              ("f" . org-roam-node-find)
              ("i" . org-roam-node-insert)
              ("n" . org-roam-capture)
              ("r" . org-roam-node-random)
              ("t" . org-roam-buffer-toggle)))

(use-package outline
  :straight (:type built-in)
  :diminish outline-minor-mode
  :hook (prog-mode . outline-minor-mode)
  :bind (:map cr-toggle-map ("o" . outline-minor-mode)))

(use-package outline-magic
  :bind (:map outline-minor-mode-map ("<C-tab>" . outline-cycle)))

(use-package pass
  :custom (pass-show-keybindings nil)
  :bind (:map cr-app-map ("P" . pass)))

(use-package password-cache
  :straight (:type built-in)
  :demand
  :custom
  (password-cache 5)
  (password-cache-expiry 600))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :init (pdf-tools-install t t t)
  :custom
  (pdf-view-use-scaling t)
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'pdf-view-mode-hook 'pdf-annot-minor-mode)
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "d") 'pdf-annot-delete)
  (define-key pdf-view-mode-map (kbd "n") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "p") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "M-n") 'pdf-view-next-page-command)
  (define-key pdf-view-mode-map (kbd "M-p") 'pdf-view-previous-page-command))

(use-package php-mode :mode ("\\.php\\'"))

(use-package prescient :hook (after-init . prescient-persist-mode))

(use-package proced
  :straight (:type built-in)
  :custom
  (proced-auto-update-flag nil)
  (proced-auto-update-interval 5)
  :bind ((:map cr-app-map ("x" . proced))
         (:map proced-mode-map ("a" . proced-toggle-auto-update))))

(use-package prog-mode
  :straight (:type built-in)
  :custom (prettify-symbols-unprettify-at-point 'right-edge)
  :config
  (defun cr-prog-mode-settings ()
    (setq-local indicate-empty-lines t
                indicate-buffer-boundaries t))
  (add-hook 'prog-mode-hook 'cr-prog-mode-settings))

(use-package project)

(use-package python-mode)

(use-package recentf
  :straight (:type built-in)
  :custom
  (recentf-max-saved-items 100)
  (recentf-keep '(file-remote-p file-readable-p))
  :config
  (add-to-list 'recentf-exclude "COMMIT_MSG")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG")
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  (with-eval-after-load 'no-littering
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  :hook (after-init . recentf-mode))

(use-package replace
  :straight (:type built-in)
  :bind ((:map occur-mode-map
               ("n" . next-line)
               ("p" . previous-line))
         (:map cr-text-map
               ;; ("f" . flush-lines)
               ;; ("k" . keep-lines)
               ("r" . query-replace)
               ("R" . query-replace-regexp))))

(use-package restclient
  :straight restclient
  :straight ob-restclient
  :mode ("\\.http\\'" . restclient-mode)
  :bind (:map restclient-mode-map ("C-c n n" . nil)))

(use-package rg
  :if (executable-find "rg")
  :config
  (rg-define-search rg-org
    :query ask
    :format regexp
    :files "org"
    :dir cr-org-dir
    :flags ("--ignore-case")
    :menu ("Custom" "o" "Org"))
  :bind ((:map cr-search-map ("g" . rg-menu))
         (:map cr-notes-map ("g" . rg-org))))

(use-package rust-mode)

(use-package savehist
  :straight (:type built-in)
  :demand
  :custom
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  :config
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (add-to-list 'savehist-additional-variables 'kmacro-ring)
  (add-to-list 'savehist-additional-variables 'last-kbd-macro)
  (add-to-list 'savehist-additional-variables 'regexp-search-ring)
  (add-to-list 'savehist-additional-variables 'register-alist)
  (add-to-list 'savehist-additional-variables 'compilation-command)
  :hook (after-init . savehist-mode))

(use-package saveplace
  :straight (:type built-in)
  :hook (after-init . save-place-mode))

(use-package selectrum
  :hook (after-init . selectrum-mode)
  :bind ("C-c r" . selectrum-repeat))

(use-package selectrum-prescient
  :hook (selectrum-mode . selectrum-prescient-mode))

(use-package server
  :straight (:type built-in)
  :demand
  :config (unless (server-running-p)(server-start)))

(use-package simple
  :straight (:type built-in)
  :diminish (visual-line-mode auto-fill-function)
  :custom
  (delete-trailing-lines t)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (set-mark-command-repeat-pop t)
  :bind (([remap comment-dwim]    . cr-comment-dwim)
         ([remap upcase-word]     . upcase-dwim)
         ([remap downcase-word]   . downcase-dwim)
         ([remap capitalize-word] . capitalize-dwim)
         ([remap zap-to-char]     . zap-up-to-char)
         (:map cr-emacs-map  ("x" . list-processes))
         (:map cr-text-map   ("d" . delete-trailing-whitespace))
         (:map cr-toggle-map
               ("C" . column-number-mode)
               ("v" . visual-line-mode)
               ("V" . toggle-truncate-lines)
               ("q" . auto-fill-mode)))
  :hook
  ((org-mode markdown-mode) . visual-line-mode)
  ((org-mode markdown-mode) . auto-fill-mode))

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  (defun cr-smartparens-settings ()
    (electric-pair-mode -1)
    (show-smartparens-mode 1))
  (add-hook 'smartparens-mode-hook 'cr-smartparens-settings)
  :hook (prog-mode . smartparens-mode)
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

(use-package so-long
  :if (>= emacs-major-version 27)
  :straight (:type built-in)
  :hook (after-init . global-so-long-mode))

(use-package sort
  :straight (:type built-in)
  :bind (:map cr-text-map ("s" . sort-lines)))

(use-package sql
  :config
  (require 'cr-private-sql nil 'noerror)
  (defun cr-isql-config ()
    (setq-local truncate-lines t))
  (add-hook 'sql-interactive-mode-hook 'cr-isql-config)
  :bind ((:map cr-app-map ("S" . sql-connect))))

(use-package ssh-config-mode)

(use-package startup
  :straight (:type built-in)
  :custom
  (initial-major-mode 'lisp-interaction-mode)
  (initial-scratch-message nil)
  (inhibit-startup-screen t))

(use-package terraform-mode :mode "\.tf\\'")

(use-package time
  :straight (:type built-in)
  :custom
  (display-time-24hr-format t)
  (display-time-default-load-average nil)
  :bind ((:map cr-toggle-map ("." . display-time-mode))
         (:map cr-app-map ("." . display-time-world))))

(use-package timer-list
  :straight (:type built-in)
  :bind (:map cr-emacs-map ("l" . list-timers)))

(use-package toml-mode)

(use-package tramp
  :straight (:type built-in)
  :custom
  (tramp-shell-prompt-pattern
   "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))

(use-package unfill
  :bind ([remap fill-paragraph] . 'unfill-toggle))

(use-package url-vars
  :straight (:type built-in)
  :custom (url-privacy-level 'high)
  :config  (url-setup-privacy-info))

(use-package vlf
  :init (require 'vlf-setup))

(use-package vterm
  :init (setq vterm-always-compile-module t)
  :custom (vterm-max-scrollback (* 20 1000))
  :bind (:map cr-app-map ("v" . vterm)))

(use-package wdired
  :straight (:type built-in)
  :after dired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-allow-to-redirect-links t)
  (wdired-create-parent-directories t))

(use-package winner
  :straight (:type built-in)
  :hook (after-init . winner-mode)
  :bind (("C-x u" . winner-undo)
         ("C-x U" . winner-redo)))

(use-package which-key
  :diminish
  :hook (after-init . which-key-mode)
  :bind (:map cr-toggle-map ("?" . which-key-mode)))

(use-package whitespace
  :straight (:type built-in)
  :bind (:map cr-toggle-map ("SPC" . whitespace-mode)))

(use-package window
  :straight (:type built-in)
  :bind ("M-o" . other-window))

(use-package ws-butler
  :diminish
  :hook (prog-mode . ws-butler-mode))

(use-package yaml-mode :mode ("\\.ya?ml\\'"))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))
