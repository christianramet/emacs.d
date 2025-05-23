;;; init.el --- My main configuration file
;; -*- lexical-binding: t -*-

;;; Code:
;;; Commentary:

;;; System tuning
(setq gc-cons-threshold (* 20 1024 1024)
      read-process-output-max (* 3 1024 1024))

;;; Variables and constants
(defconst cr-git-dir "~/git/")

(defconst cr-data-dir "~/Nextcloud/")
(defconst cr-work-dir "~/digidrive/")
(defconst cr-org-dir (expand-file-name "org" cr-data-dir))
(defconst cr-ref-dir (expand-file-name "ref" cr-org-dir))
(defconst cr-library (expand-file-name "library" cr-data-dir))
(defconst cr-dictionary (expand-file-name "resources/dictionary" cr-data-dir))

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
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(eval-when-compile
  (straight-use-package 'use-package)
  (require 'use-package))

;;; Early packages
(straight-use-package 'org)
(straight-use-package 'f)

(use-package exec-path-from-shell
  :demand
  :if (or (daemonp) (display-graphic-p))
  :config (exec-path-from-shell-initialize))

(use-package diminish :demand)

(use-package no-littering
  :demand
  :custom
  (custom-file (no-littering-expand-var-file-name "custom.el"))
  :config
  (setq backup-directory-alist
        `(("\\`/tmp/" . nil)
          ("\\`/dev/shm/" . nil)
          ("." . ,(no-littering-expand-var-file-name "backup/"))))
  (setq auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
           ,(concat temporary-file-directory "\\2") t)
          ("\\`\\(/tmp\\|/dev/shm\\)\\([^/]*/\\)*\\(.*\\)\\'" "\\3")
          (".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;;; Better defaults
(prefer-coding-system 'utf-8)

(setq-default fill-column 80
              indicate-empty-lines nil
              indicate-buffer-boundaries nil
              indent-tabs-mode nil
              tab-width 4)

(custom-set-variables
 '(disabled-command-function nil)
 '(ring-bell-function 'ignore)
 '(sentence-end-double-space nil)
 '(vc-follow-symlinks t)
 '(create-lockfiles nil)
 '(uniquify-buffer-name-style 'forward)
 '(use-dialog-box nil)
 '(use-short-answers t)
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
   '(mac-option-modifier 'meta)
   '(mac-command-modifier nil)
   ;; disable `mac-right-option-modifier' with external keyboards on MacOS
   ;; because of a bug: inverted left/right keys
   '(mac-right-option-modifier nil)))

;;; Personal prefix maps key bindings
(define-prefix-command 'cr-app-map)
(define-prefix-command 'cr-buffer-map)
(define-prefix-command 'cr-emacs-map)
(define-prefix-command 'cr-file-map)
(define-prefix-command 'cr-git-map)
(define-prefix-command 'cr-note-map)
(define-prefix-command 'cr-text-map)
(define-prefix-command 'cr-toggle-map)
(define-prefix-command 'cr-spell-map)

(bind-keys ("C-c w" . cr-app-map)
           ("C-c b" . cr-buffer-map)
           ("C-c e" . cr-emacs-map)
           ("C-c f" . cr-file-map)
           ("C-c g" . cr-git-map)
           ("C-c n" . cr-note-map)
           ("C-c t" . cr-toggle-map)
           ("C-c x" . cr-text-map)
           ("C-c z" . cr-spell-map))

;;; Packages
(use-package abbrev
  :straight (:type built-in)
  :diminish)

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

(use-package apache-mode)

(use-package async
  :hook (dired-mode . dired-async-mode))

(use-package auth-source-pass
  :if (executable-find "pass")
  :straight (:type built-in)
  :init (auth-source-pass-enable))

(use-package autorevert
  :straight (:type built-in)
  :diminish (auto-revert-mode global-auto-revert-mode)
  :custom
  (auto-revert-avoid-polling t)
  (revert-without-query (list "."))
  :init (global-auto-revert-mode)
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
  :straight (:type built-in)
  :custom
  (bibtex-include-OPTkey nil)
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-year-title-separator nil)
  (bibtex-autokey-titleword-length 10)
  (bibtex-entry-format '(opts-or-alts required-fields numerical-fields
                                      last-comma sort-fields whitespace))
  :config
  (defun cr-bibtex-settings ()
    ;; Fix for bibtex-mode initialization
    ;; https://emacs.stackexchange.com/questions/46691/initialization-of-bibtex-package
    (bibtex-set-dialect 'BibTeX))
  :hook (bibtex-mode . cr-bibtex-settings))

(use-package bookmark
  :straight (:type built-in)
  :custom
  (bookmark-save-flag 1)
  (bookmark-set-fringe-mark nil))

(use-package browse-url
  :straight (:type built-in)
  :custom
  (browse-url-handlers
   '(("\\(youtube\\.com/watch\\)\\|\\(youtu\\.be/watch\\)" . browse-url-youtube-mpv)
     ("." . eww-browse-url)))
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

(use-package citar
  :custom
  (citar-bibliography (directory-files cr-library t ".*.bib"))
  (citar-library-paths (list cr-library))
  (citar-notes-paths (list cr-ref-dir))
  (citar-file-note-extensions '("org" "md" "txt"))
  :bind (:map cr-note-map
              ("c" . citar-open)
              ("F" . citar-add-file-to-library)))

(use-package citar
  :after org
  :custom
  (org-cite-global-bibliography (directory-files cr-library t ".*.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))

(use-package citar-embark
  :demand
  :diminish
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package consult
  :custom (consult-project-root-function #'vc-root-dir)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-bookmark consult-buffer consult-ripgrep consult-theme
   :preview-key '(:debounce 0.5 any))

  (defun consult-org-rg ()
    "Ripgrep into `org-directory'"
    (interactive)
    (require 'org)
    (let ((default-directory org-directory)
          ;; follow in case work files are symlinked from somewhere else
          (consult-ripgrep-args (concat consult-ripgrep-args " --follow")))
      (consult-ripgrep)))

  (defun consult-find-home ()
    "Call `consult-find' using HOME as the `default-directory'"
    (interactive)
    (let ((default-directory "~/"))
      (consult-find)))

  (defun consult-find-org ()
    "Call `consult-find' in `org-directory''"
    (interactive)
    (require 'org)
    (let ((default-directory org-directory)
          (consult-find-args "find -L ."))
      (consult-find)))

  (defun consult-rg-org-backlinks ()
    "Call `consult-ripgrep' using current parent directory as the `default-directory',
and the current node ID as the search pattern"
    (interactive)
    (consult-ripgrep (file-name-directory (directory-file-name default-directory))
                     (concat "id:" (org-id-get))))

  (defvar consult--source-eshell
    `(:name     "eshell"
      :narrow   ?e
      :category buffer
      :face     consult-buffer
      :history  buffer-name-history
      :action   ,#'consult--buffer-action
      :items ,(lambda () (consult--buffer-query :mode 'eshell-mode
                                                :as #'buffer-name)))
    "eshell candidate source for `consult-buffer'.")

  (defvar consult--source-vterm
    `(:name     "vterm"
      :narrow   ?v
      :category buffer
      :face     consult-buffer
      :history  buffer-name-history
      :action   ,#'consult--buffer-action
      :items ,(lambda () (consult--buffer-query :mode 'vterm-mode
                                                :as #'buffer-name)))
    "vterm candidate source for `consult-buffer'.")

  (add-to-list 'consult-buffer-sources 'consult--source-eshell 'append)
  (add-to-list 'consult-buffer-sources 'consult--source-vterm 'append)

  :bind (([remap bookmark-jump] . consult-bookmark)
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
         ("C-c r" . consult-history)
         ("M-g e" . consult-flymake)
         ("M-g SPC" . consult-mark)
         ("M-g M-SPC" . consult-global-mark)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s O" . consult-multi-occur)
         ("M-s SPC"   . consult-find)
         ("M-s M-SPC" . consult-find-home)
         ("M-s /" . consult-locate)
         (:map cr-note-map (("r" . consult-org-rg)
                            ("n" . consult-find-org)
                            ("b" . consult-rg-org-backlinks)))
         (:map cr-app-map ("m" . consult-man))
         (:map cr-text-map (("f" . consult-focus-lines)
                            ("k" . consult-keep-lines)))
         (:map cr-toggle-map ("T" . consult-theme))))

(use-package consult
  :if system-is-linux-p
  :custom (consult-locate-args "locate --ignore-case --regex"))

(use-package consult
  :if system-is-osx-p
  :custom (consult-locate-args "locate -i"))

(use-package consult-recoll
  :bind ("M-s r" . consult-recoll))

(use-package corfu
  :custom (corfu-auto t)
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode)))
  :init (global-corfu-mode))

(use-package cr-focus-mode
  :straight olivetti
  :bind* ("M-O" . cr-focus-mode))

(use-package cr-functions
  :straight nil
  :demand
  :bind (([remap kill-region] . cr-backward-kill-word-or-region)
         ("M-s M-g" . cr-github-search)
         ("M-s M-G" . cr-grep-app-search)
         ("M-s M-b" . cr-duckduckgo-search)
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
               ("t" . cr-test-emacs-config))))

(use-package cr-hide-mode-line-mode
  :straight nil
  :bind (:map cr-toggle-map ("m" . cr-hide-mode-line-mode)))

(use-package cr-open-external
  :straight nil
  :bind ((:map cr-file-map
               ("x" . cr-open-file-or-directory-in-external-app))
         (:map dired-mode-map
               ("<C-return>" . cr-open-file-or-directory-in-external-app))))

(use-package cr-themes
  :straight modus-themes
  :demand
  :custom
  (cr-themes-light 'modus-operandi-tinted)
  (cr-themes-dark 'modus-vivendi-tinted)
  (cr-themes-default cr-themes-light)
  (leuven-scale-outline-headlines nil)
  (leuven-scale-org-agenda-structure nil)
  (modus-themes-org-blocks 'gray-background)
  :config (load-theme cr-themes-default t)
  :bind (:map cr-toggle-map ("t" . cr-themes-toggle)))

(use-package css-mode :mode "\\.css\\'")

(use-package csv-mode
  :mode "\\.csv\\'"
  :custom (csv-separators '("," ";" "|" " "))
  :config (add-hook 'csv-mode-hook 'csv-header-line))

(use-package dictionary
  :if (not system-is-osx-p)
  :bind ("M-s d" . dictionary-search))

(use-package osx-dictionary
  :if system-is-osx-p
  :bind ("M-s d" . osx-dictionary-search-word-at-point))

(use-package diff
  :straight (:type built-in)
  :bind (:map cr-file-map ("d" . diff-buffer-with-file)))

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-auto-revert-buffer t)
  ;; (dired-dwim-target t) ;; use M-n when copying during path selection instead
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (delete-by-moving-to-trash t)
  (dired-listing-switches "-lahv")
  :config
  (when system-is-linux-p
    (customize-set-variable 'dired-guess-shell-alist-user '(("" "xdg-open"))))
  (when system-is-osx-p
    (customize-set-variable 'dired-guess-shell-alist-user '(("" "open"))))

  (defvar cr-dired-sort-base "-lahv")
  (defun cr-dired-sort-by-name ()
    (interactive)
    (dired-sort-other (concat cr-dired-sort-base "")))
  (defun cr-dired-sort-by-size ()
    (interactive)
    (dired-sort-other (concat cr-dired-sort-base " -S")))
  (defun cr-dired-sort-by-time ()
    (interactive)
    (dired-sort-other (concat cr-dired-sort-base " -t")))

  (when (or system-is-linux-p
            (and (executable-find "gls")
                 (setq insert-directory-program "gls")))
    (customize-set-variable 'dired-listing-switches "-lahv --group-directories-first")
    (defun cr-dired-sort-by-dir ()
      (interactive)
      (dired-sort-other (concat cr-dired-sort-base " --group-directories-first"))))

  (define-prefix-command 'cr-dired-sort-map)
  (when system-is-linux-p (define-key cr-dired-sort-map (kbd "d") 'cr-dired-sort-by-dir))
  (define-key cr-dired-sort-map (kbd "n") 'cr-dired-sort-by-name)
  (define-key cr-dired-sort-map (kbd "t") 'cr-dired-sort-by-time)
  (define-key cr-dired-sort-map (kbd "s") 'cr-dired-sort-by-size)

  (define-key dired-mode-map (kbd "s") 'cr-dired-sort-map)
  (define-key dired-mode-map (kbd "[") 'dired-up-directory)

  (defun cr-ediff-marked-files ()
    "Source: https://oremacs.com/2017/03/18/dired-ediff/"
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      (lambda ()
                        (setq ediff-after-quit-hook-internal nil)
                        (set-window-configuration wnd))))
        (error "No more than 2 files should be marked"))))

  (define-key dired-mode-map (kbd "e") 'cr-ediff-marked-files)

  :hook (dired-mode . dired-hide-details-mode))

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
              ("D" . ediff-show-registry)
              ("v" . ediff-revision)))

(use-package eglot
  :bind ((:map cr-toggle-map ("." . eglot))
         (:map eglot-mode-map)
         ("C-c . =" . eglot-format)
         ("C-c . a" . eglot-code-actions)
         ("C-c . o" . eglot-code-action-organize-imports)
         ("C-c . q" . eglot-code-action-quickfix)
         ("C-c . r" . eglot-rename)
         ("C-c . R" . eglot-reconnect)
         ("C-c . x" . eglot-shutdown)
         ("C-c . X" . eglot-shutdown-all)))

(use-package eldoc :diminish
  :init (global-eldoc-mode))

(use-package elfeed
  :init (require 'cr-private-feeds nil 'noerror)
  :custom
  (elfeed-search-filter "@2-weeks-ago +unread")
  (elfeed-search-date-format '("%m-%d" 5 :left))
  (elfeed-search-title-max-width 100)
  (elfeed-show-entry-switch #'switch-to-buffer)
  :config
  (defun cr-elfeed-show-settings ()
    (setq-local shr-width fill-column
                shr-max-image-proportion 0.7
                line-spacing 0.2))

  (defun cr-elfeed-show-in-eww ()
    "Display the currently shown item in eww.
Source: https://github.com/iocanel/emacs.d/blob/master/%2Belfeed.el"
    (interactive)
    (require 'elfeed-show)
    (when (elfeed-entry-p elfeed-show-entry)
      (let ((link (elfeed-entry-link elfeed-show-entry)))
        (eww link)
        (rename-buffer (format "*elfeed eww %s*" link)))))

  :hook (elfeed-show-mode . cr-elfeed-show-settings)
  :bind ((:map cr-app-map ("f" . elfeed))
         (:map elfeed-search-mode-map ("a" . elfeed-search-show-entry))
         (:map elfeed-show-mode-map
               ("]" . elfeed-show-next)
               ("[" . elfeed-show-prev)
               ("w" . cr-elfeed-show-in-eww))))

(use-package elfeed-tube
  :after elfeed
  :demand
  :config (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

(use-package elfeed-tube-mpv
  :after elfeed-tube
  :demand
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult)
  :demand)

(use-package epg
  :straight (:type built-in)
  :custom (epg-pinentry-mode 'loopback))

(use-package eshell
  :straight (:type built-in)
  :custom
  (eshell-ls-use-colors t)
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  (eshell-destroy-buffer-when-process-dies nil)
  (eshell-visual-commands
   '("crontab" "tmux" "htop" "tail" "vi" "screen" "top" "less" "more"))
  :config
  (delete 'eshell-banner eshell-modules-list)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (defun cr-eshell-settings ()
    (hl-line-mode -1)
    (setenv "PAGER" "cat"))
  (add-hook 'eshell-mode-hook 'cr-eshell-settings)
  :bind (:map cr-app-map ("e" . eshell)))

(use-package eww
  :straight (:type built-in)
  :config
  ;; minimal rendering by default
  (setq-default shr-inhibit-images t)   ; toggle with `I`
  (setq-default shr-use-fonts t)      ; toggle with `F`

  (defun eww-toggle-images ()
    "Toggle whether images are loaded and reload the current page fro cache."
    (interactive)
    (setq-local shr-inhibit-images (not shr-inhibit-images))
    (eww-reload t)
    (message "Images are now %s"
             (if shr-inhibit-images "off" "on")))

  (define-key eww-mode-map (kbd "I") #'eww-toggle-images)
  (define-key eww-link-keymap (kbd "I") #'eww-toggle-images)

  :bind ((:map cr-app-map ("w" . eww))
         (:map eww-mode-map
               ("[" . eww-back-url)
               ("]" . eww-forward-url))))

(use-package faces
  :straight (:type built-in)
  :config
  (when system-is-linux-p
    (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 120)
    (set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono" :height 1.0)
    (set-face-attribute 'variable-pitch nil :family "DejaVu Sans" :height 1.0)
    (set-frame-font "DejaVu Sans Mono-11" nil t))
  (when system-is-osx-p
    (set-face-attribute 'default nil :font "Monaco" :height 120)
    (set-face-attribute 'fixed-pitch nil :family "Monaco" :height 1.0)
    (set-face-attribute 'variable-pitch nil :family "Verdana" :height 1.1)))

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
        (setq-local cursor-type 'bar)
      (progn (kill-local-variable 'cursor-type))))

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

(use-package flymake ;; flycheck built-in alternative
  :hook (prog-mode . flymake-mode)
  :bind ((:map cr-toggle-map ("f" . flymake-mode))
         (:map flymake-mode-map
               ("M-n" . flymake-goto-next-error)
               ("M-p" . flymake-goto-prev-error))))

(use-package flymake-languagetool
  ;; Enhances `flymake' to handle grammar errors. Do not confuse with `flyspell'
  ;; wich only uses the dictionary. Note that `flymake-languagetool-url' is
  ;; provided by `cr-private vars'
  :after flymake
  :custom (flymake-languagetool-language "fr")
  :config
  (push "WHITESPACE_RULE" flymake-languagetool-disabled-rules)

  (defun cr-languagetool-set-lang-en ()
    (interactive)
    (setq-local flymake-languagetool-language "en-US")
    (flymake-mode-on)
    (flymake-languagetool-maybe-load))

  (defun cr-languagetool-set-lang-fr ()
    (interactive)
    (setq-local flymake-languagetool-language "fr")
    (flymake-mode-on)
    (flymake-languagetool-maybe-load))

  :bind ((:map flymake-mode-map ("C-;" . flymake-languagetool-correct-dwim))
         (:map cr-spell-map
               ("E" . cr-languagetool-set-lang-en)
               ("F" . cr-languagetool-set-lang-fr))))

(use-package flyspell
  :custom
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

  (defun cr-flyspell-mode ()
    (interactive)
    (if flyspell-mode
        (flyspell-mode-off)
      (if (derived-mode-p 'prog-mode)
          (flyspell-prog-mode)
        (flyspell-mode))))
  :bind ((:map flyspell-mode-map
               ("C-." . nil)
               ("C-;" . flyspell-auto-correct-previous-word))
         (:map cr-toggle-map ("z" . cr-flyspell-mode))
         (:map cr-spell-map
               ("." . flyspell-auto-correct-word)
               ("b" . flyspell-buffer)
               ("s" . cr-save-word-to-pdict))))

(use-package follow
  :straight (:type built-in)
  :config
  (require 'winner)
  (defun cr-follow-mode-toggle ()
    (interactive)
    (if follow-mode
        (progn (winner-undo)
               (follow-mode -1))
      (follow-delete-other-windows-and-split)))
  :bind (:map cr-toggle-map ("=" . cr-follow-mode-toggle)))

(use-package frame
  :straight (:type built-in)
  :custom (default-frame-alist
           '((menu-bar-lines . 0)
             (tool-bar-lines . 0)
             (vertical-scroll-bars . nil)
             (left-fringe . 8)
             (right-fringe . 8)
             (ns-transparent-titlebar . t)))
  :config (blink-cursor-mode -1)
  :bind (:map cr-toggle-map
              ("M-RET" . toggle-frame-fullscreen)
              ("RET" . toggle-frame-maximized)))

(use-package go-mode
  :mode "\\.go\\'"
  :config (add-hook 'before-save-hook 'gofmt-before-save))

(use-package god-mode
  :config
  (with-eval-after-load 'which-key
    (which-key-enable-god-mode-support))
  (with-eval-after-load 'treemacs
    (add-to-list 'god-exempt-major-modes 'treemacs-mode))
  :bind (("C-o" . god-local-mode)
         (:map god-local-mode-map
               ("i" . god-local-mode))))

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

(use-package graphql-mode)

(use-package help
  :straight (:type built-in)
  :bind (:map help-mode-map
              ("n" . next-line)
              ("p" . previous-line)))

(use-package hl-line
  :straight (:type built-in)
  :hook ((text-mode
          prog-mode
          dired-mode
          elfeed-show-mode
          occur-mode
          org-agenda-mode
          display-time-world-mode
          profiler-report-mode
          eww-mode) . hl-line-mode)
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

(use-package ispell
  :after exec-path-from-shell
  :if (executable-find "hunspell")
  :init
  (setenv "LANG" "en_US.UTF-8")
  (defvar cr-ispell-lang1 "francais")
  (defvar cr-ispell-lang2 "english")
  (defvar cr-ispell-lang-multi (concat cr-ispell-lang1 "," cr-ispell-lang2))
  :custom
  (ispell-program-name "hunspell")
  (ispell-silently-savep t)
  (ispell-personal-dictionary cr-dictionary)
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
              ("f" . cr-ispell-set-lang1)
              ("e" . cr-ispell-set-lang2)
              ("m" . cr-ispell-set-MULTI)
              ("r" . ispell-region)))

(use-package json-mode)

(use-package ledger-mode
  :if (executable-find "ledger"))

(use-package magit
  :custom
  (magit-save-repository-buffers 'dontask)
  (magit-clone-default-directory cr-git-dir)
  :config
  (with-eval-after-load 'project
    (defun cr-remember-new-project ()
      (project-remember-projects-under default-directory))
    (add-hook 'magit-post-clone-hook 'cr-remember-new-project))
  :bind (:map cr-git-map ("c" . magit-clone)))

(use-package man
  :bind (:map cr-app-map ("m" . man)))

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :custom (markdown-fontify-code-blocks-natively t))

(use-package marginalia
  :init (marginalia-mode))

(use-package nginx-mode)

(use-package nov
  :custom
  (nov-text-width 80)
  (nov-variable-pitch t)
  :config
  (defun cr-nov-settings ()
    (face-remap-add-relative 'variable-pitch
                             :family "Georgia"
                             :height 1.2)
    (setq-local line-spacing 0.5
                left-margin-width 2)
    (nov-render-document))
  (add-hook 'nov-mode-hook 'cr-nov-settings)
  :mode ("\\.\\(epub\\|mobi\\)\\'" . nov-mode)
  :bind (:map nov-mode-map
              ("n" . next-line)
              ("p" . previous-line)))

(use-package olivetti
  :diminish
  :custom
  (olivetti-body-width (+ fill-column 30))
  (olivetti-minimum-body-width fill-column)
  :bind (:map cr-toggle-map ("o" . olivetti-mode)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  ;; https://github.com/minad/vertico#tramp-hostname-completion-and-wildcard-completion
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package org
  :custom
  (org-agenda-custom-commands
   '(("p" "Personal"
      ((agenda "")
       (tags "project")
       (stuck "")
       (todo "TODO|WAITING"))
      ((org-agenda-tag-filter '("-@work"))))
     ("r" "Weekly Review"
      ((agenda ""
               ((org-agenda-span 8)
                (org-agenda-start-day "-1w")
                (org-agenda-show-log t)
                (org-agenda-start-with-log-mode t)
                (org-agenda-archives-mode t)))
       (tags "+project/DONE")
       (tags "-project/+DONE|+DELEGATED|+CANCELED")))
     ("l" "Log"
      ((agenda ""
               ((org-agenda-show-log t)
                (org-agenda-start-with-log-mode t)
                (org-agenda-archives-mode t)
                (org-agenda-include-inactive-timestamps t)))))
     ("w" "Work"
      ((agenda "")
       (tags "project")
       (stuck "")
       (todo "TODO|WAITING"))
      ((org-agenda-tag-filter '("+@work"))))))
  (org-agenda-file-regexp "\\`[^.].*\\.org\\\(\\.gpg\\\)?\\'")
  (org-agenda-files
   '("gtd/inbox.org"
     "gtd/todos.org"
     "gtd/calendar.org"
     "gtd/tickler.org"
     "gtd/journal.org"))
  (org-agenda-log-mode-items '(closed clock))
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-show-outline-path nil)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled nil)
  (org-agenda-skip-scheduled-if-done nil)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-text-search-extra-files
   `(agenda-archives
     ,(expand-file-name "gtd/someday.org" org-directory)))
  (org-agenda-todo-ignore-scheduled 'future)
  (org-agenda-window-setup 'current-window)
  (org-attach-dir-relative t)
  (org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
  (org-capture-templates
   '(("i" "Inbox" entry
      (file "gtd/inbox.org")
      "* %i%?\n%u\n")
     ("m" "Meeting" entry
      (file "gtd/inbox.org")
      "* Meeting with %? :meeting:"
      :clock-in t)
     ("e" "Event" entry
      (file+headline "gtd/calendar.org" "Events")
      "* %i%?\n")
     ("j" "Journal" entry
      (file+olp+datetree "gtd/journal.org")
      "* %^{prompt|journal-entry}\n%U\n%?")
     ("r" "Review templates")
     ("rd" "Daily Review" entry
      (file+olp+datetree "gtd/journal.org")
      (file "templates/daily-review.org")
      :immediate-finish t
      :jump-to-captured t)
     ("rw" "Weekly Review" entry
      (file+olp+datetree "gtd/journal.org")
      (file "templates/weekly-review.org")
      :immediate-finish t
      :jump-to-captured t)
     ("rm" "Monthly Review" entry
      (file+olp+datetree "gtd/journal.org")
      (file "templates/monthly-review.org")
      :immediate-finish t
      :jump-to-captured t)))
  (org-clock-continuously nil)
  (org-clock-idle-time nil)
  (org-clock-in-resume nil)
  (org-clock-in-switch-to-state nil)
  (org-clock-persist t)
  (org-clock-report-include-clocking-task t)
  (org-confirm-babel-evaluate nil)
  (org-ctrl-k-protect-subtree t)
  (org-deadline-warning-days 14)
  (org-directory cr-org-dir)
  (org-ellipsis " ▼")
  (org-fontify-done-headline t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-imenu-depth 3)
  (org-link-file-path-type 'adaptive)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 1)
                        ("someday.org" :maxlevel . 1)))
  (org-refile-use-outline-path 'file)
  (org-show-notification-handler 'message)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-startup-folded 'showeverything)
  (org-startup-with-inline-images t)
  (org-use-speed-commands t)
  (org-tags-exclude-from-inheritance '("project"))
  (org-stuck-projects '("project" ("TODO" "WAITING") nil ""))
  (org-tag-alist
   '((:startgroup . nil) ;; Context
     ("@home"     . ?h)
     ("@work"     . ?w)
     ("@errands"  . ?e)
     ("@offline"  . ?o)
     (:endgroup   . nil)
     (:startgroup . nil) ;; Energy
     ("focus"     . ?f)
     ("casual"    . ?c)
     ("fuzzy"     . ?z)
     (:endgroup   . nil)
     (:startgroup . nil) ;; Type
     ("project"   . ?p)
     ("meeting"   . ?m)
     (:endgroup   . nil)
     (:startgroup . nil) ;; Company
     ("digi"      . ?d)
     ("krea"      . ?k)
     (:endgroup   . nil)))
  (org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "|" "DELEGATED(g@/!)" "CANCELED(c@/!)")))
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
         ("C-c l" . org-store-link)))

(use-package org
  :config
  (defvar cr-org-contexts-alist
    `(("personal" . ,cr-data-dir)
      ("work"     . ,cr-work-dir))
    "Alist of contexts: (NAME . BASE-DIR)")

  (defun cr-switch-context ()
    (interactive)
    (let* ((keys (mapcar #'car cr-org-contexts-alist))
           (prompt (format "Select context:"))
           (key (completing-read prompt keys))
           (context (car (assoc key cr-org-contexts-alist)))
           (context-path (cdr (assoc key cr-org-contexts-alist))))
      (setq org-directory (expand-file-name "org" context-path))
      (setq org-agenda-text-search-extra-files
            `(agenda-archives
              ,(expand-file-name "org/gtd/someday.org" context-path)))
      (when (eq major-mode 'org-agenda-mode)
        (org-agenda-redo-all))
      (message "You're now in %s context" context)))

  :bind ("C-c SPC" . cr-switch-context))

(use-package org-indent
  :straight (:type built-in)
  :diminish
  :custom
  (org-startup-indented t)
  (org-indent-indentation-per-level 1))

(use-package org-noter
  :straight org-noter
  :straight djvu
  :after (:any org pdf-view)
  :custom
  (org-noter-default-notes-file-names '("notes.org"))
  (org-noter-notes-search-path (list cr-ref-dir))
  (org-noter-auto-save-last-location t)
  (org-noter-always-create-frame nil)
  (org-noter-separate-notes-from-heading t)
  :bind ("C-c w n" . org-noter))

(use-package osm
  :disabled
  :bind (("C-c m h" . osm-home)
         ("C-c m s" . osm-search)
         ("C-c m v" . osm-server)
         ("C-c m t" . osm-goto)
         ("C-c m x" . osm-gpx-show)
         ("C-c m j" . osm-bookmark-jump))
  :custom
  (osm-server 'default)
  (osm-copyright nil)
  :init
  (with-eval-after-load 'org
    (require 'osm-ol)))

(use-package pass
  :custom (pass-show-keybindings nil)
  :bind (:map cr-app-map ("P" . pass)))

(use-package password-cache
  :straight (:type built-in)
  :demand
  :custom
  (password-cache t)
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

(use-package project
  :config
  (project-forget-zombie-projects)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m) t)
  :bind (:map project-prefix-map
              ("=" . project-remember-projects-under)
              ("-" . project-forget-project)))

(use-package protobuf-mode)

(use-package pulse
  :straight (:type built-in)
  :demand
  :config
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (dolist (command '(scroll-up-command scroll-down-command
                                       recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line)))

(use-package python-mode
  :custom
  (python-indent-guess-indent-offset t)
  (python-indent-guess-indent-offset-verbose nil))

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
  :init (recentf-mode))

(use-package replace
  :straight (:type built-in)
  :bind ((:map occur-mode-map
               ("n" . next-line)
               ("p" . previous-line))
         (:map cr-text-map
               ("F" . flush-lines)
               ("K" . keep-lines)
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
    :dir org-directory
    :flags ("--ignore-case")
    :menu ("Custom" "o" "Org"))
  (rg-define-search rg-org-all
    :query ask
    :format regexp
    :files "org"
    :dir "~/"
    :flags ("--ignore-case")
    :menu ("Custom" "O" "Org"))
  :bind ("C-c s" . rg-menu))

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
  :init (savehist-mode))

(use-package saveplace
  :straight (:type built-in)
  :init (save-place-mode))

(use-package vertico
  :init (vertico-mode))

(use-package server
  :disabled
  :straight (:type built-in)
  :demand
  :config (unless (server-running-p)(server-start)))

(use-package shell
  :straight (:type built-in)
  :bind (:map cr-app-map ("s" . shell)))

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
         ("M-X" . execute-extended-command-for-buffer)
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
  :init (global-so-long-mode))

(use-package sort
  :straight (:type built-in)
  :bind (:map cr-text-map ("s" . sort-lines)))

(use-package sql
  :config
  (require 'cr-private-sql nil 'noerror)
  (defun cr-isql-config ()
    (setq-local truncate-lines t))
  (add-hook 'sql-interactive-mode-hook 'cr-isql-config)
  :bind ((:map cr-app-map ("q" . sql-connect))))

(use-package ssh-config-mode)

(use-package startup
  :straight (:type built-in)
  :custom
  (initial-major-mode 'lisp-interaction-mode)
  (initial-scratch-message nil)
  (inhibit-startup-screen t))

(use-package terraform-mode)

(use-package timer-list
  :straight (:type built-in)
  :bind (:map cr-emacs-map ("l" . list-timers)))

(use-package treemacs
  :custom (treemacs-is-never-other-window t)
  :bind (("M-0" . treemacs-select-window)
         (:map cr-toggle-map ("0" . treemacs))))

(use-package tree-sitter
  :if (< emacs-major-version 29)
  :demand
  :diminish
  :straight tree-sitter
  :straight tree-sitter-langs
  :config (global-tree-sitter-mode)
  :hook (tree-sitter-mode . tree-sitter-hl-mode))

(use-package unfill
  :bind ([remap fill-paragraph] . 'unfill-toggle))

(use-package url-vars
  :straight (:type built-in)
  :custom (url-privacy-level 'high)
  :config  (url-setup-privacy-info))

(use-package vlf
  :init (require 'vlf-setup))

(use-package vterm
  :after exec-path-from-shell
  :init (setq vterm-always-compile-module t)
  :custom (vterm-max-scrollback (* 20 1000))
  :bind (:map cr-app-map ("v" . vterm)))

(use-package warnings
  :straight (:type built-in)
  :custom (warning-suppress-types '((comp))))

(use-package winner
  :straight (:type built-in)
  :init (winner-mode)
  :bind (("C-x u" . winner-undo)
         ("C-x U" . winner-redo)))

(use-package which-key
  :diminish
  :init (which-key-mode)
  :bind (:map cr-toggle-map ("?" . which-key-mode)))

(use-package whitespace
  :straight (:type built-in)
  :bind (:map cr-toggle-map ("SPC" . whitespace-mode)))

(use-package windmove
  :straight (:type built-in)
  :bind (("C-c m h" . windmove-swap-states-left)
         ("C-c m j" . windmove-swap-states-down)
         ("C-c m k" . windmove-swap-states-up)
         ("C-c m l" . windmove-swap-states-right)))

(use-package window
  :straight (:type built-in)
  :custom
  (pop-up-frames nil)
  (switch-to-prev-buffer-skip 'visible)
  :bind* (("M-o" . other-window)
          ("M-[" . previous-buffer)
          ("M-]" . next-buffer)))

(use-package ws-butler
  :diminish
  :hook (prog-mode . ws-butler-mode))

(use-package yaml-mode :mode ("\\.ya?ml\\'"))

(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode))

;;; JS
(use-package js
  :straight (:type built-in)
  :custom (js-indent-level 2)
  :config (define-key js-mode-map (kbd "M-.") nil))

(use-package typescript-mode
  :custom (typescript-indent-level 2)
  :mode "\\.ts\\'")

(use-package rjsx-mode
  :disabled
  :mode "\\.tsx\\'")

(provide 'init)
;;; init.el ends here
