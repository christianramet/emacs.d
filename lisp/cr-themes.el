;; Library for managing Emacs themes.

(defgroup cr-themes nil
  "Options for cr-themes."
  :group 'cr-themes
  :prefix "cr-themes-")

(defcustom cr-themes-light 'leuven
  "Light theme."
  :type 'plist
  :group 'cr-themes)

(defcustom cr-themes-dark 'wombat
  "Dark theme."
  :type 'plist
  :group 'cr-themes)

(defcustom cr-themes-default cr-themes-light
  "Default theme to load at startup."
  :type 'plist
  :group 'cr-themes)

(defcustom cr-themes-pair `(,cr-themes-light ,cr-themes-dark)
  "Pair of themes to be used by `cr-themes-toggle'."
  :type 'list
  :group 'cr-themes)

(defcustom cr-themes-toggle-hook nil
  "Hooks run when Emacs theme is changed with
`cr-themes-toggle'."
  :group 'cr-themes
  :type 'hook)

(defun cr-themes-current ()
  (car custom-enabled-themes))

;;;###autoload
(defun cr-themes-toggle ()
  "Interchange between the 2 themes defined in `cr-themes-pair'"
  (interactive)
  (let ((new-theme
         (car (remove (cr-themes-current) cr-themes-pair))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme new-theme t)
    (run-hooks 'cr-themes-toggle-hook)))

(defun cr-themes-pdf-view-sync-current-buffers ()
  "Toggle theme of every pdf-view buffers which are already
opened."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'pdf-view-mode)
                 (buffer-file-name buffer))
        (pdf-view-midnight-minor-mode)))))

(defun cr-themes-pdf-view-sync-new-buffers ()
  "Toggle theme of newly opened pdf-view buffers."
  (if (eq (cr-themes-current) cr-themes-dark)
      (pdf-view-midnight-minor-mode 1)
    (pdf-view-midnight-minor-mode -1)))

(add-hook 'cr-themes-toggle-hook 'cr-themes-pdf-view-sync-current-buffers)
(add-hook 'pdf-view-mode-hook 'cr-themes-pdf-view-sync-new-buffers)

(provide 'cr-themes)
