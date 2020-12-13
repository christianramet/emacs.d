(defun cr-backward-kill-word-or-region (&optional arg)
  "Calls `kill-region' when a region is active and
`backward-kill-word' otherwise. ARG is passed to
`backward-kill-word' if no region is active."
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))

(defun cr-fill-or-unfill-paragraph ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'cr-fill-or-unfill-paragraph)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun cr-comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun cr-new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

(defun cr-rename-buffer (new-buffer-name)
  "Prompts for a new buffer name using the current buffer name as an
initial value, renames the current buffer to the value entered."
  (interactive
   (let ((current-buffer-name (buffer-name)))
     (list (read-string "Rename buffer (to new name): "
                        current-buffer-name 'buffer-name-history current-buffer-name t))))
  (rename-buffer new-buffer-name))

(defun cr-goto-scratch ()
  "Jump to the *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun cr-copy-this-file ()
  "Copy current file to destination."
  (interactive)
  (let ((dest (read-file-name "Destination: " nil nil nil (file-name-nondirectory (buffer-file-name)))))
    (copy-file (buffer-file-name) dest 1)))

(defun cr-delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer.
Source: Spacemacs"
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (and (featurep 'projectile)
                   (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))

(defun cr-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting.
Source: spacemacs"
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (when (projectile-project-p)
                 (call-interactively #'projectile-invalidate-cache))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))


(defun cr-yank-filename ()
  "Show and copy the name of the current file or buffer."
  (interactive)
  (message (kill-new (buffer-name))))

(defun cr-yank-filename-fullpath ()
  "Show and copy the full path to the current file."
  (interactive)
  (message (kill-new (buffer-file-name))))

(defun cr-flush-blank-lines (start end)
  "Delete all blank lines, for the current region, or buffer if nothing is selected"
  (interactive "r")
  (flush-lines "^\\s-*$" start end nil))

(defun cr-collapse-blank-lines (start end)
  "Collapse all blank lines into one, for the current region, or buffer if nothing is selected"
  (interactive "r")
  (replace-regexp "^\n\\{2,\\}" "\n" nil start end))

(defun cr-reload-emacs ()
  "Reload the main emacs configuration file."
  (interactive)
  (if (boundp 'chemacs-emacs-profiles)
      (load-file (expand-file-name "init.el" user-emacs-directory))
    (load-file user-init-file)))

(defun cr-edit-emacs-init-config ()
  "Edit the main Emacs configuration file."
  (interactive)
  (find-file user-init-file))

(defun cr-test-emacs-config ()
  "Test if emacs starts correctly. Source: Abo-Abo"
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string (format "emacs --batch --eval \"
(condition-case e
    (progn
      (load \\\"%s\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e))))\"" (expand-file-name "init.el" user-emacs-directory))))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "All is well"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))

(defun cr-emacs-quit ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun cr-url-to-pdf-with-wkhtmltopdf ()
  "Produce a pdf from the URL at point and ask where to save it.
Requires wkhtmltopdf"
  (interactive)
  (require 'url-util)
  (let (url filename)
    (setq url (url-get-url-at-point))
    (if (string= url nil)
        (message "No URL at point.")
      (setq filename (read-file-name "Save as:" nil nil))
      (async-shell-command (format "wkhtmltopdf %s %s " url filename)))))

(defun cr-github-search ()
  "GitHub Search a query or region if any."
  (interactive)
  (browse-url
   (concat
    "https://github.com/search?q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Query: "))
    "&type=Code")))

(defun cr-duckduckgo-search ()
  "DuckDuckGO a query or region if any."
  (interactive)
  (browse-url
   (concat
    "https://duckduckgo.com/?q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Query: ")))))

(defun cr-increment-number-at-point ()
  "Increment the number at point by 1."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun cr-decrement-number-at-point ()
  "Decrement the number at point by 1."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(defun cr-switch-to-last-buffer ()
  "Switch to last open buffer in current window.
Source: `windower.el' by Ambrevar."
  (interactive)
  (if (window-dedicated-p)
      (message "Window is dedicated to its buffer")
    (switch-to-buffer (other-buffer (current-buffer) 1))))

(provide 'cr-functions)
