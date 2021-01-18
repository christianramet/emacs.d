;; Source: Spacemacs

(defun cr--open-in-external-app (file-path)
  "Open `file-path' in external application."
  (let ((process-connection-type nil))
    (if (eq system-type 'darwin)
        (start-process "" nil "open" file-path)
      (start-process "" nil "xdg-open" file-path))))

(defun cr-open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer."
  (interactive "P")
  (if arg
      (cr--open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (cr--open-in-external-app file-path)
        (message "No file associated to this buffer.")))))

(provide 'cr-open-external)
