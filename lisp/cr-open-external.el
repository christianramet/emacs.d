;; Source: Spacemacs
;; layers/+spacemacs/spacemacs-defaults/funcs.el

(defun cr--open-in-external-app (file-path)
  "Open `FILE-PATH' in external application."
  (cond
   ((eq system-type 'windows-nt)
    (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
   ((eq system-type 'darwin) (shell-command (format "open \"%s\"" file-path)))
   ((eq system-type 'gnu/linux) (let ((process-connection-type nil))
                                  (start-process "" nil "xdg-open" file-path)))))

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
