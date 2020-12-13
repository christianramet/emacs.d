(defcustom cr--immortal-buffers-list '("*scratch*" "*Messages*")
  "List of buffers deemed to be immortals")

(defun cr--kill-buffer-query-immortal ()
  "Function intented to be used with `kill-buffer-query-functions'
Return nil if the buffer to be killed is deemed immortal"
  (not (member (buffer-name) cr--immortal-buffers-list)))

(add-hook 'kill-buffer-query-functions #'cr--kill-buffer-query-immortal)

(provide 'cr-immortal-buffers)
