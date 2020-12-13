(require 'counsel)

(defun cr-counsel-eshell ()
  "Switch to an eshell buffer, or create one."
  (interactive)
  (ivy-read "Eshell buffer: " (counsel--buffers-with-mode #'eshell-mode)
            :action '(1
                      ("o" cr-counsel--switch-to-eshell "default")
                      ("k" ivy--kill-buffer-action "kill")
                      ("r" ivy--rename-buffer-action "rename"))
            :caller 'cr-counsel-eshell))

(defun cr-counsel--switch-to-eshell (name)
  "Display eshell buffer with NAME and select its window.
    Reuse any existing window already displaying the named buffer.
    If there is no such buffer, start a new `eshell' with NAME."
  (if (and (get-buffer name)
           (string= "eshell-mode" (buffer-local-value 'major-mode (get-buffer name))))
      (pop-to-buffer name '((display-buffer-reuse-window
                             display-buffer-same-window)
                            (inhibit-same-window . nil)
                            (reusable-frames . visible)))
    (eshell 99)
    (rename-buffer (format "*eshell*-%s" name))))

(defun cr-counsel-vterm ()
  "Switch to a vterm buffer, or create one."
  (interactive)
  (ivy-read "Vterm buffer: " (counsel--buffers-with-mode #'vterm-mode)
            :action '(1
                      ("o" cr-counsel--switch-to-vterm "default")
                      ("k" ivy--kill-buffer-action "kill")
                      ("r" ivy--rename-buffer-action "rename"))
            :caller 'cr-counsel-vterm))

(defun cr-counsel--switch-to-vterm (name)
  "Display vterm buffer with NAME and select its window.
    Reuse any existing window already displaying the named buffer.
    If there is no such buffer, start a new `vterm' with NAME."
  (if (and (get-buffer name)
           (string= "vterm-mode" (buffer-local-value 'major-mode (get-buffer name))))
      (pop-to-buffer name '((display-buffer-reuse-window
                             display-buffer-same-window)
                            (inhibit-same-window . nil)
                            (reusable-frames . visible)))
    (vterm (format "*vterm*-%s" name))))

(provide 'cr-counsel-terms)
