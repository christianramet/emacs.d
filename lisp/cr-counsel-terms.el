(require 'counsel)
(require 'eshell)
(require 'shell)
(require 'vterm)

(defun pop-to-term (name)
  (pop-to-buffer name '((display-buffer-reuse-window
                         display-buffer-same-window)
                        (inhibit-same-window . nil)
                        (reusable-frames . visible))))

(defun term-buffer-exist-p (name mode)
  "Return true if the `name' buffer exists in the appropriate `mode.'"
  (and (get-buffer name)
       (string= mode (buffer-local-value 'major-mode (get-buffer name)))))

(defun eshell-with-name (name)
  (eshell 99)
  (rename-buffer (format "*eshell*-%s" name)))

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
  (if (term-buffer-exist-p name "eshell-mode")
      (pop-to-term name)
    (eshell-with-name name)))

(defun vterm-with-name (name)
  (vterm (format "*vterm*-%s" name)))

(defun vterm-ssh (name host)
  (vterm-with-name name)
  (vterm-send-string (concat "ssh " host "\n")))

(defun vterm-try-tramp (name)
  "Look for any tramp context and ssh accordingly."
  (let ((host (file-remote-p default-directory 'host)))
    (if host
        (vterm-ssh name host)
      (vterm-with-name name))))

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
  (if (term-buffer-exist-p name "vterm-mode")
      (pop-to-term name)
    (vterm-try-tramp name)))

(defun shell-with-name (name)
  (shell (format "*shell*-%s" name)))

(defun cr-counsel-shell ()
  "Switch to a shell buffer, or create one."
  (interactive)
  (ivy-read "Shell buffer: " (counsel--buffers-with-mode #'shell-mode)
            :action '(1
                      ("o" cr-counsel--switch-to-shell "default")
                      ("k" ivy--kill-buffer-action "kill")
                      ("r" ivy--rename-buffer-action "rename"))
            :caller 'cr-counsel-shell))

(defun cr-counsel--switch-to-shell (name)
  "Display shell buffer with NAME and select its window.
    Reuse any existing window already displaying the named buffer.
    If there is no such buffer, start a new `shell' with NAME."
  (if (term-buffer-exist-p name "shell-mode")
      (pop-to-term name)
    (shell-with-name name)))

(provide 'cr-counsel-terms)
