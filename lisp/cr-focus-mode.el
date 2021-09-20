(require 'olivetti)
(require 'cr-hide-mode-line-mode)

(defvar cr-focus-mode-hook nil)
(define-minor-mode cr-focus-mode
  "Toggle focus mode."
  :init-value nil
  :global nil
  (if cr-focus-mode
      (progn
        (unless (derived-mode-p 'prog-mode)
          (cr-hide-mode-line-mode 1))
        (olivetti-mode 1)
        (setq focus-mode-window-snapshot (current-window-configuration))
        (delete-other-windows))
    (progn
      (cr-hide-mode-line-mode -1)
      (set-window-configuration focus-mode-window-snapshot)
      (olivetti-mode -1)))
  (run-hooks 'cr-focus-mode-hook))

(provide 'cr-focus-mode)
