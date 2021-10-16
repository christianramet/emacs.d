(defvar cr-hide-mode-line-mode-hook nil)

(define-minor-mode cr-hide-mode-line-mode
  "Toggle the hide-mode-line mode."
  :init-value nil
  :global nil
  (if cr-hide-mode-line-mode
      (setq-local mode-line-format nil)
    (kill-local-variable 'mode-line-format))
  (force-mode-line-update)
  (run-hooks 'cr-hide-mode-line-mode-hook))

(provide 'cr-hide-mode-line-mode)
