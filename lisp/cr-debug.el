(custom-set-variables
 '(debug-on-error t)
 '(garbage-collection-messages t)
 '(use-package-verbose t)
 '(use-package-expand-minimally nil)
 '(use-package-compute-statistics t))

(with-eval-after-load 'use-package
  (add-hook 'after-init-hook 'use-package-report))

(provide 'cr-debug)
