;; Functions an settings for Emacs debugging.

(custom-set-variables
 '(debug-on-error t)
 '(garbage-collection-messages t)
 '(use-package-verbose t)
 '(use-package-expand-minimally nil)
 '(use-package-compute-statistics t))

(custom-set-variables
 '(auto-revert-verbose t)
 '(byte-compile-verbose t)
 '(magit-refresh-verbose t)
 '(projectile-verbose t)
 '(which-key-is-verbose t))

(with-eval-after-load 'use-package
  (add-hook 'after-init-hook 'use-package-report))

(provide 'cr-debug)
