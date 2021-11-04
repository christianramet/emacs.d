;; Minimal init with `use-package'
;; Load this file with the following command:
;; emacs -q -l ~/.emacs.d/init-mini-use-package.el

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(package-install 'use-package)
(require 'use-package)

;; Testing area below this point

;; (use-package go-mode
;;   :ensure t)
