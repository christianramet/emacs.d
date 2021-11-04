;; Minimal init file with `package' and MELPA repository enabled
;; Load this file with the following command:
;; emacs -q -l ~/.emacs.d/init-mini.el

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Testing area below this point

;; (package-install 'go-mode)
