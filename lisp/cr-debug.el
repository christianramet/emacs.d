;;; cr-debug.el --- Debug lib -*- lexical-binding: t -*-

;; Author: Christian Ramet
;; Maintainer: Christian Ramet
;; Version: 1.0
;; Package-Requires:
;; Homepage:
;; Keywords: debug


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Functions an settings for Emacs debugging.

;;; Code:

(custom-set-variables
 '(debug-on-error t)
 '(garbage-collection-messages t)
 '(use-package-verbose t)
 '(use-package-expand-minimally nil)
 '(use-package-compute-statistics t))

(with-eval-after-load 'use-package
  (add-hook 'after-init-hook 'use-package-report))

(provide 'cr-debug)

;;; cr-debug.el ends here
