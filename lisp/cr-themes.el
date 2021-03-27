;;; cr-themes.el --- Themes settings lib -*- lexical-binding: t -*-

;; Author: Christian Ramet
;; Maintainer: Christian Ramet
;; Version: 1.0
;; Package-Requires:
;; Homepage:
;; Keywords: themes


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

;; Library for managing Emacs themes.

;;; Code:

(defgroup cr-themes nil
  "Options for cr-themes."
  :group 'cr-themes
  :prefix "cr-themes-")

(defcustom cr-themes-light 'leuven
  "Light theme."
  :type 'plist
  :group 'cr-themes)

(defcustom cr-themes-dark 'wombat
  "Dark theme."
  :type 'plist
  :group 'cr-themes)

(defcustom cr-themes-default cr-themes-light
  "Default theme to load at startup."
  :type 'plist
  :group 'cr-themes)

(defcustom cr-themes-pair `(,cr-themes-light ,cr-themes-dark)
  "Pair of themes to be used by `cr-themes-toggle'."
  :type 'list
  :group 'cr-themes)

(defun cr-themes-current ()
  (car custom-enabled-themes))

;;;###autoload
(defun cr-themes-toggle ()
  "Interchange between the 2 themes defined in `cr-themes-pair'"
  (interactive)
  (let ((new-theme (car (remove (cr-themes-current) cr-themes-pair))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme new-theme t)))

(with-eval-after-load 'pdf-view
  (defun cr-themes-pdf-view-sync ()
    "Synchronize Emacs light/dark themes with pdf-view midnight mode."
    (when (eq (cr-themes-current) cr-themes-light)
      (pdf-view-midnight-minor-mode -1))
    (when (eq (cr-themes-current) cr-themes-dark)
      (pdf-view-midnight-minor-mode 1)))
  (add-hook 'pdf-view-mode-hook 'cr-themes-pdf-view-sync))

(provide 'cr-themes)

;;; cr-themes.el ends here
