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

(defcustom cr-themes-toggle-hook nil
  "Hooks run when Emacs theme is changed with `cr-themes-toggle'."
  :group 'cr-themes
  :type 'hook)

(defun cr-themes-current ()
  (car custom-enabled-themes))

;;;###autoload
(defun cr-themes-toggle ()
  "Interchange between the 2 themes defined in `cr-themes-pair'"
  (interactive)
  (let ((new-theme (car (remove (cr-themes-current) cr-themes-pair))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme new-theme t)
    (run-hooks 'cr-themes-toggle-hook)))

(with-eval-after-load 'pdf-view
  (defun pdf-view-update-midnight-mode ()
    "Update pdf-view’s colour theme."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'pdf-view-mode)
                   (buffer-file-name buffer))
          (pdf-view-midnight-minor-mode)))))
  (add-hook 'cr-themes-toggle-hook 'pdf-view-update-midnight-mode))

(when (string= (getenv "DESKTOP_SESSION") "ubuntu")
  (defun cr-ubuntu-themes-toggle ()
    (if (eq (cr-themes-current) cr-themes-light)
        (call-process-shell-command
         "gsettings set org.gnome.desktop.interface gtk-theme Yaru")
      (call-process-shell-command
       "gsettings set org.gnome.desktop.interface gtk-theme Yaru-dark")))
  (add-hook 'cr-themes-toggle-hook 'cr-ubuntu-themes-toggle))

(provide 'cr-themes)

;;; cr-themes.el ends here
