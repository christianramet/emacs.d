;;; cr-immortal-buffers-mode.el --- Immortal buffers -*- lexical-binding: t -*-

;; Author: Christian Ramet
;; Maintainer: Christian Ramet
;; Version: 1.0
;; Package-Requires:
;; Homepage:
;; Keywords:


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

;; Make some specific buffers "immortals": they cannot be killed. Those buffers
;; can be defined using the `cr-immortal-buffers-list' variable.

;;; Code:

(defgroup cr-immortal-buffers nil
  "Build and install packages from source code")

(defcustom cr-immortal-buffers-list '("*scratch*" "*Messages*")
  "List of buffers deemed to be immortals"
  :group 'cr-immortal-buffers
  :type 'list)

(defun cr--kill-buffer-query-immortal ()
  "Function intented to be used with `kill-buffer-query-functions'
Return nil if the buffer to be killed is deemed immortal"
  (not (member (buffer-name) cr-immortal-buffers-list)))

;;;###autoload
(define-minor-mode cr-immortal-buffers-mode
  "Make the buffers in `cr-immortal-buffers' immune to kill commands."
  :group 'cr-immortal-buffers
  :global t
  (if cr-immortal-buffers-mode
      (add-hook 'kill-buffer-query-functions #'cr--kill-buffer-query-immortal)
    (remove-hook 'kill-buffer-query-functions #'cr--kill-buffer-query-immortal)))

(provide 'cr-immortal-buffers-mode)

;;; cr-immortal-buffers-mode.el ends here
