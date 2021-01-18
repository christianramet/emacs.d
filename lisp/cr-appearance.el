(defcustom cr-theme-dark 'wombat "Dark theme.")
(defcustom cr-theme-light 'leuven "Light theme.")
(defcustom cr-theme-default cr-theme-light "Default theme.")
(defcustom cr-theme-fallback 'leuven "Theme to use when `cr-theme-default' is unavailable.")

(defcustom cr-font-size 14 "Default font size.")
(defcustom cr-font-default "DejaVu Sans Mono" "Default font name.")
(defcustom cr-font-fixed "DejaVu Sans Mono" "Fixed pitch font name.")
(defcustom cr-font-variable "DejaVu Sans" "Variable pitch font name.")

(defun cr--load-theme (x)
  "Load X theme by first unloading all the current ones."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme x t))

(defun cr-toggle-themes-light-dark ()
  "Toggle between 2 preset themes: a light and a dark one."
  (interactive)
  (if (eq (car custom-enabled-themes) cr-theme-light)
      (cr--load-theme cr-theme-dark)
    (cr--load-theme cr-theme-light)))

;;; Allows consistency accross Emacs terminal, Emacs GUI, and Emacs daemon.
(defun cr--set-theme (&optional frame)
  (when frame
    (select-frame frame))
  (unless (load-theme cr-theme-default t)
    (load-theme cr-theme-fallback t)))

(defun cr--set-font (&optional frame)
  (when frame
    (select-frame frame))
  (set-face-attribute 'default nil
                      :font (font-spec :family cr-font-default :size cr-font-size))
  (set-face-attribute 'fixed-pitch nil
                      :font (font-spec :family cr-font-fixed :size cr-font-size))
  (set-face-attribute 'variable-pitch nil
                      :font (font-spec :family cr-font-variable :size cr-font-size)))

(when (daemonp)
  (add-hook 'after-make-frame-functions 'cr--set-theme)
  (unless (eq system-type 'darwin)
    (add-hook 'after-make-frame-functions 'cr--set-font)))

(when (display-graphic-p)
  (cr--set-theme)
  (unless (eq system-type 'darwin)
    (cr--set-font)))

(provide 'cr-appearance)
