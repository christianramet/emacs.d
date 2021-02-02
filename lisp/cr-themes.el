(defgroup cr-themes nil
  "Options for cr-themes.")

(defcustom cr-themes-light 'leuven
  "Light theme."
  :group 'cr-themes)

(defcustom cr-themes-dark 'wombat
  "Dark theme."
  :group 'cr-themes)

(defcustom cr-themes-default cr-themes-light
  "Default theme to load at startup."
  :group 'cr-themes)

(defcustom cr-themes-pair `(,cr-themes-light ,cr-themes-dark)
  "Pair of themes to be used by `cr-themes-toggle'."
  :group 'cr-themes)

(defun cr-themes-toggle ()
  "Interchange between the 2 themes defined in `cr-themes-pair'"
  (interactive)
  (let ((new-theme (car (remove (car custom-enabled-themes) cr-themes-pair))))
    (mapcar #'disable-theme custom-enabled-themes)
    (load-theme new-theme t)))

(provide 'cr-themes)
