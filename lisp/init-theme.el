;;; init-theme.el --- init-theme file files

;;; Commentary:
;;; Code:

(defvar my-default-theme 'solarized-dark
  "default theme to use at startup")

(after 'init
  (message "loading the theme...")
  (load-theme my-default-theme t))

(provide 'init-theme)

;;; init-theme.el ends here
