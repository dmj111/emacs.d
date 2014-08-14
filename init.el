;;; init.el --- Emacs configuration file

;;; Code:

(defconst *config-dir* (file-name-directory load-file-name)
  "Root directory for the configuration")

(defconst *is-mac* (eq system-type 'darwin))

(add-to-list 'load-path (expand-file-name "lisp" *config-dir*))


(require 'init-utilities)
(require 'init-ui)

;;; Commentary:

(message "finished loading")

(provide 'init)
;;; init.el ends here
