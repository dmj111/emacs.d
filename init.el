;;; init.el --- Emacs configuration file

;;; Commentary:


;;; Code:

(defconst *config-dir* (file-name-directory load-file-name)
  "Root directory for the configuration")

(defconst *is-mac* (eq system-type 'darwin))

(add-to-list 'load-path (expand-file-name "lisp" *config-dir*))
(add-to-list 'load-path (expand-file-name "local" *config-dir*))

;; (defvar config-dir (file-name-directory load-file-name)
;;   "Root directory for the configuration")

;; (defvar local-config-dir (expand-file-name "local" config-dir)
;;   "Root directory for local specific configurations")

;; (defvar private-config-dir (expand-file-name "private" config-dir)
;;   "Root directory for private configuration")

;; (add-to-list 'load-path local-config-dir)
;; (add-to-list 'load-path private-config-dir)

;; (setq custom-file (expand-file-name "custom.el" local-config-dir))
;; (load custom-file 1)



(require 'init-utilities)
(require 'init-ui)
(require 'init-elpa)
(require 'init-theme)
(require 'init-keybindings)


(require 'init-local nil t)


(require 'server)
(unless (server-running-p)
  (server-start))


(message "finished loading")

(provide 'init)
;;; init.el ends here
