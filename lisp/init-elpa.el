;;; init-elpa.el --- init-elpa file files

;;; Commentary:
;;; Code:

(require 'package)

;;; Also use Melpa for most packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

(package-initialize)



(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init-elpa)

;;; init-elpa.el ends here
