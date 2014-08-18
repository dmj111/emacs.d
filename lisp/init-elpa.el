;;; init-elpa.el --- init-elpa file files

;;; Commentary:
;;; Code:

(require 'cl)
(require 'package)


;;; Also use Melpa for most packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

(package-initialize)


(defvar my-packages
  '(auto-complete
    ;; ein
    ;; ess
    ;; google-c-style
    smex
    magit
    markdown-mode
    paredit
    solarized-theme
    undo-tree
    yasnippet))

(unless (every #'package-installed-p my-packages)
  (package-refresh-contents)
  (mapc '(lambda (package)
           (message "checking package")
           (unless (package-installed-p package)
             (package-install package)))
        my-packages))



(provide 'init-elpa)

;;; init-elpa.el ends here
