;;; init-python --- Python settings
;;; Commentary:
;;; Code:


(after 'python
  (message "python has been loaded")
  (defun my-python-mode-hook ()
    "Stuff to run when python-mode loads"
    (setq-default py-indent-offset 4)
    (setq indent-tabs-mode nil)
    ;;(setq py-python-command nil)
    ;;(require 'virtualenv)
    (transient-mark-mode t)

    (message "ran my-python-mode-hook"))

  (add-hook 'python-mode-hook 'my-python-mode-hook))

(after 'flymake-python-pyflakes-autoloads
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

;; Alternate setup with jedi...

(after 'jedi-autoloads
  (message "jedi setup")

  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t))



(provide 'init-python)
;;;      init-python.el ends here
