;;; init-utilities.el --- utilities for the startup file

;;; Commentary: Utilities to make the startup work better.

;;; Code:

;; Taken from http://milkbox.net/note/single-file-master-emacs-configuration/

;; Make eval-after-load a little nicer.
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))


;; From emacswiki, by AaronL
(defun call-if-fbound (function &rest args)
  "call FUNCTION with optional args, only if it is found.
     Return t if is fbound and called without error, nil otherwise"
  (when (fboundp function)
    (apply function args)
    t))

(put 'call-if-fbound 'lisp-indent-function 1)

(defun after-init (fcn)
  "add to after-init-hook, and run, if we are after-init"
  (add-hook 'after-init-hook fcn)
  (when after-init-time
    (funcall fcn)))

(provide 'init-utilities)

;;; init-utilities.el ends here
