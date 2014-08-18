;;; init-ido.el --- init-ido file files
;;; Commentary:
;;; Code:

(require 'ido)

;; Consider the settings that prelude uses.
;; -- look at ido-ubiquitous
;; -- flx-ido


;;;; Ido settings.  I am not real familiar with these yet.
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)

;; This one might not be fun.  If point is on something that looks
;; like a filename, ido assumes that is what you want.
(setq ido-use-filename-at-point 'guess)

;; (setq ido-use-url-at-point t)
;; this can give preferences to more common files.
;; (setq ido-file-extensions-order '(".py" ".cc"))
;; look for docs in ido-find-file, ido-find-dir, etc


(provide 'init-ido)

;;; init-ido.el ends here
