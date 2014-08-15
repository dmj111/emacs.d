;;; init-ui.el --- init-ui file files

;;; Commentary:

;;; Code:

;; Turn off mouse interface early in startup to avoid momentary display
(call-if-fbound menu-bar-mode -1)
(call-if-fbound 'tool-bar-mode -1)
(call-if-fbound 'scroll-bar-mode -1)

(setq inhibit-startup-screen t)


(setq-default indent-tabs-mode nil)

(column-number-mode 1)
(show-paren-mode 1)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(provide 'init-ui)

;;; init-ui.el ends here.
