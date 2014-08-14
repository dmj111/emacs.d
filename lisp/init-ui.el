;;; init-ui.el --- init-ui file files

;;; Commentary:

;;; Code:

;; Turn off mouse interface early in startup to avoid momentary display
(call-if-fbound menu-bar-mode -1)
(call-if-fbound 'tool-bar-mode -1)
(call-if-fbound 'scroll-bar-mode -1)

(setq inhibit-startup-screen t)

(provide 'init-ui)

;;; init-ui.el ends here.
