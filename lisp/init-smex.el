;;; smex.el --- smex file files
;;; Commentary:
;;; Code:

(require 'smex)

;; TODO: set save file
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)



(provide 'smex)

;;; smex.el ends here
