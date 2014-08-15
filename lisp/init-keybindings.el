;;; init-keybindings.el --- init-keybindings file files

;;; Commentary:
;;; Code:

(global-set-key [(f9)] 'recompile)
(global-set-key [(f5)] 'call-last-kbd-macro)
(global-set-key "\C-x\C-n" 'next-error)

;; TODO -- look at undoing this so that the help works better.
;; (global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-?" 'help)
(global-set-key "\C-cx" 'compile)

;; Hmm... isearch.  and regexp.
(global-set-key "\M-s" 'isearch-forward-regexp)
(global-set-key "\M-r" 'isearch-backward-regexp)

;; backward-kill-word is a fast way to delete
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


(global-set-key (kbd "M-/") 'hippie-expand)


;; This is for when alt is not meta.   I need my meta.
(setq x-alt-keysym 'meta)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

;; Sometimes M-x is just too hard to type.
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; backward-kill-word is a fast way to delete
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(provide 'init-keybindings)

;;; init-keybindingsi.el ends here
