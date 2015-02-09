;;; init.el --- Emacs configuration file

;;; Commentary:

;; stuff to practice:
;; - C-/ to undo
;; M-0 (to 9) instead of C-u 8
;; M-a, M-e
;; C-M (move by expression) f,b,d,u
;; C-M-s regexp forward search
;; M-m -- move back to indentation
;; C-u M-g M-g -- goto line in prev buffer (matches line number at point)
;;

;;;; TO LEARN
;; TODO: ace-jump
;; http://www.emacswiki.org/emacs/ImenuMode#toc10 -ido powered imenu
;; http://masteringemacs.org/article/effective-editing-movement ETAGS
;; C-x C-n / C-u C-x C-n  set /unset goal column
;; subword mode for Camel Case
;;

;;; Code:

;;;; Debugging
;; Debug if there is an error
;; (setq debug-on-error t)
;; Don't limit the print out of a variable
(setq eval-expression-print-length nil)

;; From emacswiki, by AaronL
(defun call-if-fbound (function &rest args)
  "call FUNCTION with optional args, only if it is found.
     Return t if is fbound and called without error, nil otherwise"
  (when (fboundp function)
    (apply function args)
    t))

(put 'call-if-fbound 'lisp-indent-function 1)

;; Turn off mouse interface early in startup to avoid momentary display
(call-if-fbound menu-bar-mode -1)
(call-if-fbound 'tool-bar-mode -1)
(call-if-fbound 'scroll-bar-mode -1)

(setq inhibit-startup-screen t)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp)

(defconst *config-dir* (file-name-directory load-file-name)
  "Root directory for the configuration")
(defconst *local-dir* (expand-file-name "local" *config-dir*)
  "Root directory for local configuation")

(setq custom-file (expand-file-name "custom.el" *local-dir*))

(defconst *is-mac* (eq system-type 'darwin))

(add-to-list 'load-path (expand-file-name "lisp" *config-dir*))
(add-to-list 'load-path (expand-file-name "local" *config-dir*))

;; Try to load local settings ahead of time
(require 'init-local-preload nil t)
;;;; Utilities

;; Taken from http://milkbox.net/note/single-file-master-emacs-configuration/

;; Make eval-after-load a little nicer.
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn
        (message "after loading %s" ,mode)
        ,@body)))

;;;; User interface

(setq-default indent-tabs-mode nil)

(column-number-mode 1)
(show-paren-mode 1)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;;; enable features

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (quote [M-down])  'scroll-up-line)
(global-set-key (quote [M-up])  'scroll-down-line)

;;;; packages
(require 'cl)
(require 'package)

;;; Also use Melpa for most packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

;; Stable packages?
;; (add-to-list 'package-archives
;;  '("melpa-stable" . "http://stable.melpa.org/packages/") t)


(package-initialize)

;; Instal some packages I use...
(defvar my-packages
  '(auto-complete
    smex
    magit
    markdown-mode
    paredit
    ;; solarized-theme
    undo-tree
    yasnippet))

(unless (every #'package-installed-p my-packages)
  (package-refresh-contents)
  (mapc #'(lambda (package)
            (message "checking package")
            (unless (package-installed-p package)
              (package-install package)))
        my-packages))

;;;; Theme

(defvar my-default-theme 'solarized-dark
  "default theme to use at startup")

;; After init.el is loaded, set the theme.
(after 'init
  (message "loading the theme...")
  (load-theme my-default-theme t))

;;;; Keybindings
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

;; From emacs-prelude
;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))


;; Sometimes M-x is just too hard to type.
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(define-key global-map (kbd "RET") 'newline-and-indent)

;; backward-kill-word is a fast way to delete
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; This is for when alt is not meta.   I need my meta.
(setq x-alt-keysym 'meta)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

;;;; ido
;; Its built-in...
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

;; Avoid pinging with C-x f
(setq ffap-machine-p-known 'reject)

;; (setq ido-use-url-at-point t)
;; this can give preferences to more common files.
;; (setq ido-file-extensions-order '(".py" ".cc"))
;; look for docs in ido-find-file, ido-find-dir, etc

;;;; magit
(after "magit-autoloads"
  (global-set-key "\C-xg" 'magit-status)


  ;; full-scrreen magit-status
  ;; from magnars --
  ;;    https://github.com/magnars/.emacs.d/blob/master/setup-magit.el

  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(after 'magit
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;;;; python

(after 'python
  (defun my-python-mode-hook ()
    "Stuff to run when python-mode loads"
    (setq-default py-indent-offset 4)
    (setq indent-tabs-mode nil)
    ;;(setq py-python-command nil)
    ;;(require 'virtualenv)
    (transient-mark-mode t)
    (message "ran my-python-mode-hook"))

  (add-hook 'python-mode-hook 'my-python-mode-hook)

  (after "flymake-python-pyflakes-autoloads"
    (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

  ;; Alternate setup with jedi...
  (after "jedi-autoloads"
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:setup-keys t)
    (setq jedi:complete-on-dot t)))


;; For now, my org setup is such a mess... put it here.
(require 'init-org)

;;;; Shell-script mode

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;;;; elisp

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)


;;;; recentf

;; emacs-prelude
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode 1)

;; move to another file...
(require 'dash)
(defun recentf-ido-find-file ()
  "Find recent file with ido."
  (interactive)
  (let ((file (ido-completing-read
               "Chose recent file:"
               (-map 'abbreviate-file-name recentf-list) nil t)))
    (when file (find-file file))))

(global-set-key (kbd "C-c f") 'recentf-ido-find-file)

(when (file-exists-p custom-file)
  (load custom-file))

;;;; programming mode
;; TODO: flyspell on mac!!
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook
          (lambda () (add-hook 'before-save-hook
                               'whitespace-cleanup nil t)))

;;;; uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;;;; undo-tree
(after "undo-tree-autoloads"
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer--timestamps t)
  (message "foo"))



;;;; smex

(after "smex-autoloads"
  ;; TODO: set save file
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))


;;;; js2

(after "js2-mode-autoloads"
  (add-to-list 'auto-mode-alist '("\\.json" . js-mode))
  ;; (add-hook 'js-mode-hook 'js2-minor-mode)
  (setq js2-highlight-level 3))

(after 'js2-mode
  (after "ac-js2-autoloads"
    (add-hook 'js2-mode-hook 'ac-js2-mode)))


;;;; auto-complete
(after "auto-complete-autoloads"
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories
               (expand-file-name "ac-dict" *config-dir*))
  (ac-config-default)
  ;; Trigger key
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>"))



;;;; yasnippet
(after "yasnippet-autoloads"
  (setq yas-snippet-dirs
        (append yas-snippet-dirs
                (expand-file-name "snippets" *local-dir*)))
  (yas-global-mode 1))


;;;; c++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;;; cpputils-cmake
(after "cpputils-cmake-autoloads")

(after "google-c-style-autoloads"
  (add-hook 'c-mode-common-hook 'google-set-c-style))


(after 'org-mode
  ;; http://endlessparentheses.com/inserting-the-kbd-tag-in-org-mode.html?source=rss
  (define-key org-mode-map "\C-ck" #'endless/insert-key)
  (defun endless/insert-key (key)
    "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
    (interactive "kType key sequence: ")
    (let* ((is-org-mode (derived-mode-p 'org-mode))
           (tag (if is-org-mode
                    "@@html:<kbd>%s</kbd>@@"
                  "<kbd>%s</kbd>")))
      (if (null (equal key "
"))
          (insert
           (format tag (help-key-description key nil)))
        (insert (format tag ""))
        (forward-char (if is-org-mode -8 -6))))))

(after "org-plus-contrib-autoloads"
  (require 'org-drill))


(after "flycheck-autoloads"
  (add-hook 'js-mode-hook
          (lambda () (flycheck-mode t))))

;;;; open-with

(when *is-mac*
  ;; Copied from emacs-prelude
  (defun prelude-open-with ()
    "Simple function that allows us to open the underlying
file of a buffer in an external program."
    (interactive)
    (when buffer-file-name
      (shell-command (concat
                      (if (eq system-type 'darwin)
                          "open"
                        (read-shell-command "Open current file with: "))
                      " "
                      buffer-file-name))))

  (global-set-key (kbd "C-c o") 'prelude-open-with))

(require 'server)
(unless (server-running-p)
  (server-start))

;; Load the local file, if it exists.
(require 'init-local nil t)

(provide 'init)


;; Consider something like this in local:
;;;; Example init-local.el
;;(add-to-list 'exec-path "/usr/local/bin" t)
;;(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;;(setq my-default-theme 'zenburn)
;; (provide 'init-local)

;;; init.el ends here

;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary
