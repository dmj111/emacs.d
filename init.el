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
;; ace-jump

;;;; TO LEARN
;; TODO: ace-jump
;; http://www.emacswiki.org/emacs/ImenuMode#toc10 -ido powered imenu
;; http://masteringemacs.org/article/effective-editing-movement ETAGS
;; C-x C-n / C-u C-x C-n  set /unset goal column
;; subword mode for Camel Case
;;
;; - kmacro-set-counter and kmacro-insert-counter
;;   looks like my C-x C-k keybindings interfere...
;; - try avy mode instead of acejump
;; -
;; TODO
;; - js2 setup
;; -  http://emacs.stackexchange.com/questions/2867/how-should-i-change-my-workflow-when-moving-from-ido-to-helm

;;; Code:

;;;; Debugging settings.

;; Debug if there is an error
;; (setq debug-on-error t)

;; Don't limit the print out of a variable
(setq eval-expression-print-length nil)

(setq inhibit-startup-screen t)

;; Turn off mouse interface early in startup to avoid momentary display
;; The macro is for non-windowed emacs.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; make yes/no shorter, and a frequent alias.
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp)

;; Give names to some config directories.
(defconst *config-dir* (file-name-directory load-file-name)
  "Root directory for the configuration")
(defconst *local-dir* (expand-file-name "local" *config-dir*)
  "Root directory for local configuation")

;; Keep the custom file in local-dir so it is out of git's way, and
;; doesn't mess with this file.
(setq custom-file (expand-file-name "custom.el" *local-dir*))

(defconst *is-mac* (eq system-type 'darwin))

;; Add a local lisp directory to the load path.
(add-to-list 'load-path *local-dir*)

;; Delete soon
;; Taken from http://milkbox.net/note/single-file-master-emacs-configuration/
;;
;; Make eval-after-load a little nicer.  For elpa autoloads, call it
;; with a string name.  For require's, call it with a symbol.
;; (defmacro after (mode &rest body)
;;   "`eval-after-load' MODE evaluate BODY."
;;   (declare (indent defun))
;;   `(eval-after-load ,mode
;;      '(progn
;;         (message "after loading %s" ,mode)
;;         ,@body)))

;; Try to load local settings ahead of time
(require 'init-local-preload nil t)

;;;; Utilities


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

;;;; package stuff..
(require 'cl)


(require 'package)
(package-initialize)

;; Also use Melpa for most packages

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)


;; make sure use-package is loaded
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)


(use-package zenburn-theme :ensure t)
(defvar my-default-theme 'zenburn
  "default theme to use at startup")

;; After init.el is loaded, set the theme.
(eval-after-load 'init
  '(progn
     (message "loading the theme...")
     ;; To change, make sure the package is loaded, and then set
     ;; my-default-theme.
     (load-theme my-default-theme t)))


(use-package dash
  :ensure t)


(use-package dash :ensure t)
(use-package markdown-mode :ensure t)
(use-package paredit :ensure t)


;;;; Theme




;;;; Keybindings
(global-set-key [(f5)] 'call-last-kbd-macro)
(global-set-key "\C-x\C-n" 'next-error)
(global-set-key (quote [S-M-down])  'next-error)
(global-set-key (quote [S-M-up])  'prev-error)

(global-set-key "\M-?" 'help)
(global-set-key "\C-cx" 'compile)
(global-set-key [(f9)] 'recompile)

;; Hmm... isearch.  and regexp.
;; (global-set-key "\M-s" 'isearch-forward-regexp)
;; (global-set-key "\M-r" 'isearch-backward-regexp)

;; backward-kill-word is a fast way to delete
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;;; Hippie-expand
;; From emacs-prelude
;; hippie expand is dabbrev expand on steroids

(global-set-key [remap dabbrev-expand] 'hippie-expand)

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

;; TODO - detect electric indent, and don't bother if it exists
(define-key global-map (kbd "RET") 'newline-and-indent)


;; backward-kill-word is a fast way to delete
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; This is for when alt is not meta.   I need my meta.
(setq x-alt-keysym 'meta)

(when *is-mac*
  (setq mac-command-modifier 'meta))

(use-package ido
  ;; Consider the settings that prelude uses.
  ;; -- look at ido-ubiquitous
  ;; -- flx-ido
  :config 
  ;; https://www.masteringemacs.org/article/introduction-to-ido-mode
  ;; Ido settings.  I am not real familiar with these yet.
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-create-new-buffer 'always)
  (ido-mode 1)


  ;; This one might not be fun.  If point is on something that looks
  ;; like a filename, ido assumes that is what you want.
  (setq ido-use-filename-at-point 'guess)

  ;; Avoid pinging with C-x f
  (setq ffap-machine-p-known 'reject)

  ;; (setq ido-use-url-at-point t)
  ;; this can give preferences to more common files.
  ;; (setq ido-file-extensions-order '(".py" ".cc"))
  ;; look for docs in ido-find-file, ido-find-dir, etc
  )

;;;; magit
(use-package magit
  ;; :ensure t
  :bind (("\C-xg" . magit-status)
         :map magit-status-mode-map
         ("q" . magit-quit-session))
  :config

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

;;;; python
(use-package python
  :config
  (defun my-python-mode-hook ()
    "Stuff to run when python-mode loads"
    (setq-default py-indent-offset 4)
    (setq indent-tabs-mode nil)
    ;;(setq py-python-command nil)
    ;;(require 'virtualenv)
    (transient-mark-mode t)
    (message "ran my-python-mode-hook"))

  (add-hook 'python-mode-hook 'my-python-mode-hook)

  (use-package flymake-python-pyflakes-autoloads
    :config
    (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

  (use-package jedi-autoloads
    :config
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:setup-keys t)
    (setq jedi:complete-on-dot t)))

;;;; Shell-script mode

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;;;; elisp

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)


(use-package recentf
;;;; recentf
  :bind
  (("C-x C-r" . recentf-ido-find-file))
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15)
  (recentf-mode 1)
  ;; (move to another file to setup an autoload?)
  (defun recentf-ido-find-file ()
    "Find recent file with ido."
    (interactive)
    (let ((file (ido-completing-read
                 "Chose recent file:"
                 (-map 'abbreviate-file-name recentf-list) nil t)))
      (when file (find-file file)))))




;;; Custom file
(when (file-exists-p custom-file)
  (load custom-file))

;;;; programming mode
;; TODO: flyspell on mac!!
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Used this at previous job - not sure if still want it.
;; (add-hook 'prog-mode-hook
;;           (lambda () (add-hook 'before-save-hook
;;                                'whitespace-cleanup nil t)))

;;;; uniquify
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;;;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer--timestamps t))



;;;; smex
(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))



;;;; js2
(use-package js2-mode
  :mode (("\\.json\\'" . js-mode)
         ("\\.js\\'" . js2-mode))
  
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (setq js2-highlight-level 3)

  (use-package ac-js2
    :config
    (add-hook 'js2-mode-hook 'ac-js2-mode)))

;;;; auto-complete
(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories
               (expand-file-name "ac-dict" *config-dir*))
  (ac-config-default)
  ;; Trigger key
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>"))

;;;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode -1)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'snippet-mode-hook 'yas-minor-mode)  
  (let ((local (expand-file-name "snippets" *local-dir*)))
    (when (file-exists-p local)
      (add-to-list 'yas-snippet-dirs local)))
  (let ((local (expand-file-name "snippets" *config-dir*)))
    (when (file-exists-p local)
      (add-to-list 'yas-snippet-dirs local)))
  (setq yas-prompt-functions
        '(yas-ido-prompt
          yas-x-prompt
          yas-dropdown-prompt
          yas-completing-prompt
          yas-no-prompt)))


;;;; c++

(use-package c++-mode
  :mode "\\.h\\'")


;;;; cpputils-cmake
(use-package cpputils-cmake)

(use-package google-c-style
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style))

(use-package org
  :bind (([f6] . org-capture))
  :config
  (message "first use-package-org")
  
  (add-hook 'org-mode-hook (lambda ()
                             (auto-fill-mode 1)))

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

(use-package org-plus-contrib-autoloads
  :commands org-drill)

(use-package org-drill
  :config
  (setq org-drill-add-random-noise-to-intervals-p t))

(use-package flycheck
  :config
  (add-hook 'js-mode-hook
          (lambda () (flycheck-mode t))))


(use-package ace-window
  :bind ([(f12)] . ace-window))


(use-package ace-jump-mode
  :config
  (add-hook 'ace-jump-mode-before-jump-hook
            (lambda () (push-mark (point) t))))

;; http://irreal.org/blog/?p=760
(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package lacarte
  :bind ([?\M-`] . lacarte-execute-command))



(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; (use-package swiper
;;   :bind (("C-s" . swiper)
;;          ("M-x" . counsel-M-x)
;;          ("C-x C-f" . counsel-find-file)
;;          ("<f1> f" . counsel-describe-function)
;;          ("<f1> v" . counsel-describe-variable)
;;          ("<f1> l" . counsel-load-library)
;;          ("<f2> i" . counsel-info-lookup-symbol)
;;          ("<f2> u" . counsel-unicode-char)
;;          ("C-c g" . counsel-git)
;;          ("C-c j" . counsel-git-grep)
;;          ("C-c k" . counsel-ag)
;;          ("C-x l" . counsel-locate)
;;          ("C-S-o" . counsel-rhythmbox))
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-height 10)
;;   (setq ivy-count-format "(%d/%d) "))


(when nil
  (use-package helm
    :config
    (require 'helm-config)
    (helm-mode 1)
    ("M-x" . 'undefined)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
    ;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (message "used helm")))

  

(when nil
  (use-package helm
    :config
    (require 'helm-config)
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.

    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t)

    (helm-mode 1)))

(use-package avy
  :ensure t
  :bind (;; ("M-s" . avy-goto-word-1)
         ("C-:" . avy-goto-char)))



;;;; winner
(winner-mode t)

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

(defun clear-kill-ring ()
  (interactive)
  (setq kill-ring nil)
  (garbage-collect))

(defalias 'list-buffers 'ibuffer)

;; server
(require 'server)
(unless (server-running-p)
  (server-start))


(setq apropos-sort-by-scores t)
;; Load the local file, if it exists.

(global-set-key (kbd "M-i") 'imenu)
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


