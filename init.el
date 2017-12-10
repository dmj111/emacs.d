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
  "Root directory for the configuration.")
(defconst *local-dir* (expand-file-name "local" *config-dir*)
  "Root directory for local configuation.")

;; Keep the custom file in local-dir so it is out of git's way, and
;; doesn't mess with this file.
(setq custom-file (expand-file-name "custom.el" *local-dir*))

(defconst *is-mac* (eq system-type 'darwin))

(defvar *anaconda-directory* "/Users/dave/anaconda"
  "Anaconda installation directory.")

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

;; http://pragmaticemacs.com/emacs/use-your-digits-and-a-personal-key-map-for-super-shortcuts/
;; unset C- and M- digit keys
(dotimes (n 9)
  (global-unset-key (kbd (format "C-%d" (+ 1 n))))
  (global-unset-key (kbd (format "M-%d" (+ 1 n)))))

(define-prefix-command 'dmj-map)
(global-set-key (kbd "C-1") 'dmj-map)

(define-key dmj-map (kbd "r") 'recompile)

(defun dmj-test-foo()
  "This is a silly test"
  (interactive)
  (message "foo"))
(define-key dmj-map (kbd "[") 'dmj-test-foo)
(define-key dmj-map (kbd "l") 'delete-trailing-whitespace)
(define-key dmj-map (kbd "w") 'whitespace-mode)



(require 'package)
(package-initialize)

;; Also use Melpa for most packages

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
;; (when *is-mac*
  ;; (add-to-list 'package-archives
  ;;              '("marmalade" . "http://marmalade-repo.org/packages/") t))
(require 'package)
(add-to-list 'package-archives
         '("melpa" . "http://melpa.org/packages/") t)

(when (not package-archive-contents)
    (package-refresh-contents))

;; make sure use-package is loaded
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package zenburn-theme :ensure t)

(defvar my-default-theme 'zenburn
  "Default theme to use at startup.")

;; After init.el is loaded, set the theme.
(eval-after-load 'init
  '(progn
     (message "loading the theme...")
     ;; To change, make sure the package is loaded, and then set
     ;; my-default-theme.
     (load-theme my-default-theme t)))


(use-package dash :ensure t)
(use-package markdown-mode :ensure t)

;; http://danmidwood.com/content/2014/11/21/animated-paredit.html
(use-package paredit :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode))))

;;;; Theme


;;;; Keybindings
(global-set-key [(f5)] 'call-last-kbd-macro)
(global-set-key "\C-x\C-n" 'next-error)
(global-set-key (quote [S-M-down])  'next-error)
(global-set-key (quote [S-M-up])  'prev-error)

(global-set-key "\M-?" 'help)
(global-set-key "\C-cx" 'compile)
(global-set-key [(f9)] 'recompile)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
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
  :disabled t
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
  :ensure t
  :bind (("\C-xg" . magit-status))
  :config

  ;; (eval-after-load 'swiper
  ;;   (setq magit-completing-read-function 'ivy-completing-read))

  ;; full-scrreen magit-status
  ;; from magnars --
  ;;    https://github.com/magnars/.emacs.d/blob/master/setup-magit.el
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows)))


(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (projectile-global-mode t))


;;;; python
(use-package python
  :ensure t
  :init
  (defun my-python-mode-hook ()
    "Stuff to run when python-mode loads"
    (setq-default py-indent-offset 4)
    (setq indent-tabs-mode nil)
    ;;(setq py-python-command nil)
    ;;(require 'virtualenv)
    (transient-mark-mode t)
    (message "ran my-python-mode-hook"))

  (add-hook 'python-mode-hook 'my-python-mode-hook)
  ;; (use-package flymake-python-pyflakes-autoloads
  ;;   :init
  ;;   (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

  ;; (use-package jedi-autoloads
  ;;   :init
  ;;   (add-hook 'python-mode-hook 'jedi:setup)
  ;;   (setq jedi:setup-keys t)
  ;;   (setq jedi:complete-on-dot t))
  )

;;;; Shell-script mode

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;;;; elisp

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)


(use-package recentf
  :ensure t
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

(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))


;; Used this at previous job - not sure if still want it.
;; (add-hook 'prog-mode-hook
;;           (lambda () (add-hook 'before-save-hook
;;                                'whitespace-cleanup nil t)))

;;;; uniquify

;; (use-package uniquify
;;   :config
(setq uniquify-buffer-name-style 'forward)

;;;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer--timestamps t))


;;;; smex
(use-package smex
  :ensure t
  :config
  (smex-initialize))


;;;; js2
(use-package js2-mode
  :ensure t
  :mode (("\\.json\\'" . js-mode)
         ("\\.js\\'" . js2-mode))
  :init
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (setq js2-highlight-level 3)

  (use-package ac-js2
    :init
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
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'snippet-mode-hook 'yas-minor-mode)

  :config
  (let ((local (expand-file-name "snippets" *local-dir*)))
    (when (file-exists-p local)
      (add-to-list 'yas-snippet-dirs local)))
  (let ((local (expand-file-name "snippets" *config-dir*)))
    (when (file-exists-p local)
      (add-to-list 'yas-snippet-dirs local)))
  (yas-reload-all)
  (yas-global-mode -1)
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
(use-package cpputils-cmake
  :ensure t)


(use-package google-c-style
  :ensure t
  :init
  (add-hook 'c-mode-common-hook 'google-set-c-style))

(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)

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
        (forward-char (if is-org-mode -8 -6)))))

  (setq org-export-htmlize-output-type 'css)
  (setq org-src-fontify-natively t)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-directory "~/repos/private/notes")
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)

  (setq org-default-notes-file "log.org")
  (define-key global-map "\C-cc" 'org-capture)

  (setq dmj-org-capture-templates
        '(("r" "Reference" entry (file "reference.org")
           "* %? %^g" :prepend t)
          ;; ("t" "Todo Inbox" entry (file+headline "" "Todos")
          ;;  "* TODO %?\n  Added %u\n  %i" :prepend t)
          ("b" "Bookmark" entry (file "bookmarks.org")
           "* %?\n %I")
          ;; http://members.optusnet.com.au/~charles57/GTD/datetree.html
          ("n" "Notes inbox" entry (file+headline "log.org" "Inbox")
           "* %^{Description} %^g %?
Added: %U")
          ("t" "make a task" entry (file+datetree "log.org")
           "* TODO %^{Description} %^g
 %?
Added: %U")))

  (setq org-capture-templates dmj-org-capture-templates)
  ;; http://doc.norang.ca/org-mode.html#Refiling
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets (quote ((nil :maxlevel . 3))))


  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sh . t)
     (emacs-lisp . t)
     ))

)

;; (use-package org-plus-contrib-autoloads
;;   :commands org-drill)

;; (use-package org-drill
;;   :config
;;   (setq org-drill-add-random-noise-to-intervals-p t))

(use-package flycheck
  :init
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


;; [ ] https://github.com/abo-abo/swiper
;; [ ] http://oremacs.com/swiper/
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x b" . ivy-switch-buffer)
   :map ivy-mode-map
   ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-ignore-order)))
  ;;(setq ivy-count-format "(%d/%d) ")
  )



(use-package counsel :ensure t)
;; commands
;; counsel-ag grep
;; counsel-recentf
;; counsel-git-grep
;; counsel-git find file
;; counsel-describbindings
;; counsel-yank-pop
;; counsel-projectile

(use-package swiper
  :ensure t
  ;; C-j to select current
  ;; C-M-j to select current value (creat new file)
  ;; M-j to select word at point.
  :bind (("C-s" . swiper)
         ("C-x C-r" . counsel-recentf)
         ("M-x" . counsel-M-x)
         ("\C-x\C-m" . counsel-M-x)
         ("\C-xm" . counsel-M-x)
         ("\C-c\C-m" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-load-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rhythmbox)
         ("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  )

;; [ ] http://tuhdo.github.io/helm-intro.html
(use-package helm
  :disabled t
  :config
  (require 'helm-config)
  (helm-mode 1)
  ("M-x" . 'undefined)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
  ;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (message "used helm"))


(use-package helm
  :disabled t
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

  (helm-mode 1))

(use-package avy
  :ensure t
  :bind (
         ("C-:" . avy-goto-char)
         :map dmj-map
         ("w" . avy-goto-word-1)
         ("1" . avy-goto-char-timer)))

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


;; http://stackoverflow.com/questions/20967818/emacs-function-to-case-insensitive-sort-lines
(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))


;; http://stackoverflow.com/a/13408008
(use-package ansi-color
  :ensure t
  :init
  (defun colorize-compilation-buffer ()
    "Colorize compiler output."
    (read-only-mode -1)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode 1))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))



(global-set-key (kbd "M-i") 'imenu)

;;(set-frame-font "Cousine-14" nil t)


(unless t
  (set-frame-font "inconsolata-14")
  (set-frame-font "monaco-12")
  (set-frame-font "menlo-12")
  (set-frame-font "-*-Andale Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
  (set-frame-font "Cousine-14")
  (set-frame-font "source code pro-12")
  ;; Test string for fonts:
  ;; abcdefghijklmnopqrstuvqxyz
  ;; ABCDEFGHIJKLMNOPQRSTUVQXYZ
  ;; 0123456789
  ;; O, o, 0
  ;; 1 I i L l
  ;; 5 S s
  ;; 2 Z z
  ;; ( { [ ] } )
)

(use-package golden-ratio
  :disabled t
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode 1))

;; google-this
(use-package google-this
  :config
  (google-this-mode 1))

(use-package wttrin
  :commands (wttrin)
  :config
  (setq wttrin-default-cities '("State College, Pennsylvania")))


;; https://github.com/abo-abo/hydra
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))
  )


(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '("http://pragmaticemacs.com")))



(use-package conda
  :ensure t
  :config
  ;; Make sure pylint is installed.
  ;; make sure a default environment is set
  ;;   conda env export > environment.yml
  ;; https://github.com/necaris/conda.el
  ;; if you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation (see below for details), include:
  (conda-env-autoactivate-mode t)
  ;; If your Anaconda installation is anywhere other than the default (~/.anaconda3) then set the conda-anaconda-home custom variable to the installation path. For instance, in my configuration I have:

  (custom-set-variables
   '(conda-anaconda-home *anaconda-directory*)))


(use-package flycheck
  :ensure t
  :init
  (defun flycheck-python-setup ()
    (flycheck-mode))
  (add-hook 'python-mode-hook #'flycheck-python-setup))




;; http://jblevins.org/log/mmm
(defun my-mmm-markdown-auto-class (lang &optional submode)
  "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
  (let ((class (intern (concat "markdown-" lang)))
        (submode (or submode (intern (concat lang "-mode"))))
        (front (concat "^```" lang "[\n\r]+"))
        (back "^```"))
    (mmm-add-classes (list (list class :submode submode :front front :back back)))
    (mmm-add-mode-ext-class 'markdown-mode nil class)))


(use-package mmm-mode
  :ensure t
  :config
  (setq mmm-global-mode 'maybe)

  ;; Note: rememb
  ;; Mode names that derive directly from the language name
  (mapc 'my-mmm-markdown-auto-class
        '("c" "cpp" "css" "html" "latex" "lisp" "makefile"
          "markdown" "python" "r" "xml"))
  (global-set-key (kbd "C-c m") 'mmm-parse-buffer)
  )



(add-to-list 'exec-path "/usr/local/opt/llvm/bin/" t)

(use-package flycheck
  :ensure t
  :config
  ;; TODO [ ] https://github.com/abo-abo/hydra/wiki/Flycheck
  ;; Force flycheck to always use c++11 support. We use
  ;; the clang language backend so this is set to clang
  ;; (add-hook 'c++-mode-hook
  ;;           (lambda () (setq flycheck-clang-language-standard "c++11")))
  ;; Turn flycheck on everywhere
  (global-flycheck-mode)

  (use-package flycheck-pyflakes
    :ensure t))


;; Load rtags and start the cmake-ide-setup process
(use-package rtags
  :disabled t
  :config
  ;; Set rtags to enable completions and use the standard keybindings.
  ;; A list of the keybindings can be found at:
  ;; http://syamajala.github.io/c-ide.html

  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (rtags-enable-standard-keybindings)

  (use-package company-rtags
    :ensure t
    :config
    (setq rtags-completions-enabled t)
    (eval-after-load 'company
      '(add-to-list
        'company-backends 'company-rtags))
    (setq rtags-autostart-diagnostics t)
    (rtags-enable-standard-keybindings))

  ;; Load the local file, if it exists.
  )
;; (use-package heml-rtags
;;   :ensure t
;;   :config
;;   (setq rtags-use-helm t))

(use-package company
  :disabled t
  :config
  (global-company-mode 1))

(use-package clang-format
  :ensure t
  :config
  (add-hook 'c++-mode-hook (lambda ()
                             (add-hook 'before-save-hook #'clang-format-buffer nil t)))
  (global-set-key [C-M-tab] 'clang-format-region))

(use-package ggtags
  :disabled t)


(use-package cc-mode
  :config
  (use-package semantic
    :disabled t
    :config
    (global-semanticdb-minor-mode 1)
    (global-semantic-idle-scheduler-mode 1)
    (global-semantic-stickyfunc-mode 1)

    (semantic-mode 1)

    (defun alexott/cedet-hook ()
      (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
      (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))
    (add-hook 'c-mode-common-hook 'alexott/cedet-hook)
    (add-hook 'c-mode-hook 'alexott/cedet-hook)
    (add-hook 'c++-mode-hook 'alexott/cedet-hook)))


;; Enable EDE only in C/C++
(use-package ede
  :disabled t
  :config
  (global-ede-mode))

(use-package helm-gtags
  :disabled t
  :init
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\C-cg"
        helm-gtags-suggested-key-mapping t)
  ;; Enable helm-gtags-mode in Dired so you can jump to any tag
  ;; when navigate project tree with Dired
  (add-hook 'dired-mode-hook 'helm-gtags-mode)

  ;; Enable helm-gtags-mode in Eshell for the same reason as above
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)

  ;; Enable helm-gtags-mode in languages that GNU Global supports
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'java-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)

  ;; key bindings
  (with-eval-after-load 'helm-gtags
    (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
    (define-key helm-gtags-mode-map (kbd "C-c g C-j") 'helm-gtags-select)
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)))


(use-package move-text
  :ensure t
  :config
  (global-set-key [\M-\S-up] 'move-text-up)
  (global-set-key [\M-\S-down] 'move-text-down))

(use-package wgrep
  :ensure t)

;; wgrep replacement :
;; https://sam217pa.github.io/2016/09/11/nuclear-power-editing-via-ivy-and-ag/
;; counsel-ag,
;; C-c C-o ivy occur
;; C-x C-q - wgrep
;;


;; http://syamajala.github.io/c-ide.html

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
