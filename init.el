;;; init.el --- Emacs configuration file

;;; Commentary:



;;; Code:

(require 'cl)

(defconst *config-dir* (file-name-directory load-file-name)
  "Root directory for the configuration")

(defconst *is-mac* (eq system-type 'darwin))

(add-to-list 'load-path (expand-file-name "lisp" *config-dir*))
(add-to-list 'load-path (expand-file-name "local" *config-dir*))

;; (defvar config-dir (file-name-directory load-file-name)
;;   "Root directory for the configuration")

;; (defvar local-config-dir (expand-file-name "local" config-dir)
;;   "Root directory for local specific configurations")

;; (defvar private-config-dir (expand-file-name "private" config-dir)
;;   "Root directory for private configuration")

;; (add-to-list 'load-path local-config-dir)
;; (add-to-list 'load-path private-config-dir)

;; (setq custom-file (expand-file-name "custom.el" local-config-dir))
;; (load custom-file 1)



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

;; User interface

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


;;;; packages



(require 'package)

;;; Also use Melpa for most packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

(package-initialize)


;; (defvar my-packages
;;   '(auto-complete
;;     smex
;;     magit
;;     markdown-mode
;;     paredit
;;     solarized-theme
;;     undo-tree
;;     yasnippet))

;; (unless (every #'package-installed-p my-packages)
;;   (package-refresh-contents)
;;   (mapc #'(lambda (package)
;;             (message "checking package")
;;             (unless (package-installed-p package)
;;               (package-install package)))
;;         my-packages))


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


;;;; ido

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

;;;; magit


(after "magit-autoloads"
  (global-set-key "\C-xg" 'magit-status))


;;;; python

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

(after "flymake-python-pyflakes-autoloads"
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

;; Alternate setup with jedi...
(after "jedi-autoloads"
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t))


;;;; org

;; maybe set this per project?

(after 'org
  (setq org-export-htmlize-output-type 'css)
  (setq org-src-fontify-natively t)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-directory "~/repos/notes/organizer")
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)

  (setq org-default-notes-file "~/repos/notes/organizer/notes.org")
  (define-key global-map "\C-cc" 'org-capture)

  (setq org-capture-templates
        '(("r" "Reference" entry (file "~/repos/notes/reference.org")
           "* %? %^g" :prepend t)
          ("t" "Todo Inbox" entry
           (file+headline "gootodo.org" "Inbox")
           "** %?\n %I")
          ("b" "Bookmark" entry (file "~/repos/notes/bookmarks.org")
           "** %?\n %I")
          ;; ("t" "Todo Inbox" entry (file+headline "~/repos/organizer/gtd.org"
          ;;                                        "Inbox")
          ;;  "** %?\n %I")
          ("w" "Weight" table-line
           (file+headline "log.org" "Weight")
           "| %u | %? | ")
          ("z" "Exercise (table)" table-line
           (file+headline "log.org" "Exercise")
           "| %u | %? | ")
          ("e" "Exercise entry" entry
           (file+headline "log.org" "Exercise")
           "* %U \n   %?")
          ;; ("j" "Journal entry" "entry"
          ;;  (function )
          ;;  )
          ("l" "Log entry" entry
           (file+headline "log.org" "Log 2013")
           "* %U \n   %?")
          ("i" "idea entry" entry
           (file+headline "log.org" "Ideas")
           "* %?")
          ("o" "blOg idea entry" entry
           (file+headline "log.org" "Blog Ideas")
           "* %?")
          ))

  ;; http://doc.norang.ca/org-mode.html#Refiling
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets (quote ((nil :level . 2))))

  ;; (setq org-agenda-files (list
  ;;                         "~/repos/notes/organizer"
  ;;                         "~/repos/notes/organizer/gtd.org"))

  (setq org-agenda-files
        (list "~/repos/notes/gootodo.org"))

  ;; safety issue!!
  (setq org-babel-confirm-evaluate nil)

  ;; have to evaluate stuff separate from the publish process.
  ;; this is probably fine, because most of the time I want to
  ;; run this stuff by hand anyway.
  (setq org-export-babel-evaluate nil)

      ;;; Enable babel languages.  emacs-lisp is loaded by default,
      ;;; but we like python.  ditaa is fun for images, it seems.

  (call-if-fbound 'org-babel-do-load-languages
    'org-babel-load-languages
    '(
      (cpp . nil)
      (ditaa . t)
      (dot . t)
      (emacs-lisp . t)
      (gnuplot . nil) ; need gnuplot-mode
      (haskell . t)
      (latex . t) ; need auctex, reftex?
      (ly . nil) ; lilypond music notation
      (ocaml . t)
      (octave . t) ; needs tuareg
      (python . t)
      (R . t ) ; R, ess-mode
      (scheme . t)
      (sh . t)
      (sqlite . nil)
      ))

  ;;  (require 'htmlize))
  ;;
  (setq org-babel-default-header-args
        (cons '(:noweb . "yes")
              (assq-delete-all :noweb org-babel-default-header-args)))

      ;;; From http://www.brool.com/index.php/using-org-mode-with-gtd
  (setq org-agenda-custom-commands
        '(("w" todo "WAITING" nil)
          ("n" todo "NEXT" nil)
          ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT")))))

  ;; This is for the todo tracking
  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-dim-blocked-tasks 'invisible)

      ;;; From   http://justinlilly.com/emacs/orgmode_static_site_generator.html
;;  (require 'org-publish)
  (setq my-blog-style
        "<link href='http://fonts.googleapis.com/css?family=Trykker|Stoke' rel='stylesheet' type='text/css'>
    ")

  ;; TODO -- fix
  ;; (eval-after-load 'org-publish
  ;;   '(require 'my-org-projects))


  ;; (defun org-dblock-write:my-get-posts (params)
  ;;   (let ((path (plist-get params :path)))
  ;;     (let ((files
  ;;            (mapcar (lambda (f)
  ;;                      (concat (file-name-as-directory path)
  ;;                              f))
  ;;                    (directory-files path nil "\.*org$"))))
  ;;       (setq files (nreverse files))
  ;;       (dolist (file files)
  ;;         (let ((title (org-publish-find-title file)))
  ;;           (message file)
  ;;           (message title)
  ;;           (auto-fill-mode 0)
  ;;           (insert (format " - [[file:%s][%s]]" file title))
  ;;           (auto-fill-mode 1)
  ;;           (newline))))))


  (defun gtd ()
    "quickly load the gtd file"
    (interactive)
    (find-file "~/repos/notes/organizer/gtd.org"))

  (defun my-notes ()
    "quickly load my notes file"
    (interactive)
    (find-file "~/repos/notes/organizer/notes.org"))

  ;; (defun my-org-mode-hook ()
  ;;   (auto-fill-mode))


  (defun my-org-confirm-babel-evaluate (lang body)
    (not (or (string= lang "dot") (string= lang "ditaa")))) ; don't ask for ditaa

  (defun my-mark-dot-ditaa-safe ()
    (interactive)
    (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate))

  ;; (add-hook 'org-mode-hook 'my-org-mode-hook)



;;; From https://github.com/jkitchin/jmax/blob/master/jmax.org

  ;; (after 'org ; here is a way to get pydoc in a link: [[pydoc:numpy]]
  ;;   (setq org-link-abbrev-alist
  ;;         '(("pydoc" . "shell:pydoc %s")))

    ;; these allow me to write mod:numpy or func:numpy.dot to get
    ;; clickable links to documentation

    ;; (org-add-link-type
    ;;  "mod"
    ;;  (lambda (arg)
    ;;    (shell-command (format "pydoc %s" arg) nil))
    ;;  (lambda (path desc format)
    ;;    (cond
    ;;     ((eq format 'latex)
    ;;      (format "\\texttt{%s}" path)))))

    ;; (org-add-link-type
    ;;  "func"
    ;;  (lambda (arg)
    ;;    (shell-command (format "pydoc %s" arg) nil))
    ;;  (lambda (path desc format)
    ;;    (cond
    ;;     ((eq format 'latex)
    ;;      (format "\\texttt{%s}" path)))))
)

;;;; Org publish setup from jkitchin

  ;; (setq org-export-latex-default-packages-alist
  ;;       (quote
  ;;        (("AUTO" "inputenc" t)
  ;;         ("" "fixltx2e" nil)
  ;;         ("" "url")
  ;;         ("" "graphicx" t)
  ;;         ("" "minted" t)
  ;;         ("" "color" t)
  ;;         ("" "longtable" nil)
  ;;         ("" "float" nil)
  ;;         ("" "wrapfig" nil)
  ;;         ("" "soul" t)
  ;;         ("" "textcomp" t)
  ;;         ("" "amsmath" t)
  ;;         ("" "marvosym" t)
  ;;         ("" "wasysym" t)
  ;;         ("" "latexsym" t)
  ;;         ("" "amssymb" t)
  ;;         ("linktocpage,
  ;;   pdfstartview=FitH,
  ;;   colorlinks,
  ;;   linkcolor=blue,
  ;;   anchorcolor=blue,
  ;;   citecolor=blue,
  ;;   filecolor=blue,
  ;;   menucolor=blue,
  ;;   urlcolor=blue" "hyperref" t)
  ;;         ("" "attachfile" t)
  ;;         "\\tolerance=1000")))




  (after 'org-publish

    (setq my-html-preamble
          "
    <div id=\"header\">
    <h1> a mingled cup </h1>
    <ul id='nav'>

    <li><a href='/'>Home</a></li>
    </ul>
    </div>")

    (setq google-tracking-code "")
    (setq my-html-preamble (concat google-tracking-code my-html-preamble)))

  ;;   (push
  ;;    '("octo-posts" .
  ;;      (
  ;;       :base-directory "~/home/www"
  ;;                       :html-table-tag "<table border='2' padding='5'>"
  ;;                       ;;    :html-table-tag "<table border=\"2\" cellspacing=\"10\" cellpadding=\"10\" rules=\"groups\" frame=\"hsides\" rules=\"all\">"
  ;;                       :base-extension "org"
  ;;                       :publishing-directory "~/home/octopress/source/"
  ;;                       :sub-superscript nil
  ;;                       :recursive t
  ;;                       :publishing-function org-publish-org-to-html
  ;;                       :headline-levels 4
  ;;                       :html-extension "html"
  ;;                       :body-only t))
  ;;    org-publish-project-alist)


  ;; ;;; This one pushes other files in org_posts into the source directory
  ;;   (push
  ;;    '("blog-extra" .
  ;;      (:base-directory "~/home/www/"
  ;;                       :publishing-directory "~/home/octopress/source/"
  ;;                       :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|svg"
  ;;                       :publishing-function org-publish-attachment
  ;;                       :recursive t
  ;;                       :author nil
  ;;                       ))
  ;;    org-publish-project-alist
  ;;    )

  ;;   (push `("octo":components ("octo-posts" "octo-extra"))
  ;;         org-publish-project-alist)


  ;;;; This is the configuration for my org-only site, before octopress.
  ;;;; Keeping it here for reference.

    ;; (setq org-export-html-mathjax-options
    ;;       '((path "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
    ;;         (scale "100")
    ;;         (align "center")
    ;;         (indent "2em")
    ;;         (mathml nil)))



    ;; (defun org-dblock-write:my-get-posts (params)
    ;;   "make a list of links for a directory"
    ;;   (let ((path (plist-get params :path)))
    ;;     (let ((files
    ;;            (mapcar (lambda (f)
    ;;                      (concat (file-name-as-directory path)
    ;;                              f))
    ;;                    (directory-files path nil "^[a-zA-z0-9].*\.org$"))))
    ;;       (setq files (nreverse files))
    ;;       (dolist (file files)
    ;;         (let ((title (org-publish-find-title file)))
    ;;           (message file)
    ;;           (message title)
    ;;           (auto-fill-mode 0)
    ;;           (insert (format " - [[file:%s][%s]]" file title))
    ;;           (newline)
    ;;           (auto-fill-mode 1)
    ;;           )))))



    ;; (setq org-publish-project-alist
    ;;       `(("blog"
    ;;          :components ("blog-content" "blog-static"))

    ;;         ;; This is where org generated files will go.
    ;;         ("blog-content"
    ;;          :base-directory "~/home/www/"
    ;;          :base-extension "org"
    ;;          :publishing-directory "~/www/"
    ;;          :recursive t
    ;;          :publishing-function org-publish-org-to-html
    ;;          :export-with-tags nil
    ;;          :headline-levels 4             ; Just the default for this project.
    ;;          :table-of-contents nil
    ;;          :section-numbers nil
    ;;          :sub-superscript nil
    ;;          :todo-keywords nil
    ;;          :author "dave jones"
    ;;          :creator-info nil
    ;;          :html-preamble ,my-html-preamble
    ;;          :html-postamble nil
    ;;          :style
    ;;          "
    ;; <link href='/static/style.css' rel='stylesheet' type='text/css'>
    ;; <link href='/static/css-light.css' rel='stylesheet' type='text/css'>
    ;;   "
    ;;          :timestamp t
    ;;          :exclude-tags ("noexport" "todo")
    ;;          :auto-preamble t)

    ;;         ;; This is where non-org files will go.
    ;;         ("blog-static"
    ;;          :base-directory "~/home/www/static/"
    ;;          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|otf\\|html"
    ;;          :publishing-directory "~/www/static/"
    ;;          :recursive t
    ;;          :publishing-function org-publish-attachment)))


    ;; (setq org-publish-project-alist
    ;;       (append org-publish-project-alist
    ;;               `(("blog-exp"
    ;;                  :components ("blog-content-exp" "blog-static-exp"))

    ;;                 ;; This is where org generated files will go.
    ;;                 ("blog-content-exp"
    ;;                  :base-directory "~/home/www/"
    ;;                  :base-extension "org"
    ;;                  :publishing-directory "~/www-exp/"
    ;;                  :recursive t
    ;;                  :publishing-function org-publish-org-to-html
    ;;                  :export-with-tags nil
    ;;                  :headline-levels 4             ; Just the default for this project.
    ;;                  :table-of-contents nil
    ;;                  :section-numbers nil
    ;;                  :sub-superscript nil
    ;;                  :todo-keywords nil
    ;;                  :author "Dave Jones"
    ;;                  :creator-info nil
    ;;                  :html-preamble ,my-html-preamble
    ;;                  :html-postamble nil
    ;;                  :style
    ;;                  "
    ;; <link href='/static/style.css' rel='stylesheet' type='text/css'>
    ;; <link href='/static/css-light.css' rel='stylesheet' type='text/css'>
    ;;   "
    ;;                  :timestamp t
    ;;                  :exclude-tags ("noexport" "todo")
    ;;                  :auto-preamble t)

    ;;                 ;; This is where non-org files will go.
    ;;                 ("blog-static-exp"
    ;;                  :base-directory "~/home/www/static/"
    ;;                  :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|otf\\|html"
    ;;                  :publishing-directory "~/www-exp/static/"
    ;;                  :recursive t
    ;;                  :publishing-function org-publish-attachment))))




;;;; Shell-script mode


(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;;;; smex

(after "smex-autoloads"
  ;; TODO: set save file
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))



;; Load the local file, if it exists.
(require 'init-local nil t)



(require 'server)
(unless (server-running-p)
  (server-start))


(message "finished loading")

(after "undo-tree-autoloads"
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer--timestamps t)
  (message "foo"))



(require 'uniquify)
(require 'recentf)
(recentf-mode t)
(winner-mode t)

(provide 'init)

;;; init.el ends here

;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary
