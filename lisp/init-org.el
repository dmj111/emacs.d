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



(provide 'init-org)
