(setq vc-handled-backends nil)

;; -*- lexical-binding: t; -*-
;; Use a hook, so the message doesn't get clobbered by
;; other messages.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "Emacs ready in %s with %d garbage collections."
    (format "%.2f seconds"
            (float-time
             (time-subtract after-init-time before-init-time)))
    gcs-done)))

;; Install use-package
(straight-use-package 'use-package)

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(add-to-list 'load-path "~/.emacs.d/lisp")

(use-package cus-edit
  :defer t
  :custom
  (custom-file null-device "Don't store customizations"))

(savehist-mode)

(let ((backup-directory "~/.backups"))
  (make-directory backup-directory t)
  (setq backup-directory-alist `(("." . ,backup-directory))))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      vc-make-backup-files t)

(setq create-lockfiles nil)

(with-system darwin
  (setq exec-path-from-shell-arguments nil)
  (use-package exec-path-from-shell :straight t)
  (exec-path-from-shell-initialize)
  (setenv "LANG" "en_US.UTF-8")
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq ns-right-alternate-modifier nil)
  (setq mac-right-option-modifier nil)
  (let ((homebrew-lisp "/usr/local/share/emacs/site-lisp/"))
    (if (file-directory-p homebrew-lisp)
        (let ((default-directory homebrew-lisp))
          (normal-top-level-add-subdirs-to-load-path))))
  (if (boundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode))
)

(when (window-system)
  (set-frame-font "Fira Code")
  (set-face-attribute 'default nil :height 120)
  (load-theme 'material-frank t))

(when (window-system)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (set-frame-parameter nil 'fullscreen 'fullscreen))

(setq initial-scratch-message nil)

(setq org-support-shift-select t)

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq visual-line-mode t)

(setq desktop-path '("~/.emacs.d"))
(setq desktop-restore-frames nil)
(desktop-save-mode 1)

;(use-package powerline :straight t)
;(use-package spaceline :straight t)
;(require 'spaceline-config)
;(spaceline-emacs-theme)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package olivetti :straight t)
(use-package hide-mode-line :straight t)

(defun fp/focus-mode ()
  "Enter focused writing mode"
  (interactive)
  (progn
    (if (bound-and-true-p olivetti-mode)
        (progn
          (olivetti-mode 0)
          (hide-mode-line-mode 0))
      (progn
        (olivetti-mode 1)
        (hide-mode-line-mode 1)
        (olivetti-set-width 85)))))
(global-set-key (kbd "s-o") 'fp/focus-mode)

(global-set-key (kbd "C-x s") 'save-buffer)

(use-package ivy :straight t
  :config
  (ivy-mode 1))

(setq org-directory "~/Dropbox/org")

(setq org-default-notes-file (concat org-directory "/todo.org"))
(setq org-default-journal-file (concat org-directory "/journal.org"))

(setq org-agenda-files
      (list
       org-default-journal-file
       org-default-notes-file
       (concat org-directory "/inbox.org")))

(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "CANCELLED")))

(setq org-log-done 'time)

(setq
   org-capture-templates
   '(
     ("t" "Todo Item"
      entry (file+headline org-default-notes-file "Tasks")
      "* TODO %?\n  %T\n\n  %a"
      :empty-lines 1)
     ("j" "Journal Entry"
      entry (file+olp+datetree org-default-journal-file)
      "* %:description%?\n  %T\n\n  %a\n  %l\n\n  %i"
      :empty-lines 1)))

(setq fp/snippets-file (concat org-directory "/snippets.org"))
(setq fp/links-file (concat org-directory "/links.org"))
(setq org-refile-targets
      `(((,fp/snippets-file) :maxlevel . 1)
        ((,fp/links-file) :maxlevel . 3)
        (org-agenda-files :maxlevel . 2)))
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-outline-path 'file)

(use-package deft
  :straight t
  :bind ("<f9>" . deft)
  :config
  (setq
   deft-extensions '("org" "txt" "rst" "md")
   deft-default-extension "org"
   deft-directory org-directory
   deft-use-filter-string-for-filename t
   ;; The following convienently makes Deft aware
   ;; of #+title lines in Org files
   deft-org-mode-title-prefix t))

(use-package org-bullets
   :straight t
   :init
   (setq org-bullets-bullet-list
         '("\u25C9" "\u25CE" "\u26AB" "\u25CB" "\u25BA" "\u25C7"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-image-actual-width '(400))

(setq org-export-dispatch-use-expert-ui t)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(define-key org-mode-map (kbd "C-c C-x C-7")
  'org-toggle-pretty-entities)

(add-to-list 'org-latex-packages-alist '("" "hologo"))

(let ((custom-entities '(("=TeX=" "=\\hologo{TeX}=" "nil" "TeX =T<sub>e</sub>X=" "-" "-" "-") ("=LaTeX=" "=\\hologo{LaTeX}=" "nil" "TeX =L<sup>a</sup>T<sub>e</sub>X=" "-" "-" "-") ("=XeLaTeX=" "=\\hologo{XeLaTeX}=" "nil" "TeX =X<sub class =\"revcap\">e</sub>L<sup>a</sup>T<sub>e</sub>X=" "-" "-" "-") ("=LuaTeX=" "=\\hologo{LuaTeX}=" "nil" "TeX =LuaT<sub>e</sub>X=" "-" "-" "-") ("=LuaLaTeX=" "=\\hologo{LuaLaTeX}=" "nil" "TeX =LuaL<sup>a</sup>T<sub>e</sub>X=" "-" "-" "-") ("=pdfLaTeX=" "=\\hologo{pdfLaTeX}=" "nil" "TeX =pdfL<sup>a</sup>T<sub>e</sub>X=" "-" "-" "-") ("=BibLaTeX=" "=\\hologo{BibLaTeX}=" "nil" "TeX =BibL<sup>a</sup>T<sub>e</sub>X=" "-" "-" "-"))))
(cl-loop for entity in custom-entities
         do
         (cl-flet
             ((unverb (text) (string-trim text "=" "="))
              (fallback-if-dash (text fallback)
                (if (string= "-" text) fallback text)))
           (let ((entity-string (unverb (nth 0 entity)))
                 (entity-latex (unverb (nth 1 entity)))
                 (entity-math (unverb (nth 2 entity)))
                 (entity-html (unverb (nth 3 entity)))
                 (entity-ascii (unverb (nth 4 entity)))
                 (entity-latin1 (unverb (nth 5 entity)))
                 (entity-utf8 (unverb (nth 6 entity))))
             (add-to-list
              'org-entities-user
              (list entity-string
                    entity-latex
                    (if (string= "t" entity-math) t nil)
                    (if (string-prefix-p "TeX" entity-html)
                        (format "<span class=\"tex\">%s</span>"
                                (unverb (substring entity-html 4)))
                      (unverb entity-latex))
                    (fallback-if-dash entity-ascii entity-string)
                    (fallback-if-dash entity-latin1 entity-string)
                    (fallback-if-dash entity-utf8 entity-string)))))
         )
)

(setq org-hide-macro-markers t)

(setq org-export-global-macros
      `(
        ("index"
         . ,(concat
             "(eval (format \""
             "@@latex: "
             "\\\\index{%1$s}"
             "@@\\n"
             "{{{if-export(html,#+index: %1$s)}}}\\n"
             "\""
             " $1))"))
        ("package"
         . ,(concat
             "{{{index($1@\\texttt{$1} (package))}}}"
             "{{{index(package!$1@\\texttt{$1})}}}"))
        ("binding"
         . ,(concat
             "{{{index(key binding!$1@\\texttt{$1}\\, "
             "\\texttt{$2})}}}"))
        ("tag"
         . ,(concat
             "(eval (format \""
             "@@latex: "
             "{\\\\vspace{0.3cm}\\\\hfill"
             "\\\\footnotesize\\\\texttt{%1$s}$\\\\equiv$}"
             "\\\\vspace{-0.3cm}"
             "@@\n"
             "{{{if-export(html,#+caption: %1$s)}}}\n"
             "#+attr_latex: :options bgcolor=sourcebg"
             "\""
             " $1))"))
        ("if-export"
         . ,(concat
             "(eval (if (org-export-derived-backend-p "
             "org-export-current-backend (intern $1)) $2))"))
        ))

(setq org-export-with-smart-quotes t)

(defvar org-export-output-directory-prefix
  "export_" "prefix of directory used for org-mode export")

(defadvice org-export-output-file-name (before org-add-export-dir activate)
  "Modifies org-export to place exported files in a different directory"
  (when (not pub-dir)
    (setq pub-dir (concat org-export-output-directory-prefix (substring extension 1)))
    (when (not (file-directory-p pub-dir))
      (make-directory pub-dir))))

(use-package tex
  :straight auctex)
(use-package cdlatex
  :straight t)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(require 'org)
(require 'ox-latex)

(setq org-latex-compiler "xelatex")

(add-to-list 'org-latex-packages-alist
             '("AUTO" "babel" t))

(setcdr (assoc "de" org-latex-babel-language-alist) "ngerman")

(setq org-latex-pdf-process
      '("cd %o && latexmk -shell-escape -pdfxe -8bit %b"))

(setq org-preview-latex-default-process 'dvisvgm)
(add-to-list
 'org-preview-latex-process-alist
 '(dvisvgm :programs
           ("latex" "dvisvgm")
           :description "dvi > svg"
           :message "you need to install the programs: latex and dvisvgm."
           :use-xcolor t
           :image-input-type "xdv"
           :image-output-type "svg"
           :image-size-adjust (1.7 . 1.5)
           :latex-compiler
           ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
           :image-converter
           ("dvisvgm %f -n -b min -c %S -o %O")))

(add-to-list
 'org-latex-classes
 '("fpbarticle"
   "\\documentclass[11pt,a4paper]{fpbarticle}
   [DEFAULT-PACKAGES]
   [PACKAGES]
   [EXTRA]
   \\graphicspath{{../}}
   "
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   ("\\paragraph{%s}" . "\\paragraph*{%s}")
   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(setq org-latex-default-class "fpbarticle")

(setq org-latex-listings 'minted)
(add-to-list 'org-latex-minted-langs '(org "md"))
(setq org-latex-minted-options
      '(("breaklines" "true") ("breakafter" "/") ("bgcolor" "sourcebg")))

(use-package ob-mermaid :straight t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (latex . t)
   (mermaid .t)
   (plantuml . t)))
(let
    ((mmdc-binary (locate-file "mmdc" exec-path exec-suffixes 1)))
  (if mmdc-binary
      (setq ob-mermaid-cli-path mmdc-binary)
    (message "Mermaid CLI mmdc not found")))
(setq org-plantuml-exec-mode 'plantuml)
(use-package plantuml-mode :straight t)
(setq plantuml-default-exec-mode 'executable)
(setq plantuml-indent-level 2)
;;(require 'ox-extra)
;;(ox-extras-activate '(ignore-headlines))

(setq org-html-doctype "html5")

(setq org-html-head "
<style type=\"text/css\">
  <!--/*--><![CDATA[/*><!--*/
    @import url('https://fonts.googleapis.com/css2?family=Fira+Code&display=swap');
    @import url('https://fpb-web-resources.s3.eu-central-1.amazonaws.com/tcaps.css');
    body {
        font-family: 'Times New Roman';
        font-size: 16pt;
        background-color: #f8f8f8;
        hyphens: auto;
    }
    pre, code {
        font-family: 'Fira Code';
    }
    code {
        word-wrap: break-word;
        overflow-wrap: anywhere;
        font-size: 85%;
    }
    a {
        text-decoration: none;
        word-wrap: break-word;
        overflow-wrap: anywhere;
    }
    h1, h2, h3, h4 {
        font-weight: bold;
        color: #322d26;
    }
    h2 {
        padding-top: 2em;
        margin-top: 0;
    }
    .subtitle {
        font-size: 12pt;
        font-weight: normal;
    }
    #preamble p {
        font-size: 12pt;
    }
    pre {
        font-size: 70%;
    }
    pre.src {
        background-color: #fff;
        border:0;
        box-shadow: none;
        border-top: 1px solid #ccc;
        border-bottom: 1px solid #ccc;
        margin: 0;
        padding: 8pt;
    }
    pre.src::before {
        font-size: 80%;
        font-style: italic;
        border: 0;
        bottom: 0;
        background-color: inherit;
    }
    span.listing-number {
        display: none;
    }
    label.org-src-name {
        font-size: smaller;
        font-style: italic;
    }
    label.org-src-name + pre.src {
        background-color: #f8f8f7;
        box-shadow: 3px 3px 3px #eee;
        border: 1px solid #ccc;
    }
    .org-comment, .org-comment-delimiter, .org-doc {
        color: #9c6645;
        font-style: italic;
    }
    .org-string {
        color: #477c9c;
    }
    .section-number-1, .section-number-2, .section-number-3,
    .section-number-4, .section-number-5 {
        color: #aaaaaa;
    }
    .dropcap p::first-letter {
        color: #A52A2A;
        float: left;
        font-size: 1.9em;
        margin: 0 .1em 0 0;
        line-height: 1.2;
        font-family: 'Typographer Caps';
    }
    table {
        font-size: 80%;
        border-top: 2px solid #888;
        border-bottom: 2px solid #888;
    }
    thead {
        border-bottom: 1px solid #888;
    }
    th {
        padding-right: 1em;
    }
    td {
        padding-top: 6pt;
        padding-right: 1em;
    }
    #bibliography table {
        background-color: #faf8f5;
    }
    #bibliography h2 {
        display: none;
    }
    td > blockquote {
        font-style: italic;
        margin-top: 4pt;
        margin-left: 0;
    }
    td.bibtexnumber {
        white-space: nowrap;
        text-align: left;
    }
    .responsive {
        overflow-x: auto;
    }
    .org-ref-reference::before {
        content: '[';
    }
    .org-ref-reference::after {
        content: ']';
    }
    div#table-of-contents {
        display: none;
    }
    div#table-of-contents h2 {
        display: none;
    }
    #postamble {
        display: none;
    }
    #text-table-of-contents ul {
        list-style-type: none;
        padding-inline-start: 1em;
        margin-block-start: 0;
    }
    #text-table-of-contents a {
        color: black;
    }
    #text-table-of-contents > ul > li {
        padding-top: .5em;
    }
    .tex {
        font-size: 1em;
    }
    .tex sub {
        text-transform: uppercase;
        font-size: 0.95em;
        vertical-align: -0.5ex;
        margin-left: -0.1667em;
        margin-right: -0.125em;
    }
    .tex sup {
        text-transform: uppercase;
        font-size: 0.75em;
        vertical-align: 0.25em;
        margin-left: -0.36em;
        margin-right: -0.15em;
    }
    .revcap {
        display: inline-block;
        text-transform: uppercase;
        -webkit-transform: rotateY(180deg);
        -moz-transform: rotateY(180deg);
        -ms-transform: rotateY(180deg);
        transform: rotateY(180deg);
    }
    @media only screen and (max-width: 799px) {
        body {
            margin: 5%;
        }
    }
    @media only screen and (min-width: 800px) {
        #content {
            margin-left: 200px;
            padding-left: 4em;
            display: block;
            overflow: auto;
            max-width: 48em;
        }
        div#table-of-contents {
            position: fixed;
            top: 75px;
            left: 0;
            bottom: 0;
            width: 200px;
            padding: 0em 1em 1em 1em;
            font-size: 75%;
            overflow-y: scroll;
            display: block;
        }
        #postamble {
            display: inline-block;
            margin-left: 200px;
            padding-left: 4em;
        }
        .title {
            text-align: left;
            position: fixed;
            font-size: 12pt;
            top: 0;
            left: 0;
            width: 200px;
            margin: 0;
            padding: 1em;
            border-bottom: 2px solid #a52a2a;
        }
    }
  /*]]>*/-->
</style>
")
(setq org-html-head-no-toc "
<style type=\"text/css\">
  <!--/*--><![CDATA[/*><!--*/
    @import url('https://fonts.googleapis.com/css2?family=Fira+Code&display=swap');
    @import url('https://fpb-web-resources.s3.eu-central-1.amazonaws.com/tcaps.css');
    body {
        font-family: 'Times New Roman';
        font-size: 16pt;
        background-color: #f8f8f8;
        hyphens: auto;
    }
    pre, code {
        font-family: 'Fira Code';
    }
    code {
        word-wrap: break-word;
        overflow-wrap: anywhere;
        font-size: 85%;
    }
    a {
        text-decoration: none;
        word-wrap: break-word;
        overflow-wrap: anywhere;
    }
    h1, h2, h3, h4 {
        font-weight: bold;
        color: #322d26;
    }
    h2 {
        padding-top: 2em;
        margin-top: 0;
    }
    .subtitle {
        font-size: 12pt;
        font-weight: normal;
    }
    #preamble p {
        font-size: 12pt;
    }
    pre {
        font-size: 70%;
    }
    pre.src {
        background-color: #fff;
        border:0;
        box-shadow: none;
        border-top: 1px solid #ccc;
        border-bottom: 1px solid #ccc;
        margin: 0;
        padding: 8pt;
    }
    pre.src::before {
        font-size: 80%;
        font-style: italic;
        border: 0;
        bottom: 0;
        background-color: inherit;
    }
    span.listing-number {
        display: none;
    }
    label.org-src-name {
        font-size: smaller;
        font-style: italic;
    }
    label.org-src-name + pre.src {
        background-color: #f8f8f7;
        box-shadow: 3px 3px 3px #eee;
        border: 1px solid #ccc;
    }
    .org-comment, .org-comment-delimiter, .org-doc {
        color: #9c6645;
        font-style: italic;
    }
    .org-string {
        color: #477c9c;
    }
    .section-number-1, .section-number-2, .section-number-3,
    .section-number-4, .section-number-5 {
        color: #aaaaaa;
    }
    .dropcap p::first-letter {
        color: #A52A2A;
        float: left;
        font-size: 1.9em;
        margin: 0 .1em 0 0;
        line-height: 1.2;
        font-family: 'Typographer Caps';
    }
    table {
        font-size: 80%;
        border-top: 2px solid #888;
        border-bottom: 2px solid #888;
    }
    thead {
        border-bottom: 1px solid #888;
    }
    th {
        padding-right: 1em;
    }
    td {
        padding-top: 6pt;
        padding-right: 1em;
    }
    #bibliography table {
        background-color: #faf8f5;
    }
    #bibliography h2 {
        display: none;
    }
    td > blockquote {
        font-style: italic;
        margin-top: 4pt;
        margin-left: 0;
    }
    td.bibtexnumber {
        white-space: nowrap;
        text-align: left;
    }
    .responsive {
        overflow-x: auto;
    }
    .org-ref-reference::before {
        content: '[';
    }
    .org-ref-reference::after {
        content: ']';
    }
    div#table-of-contents {
        display: none;
    }
    div#table-of-contents h2 {
        display: none;
    }
    #postamble {
        display: none;
    }
    #text-table-of-contents ul {
        list-style-type: none;
        padding-inline-start: 1em;
        margin-block-start: 0;
    }
    #text-table-of-contents a {
        color: black;
    }
    #text-table-of-contents > ul > li {
        padding-top: .5em;
    }
    .tex {
        font-size: 1em;
    }
    .tex sub {
        text-transform: uppercase;
        font-size: 0.95em;
        vertical-align: -0.5ex;
        margin-left: -0.1667em;
        margin-right: -0.125em;
    }
    .tex sup {
        text-transform: uppercase;
        font-size: 0.75em;
        vertical-align: 0.25em;
        margin-left: -0.36em;
        margin-right: -0.15em;
    }
    .revcap {
        display: inline-block;
        text-transform: uppercase;
        -webkit-transform: rotateY(180deg);
        -moz-transform: rotateY(180deg);
        -ms-transform: rotateY(180deg);
        transform: rotateY(180deg);
    }
    body {
        margin: 5%;
    }
    #content {
        display: block;
        overflow: auto;
        max-width: 48em;
    }
  /*]]>*/-->
</style>
")

(defun fp/html-filter (text backend info)
  (when (org-export-derived-backend-p backend 'html)
    (if (and
         (not (plist-get info :with-toc))
         (string= (plist-get info :html-head) org-html-head))
        (plist-put info :html-head org-html-head-no-toc)))
  text)
(add-to-list 'org-export-filter-parse-tree-functions 'fp/html-filter)

(setq org-html-htmlize-output-type 'css)

(require 'bibtex)
;;(require 'ox-bibtex)
;; Remove the "cite" entry added by ox-bibtex, otherwise we'll get a
;; warning that it is redefined by org-ref
(setq org-link-parameters
      (assoc-delete-all "cite" org-link-parameters))
(bibtex-set-dialect 'biblatex)
(add-to-list
 'bibtex-biblatex-entry-alist
 '("software" "Software"
   (("title") ("author") ("date") ("url") ("abstract"))
   nil
   "keywords"))
(setq
 my-bibliography-dir (file-name-as-directory "~/References")
 my-bib-file (concat my-bibliography-dir "cslab.bib"))

(use-package org-ref
  :straight t
  :config
  (setq
   reftex-default-bibliography (list my-bib-file)
   org-ref-default-bibliography (list my-bib-file)
   bibtex-completion-bibliography (list my-bib-file)
   org-ref-bibliography-notes (concat my-bibliography-dir "notes.org")
   bibtex-completion-notes-path org-ref-bibliography-notes
   bibtex-completion-pdf-field "file"
   ;; open pdf with system pdf viewer (works on mac)
   bibtex-completion-pdf-open-function
   (lambda (fpath)
     (progn
       (message fpath)
       (start-process "open" "*open*" "open" fpath)))))

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (org-open-file
   (car (bibtex-completion-find-pdf
         (car (org-ref-get-bibtex-key-and-file))))))
(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (python . t)
   (C . t)))
(setq org-confirm-babel-evaluate nil)

(load "org-present.el")
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(org-link-set-parameters
 "message"
 :follow
 (lambda (url)
   (let
       ((goto-url (format "message:%s" url)))
     (message "%s" goto-url)
     (browse-url goto-url))))

(require 'ivy-org-fts)
(setq org-fts-input-args '())
(define-prefix-command 'ctrl-c-o-map)
(global-set-key (kbd "C-c o") 'ctrl-c-o-map)
(global-set-key (kbd "C-c o s") 'ivy-org-fts-search)
(global-set-key (kbd "C-c o f") 'ivy-org-fts-find-org-file)

(use-package yaml-mode
  :straight t)

(use-package pdf-tools
  :straight t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (setq pdf-view-use-scaling t))

(cond ((executable-find "enchant-2")
       (setq-default ispell-program-name "enchant-2"))
      ((executable-find "aspell")
       (setq-default ispell-program-name "aspell"))
      (t
       (message "Neither enchant nor aspell could be found, falling back to ispell")))
;; Make 'ispell-dictionary' safe for local strings, to enable
;; setting the spell check language locally in a file
(put 'ispell-dictionary 'safe-local-variable 'stringp)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(use-package guess-language
  :straight t)

(use-package rg :straight t)

(use-package projectile :straight t)
(projectile-global-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package dsvn :straight t)

(setq compilation-scroll-output t)


(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(require 'compile)
(setq compilation-error-regexp-alist
      (delete 'maven compilation-error-regexp-alist))
(add-to-list
 'compilation-error-regexp-alist-alist
 '(bandit "^ *Location: \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))
(add-to-list 'compilation-error-regexp-alist 'bandit)

(add-hook 'org-mode-hook (lambda () (setq indent-tabs-mode nil)))

(use-package blacken
  :straight t
  :custom
  ;; set this to nil to let black pick up whatever the project
  ;; has configured
  (blacken-line-length nil))
(global-set-key (kbd "C-c b") 'blacken-buffer)

(use-package pyvenv
  :straight t)

(use-package elpy
  :straight t
  :custom
  (elpy-formatter "black")
  :init
  (elpy-enable))

(use-package magit
  :straight t
  :bind (("C-c s" . magit-status)))
(use-package forge :after magit
  :straight t
  :config
  (push '("de-git01.contact.de" "de-git01.contact.de/api/v4"
          "de-git01.contact.de" forge-gitlab-repository)
        forge-alist))

(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c n") 'next-error)

(use-package rjsx-mode
  :straight t)

(use-package go-mode
  :straight t)

(when (require 'mu4e nil 'noerror)
  (require 'mu4e)
  (global-set-key (kbd "s-m") 'mu4e))

(setq
 ;; ** General, UI etc.
 mu4e-use-fancy-chars t
 ;; attempt to show images when viewing messages
 mu4e-view-show-images t
 mu4e-headers-include-related nil
 mu4e-confirm-quit nil
 mu4e-attachment-dir "~/Desktop"
 mu4e-headers-date-format "%Y-%m-%d"
 ;;mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout"
 mu4e-html2text-command "/usr/local/bin/w3m -T text/html"
 ;;mu4e-html2text-command "/usr/local/bin/html2text -utf8 -nobs -width 72"
 mu4e-change-filenames-when-moving t
 mu4e-headers-fields (quote
                      ((:human-date . 12)
                       (:flags . 6)
                       (:from-or-to . 22)
                       (:subject)))
 ;;mu4e-mu-binary     "/usr/local/bin/mu"
 ;;mu4e-maildir       "~/.mail"   ;; top-level Maildir
 mu4e-compose-complete-only-after "2014-01-01"
 ;; mu4e-compose-complete-only-personal t
 mu4e-get-mail-command
 "mbsync work:Inbox work:Archive work:Sent gmail:Inbox"
 message-send-mail-function   'smtpmail-send-it
 mu4e-context-policy 'pick-first
 mu4e-compose-context-policy 'ask
 mu4e-maildir-shortcuts '(("/work/Archive" . ?a)
                          ("/work/Inbox"   . ?i)
                          ("/work/Sent"    . ?s)
                          ("/work/Drafts"  . ?d)
                          ("/gmail/Inbox" . ?g))

 ;; a  list of user's e-mail addresses
 mu4e-user-mail-address-list  '("fp@contact.de"
                                "frank.patz-brockmann@contact-software.com"
                                "frank@contact.de"
                                "frank.patz@contact.de"
                                "frank.patz@gmail.com"
                                "frank.patz@googlemail.com")
 )

(setq epa-pinentry-mode 'loopback)
  (when (require 'mu4e nil 'noerror)
    (setq
     mu4e-contexts
     `( ,(make-mu4e-context
          :name "work"
          :enter-func (lambda () (mu4e-message "fp@contact.de"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg
                           '(:to :from :cc :bcc) "@contact")))
          :vars '(
                  (user-mail-address . "fp@contact.de")
                  (mu4e-reply-to-address . "fp@contact.de")
                  (mu4e-sent-folder . "/work/Sent")
                  (mu4e-drafts-folder . "/work/Drafts")
                  (mu4e-trash-folder . "/work/Gel&APY-schte Elemente")
                  (mu4e-refile-folder . "/work/Archive")
                  (user-full-name . "Frank Patz-Brockmann")
                  (mu4e-compose-signature .
                                          "Frank Patz-Brockmann\nhttp://www.contact-software.com/\n")
                  (smtpmail-smtp-server . "smtp.office365.com")
                  (smtpmail-default-smtp-server . "smtp.office365.com")
                  (smtpmail-queue-mail . nil)
                  (smtpmail-queue-dir . "~/.mail/work/Queue")))
        ,(make-mu4e-context
          :name "gmail"
          :enter-func (lambda () (mu4e-message "frank.patz@gmail.com"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg
                           '(:to :from :cc :bcc) "@g")))
          :vars '(
                  (user-mail-address . "frank.patz@gmail.com")
                  (user-full-name . "Frank Patz-Brockmann")
                  (mu4e-sent-folder . "/gmail/[Google Mail]/.Gesendet")
                  (mu4e-drafts-folder . "/gmail/[Google Mail]/.Entw&APw-rfe")
                  (mu4e-trash-folder . "/gmail/[Google Mail]/.Papierkorb")
                  (mu4e-refile-folder . "/gmail/[Google Mail]/.Alle Nachrichten")
                  (mu4d-compose-signature . "Frank Patz-Brockmann\nfrank.patz@gmail.com\n")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-default-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
                  ;; (smtpmail-local-domain . "gmail.com")
                  ;; (smtpmail-queue-mail . nil)
                  ;; (smtpmail-starttls-credentials . '(("smtp.gmail.com" "587" nil nil)))
                  ;; (smtpmail-auth-credentials . (expand-file-name "~/.authinfo"))
                  ;; (starttls-extra-arguments . nil)
                  ;; (starttls-gnutls-program . "/usr/local/bin/gnutls-cli")
                  ;; (starttls-use-gnutls . t)
                  )))
     ))

(when (require 'mu4e nil 'noerror)
  (add-to-list 'mu4e-view-actions
               '("browser view" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-bookmarks
               '("m:/gmail/Inbox OR m:/work/INBOX"       "Unified Inbox"     ?i))
  ;;store org-mode links to messages
  (require 'org-mu4e)
  ;;store link to message if in header view, not to header query
  (setq org-mu4e-link-query-in-headers-mode nil))

(when (require 'mu4e nil 'noerror)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq org-mu4e-convert-to-html t))

(when (require 'mu4e nil 'noerror)
  (defun my-execute-mu4e-marks ()
    "Execute marks in header mode without asking for confirmation"
    (interactive)
    (mu4e-mark-execute-all t))
  (eval-after-load 'mu4e-headers
    '(define-key mu4e-headers-mode-map "x" 'my-execute-mu4e-marks)))

(require 'org-protocol)
(use-package org-capture-pop-frame
  :straight t
  :config
  (setq
   ocpf-frame-parameters
     '((name . "org-capture-pop-frame")
       (width . 121)
       (height . 30)
       (tool-bar-lines . 0)
       (menu-bar-lines . 0))))

(server-start)
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(setq dired-use-ls-dired nil)

(setq-default indent-tabs-mode nil)
(setq rg-executable-path "rg")

(use-package vterm
  :straight t
  :custom
  (vterm-shell (concat (locate-file "bash" exec-path exec-suffixes 1) " -i")))
