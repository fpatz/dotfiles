;; This is init.el, and it is generated from emacs.org!
(setq package-enable-at-startup nil)
;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.9)
(straight-use-package 'org)
(org-babel-load-file "~/.emacs.d/emacs.org")
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
