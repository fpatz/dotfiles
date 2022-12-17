(deftheme material-frank
  "Created 2015-12-21.")

(custom-theme-set-faces
 'material-frank
 '(bold ((((class color) (min-colors 89)) (:weight bold))))
 '(bold-italic ((((class color) (min-colors 89)) (:slant italic :weight bold))))
 '(underline ((((class color) (min-colors 89)) (:underline t))))
 '(italic ((((class color) (min-colors 89)) (:slant italic))))
 '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "#ff8A65"))))
 '(font-lock-comment-delimiter-face ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(font-lock-comment-face ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(font-lock-constant-face ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(font-lock-doc-face ((((class color) (min-colors 89)) (:foreground "moccasin"))))
 '(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground "#84ffff"))))
 '(font-lock-keyword-face ((((class color) (min-colors 89)) (:foreground "#fff59d"))))
 '(font-lock-negation-char-face ((((class color) (min-colors 89)) (:foreground "#4dd0e1"))))
 '(font-lock-preprocessor-face ((((class color) (min-colors 89)) (:foreground "gold"))))
 '(font-lock-regexp-grouping-backslash ((((class color) (min-colors 89)) (:foreground "#fff59d"))))
 '(font-lock-regexp-grouping-construct ((((class color) (min-colors 89)) (:foreground "#b39ddb"))))
 '(font-lock-string-face ((((class color) (min-colors 89)) (:foreground "#9ccc65"))))
 '(font-lock-type-face ((((class color) (min-colors 89)) (:foreground "#84ffff"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "#ffcc80"))))
 '(font-lock-warning-face ((((class color) (min-colors 89)) (:weight bold :foreground "#f36c60"))))
 '(shadow ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(success ((((class color) (min-colors 89)) (:foreground "SeaGreen2"))))
 '(error ((((class color) (min-colors 89)) (:foreground "#f36c60"))))
 '(warning ((((class color) (min-colors 89)) (:foreground "#ff9800"))))
 '(match ((((class color) (min-colors 89)) (:foreground "#20252a" :background "#8bc34a" :inverse-video nil))))
 '(isearch ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#8bc34a"))))
 '(lazy-highlight ((((class color) (min-colors 89)) (:foreground "#20252a" :background "#8bc34a" :inverse-video nil))))
 '(isearch-fail ((((class color) (min-colors 89)) (:background "#20252a" :inherit font-lock-warning-face :inverse-video t))))
 '(ido-subdir ((((class color) (min-colors 89)) (:foreground "#b39ddb"))))
 '(ido-first-match ((((class color) (min-colors 89)) (:foreground "#ff9800"))))
 '(ido-only-match ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(ido-indicator ((((class color) (min-colors 89)) (:foreground "#f36c60" :background "#20252a"))))
 '(ido-virtual ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(cursor ((((class color) (min-colors 89)) (:background "#ff9800"))))
 '(fringe ((((class color) (min-colors 89)) (:background "#2a2f33"))))
 '(border ((((class color) (min-colors 89)) (:background "#37474f"))))
 '(vertical-border ((((class color) (min-colors 89)) (:background "#555555" :foreground "#555555"))))
 '(highlight ((((class color) (min-colors 89)) (:inverse-video nil :background "#37474f"))))
 '(mode-line ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#1c1f26"))))
 '(mode-line-buffer-id ((((class color) (min-colors 89)) (:foreground "#ffffff" :background nil))))
 '(mode-line-inactive ((((class color) (min-colors 89)) (:inherit mode-line :foreground "#a7adba" :background "#1c1f26" :weight normal :box nil))))
 '(mode-line-emphasis ((((class color) (min-colors 89)) (:foreground "#ffffff" :slant italic))))
 '(mode-line-highlight ((((class color) (min-colors 89)) (:foreground "#b39ddb" :box nil))))
 '(minibuffer-prompt ((((class color) (min-colors 89)) (:foreground "#4dd0e1"))))
 '(region ((((class color) (min-colors 89)) (:background "#555555"))))
 '(secondary-selection ((((class color) (min-colors 89)) (:background "#bf616a"))))
 '(header-line ((((class color) (min-colors 89)) (:inherit mode-line :foreground "#b39ddb" :background nil))))
 '(trailing-whitespace ((((class color) (min-colors 89)) (:foreground "#f36c60" :inverse-video t :underline nil))))
 '(link ((((class color) (min-colors 89)) (:foreground nil :underline t))))
 '(widget-button ((((class color) (min-colors 89)) (:underline t :weight bold))))
 '(widget-field ((((class color) (min-colors 89)) (:background "#37474f" :box (:line-width 1 :color "#ffffff")))))
 '(compilation-column-number ((((class color) (min-colors 89)) (:foreground "#fff59d"))))
 '(compilation-line-number ((((class color) (min-colors 89)) (:foreground "#fff59d"))))
 '(compilation-mode-line-exit ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(compilation-mode-line-fail ((((class color) (min-colors 89)) (:foreground "#f36c60"))))
 '(compilation-mode-line-run ((((class color) (min-colors 89)) (:foreground "#4dd0e1"))))
 '(org-agenda-structure ((((class color) (min-colors 89)) (:foreground "#81d4fa" :bold t))))
 '(org-agenda-date ((((class color) (min-colors 89)) (:foreground "#4dd0e1" :underline nil))))
 '(org-agenda-done ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(org-agenda-dimmed-todo-face ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(org-block ((((class color) (min-colors 89)) (:foreground "#8bc34a" :background "#21262a" :extend t))))
 '(org-code ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(org-column ((((class color) (min-colors 89)) (:background "#37474f"))))
 '(org-column-title ((((class color) (min-colors 89)) (:inherit org-column :weight bold :underline t))))
 '(org-date ((((class color) (min-colors 89)) (:foreground "#80cbc4" :underline t))))
 '(org-document-info ((t (:foreground "#81d4fa" :height 1.0))))
 '(org-document-info-keyword ((t (:foreground "#8bc34a" :height 1.0))))
 '(org-document-title ((t (:foreground "#ffffff" :weight bold :height 1.0))))
 '(org-done ((((class color) (min-colors 89)) (:foreground "#8bc34a" :bold t :background "#1b5e20"))))
 '(org-ellipsis ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(org-footnote ((((class color) (min-colors 89)) (:foreground "#81d4fa"))))
 '(org-formula ((((class color) (min-colors 89)) (:foreground "#f36c60"))))
 '(org-hide ((((class color) (min-colors 89)) (:foreground "#20252a" :background "#20252a"))))
 '(org-link ((((class color) (min-colors 89)) (:foreground "#3dc0d1" :underline nil))))
 '(org-scheduled ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(org-scheduled-previously ((((class color) (min-colors 89)) (:foreground "#ff9800"))))
 '(org-scheduled-today ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(org-special-keyword ((((class color) (min-colors 89)) (:foreground "#b0bec5"))))
 '(org-table ((((class color) (min-colors 89)) (:foreground "#e3f2fd" :background "#1c1f26"))))
 '(org-todo ((((class color) (min-colors 89)) (:foreground "#ffab91" :bold t :background "#dd2c00"))))
 '(org-upcoming-deadline ((((class color) (min-colors 89)) (:foreground "#ff9800"))))
 '(org-warning ((((class color) (min-colors 89)) (:weight bold :foreground "#f36c60"))))
 '(org-block-begin-line ((((class color) (min-colors 89)) (:height 1.0 :foreground "#888888" :background "#21262a" :extend t))))
 '(org-block-end-line ((((class color) (min-colors 89)) (:height 1.0 :foreground "#888888" :background "#21262a" :extend t))))
 '(org-level-1 ((((class color) (min-colors 89)) (:height 1.0 :weight bold :inherit outline-1))))
 '(org-level-2 ((((class color) (min-colors 89)) (:height 1.0 :inherit outline-2))))
 '(org-level-3 ((((class color) (min-colors 89)) (:height 1.0 :inherit outline-3))))
 '(org-level-4 ((((class color) (min-colors 89)) (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((((class color) (min-colors 89)) (:inherit outline-5))))
 '(org-level-6 ((((class color) (min-colors 89)) (:inherit outline-6))))
 '(org-level-7 ((((class color) (min-colors 89)) (:inherit outline-7))))
 '(org-level-8 ((((class color) (min-colors 89)) (:inherit outline-8))))
 '(biblio-results-header-face ((((class color) (min-colors 89)) (:inherit custom-modified))))
 '(helm-source-header ((((class color) (min-colors 89)) (:inherit custom-modified))))
 '(message-header-other ((((class color) (min-colors 89)) (:foreground nil :background nil :weight normal))))
 '(message-header-subject ((((class color) (min-colors 89)) (:inherit message-header-other :weight bold :foreground "#fff59d"))))
 '(message-header-to ((((class color) (min-colors 89)) (:inherit message-header-other :weight bold :foreground "#ff9800"))))
 '(message-header-cc ((((class color) (min-colors 89)) (:inherit message-header-to :foreground nil))))
 '(message-header-name ((((class color) (min-colors 89)) (:foreground "#4dd0e1" :background nil))))
 '(message-header-newsgroups ((((class color) (min-colors 89)) (:foreground "#81d4fa" :background nil :slant normal))))
 '(message-separator ((((class color) (min-colors 89)) (:foreground "#b39ddb"))))
 '(outline-1 ((((class color) (min-colors 89)) (:inherit nil :foreground "#eceff1"))))
 '(outline-2 ((((class color) (min-colors 89)) (:inherit nil :foreground "#e1f5fe"))))
 '(outline-3 ((((class color) (min-colors 89)) (:inherit nil :foreground "#a5d6a7"))))
 '(outline-4 ((((class color) (min-colors 89)) (:inherit nil :foreground "#ffcc80"))))
 '(outline-5 ((((class color) (min-colors 89)) (:inherit nil :foreground "#b3e5fc"))))
 '(outline-6 ((((class color) (min-colors 89)) (:inherit nil :foreground "CadetBlue1"))))
 '(outline-7 ((((class color) (min-colors 89)) (:inherit nil :foreground "aquamarine1"))))
 '(outline-8 ((((class color) (min-colors 89)) (:inherit nil :foreground "#b39ddb"))))
 '(mu4e-header-face ((((class color) (min-colors 89)) (:foreground "#a7adba" :inherit nil))))
 '(mu4e-header-highlight-face ((((class color) (min-colors 89)) (:underline nil :inherit region :background "#37474f"))))
 '(mu4e-header-marks-face ((((class color) (min-colors 89)) (:underline nil :foreground "#fff59d"))))
 '(mu4e-flagged-face ((((class color) (min-colors 89)) (:foreground "#ff9800" :inherit nil))))
 '(mu4e-forwarded-face ((((class color) (min-colors 89)) (:foreground "#81d4fa" :inherit nil))))
 '(mu4e-replied-face ((((class color) (min-colors 89)) (:foreground "#8bc34a" :inherit nil))))
 '(mu4e-unread-face ((((class color) (min-colors 89)) (:foreground "#ffffff" :inherit nil))))
 '(mu4e-cited-1-face ((((class color) (min-colors 89)) (:inherit outline-1 :slant normal))))
 '(mu4e-cited-2-face ((((class color) (min-colors 89)) (:inherit outline-2 :slant normal))))
 '(mu4e-cited-3-face ((((class color) (min-colors 89)) (:inherit outline-3 :slant normal))))
 '(mu4e-cited-4-face ((((class color) (min-colors 89)) (:inherit outline-4 :slant normal))))
 '(mu4e-cited-5-face ((((class color) (min-colors 89)) (:inherit outline-5 :slant normal))))
 '(mu4e-cited-6-face ((((class color) (min-colors 89)) (:inherit outline-6 :slant normal))))
 '(mu4e-cited-7-face ((((class color) (min-colors 89)) (:inherit outline-7 :slant normal))))
 '(mu4e-ok-face ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(mu4e-highlight-face ((((class color) (min-colors 89)) (:inherit highlight))))
 '(mu4e-title-face ((((class color) (min-colors 89)) (:inherit nil :foreground "#8bc34a"))))
 '(custom-variable-tag ((((class color) (min-colors 89)) (:foreground "#4dd0e1"))))
 '(custom-group-tag ((((class color) (min-colors 89)) (:foreground "#4dd0e1"))))
 '(custom-state ((((class color) (min-colors 89)) (:foreground "#8bc34a"))))
 '(default ((((class color) (min-colors 89)) (:foreground "#dedede" :background "#20252a")))))

(provide-theme 'material-frank)
