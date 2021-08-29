(defface octo-emphasis1 '((t :inherit italic))
  "Face used for simple emphasis.")

(defface octo-bold1 '((t :inherit bold))
  "Face used for bold emphasis.")

(defface octo-shadow-face
  '((t (:inherit shadow)))
  "Face for Octo headers and glue.")

(defface octo-arrow-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for Octo choice arrows.")

(defconst octo-loc-header
  "^\\(?1:#+\\)\\s-*\\(?2:[[:alnum:]_-]+\\)"
  "Regexp identifying Octo location headers.
Group 1 matches the octothorpe signs preceding the title.
Group 2 matches the header title.")

(defconst octo-storylet-header
  "^\\(?1:===\\)\\s-*\\(?2:[[:alnum:]_-]+\\)"
  "Regexp identifying Octo storylet headers.
Group 1 matches the octothorpe signs preceding the title.
Group 2 matches the header title.")

(defconst octo-branch-header
  "^\\(?1:\\-\\-\\-\\)\\s-*\\(?2:[[:alnum:]_-]+\\)"
  "Regexp identifying Octo branch headers.
Group 1 matches the hyphen signs preceding the title.
Group 2 matches the header title.")

(defvar octo-fields
  '("title" "subtitle" "goto"))

;; Expression keywords, not sure exactly how to make these highlight only inside expressions, yet.
(defvar octo-keywords
  '("mod" "raise" "raise-by" "lower" "lower-by" "not" "high" "up" "mid" "down" "low" "if" "then" "else" "location" "and" "or" "true" "false" "yes" "on" "no" "off"))

(defvar octo-expr-fields
  '("visible-if" "priority" "selectable-if" "frequency" "min-choices" "max-choices" "max-visits" "game-over")
  )

(defvar octo-tab-width 4 "Width of a tab for Octo mode")

(defvar octo-font-lock-defaults
  `((
     ("$--.*$" . font-lock-comment-face)

     ;; headers
     (,octo-loc-header
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face))
     (,octo-storylet-header
      (1 'octo-shadow-face)
      (2 font-lock-constant-face))
     (,octo-branch-header
      (1 'octo-shadow-face)
      (2 font-lock-function-name-face))

     ;; Variable properties
     ("^%\s-*\\([[:alnum:]_-]+\\)\\:" 1 font-lock-variable-name-face)

     ;; Quality declarations
     ("^\\s-*\\(?1:VEC\\|INT\\|BOOL\\|RANGE\[[[:alnum:]\.]+\]\\)\\s-+\\(?2:[[:word:]_-]+\\)"
      (1 font-lock-builtin-face)
      (2 font-lock-variable-name-face))

     ;; Italic text
     ("/[^/\r\n]*/" . 'octo-emphasis1)

     ;; Bold text
     ("\\*[^ ][^\*]*?\\*" . 'octo-bold1)

     ;; stuff between double quotes
     ("\"[^\"]*\"" . font-lock-string-face)

     ;; Choices
     ("^\\(?1:\*\\) "
      (1 font-lock-keyword-face)
      ("\\(-\\|#|=\\)>"
       (save-excursion (move-end-of-line 1) (point))
       nil
       (0 'octo-arrow-face t)))

     ;; Field names
     (,(concat "^\\(" (regexp-opt octo-fields 'words) "\\):") 1 font-lock-type-face)

     ;; Expression field names
     (,(concat "^\\(" (regexp-opt octo-expr-fields 'words) "\\):")
      ;; fontify the field name
      (1 font-lock-type-face)
      ;; look for expression keywords after the ":"
      (,(regexp-opt octo-keywords 'words)
       ;; set the limit of the search to the current line only
       (save-excursion (move-end-of-line 1) (point))
       ;; when we've found all the keywords in the region, move back
       ;; to the `:' marker
       (re-search-backward ":")
       ;; fontify matched symbols as keywords
       (0 font-lock-keyword-face))
      ))))

(define-derived-mode octo-mode text-mode "Octo script"
  "Octo mode is a major mode for editing Octo files"
  ;; you again used quote when you had '((mydsl-hilite))
  ;; I just updated the variable to have the proper nesting (as noted above)
  ;; and use the value directly here
  (setq font-lock-defaults octo-font-lock-defaults)

  ;; when there's an override, use it
  ;; otherwise it gets the default value
  (when octo-tab-width
    (setq tab-width octo-tab-width))

  ;; for comments
  ;; overriding these vars gets you what (I think) you want
  ;; they're made buffer local when you set them
  ;;(setq comment-start "$--")
  ;;(setq comment-end "")

  ;; (modify-syntax-entry ?# "< b" mydsl-mode-syntax-table)
  ;; (modify-syntax-entry ?\n "> b" mydsl-mode-syntax-table))
)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.octo\\'" . octo-mode))

(provide 'octo-mode)
