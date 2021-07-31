(defface octo-emphasis1 '((t :inherit italic))
  "Face used for simple emphasis.")

(defface octo-shadow-face
  '((t (:inherit shadow)))
  "Face for Octo headers and glue.")

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
  '("title" "subtitle" "visible-if" "selectable-if" "priority" "frequency" "min-choices" "max-choices" "max-visits" "goto" "game-over"))

;; Expression keywords, not sure exactly how to make these highlight only inside expressions, yet.
(defvar octo-keywords
  '("mod" "raise" "raise-by" "lower" "lower-by" "not" "high" "up" "mid" "down" "low" "if" "location" "and" "or"))

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

     ;; stuff between double quotes
     ("\"[^\r\n]*\\?" . font-lock-string-face)

     ;; Field names
     (,(concat "^\\(" (regexp-opt octo-fields 'words) "\\):") 1 font-lock-type-face)
     )))

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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.octo\\'" . octo-mode))

(provide 'octo-mode)
