(defface twine-passage-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for passages.")

(setq twine-keywords '("display" "if" "endif" "else" "silently" "endsilently" "set" "becomes" "replace" "endreplace" "gains" "continue" "endcontinue") )
(setq twine-keywords-regexp (regexp-opt twine-keywords 'words))

(setq twine-keywords nil)

(setq twine-font-lock-keywords
  `(
    (,"\\[\\[[^\]]+\\]\\]" . font-lock-constant-face)
    (,"\:\:.*" . 'twine-passage-face)
    (,"\$[a-z]+" . font-lock-variable-name-face)
    (,twine-keywords-regexp . font-lock-keyword-face)
    (,"\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\"" . font-lock-string-face)
    ;; note: order above matters. “mylsl-keywords-regexp” goes last because
    ;; otherwise the keyword “state” in the function “state_entry”
    ;; would be highlighted.
))

(define-derived-mode twine-mode text-mode "Twine"
  "Twine mode is a major mode for editing tws files"
  (setq font-lock-defaults '((twine-font-lock-keywords)))
  (setq twine-keywords-regexp nil)
  )

(provide 'twine-mode)
