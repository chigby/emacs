(defface twine-passage-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for passages.")

(setq sugarcube-macros '("capture" "set" "unset" "remember" "forget" "if" "run" "script" "include" "nobr" "silently" "replacelink" "becomes" "else" "elseif" "continuelink" "insertlink" "randomizelink" "revertlink" "reviselink" "linkreplace"))
(setq sugarcube-macros-regexp
      (concat "\\(<<\\)/?" (regexp-opt sugarcube-macros 'words)))
(setq thing (concat "\\(<<\\)" sugarcube-macros-regexp))

(define-derived-mode sugarcube-mode text-mode "Sugarcube mode"
  "Twine mode is a major mode for editing tws files")

(font-lock-add-keywords
 'sugarcube-mode
 `((,"\:\:.*" . 'twine-passage-face)
   (,"\\[\\[[^\]]+\\]\\]" . font-lock-constant-face)
   (,"\$[a-z]+" . font-lock-variable-name-face)
   (,sugarcube-macros-regexp
     (1 font-lock-keyword-face)
     (2 font-lock-type-face)
     ("\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\""
      (save-excursion (re-search-forward ">>"))
      (re-search-backward "<<")
      (0 font-lock-string-face))
     (">>" (save-excursion (re-search-forward ">>")) (re-search-backward "<<") (0 font-lock-keyword-face)))
   ))

(provide 'sugarcube-mode)
