;;; chn-elm.el --- Elm Language Support

(use-package elm-mode
  :bind
  (:map elm-mode-map
        ("C-c C-c" . elm-compile-main))
  :hook
  (elm-mode . elm-indent-mode)
  :custom
  (elm-format-on-save t)
  (elm-package-json "elm.json" "Support elm 0.19"))


(provide 'chn-elm)
