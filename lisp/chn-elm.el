;;; chn-elm.el --- Elm Language Support  -*- lexical-binding: t; -*-

(use-package elm-mode
  :ensure t
  :bind
  (:map elm-mode-map
        ("C-c C-c" . elm-compile-main))
  :hook
  (elm-mode . elm-indent-mode)
  :custom
  (elm-format-on-save t)
  (elm-package-json "elm.json" "Support elm 0.19"))


(provide 'chn-elm)
