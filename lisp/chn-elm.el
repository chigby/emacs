;;; chn-elm.el --- Elm Language Support

(use-package elm-mode
  :defer t
  :config
  (setq elm-format-on-save t)
  (bind-key "C-c C-c" 'elm-compile-main elm-mode-map)
  :hook
  (elm-mode . elm-indent-mode)
  :custom
  (elm-package-json "elm.json" "Support elm 0.19"))


(provide 'chn-elm)
