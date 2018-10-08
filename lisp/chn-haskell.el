;;; chn-haskell.el --- "We must use this because Hask is not a category"

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(provide 'chn-haskell)
