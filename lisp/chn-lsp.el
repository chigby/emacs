;;; chn-lsp.el --- Lexicon of ineffable secrets

(use-package lsp-mode
  :hook (haskell-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-haskell
  :defer t)

(provide 'chn-lsp)
