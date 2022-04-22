;;; chn-rust.el --- the rust programming language

(use-package rust-mode
  :defer t
  :config
  (add-hook 'rust-mode-hook #'smartparens-mode))

(use-package cargo
  :after rust
  :hook (rust-mode . cargo-minor-mode)
  )

(provide 'chn-rust)
