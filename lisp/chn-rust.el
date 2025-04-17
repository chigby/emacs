;;; chn-rust.el --- the rust programming language

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package cargo
  :after rust
  :hook (rust-mode . cargo-minor-mode)
  )

(provide 'chn-rust)
