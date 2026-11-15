;;; chn-rust.el --- the rust programming language  -*- lexical-binding: t; -*-

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package cargo
  :after rust
  :hook (rust-mode . cargo-minor-mode)
  )

(provide 'chn-rust)
