;;; chn-rust.el --- the rust programming language

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook #'smartparens-mode))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  )

(provide 'chn-rust)
