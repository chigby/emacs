;; Rust

(package-require 'rust-mode)
(package-require 'cargo)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
