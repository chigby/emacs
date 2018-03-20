;; js2-mode
(package-require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq-default js2-global-externs
              '("PIXI" "ROT" "_"))
