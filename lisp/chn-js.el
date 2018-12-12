;;; chn-js.el --- Molten subterranean flamespeak of geomancers

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :commands js2-mode
  :config
  (setq-default
   js2-global-externs
   '("PIXI" "ROT" "_" "module" "require" "__dirname" "process" "console" "$"))
  )

(provide 'chn-js)
