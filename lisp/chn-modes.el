;;; modes.el -- configuration for various and sundry modes

;;; Misc.
(use-package sugarcube-mode
  :ensure nil ;; it's a local package, don't try to install it from a repo
  :mode "\\.twee\\'")

(use-package chn-octo-mode
  :ensure nil
  :mode "\\.octo\\'"
  )

(use-package find-dired
  :ensure nil
  :custom
  find-ls-option '("-print0 | xargs -0 ls -ldh" . "-ldh"))
