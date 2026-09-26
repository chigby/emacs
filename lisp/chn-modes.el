;;; modes.el -- configuration for various and sundry modes

;;; Misc.
(use-package sugarcube-mode
  :ensure nil ;; it's a local package, don't try to install it from a repo
  :mode "\\.twee\\'")

(use-package chn-octo-mode
  :ensure nil
  :mode "\\.octo\\'"
  )
