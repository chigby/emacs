;;; early-init.el --- Early Init File -*- lexical-binding: t -*-


;; In emacs 27 and greater, package initialization happens after
;; early-init, but before user-init.  Straight.el recommends disabling
;; this so that it can fully handle the initialization of packages.
(setq package-enable-at-startup nil)

;; Do not allow loading from the package cache -- see above.
(setq package-quickstart nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Turn off GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-dialog-box t)
(setq use-file-dialog nil)
(setq inhibit-startup-echo-area-message user-login-name) ; read the docstring

;;; early-init.el ends here
