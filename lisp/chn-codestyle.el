;;; chn-codestyle.el --- Altar of Perfectionism

;; Tabs, begone!
(setq-default indent-tabs-mode nil)

(use-package ethan-wspace
  :config
  (global-ethan-wspace-mode 1)
  :diminish ethan-wspace-mode)

(setq mode-require-final-newline nil)
(setq require-final-newline nil)

;; Indentation station
(setq-default tab-width 4)

(setq c-basic-offset 4)
(setq css-indent-offset 2)
(setq sh-basic-offset 2)
(setq js2-basic-offset 2)
(setq lua-indent-level 2)
(setq ruby-deep-indent-paren nil)
(setq elm-indent-offset 4)

(provide 'chn-codestyle)
