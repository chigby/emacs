;;; chn-codestyle.el --- Altar of Perfectionism

;; Tabs, begone!
(setq-default indent-tabs-mode nil)

(elpaca prettier)

(use-package flycheck
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

(setq mode-require-final-newline nil)
(setq require-final-newline nil)

;; Indentation station
(setq-default tab-width 4)

(setq c-basic-offset 4)
(setq css-indent-offset 2)
(setq sh-basic-offset 2)
(setq js-indent-level 2)
(setq lua-indent-level 2)
(setq elm-indent-offset 4)

(use-package editorconfig
  :hook prog-mode
  :init
  (add-hook 'editorconfig-after-apply-functions (lambda (_) (setq web-mode-attr-indent-offset nil))))

(provide 'chn-codestyle)
