;;; chn-codestyle.el --- Altar of Perfectionism

;; Tabs, begone!
(setq-default indent-tabs-mode nil)

(straight-use-package 'prettier)

(use-package flycheck
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

(use-package ethan-wspace
  :config
  (global-ethan-wspace-mode 1)

  ;; Do not insert newline at EOF in snippets files, often we want this.
  (add-hook 'snippet-mode-hook
            '(lambda()
               (setq ethan-wspace-errors (remove 'no-nl-eof ethan-wspace-errors))))
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
(setq elm-indent-offset 4)

(provide 'chn-codestyle)
