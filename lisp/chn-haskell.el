;;; chn-haskell.el --- "We must use this because Hask is not a category"

(defun haskell-setup ()
  "Setup variables for editing Haskell files."
  (setq whitespace-line-column 70)
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list (number-sequence 2 80 2))
  (haskell-indentation-mode 0)
  (setq indent-line-function 'indent-relative))

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'haskell-setup))

(provide 'chn-haskell)
