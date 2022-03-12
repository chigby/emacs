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
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)
  ;;(add-hook 'haskell-mode-hook 'haskell-setup)
  )

(-when-let (ghcup-bin (file-if-exists "~/.ghcup/bin"))
  (let* ((expanded (expand-file-name ghcup-bin)))
    (add-to-list 'exec-path (expand-file-name ghcup-bin))
    (setenv "PATH" (format "%s:%s" (expand-file-name ghcup-bin) (getenv "PATH")))))

(provide 'chn-haskell)
