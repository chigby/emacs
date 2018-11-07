;;; chn-eshell.el --- The universal command line

(global-set-key (kbd "C-z") 'eshell)

;; Complete as much as possible, then wait
(setq eshell-cmpl-cycle-completions nil)

(add-hook 'eshell-mode-hook
          (lambda ()
            (company-mode 0)))


(provide 'chn-eshell)
