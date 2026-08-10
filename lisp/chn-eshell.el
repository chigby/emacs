;;; chn-eshell.el --- The universal command line

(global-set-key (kbd "C-z") 'eshell)

;; Complete as much as possible, then wait
(setq eshell-cmpl-cycle-completions nil)

(add-hook 'eshell-mode-hook
          (lambda ()
            (company-mode 0)))

(defun chn/setup-shell ()
  (setq-local comint-prompt-read-only t)
  (setq-local comint-process-echoes t)
  (setq show-trailing-whitespace nil)

  ;; Remove the "kill running process?" query on exit.
  (let ((proc (get-buffer-process (current-buffer))))
   (when (processp proc)
     (set-process-query-on-exit-flag proc nil))))

(use-package coterm
  :ensure t
  :config (coterm-mode))

(use-package shell
  :ensure nil ;; built-in library
  :after (consult)
  :commands shell
  :bind (
         ("C-x C-z" . shell)
         :map shell-mode-map
         ([remap comint-history-isearch-backward-regexp] . consult-history)
         )
  :hook
  (shell-mode . chn/setup-shell))

(provide 'chn-eshell)
