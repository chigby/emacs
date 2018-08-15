;;; Python

(require 'virtualenvwrapper)
(setq venv-location "~/.virtualenvs")

(defun python-format ()
  (interactive)
  (when (eq major-mode 'python-mode)
    (shell-command
     (format "yapf --in-place %s" (shell-quote-argument  (buffer-file-name))))
    (revert-buffer t t t)))

(defun chn-python-hook ()
  (local-set-key (kbd "C-c C-z") 'run-test-file)
  (local-set-key [f6] 'flymake-mode)
  (local-unset-key (kbd "C-j"))
  (local-set-key (kbd "C-j") 'end-of-line-indent)
  (local-set-key (kbd "\C-c>") 'indent-region)
  (local-set-key (kbd "\C-c<") 'unindent-region)
  (local-set-key (kbd "C-c C-a") 'python-format))

(add-hook 'python-mode-hook 'chn-python-hook)
