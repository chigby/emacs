(package-require 'git-commit)
(require 'git-commit)

(package-require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(defun turn-off-ethan-wspace ()
  (ethan-wspace-mode -1))
(add-hook 'git-commit-mode-hook 'turn-off-ethan-wspace)
