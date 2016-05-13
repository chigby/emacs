(package-require 'git-commit)

(require 'git-commit)

(defun turn-off-ethan-wspace ()
  (ethan-wspace-mode -1))
(add-hook 'git-commit-mode-hook 'turn-off-ethan-wspace)
