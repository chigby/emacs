(global-set-key (kbd "C-x g") 'magit-status)

(setq magit-auto-revert-mode nil)

(defun turn-off-ethan-wspace ()
  (ethan-wspace-mode -1))
(add-hook 'git-commit-mode-hook 'turn-off-ethan-wspace)

;; We're not using vc for anything, and it's enabled by default, so
;; turn it off.
(setq vc-handled-backends nil)
