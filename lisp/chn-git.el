;;; chn-git.el --- Taming the quanta of versioning

(use-package magit
  :defer t
  :commands magit-status
  :config
  (setq magit-auto-revert-mode nil)
  (setq magit-diff-refine-hunk t)
  :bind
  ("C-x g" . magit-status)
  ("C-c g" . magit-file-dispatch))

(use-package git-link
  :commands git-link)

;; (defun turn-off-ethan-wspace ()
;;   (ethan-wspace-mode -1))
;; (add-hook 'git-commit-mode-hook 'turn-off-ethan-wspace)

;; We're not using vc for anything, and it's enabled by default, so
;; turn it off.
;; actually rg-el uses this.. so let's turn it back on? chn 2020-06-15
;; (setq vc-handled-backends nil)

(provide 'chn-git)
