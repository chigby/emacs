(package-require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(package-require 'syntax-subword)
(global-syntax-subword-mode)
(setq syntax-subword-skip-spaces 'consistent)
(setq save-interprogram-paste-before-kill t)
(package-require 'undo-tree)
(global-undo-tree-mode)
