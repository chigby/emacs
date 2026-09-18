(global-set-key (kbd "\C-c>") 'indent-region)
(global-set-key (kbd "\C-c<") 'unindent-region)

(global-set-key (kbd "M-r") 'isearch-backward-regexp) ; was move-to-window-line

(global-set-key [f5] 'kmacro-start-macro-or-insert-counter)  ;; use C-x e to end macro
(global-set-key [f7] 'revert-buffer)

(global-set-key (kbd "C-c C-z") 'run-test-file)
(global-set-key (kbd "C-c t") 'run-nearest-test)
