;; Command is meta in OS X.
(setq ns-command-modifier (quote meta))

(global-set-key (kbd "C-M-k") 'delete-enclosed-text) ; was kill-sexp
(global-set-key (kbd "M-8") 'goto-match-paren)

(global-set-key (kbd "C-x C-z") 'shell)
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "\C-c>") 'indent-region)
(global-set-key (kbd "\C-c<") 'unindent-region)

(global-set-key (kbd "M-r") 'isearch-backward-regexp) ; was move-to-window-line

(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key [f7] 'revert-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "M-c") 'comment-or-uncomment-region) ; was capitalize-word
(global-set-key (kbd "C-a") 'smart-beginning-of-line) ; was beginning-of-line

(global-set-key (kbd "M-=") 'count-words) ; was count-words-region

(global-set-key "\C-x~" 'set-80-columns)
;(global-set-key "\C-x M-q" 'unfill-paragraph)

(global-set-key (kbd "C-c C-z") 'run-test-file)
(global-set-key (kbd "C-c t") 'run-nearest-test)
