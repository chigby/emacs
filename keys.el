;; Command is meta in OS X.
(setq ns-command-modifier (quote meta))

;; taken from http://xahlee.org/emacs/effective_emacs.html -- try this one out??
(global-set-key (kbd "M-j") 'backward-char) ; was indent-new-comment-line
(global-set-key (kbd "M-l") 'forward-char)  ; was downcase-word
(global-set-key (kbd "M-i") 'previous-line) ; was tab-to-tab-stop
(global-set-key (kbd "M-k") 'next-line) ; was kill-sentence
(global-set-key (kbd "C-M-j") 'backward-word) ; was comment-indent-new-line
(global-set-key (kbd "C-M-l") 'forward-word)  ; was reposition-window

(global-set-key (kbd "C-M-k") 'delete-enclosed-text) ; was kill-sexp

(global-set-key (kbd "C-z") 'visit-ansi-term)
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "\C-c>") 'indent-region)
(global-set-key (kbd "\C-c<") 'unindent-region)

(global-set-key (kbd "M-s") 'isearch-forward-regexp)  ; was unbound
(global-set-key (kbd "M-r") 'isearch-backward-regexp) ; was move-to-window-line

(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key [f7] 'revert-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)

;; quicker window switching:
;; http://www.emacsblog.org/2008/05/01/quick-tip-easier-window-switching-in-emacs/
(windmove-default-keybindings)