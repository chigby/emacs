;; Command is meta in OS X.
(setq ns-command-modifier (quote meta))

;; taken from http://xahlee.org/emacs/effective_emacs.html -- try this one out??
(global-set-key (kbd "M-j") 'backward-char) ; was indent-new-comment-line
(global-set-key (kbd "M-l") 'forward-char)  ; was downcase-word
(global-set-key (kbd "M-i") 'previous-line) ; was tab-to-tab-stop
(global-set-key (kbd "M-k") 'next-line) ; was kill-sentence

;; the below have also been forward-word, forward-same-syntax and
;; forward-to-word (backward: mutatis mutandis).  Not sure which is
;; best.

;; C-M-j was comment-indent-new-line.  Also: should I be stealing from
;; evil-mode instead of viper mode?
(global-set-key (kbd "C-M-j") 'evil-backward-word)
(global-set-key (kbd "C-M-l") 'evil-forward-word)  ; was reposition-window

(global-set-key (kbd "C-M-k") 'delete-enclosed-text) ; was kill-sexp
(global-set-key (kbd "M-8") 'goto-match-paren)

(global-set-key (kbd "C-x C-z") 'shell)
(global-set-key (kbd "C-z") 'eshell)
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "M-]") 'other-window)
(global-set-key (kbd "M-[") 'prev-window)

(global-set-key (kbd "\C-c>") 'indent-region)
(global-set-key (kbd "\C-c<") 'unindent-region)

(global-set-key (kbd "M-s") 'isearch-forward-regexp)  ; was unbound
(global-set-key (kbd "M-r") 'isearch-backward-regexp) ; was move-to-window-line

(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key [f7] 'revert-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "M-c") 'comment-or-uncomment-region) ; was capitalize-word

(global-set-key (kbd "M-=") 'count-words) ; was count-words-region

(global-set-key (kbd "C-x C-j") 'dired-jump) ; was count-words-region

(program-shortcut "mutt"  (kbd "C-c m") t)  ; mail client

(global-set-key "\C-x~" 'set-80-columns)
;(global-set-key "\C-x M-q" 'unfill-paragraph)
