;;; chn-navigation.el --- correspondance, time, and movement

(use-package symbol-overlay)

(global-set-key (kbd "M-m") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "S-<f8>") 'symbol-overlay-remove-all)


(use-package syntax-subword
  :config
  (global-syntax-subword-mode)
  (setq syntax-subword-skip-spaces 'consistent))

;; taken from http://xahlee.org/emacs/effective_emacs.html -- try this one out??
(global-set-key (kbd "M-j") 'backward-char) ; was indent-new-comment-line
(global-set-key (kbd "M-l") 'forward-char)  ; was downcase-word
(global-set-key (kbd "M-i") 'previous-line) ; was tab-to-tab-stop
(global-set-key (kbd "M-k") 'next-line) ; was kill-sentence

;; the below have also been forward-word, forward-same-syntax and
;; forward-to-word (backward: mutatis mutandis).  Not sure which is
;; best.

(global-set-key (kbd "M-u") 'backward-word) ; was syntax-subword-upcase
(global-set-key (kbd "M-o") 'forward-word)  ; was.. something about faces?

(provide 'chn-navigation)
