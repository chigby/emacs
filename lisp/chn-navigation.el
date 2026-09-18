;;; chn-navigation.el --- correspondance, time, and movement

(use-package symbol-overlay :ensure t)

(global-set-key (kbd "M-m") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "S-<f8>") 'symbol-overlay-remove-all)


(use-package syntax-subword
  :ensure t
  :config
  (global-syntax-subword-mode)
  (setq syntax-subword-skip-spaces 'consistent))

(defkeys global-map
         "M-j" backward-sexp
         "M-l" forward-sexp
         ;; "M-i" backward-list
         ;; "M-k" forward-list
         "M-o" down-list
         "M-u" backward-up-list
         )

(provide 'chn-navigation)


;; Balanced Expressions
;;
;; Shortcut	Command Invoked	  Description
;; C-M-f	forward-sexp	  Forward over an sexps.
;; C-M-b	backward-sexp	  Backward over a sexps.
;; C-M-d	down-list	      Move down into a sexps.
;; C-M-u	backward-up-list  Move up out of a sexps.
;; C-M-n	forward-list	  Move forward to the sexps.
;; C-M-p	backward-list	  Move backward to the previous sexps.
;; C-M-k	kill-sexp	      Kill down into sexps.
