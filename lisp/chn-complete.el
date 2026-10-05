;;; chn-complete.el --- What was sundered and undone / shall be whole

(use-package vertico
  :ensure t
  :hook (emacs-startup . vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("C-w" . vertico-directory-delete-word)
              ("M-DEL" . vertico-directory-delete-word)))

;; what is the equivalent of ido-use-virtual-buffers here, for buffer
;; completion?  Apparently it's to use consult?
;; https://www.reddit.com/r/emacs/comments/n646td/equivalent_of_idousevirtualbuffers_for_selectrum/
;; i.e. consult-buffer

(use-package marginalia
  :ensure t
  :hook emacs-startup)

;; (let ((map minibuffer-local-completion-map))
;;       (define-key map (kbd "SPC") nil)
;;       (define-key map (kbd "?") nil))

;; (setq completion-category-overrides
;;       ;; `partial-completion' is a killer app for files, because it
;;       ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
;;       '(
;;         (bookmark (styles . (basic)))
;;         ))

(provide 'chn-complete)
