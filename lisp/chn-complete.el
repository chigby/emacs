;;; chn-complete.el --- What was sundered and undone / shall be whole

(use-package company
  :demand t
  :commands company-mode
  :config
  (global-company-mode)
  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-require-match nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  )

(use-package vertico
  :straight (vertico
             :files (:defaults "extensions/vertico-directory.el")
             :includes (vertico-directory))
  :init
  (vertico-mode)

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
  :straight nil
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

(defun chn-orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism dispatcher with comma suffix."
  (when (string-suffix-p "," pattern)
    `(orderless-initialism . ,(substring pattern 0 -1))))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (orderless-style-dispatchers '(chn-orderless-initialism-dispatcher)))

(setq completion-category-overrides
      '((file (styles . (basic partial-completion orderless)))
          (project-file (styles . (basic substring partial-completion orderless)))
          (imenu (styles . (basic substring orderless)))
          (kill-ring (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))))

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
