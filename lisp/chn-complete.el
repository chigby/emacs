;;; chn-complete.el --- What was sundered and undone / shall be whole

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

(use-package dabbrev
  :ensure nil
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  (dabbrev-case-fold-search nil)
  )

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

(defun chn-orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism dispatcher with comma suffix."
  (when (string-suffix-p "," pattern)
    `(orderless-initialism . ,(substring pattern 0 -1))))

(defun chn-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher with equals sign suffix."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(use-package marginalia
  :ensure t
  :hook emacs-startup)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (orderless-style-dispatchers '(chn-orderless-initialism-dispatcher chn-orderless-literal-dispatcher)))

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

(use-package embark
  :ensure t

  :bind
  (("M-." . embark-act)         ;; pick some comfortable binding
   ("C-." . embark-dwim)        ;; good alternative: M-.
   ;; ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'

   :map minibuffer-local-map
   ("C-c C-c" . embark-collect)
   ("C-c C-e" . embark-export))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  :ensure t
  :demand t
  :bind (
         ([remap goto-line] . consult-goto-line)
         ([remap yank-pop] . consult-yank-pop)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ("C-c m" . consult-man)
         (:map goto-map
               ("e" . consult-compile-error)
               ("f" . consult-flymake)
               ("o" . consult-outline)
               ("m" . consult-mark)
               ("k" . consult-global-mark)
               ("i" . consult-imenu)
               ("I" . consult-imenu-multi))
         (:map search-map
               ("d" . consult-find)
               ("D" . consult-locate)
               ("f" . consult-fd)
               ("g" . consult-grep)
               ("G" . consult-git-grep)
               ("r" . consult-ripgrep)
               ("l" . consult-line)
               ("L" . consult-line-multi)
               ("k" . consult-keep-lines)
               ("u" . consult-focus-lines)
               ("e" . consult-isearch-history))

         (:map isearch-mode-map
               ("M-s l" . consult-line)              ;; needed by consult-line to detect isearch
               ("M-s L" . consult-line-multi))       ;; needed by consult-line to detect isearch

         ;; :map shell-mode-map
         ;; ("M-r" . consult-history)
         )
  :config
  (setq consult-narrow-key "<") ;; another idea: "C-+"
  )

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'chn-complete)
