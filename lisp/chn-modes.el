;;; modes.el -- configuration for various and sundry modes

;; dhall
(use-package dhall-mode
  :mode "\\.dhall\\'"
  :custom
  (dhall-format-at-save . nil))

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(use-package dockerfile-mode
  :mode (("Dockerfile" . dockerfile-mode)))


;;; Ansi-term

;; let the shell know we want utf-8 everywhere
(defadvice ansi-term (after advise-ansi-term-coding-system)
    (set-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

(setenv "LC_CTYPE" "en_US.UTF-8")

;; kill ended process buffers
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Make urls clickable in the terminal.
(defun my-term-hook ()
  (goto-address-mode))

(add-hook 'term-mode-hook 'my-term-hook)


;;; Misc.

(use-package prog-mode
  :ensure nil
  :custom
  (show-trailing-whitespace t)
  :bind
  (:map prog-mode-map
        ("C-c w" . delete-trailing-whitespace))
  :custom-face
  ;; (trailing-whitespace ((t (:foreground nil :background nil :underline (:style wave :color "#bf5f00")))))
  (trailing-whitespace ((t (:foreground nil :background "#fac200"))))
  )

(use-package text-mode
  :ensure nil
  :bind
  (:map text-mode-map
        ("M-c" . capitalize-word))
  :hook
  (text-mode . visual-line-mode))

(autoload 'awk-mode "cc-mode" nil t)

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "/")

  (uniquify-ignore-buffers-re "^\\*") ;; don't muck with special buffers
  )

(use-package ispell
  :ensure nil
  :custom
  (ispell-program-name "hunspell"))

(use-package sugarcube-mode
  :ensure nil ;; it's a local package, don't try to install it from a repo
  :mode "\\.twee\\'")

(use-package chn-octo-mode
  :ensure nil
  :mode "\\.octo\\'"
  )

(use-package find-dired
  :ensure nil
  :custom
  find-ls-option '("-print0 | xargs -0 ls -ldh" . "-ldh"))

; properly format ansi colors on shell-command
; see http://stackoverflow.com/questions/5819719/emacs-shell-command-output-not-showing-ansi-colors-but-the-code



;; (defadvice display-message-or-buffer (before ansi-color activate)
;;   "Process ANSI color codes in shell output."
;;   (let ((buf (ad-get-arg 0)))
;;     ))

(use-package calc
  :ensure nil
  :commands calc
  :custom
  (calc-settings-file (concat emacs-root "lisp/calc-settings.el")))