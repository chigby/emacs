;;; modes.el -- configuration for various and sundry modes

;; dhall
(use-package dhall-mode
  :config
  (setq dhall-format-at-save nil))


;;; Ruby

;; Turn on ruby mode for vagrantfiles.
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(defun chn-ruby-keys ()
  (local-set-key (kbd "C-c C-z") 'run-test-file))
(add-hook 'ruby-mode-hook 'chn-ruby-keys)


;;; Eshell

;; scroll to the bottom
(require 'eshell)
(setq eshell-scroll-to-bottom-on-output t)
(setq eshell-scroll-show-maximum-output t)
(setq eshell-cmpl-ignore-case t)
(add-to-list 'eshell-output-filter-functions 'eshell-postoutput-scroll-to-bottom)

(defadvice eshell-handle-ansi-color (around test activate)
   (ansi-color-apply-on-region (1- eshell-last-output-start)
                               (1- eshell-last-output-end)))

(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)


;;; Ansi-term

;; let the shell know we want utf-8 everywhere
(defadvice ansi-term (after advise-ansi-term-coding-system)
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
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

(defun chn-mail-mode-keys ()
  (define-key mail-mode-map [(control c) (control c)]
    (lambda ()
      (interactive)
      (save-buffer)
      (server-edit)))
  (define-key mail-mode-map (kbd "C-c C-d") 'kill-old-message)
  (local-set-key (kbd "C-c C-d") 'kill-old-message))

  (add-to-list 'auto-mode-alist '("/mutt\\|itsalltext.*mail\\.google" . mail-mode))
  (add-hook 'mail-mode-hook 'turn-on-auto-fill)
  (add-hook 'mail-mode-hook 'chn-mail-mode-keys)

(defun chn-text-mode-keys ()
  (local-set-key (kbd "M-c") 'capitalize-word))
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'chn-text-mode-keys)

(autoload 'awk-mode "cc-mode" nil t)

(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching
(setq ido-use-virtual-buffers t)  ;; Find past buffers as well as existing ones

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(setq-default ispell-program-name "aspell")

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(defun chn-yaml-keys ()
  (local-set-key (kbd "C-c C-z") 'run-test-file))
(add-hook 'yaml-mode-hook 'chn-yaml-keys)

(require 'twine-mode)
(add-to-list 'auto-mode-alist '("\\.tws$" . twine-mode))

(setq scss-compile-at-save nil)

(setq find-ls-option '("-print0 | xargs -0 ls -ldh" . "-ldh"))

; properly format ansi colors on shell-command
; see http://stackoverflow.com/questions/5819719/emacs-shell-command-output-not-showing-ansi-colors-but-the-code
(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))

(setq ag-reuse-buffers 't)

(setq calc-settings-file (concat emacs-root "lisp/calc-settings.el"))

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(add-to-list 'auto-mode-alist '("\\.sls\\'" . salt-mode))
