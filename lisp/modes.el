;;; modes.el -- configuration for various and sundry modes

;;; elm
(package-require 'elm-mode)

;;; Python

(defun chn-python-hook ()
  (smart-tab-mode 1)
  (local-set-key (kbd "C-c C-z") 'run-python-test-file)
  (local-set-key [f6] 'flymake-mode)
  (local-unset-key (kbd "C-j"))
  (local-set-key (kbd "C-j") 'end-of-line-indent)
  (local-set-key (kbd "\C-c>") 'indent-region)
  (local-set-key (kbd "\C-c<") 'unindent-region))

(add-hook 'python-mode-hook 'chn-python-hook)
(package-require 'virtualenvwrapper)
(require 'virtualenvwrapper)
(setq venv-location "~/.virtualenvs")


;; javascript
(setq js-level-indent 2)
(setq js-indent-level 2)

;; coffeescript
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook 'coffee-custom)

;; lua
(package-require 'lua-mode)
(setq lua-indent-level 2)



;;; Web-Mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.inc?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("ctemplate"    . "\\.inc\\'")
        ("django" . "/django.*\\.html\\'")
        ("django" . "/littleweaverweb.*\\.html\\'")
        ("django" . "/muckrack.*\\.html\\'")
        ("erb" . "/ns-www.*\\.html\\'"))
)

(defun chn-web-mode-keys ()
  (setq web-mode-markup-indent-offset 2)
  (local-set-key (kbd "C-c C-z") 'run-test-file))
(add-hook 'web-mode-hook 'chn-web-mode-keys)

(set-face-attribute 'web-mode-html-tag-face nil :inherit 'font-lock-function-name-face :foreground nil)
(set-face-attribute 'web-mode-html-tag-bracket-face nil :inherit 'default :foreground nil)
(set-face-attribute 'web-mode-html-attr-name-face nil :inherit 'font-lock-function-name-face :foreground nil)
(set-face-attribute 'web-mode-symbol-face nil :inherit 'font-lock-constant-face :foreground nil)


;;; Ruby

(package-require 'rvm)

;; Turn on ruby mode for vagrantfiles.
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(defun chn-ruby-keys ()
  (local-set-key (kbd "C-c C-z") 'run-test-file))
(add-hook 'ruby-mode-hook 'chn-ruby-keys)

;; turn off overzealous indentation
(setq ruby-deep-indent-paren nil)


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


;;; SQL

(add-to-list 'auto-mode-alist '("psql.edit" . sql-mode))
(setq sql-postgres-options (list "-p 6000"))


;;; Puppet

(package-require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))


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

(setq linum-mode-inhibit-modes-list
      '(term-mode eshell-mode comint-mode w3m-mode shell-mode eww-mode
                  ack-and-a-half-mode ag-mode mu4e-main-mode mu4e-headers-mode
                  mu4e-view-mode mu4e-compose-mode package-menu-mode
                  compilation-mode))

(defadvice linum-on (around linum-on-inhibit-for-modes)
  "Stop the load of linum-mode for some major modes."
    (unless (member major-mode linum-mode-inhibit-modes-list)
      ad-do-it))

(ad-activate 'linum-on)
(global-linum-mode t)

(autoload 'awk-mode "cc-mode" nil t)

(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching
(setq ido-use-virtual-buffers t)  ;; Find past buffers as well as existing ones

(package-require 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)
(defun chn-term-mode-hook ()
  (yas-minor-mode -1))
(defun chn-shell-mode-hook ()
  (yas-minor-mode -1))
(add-hook 'term-mode-hook 'chn-term-mode-hook)
(add-hook 'shell-mode-hook 'chn-shell-mode-hook)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(setq-default ispell-program-name "aspell")

;; (autoload 'markdown-mode "markdown-mode"
;;   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

(package-require 'markdown-mode)
(require 'markdown-mode)
(define-key markdown-mode-map (kbd "<tab>") nil)
(setq markdown-command
      (concat "python -c \"import sys, markdown2 as m;"
              "print m.markdown(sys.stdin.read()).strip()\""))

(package-require 'yaml-mode)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(defun chn-yaml-keys ()
  (local-set-key (kbd "C-c C-z") 'run-test-file))
(add-hook 'yaml-mode-hook 'chn-yaml-keys)

(require 'twine-mode)
(add-to-list 'auto-mode-alist '("\\.tws$" . twine-mode))

(require 'w3m-load)
(defun chn-w3m-mode-hook ()
  (yas-minor-mode -1))
(add-hook 'w3m-mode-hook 'chn-w3m-mode-hook)

(setq scss-compile-at-save nil)

(setq find-ls-option '("-print0 | xargs -0 ls -ldh" . "-ldh"))

(setq insert-directory-program (executable-find "gls"))

(setq dired-listing-switches "-alh")

; properly format ansi colors on shell-command
; see http://stackoverflow.com/questions/5819719/emacs-shell-command-output-not-showing-ansi-colors-but-the-code
(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))

(package-require 'evil)
(require 'evil)
(setq evil-default-cursor 'box)
(setq evil-default-state 'emacs)

(package-require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(setq ag-reuse-buffers 't)

(setq calc-settings-file (concat emacs-root "lisp/calc-settings.el"))

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
