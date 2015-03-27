;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python

(defun chn-python-hook ()
  (whitespace-mode)
  (smart-tab-mode 1)
  (local-set-key [f6] 'flymake-mode)
  (local-unset-key (kbd "C-j"))
  (local-set-key (kbd "C-j") 'end-of-line-indent)
  (local-unset-key (kbd "M-m"))
  (local-unset-key (kbd "C-a"))
  (local-unset-key (kbd "C-c C-z")) ;; was python-shell-switch-to-shell
  (local-set-key (kbd "C-a") 'back-to-indentation)
  (local-set-key (kbd "M-m") 'move-beginning-of-line)
  (local-set-key (kbd "\C-c>") 'indent-region)
  (local-set-key (kbd "\C-c<") 'unindent-region))

(add-hook 'python-mode-hook 'chn-python-hook)

(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure

;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)


;; javascript
(setq js-level-indent 2)
(setq js-indent-level 2)

;; coffeescript
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook 'coffee-custom)

;; lua
(setq lua-indent-level 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PHP

(require 'php-mode)
(defun clean-php-mode ()
  (setq c-basic-offset 4) ; 4 tabs indenting
  (setq indent-tabs-mode nil)
  (setq fill-column 78)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-close 'c-lineup-arglist-operators)
  (c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-math)) ; for DBTNG fields and values

(defun chn-python-keys ()
  (local-set-key (kbd "C-M-j") 'backward-word)
  (local-set-key (kbd "M-j") 'backward-char))

(add-hook 'php-mode-hook 'clean-php-mode)
(add-hook 'php-mode-hook 'chn-python-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web-Mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(defun chn-web-mode-keys ()
  (local-set-key (kbd "C-c C-z") 'run-test-file))
(add-hook 'web-mode-hook 'chn-web-mode-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ruby

;; Turn on ruby mode for vagrantfiles.
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(defun chn-ruby-keys ()
  (local-set-key (kbd "C-a") 'back-to-indentation)
  (local-set-key (kbd "C-c C-z") 'run-test-file)
  (local-set-key (kbd "M-m") 'move-beginning-of-line))
(add-hook 'ruby-mode-hook 'chn-ruby-keys)
; turn off overzealous indentation
(setq ruby-deep-indent-paren nil)
;(add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)

;(rvm-use-default)
;(setq rspec-use-rvm t)
;(setq rspec-use-rake-flag nil)
;(setq rspec-spec-command "rspec")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL

(add-to-list 'auto-mode-alist '("psql.edit" . sql-mode))
(setq sql-postgres-options (list "-p 6000"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Puppet

(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc.
;; Yes, you can do this same trick with the cool "It's All Text" firefox add-on :-)

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
                  ack-and-a-half-mode))

(defadvice linum-on (around linum-on-inhibit-for-modes)
  "Stop the load of linum-mode for some major modes."
    (unless (member major-mode linum-mode-inhibit-modes-list)
      ad-do-it))

(ad-activate 'linum-on)
(if (eq global-linum-mode nil) (global-linum-mode))

(autoload 'awk-mode "cc-mode" nil t)

(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching
(setq ido-use-virtual-buffers t)  ;; Find past buffers as well as existing ones

(require 'whitespace)  ;; display whitespace as characters
;; display only tails of lines longer than 80 columns, tabs and
;; trailing whitespaces
(setq whitespace-line-column 79
      whitespace-style '(tabs tab-mark indentation::space trailing lines-tail))

(require 'yasnippet)
(yas-global-mode 1)
(defun chn-term-mode-hook ()
  (setq yas-minor-mode -1))
(add-hook 'term-mode-hook 'chn-term-mode-hook)

(require 'uniquify)
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

(require 'markdown-mode)
(define-key markdown-mode-map (kbd "<tab>") nil)
(setq markdown-command
      (concat "python -c \"import sys, markdown2 as m;"
              "print m.markdown(sys.stdin.read()).strip()\""))

(require 'git-commit)
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
(add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0)))
(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
(add-hook 'git-commit-commit-hook
          (lambda () (server-edit)))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'w3m-load)
(defun chn-w3m-mode-hook ()
  (yas-minor-mode -1))
(add-hook 'w3m-mode-hook 'chn-w3m-mode-hook)

(setq scss-compile-at-save nil)

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ldh" . "-ldh"))

; properly format ansi colors on shell-command
; see http://stackoverflow.com/questions/5819719/emacs-shell-command-output-not-showing-ansi-colors-but-the-code
(require 'ansi-color)
(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))

(require 'evil)
(setq evil-default-cursor 'box)
