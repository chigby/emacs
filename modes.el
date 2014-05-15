;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python

(add-hook 'python-mode-hook
          (lambda ()
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
            (local-set-key (kbd "\C-c<") 'unindent-region)
            ))

(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))

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

(add-hook 'php-mode-hook 'clean-php-mode)
(add-hook 'php-mode-hook
          (lambda()
          (local-set-key (kbd "C-M-j") 'backward-word)
          (local-set-key (kbd "M-j") 'backward-char)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ruby

;; Turn on ruby mode for vagrantfiles.
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-a") 'back-to-indentation)
            (local-set-key (kbd "M-m") 'move-beginning-of-line)))

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
  (add-to-list 'auto-mode-alist '("/mutt\\|itsalltext.*mail\\.google" . mail-mode))
  (add-hook 'mail-mode-hook 'turn-on-auto-fill)
  (add-hook
   'mail-mode-hook
   (lambda ()
     (define-key mail-mode-map [(control c) (control c)]
       (lambda ()
         (interactive)
         (save-buffer)
         (server-edit)))
     (define-key mail-mode-map (kbd "C-c C-d") 'kill-old-message)))

(add-hook 'mail-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-d") 'kill-old-message)))

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook
          (lambda ()
            (local-set-key (kbd "M-c") 'capitalize-word)))

(setq linum-mode-inhibit-modes-list
      '(term-mode eshell-mode comint-mode w3m-mode shell-mode))

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
(setq yas-snippet-dirs '("~/.emacs.d/snippets" "~/.emacs.d/elpa/yasnippet-0.8.0/snippets"))
(yas-global-mode 1)

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
(add-hook 'w3m-mode-hook (lambda()
        (yas-minor-mode -1)))

(setq scss-compile-at-save nil)
