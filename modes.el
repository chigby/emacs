;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python

(require 'python-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (whitespace-mode)
            (local-set-key [f6] 'flymake-mode)
            (local-unset-key (kbd "C-j"))
            (local-set-key (kbd "C-j") 'end-of-line-indent)
            (local-unset-key (kbd "M-m"))
            (local-unset-key (kbd "C-a"))
            (local-set-key (kbd "C-a") 'back-to-indentation)
            (local-set-key (kbd "M-m") 'move-beginning-of-line)
            (local-set-key (kbd "\C-c>") 'indent-region)
            (local-set-key (kbd "\C-c<") 'unindent-region)
            ))

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/vendor/flymake-python/pyflymake.py" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))


;; Additional functionality that makes flymake error messages appear
;; in the minibuffer when point is on a line containing a flymake
;; error. This saves having to mouse over the error, which is a
;; keyboard user's annoyance

;;flymake-ler(file line type text &optional full-file)
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the
message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
          (let ((err (car (second elem))))
            (message "%s" (fly-pyflake-determine-message err)))))))

(defun fly-pyflake-determine-message (err)
  "pyflake is flakey if it has compile problems, this adjusts the
message to display, so there is one ;)"
  (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
        ((null (flymake-ler-file err))
         ;; normal message do your thing
         (flymake-ler-text err))
        (t ;; could not compile err
         (format "compile error, problem on line %s" (flymake-ler-line err)))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-mode (before post-command-stuff activate compile)
  "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer (rather than having to mouse over
it)"
  (set (make-local-variable 'post-command-hook)
       (cons 'show-fly-err-at-point post-command-hook)))

(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Perl

(add-hook 'cperl-mode-hook 'n-cperl-mode-hook t)
(defun n-cperl-mode-hook ()
;;   (setq cperl-indent-level 4)
;;   (setq cperl-continued-statement-offset 0)
;;   (setq cperl-extra-newline-before-brace t)
  (setq cperl-indent-level 4
        cperl-close-paren-offset -4
        cperl-continued-statement-offset 4
        cperl-indent-parens-as-block t
        cperl-tab-always-indent t
        cperl-highlight-variables-indiscriminately t)
  (set-face-background 'cperl-array-face zenburn-bg)
  (set-face-background 'cperl-hash-face zenburn-bg)
  (set-face-foreground 'cperl-hash-face zenburn-blue)
  (set-face-foreground 'cperl-array-face zenburn-green+4)
  )

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
;;; C mode

;; Make a non-standard key binding.  We can put this in
;; c-mode-base-map because c-mode-map, c++-mode-map, and so on,
;; inherit from it.
(defun my-c-initialization-hook ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

;; offset customizations not in my-c-style
;; This will take precedence over any setting of the syntactic symbol
;; made by a style.
(setq c-offsets-alist '((member-init-intro . ++)))

;; Create my personal style.
(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)
                                   (defun-open after)
                                   (inline-open after)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (inclass           . 4) ;; mine
                                   (access-label      . /) ;; mine
                                   (topmost-intro     . 0) ;; mine
;;                                 (comment-intro     . 0) ;; mine
                                   (knr-argdecl-intro . 0))) ;; used to be -
    (c-echo-syntactic-information-p . t))
  "My C Programming Style")
(c-add-style "PERSONAL" my-c-style)

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; set my personal style for the current buffer
  (c-set-style "PERSONAL")
  ;; other customizations
  (setq tab-width 8
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  ;; we like auto-newline, but not hungry-delete
  (c-toggle-auto-newline 1))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL

(add-to-list 'auto-mode-alist '("psql.edit" . sql-mode))
(setq sql-postgres-options (list "-p 6000"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell Scripts

;; Turn on shell-script-mode mode for aliases.
(add-to-list 'auto-mode-alist '(".aliases" . shell-script-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Puppet

;; (autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc.


(add-hook 'text-mod-hook
          (lambda ()
            (local-set-key (kbd "M-c") 'capitalize-word)))

(setq linum-mode-inhibit-modes-list '(term-mode eshell-mode))

(defadvice linum-on (around linum-on-inhibit-for-modes)
  "Stop the load of linum-mode for some major modes."
    (unless (member major-mode linum-mode-inhibit-modes-list)
      ad-do-it))

(ad-activate 'linum-on)
(if (eq global-linum-mode nil) (global-linum-mode))

(autoload 'longlines-mode
  "longlines.el"
  "Minor mode for automatically wrapping long lines." t)

(autoload 'awk-mode "cc-mode" nil t)

(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching

(require 'whitespace)  ;; display whitespace as characters
;; display only tails of lines longer than 80 columns, tabs and
;; trailing whitespaces
(setq whitespace-line-column 79
      whitespace-style '(tabs tab-mark indentation::space trailing lines-tail))

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/snippets")

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers



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
