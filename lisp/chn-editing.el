;;; chn-editing.el --- manipulation and correspondance

(use-package smartparens
  :diminish smartparens-mode
  :commands
  smartparens-mode
  smartparens-strict-mode
  sp-local-pair
  :bind (("<C-right>" . sp-forward-slurp-sexp)
         ("<C-left>" . sp-forward-barf-sexp)
         ("C-," . sp-rewrap-sexp))
  :hook  (((lisp-data-mode emacs-lisp-mode) . smartparens-strict-mode)
          ((elm-mode haskell-mode js-base-mode python-base-mode rust-mode) . smartparens-mode))
  :config
  (require 'smartparens-config)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "[" "]" :wrap "C-c [")
  (sp-pair "{" "}" :wrap "C-c {"))

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package undo-tree
  :diminish undo-tree-mode
  :custom
  (undo-tree-history-directory-alist `(("." . ,(concat emacs-root "undo-tree"))))
  :hook (emacs-startup . global-undo-tree-mode))

 ;; Eliminate duplicates in the kill ring.
(setq kill-do-not-save-duplicates t)

;; Preserve text copied from the operating system before overwriting
;; it with something else from within emacs.
(setq save-interprogram-paste-before-kill t)

;; Duplicate start of line or region with C-<end>.
;; From http://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion
(defun duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-start-of-line)))
(defun duplicate-start-of-line ()
  (if (bolp)
      (progn
        (end-of-line)
        (duplicate-start-of-line)
        (beginning-of-line))
    (let ((at-eob (eobp))
          (text (buffer-substring (point)
                                  (beginning-of-thing 'line))))
      (when at-eob (end-of-line) (open-line 1))
      (forward-line)
      (push-mark)
      (insert text)
      (open-line 1))))
(defun duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning) end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))
(global-set-key (kbd "C-<end>") 'duplicate-start-of-line-or-region)

(defun open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun vi-open-line (&optional abovep)
  "Insert a newline below the current line and put point at beginning.
With a prefix argument, insert a newline above the current line."
  (interactive "P")
  (if abovep
      (open-line-above)
    (open-line-below)))

(global-set-key (kbd "C-o") 'open-line-above)
(global-set-key (kbd "C-j") 'open-line-below)

(defun chn/isearch-comment-result ()
  "During an incremental search, comment lines spanned from
search start to search end."
  (interactive)
  (isearch-exit)
  (activate-mark)
  (call-interactively 'comment-line))
(define-key isearch-mode-map (kbd "C-;") 'chn/isearch-comment-result)

(defun chn/occur-dwim ()
  "Call `occur' with a default argument of symbol under cursor."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))
(global-set-key (kbd "M-s p") 'chn/occur-dwim)

(defun chn/pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
Version 2016-04-04"
  (interactive)
  (set-mark-command t))

(global-set-key (kbd "<f2>") 'pop-global-mark)
(global-set-key (kbd "<f3>") 'chn/pop-local-mark-ring)

;; non-editing
(setq view-read-only t)

(provide 'chn-editing)
