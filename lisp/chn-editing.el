;;; chn-editing.el --- manipulation and correspondance

(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil))

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))

(global-syntax-subword-mode)
(setq syntax-subword-skip-spaces 'consistent)
(setq save-interprogram-paste-before-kill t)
(global-undo-tree-mode)

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
    (let ((text (buffer-substring (point)
                                  (beginning-of-thing 'line))))
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

(provide 'chn-editing)
