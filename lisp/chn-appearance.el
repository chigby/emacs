;;; chn-appearance.el --- Silver, copper, gold.

;; move cursor one line when going past end of page
;; from http://orestis.gr/blog/2008/02/28/emacs-goals/
(setq scroll-step 1)

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Line numbers and other indicators
(require 'hl-line)
(require 'display-line-numbers)

(defun chn/numbers-toggle ()
  "Toggle line numbers."
  (interactive)
  (if (bound-and-true-p global-display-line-numbers-mode)
      (global-display-line-numbers-mode -1)
    (global-display-line-numbers-mode 1)))

(defun chn/hl-line-toggle ()
  "Toggle line highlighting."
  (interactive)
  (if (bound-and-true-p global-hl-line-mode) (global-hl-line-mode -1) (global-hl-line-mode 1)))

(defun chn/code-visibility ()
  "Enable or disable code visibility markers."
  (interactive)
  (chn/numbers-toggle)
  (chn/hl-line-toggle))

(let ((map global-map))
  (define-key map (kbd "C-c z") 'chn/code-visibility))

(defun chn/display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(provide 'chn-appearance)
