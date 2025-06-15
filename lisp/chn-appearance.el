;;; chn-appearance.el --- Silver, copper, gold.

(when window-system
  (setq frame-title-format '("" "%f - " invocation-name "@" system-name))
  (mouse-wheel-mode t)
  (blink-cursor-mode t)
  (setq blink-cursor-blinks 100))

(setq column-number-mode t)
(setq initial-scratch-message ";; Blessed art thou, who hath come to the One True Editor.

;; There is a vitality, a life force, a quickening that is translated
;; through you into action, and there is only one of you in all time,
;; this expression is unique, and if you block it, it will never exist
;; through any other medium; and be lost. The world will not have it. It
;; is not your business to determine how good it is, not how it compares
;; with other expression. It is your business to keep it yours clearly
;; and directly, to keep the channel open.

;; The mind of the moon is oblique & manifold, running at cross-currents
;; to the established vectors of the human psyche. Those among us who
;; choose trust the moon do so at their own peril — they are the bravest
;; among us, or the most foolish. perhaps, in a way, they are both.

;; Roll on, thou deep and dark blue Ocean, roll!
;; Ten thousand fleets sweep over thee in vain;
;; Man marks the earth with ruin; his control
;; Stops with the shore

")

;; Themes
(defun chn/disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun chn/load-theme (theme)
  "Enhance `load-theme' by first disabling enabled themes."
  (chn/disable-all-themes)
  (load-theme theme))

(elpaca solarized-theme)
(elpaca base16-theme)
(elpaca ef-themes (chn/load-theme 'ef-light))

(defun white-theme ()
  "A white-background for the brightest of days"
  (interactive)
  (chn/load-theme 'ef-light))

(defun light-theme ()
  "A low-contrast light theme to combat screen glare"
  (interactive)
  (chn/load-theme 'ef-melissa-light))

(defun dark-theme ()
  "A dark theme to combat night-blindness"
  (interactive)
  (chn/load-theme 'ef-dark))

(defun cyber-theme ()
  "A festive, dark theme for revels and the cybernetic midnight horizon"
  (interactive)
  (chn/load-theme 'ef-cherie))

(defun ns-font-setup ()
  (set-fontset-font "fontset-default" 'symbol "Menlo")
  (set-face-attribute 'default nil :foundry "apple" :family "Menlo" :height 140)
  (setq default-frame-alist '((cursor-type . box))))

;; move cursor one line when going past end of page
;; from http://orestis.gr/blog/2008/02/28/emacs-goals/
(setq scroll-step 1)

;; Needed for ansi-term mode.
(setq term-default-bg-color "#3f3f3f") ;; or use nil
(setq term-default-fg-color "#dcdccc")

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

(use-package minions :ensure t
  :hook (elpaca-after-init . minions-mode)
  :custom (minions-mode-line-lighter "--"))

(provide 'chn-appearance)
