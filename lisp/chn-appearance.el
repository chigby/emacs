;;; chn-appearance.el --- Silver, copper, gold.

(when window-system
  (setq frame-title-format '("" "%f - " invocation-name "@" system-name))
  (mouse-wheel-mode t)
  (blink-cursor-mode t)
  (setq blink-cursor-blinks 100))

(setq column-number-mode t)
(setq initial-scratch-message ";; Blessed art thou, who hath come to the One True Editor.

;; No one should ever work.
;; Work is the source of nearly all the misery in the world. Almost
;; any evil you’d care to name comes from working or from living in a
;; world designed for work. In order to stop suffering, we have to
;; stop working.

;; Curiosity, not ambition.

")

;; Themes
(defun chn/disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun chn/load-theme (theme)
  "Enhance `load-theme' by first disabling enabled themes."
  (chn/disable-all-themes)
  (load-theme theme))

(use-package solarized-theme)
(use-package base16-theme
  :defer t)
(use-package doom-themes
  :defer t)

(defun white-theme ()
  "A white-background for the brightest of days"
  (interactive)
  (chn/load-theme 'doom-one-light))

(defun light-theme ()
  "A low-contrast light theme to combat screen glare"
  (interactive)
  (chn/load-theme 'solarized-light))
;; try leuven-theme here??

(defun dark-theme ()
  "A dark theme to combat night-blindness"
  (interactive)
  (chn/load-theme 'base16-materia))

(defun cyber-theme ()
  "A festive, dark theme for revels and the cybernetic midnight horizon"
  (interactive)
  (chn/load-theme 'doom-laserwave))

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

(provide 'chn-appearance)
