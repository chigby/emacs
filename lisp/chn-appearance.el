(menu-bar-mode -1)

(when window-system
  (setq frame-title-format '("" "%f - " invocation-name "@" system-name))
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode t)
  (setq blink-cursor-blinks 100))

(setq column-number-mode t)
(setq inhibit-splash-screen t)
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
(use-package base16-theme)

(defun light-theme ()
  "A low-contrast light theme to combat screen glare"
  (interactive)
  (chn/load-theme 'solarized-light))
;; try leuven-theme here??

(defun dark-theme ()
  "A dark theme to combat night-blindness"
  (interactive)
  (chn/load-theme 'base16-materia))

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

;; Line numbers in buffers
(global-linum-mode t)
(setq linum-format 'dynamic)

(setq linum-mode-inhibit-modes-list
      '(term-mode eshell-mode comint-mode w3m-mode shell-mode
                  ag-mode package-menu-mode
                  compilation-mode messages-mode magit-status-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-mode-inhibit-modes-list))
    (linum-mode 1)))

(set-face-attribute 'linum nil :weight 'normal)
