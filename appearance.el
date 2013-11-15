(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(setq inhibit-splash-screen t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(defun ns-font-setup ()
  (set-fontset-font "fontset-default" 'symbol "Menlo")
  (set-face-attribute 'default nil :foundry "apple" :family "Menlo" :height 140))

(defun x-font-setup ()
  (set-fontset-font "fontset-default" 'symbol "Liberation Mono-9")
  (set-face-attribute 'default nil :font "Liberation Mono-9")
  (setq default-frame-alist '(
                              (font . "Liberation Mono-9")
                              ))

  (setq initial-frame-alist '(
                              (font . "Liberation Mono-9")
                              )))

(cond ((eq window-system 'ns) (ns-font-setup) (set-fringe-mode 0) (load-theme 'solarized-light t))
      ((eq window-system 'x) (x-font-setup) (menu-bar-mode -1))
      ((eq window-system nil) (menu-bar-mode -1)))

;; The command below fixes problems with problems with whitespace-mode
;; in emacs22.
(if (= 22 emacs-major-version)
    (setq whitespace-display-mappings '((space-mark ?\  [?.]) (newline-mark ?\n [?$ ?\n]) (tab-mark ?\t [?\\ ?\t]))))

;; move cursor one line when going past end of page
;; from http://orestis.gr/blog/2008/02/28/emacs-goals/
(setq scroll-step 1)

;; Needed for ansi-term mode.
(setq term-default-bg-color "#3f3f3f") ;;or use nil
(setq term-default-fg-color "#dcdccc")

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq linum-format
      (lambda (line)
        (propertize (format
                     (let ((w (length (number-to-string
                                       (count-lines (point-min) (point-max))))))
                       (concat "%" (number-to-string w) "d "))
                     line)
                    'face 'linum)))

(blink-cursor-mode t)
