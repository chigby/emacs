(package-require 'solarized-theme)

(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq frame-title-format '("" "%f - " invocation-name "@" system-name))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'light 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)))
(load-theme 'solarized-light t)

(defun ns-font-setup ()
  (set-fontset-font "fontset-default" 'symbol "Menlo")
  (set-face-attribute 'default nil :foundry "apple" :family "Menlo" :height 140)
  (setq default-frame-alist '((cursor-type . box))))

(defun x-font-setup ()
  (set-fontset-font "fontset-default" 'symbol "Ubuntu Mono-12")
  (set-face-attribute 'default nil :font "Ubuntu Mono-12")
  (setq default-frame-alist '(
                              (font . "Ubuntu Mono-12")
                              ))

  (setq initial-frame-alist '(
                              (font . "Ubuntu Mono-12")
                              )))

(cond ((eq window-system 'ns) (ns-font-setup) (set-fringe-mode 0))
      ((eq window-system 'x) (x-font-setup) (menu-bar-mode -1))
      ((null window-system) (menu-bar-mode -1)))

;; move cursor one line when going past end of page
;; from http://orestis.gr/blog/2008/02/28/emacs-goals/
(setq scroll-step 1)

;; Needed for ansi-term mode.
(setq term-default-bg-color "#3f3f3f") ;; or use nil
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

(set-face-attribute 'linum nil :weight 'normal)
(blink-cursor-mode t)
(setq blink-cursor-blinks 100)

(add-to-list 'display-buffer-alist '("\\*Help\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*Shell Command Output\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*Apropos\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*Ack-and-a-half\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*ag search" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*ag dired\\*" chn-temp-window))

;; Sometimes emacs smart-splitting is a little too smart.
(setq split-height-threshold 90)
