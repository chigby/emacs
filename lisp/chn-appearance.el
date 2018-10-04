(menu-bar-mode -1)

(when window-system
  (setq frame-title-format '("" "%f - " invocation-name "@" system-name))
  (-bar-mode -1)
  (scroll-bar-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode t)
  (setq blink-cursor-blinks 100))

(setq column-number-mode t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

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

(add-to-list 'display-buffer-alist '("\\*Help\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*Shell Command Output\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*Apropos\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*Ack-and-a-half\\*" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*ag search" chn-temp-window))
(add-to-list 'display-buffer-alist '("\\*ag dired\\*" chn-temp-window))

;; Sometimes emacs smart-splitting is a little too smart.
(setq split-height-threshold 90)
