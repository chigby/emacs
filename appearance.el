(setq inhibit-splash-screen t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(fringe-mode 'none)

(set-default-font "-apple-monaco-medium-r-normal--14-140-72-72-m-140-mac-roman")

;; The command below fixes problems with problems with whitespace-mode
;; in emacs22.
(if (= 22 emacs-major-version)
    (setq whitespace-display-mappings '((space-mark ?\  [?.]) (newline-mark ?\n [?$ ?\n]) (tab-mark ?\t [?\\ ?\t]))))

;; move cursor one line when going past end of page
;; from http://orestis.gr/blog/2008/02/28/emacs-goals/
(setq scroll-step 1)

(require 'color-theme)
(if (or (eq window-system 'ns) (string= (getenv "TERM") "xterm-256color"))
    (progn
      (require 'zenburn) (color-theme-zenburn)))

(setq linum-format
          (lambda (line)
            (propertize (format
                         (let ((w (length (number-to-string
                                           (count-lines (point-min) (point-max))))))
                           (concat "%" (number-to-string w) "d "))
                         line)
                        'face 'linum)))


(add-to-list 'default-frame-alist '(cursor-color . "black"))
(blink-cursor-mode t)

;; (custom-set-faces
;;  '(flymake-errline ((((class color)) (:background "zenburn-bg" :foreground "#ef8500"))))
;;  '(flymake-warnline ((((class color)) (:background "zenburn-bg" :foreground "#93e0e3"))))
;; )