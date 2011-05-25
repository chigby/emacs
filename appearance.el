(setq inhibit-splash-screen t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (not (eq window-system 'ns)) (menu-bar-mode -1) (menu-bar-mode t))

(set-fringe-mode 0)

(set-face-font 'default "Menlo-14")

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

(if (or (eq window-system 'ns) (string= (getenv "TERM") "xterm-256color"))
    (color-theme-zenburn))

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

