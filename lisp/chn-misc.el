(setq column-number-mode t)
(setq ns-use-system-highlight-color nil)
(fset 'yes-or-no-p 'y-or-n-p)

(require 'cl-lib)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

(setq confirm-kill-emacs #'yes-or-no-p)

; No audible bell.
(setq ring-bell-function (lambda () (message "*beep*")))

; Only start one server.
(require 'server)
(or (server-running-p)
    (server-start))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom stuff

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(rst-level-face-base-light 50)
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t))
