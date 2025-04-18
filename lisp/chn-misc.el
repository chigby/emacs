;; (require 'cl-lib)
;; (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;;   "Prevent annoying \"Active processes exist\" query when you quit Emacs."
;;   (cl-letf (((symbol-function #'process-list) (lambda ())))
;;     ad-do-it))

; Only start one server.
;; (require 'server)
;; (or (server-running-p)
;;     (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom stuff

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(rst-level-face-base-light 50)
 '(show-paren-mode t nil (paren)))
