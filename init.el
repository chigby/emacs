(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(dolist (dir '(
               "/opt/local/Library/Frameworks/Python.framework/Versions/Current/bin"
               "/opt/local/bin"
               "/opt/local/share/emacs/site-lisp"
               "/usr/local/bin"
               "~/bin"))
  (if (file-directory-p dir) (add-to-list 'exec-path (expand-file-name dir))))

(setenv "PATH"
        (concat
         "/opt/local/Library/Frameworks/Python.framework/Versions/Current/bin" ":"
         "/opt/local/bin" ":"
         (expand-file-name "~/bin") ":"
         "/usr/local/bin" ":"
         "/usr/local/texlive/2013/bin/x86_64-darwin/" ":"
         (getenv "PATH")))

(setenv "MANPATH" (shell-command-to-string "manpath"))

;; Autosave and Backup
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(if (file-directory-p backup-dir)
    (progn
      (setq backup-directory-alist (list (cons ".*" backup-dir)))
      (setq tramp-backup-directory-alist backup-directory-alist))
  (message (concat "Directory does not exist: " backup-dir)))
(if (not (file-directory-p autosave-dir))
    (make-directory autosave-dir t))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(load-library "functions")

(require 'package)

(setq package-list '(ack-and-a-half auctex puppet-mode apache-mode dsvn whitespace php-mode markdown-mode js2-mode yasnippet git-commit yaml-mode rvm lua-mode))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; install missing packages from package-list
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(load-library "modes")
(load-library "keys")
(load-library "appearance")
(load-library "tabs")

(setq-default indent-tabs-mode nil)

(setq column-number-mode t)
(setq ns-use-system-highlight-color nil)
(fset 'yes-or-no-p 'y-or-n-p)

;; Trailing whitespace is unnecessary.
(add-hook 'before-save-hook (lambda () (if (not (eq 'markdown-mode major-mode)) (whitespace-cleanup))))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying 'Active processes exist' query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(setq confirm-kill-emacs #'yes-or-no-p)

; No audible bell.
(setq ring-bell-function (lambda () (message "*beep*")))

; Only start one server.
(require 'server)
(or (server-running-p)
    (server-start))

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

(eshell)
