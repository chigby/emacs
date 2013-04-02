(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "/opt/local/bin")
(add-to-list 'exec-path "/usr/local/bin")

;; Autosave and Backup
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(if (file-directory-p backup-dir)
    (setq backup-directory-alist (list (cons ".*" backup-dir)))
  (message (concat "Directory does not exist: " backup-dir)))
(if (not (file-directory-p autosave-dir))
    (make-directory autosave-dir t))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

(defun resize-and-reposition-frame ()
  "Change the size of the Emacs frame and reposition it to the default location."
  (interactive)
  (set-frame-height (selected-frame) 85)
  (set-frame-width (selected-frame) 90)
  (set-frame-position (selected-frame) 0 0))

(defun resize-and-reposition-frame-small ()
  (interactive)
  (set-frame-height (selected-frame) 49)
  (set-frame-width (selected-frame) 80)
  (set-frame-position (selected-frame) 0 0 ))

(defun resize-and-reposition-frame-large ()
  (interactive)
  (set-frame-height (selected-frame) 95)
  (set-frame-width (selected-frame) 174)
  (set-frame-position (selected-frame) 0 0)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-vertically)
  (other-window 2)
  (split-window-vertically))

(if (and (eq window-system 'ns) (> (x-display-pixel-height) 1400) )
    (resize-and-reposition-frame))

 ;;; This is for GNU Emacs 21
    (if (= 21 emacs-major-version)
	(load "term/xterm-256color"))

(load-library "functions")

(require 'package)

(setq package-list '(ack-and-a-half auctex puppet-mode apache-mode dsvn whitespace php-mode markdown-mode python js2-mode yasnippet color-theme-solarized git-commit))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; install missing packages from package-list
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(load-library "modes")
(load-library "keys")
(load-library "appearance")
;;(load-library "tabs")

(setq-default indent-tabs-mode nil)

(setq column-number-mode t)
(setq ns-use-system-highlight-color nil)
(show-paren-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Trailing whitespace is unnecessary.
(add-hook 'before-save-hook (lambda () (whitespace-cleanup)))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(setq confirm-kill-emacs #'yes-or-no-p)

; No audible bell.
(setq ring-bell-function (lambda () (message "*beep*")))

(server-start)

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
 '(show-paren-mode t)
 '(tab-width 8)
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:background "zenburn-bg" :foreground "#ef8500"))))
 '(flymake-warnline ((((class color)) (:background "zenburn-bg" :foreground "#93e0e3")))))

(eshell)
