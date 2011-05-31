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
  (set-frame-height (selected-frame) 85)
  (set-frame-width (selected-frame) 180)
  (set-frame-position (selected-frame) 0 0)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-vertically)
  (windmove-right)
  (split-window-vertically))

(if (and (eq window-system 'ns) (> (x-display-pixel-height) 1400) )
    (resize-and-reposition-frame))

 ;;; This is for GNU Emacs 21
    (if (= 21 emacs-major-version)
        (load "term/xterm-256color"))

(load-library "functions")

(vendor 'full-ack 'ack 'ack-same 'ack-find-same-file 'ack-find-file 'ack-interactive)
(vendor 'puppet-mode)
(vendor 'apache-mode)
(vendor 'js2)
(vendor 'psvn)
(vendor 'whitespace)
(vendor 'php-mode)
(vendor 'color-theme)
(vendor 'cc-mode)
(vendor 'flymake)
(vendor 'cperl-mode)
(vendor 'yasnippet)
(vendor 'pymacs)
(vendor 'autopair)
(vendor 'zenburn)

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
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))


(setq confirm-kill-emacs #'yes-or-no-p)

; No audible bell.
(setq ring-bell-function (lambda () (message "*beep*")))

(setenv "PYTHONPATH" "/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/python_daemon-1.4.6-py2.6.egg:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/lockfile-0.8-py2.6.egg:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/BeautifulSoup-3.1.0.1-py2.6.egg:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/httplib2-0.6.0-py2.6.egg:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/ropemacs-0.6-py2.6.egg:/Users/cameronh/hg/mtg:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/src/python-magic:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python26.zip:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/plat-darwin:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/plat-mac:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/plat-mac/lib-scriptpackages:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/lib-tk:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/lib-old:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/lib-dynload:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/setuptools-0.6c11-py2.6.egg-info")

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

