(let ((paths (mapcar 'expand-file-name '("/opt/local/Library/Frameworks/Python.framework/Versions/Current/bin"
            "/opt/local/bin"
            "/opt/local/sbin"
            "/opt/local/share/emacs/site-lisp"
            "/usr/local/bin"
            "/usr/local/texlive/2013/bin/x86_64-darwin/"
            "~/bin"))))
  (setenv "PATH" (apply 'concat
                        (append (mapcar (lambda (i) (concat i ":")) paths)
                                (list (getenv "PATH")))))
  (dolist (path paths) (when (file-directory-p path)
                         (add-to-list 'exec-path path))))

(setenv "MANPATH" (shell-command-to-string "manpath"))

;; Autosave and Backup
(setq autosave-dir (expand-file-name (concat emacs-root "autosave/")))
(setq backup-dir (expand-file-name (concat emacs-root "backup")))
(if (not (file-directory-p backup-dir))
    (make-directory backup-dir t))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq tramp-backup-directory-alist backup-directory-alist)

(if (not (file-directory-p autosave-dir))
    (make-directory autosave-dir t))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
