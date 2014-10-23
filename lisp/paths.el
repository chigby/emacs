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
