(let ((paths (mapcar 'expand-file-name '("/opt/local/Library/Frameworks/Python.framework/Versions/Current/bin"
            "/opt/local/bin"
            "/opt/local/sbin"
            "/opt/local/share/emacs/site-lisp"
            "/usr/local/bin"
            "~/.nimble/bin"
            "C:/Program Files/Git/bin" ;; needed for magit
            "/usr/local/texlive/2013/bin/x86_64-darwin/"
            "~/bin"))))
  (setenv "PATH" (apply 'concat
                        (append (mapcar (lambda (i) (concat i ":")) paths)
                                (list (getenv "PATH")))))
  (dolist (path paths) (when (file-directory-p path)
                         (add-to-list 'exec-path path))))

(setenv "MANPATH" (shell-command-to-string "manpath"))
