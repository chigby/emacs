;;; chn-lib --- Component compounds of a wild law

(use-package dash)
(use-package dash-functional)
(use-package s)
(use-package f)

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(defun file-if-exists (path)
  (if (file-exists-p path) path nil))

(defun ubuntu-shell-command (command)
  "Execute a shell command in the Windows Subsystem for Linux environment."
  (shell-command (format "C:\\windows\\system32\\wsl.exe bash -ic \"%s\"" command)))

(defun execute-bash-script (filename)
  "Cross platform bash commands (requires ubuntu on windows)."
  (cond
   ((eq system-type 'gnu/linux)
    (shell-command filename))
   ((eq system-type 'windows-nt)
    (ubuntu-shell-command filename))
  ))

(provide 'chn-lib)
