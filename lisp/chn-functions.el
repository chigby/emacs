;; helpful: http://emacswiki.org/emacs/DynamicBindingVsLexicalBinding
;; modify C-c C-z to prompt for a command if none is defined?
(defun keybind-shell-command (command)
  (interactive "sCommand: ")
  (lexical-let ((cmd command))
  (define-key global-map (kbd "C-c C-z")
        (lambda ()
           (interactive)
           (shell-command cmd)))))

;; use-feature files
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

