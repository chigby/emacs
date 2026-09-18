;; helpful: http://emacswiki.org/emacs/DynamicBindingVsLexicalBinding
;; modify C-c C-z to prompt for a command if none is defined?
(defun keybind-shell-command (command)
  (interactive "sCommand: ")
  (lexical-let ((cmd command))
  (define-key global-map (kbd "C-c C-z")
        (lambda ()
           (interactive)
           (shell-command cmd)))))
