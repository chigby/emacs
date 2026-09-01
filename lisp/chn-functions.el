;; helpful: http://emacswiki.org/emacs/DynamicBindingVsLexicalBinding
;; modify C-c C-z to prompt for a command if none is defined?
(defun keybind-shell-command (command)
  (interactive "sCommand: ")
  (lexical-let ((cmd command))
  (define-key global-map (kbd "C-c C-z")
        (lambda ()
           (interactive)
           (shell-command cmd)))))

;; see: http://emacs-fu.blogspot.com/2008/12/running-console-programs-inside-emacs.html

(defun term-start-or-switch (prg &optional use-existing)
  "* run program PRG in a terminal buffer. If USE-EXISTING is non-nil "
  " and PRG is already running, switch to that buffer instead of starting"
  " a new instance."
  (interactive)
  (let ((bufname (concat "*" prg "*")))
    (when (not (and use-existing
                 (let ((buf (get-buffer bufname)))
                   (and buf (buffer-name (switch-to-buffer bufname))))))
      (ansi-term prg prg))))

(defmacro program-shortcut (name key &optional use-existing)
  "* macro to create a key binding KEY to start some terminal program PRG;
    if USE-EXISTING is true, try to switch to an existing buffer"
  `(global-set-key ,key
     '(lambda()
        (interactive)
        (term-start-or-switch ,name ,use-existing))))

(defun insert-date ()
  "Insert the current date."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%T%z")))

(defun set-80-columns ()
  "Set the selected window to 80 columns (ish)."
  (interactive)
  (set-frame-size (selected-frame) 84 53))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; (defun run-nearest-test ()
;;   (interactive)
;;   ;; currently, this only works for python tests (due to its
;;   ;; interaction with the test runner.. future ruby support for this
;;   ;; should use the line number of the individual test to be run).
;;   (let ((spec-class-function (which-function)))
;;     (run-test-file (concat "." spec-class-function))))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun titleize-region ($from $to)
  (interactive "r")
  (let ((output
         (s-titleized-words (buffer-substring-no-properties $from $to))))
  (save-excursion
    (delete-region $from $to)
    (goto-char $from)
    (insert output)
    )))

(defun remove-electric-indent-mode ()
  (electric-indent-local-mode -1))
