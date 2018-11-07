(defalias 'qrr 'query-replace-regexp)

(defun note ()
  "Jump to the Notesy folder inside Dropbox."
  (interactive)
  (dired "~/Dropbox/notesy"))

(defun dot-emacs ()
  "Return to the sanctum"
  (interactive)
  (dired emacs-root))
(global-set-key (kbd "C-<f6>") 'dot-emacs)

(defun end-of-line-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun delete-enclosed-text ()
  "Deletes text between a pair of delimiters"
  (interactive)
  (sp-beginning-of-sexp)
  (sp-kill-hybrid-sexp 1))

(defun goto-match-paren (arg)
     "Go to the matching parenthesis if on parenthesis. Else go to the
   opening parenthesis one level up."
     (interactive "p")
     (cond ((looking-at "\\s\(") (forward-list 1))
           (t
            (backward-char 1)
            (cond ((looking-at "\\s\)")
                   (forward-char 1) (backward-list 1))
                  (t
                   (while (not (looking-at "\\s("))
                     (backward-char 1)
                     (cond ((looking-at "\\s\)")
                            (message "->> )")
                            (forward-char 1)
                            (backward-list 1)
                            (backward-char 1)))
                     ))))))

(defun increment-number-at-point (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

 (global-set-key (kbd "C-c +") 'increment-number-at-point)

;; Macro to apply a fill to a specific line while leaving adjacent
;; lines untouched.
(fset 'inline-fill
   [?\C-a ?\C-e ?\C-o ?\C-a return ?\M-q backspace ?\M-\} ?\C-d])

(require 'term)
(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunct one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/zsh")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))

(defun changelog-entry ()
  (interactive)
  (goto-char (point-min))
  (search-forward ">")
  (kill-ring-save (point-min) (point))
  (goto-char (point-min))
  (open-line 1)
  (goto-char (point-min))
  (yank)
  (insert " ")
  (shell-command "gdate -R" 1)
  (goto-char (point-min))
  (search-forward ")")
  (backward-char 1)
  (increment-number-at-point)
  (search-forward "*")
  (kill-line)
)

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

(autoload 'vc-git-root "vc-git")

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
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
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
(fset 'npcloc
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 109 101 110 117 46 return 67108896 5 2 134217847 1 15 119 105 116 104 105 110 116 105 116 108 101 32 61 32 34 25 34 2 24 24 134217848 116 105 116 108 tab return 1 15 119 105 116 104 105 110 108 105 110 107 32 61 32 34 25 46 109 100 34] 0 "%d")) arg)))
