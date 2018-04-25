(package-require 'dash)
(package-require 'dash-functional)

(require 'dash)
(require 'dash-functional)

(defalias 'qrr 'query-replace-regexp)

(defun note ()
  "Jump to the Notesy folder inside Dropbox."
  (interactive)
  (dired "~/Dropbox/notesy"))

(defun prev-window ()
  (interactive)
  (other-window -1))

(defun end-of-line-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun delete-enclosed-text ()
  "Deletes text between a pair of delimiters"
  (interactive)
  (save-excursion
    (let (p1 p2)
      (skip-chars-backward "^(<['\"") (setq p1 (point)) (backward-char 1)
      (forward-sexp 1) (backward-char 1) (setq p2 (point))
      (delete-region p1 p2))))

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

(defun kill-old-message ()
  "Delete everything in the buffer except sig line."
  (interactive)
  (save-excursion
    (let (p1 p2)
      (setq p1 (point))
      (goto-char (point-max))
      (previous-line 2)
      (setq p2 (point))
      (delete-region p1 p2))))

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

(defun set-test-file (filename)
  "Save the name of a ruby test/spec file for later."
  (setq chn-test-file filename))

(defun run-test-file ()
  "If we are visiting a test file, run that. Otherwise, run the last one."
  (interactive)
  (let ((file-name (buffer-file-name (current-buffer))))
    (when (string-match "_test.rb$" file-name)
      (set-test-file file-name))
    (if (boundp 'chn-test-file)
        (shell-command (format "ruby -Itest %s" chn-test-file))
      (message "No test file defined. Try running one."))))

(autoload 'vc-git-root "vc-git")

(defun run-python-test-file ()
  "If we are visiting a test file, run that. Otherwise, run the last one."
  (interactive)
  (let* ((file-name (buffer-file-name (current-buffer)))
         (default-directory (vc-git-root (file-name-directory file-name))))
    (when (string-match "test_[a-z_]+.py$" file-name)
      (set-test-file file-name))
    (if (boundp 'chn-test-file)
        (shell-command (format "./runtest %s" chn-test-file))
      (message "No test file defined. Try running one."))))

;; display temporary/help messages in window "1" unless there is only
;; 1 window, then pop up another one using emacs default settings.
(defun right-edge (window) (nth 2 (window-edges window)))
(defun top-edge (window) (nth 1 (window-edges window)))

(defun chn-temp-window (buffer alist)
  (if (= (count-windows) 1)
      (display-buffer-pop-up-window buffer alist)
    (let ((desired-window (-max-by (-on '> 'right-edge) (--filter (= 0 (top-edge it)) (window-list)))))
      (set-window-buffer desired-window buffer)
      desired-window)))

(defun file-if-exists (path)
  (if (file-exists-p path) path nil))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun chn-dired-create-file (file)
  "Create a file called FILE.
If FILE already exists, signal an error."
  (interactive
   (list (read-file-name "Create file: " (dired-current-directory))))
  (let* ((expanded (expand-file-name file))
         (try expanded)
         (dir (directory-file-name (file-name-directory expanded)))
         new)
    (if (file-exists-p expanded)
        (error "Cannot create file %s: file exists" expanded))
    ;; Find the topmost nonexistent parent dir (variable `new')
    (while (and try (not (file-exists-p try)) (not (equal new try)))
      (setq new try
            try (directory-file-name (file-name-directory try))))
    (when (not (file-exists-p dir))
      (make-directory dir t))
    (write-region "" nil expanded t)
    (when new
      (dired-add-file new)
      (dired-move-to-filename))))
