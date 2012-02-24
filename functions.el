(defalias 'qrr 'query-replace-regexp)

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

(global-set-key (kbd "M-8") 'goto-match-paren)

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

;; For loading libraries from the vendor directory
;; Modified from defunkt's original version to support autoloading.
;; http://github.com/defunkt/emacs/blob/master/defunkt/defuns.el
(defun vendor (library &rest autoload-functions)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/vendor/" file))
         (suffix (concat normal ".el"))
         (personal (concat "~/.emacs.d/" file))
	 (found nil))
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (set 'found t))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (set 'found t))
     ((file-exists-p suffix)  (set 'found t)))
    (when found
      (if autoload-functions
          (dolist (autoload-function autoload-functions)
            (autoload autoload-function (symbol-name library) nil t))
        (require library)))
    (when (file-exists-p (concat personal ".el"))
      (load personal))))

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

;;; Final version: while
     (defun count-words-region (beginning end)
       "Print number of words in the region."
       (interactive "r")
       (message "Counting words in region ... ")

     ;;; 1. Set up appropriate conditions.
       (save-excursion
         (let ((count 0))
           (goto-char beginning)

     ;;; 2. Run the while loop.
           (while (and (< (point) end)
                       (re-search-forward "\\w+\\W*" end t))
             (setq count (1+ count)))

     ;;; 3. Send a message to the user.
           (cond ((zerop count)
                  (message
                   "The region does NOT have any words."))
                 ((= 1 count)
                  (message
                   "The region has 1 word."))
                 (t
                  (message
                   "The region has %d words." count))))))