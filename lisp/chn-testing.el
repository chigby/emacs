;;; chn-testing.el --- Proving ground of all reason

(require 'chn-lib)

(defun run-test-file (&optional command-suffix)
  "If we are visiting a test file, run that. Otherwise, run the last one."
  (interactive)
  (let ((file-name (buffer-file-name (current-buffer)))
        (suffix (if command-suffix command-suffix "")))
    (cond
     ((string-match "\(_spec.rb\\|_test.rb\)$" file-name)
      (set-test-file file-name suffix))
     ((string-match "\\(test_.*\.py\\|_test.py\\)$" file-name)
      (set-test-file (module-spec-from-filename file-name) suffix))
     )
    (if (boundp 'chn-test-file)
        (run-test-command chn-test-file)
      (message "No test file defined. Try running one."))))

;; let's make this (C-c t) or (C-c C-t)
(defun run-nearest-test ()
  (interactive)
  ;; currently, this only works for python tests (due to its
  ;; interaction with the test runner.. future ruby support for this
  ;; should use the line number of the individual test to be run).
  (let ((spec-class-function (which-function)))
    (run-test-file (concat "." spec-class-function))))

(defun set-test-file (filename command-suffix)
  "Save the name of a test file (or testable object) for later."
  (setq chn-test-file (concat filename command-suffix)))

(defun run-test-command (filename)
  (let* ((default-directory (vc-git-root buffer-file-name)))
    (cond
     ((file-readable-p "scripts/test.sh")
      (execute-bash-script (format "scripts/test.sh %s" filename)))
     ((file-readable-p "scripts/test")
      (shell-command (format "scripts/test %s" filename)))
     ((file-readable-p "bin/rails")
      (shell-command (format "bin/rails test %s" filename)))
     )))

(provide 'chn-testing)
