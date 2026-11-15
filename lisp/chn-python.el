;;; chn-python.el --- For the pseudonatural integration of parts  -*- lexical-binding: t; -*-

(require 'which-func)
(require 'transient)

;; (defvar chn-active-test-spec nil
;;   "The path to the most recently run python test file")

(defvar-local chn-test-project-root nil)

(defun chn-test-project-root ()
  (or
   chn-test-project-root
   (let ((root (locate-dominating-file default-directory ".git")))
     (unless root
       (error "Couldn't locate project root folder.  Make sure the current file is inside a project"))
     (setq chn-test-project-root (expand-file-name root))))
  )

(defun chn-test-filename ()
  (file-relative-name (buffer-file-name) (chn-test-project-root)))

(defun chn-test-module ()
  (s-replace "/" "." (file-name-sans-extension (chn-test-filename))))

(defun chn-test-module-single-function ()
  (concat (chn-test-module) "." (get-function-spec-under-point)))

(defun get-function-spec-under-point ()
  (interactive)
  (save-excursion
    (treesit-beginning-of-defun)
    (end-of-line)
    (which-function)))

(transient-define-prefix chn-run-python-tests ()
  "Run tests for python projects."
  :value '("--verbosity=1" "--keepdb")
  ["Django test options"
   ("-k" "keepdb" "--keepdb")
   ("-f" "failfast" "--failfast")
   ("-v" "verbosity" "--verbosity=" :choices ("0" "1" "2" "3") :always-read t)
   ]

  ["Actions"
   ("a" "all tests in file" chn-test-all)
   ("s" "single test under point" chn-test-single)
   ])

(transient-define-suffix chn-test-all (&optional args)
  "Run all tests "
  (interactive (list (transient-args transient-current-command)))
  (let ((test-target (chn-test-module)))
    (chn-test-do-compilation (append args (list test-target)))
  ))

(transient-define-suffix chn-test-single (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (let ((test-target (chn-test-module-single-function)))
    (chn-test-do-compilation (append args (list test-target)))
    ))

(defun chn-django-manage-cmd (subcmd args)
  (let ((python-args "-W default::DeprecationWarning"))
    (format "python %s manage.py %s %s" python-args subcmd args)
    ))

(defun chn-docker-compose-exec (container command)
  (format "docker compose exec -T %s /bin/bash -c \"%s\"" container command))

(defun chn-test-compilation-buffer-name (compilation-mode)
  "Include the project name in the buffer name, if one exists."
  (if (project-current)
      (concat "*" (downcase compilation-mode) "-" (project-name (project-current)) "*")
    (compilation--default-buffer-name compilation-mode))
  )

(defun chn-test-do-compilation (&optional args)
  "Execute django tests with the given arguments."
  (let ((test-args (string-join args " "))
        (compilation-buffer-name-function #'chn-test-compilation-buffer-name))
    (compile
     (chn-docker-compose-exec "django" (chn-django-manage-cmd "test" test-args)))
    )
  )

(defun chn-run-tests ()
  "Execute all or part of a test suite, or re-run one already executed."
  (interactive)
  (if (s-starts-with? "test_" (file-relative-name buffer-file-name))
      (chn-run-python-tests)
    (if-let* ((buf (get-buffer (chn-test-compilation-buffer-name "compilation"))))
        (with-current-buffer buf (recompile))
      (user-error "Test specification for this project not found. Try running a test."))))

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :custom
  (compilation-scroll-output 'first-error)
  :bind
  (:map python-base-mode-map
        ("C-c C-z" . chn-run-tests)
        ("M-k" . python-nav-forward-statement)
        ("C-c >" . indent-region)))

(use-package docker-tramp
  :disabled)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-base-mode . ("uvx" "ruff" "server"))
               ;; <https://github.com/joaotavora/rassumfrassum#bundled-presets>
               ;;'(python-base-mode . ("uvx" "--from" "rassumfrassum" "rass" "basedruff"))
               ))
(add-hook 'python-base-mode-hook
          (lambda ()
            (eglot-ensure)
            (add-hook 'after-save-hook 'eglot-format nil t)
            (add-to-list 'eglot-stay-out-of 'xref)))

(defun module-spec-from-filename (filename)
  (let* ((root-dir (expand-file-name (vc-git-root filename))))
    (s-replace-all
     `(("/" . ".") (,root-dir . "") (".py" . ""))
     filename)))

(use-feature which-func
  :hook ((python-base-mode) . which-function-mode))

(provide 'chn-python)
