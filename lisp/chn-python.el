;;; chn-python.el --- For the pseudonatural integration of parts

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :bind
  (:map python-base-mode-map
        ("<f6>" . compile)
        ("C-c t" . run-nearest-test)
        ("M-k" . python-nav-forward-statement)
        ("C-c >" . indent-region))
  :hook
  (python-base-mode . configure-test-compilation)
  :config
  (defun configure-test-compilation ()
    (let ((target-file (file-relative-name buffer-file-name))
          (default-directory (or (vc-root-dir)
                                 (locate-dominating-file "." ".git")
                                 default-directory)))
      (when (and
             (s-starts-with? "test_" target-file)
             (file-exists-p "docker-compose.yaml"))
        (setq-local compile-command
                    (concat "docker compose exec -T django /bin/bash -c \"python -W default::DeprecationWarning manage.py test --noinput --keepdb --verbosity 1 "
                            (if buffer-file-name
                                (shell-quote-argument
                                 (s-replace
                                  "/"
                                  "."
                                  (file-name-sans-extension (file-relative-name buffer-file-name))
                                  )))
                            "\" "
                            ))
        ))))

(use-package docker-tramp
  :disabled)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-base-mode . ("uvx" "ruff" "server"))))
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

(provide 'chn-python)
