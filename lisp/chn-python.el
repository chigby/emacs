;;; chn-python.el --- The programmable pseudonatural

(use-package python
  :commands python-mode
  :config
  (use-package virtualenvwrapper
    :commands venv-workon
    :init
    (setq venv-location "~/.virtualenvs"))
  (bind-key "C-c C-z" nil python-mode-map) ;; used for running tests
  (bind-key "C-j" 'end-of-line-indent python-mode-map)
  (bind-key "C-c >" 'indent-region python-mode-map)
  (bind-key "C-c <" 'unindent-region python-mode-map))

(use-package docker-tramp)

(defun module-spec-from-filename (filename)
  (message filename)
  (let* ((root-dir (expand-file-name (vc-git-root filename))))
    (s-replace-all
     `(("/" . ".") (,root-dir . "") (".py" . ""))
     filename)))

(provide 'chn-python)
