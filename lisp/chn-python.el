;;; chn-python.el --- For the pseudonatural integration of parts

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :bind
  (:map python-base-mode-map
        ([remap python-shell-switch-to-shell] . run-test-file)
        ("C-j" . end-of-line-indent)
        ("C-c <" . unindent-region)
        ("C-c >" . indent-region)))

(use-package docker-tramp
  :disabled)

(defun module-spec-from-filename (filename)
  (let* ((root-dir (expand-file-name (vc-git-root filename))))
    (s-replace-all
     `(("/" . ".") (,root-dir . "") (".py" . ""))
     filename)))

(provide 'chn-python)
