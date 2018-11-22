;;; chn-html.el --- Quanta of the web

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'chn/check-hugo)
  (add-hook 'web-mode-hook 'chn/check-django))

(defun chn/check-hugo ()
  (let ((project-root (vc-git-root buffer-file-name)))
    (when (and (file-directory-p (concat project-root "content"))
               (or (f-exists (concat project-root "config.json")) (f-exists? (concat project-root "config.toml"))))
      (web-mode-set-engine "go"))))

(defun chn/check-django ()
  (let ((project-root (vc-git-root buffer-file-name)))
    (when (f-exists? (concat project-root "manage.py"))
      (web-mode-set-engine "django"))))

(provide 'chn-html)
