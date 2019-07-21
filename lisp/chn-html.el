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

(use-package emmet-mode
  :commands emmet-mode
  :config
  ;; C-j is my open-line-below binding that I like too much to clobber
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  :hook (html-mode web-mode))

(defun in-project-root? (file)
  (f-exists? (concat (vc-git-root buffer-file-name) file)))

(defun chn/check-hugo ()
  (when (and (in-project-root? "content") (-any? 'in-project-root? '("config.json" "config.yaml" "config.toml")))
    (web-mode-set-engine "go")))

(defun chn/check-django ()
  (when (in-project-root? "manage.py") (web-mode-set-engine "django")))

(provide 'chn-html)
