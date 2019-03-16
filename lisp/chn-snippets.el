;;; chn-snippets.el --- Repetition is the enemy of progress

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :init
  (add-hook 'js-mode-hook #'yas-minor-mode)
  :bind ("C-<tab>" . yas-expand)
  :config
  ;; prevent TAB conflicts with other things
  (define-key yas-minor-mode-map [tab] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (yas-reload-all))

(provide 'chn-snippets)
