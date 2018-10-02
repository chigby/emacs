;;; chn-snippets.el --- Repetition is the enemy of progress

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :bind ("s-<tab>" . yas-expand)
  :config
  ;; prevent TAB conflicts with other things
  (define-key yas-minor-mode-map [tab] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (yas-reload-all))

(provide 'chn-snippets)
