;;; chn-snippets.el --- Repetition is the enemy of progress

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :hook ((js-base-mode python-base-mode ruby-mode) . yas-minor-mode)
  :bind (("C-<tab>" . yas-expand)
         :map yas-minor-mode-map
         ([tab] . nil)  ;; prevent TAB conflicts with other things
         )
  :config (yas-reload-all))

(provide 'chn-snippets)
