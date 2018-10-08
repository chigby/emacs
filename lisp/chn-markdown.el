;;; chn-markdown.el --- Common tongue of HTML

(use-package markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode))
  :config
  ;; prevent TAB conflicts with other things
  (define-key markdown-mode-map (kbd "<tab>") nil)
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(provide 'chn-markdown)
