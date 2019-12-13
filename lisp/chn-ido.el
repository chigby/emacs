;;; chn-ido.el --- Tireless minibuffer companion

(ido-mode t)
(setq ido-enable-flex-matching t ;; fuzzy matching
      ido-use-virtual-buffers t) ;; Find past buffers as well as existing ones

(provide 'chn-ido)
