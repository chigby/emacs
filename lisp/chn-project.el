;;; chn-project.el --- taskmasters, overseers, and orchestrators

(use-package ag
  :bind ("M-'" . ag-project))

(require 'bookmark)
(bookmark-maybe-load-default-file)
(global-set-key (kbd "C-<f6>") 'bookmark-bmenu-list)

;; refer to https://oremacs.com/2015/01/06/rushing-headlong/
(use-package headlong
  :bind ("M-;" . headlong-bookmark-jump))


(provide 'chn-project)
