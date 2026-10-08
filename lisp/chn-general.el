;;; chn-general.el --- Tools, fundaments, oddities various and sundry

(prefer-coding-system 'utf-8)
(transient-mark-mode t)

;; Keep up to 100 recent files in 'M-x b' history
(setq recentf-max-saved-items 100)

(use-package which-func
  :ensure nil
  :hook ((python-base-mode) . which-function-mode))

(provide 'chn-general)
