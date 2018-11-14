;;; chn-complete.el --- What was sundered and undone / shall be whole

(use-package company
  :demand t
  :commands company-mode
  :config
  (global-company-mode)
  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-require-match nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  )

(provide 'chn-complete)
