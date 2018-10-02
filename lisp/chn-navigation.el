;;; chn-navigation.el --- correspondance, time, and movement

(use-package syntax-subword
  :config
  (global-syntax-subword-mode)
  (setq syntax-subword-skip-spaces 'consistent))

(provide 'chn-navigation)
