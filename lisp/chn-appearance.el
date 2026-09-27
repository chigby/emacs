;;; chn-appearance.el --- Silver, copper, gold.

(defun chn/display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(provide 'chn-appearance)
