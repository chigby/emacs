;;; chn-emacs.el --- Begin the normalization procedure


(use-package emacs
  :ensure nil
  :custom
  (confirm-kill-emacs #'yes-or-no-p)
  (ring-bell-function (lambda () (message "*beep*")))
  :preface
  (defun chn/display-ansi-codes (buf &rest _)
      (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))
      )

  :init
  (advice-add #'display-message-or-buffer :before #'chn/display-ansi-codes)
  )

(provide 'chn-emacs)