;;; chn-emacs.el --- Begin the normalization procedure


(use-package emacs
  :ensure nil
  :custom
  (confirm-kill-emacs #'yes-or-no-p)
  (ring-bell-function (lambda () (message "*beep*")))

  ;; Completion configuration (via Corfu)
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Disable Ispell completion function.
  (text-mode-ispell-word-completion nil)
  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

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