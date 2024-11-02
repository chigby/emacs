;;; chn-nim.el --- the nim programming language

(use-package nim-mode
  :defer t)

(when-let (nimble-bin (file-if-exists "~/.nimble/bin"))
  (let* ((expanded (expand-file-name nimble-bin)))
    (add-to-list 'exec-path (expand-file-name nimble-bin))
    (setenv "PATH" (format "%s:%s" (expand-file-name nimble-bin) (getenv "PATH")))))

(provide 'chn-nim)
