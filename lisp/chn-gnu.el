(defun x-font-setup ()
  (set-fontset-font "fontset-default" 'symbol "Ubuntu Mono-12")
  (set-face-attribute 'default nil :font "Ubuntu Mono-12")
  (setq default-frame-alist '(
                              (font . "Ubuntu Mono-12")
                              ))

  (setq initial-frame-alist '(
                              (font . "Ubuntu Mono-12")
                              )))
