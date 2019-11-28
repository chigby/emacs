(require 'chn-lib)

(defun x-font-setup ()
  (set-fontset-font "fontset-default" 'symbol "Ubuntu Mono-12")
  (set-face-attribute 'default nil :font "Ubuntu Mono-12")
  (setq default-frame-alist '(
                              (font . "Ubuntu Mono-12")
                              ))

  (setq initial-frame-alist '(
                              (font . "Ubuntu Mono-12")
                              )))


(set-default-font "Hack 13")

(-when-let (nodenv-root (chn/exec-if-exec "nodenv" "root"))
  (let ((nodenv-shims (concat nodenv-root "/shims")))
    (add-to-list 'exec-path nodenv-shims)
    (setenv "PATH" (concat nodenv-shims ":" (getenv "PATH")))))
