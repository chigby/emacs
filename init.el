(setq emacs-root (expand-file-name "~/.emacs.d/"))

(add-to-list 'load-path (concat emacs-root "lisp"))

(load-library "paths")
(load-library "functions")
(load-library "packages")

(load-library "modes")
(load-library "keys")
(load-library "appearance")
(load-library "tabs")

(load-library "misc")

(eshell)
