;; init.el

;; The Xytaxehedron held to the stars,
;; The incantation uttered with eager tongues,
;; What long-shackled powers of the elder dark
;; have our conjurings loosed?

(setq emacs-root (expand-file-name "~/.emacs.d/"))

(add-to-list 'load-path (concat emacs-root "lisp"))
(add-to-list 'load-path (concat emacs-root "site-lisp"))

(setq custom-file (concat emacs-root "custom.el"))

(load-library "paths") ;; exec paths for python, macports; backup and autosave dirs
(load-library "functions") ;; my own one-off functions
(load-library "packages") ;; install packages from melpa, etc.
(load-library "twine-mode") ;; my twine mode
(load-library "modes") ;; mode-specific settings
(load-library "keys") ;; my own keybindings
(load-library "mail") ;; mu4e configuration
(load-library "appearance") ;; fonts, colors, themes
(load-library "chn-ag") ;; the silver searcher settings
(load-library "tabs") ;; special settings for the "tab" key
(load-library "editing") ;; text editing niceties
(load-library "codestyle") ;; text editing niceties
(load-library "misc") ;; hard-to-classify or not-yet-classified

(eshell) ;; begin with a shell

;; Load custom settings
(load custom-file 'noerror)
