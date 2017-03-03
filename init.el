;; init.el

;; The Xytaxehedron held to the stars,
;; The incantation uttered with eager tongues,
;; What long-shackled powers of the elder dark
;; have our conjurings loosed?

(setq emacs-root (expand-file-name "~/.emacs.d/"))

(add-to-list 'load-path (concat emacs-root "lisp"))
(add-to-list 'load-path (concat emacs-root "site-lisp"))

(setq custom-file (concat emacs-root "custom.el"))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defun package-require (package)
  (when (not (package-installed-p package))
    (package-install package)))

(load-library "chn-paths") ;; exec paths for python, macports; backup and autosave dirs
(load-library "chn-functions") ;; my own one-off functions
(load-library "chn-twine-mode") ;; my twine mode
(load-library "chn-modes") ;; mode-specific settings
(load-library "chn-keys") ;; my own keybindings
(load-library "chn-mail") ;; mu4e configuration
(load-library "chn-appearance") ;; fonts, colors, themes
(load-library "chn-ag") ;; the silver searcher settings
(load-library "chn-git") ;; git settings
(load-library "chn-haskell") ;; haskell settings
(load-library "chn-python") ;; python settings
(load-library "chn-tabs") ;; special settings for the "tab" key
(load-library "chn-editing") ;; text editing niceties
(load-library "chn-codestyle") ;; text editing niceties
(load-library "chn-misc") ;; hard-to-classify or not-yet-classified

(eshell) ;; begin with a shell

;; Load custom settings
(load custom-file 'noerror)
