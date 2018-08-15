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
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-refresh-contents)

;; Check for and install missing packages
(add-to-list 'package-selected-packages 'ag)
(add-to-list 'package-selected-packages 'ahk-mode)
(add-to-list 'package-selected-packages 'cargo)
(add-to-list 'package-selected-packages 'dash)
(add-to-list 'package-selected-packages 'dash-functional)
(add-to-list 'package-selected-packages 'dhall-mode)
(add-to-list 'package-selected-packages 'elixir-mode)
(add-to-list 'package-selected-packages 'elm-mode)
(add-to-list 'package-selected-packages 'ethan-wspace)
(add-to-list 'package-selected-packages 'expand-region)
(add-to-list 'package-selected-packages 'haskell-mode)
(add-to-list 'package-selected-packages 'js2-mode)
(add-to-list 'package-selected-packages 'lua-mode)
(add-to-list 'package-selected-packages 'magit)
(add-to-list 'package-selected-packages 'markdown-mode)
(add-to-list 'package-selected-packages 'powershell)
(add-to-list 'package-selected-packages 'puppet-mode)
(add-to-list 'package-selected-packages 'rust-mode)
(add-to-list 'package-selected-packages 'salt-mode)
(add-to-list 'package-selected-packages 'scala-mode)
(add-to-list 'package-selected-packages 'solarized-theme)
(add-to-list 'package-selected-packages 'solarized-theme)
(add-to-list 'package-selected-packages 'syntax-subword)
(add-to-list 'package-selected-packages 'undo-tree)
(add-to-list 'package-selected-packages 'virtualenvwrapper)
(add-to-list 'package-selected-packages 'web-mode)
(add-to-list 'package-selected-packages 'yaml-mode)
(add-to-list 'package-selected-packages 'yasnippet)

(package-install-selected-packages)

(load-library "chn-paths") ;; exec paths for python, macports; backup and autosave dirs
(load-library "chn-functions") ;; my own one-off functions
(load-library "chn-twine-mode") ;; my twine mode
(load-library "chn-modes") ;; mode-specific settings
(load-library "chn-keys") ;; my own keybindings
;; (load-library "chn-mail") ;; mu4e configuration
(load-library "chn-appearance") ;; fonts, colors, themes
(load-library "chn-ag") ;; the silver searcher settings
(load-library "chn-git") ;; git settings
(load-library "chn-js") ;; javascript settings
(load-library "chn-haskell") ;; haskell settings
(load-library "chn-scala") ;; scala settings
(load-library "chn-python") ;; python settings
(load-library "chn-tabs") ;; special settings for the "tab" key
(load-library "chn-editing") ;; text editing niceties
(load-library "chn-codestyle") ;; text editing niceties
(load-library "chn-misc") ;; hard-to-classify or not-yet-classified

(eshell) ;; begin with a shell

;; Load custom settings
(load custom-file 'noerror)
