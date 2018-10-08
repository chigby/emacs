;;; init.el --- No small task, to awaken a universe from slumber.

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
(when (not package-archive-contents)
  (package-refresh-contents))
(add-to-list 'package-selected-packages 'use-package)
(package-install-selected-packages)

(require 'use-package)

;; Install every declared package, instead of making us do that
;; manually.
(setq use-package-always-ensure t)
;; "Diminished modes are minor modes with no modeline display."
;; We want this feature of use-package.
(use-package diminish)

(require 'chn-lib)

;; Check for and install missing packages
(add-to-list 'package-selected-packages 'ahk-mode)
(add-to-list 'package-selected-packages 'elixir-mode)
(add-to-list 'package-selected-packages 'elm-mode)
(add-to-list 'package-selected-packages 'haskell-mode)
(add-to-list 'package-selected-packages 'js2-mode)
(add-to-list 'package-selected-packages 'lua-mode)
(add-to-list 'package-selected-packages 'magit)
(add-to-list 'package-selected-packages 'markdown-mode)
(add-to-list 'package-selected-packages 'powershell)
(add-to-list 'package-selected-packages 'puppet-mode)
(add-to-list 'package-selected-packages 'salt-mode)
(add-to-list 'package-selected-packages 'scala-mode)
(add-to-list 'package-selected-packages 'virtualenvwrapper)
(add-to-list 'package-selected-packages 'yaml-mode)

(require 'chn-elisp)
(require 'chn-general)
(require 'chn-editing)
(require 'chn-project)
(require 'chn-rust)
(require 'chn-html)
(require 'chn-snippets)
(require 'chn-navigation)
(require 'chn-codestyle)

(load-file (expand-file-name
            (cond ((eq system-type 'windows-nt) "lisp/chn-windows.el")
                  ((eq system-type 'gnu/linux) "lisp/chn-gnu.el")
                  (t "default-system.el"))
            user-emacs-directory))

(load-library "chn-paths") ;; exec paths for python, macports; backup and autosave dirs
(load-library "chn-functions") ;; my own one-off functions
(load-library "chn-twine-mode") ;; my twine mode
(load-library "chn-modes") ;; mode-specific settings
(load-library "chn-keys") ;; my own keybindings
;; (load-library "chn-mail") ;; mu4e configuration
(load-library "chn-appearance") ;; fonts, colors, themes
(load-library "chn-git") ;; git settings
(load-library "chn-js") ;; javascript settings
(load-library "chn-haskell") ;; haskell settings
(load-library "chn-scala") ;; scala settings
(load-library "chn-python") ;; python settings
(load-library "chn-misc") ;; hard-to-classify or not-yet-classified

(eshell) ;; begin with a shell

;; Load custom settings
(load custom-file 'noerror)

(add-hook 'kill-buffer-query-functions
          (lambda () (not (member (buffer-name) '("*scratch*" "scratch.el")))))

(load (expand-file-name "local.el" user-emacs-directory) 'no-error)
