;;; init.el --- No small task, to awaken a universe from slumber.

;; The Xytaxehedron held to the stars,
;; The incantation uttered with eager tongues,
;; What long-shackled powers of the elder dark
;; have our conjurings loosed?

(setq emacs-root (file-name-directory
                  (or (buffer-file-name) (file-chase-links load-file-name))))

(add-to-list 'load-path (concat emacs-root "lisp"))
(add-to-list 'load-path (concat emacs-root "site-lisp"))

;; Define where the custom user settings are kept
(setq custom-file (concat emacs-root "custom.el"))
;; Load custom settings
(load custom-file 'noerror)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
(package-install 'use-package))

(require 'use-package)

;; Install every declared package, instead of making us do that
;; manually.
(setq use-package-always-ensure t)
;; "Diminished modes are minor modes with no modeline display."
;; We want this feature of use-package.
(use-package diminish)

(require 'chn-lib)

(require 'chn-complete)
(require 'chn-git)
(require 'chn-elisp)
(require 'chn-general)
(require 'chn-editing)
(require 'chn-project)
(require 'chn-elm)
(require 'chn-rust)
(require 'chn-haskell)
(require 'chn-html)
(require 'chn-markdown)
(require 'chn-js)
(require 'chn-ruby)
(require 'chn-python)
(require 'chn-snippets)
(require 'chn-navigation)
(require 'chn-codestyle)
(require 'chn-testing)
(require 'chn-dired)
(require 'chn-eshell)
(require 'chn-window-nav)
(require 'chn-ido)

(load-file (expand-file-name
            (cond ((eq system-type 'windows-nt) "lisp/chn-windows.el")
                  ((eq system-type 'gnu/linux) "lisp/chn-gnu.el")
                  (t "default-system.el"))
            user-emacs-directory))

(load-library "chn-functions") ;; my own one-off functions
(load-library "chn-sugarcube-mode") ;; my twine mode
(load-library "chn-modes") ;; mode-specific settings
(load-library "chn-keys") ;; my own keybindings
(load-library "chn-appearance") ;; fonts, colors, themes
(load-library "chn-scala") ;; scala settings
(load-library "chn-misc") ;; hard-to-classify or not-yet-classified

(add-hook 'kill-buffer-query-functions
          (lambda () (not (member (buffer-name) '("*scratch*" "scratch.el")))))

(load (expand-file-name "local.el" user-emacs-directory) 'no-error)
