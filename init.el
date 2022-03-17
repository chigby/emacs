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

;; straight.el
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; "Diminished modes are minor modes with no modeline display."
;; We want this feature of use-package.
(use-package diminish)

(require 'chn-lib)

(require 'chn-appearance)
(require 'chn-lsp)
(require 'chn-complete)
(require 'chn-git)
(require 'chn-elisp)
(require 'chn-general)
(require 'chn-editing)
(require 'chn-project)
(require 'chn-elm)
(require 'chn-rust)
(require 'chn-nim)
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

(load-file (expand-file-name
            (cond ((eq system-type 'windows-nt) "lisp/chn-windows.el")
                  ((eq system-type 'gnu/linux) "lisp/chn-gnu.el")
                  (t "default-system.el"))
            user-emacs-directory))

(load-library "chn-functions") ;; my own one-off functions
(load-library "chn-sugarcube-mode") ;; my twine mode
(load-library "chn-octo-mode") ;; mode for editing Octo files
(load-library "chn-modes") ;; mode-specific settings
(load-library "chn-keys") ;; my own keybindings
(load-library "chn-scala") ;; scala settings
(load-library "chn-misc") ;; hard-to-classify or not-yet-classified

(add-hook 'kill-buffer-query-functions
          (lambda () (not (member (buffer-name) '("*scratch*" "scratch.el")))))

(load (expand-file-name "local.el" user-emacs-directory) 'no-error)
