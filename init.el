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

(defvar native-comp-deferred-compilation-deny-list nil)

;;; Package configuration
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-install-upgrade-built-in t)

;;;; use-package
(defmacro use-feature (name &rest args)
  "`use-package' for packages which do not require installation.
  See `use-package' for NAME and ARGS."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

(if init-file-debug
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
    (setq use-package-verbose nil
          use-package-expand-minimally t))

;;; A macro to bind keys
;; via https://www.reddit.com/r/emacs/comments/1207uds/comment/jdham2y/
(defmacro defkeys (mapname &rest body)
  `(let ((defs '(,@body)))
     (while defs
       (define-key
        ,mapname
        (if (vectorp (car defs))
            (car defs)
          (read-kbd-macro (car defs)))
        (if (or (listp (cadr defs)) (functionp (cadr defs)))
            (cadr defs)
          (if `(keymapp (bound-and-true-p ,(cadr defs)))
              (eval (cadr defs)))))
       (setq defs (cddr defs)))))

(use-package f :ensure t)

;; "Diminished modes are minor modes with no modeline display."
;; We want this feature of use-package.
(use-package diminish :ensure t)

(require 'chn-emacs)
(require 'chn-lib)

(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"
                 "v0.23.3"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.23.5"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
        ;;(dockerfile . (""))a
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
        (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"
                    "v0.23.1"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"
                 "v0.20.1"))
        (javascript .
                    ("https://github.com/tree-sitter/tree-sitter-javascript"
                     "v0.20.1" "src"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"
                 "v0.20.2"))
        (just . ("https://github.com/IndianBoy42/tree-sitter-just" "main"))
        (markdown . ("https://github.com/ikatyang/tree-sitter-markdown"
                     "v0.7.1"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"
                   "v0.20.4"))
        (regex . ("https://github.com/tree-sitter/tree-sitter-regex"
                  "v0.24.3"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"
                 "v0.21.2"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml"
                 "v0.5.1"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript"
                "v0.20.3" "tsx/src"))
        (typescript .
                    ("https://github.com/tree-sitter/tree-sitter-typescript"
                     "v0.20.3" "typescript/src"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

(customize-set-variable 'treesit-font-lock-level 4)

(require 'chn-appearance)
(require 'chn-lsp)
(require 'chn-complete)
(require 'chn-git)
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

;;; Plain text
;; Borrowed from https://protesilaos.com/emacs/dotemacs
(defun simple-unfill-region-or-paragraph ()
  "Unfill current paragraph or the active region."
  (interactive)
  (unless mark-ring ; needed when entering a new buffer
    (push-mark (point) t nil))
  (let ((fill-column most-positive-fixnum))
    (if (region-active-p)
        (fill-region (region-beginning) (region-end))
      (fill-paragraph))))

(defkeys global-map
         "M-Q" simple-unfill-region-or-paragraph
         "M-=" count-words
         )

;;; puni
(use-package puni
  :ensure t
  :hook ((elm-mode haskell-mode js-base-mode python-base-mode rust-mode) . puni-mode)
  :bind (:map puni-mode-map
              ("M-i" . puni-change-inner))
  :init
  (setq puni-read-char-for-change-inner t))

;;; expreg
(use-package expreg
  :ensure t
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))

;;; Crux
(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)))

;;; Docker
(use-package docker
  :bind ("C-c d" . docker))

(use-feature  dockerfile-ts-mode
  :mode "\\(Containerfile\\|Dockerfile\\)\\'")

;;; Just
(use-package just-ts-mode
  :ensure t
  :defer t
  )

;;; Platform-specific code
;;;; Windows
(use-package chn-windows
  :ensure nil
  :if (equal system-type 'windows-nt))
;;;; GNU/Linux
(use-package chn-gnu
  :ensure nil
  :if (equal system-type 'gnu/linux))
;;;; macOS
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta))

(load-library "chn-functions") ;; my own one-off functions
(load-library "chn-modes") ;; mode-specific settings
(load-library "chn-keys") ;; my own keybindings

(add-hook 'kill-buffer-query-functions
          (lambda () (not (member (buffer-name) '("*scratch*" "scratch.el")))))

(use-package extra-config :ensure nil :if (f-exists-p "~/extra")
  :load-path "~/extra")


;; Local Variables:
;; outline-minor-mode-cycle: t
;; outline-regexp: ";;;+ "
;; eval: (outline-minor-mode)
;; End:
