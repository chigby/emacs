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
;;(defvar comp-deferred-compilation-deny-list nil)
;; elpaca
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t)
)

;;(elpaca (dash :wait t) (message "dash done"))
(elpaca f)
(elpaca s)
(elpaca-wait)
;;(require 'dash)
(require 's)
(require 'f)


;; "Diminished modes are minor modes with no modeline display."
;; We want this feature of use-package.
(use-package diminish :ensure t :demand t)

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

(use-package chn-windows
  :ensure nil
  :if (equal system-type 'windows-nt))

(use-package chn-gnu
  :ensure nil
  :if (equal system-type 'gnu/linux))

(load-library "chn-functions") ;; my own one-off functions
(load-library "chn-modes") ;; mode-specific settings
(load-library "chn-keys") ;; my own keybindings
(load-library "chn-scala") ;; scala settings
(load-library "chn-misc") ;; hard-to-classify or not-yet-classified

(add-hook 'kill-buffer-query-functions
          (lambda () (not (member (buffer-name) '("*scratch*" "scratch.el")))))

(use-package extra-config :ensure nil :if (f-exists-p "~/extra")
  :load-path "~/extra")
