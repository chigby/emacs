;;; chn-html.el --- Quanta of the web

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.njk\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :hook
  (web-mode . chn/check-hugo)
  (web-mode . chn/check-django)
  :bind
  (:map web-mode-map
        ([remap web-mode-comment-or-uncomment] . project-switch-project)))

(use-package emmet-mode
  :commands emmet-mode
  :config
  ;; C-j is my open-line-below binding that I like too much to clobber
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  :hook (html-mode web-mode))

(defun in-project-root? (file)
  (f-exists? (concat (vc-git-root buffer-file-name) file)))

(defun chn/check-hugo ()
  (when (and (in-project-root? "content") (-any? 'in-project-root? '("config.json" "config.yaml" "config.toml")))
    (web-mode-set-engine "go")))

(defun chn/check-django ()
  (when (in-project-root? "manage.py") (web-mode-set-engine "django")))

(use-package sass-mode
  :mode (("\\.sass\\'" . sass-mode)))

(defconst sass-line-keywords
  '(("@\\(\\w+\\)"    0 font-lock-keyword-face sass-highlight-directive)
("/[/*].*"  0 font-lock-comment-face)
("[=+]\\w+" 0 font-lock-variable-name-face sass-highlight-script-after-match)
("!\\w+"    0 font-lock-variable-name-face sass-highlight-script-after-match)
(":\\w+"    0 font-lock-variable-name-face)
("\\w+\s*:" 0 font-lock-variable-name-face)
("\\(\\w+\\)\s*="  1 font-lock-variable-name-face sass-highlight-script-after-match)
("\\(:\\w+\\)\s*=" 1 font-lock-variable-name-face sass-highlight-script-after-match)
(".*"      sass-highlight-selector)))

(defconst sass-selector-font-lock-keywords
  '( ;; Attribute selectors (e.g. p[foo=bar])
("\\[\\([^]=]+\\)" (1 font-lock-variable-name-face)
 ("[~|$^*]?=\\([^]=]+\\)" nil nil (1 font-lock-string-face)))
("&"       0 font-lock-constant-face)
("\\.\\w+" 0 font-lock-type-face)
("#\\w+"   0 font-lock-keyword-face)
;; Pseudo-selectors, optionally with arguments (e.g. :first, :nth-child(12))
("\\(::?\\w+\\)" (1 font-lock-variable-name-face)
 ("(\\([^)]+\\))" nil nil (1 font-lock-string-face)))))
(defconst sass-non-block-openers
  '("^.*,$" ;; Continued selectors
"^ *@\\(extend\\|debug\\|warn\\|include\\|import\\)" ;; Single-line mixins
"^ *[$!]"     ;; Variables
".*[^\s-]+: [^\s-]" ;; a setting of some sort
))

(provide 'chn-html)
