;;; chn-general.el --- Tools, fundaments, oddities various and sundry

(prefer-coding-system 'utf-8)
;; Consolidate autosaves
(setq autosave-dir (expand-file-name (concat emacs-root "autosave/")))
(if (not (file-directory-p autosave-dir))
    (make-directory autosave-dir t))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(transient-mark-mode t)

;; Keep up to 100 recent files in 'M-x b' history
(setq recentf-max-saved-items 100)

;; Keep minibuffer history across sessions
(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))
(use-package which-func
  :ensure nil
  :hook ((python-base-mode) . which-function-mode))

(provide 'chn-general)
