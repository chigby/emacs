;;; chn-general.el --- Tools, fundaments, oddities various and sundry

;; Use UTF8 whenever possible.
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Brevity is the soul of wit.
(fset 'yes-or-no-p 'y-or-n-p)

;; Consolidate backups
(setq backup-dir (expand-file-name (concat emacs-root "backup")))
(when (not (file-directory-p backup-dir))
    (make-directory backup-dir t))
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq tramp-backup-directory-alist backup-directory-alist)

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

;; Preserve text copied from the operating system before overwriting
;; it with something else from within emacs.
(setq save-interprogram-paste-before-kill t)

(transient-mark-mode t)

;; Keep up to 100 recent files in 'M-x b' history
(setq recentf-max-saved-items 100)

;; Keep minibuffer history across sessions
(savehist-mode 1)

(provide 'chn-general)
