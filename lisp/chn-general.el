;;; chn-general.el --- Tools, fundaments, oddities various and sundry

;; Use UTF8 whenever possible.
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Brevity is the soul of wit.
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable lockfiles (I almost never run more than one emacs instance)
(setq create-lockfiles nil)

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
(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

;; via https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/#h:83c8afc4-2359-4ebe-8b5c-f2e5257bdda3
(defun chn/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'chn/keyboard-quit-dwim)


(provide 'chn-general)
