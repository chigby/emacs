;;; chn-project.el --- taskmasters, overseers, and orchestrators

(use-package rg
  :config
  (setq rg-default-alias-fallback "all")
  :custom
  (rg-enable-menu)
  :bind ("M-'" . rg-project)
  ("C-M-'" . rg-menu))

(require 'bookmark)
(bookmark-maybe-load-default-file)
(global-set-key (kbd "C-<f6>") 'bookmark-bmenu-list)

;; Headlong -- deprecated
;; ;; refer to https://oremacs.com/2015/01/06/rushing-headlong/
;; (use-package headlong
;;   :bind ("M-;" . headlong-bookmark-jump))
;; consider alternatives: https://www.reddit.com/r/emacs/comments/q1zi0t/package_shoutout_consultdir_and_embark/
;; consult-dir slash embark?
;; see also https://protesilaos.com/emacs/dotemacs section 3.1.5.1 (switch to directories)
;; also relevant (but not as a headlong replacement) is section 3.1.7 of the above document
;; more about project.el: https://www.reddit.com/r/emacs/comments/kfubcb/comment/ggav4un/

(use-package project
  :ensure nil ;; elpaca shouldn't manage the built-in library
  :custom
  (project-key-prompt-style 'brackets)
  (project-switch-commands
   '((?f "File" project-find-file)
     (?d "Dired" project-dired)
     (?r "Ripgrep" rg-project)
     (?m "Magit" magit-project-status)
     (?b "Buffer" project-switch-to-buffer)
     ))
  :bind ("M-;" . project-switch-project))

;; some next ideas:
;;
;; 1. figure out how to used `fd' instead of `find' for
;; `project-files'.  See
;; https://www.manueluberti.eu/emacs/2020/11/14/extending-project/ for
;; more information.
;; 2. Consider binding C-c f (or C-c C-f) to project-find-file --
;; though I'm also okay to ponder the default keybinding for a while
;; to see if, I dunno, it builds my character (and loops me into the
;; rest of the commands that C-x p prefixes, maybe).


(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(provide 'chn-project)
