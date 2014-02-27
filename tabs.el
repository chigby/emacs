;;; Tab management

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; If there is a tab, make it the size of 2 spaces
(setq-default tab-width 4)

;; Mode specific indent sizes
;; TODO: Consider putting these in their own mode specific inits
(setq c-basic-offset 4)
(setq css-indent-offset 2)
(setq sh-basic-offset 2)
(set-default 'javascript-indent-level 2)

(global-smart-tab-mode 1)
