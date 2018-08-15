;; scala-mode2

; turn off tab indenting in scala
(add-hook 'scala-mode-hook (lambda () (setq indent-tabs-mode nil)))

; fix indenting to be more what i want
(setq scala-indent:indent-value-expression nil)
(setq scala-indent:align-parameters nil)
(setq scala-indent:align-forms nil)
(setq scala-indent:use-javadoc-style t)
; don't case fold for ctags
(setq tags-case-fold-search nil)
