(require 'package)

(setq package-list '(ack-and-a-half auctex puppet-mode apache-mode dsvn whitespace php-mode markdown-mode js2-mode yasnippet git-commit yaml-mode rvm lua-mode))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; install missing packages from package-list
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))
