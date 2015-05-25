(require 'package)

(setq package-list '(ack-and-a-half auctex puppet-mode apache-mode dsvn js2-mode))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defun package-require (package)
  (when (not (package-installed-p package))
    (package-install package)))
