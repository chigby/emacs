(file-if-exists "~/.zshrc")

(add-to-list 'load-path (file-if-exists "/opt/local/share/emacs/site-lisp/mu4e"))

(require 'mu4e)

(setq
 mu4e-maildir "~/Mail"
 mu4e-get-mail-command "mbsyncwrapper"
 mu4e-drafts-folder "/chn/[Gmail]/.Drafts"
 mu4e-sent-folder "/chn/[Gmail]/.Sent Mail"
 mu4e-trash-folder "/chn/[Gmail]/.Trash"
 mu4e-sent-messages-behavior 'delete
 mu4e-headers-skip-duplicates t
 mu4e-confirm-quit nil
 )

(setq
 user-mail-address "chn@nullsurface.com"
 user-full-name "Cameron Higby-Naquin"
 mu4e-compose-signature "Cameron"
 mu4e-user-mail-address-list
 '("chn@nullsurface.com"
   "me@chigby.org"
   "simulacrum@chigby.org"
   "chigby@sdf.lonestar.org")
 mu4e-attachment-dir "~/Downloads")


;; Columns to display in message list view
(setq
 mu4e-headers-fields
 '((:human-date . 10)
   (:flags . 5)
   (:from-or-to . 22)
   (:subject))
 mu4e-headers-time-format "Tdy %H:%M"
 mu4e-headers-date-format "%e %b %y"
 ;;mu4e-use-fancy-chars nil
 )

;; Showing HTML
(setq
 mu4e-html2text-command "w3m -T text/html"
 ;; mu4e-html2text-command 'mu4e-shr2text
 mu4e-view-prefer-html t
 mu4e-view-show-images t)

(package-require 'async)
(require 'smtpmail-async)


;; use msmtp to send email
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/opt/local/bin/msmtp")
;; tell msmtp to choose the SMTP server according to the from field in the outgoing email
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)

(setq
;; send-mail-function 'async-smtpmail-send-it
 ;; message-send-mail-function 'async-smtpmail-send-it
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it

 smtpmail-stream-type 'starttls
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-user "chn@nullsurface.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587
 smtpmail-queue-mail nil
 smtpmail-queue-dir "~/Mail/chn/Queue/cur")
