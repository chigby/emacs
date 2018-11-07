(file-if-exists "~/.zshrc")

;; this file (and mu4e itself) is installed with the command
;; sudo port install mu +emacs
(add-to-list 'load-path (file-if-exists "/opt/local/share/emacs/site-lisp/mu4e"))

(require 'mu4e)
(require 'mu4e-contrib)

(setq
 mu4e-maildir (concat (getenv "HOME") "/Mail")
 mu4e-get-mail-command "mbsync --all"
 mu4e-drafts-folder "/chn/[Gmail]/.Drafts"
 mu4e-sent-folder "/chn/[Gmail]/.Sent Mail"
 mu4e-trash-folder "/chn/[Gmail]/.Trash"
 mu4e-sent-messages-behavior 'delete
 mu4e-headers-skip-duplicates t
 mu4e-confirm-quit nil
 mu4e-index-update-error-continue nil
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
 ;;mu4e-html2text-command 'mu4e-shr2text
 mu4e-view-prefer-html nil
 mu4e-view-show-images t)

(add-to-list
 'mu4e-view-actions
 '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(add-hook 'mu4e-view-mode-hook (lambda () (visual-line-mode)))
(add-hook
 'mu4e-compose-pre-hook
 (lambda ()
   "Set the From address based on the To address of the original."
   (let ((msg mu4e-compose-parent-message))
     (when msg
       (setq user-mail-address
             (cond
              ((mu4e-message-contact-field-matches msg :to "me@chigby.org")
               "me@chigby.org")
              ((mu4e-message-contact-field-matches msg :to "simulacrum@chigby.org")
               "simulacrum@chigby.org")
              ((mu4e-message-contact-field-matches msg :to "chigby@sdf.lonestar.org")
               "chigby@sdf.lonestar.org")
              (t "chn@nullsurface.com")))))))

(package-require 'async)
(require 'smtpmail-async)

;; use msmtp to send email
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/opt/local/bin/msmtp")
;; tell msmtp to choose the SMTP server according to the from field in the outgoing email
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)

;; (setq
;; ;; send-mail-function 'async-smtpmail-send-it
;;  ;; message-send-mail-function 'async-smtpmail-send-it
;;  send-mail-function 'smtpmail-send-it
;;  message-send-mail-function 'smtpmail-send-it

;;  smtpmail-stream-type 'starttls
;;  smtpmail-default-smtp-server "smtp.gmail.com"
;;  smtpmail-smtp-user "chn@nullsurface.com"
;;  smtpmail-smtp-server "smtp.gmail.com"
;;  smtpmail-smtp-service 587
;;  smtpmail-queue-mail nil
;;  smtpmail-queue-dir "~/Mail/chn/Queue/cur")

(global-set-key (kbd "C-c m") 'mu4e)

(define-key mu4e-main-mode-map (kbd ".") 'mu4e-update-mail-and-index)
(define-key mu4e-headers-mode-map (kbd ".") 'mu4e-update-mail-and-index)

(defun kill-old-message ()
  "Delete everything in the buffer except sig line."
  (interactive)
  (save-excursion
    (let (p1 p2)
      (setq p1 (point))
      (goto-char (point-max))
      (previous-line 2)
      (setq p2 (point))
      (delete-region p1 p2))))

(defun chn-mail-mode-keys ()
  (define-key mail-mode-map [(control c) (control c)]
    (lambda ()
      (interactive)
      (save-buffer)
      (server-edit)))
  (define-key mail-mode-map (kbd "C-c C-d") 'kill-old-message)
  (local-set-key (kbd "C-c C-d") 'kill-old-message))

(add-to-list 'auto-mode-alist '("/mutt\\|itsalltext.*mail\\.google" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
(add-hook 'mail-mode-hook 'chn-mail-mode-keys)
