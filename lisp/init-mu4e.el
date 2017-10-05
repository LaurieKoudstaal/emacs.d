(when (file-accessible-directory-p "/usr/local/share/emacs/site-lisp/mu/mu4e")
      (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
      (require 'mu4e)
      (setq
       mu4e-maildir       "~/Maildir"   ;; top-level Maildir
       mu4e-sent-folder   "/Sent Items"       ;; folder for sent messages
       mu4e-drafts-folder "/Drafts"     ;; unfinished messages
       mu4e-trash-folder  "/Trash"      ;; trashed messages
       mu4e-refile-folder "/Archive")   ;; saved messages

      (setq
       user-mail-address "laurie.koudstaal@fastmail.com"
       user-full-name "Laurie Koudstaal"
       )

      ;; tell message-mode how to send mail
      (require 'smtpmail)
      (setq message-send-mail-function 'message-send-mail-with-sendmail
	    smtpmail-smtp-server "smtp.fastmail.com"
	    mtpmail-stream-type  'ssl
	    smtpmail-smtp-service 465
	    sendmail-program "/usr/local/bin/msmtp"))
(provide 'init-mu4e)
