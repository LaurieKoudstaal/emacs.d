;; To setup on mac, make sure that you've installed gpg
;; and pinentry-mac from homebrew
(require 'epa-file)
(require-package 'pinentry)
(setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
(pinentry-start)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)

(defun offlineimap-get-password (host port)
      (let* ((netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
             (hostentry (netrc-machine netrc host port port)))
        (when hostentry (netrc-get hostentry "password"))))

(provide 'init-gpg)
