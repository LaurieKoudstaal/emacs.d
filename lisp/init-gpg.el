;; To setup on mac, make sure that you've installed gpg
;; and pinentry-mac from homebrew
(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)
(provide 'init-gpg)
