;; ADDITIONAL PACKAGE ARCHIVES
(require 'package)

;; I like marmalade for breakfast.
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Elpy is helpful for python
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

;; Melpa is the beez-neez of package repositories.
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; I understand this probably isn't needed
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; So that I can add sunrise commander
(add-to-list 'package-archives '("sunrise-commander" . "http://joseito.republika.pl/sunrise-commander/") t)

;; Enable on-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(provide 'init-elpa)
