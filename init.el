;;; An emacs init based on Steve Purcell's init.el

(package-initialize)

(let ((minver "24"))
  (when (version<= emacs-version "24")
    (error "Emacs out of date for this configuration. Upgrade to at least %s" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *is-a-mac* (eq system-type 'darwin))

;; BOOTSTRAP CONFIG
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-elpa)
(require 'init-exec-path)
(require 'init-osx-compat)

;; OTHER CONFIG
(require 'init-osx-keys)
;; (require 'init-elpy)
(require 'init-themes)
(require 'init-gui)
(require 'init-org)
(require 'init-org-bullets)
(require 'init-python-mode)
(require 'init-smex)
(require 'init-multiple-cursors)
(require 'init-god-mode)
(require 'init-whitespace)
(require 'init-magit)
(require 'init-backups)
(require 'init-web-mode)
(require 'init-eshell)
(require 'init-tramp)
(require 'init-shell)
(require 'init-ido)
(require 'init-undo-tree)
(require 'init-js2)
(require 'init-mssql)


