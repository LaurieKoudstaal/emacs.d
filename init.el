;;; An emacs init based on Steve Purcell's init.el


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
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
(require 'init-python-mode)
(require 'init-smex)


;; WHITESPACE SETTINGS
(require 'whitespace)
 (setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; MAGIT commands
(global-set-key (kbd "C-x g") 'magit-status)


;; GET RID OF TOOLBAR
;; (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; HANDY F1
(global-set-key (kbd "<f1>") 'execute-extended-command)

;; ADD ASPELL
(setq-default ispell-program-name "/usr/local/bin/aspell")




;; BACKUPS
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; FOR JSX
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; FIX A BUG IN ESHELL SO I CAN RUN SOURCES
(defadvice eshell-gather-process-output (before absolute-cmd (command args) act)
  (setq command (file-truename command)))


;; MAKE SSH THE DEFAULT TRAMP MODE
(require 'tramp) (setq tramp-default-method "ssh")

;; RESTART SHELL PROCESS
(defun restart-shell () (interactive) (shell (current-buffer)))

;; RUN SHELL ON RDS
(defun rds-shell (arg)
  (interactive "P")
  (let ((default-directory "/ssh:veeva@173.255.143.2:"))
    (setq current-prefix-arg arg)
    (call-interactively 'shell)))

;; INTERACTIVELY DO THINGS
(require 'ido)
(ido-mode t)

;; START SERVER SO EMACSCLIENT WORKS
(server-start)

;; MULTIPLE CURSORS
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; EVIL MODE
(add-to-list 'load-path "~/.emacs.d/undo-tree")
(require 'undo-tree)
(global-undo-tree-mode 1)

;; (add-to-list 'load-path "~/.emacs.d/evil")
;; (require 'evil)
;; (evil-mode 1)

;; GOD MODE
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)

;; JS2 MODE
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;; mssql
;; (add-to-list 'load-path "~/Documents/sources/mssql/emacs/")
;; (require 'sql-ms)

;; FIX SQL MODE TO USE sqsh
(set 'sql-ms-program "sqsh")
   (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (setq sql-prompt-regexp "^[_[:alpha:]]*[=][#>] ")
              (setq sql-prompt-cont-regexp "^[_[:alpha:]]*[-][#>] ")))
;; (setq sql-ms-options (remove "-n" sql-ms-options))

;; (defun sql-comint-ms (product options)
;;   "Create comint buffer and connect to Microsoft SQL Server."
;;   ;; Put all parameters to the program (if defined) in a list and call
;;   ;; make-comint.
;;   (let ((params options))
;;     (if (not (string= "" sql-server))
;;         (setq params (append (list "-S" sql-server) params)))
;;     (if (not (string= "" sql-database))
;;         (setq params (append (list "-D" sql-database) params)))
;;     (if (not (string= "" sql-user))
;;     (setq params (append (list "-U" sql-user) params)))
;;     (if (not (string= "" sql-password))
;;     (setq params (append (list "-P" sql-password) params))
;;       (if (string= "" sql-user)
;;       ;; if neither user nor password is provided, use system
;;       ;; credentials.
;;       (setq params (append (list "-E") params))
;;     ;; If -P is passed to ISQL as the last argument without a
;;     ;; password, it's considered null.
;;     (setq params (append params (list "-P")))))
;;     (sql-comint product params)))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-faces-vector
;;    [default default default italic underline success warning error])
;;  '(ansi-color-names-vector
;;    ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
;;  '(custom-enabled-themes (quote (twilight-bright)))
;;  '(custom-safe-themes
;;    (quote
;;     ("0022e0b80aaf697a4dc41322d5270aff5c4dae342c09a559abb91fd2bc64e755" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" "39e93a10eb292941640adfe28509e0c3eeb84e30cbfed6ef9841be136081ca34" "0ae52e74c576120c6863403922ee00340a3bf3051615674c4b937f9c99b24535" "bcd39b639704f6f28ab61ad1ac8eb4625be77d027b4494059e8ada22ce281252" "9e76732c9af8e423236ff8e37dd3b9bc37dacc256e42cc83810fb824eaa529b9" "07840b49217157323d6ea4ccbdecc451b5989ebdc6e06cb0b4d742a141475a44" "08dc5159473fa2250619880857eee06b7f4067f5f15b0ee8878c91f135cef6d5" "1a2b131a7844bad234832963d565097efc88111b196fb75757885c159c5f8137" "46b20113556c07c1173d99edc6609473a106c13871da8fc9acb6534224f1e3e4" "3a3917dbcc6571ef3942c2bf4c4240f70b5c4bc0b28192be6d3f9acd83607a24" "90b1aeef48eb5498b58f7085a54b5d2c9efef2bb98d71d85e77427ce37aec223" "3fb38c0c32f0b8ea93170be4d33631c607c60c709a546cb6199659e6308aedf7" "4af6fad34321a1ce23d8ab3486c662de122e8c6c1de97baed3aa4c10fe55e060" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "3b24f986084001ae46aa29ca791d2bc7f005c5c939646d2b800143526ab4d323" "37def0fac11a4890922af9febc8394e3b6e3c68904a294a2d440b1904e979c7e" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "c58382b9c4fff1aa94b8e3f0f81b0212bb554e83f76957bab735f960a4c441b1" default)))
;;  '(fci-rule-color "#f1c40f" t)
;;  '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
;;  '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
;;  '(nrepl-message-colors
;;    (quote
;;     ("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
;;  '(org-agenda-files (quote ("~/Documents/notes.org")))
;;  '(package-selected-packages
;;    (quote
;;     (god-mode go-mode minimal-theme basic-theme plan9-theme jsx-mode web-mode skewer-mode js2-mode el-get multiple-cursors csv-mode gandalf-theme twilight-bright-theme org-plus-contrib magit exec-path-from-shell elpy)))
;;  '(sml/active-background-color "#34495e")
;;  '(sml/active-foreground-color "#ecf0f1")
;;  '(sml/inactive-background-color "#dfe4ea")
;;  '(sml/inactive-foreground-color "#34495e")
;;  '(sql-ms-login-params (quote (user password server)))
;;  '(sql-ms-options nil)
;;  '(sql-sybase-program "/usr/local/bin/sqsh")
;;  '(vc-annotate-background "#ecf0f1")
;;  '(vc-annotate-color-map
;;    (quote
;;     ((30 . "#e74c3c")
;;      (60 . "#c0392b")
;;      (90 . "#e67e22")
;;      (120 . "#d35400")
;;      (150 . "#f1c40f")
;;      (180 . "#d98c10")
;;      (210 . "#2ecc71")
;;      (240 . "#27ae60")
;;      (270 . "#1abc9c")
;;      (300 . "#16a085")
;;      (330 . "#2492db")
;;      (360 . "#0a74b9"))))
;;  '(vc-annotate-very-old-color "#0a74b9"))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
