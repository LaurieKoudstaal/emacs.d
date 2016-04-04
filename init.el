;; KEYBOARD MODIFICATIONS
(setq mac-command-modifier 'control)


;; ADDITIONAL PACKAGE ARCHIVES
(require 'package)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


;; EXEC PATH FROM SHELL
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; COPY PYTHON PATH FROM SHELL
(exec-path-from-shell-copy-env "PYTHONPATH")

;; ADDITIONAL PACKAGES
(elpy-enable)

;; ELPY USE IPYTHON
;; (elpy-use-ipython)

;; PYTHON MODE
(add-hook 'python-mode-hook
	  (lambda ()
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; WHITESPACE SETTINGS
(require 'whitespace)
 (setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; MAGIT commands
(global-set-key (kbd "C-x g") 'magit-status)


;; GET RID OF TOOLBAR
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; HANDY F1
(global-set-key (kbd "<f1>") 'execute-extended-command)

;; ADD ASPELL
(setq-default ispell-program-name "/usr/local/bin/aspell")

;; TSV in ORGMODE
(defun my-export-to-parent ()
  "Exports the table in the current buffer back to its parent DSV file and
    then closes this buffer."
  (let ((buf (current-buffer)))
    (org-table-export parent-file export-func)
    (set-buffer-modified-p nil)
    (switch-to-buffer (find-file parent-file))
    (kill-buffer buf)))

(defun my-edit-dsv-as-orgtbl (&optional arg)
  "Convet the current DSV buffer into an org table in a separate file. Saving
    the table will convert it back to DSV and jump back to the original file"
  (interactive "P")
  (let* ((buf (current-buffer))
         (file (buffer-file-name buf))
         (txt (substring-no-properties (buffer-string)))
         (org-buf (find-file-noselect (concat (buffer-name) ".org"))))
    (save-buffer)
    (with-current-buffer org-buf
      (erase-buffer)
      (insert txt)
      (org-table-convert-region 1 (buffer-end 1) arg)
      (setq-local parent-file file)
      (cond
       ((equal arg '(4)) (setq-local export-func "orgtbl-to-csv"))
       ((equal arg '(16)) (setq-local export-func "orgtbl-to-tsv"))
       (t (setq-local export-func "orgtbl-to-tsv")))
      (add-hook 'after-save-hook 'my-export-to-parent nil t))
    (switch-to-buffer org-buf)
    (kill-buffer buf)))

;; Open the current TSV file as an Org table
(global-set-key (kbd "C-c |") 'my-edit-dsv-as-orgtbl)


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

;; ORG MODE BINDING FOR capture new item
(eval-after-load 'org-mode
  '(define-key org-mode-map (kbd "C-c i") 'org-id-get-create))

;; INTERACTIVELY DO THINGS
(require 'ido)
(ido-mode t)


;; START SERVER SO EMACSCLIENT WORKS
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (twilight-bright)))
 '(custom-safe-themes
   (quote
    ("39e93a10eb292941640adfe28509e0c3eeb84e30cbfed6ef9841be136081ca34" "0ae52e74c576120c6863403922ee00340a3bf3051615674c4b937f9c99b24535" "bcd39b639704f6f28ab61ad1ac8eb4625be77d027b4494059e8ada22ce281252" "9e76732c9af8e423236ff8e37dd3b9bc37dacc256e42cc83810fb824eaa529b9" "07840b49217157323d6ea4ccbdecc451b5989ebdc6e06cb0b4d742a141475a44" "08dc5159473fa2250619880857eee06b7f4067f5f15b0ee8878c91f135cef6d5" "1a2b131a7844bad234832963d565097efc88111b196fb75757885c159c5f8137" "46b20113556c07c1173d99edc6609473a106c13871da8fc9acb6534224f1e3e4" "3a3917dbcc6571ef3942c2bf4c4240f70b5c4bc0b28192be6d3f9acd83607a24" "90b1aeef48eb5498b58f7085a54b5d2c9efef2bb98d71d85e77427ce37aec223" "3fb38c0c32f0b8ea93170be4d33631c607c60c709a546cb6199659e6308aedf7" "4af6fad34321a1ce23d8ab3486c662de122e8c6c1de97baed3aa4c10fe55e060" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "3b24f986084001ae46aa29ca791d2bc7f005c5c939646d2b800143526ab4d323" "37def0fac11a4890922af9febc8394e3b6e3c68904a294a2d440b1904e979c7e" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "c58382b9c4fff1aa94b8e3f0f81b0212bb554e83f76957bab735f960a4c441b1" default)))
 '(fci-rule-color "#f1c40f")
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(nrepl-message-colors
   (quote
    ("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(package-selected-packages
   (quote
    (multiple-cursors csv-mode gandalf-theme twilight-bright-theme org-plus-contrib magit exec-path-from-shell elpy)))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(vc-annotate-background "#ecf0f1")
 '(vc-annotate-color-map
   (quote
    ((30 . "#e74c3c")
     (60 . "#c0392b")
     (90 . "#e67e22")
     (120 . "#d35400")
     (150 . "#f1c40f")
     (180 . "#d98c10")
     (210 . "#2ecc71")
     (240 . "#27ae60")
     (270 . "#1abc9c")
     (300 . "#16a085")
     (330 . "#2492db")
     (360 . "#0a74b9"))))
 '(vc-annotate-very-old-color "#0a74b9"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
