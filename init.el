
;;; Detect system-type
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-win* (eq system-type 'windows-nt))

;;; Set up melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired  
  ;; (add-to-list 'package-archives (cons "marmalade" (concat proto "://marmalade-repo.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)



;;; On-demand installation of packages
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


;;; GUI
(menu-bar-mode -1)
(tool-bar-mode -1)


;;; Fonts
(when *is-win*
  (add-to-list 'default-frame-alist '(font . "Consolas-10" ))
  (set-face-attribute 'default t :font "Consolas-10" ))


;;; Keyboard Remappings
(when *is-a-mac*
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'meta))
(when *is-win*
  (setq w32-apps-modifier 'control))



;;; Better defaults package
(require-package 'better-defaults)
(require 'better-defaults)



;;; God mode
(require-package 'god-mode)
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'c/god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'c/god-mode-update-cursor)

(defun c/god-mode-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond ((or (bound-and-true-p god-local-mode)
               (bound-and-true-p buffer-read-only))
           (progn
             (set-face-background 'mode-line (if limited-colors-p "white" "salmon3"))
             (set-face-background 'mode-line-inactive (if limited-colors-p "white" "salmon4"))
             (set-face-background 'cursor "salmon")))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'cursor "light gray"))))))


;;; Org Mode
(add-to-list 'auto-mode-alist '("\\org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/refile.org"))
(define-key global-map "\C-cc" 'org-capture)


;;; Org Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages '((sqlite . t)))


;;; Groovy mode
(require-package 'groovy-mode)
(require 'groovy-mode)



;;; Magit setup
(global-set-key (kbd "C-x g") 'magit-status)


;; Custom settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
    ("cdbd0a803de328a4986659d799659939d13ec01da1f482d838b68038c1bb35e8" "599f1561d84229e02807c952919cd9b0fbaa97ace123851df84806b067666332" default)))
 '(org-agenda-files
   (quote
    ("~/org/projects.org")))
 '(package-selected-packages (quote (groovy-mode god-mode better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;;; Enable theme
(require-package 'zenburn-theme)
(require 'zenburn-theme)
(load-theme 'zenburn)

