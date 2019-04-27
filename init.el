
;;; Detect system-type
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-win* (eq system-type 'windows-nt))


;;; Add site lisp
(eval-when-compile (require 'cl))
(defun sanityinc/add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (remove-if-not
              (lambda (dir) (file-directory-p dir))
              (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
             load-path)))))

(sanityinc/add-subdirs-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))

(add-to-list 'load-path
 (expand-file-name "site-lisp/magit/lisp" user-emacs-directory))


;;; GUI elements
(menu-bar-mode 1)
(tool-bar-mode -1)


;;; Fonts
;(when *is-win*
;  (add-to-list 'default-frame-alist '(font . "Consolas-10" ))
;  (set-face-attribute 'default t :font "Consolas-10" ))


;;; Keyboard Remappings
(when *is-a-mac*
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'meta))
(when *is-win*
  (setq w32-apps-modifier 'control))



;;; Better defaults package
(require 'better-defaults)


;;; Org Mode
;(add-to-list 'auto-mode-alist '("\\org\\'" . org-mode))
;(global-set-key "\C-cl" 'org-store-link)
;(global-set-key "\C-ca" 'org-agenda)
;(setq org-directory "~/org")
;(setq org-default-notes-file (concat org-directory "/refile.org"))
;(define-key global-map "\C-cc" 'org-capture)


;;; Org Babel languages
;(org-babel-do-load-languages
; 'org-babel-load-languages '((sqlite . t)))


;;; Magit setup
(require 'dash)
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)






