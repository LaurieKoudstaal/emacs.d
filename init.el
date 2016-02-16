
;; KEYBOARD MODIFICATIONS
(setq mac-command-modifier 'control)


;; ADDITIONAL PACKAGE ARCHIVES
(require 'package)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)


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
(elpy-use-ipython)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (leuven)))
 '(package-selected-packages (quote (magit exec-path-from-shell elpy)))
 '(python-indent-guess-indent-offset nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
