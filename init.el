
;;; Detect system-type
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-win* (eq system-type 'windows-nt))


;; useful after-load macro
(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


;;; init exec path
(after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))


(when (memq window-system '(mac ns x))
  (setq-default exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))



;;; Add site lisp
(eval-when-compile (require 'cl))
(defun lkk/list-recursive-subdirs (parent-dir)
  (let* ((sub-dirs (remove-if-not
		    (lambda (dir) (file-directory-p dir))
		    (directory-files (expand-file-name parent-dir) t "^[^\\.]"))))
    (cons parent-dir
            (when (not (null sub-dirs))
      	      (apply #'append (mapcar 'lkk/list-recursive-subdirs sub-dirs))))))
	  
(defun lkk/add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path      
            (append (remove-if
                     (lambda (dir) (null (file-expand-wildcards (expand-file-name "*.el" dir))))
                     (lkk/list-recursive-subdirs parent-dir))
		     load-path)))))

(lkk/add-subdirs-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))



;;; Fonts
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10" ))
(set-face-attribute 'default t :font "DejaVu Sans Mono-10")
(set-face-attribute 'variable-pitch t :font "DejaVu Sans-10")



;;;Page break lines
(require 'page-break-lines)
(require 'tinypage)
(page-break-lines-mode)



;;; Keyboard Remappings
(when *is-a-mac*
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'meta))
(when *is-win*
  (setq w32-apps-modifier 'control))


;;; Better defaults package
(require 'better-defaults)


;;; Magit setup
(require 'magit)
(when *is-win*
  ;;; Use ssh-agency
  (require 'ssh-agency)
  (setenv "SSH_ASKPASS" "git-gui--askpass"))
(global-set-key (kbd "C-c g") 'magit-status)


;;; restore some GUI elements
(menu-bar-mode 1)
(tool-bar-mode -1)


;;; smex
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;; ido-completing-read+
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(setq magit-completing-read-function 'magit-ido-completing-read)
(setq gnus-completing-rad-function 'gnus-ido-completing-read)
(setq ess-use-ido t)


;;; Org Mode
(require 'org)
(require 'org-info)
(require 'org-show)
(require 'org-contacts)
(add-to-list 'auto-mode-alist '("\\org\\'" . org-mode))
(setq org-directory (expand-file-name "~/org"))
(setq org-default-notes-file (expand-file-name "/organiser.org" org-directory))
(setq org-agenda-files nil)
(add-to-list 'org-agenda-files org-directory)

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d@/!)")
                          (type "SCHEDULED(s)" "|" "CLOSED(c@)")))

(setq org-todo-keyword-faces '(("TODO" . org-todo) ("WAITING" . org-special-keyword) ("SCHEDULED" . org-document-info)))


(setq org-capture-templates
 '(("t" "Todo" entry (file+headline org-default-notes-file "Inbox")
    "* TODO %?")
   ("s" "Meeting" entry (file+headline org-default-notes-file "Inbox")
    "** SCHEDULED %?")
   ("h" "Headline" entry (file org-default-note-file)
    "* %?")
   ("n" "Quick Note" entry (file+headline org-default-note-file "Notes")
    " - %? %U")))

(setq lkk/sorting-strategy '(deadline-up scheduled-up priority-down category-keep))
(setq lkk/sorting-strategy-name '(alpha-up))

(setq org-agenda-custom-commands
      '(("w" "Work" tags-todo "work" ((org-agenda-sorting-strategy '(deadline-up scheduled-up priority-down category-up))))
        ("h" "Home TODOs" tags-todo "home" ((org-agenda-sorting-strategy lkk/sorting-strategy)))
        ("n" "All by name" alltodo "" ((org-agenda-sorting-strategy '(alpha-up))))
        ("u" "Uncategorised TODOs" tags-todo "-work&-home&-personal" nil)))


(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-refile-targets
      '((nil :maxlevel . 2)
        (org-agenda-files :maxlevel . 2)))

; set org mode to format time in hours only
(setq org-duration-format (quote h:mm))

;;; Org Journal
; (require 'org-journal)
(setq org-journal-dir (concat org-directory "/journal"))
(defun lkk/get-formal-time ()
  (replace-regexp-in-string "m$" ".m." (s-downcase (format-time-string "%l:%M %p"))))
(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-journal-date-format "%A, %d %B %Y")
(setq org-journal-time-format "%l:%M %p")

;;; Org Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages '((sqlite . t)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)



;;; prose mode
(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  nil " Prose" nil
  (if prose-mode
      (progn
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        (set (make-local-variable 'blink-cursor-interval) 0.6)
        (set (make-local-variable 'show-trailing-whitespace) nil)
        (set (make-local-variable 'line-spacing) 0.2)
        (set (make-local-variable 'electric-pair-mode) nil)
        (ignore-errors (flyspell-mode 1))
        (visual-line-mode 1))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'blink-cursor-interval)
    (kill-local-variable 'show-trailing-whitespace)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'electric-pair-mode)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (visual-line-mode -1)
    (when (fboundp 'writeroom-mode)
      (writeroom-mode 0))))



;;; Python IDE
(require 'jedi)
(require 'elpy)
(elpy-enable)
(setq elpy-rpc-backend "jedi")


;;; W3M stuff
(unless *is-win*
  (setq lkk/emacs-w3m-directory (expand-file-name "lisp/emacs-w3m/" user-emacs-directory))
  (when (file-exists-p lkk/emacs-w3m-directory)
    (require 'flim)
    (require 'apel)
    (add-to-list 'load-path lkk/emacs-w3m-directory)
    (add-to-list 'load-path (expand-file-name "shimbun" lkk/emacs-w3m-directory))
    (setq w3m-command "/usr/bin/w3m")
    (require 'w3m)))



;; deft
(require 'deft)
(setq deft-recursive t)



;;; Rest client for making REST requests from emacs
(require 'restclient)


;;; Markdown mode
(require 'markdown-mode)



;;; Pomodoro
(require 'org-pomodoro)
(global-set-key (kbd "C-c p") 'org-pomodoro)
(setq org-pomodoro-start-sound-p nil)
(setq org-pomodoro-ticking-frequency 300)
(setq org-pomodoro-ticking-sound-p t)
(setq org-pomodoro-ticking-sound (concat org-directory "/sounds/ticktock.wav"))
(setq org-pomodoro-ticking-sound-states '(:pomodoro))
(setq org-pomodoro-finished-sound (concat org-directory "/sounds/bell.wav"))
(setq org-pomodoro-long-break-sound (concat org-directory "/sounds/foghorn.wav"))
(setq org-pomodoro-short-break-sound (concat org-directory "/sounds/bell_multiple.wav"))



;;; God mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(define-key god-local-mode-map (kbd ".") 'repeat)
(defun lkk/god-mode-background
  (if (or god-local-mode buffer-read-only)
      (set-background-color "coral4")
    (set-background-color"#00393B")))

(add-hook 'god-mode-enabled-hook 'lkk/god-mode-background)
(add-hook 'god-mode-disabled-hook 'lkk/god-mode-background)


;;; Zenburn theme
(add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))
(setq zenburn-use-variable-pitch t)
(setq zenburn-scale-org-headlines t)
(setq zenburn-scale-outline-headlines t)
(load-theme 'zenburn t)
(load-theme 'birbo)


;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))



