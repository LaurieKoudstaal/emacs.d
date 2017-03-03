(require-package 'org-plus-contrib)
(when (< emacs-major-version 24)
  (require-package 'org))
(require-package 'org-fstree)
(when *is-a-mac*
  (require-package 'org-mac-iCal))
(require-package 's)
(require 's)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'org-velocity)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-archive-mark-done nil
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80
      )

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
        (flyspell-mode 1)
        (when (fboundp 'visual-line-mode)
          (visual-line-mode 1)))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'show-trailing-whitespace)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (when (fboundp 'visual-line-mode)
      (visual-line-mode -1))))

(setq org-support-shift-select t)

;; CAPTURING
(global-set-key "\C-cc" 'org-capture)

(defun my/task-template ()
  (concat "** NEXT %?\n%U\n"))

(defun my/project-template ()
  (concat "** PROJECT %?\n%U\n"))

(defun my/iterate-file-number (filename num)
  (if (file-exists-p filename)
      (iterate-file-number filename (+ num 1))
    (concat filename (number-to-string num))))

;; TODO: Iterate over project with same category
(defun my/convert-project-name-to-category (project-name)
  (let (initials)
    (dolist (word (s-split-words (s-titleized-words project-name)) initials)
      (setq initials (concat initials (s-left 2 word))))))

(defun my/convert-project-name-to-filename ()
  (expand-file-name
   (format "%s.org" 
	   (my/iterate-file-number
	    (my/convert-project-name-to-category my/project-name) 1))
	   "~/egnyte/projects"))

(setq my/project-name "default")

(setq org-capture-templates
      '(("t" "todo" entry (file+headline "projects.org" "No Project")  ; "" => org-default-notes-file
         (function my/task-template) :clock-resume t)
	("p" "project" entry (file "projects.org")  ; "" => org-default-notes-file
         (function my/project-template) :clock-resume t)
        ("n" "note" entry (file+headline "projects.org" "No Project")
         "** %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ))

;; REFILING
(setq org-refile-use-cache nil)

;; Set the agenda files
(setq org-agenda-files '("~/org/projects.org" "~/egnyte/projects"))
(setq org-directory "~/org")


; Targets include this file and any file contributing to the agenda - up to 2 levels deep
(setq org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 2)))


;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which suppresses `org-refile-target-verify-function'."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; TO-DO SETTINGS
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "BACKLOG(b)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face)
	      ("BACKLOG" :inherit shadow))))

;; AGENDA VIEWS
(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


(let ((active-project-match "-INBOX/PROJECT"))
  
  (setq org-stuck-projects
        `(,active-project-match ("NEXT")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        `(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("g" "GTD"
           ((agenda "" nil)
            (tags "INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-INBOX/NEXT"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        ;; TODO: skip if a parent is WAITING or HOLD
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        ;; TODO: skip if a parent is a project
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED" "BACKLOG"))
                                (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/DELEGATED"
                       ((org-agenda-overriding-header "Delegated")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
	    (tags-todo "-INBOX/BACKLOG"
                       ((org-agenda-overriding-header "Backlog")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/HOLD"
                       ((org-agenda-overriding-header "On Hold")
                        ;; TODO: skip if a parent is WAITING or HOLD
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            ;; (tags-todo "-NEXT"
            ;;            ((org-agenda-overriding-header "All other TODOs")
            ;;             (org-match-list-sublevels t)))
            )))))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)


;; ORG CLOCK
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))


;;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
  (add-hook 'org-clock-in-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
  (add-hook 'org-clock-out-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                "tell application \"org-clock-statusbar\" to clock out"))))

;;; ARCHIVING
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")


;;; TSV in ORGMODE
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


(defun lkk/org-id-get-create ()
  (interactive)
  (org-set-property "CATEGORY" (lkk/generate-human-readable-id)))

;; ORG MODE: ADD CUSTOMER
(defun lkk/org-add-customer (arg)
  (interactive "MCustomer:")
  (org-set-property "CUSTOMER" arg))

;;(eval-after-load 'org-mode
;;		 '(define-key org-mode-map (kbd "C-c a") 'lkk/org-add-customer))

;; ORG MODE BINDING FOR capture new item
;;(eval-after-load 'org-mode
;;  '(define-key org-mode-map (kbd "C-c i") 'lkk/org-id-get-create))

;; ADD ORG LANGUAGES
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (sh . t)
   (sql . t)
   (sqlite . t)
   (awk . t)))

(setq org-hid-leading-stars t)
(require-package 'org-bullets)

;; USE fixed-pitch FOR TABLES
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)


;; define "R" as the prefix key for reviewing what happened in various
;; time periods
(add-to-list 'org-agenda-custom-commands
             '("R" . "Review" )
             )

;; Common settings for all reviews
(setq efs/org-agenda-review-settings
      '((org-agenda-files '("~/egnyte/org/projects.org" "~/egnyte/projects"))
        (org-agenda-show-all-dates t)
        (org-agenda-start-with-log-mode t)
        (org-agenda-start-with-clockreport-mode t)
        (org-agenda-archives-mode t)
        ;; I don't care if an entry was archived
        (org-agenda-hide-tags-regexp
         (concat org-agenda-hide-tags-regexp
                 "\\|ARCHIVE"))
      ))
;; Show the agenda with the log turn on, the clock table show and
;; archived entries shown.  These commands are all the same exept for
;; the time period.
(add-to-list 'org-agenda-custom-commands
             `("Rw" "Week in review"
                agenda ""
                ;; agenda settings
                ,(append
                  efs/org-agenda-review-settings
                  '((org-agenda-span 'week)
                    (org-agenda-start-on-weekday 0)
                    (org-agenda-overriding-header "Week in Review"))
                  )
                ("~/org/review/week.html")
                ))


(add-to-list 'org-agenda-custom-commands
             `("Rd" "Day in review"
                agenda ""
                ;; agenda settings
                ,(append
                  efs/org-agenda-review-settings
                  '((org-agenda-span 'day)
                    (org-agenda-overriding-header "Week in Review"))
                  )
                ("~/org/review/day.html")
                ))

(add-to-list 'org-agenda-custom-commands
             `("Rm" "Month in review"
                agenda ""
                ;; agenda settings
                ,(append
                  efs/org-agenda-review-settings
                  '((org-agenda-span 'month)
                    (org-agenda-start-day "01")
                    (org-read-date-prefer-future nil)
                    (org-agenda-overriding-header "Month in Review"))
                  )
                ("~/org/review/month.html")
                ))


(provide 'init-org)
