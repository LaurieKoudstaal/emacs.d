(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "WAITING" "|" "DONE" "CANCELLED")))

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

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/default.org"))

(org-babel-do-load-languages
 'org-babel-load-languages (quote ((emacs-lisp . t)
                                   (sqlite . t)
                                   (sh . t)
                                   (R . t)
                                   (python . t))))
;; ORG MODE BINDING FOR capture new item
(eval-after-load 'org-mode
  '(define-key org-mode-map (kbd "C-c i") 'org-id-get-create))

;; SET ORG MODE TEMPLATES
(setq org-capture-templates
      '(("t" "todo" entry (file+headline (concat org-directory "/default.org") "Unfiled")
         "* TODO %? \n%i \n%a")
        ("n" "note" entry (file (concat org-directory "/default.org"))
         "* %? \n%U\n%a\n")
        ("j" "journal" entry (file+datetree (concat org-directory "/journal.org"))
         "* %?\n%U\n")
        ("s" "study" entry (file (concat org-directory "/studynotes.org"))
         "* %?")
        ))

(provide 'init-org)

