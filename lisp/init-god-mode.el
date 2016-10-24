;; GOD MODE
(require-package 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(eval-after-load 'god-mode (god-mode))

(add-to-list 'god-exempt-major-modes 'org-agenda-mode)

(provide 'init-god-mode)

