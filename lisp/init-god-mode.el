;; GOD MODE
(require-package 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(provide 'init-god-mode)
