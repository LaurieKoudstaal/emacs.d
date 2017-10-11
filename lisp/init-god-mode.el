;; GOD MODE
(require-package 'god-mode)
(global-set-key (kbd "M-g") 'god-mode-all)

(defun my-update-cursor ()
  (cond ((or (bound-and-true-p god-mode)
	     (bound-and-true-p god-global-mode)
	     (bound-and-true-p god-local-mode))
	 (set-cursor-color "lime green"))
	(t (set-cursor-color "dark orange"))))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(eval-after-load 'god-mode (god-mode))

(add-to-list 'god-exempt-major-modes 'org-agenda-mode)

(provide 'init-god-mode)

