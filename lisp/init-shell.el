

;; RESTART SHELL PROCESS
(defun restart-shell () (interactive) (shell (current-buffer)))

;; RUN SHELL ON RDS
(defun rds-shell (arg)
  (interactive "P")
  (let ((default-directory "/ssh:veeva@173.255.143.2:"))
    (setq current-prefix-arg arg)
    (call-interactively 'shell)))

(provide 'init-shell)
