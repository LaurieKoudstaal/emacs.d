

;; RESTART SHELL PROCESS
(defun restart-shell () (interactive) (shell (current-buffer)))

;; RUN SHELL ON RDS
(defun rds-shell (arg)
  (interactive "P")
  (let ((default-directory "/ssh:veeva@access.veeva.com.au:~/"))
    (setq current-prefix-arg arg)
    (call-interactively 'shell)))

(provide 'init-shell)
