(require-package 'exec-path-from-shell)

;; EXEC PATH FROM SHELL
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  ;; COPY PYTHON PATH FROM SHELL
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(provide 'init-exec-path)
