

;; MAKE SSH THE DEFAULT TRAMP MODE
(require-package 'tramp)
(setq tramp-default-method "ssh")
(provide 'init-tramp)
