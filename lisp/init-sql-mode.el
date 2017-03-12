;; Make sure the when connecting to mysql, we use the local mysql
(defadvice sql-mysql (around sql-mysql-around activate)
  "Reset to local home, then connect"
  (let ((default-directory "~"))
    ad-do-it))
(provide 'init-sql-mode)
