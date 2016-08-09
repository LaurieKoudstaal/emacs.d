
;; FIX SQL MODE TO USE sqsh
(set 'sql-ms-program "sqsh")
   (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (setq sql-prompt-regexp "^[_[:alpha:]]*[=][#>] ")
              (setq sql-prompt-cont-regexp "^[_[:alpha:]]*[-][#>] ")))
(provide 'init-mssql)
