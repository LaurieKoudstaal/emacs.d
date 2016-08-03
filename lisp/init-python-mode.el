(add-hook 'python-mode-hook
	  (lambda ()
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(provide 'init-python-mode)
