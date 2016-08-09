
;; FIX A BUG IN ESHELL SO I CAN RUN SOURCES
(defadvice eshell-gather-process-output (before absolute-cmd (command args) act)
  (setq command (file-truename command)))
(provide 'init-eshell)
