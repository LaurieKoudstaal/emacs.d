(require-package 'whitespace)
(require 'whitespace)
 (setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
(provide 'init-whitespace)