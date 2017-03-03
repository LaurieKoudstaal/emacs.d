;;; ID FOR NEW ITEMS 2
;; MAKE AN ID THAT LOOKS A BIT HUMAN READABLE
(defun lkk/generate-human-readable-id ()
  (defconst con-list "BCDFGHJKLMNPQRSTVWXZ")
  (defconst vow-list "AEIOUY")
  (defconst num-list "0123456789")
  (defvar ret-id "")
  (setq ret-id (concat (string (elt con-list (random 20)))
		       (string (elt vow-list (random 6)))
		       (string (elt con-list (random 20)))
		       (string (elt num-list (random 10)))))
  ret-id)

;;; ID FOR NEW ITEMS
;; MAKE 4-BASE36 ID
(defun lkk/base-36 (n)
  (defconst char-list "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
  (defvar encoding "")
  (setq encoding "")
  (while (> n 0)
    (setq encoding (concat encoding (string (elt char-list (random 36)))))
    (setq n (- n 1)))
  encoding)

