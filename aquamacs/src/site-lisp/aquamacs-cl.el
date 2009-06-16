;; we try not to require `cl' (at runtime) so as to not
;; import stuff into the general Emacs namespace

;; import only macros from `cl'
(eval-when-compile (require 'cl))

;; define functions with "aq-" prefix

(defun aq-copy-list (list)
  "Return a copy of LIST, which may be a dotted list.
The elements of LIST are not copied, just the list structure itself."
  (if (consp list)
      (let ((res nil))
	(while (consp list) (push (pop list) res))
	(prog1 (nreverse res) (setcdr res list)))
    (car list)))


(provide 'aquamacs-cl)
