

(defmacro protect (&rest body)
  "Execute body, catching errors.
If errors are signaled, they are logged using `message'."
  `(condition-case err
       (progn ,@body)
     (error (message "%s: %s" (car err) (cdr err)) nil)))


(provide 'aquamacs-macros)