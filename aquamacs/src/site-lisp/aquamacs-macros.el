;; Profiling code
(defmacro ats (txt) nil)
;; (defvar aq-starttime 0)
;; (defun ats (txt)
;;   (message "ATS %s:  %s" (time-since aq-starttime) txt))
;; (setq aq-starttime (current-time))
(ats "started")

(defmacro protect (&rest body)
  "Execute body, catching errors.
If errors are signaled, they are logged using `message'."
  `(condition-case err
       (progn ,@body)
     (error (message "%s: %s" (car err) (cdr err)) nil)))


(provide 'aquamacs-macros)
