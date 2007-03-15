;;; amsthm.el --- Style hook for the AMS-LaTeX amsthm package.
;;;
;;; AUTHOR: Carsten Dominik <dominik@strw.leidenuniv.nl>

;;; Code:

(TeX-add-style-hook "amsthm"
 (function
  (lambda ()
    (LaTeX-add-environments
     '("proof" (lambda (env &rest ignore)
		 (LaTeX-insert-environment 
		  env
		  (let ((heading (read-string "(optional) Heading: ")))
		    (if (string= heading "")
			""
		      (format "[%s]" heading))))))
     )
    (TeX-add-symbols
     '("newtheorem" "Environment name" ["Share numbering with"] "Heading"
       ["Number subordinated in each"])
     '("newtheorem*" "Environment name" "Heading")
     '("theoremstyle" LaTeX-amsthm-complete-theoremstyle)
     ))))

(defun LaTeX-amsthm-complete-theoremstyle (&rest ignore)
  (insert TeX-grop
	  (completing-read  "Style: " '(("plain" . nil)
					("definition" . nil)
					("remark" . nil)))
	  TeX-grcl))

;;; amsthm.el ends here.
