;; multind.el - AUCTeX support for multiple indices with multind.sty.
;; Author: C. Dominik <dominik@strw.leidenuniv.nl>
;; Last change: 25 Jan 1999

(TeX-add-style-hook "multind"
  (lambda ()

    ;; Commands
    (TeX-add-symbols
     '("makeindex" "Indextag")
     '("index" TeX-arg-index-tag TeX-arg-index)
     '("printindex" TeX-arg-index-tag "Title")
     "printindex" "indexspace")

    ;; Parsing index macros
    (setq LaTeX-auto-regexp-list
	  (append
	   ;; The first regexp is faster, but less accurate
	   ;; '(("\\\\index\\*?{[^{}]*}{\\([^}]*\\)" 1 LaTeX-auto-index-entry))
	   ;; The second regexp is very good, but slower
	   '(("\\\\index\\*?{[^{}]*}{\\([^}{]*\\({[^}{]*\\({[^}{]*\\({[^}{]*}[^}{]*\\)*}[^}{]*\\)*}[^}{]*\\)*\\)}"
	      1 LaTeX-auto-index-entry))
	   LaTeX-auto-regexp-list))

    ;; Completion for index entries in the |see and \index commands
    (setq TeX-complete-list 
	  (append
	   '(("\\\\index{[^{}]*}{\\([^{}\n\r]*\\)" 1 LaTeX-index-entry-list)
	     ("|see{\\([^}]*\\)" 1 LaTeX-index-entry-list))
	   TeX-complete-list))

    ;; RefTeX support
    (and (fboundp 'reftex-add-index-macros)
	 (reftex-add-index-macros '(multind)))))

(defvar LaTeX-multind-package-options nil
  "Package options for the multind package.")

;; multind.el ends here
