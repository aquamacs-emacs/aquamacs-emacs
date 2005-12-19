;; makeidx.el - AUCTeX support for makeidx.sty
;; Author: C. Dominik <dominik@strw.leidenuniv.nl>
;; Last change: 25 Jan 1999

(TeX-add-style-hook "makeidx"
  (lambda ()
    (TeX-add-symbols 
     "printindex" "indexspace")

    ;; Parsing the default index macro is defined in latex.el
    ;; The same is true form completion in the index macro

    ;; Completion for the |see macro
    (setq TeX-complete-list
	  (append
	   '(("|see{\\([^{}\n\r]*\\)" 1 LaTeX-index-entry-list))
	   TeX-complete-list))

    ;; RefTeX support
    (and (fboundp 'reftex-add-index-macros)
	 (reftex-add-index-macros '(default)))))

(defvar LaTeX-makeidx-package-options nil
  "Package options for the makeidx package.")

;; makeidx.el ends here