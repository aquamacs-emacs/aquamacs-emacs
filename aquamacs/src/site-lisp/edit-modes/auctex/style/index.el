;; index.el - AUCTeX support for indices with index.sty.
;; Author: C. Dominik <dominik@strw.leidenuniv.nl>
;; Last change: 25 Jan 1999

(TeX-add-style-hook "index"
  (lambda ()

    (TeX-add-symbols

     ;; New indices
     '("newindex" [ "Counter" ] "Tag"
       "Extension of raw index" "Extension of processed index" "Index title")
     '("renewindex" [ "Counter" ] "Tag" 
       "Extension of raw index" "Extension of processed index" "Index title")
     "makeindex"
     '("disableindex" "Tag[,tag...]")

     ;; Printing the index
     '("printindex" [ "Indextag" ] [ "Prologue" ])
     "indexspace"

     ;; Index entries
     '("index" [ TeX-arg-index-tag ] TeX-arg-index)
     '("index*" [ TeX-arg-index-tag ] TeX-arg-index)

     ;; Showidx-like stuff
     "proofmodetrue" "proofmodefalse" '("indexproofstyle" "Style")

     ;; Shortcuts (THESE ARE DEPRECATED AND SHOULD NOT BE USED
     "shortindexingon" "shortindexinoff")

    ;; Parsing index macros
    (setq LaTeX-auto-regexp-list
	  (append

	   ;; The first regexp is faster, but less accurate
	   ;;'(("\\\\index\\*?\\[[^{}]*\\]{\\([^}]*\\)"
	   ;;   1 LaTeX-auto-index-entry))

	   ;; The second regexp is very good, but slower.
	   '(("\\\\index\\*?\\[[^{}]*\\]{\\([^}{]*\\({[^}{]*\\({[^}{]*\\({[^}{]*}[^}{]*\\)*}[^}{]*\\)*}[^}{]*\\)*\\)}"
	      1 LaTeX-auto-index-entry))

	   LaTeX-auto-regexp-list))

    ;; Completion for the index entries in \index and |see commands
    (setq TeX-complete-list
	  (append
	   '(("\\\\index\\*?\\(\\[[^][{}]*\\]\\)?{\\([^{}\n\r]*\\)" 
	      2 LaTeX-index-entry-list)
	     ("|see{\\([^}]*\\)" 1 LaTeX-index-entry-list))
	   TeX-complete-list))

    ;; RefTeX support
    (and (fboundp 'reftex-add-index-macros)
	 (reftex-add-index-macros '(index)))))

(defvar LaTeX-index-package-options nil
  "Package options for the index package.")

;; index.el ends here
