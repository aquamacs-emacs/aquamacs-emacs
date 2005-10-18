;; subfigure.el --- AUCTeX style file for subfigure.sty
;; Copyright (C) 2003 Reiner Steib

;; Author: Reiner Steib  <Reiner.Steib@gmx.de>
;; Keywords: tex

;;; Commentary:
;;
;; AUCTeX style file for subfigure.sty

;;; Code:

(TeX-add-style-hook
 "subfigure"
 (lambda ()
   (TeX-add-symbols
    '("subfigure"  [ "list entry" ] [ "sub caption" ] "figure")
    '("subtable"   [ "list entry" ] [ "sub caption" ] "figure")
    '("Subref" TeX-arg-label)
    '("subref" TeX-arg-label))
   ;; TODO: add \subfig* lengths

   ;; Install completion for labels:
   (setq TeX-complete-list
	 (append
	  '(("\\\\subref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
	    ("\\\\Subref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}"))
	  TeX-complete-list))
   ;; font-lock:
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (add-to-list 'font-latex-match-textual-keywords-local "subfigure")
     (add-to-list 'font-latex-match-textual-keywords-local "subtable")
     (font-latex-match-textual-make)
     (add-to-list 'font-latex-match-reference-keywords-local "Subref")
     (add-to-list 'font-latex-match-reference-keywords-local "subref")
     (font-latex-match-reference-make))))

;; subfigure.el ends here
