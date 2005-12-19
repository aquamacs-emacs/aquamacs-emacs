;; subfigure.el --- AUCTeX style file for subfigure.sty
;; Copyright (C) 2003, 2005 Reiner Steib

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

(defvar LaTeX-subfigure-package-options '("normal" "hang" "center"
					  "centerlast" "nooneline"
					  "raggedright" "isu" "anne"
					  "scriptsize" "footnotesize"
					  "small" "normalsize" "large"
					  "Large" "rm" "sf" "tt" "md"
					  "bf" "up" "it" "sl" "sc" "RM"
					  "SF" "TT" "MD" "BF" "IT" "SL"
					  "SC" "UP" "figbotcap"
					  "figtopcap" "tabbotcap"
					  "tabtopcap" "FIGBOTCAP"
					  "FIGTOPCAP" "TABBOTCAP"
					  "TABTOPCAP" "loose" "tight")
  "Package options for the subfigure package.")

;; subfigure.el ends here
