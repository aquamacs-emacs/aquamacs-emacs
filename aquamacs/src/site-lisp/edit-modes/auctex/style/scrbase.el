;;; scrbase.el --- AUCTeX style for the KOMA-Script bundle.

;; Copyright (C) 2002 Mark Trettin
;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

;; Author: Mark Trettin <Mark.Trettin@gmx.de>
;; Created: 2002-09-26
;; Version: $Id: scrbase.el,v 1.1 2005/10/18 08:34:29 davidswelt Exp $
;; Keywords: tex
;; License: GPL, see the file COPYING in the base directory of AUCTeX

;;; Commentary:

;; This file adds support for the KOMA-Script bundle.  This file
;; contains the base definitions that work with all KOMA-Script
;; classes (scrarctl.cls, scrreprt.cls, scrbook.cls and scrlttr2.cls).
;; You need this file since it's loaded from the class-styles.

;;; Code:
(TeX-add-style-hook "scrbase"
  (lambda ()
    (TeX-add-symbols
     "appendixmore"
     "autodot"
     '("addtokomafont" TeX-arg-KOMA-fontelements t)
     '("areaset" [ "BCOR" ] "Width" "Height")
     '("captionabove" [ "Lof entry" ] "Caption")
     '("captionbelow" [ "Lof entry" ] "Caption")
     '("cleardoubleemptypage")
     '("cleardoubleplainpage")
     '("cleardoublestandardpage")
     '("dedication" t)
     '("deffootnote" [ "Mark width" ] "Indent" "Parindent" "Definition")
     '("deffootnotemark" "Definition")
     '("extratitle" t)
     '("ifpdfoutput" t nil)
     '("ifthispageodd" t nil)
     '("lowertitleback" t)
     '("maketitle" [ "Pagenumber" ])
     '("marginline" t)
     '("publishers" "Publishers")
     '("sectionmark" "Running head")
     '("setbibpreamble" "Preamble")
     '("setcaphanging")
     '("setcapindent" "Indent")
     '("setcapindent*" "X-Indent")
     '("setcapmargin" [ "Margin left" ] "Margin")
     '("setcapmargin*" [ "Margin inside" ] "Margin")
     '("setcapwidth" [ TeX-arg-KOMA-capjust ] "Width")
     '("setindexpreamble" "Preamble")
     '("setkomafont" TeX-arg-KOMA-fontelements t)
     '("subject" "Subject")
     '("subsectionmark" "Running head")
     '("textsubscript" "Text")
     '("thanks" "Footnote")
     '("thefootnotemark")
     '("titlehead" t)
     '("uppertitleback" t)
     '("usekomafont" TeX-arg-KOMA-fontelements))
    (LaTeX-add-environments
     '("labeling" (lambda (env &rest ignore)
		    (LaTeX-insert-environment
		     env
		     (let ((delim (read-string "(Optional) Delimiter: "))
			   (width (read-string "Longest item: ")))
		       (concat
			(if (not (zerop (length delim)))
			    (format "[%s]" delim))
			(format "{%s}" width))))
		    (LaTeX-find-matching-begin)
		    (end-of-line 1)
		    (LaTeX-insert-item)))
     '("addmargin" (lambda (env &rest ignore)
		     (LaTeX-insert-environment
		      env
		      (let ((leftin (read-string "(Optional) Left Indentation: "))
			    (indent (read-string "Indentation: ")))
			(concat
			 (if (not (zerop (length leftin)))
			     (format "[%s]" leftin))
			 (format "{%s}" indent))))))
     '("addmargin*" (lambda (env &rest ignore)
		      (LaTeX-insert-environment
		       env
		       (let ((innin (read-string "(Optional) Innner Indentation: "))
			     (indent (read-string "Indentation: ")))
			 (concat
			  (if (not (zerop (length innin)))
			      (format "[%s]" innin))
			  (format "{%s}" indent))))))
     '("captionbeside" (lambda (env &rest ignore)
			 (LaTeX-insert-environment
			  env
			  (let ((lofent (read-string "(Optional) Lof Entry: "))
				(title (read-string "Caption: "))
				(place (read-string "(Optional) Placement (l,r,o,i): "))
				(width (read-string "(Optional) Width: "))
				(offset (read-string "(Optional) Offset: ")))
			    (concat
			     (if (not (zerop (length lofent)))
				 (format "[%s]" lofent))
			     (format "{%s}" title)
			     (if (not (zerop (length place)))
				 (format "[%s]" place))
			     (if (not (zerop (length width)))
				 (format "[%s]" width))
			     (and
			      (not (zerop (length place)))
			      (not (zerop (length offset)))
			      (format "[%s]%s" offset
				      (if (y-or-n-p "Starred? ")
					  "*" "")))))))))
    (LaTeX-section-list-add-locally '(("addpart" 0)
				      ("addsec" 2)
				      ("minisec" 7)))
    ;; This doesn't work. Maybe it's refTeX's label insertion?
    (make-local-variable 'LaTeX-section-label)
    (setq LaTeX-section-label (append
			       LaTeX-section-label
			       '(("addpart" . nil)
				 ("addsec" . nil)
				 ("minisec" . nil))))
    ;; Definitions for font-latex
    (when (and (featurep 'font-latex)
	       (eq TeX-install-font-lock 'font-latex-setup))
      ;; Textual keywords
      (setq font-latex-match-textual-keywords-local
	    (append font-latex-match-textual-keywords-local
		    '("captionabove"
		      "captionbelow"
		      "dedication"
		      "extratitle"
		      "lowertitleback"
		      "maketitle"
		      "marginline"
		      "publishers"
		      "subject"
		      "sectionmark"
		      "setbibpreamble"
		      "setindexpreamble"
		      "subsectionmark"
		      "textsubscript"
		      "titlehead"
		      "uppertitleback")))
      (font-latex-match-textual-make)
      ;; Function keywords
      (setq font-latex-match-function-keywords-local
	    (append font-latex-match-function-keywords-local
		    '("deffootnote"
		      "deffootnotemark"
		      "ifpdfoutput"
		      "ifthispageodd")))
      (font-latex-match-function-make)
      ;; Variable keywords
      (setq font-latex-match-variable-keywords-local
	    (append font-latex-match-variable-keywords-local
		    '("addtokomafont"
		      "areaset"
		      "setcaphanging"
		      "setcapindent"
		      "setcapmargin"
		      "setcapwidth"
		      "setkomafont"
		      "typearea"
		      "usekomafont")))
      (font-latex-match-variable-make)
      ;; Warning keywords
      (setq font-latex-match-warning-keywords-local
	    (append font-latex-match-warning-keywords-local
		    '("cleardoublestandardpage"
		      "cleardoubleplainpage"
		      "cleardoubleemptypage")))
      (font-latex-match-warning-make)
      ;; Sectioning keywords
      (add-to-list 'font-latex-match-sectioning-1-keywords-local "addpart")
      (font-latex-match-sectioning-1-make)
      (add-to-list 'font-latex-match-sectioning-2-keywords-local "addsec")
      (font-latex-match-sectioning-2-make)
      (add-to-list 'font-latex-match-sectioning-4-keywords-local "minisec")
      (font-latex-match-sectioning-4-make))))

(defun TeX-arg-KOMA-setpreamble (optional &optional prompt)
  "Prompt for KOMA-Script's \\set*preamble position with completion."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt optional prompt "Position")
    '(("") ("l") ("r") ("c") ("o") ("u")
      ("lo") ("lu") ("ro") ("ru") ("co") ("cu"))
    nil t)
   optional))

(defun TeX-arg-KOMA-capjust (optional &optional prompt)
  "Prompt for KOMA-Script's \\setcapwidth justification with completion."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt optional prompt "Justification")
    '(("") ("l") ("r") ("c") ("i") ("o"))
    nil t)
   optional))

(defun TeX-arg-KOMA-fontelements (optional &optional prompt)
  "Prompt for KOMA-Script's fontelements with completion."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt optional prompt "Element")
    '(("")
      ("caption") ("captionlabel")
      ("descriptionlabel") ("dictumauthor") ("dictumtext")
      ("footnote") ("footnotelabel") ("footnotereference")
      ("pagefoot") ("pagehead") ("pagenumber")
      ("sectioning") ("part") ("partnumber") ("chapter") ("section")
      ("subsection") ("subsubsection") ("paragraph") ("subparagraph")
      ("title"))
    nil t)
   optional))
 
(add-to-list 'LaTeX-item-list '("labeling" . LaTeX-item-argument))

;;; scrbase.el ends here
