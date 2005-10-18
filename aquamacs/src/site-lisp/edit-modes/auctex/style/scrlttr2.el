;;; -*- emacs-lisp -*-
;;; scrlttr2.el -- AUC TeX style for scrlttr2.cls

;; Copyright (C) 2002 Free Software Foundation
;; License: GPL, see the file COPYING in the base directory of AUC TeX

;; Author: Mark Trettin <Mark.Trettin@gmx.de>
;; Created: 2002-10-26
;; Version: $Id: scrlttr2.el,v 1.1 2005/10/18 08:34:29 davidswelt Exp $
;; Keywords: tex

;;; Commentary: 

;; This file adds support for `scrlttr2.cls'.

;; Since I just recently switched from `g-brief.cls' to the
;; KOMA-Script letter class *and* I don't really write many
;; snailmails, there are probably some superflous macros included and
;; important ones left out. Comments appreciated.

;; I left out any length and positioning macros since those should be
;; set in a personal `*.lco'-File. IMHO.

;; This file is part of AUCTeX.

;;; Code
(TeX-add-style-hook "scrlttr2"
  (lambda ()
    (TeX-add-symbols
     '("AtBeginLetter" t)
     '("KOMAoptions" t)
     '("LetterOptionNeedsPapersize" "Name of lco file" "Paper size")
     '("LoadLetterOption" "Name of lco file")
     '("addrchar" "Initial letter")
     '("addrentry" "Lastname" "Firstname" "Address" "Telephone" "F1"
       "F2" "F3" "F4" "Key")
     '("addtokomafont" TeX-arg-KOMA-scrlttr-fontelements t)
     '("addtolengthplength" [ "Factor" ] "Name of length"
       "Name of pseudo length")
     '("addtoreffields" TeX-arg-KOMA-scrlttr-vars)
     '("adrchar" "Initial letter")
     '("adrentry" "Lastname" "Firstname" "Address" "Telephone" "F1"
       "F2" "Comment" "Key")
     '("bankname" t)
     '("captionsUKenglish" nil)
     '("captionsUSenglish" nil)
     '("captionsamerican" nil)
     '("captionsaustrian" nil)
     '("captionsbritish" nil)
     '("captionscroatian" nil)
     '("captionsdutch" nil)
     '("captionsenglish" nil)
     '("captionsfrench" nil)
     '("captionsgerman" nil)
     '("captionsitalian" nil)
     '("captionsngerman" nil)
     '("captionsspanish" nil)
     '("cc" t)
     '("ccname" t)
     '("cleardoubleemptypage")
     '("cleardoubleplainpage")
     '("cleardoublestandardpage")
     '("closing" "Closing Phrase")
     '("customername" t)
     '("dateUKenglish" nil)
     '("dateUSenglish" nil)
     '("dateamerican" nil)
     '("dateaustrian" nil)
     '("datebritish" nil)
     '("datecroatian" nil)
     '("datedutch" nil)
     '("dateenglish" nil)
     '("datefrench" nil)
     '("dategerman" nil)
     '("dateitalian" nil)
     '("datename" t)
     '("datengerman" nil)
     '("datespanish" nil)
     '("emailname" t)
     '("encl" t)
     '("enclname" t)
     '("faxname" t)
     '("firstfoot" t)
     '("firsthead" t)
     '("headfromname" t)
     '("headtoname" t)
     '("ifkomavarempty" TeX-arg-KOMA-scrlttr-vars 2)
     '("ifkomavarempty*" TeX-arg-KOMA-scrlttr-vars 2)
     '("invoicename" t)
     '("myrefname" t)
     '("newcaptionname" "Language" "Term" "Definition")
     '("newkomavar" [ "Description" ] "Name")
     '("newkomavar*" [ "Description" ] "Name")
     '("nextfoot" t)
     '("nexthead" t)
     '("opening" "Opening")
     '("pagename" t)
     '("phonename" t)
     '("providecaptionname" "Language" "Term" "Definition")
     '("ps")
     '("raggedsignature" nil)
     '("renewcaptionname" "Language" "Term" "Definition")
     '("setkomafont" TeX-arg-KOMA-scrlttr-fontelements t)
     '("setkomavar" TeX-arg-KOMA-scrlttr-vars [ "Description" ] t)
     '("setkomavar*" TeX-arg-KOMA-scrlttr-vars "Description")
     '("setlengthtoplength" [ "Factor" ] "Name of length"
       "Name of pseudo length")
     '("subjectname" t)
     '("usekomafont" TeX-arg-KOMA-scrlttr-fontelements)
     '("usekomavar" [ "Command" ] TeX-arg-KOMA-scrlttr-vars)
     '("usekomavar*" [ "Command" ] TeX-arg-KOMA-scrlttr-vars)
     '("useplength" "Name")
     '("wwwname" t)
     '("yourmailname" t)
     '("yourrefname" t))
    (LaTeX-add-environments
     '("letter" (lambda (env &rest ignore)
		  (LaTeX-insert-environment
		   env
		   (let ((options (read-string "Optional options: "))
			 (recip (read-string "Recipient: ")))
		     (concat
		      (if (not (zerop (length options)))
			  (format "[%s]" options))
		      (format "{%s}" recip)))))))
    ;; Definitions for font-latex
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     ;; Textual keywords
     (setq font-latex-match-textual-keywords-local
	   (append font-latex-match-textual-keywords-local
		   '("addrentry"
		     "adrentry"
		     "bankname"
		     "cc"
		     "ccname"
		     "closing"
		     "customername"
		     "datename"
		     "emailname"
		     "encl"
		     "enclname"
		     "faxname"
		     "firstfoot"
		     "firsthead"
		     "headfromname"
		     "headtoname"
		     "invoicename"
		     "myrefname"
		     "nextfoot"
		     "nexthead"
		     "opening"
		     "pagename"
		     "phonename"
		     "ps"
		     "subjectname"
		     "wwwname"
		     "yourmailname"
		     "yourrefname")))
     (font-latex-match-textual-make)
     ;; Function keywords
     (setq font-latex-match-function-keywords-local
	   (append font-latex-match-function-keywords-local
		   '("AtBeginLetter"
		     "LetterOptionNeedsPapersize"
		     "LoadLetterOption"
		     "addrchar"
		     "adrchar"
		     "ifkomavarempty")))
     (font-latex-match-function-make)
     ;; Variable keywords
     (setq font-latex-match-variable-keywords-local
	   (append font-latex-match-variable-keywords-local
		   '("KOMAoptions"
		     "addtokomafont"
		     "addtolengthplength"
		     "addtoreffields"
		     "newcaptionname"
		     "newkomavar"
		     "providecaptionname"
		     "renewcaptionname"
		     "setkomafont"
		     "setkomavar"
		     "setlengthtoplength"
		     "usekomafont"
		     "usekomavar"
		     "useplength")))
     (font-latex-match-variable-make)
     ;; Warning keywords
     (setq font-latex-match-warning-keywords-local
	   (append font-latex-match-warning-keywords-local
		   '("cleardoublestandardpage"
		     "cleardoubleplainpage"
		     "cleardoubleemptypage")))
     (font-latex-match-warning-make))))

(defun TeX-arg-KOMA-scrlttr-vars (optional &optional prompt)
  "Prompt for KOMA-Script's scrlttr2 predefined variables with completion."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt optional prompt "Variable")
    '(("")
      ("backaddress") ("backaddressseparator")
      ("ccseparator") ("customer")
      ("date")
      ("emailseparator") ("enclseparator")
      ("faxseparator") ("frombank") ("fromaddress") ("fromemail")
      ("fromfax") ("fromlogo") ("fromname") ("fromphone") ("fromurl")
      ("invoice")
      ("location")
      ("myref")
      ("place") ("placeseparator") ("phoneseparator")
      ("signature") ("specialmail") ("subject") ("subjectseparator")
      ("title") ("toname") ("toaddress")
      ("yourmail") ("yourref"))
    nil nil)
   optional))

(defun TeX-arg-KOMA-scrlttr-fontelements (optional &optional prompt)
  "Prompt for KOMA-Script's scrlttr2 fontelements with completion."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt optional prompt "Element")
    '(("")
      ("backaddress")
      ("descriptionlabel")
      ("fromaddress") ("fromname")
      ("pagefoot") ("pagehead") ("pagenumber")
      ("subject")
      ("title"))
    nil t)
   optional))
;;; scrlttr2.el ends here
