;;; scrpage2.el -- AUCTeX style for scrpage2.sty

;; License:  GPL, see the file COPYING in the base directory of AUCTeX
;; Author:   Ralf Angeli <angeli@iwi.uni-sb.de>
;; Created:  2003-11-01
;; Keywords: tex

;;; Commentary:

;; This file adds support for `scrpage2.sty'.

;;; Code:

(TeX-add-style-hook
 "scrpage2"
 (lambda ()

   ;; New symbols
   (TeX-add-symbols
    '("lehead" [ "scrplain-left-even" ] "scrheadings-left-even")
    '("cehead" [ "scrplain-center-even" ] "scrheadings-center-even")
    '("rehead" [ "scrplain-right-even" ] "scrheadings-right-even")
    '("lefoot" [ "scrplain-left-even" ] "scrheadings-left-even")
    '("cefoot" [ "scrplain-center-even" ] "scrheadings-center-even")
    '("refoot" [ "scrplain-right-even" ] "scrheadings-right-even")
    '("lohead" [ "scrplain-left-odd" ] "scrheadings-left-odd")
    '("cohead" [ "scrplain-center-odd" ] "scrheadings-center-odd")
    '("rohead" [ "scrplain-right-odd" ] "scrheadings-right-odd")
    '("lofoot" [ "scrplain-left-odd" ] "scrheadings-left-odd")
    '("cofoot" [ "scrplain-center-odd" ] "scrheadings-center-odd")
    '("rofoot" [ "scrplain-right-odd" ] "scrheadings-right-odd")
    '("ihead" [ "scrplain-inside" ] "scrheadings-inside")
    '("chead" [ "scrplain-center" ] "scrheadings-center")
    '("ohead" [ "scrplain-outside" ] "scrheadings-outside")
    '("ifoot" [ "scrplain-inside" ] "scrheadings-inside")
    '("cfoot" [ "scrplain-center" ] "scrheadings-center")
    '("ofoot" [ "scrplain-outside" ] "scrheadings-outside")
    '("clearscrheadfoot")
    '("clearscrheadings")
    '("clearscrplain")
    '("automark" [ "Right page" ] "Left page")
    '("headmark")
    '("manualmark")
    '("pagemark")
    '("leftmark")
    '("rightmark")
    '("setfootwidth" [ "Offset" ] "Width")
    '("setheadwidth" [ "Offset" ] "Width")
    '("setfootbotline" [ "Length" ] "Thickness")
    '("setfootsepline" [ "Length" ] "Thickness")
    '("setheadtopline" [ "Length" ] "Thickness")
    '("setheadsepline" [ "Length" ] "Thickness")
    '("deftripstyle" "Name" [ "Thickness of outer line" ]
      [ "Thickness of inner line" ] "Inner box of page head"
      "Center box of page head" "Outer box of page head"
      "Inner box of page foot" "Center box of page foot"
      "Outer box of page foot")
    '("defpagestyle" "Name" "Head definition" "Foot definition")
    '("newpagestyle" "Name" "Head definition" "Foot definition")
    '("renewpagestyle" "Name" "Head definition" "Foot definition")
    '("providepagestyle" "Name" "Head definition" "Foot definition"))

    ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (setq font-latex-match-variable-keywords-local
	   (append font-latex-match-variable-keywords-local
		   '("lehead"
		     "cehead"
		     "rehead"
		     "lefoot"
		     "cefoot"
		     "refoot"
		     "lohead"
		     "cohead"
		     "rohead"
		     "lofoot"
		     "cofoot"
		     "rofoot"
		     "ihead"
		     "chead"
		     "ohead"
		     "ifoot"
		     "cfoot"
		     "ofoot"
		     "automark"
		     "setfootwidth"
		     "setheadwidth"
		     "setfootbotline"
		     "setfootsepline"
		     "setheadtopline"
		     "setheadsepline")))
     (font-latex-match-variable-make)
     (setq font-latex-match-function-keywords-local
	   (append font-latex-match-function-keywords-local
		   '("deftripstyle"
		     "defpagestyle"
		     "newpagestyle"
		     "renewpagestyle"
		     "providepagestyle")))
     (font-latex-match-function-make))))

(defvar LaTeX-scrpage2-package-options '("headinclude" "headexclude"
					 "footinclude" "footexclude"
					 "mpinclude" "mpexclude"
					 "headtopline" "headsepline"
					 "footsepline" "footbotline"
					 "plainheadtopline" "plainheadsepline"
					 "plainfootsepline" "plainfootbotline"
					 "ilines" "clines" "olines"
					 "automark" "manualmark"
					 "autooneside" "markuppercase"
					 "markusedcase" "nouppercase"
					 "komastyle" "standardstyle")
  "Package options for the scrpage2 package.")

;;; scrpage2.el ends here