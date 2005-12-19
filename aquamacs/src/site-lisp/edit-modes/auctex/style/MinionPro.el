;;; MinionPro.el -- AUCTeX style for MinionPro.sty

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Mark Trettin <Mark.Trettin@gmx.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2005-11-26
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary: 

;; This file adds support for `MinionPro.sty' (v2.0). 

;;; Code

(TeX-add-style-hook
 "MinionPro"
 (lambda ()
   (TeX-add-symbols
    ;; New symbols
    '("figureversion"
      (TeX-arg-eval completing-read "Figure style: "
		    '(("text") ("osf")
		      ("lining") ("lf")
		      ("tabular") ("tab")
		      ("proportional") ("prop"))))
    '("smallfrac" "Numerator" "Denominator")
    '("slantfrac" "Numerator" "Denominator")
    ;; IMHO they should be added to the other \text.. and \..shape commands
    '("textsw" 1)
    '("textssc" 1)
    "sscshape"
    "swshape")
   ;; Fontification
   ;; FIXME: fontification for the textcommands suck
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (setq font-latex-match-textual-keywords-local
	   (append font-latex-match-textual-keywords-local
		   '("smallfrac"
		     "slantfrac"
		     "textsw"
		     "textssc")))
     (font-latex-match-textual-make)
     (add-to-list 'font-latex-match-variable-keywords-local
		  "figureversion")
     (font-latex-match-variable-make))))

(defvar LaTeX-MinionPro-package-options
  '("smallfamily" "medfamily" "fullfamily" "noopticals" "opticals"
    "slides" "textosf" "mathosf" "osf" "textlf" "mathlf" "lf"
    "mathtabular" "mnsy" "cmsy" "swash" "abx" "amsbb" "fourierbb"
    "lucidabb" "mixedgreek" "italicgreek" "frenchmath" "minionint"
    "footnotefigures")
"Package options for the MinionPro package.")

;;; MinionPro.el ends here
