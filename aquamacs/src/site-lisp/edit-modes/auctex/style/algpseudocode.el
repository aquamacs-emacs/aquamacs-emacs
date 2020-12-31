;;; algpseudocode.el --- AUCTeX style for the (LaTeX) algpseudocode package

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Uwe Brauer <oub@mat.ucm.es>
;; Created: 2020-01-26
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:
;; This file adds support for the algpseudocode package.

;;; Code:

(defvar LaTeX-algpseudocode-package-options
  '("compatible" "nocompatible")
  "Package options for the algpseudocode package.")


(TeX-add-style-hook
 "algpseudocode"
 (lambda ()
   (TeX-add-symbols
    '("algref" 2)
    '("algstore" 1)
    '("algrestore" 1)
    '("algstore*" 1)
    '("algrestore*" 1)
    '("Procedure" 2)
    '("Comment" 1)
    '("State" 0)
    '("While" 0)
    '("EndWhile" 0)
    '("EndProcedure" 0)
    '("Repeat" 0)
    '("Until" 0)
    '("For" 1)
    '("ForAll" 1)
    '("EndFor" 0)
    '("If" 1)
    '("ElsIf" 1)
    '("Else" 0)
    '("EndIf" 0)
    '("Function" 2)
    '("EndFunction" 0)
    '("Loop" 0)
    '("EndLoop" 0)
    '("Require" 0)
    '("Ensure" 0)
    '("State" 0)
    '("Statex" 0)
    '("Call" 0))
   (LaTeX-add-environments
    '("algorithmic" [ "Number" ]))
   LaTeX-dialect))

;;; algpseudocode.el ends here
