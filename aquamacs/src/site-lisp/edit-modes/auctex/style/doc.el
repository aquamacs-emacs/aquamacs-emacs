;;; doc.el --- AUCTeX style for `doc.sty'

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Frank Küster <frank@kuesterei.ch>
;; Maintainer: auctex-devel@gnu.org
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

;; This file adds support for `doc.sty'.

;;; Code:

(defun LaTeX-env-no-comment (environment)
  "Insert ENVIRONMENT and make sure there is no commented empty line inside."
  (LaTeX-insert-environment environment)
  (unless (TeX-active-mark)
    (when (save-excursion
	    (beginning-of-line)
	    (looking-at (concat "[ \t]+$\\|[ \t]*"
				TeX-comment-start-regexp "+[ \t]*$")))
      (delete-region (line-beginning-position) (line-end-position))
      (indent-according-to-mode))))

(TeX-add-style-hook
 "doc"
 (function
  (lambda ()
    (LaTeX-add-environments
     "theglossary"
     '("macrocode" LaTeX-env-no-comment)
     '("macro" "Macro"))
    (TeX-add-symbols
     "EnableCrossrefs"
     "DisableCrossrefs"
     "DoNotIndex"
     "DontCheckModules"
     "CheckModules"
     "Module"
     '("DescribeMacro" "Macro")
     '("DescribeEnv" "Environment")
     "verbatim"
     "verb"
     "parg"
     "oarg"
     "marg"
     "meta"
     "cmd"
     "makelabel"
     "MacroFont"
     "MacroFont"
     "AltMacroFont"
     "AltMacroFont"
     "PrintMacroName"
     "PrintDescribeMacro"
     "PrintDescribeEnv"
     "PrintEnvName"
     "MakePrivateLetters"
     "actualchar"
     "quotechar"
     "levelchar"
     "encapchar"
     "verbatimchar"
     "SpecialIndex"
     "SpecialMainIndex"
     "SpecialMainEnvIndex"
     "SpecialUsageIndex"
     "SpecialEnvIndex"
     "SortIndex"
     "LeftBraceIndex"
     "RightBraceIndex"
     "PercentIndex"
     "OldMakeindex"
     "PercentIndex"
     "IndexPrologue"
     "IndexParms"
     "subitem"
     "subsubitem"
     "indexspace"
     "efill"
     "pfill"
     "PrintIndex"
     '("changes" "version" "date (YYYY/MM/DD)")
     "generalname"
     "RecordChanges"
     "GlossaryPrologue"
     "GlossaryParms"
     "PrintChanges"
     "AlsoImplementation"
     "StopEventually"
     "OnlyDescription"
     "Finale"
     "IndexInput"
     "maketitle"
     "MakeShortVerb"
     "DeleteShortVerb"
     "MakeShortverb"
     "DeleteShortverb"
     "CheckSum"
     "CharacterTable"
     "CharTableChanges"
     "CodelineNumbered"
     "CodelineIndex"
     "PageIndex"
     "theCodelineNo"
     "theCodelineNo"
     "DocstyleParms"
     "MakePercentIgnore"
     "MakePercentComment"
     "DocInput"
     "DocInclude"
     "GetFileInfo"
     "filename"
     "fileinfo"))))

;; Local Variables:
;; coding: iso-8859-1
;; End:
