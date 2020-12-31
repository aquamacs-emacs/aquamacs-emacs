;;; fbox.el --- AUCTeX style for `fbox.sty' (v0.04)

;; Copyright (C) 2019--2020 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2019-11-08
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

;; This file adds support for `fbox.sty' (v0.04) from 2020/01/03.
;; `fbox.sty' is part of TeXLive.

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(TeX-add-style-hook
 "fbox"
 (lambda ()
   (TeX-add-symbols
    '("fbox"     [ "Frame parts (combination of lrtb)" ] t)
    '("fbox*"    [ "Frame parts (combination of lrtb)" ] t)
    '("fparbox"  [ "Frame parts (combination of lrtb)" ] t)
    '("fparbox*" [ "Frame parts (combination of lrtb)" ] t))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("fbox"    "*[{")
				("fparbox" "*[{"))
			      'function)))
 LaTeX-dialect)

;;; fbox.el ends here
