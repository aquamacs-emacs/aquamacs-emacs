;;; ltxguide.el --- AUCTeX style for `ltxguide.cls' (2001/05/28)

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2020-01-05
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

;; This file adds support for `ltxguide.cls' from 2001/05/28.
;; `ltxguide.cls' is part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(defvar LaTeX-article-class-options)

(defun LaTeX-item-ltxguide-decl ()
  "Insert line break macro on the previous line.
For decl environment provided by ltxguide.cls."
  (save-excursion
    (end-of-line 0)
    (just-one-space)
    (TeX-insert-macro "\\")))

(TeX-add-style-hook
 "ltxguide"
 (lambda ()

   ;; ltxguide.cls loads shortvrb.sty and sets | as a shorthand.
   ;; Append it to a local version of `LaTeX-shortvrb-chars' before
   ;; running the style hook for `shortvrb':
   (add-to-list (make-local-variable 'LaTeX-shortvrb-chars) ?| t)

   ;; Run style hooks for packages loaded by default:
   (TeX-run-style-hooks "shortvrb" "article")

   (TeX-add-symbols
    "clsguide"
    "usrguide"
    "fntguide"
    "cfgguide"
    "cyrguide"
    "modguide"
    "sourcecode"
    "LaTeXbook"
    "LaTeXcomp"
    "LaTeXGcomp"
    "LaTeXWcomp"
    "babel"
    "ctan"
    "eg"
    "ie"
    "SLiTeX"
    '("m" "Argument")
    '("arg" "Argument")
    '("oarg" "Argument")
    "NFSS"
    "AmSLaTeX"
    '("URL" "URL")
    '("NEWdescription" TeX-arg-date)
    '("NEWfeature" TeX-arg-date))

   (LaTeX-add-environments
    '("decl" LaTeX-env-args [ "Date" ]))

   ;; Enable `LaTeX-insert-item' in decl-environments:
   (add-to-list 'LaTeX-item-list
		'("decl" . LaTeX-item-ltxguide-decl)
		t)

   ;; Make the next 2 macros stay in their own line:
   (LaTeX-paragraph-commands-add-locally '("NEWdescription"
					   "NEWfeature"))

   ;; Verbatim-like macros with braces as delimiters:
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "URL")

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("m"              "{")
				("arg"            "{")
				("oarg"           "{")
				("NEWfeature"     "{")
				("NEWdescription" "{"))
			      'textual)
     (font-latex-add-keywords '(("URL" ""))
			      'reference)))
 LaTeX-dialect)

(defvar LaTeX-ltxguide-class-options
  (progn
    (TeX-load-style "article")
    LaTeX-article-class-options)
  "Options for the ltxguide document class.")

;;; ltxguide.el ends here
