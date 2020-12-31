;;; shortvrb.el --- AUCTeX style for `shortvrb.sty'

;; Copyright (C) 2009, 2014, 2018, 2020 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2009-12-23
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

;; This file adds support for `shortvrb.sty'.

;; XXX: We might want provide users with the possibility to activate
;; something like this for any file (incl. Plain TeX).  That would
;; bring us one step closer to the goal of displaying texbook.tex
;; without font locking going haywire.

;; FIXME: The code does not work for preview.dtx because in that file
;; the style list is empty.  In its master file, preview.drv, it
;; works, however.  However, even if the style file is loaded by hand,
;; it fails to fontify verbatim text in the documentation parts of the
;; file.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-to-syntax-alist
		  "font-latex"
		  (list))

(TeX-add-style-hook
 "shortvrb"
 (lambda ()

   ;; Ispell: Add entries to `ispell-tex-skip-alist':
   (when LaTeX-shortvrb-chars
     (TeX-ispell-skip-setcar
      (mapcar (lambda (char)
		(let ((str (char-to-string char)))
		  (cons str str)))
	      LaTeX-shortvrb-chars)))

   ;; Fontification
   (when (and LaTeX-shortvrb-chars
	      (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-to-syntax-alist
      (mapcar (lambda (char) (cons char "|"))
	      LaTeX-shortvrb-chars))))
 LaTeX-dialect)

;;; shortvrb.el ends here
