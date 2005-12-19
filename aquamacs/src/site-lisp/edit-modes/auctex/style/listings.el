;;; listings.el --- AUCTeX style for `listings.sty'

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-10-17
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

;; This file adds support for `listings.sty'.
;;
;; FIXME: Please make me more sophisticated!

;;; Code:

(TeX-add-style-hook
 "listings"
 (lambda ()
   ;; New symbols
   (TeX-add-symbols
    '("lstalias" ["Alias dialect"] "Alias" ["Dialect"] "Language")
    '("lstdefinestyle" "Style name" "Arguments (key=value list)")
    '("lstinline" TeX-arg-verb)
    '("lstinputlisting" ["Arguments (key=value list)"] TeX-arg-file)
    "lstlistoflistings"
    '("lstnewenvironment" "Name" ["Number or arguments"] ["Default argument"]
      "Starting code" "Ending code")
    "lstset")
   ;; New environments
   (LaTeX-add-environments
    "lstlisting")
   ;; Filling
   (make-local-variable 'LaTeX-indent-environment-list)
   (add-to-list 'LaTeX-indent-environment-list
		'("lstlisting" current-indentation))
   (make-local-variable 'LaTeX-verbatim-regexp)
   (setq LaTeX-verbatim-regexp (concat LaTeX-verbatim-regexp "\\|lstlisting"))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (add-to-list 'font-latex-match-function-keywords-local "lstnewenvironment")
     (font-latex-match-function-make)
     (add-to-list 'font-latex-match-reference-keywords-local "lstinputlisting")
     (font-latex-match-reference-make)
     (add-to-list 'font-latex-match-textual-keywords-local "lstinline")	; Better
									; idea?
     (add-to-list 'font-latex-match-textual-keywords-local "lstlistoflistings")
     (font-latex-match-textual-make)
     (add-to-list 'font-latex-match-variable-keywords-local "lstalias")
     (add-to-list 'font-latex-match-variable-keywords-local "lstdefinestyle")
     (add-to-list 'font-latex-match-variable-keywords-local "lstset")
     (font-latex-match-variable-make)
     ;; For syntactic fontification, e.g. verbatim constructs.
     (font-latex-set-syntactic-keywords)
     ;; Tell font-lock about the update.
     (setq font-lock-set-defaults nil)
     (font-lock-set-defaults))))

(defvar LaTeX-listings-package-options '("draft" "final" "savemem" 
					 "noaspects")
  "Package options for the listings package.")

;;; listings.el ends here
