;;; listings.el --- AUCTeX style for `listings.sty'

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-10-17
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
     (font-latex-add-keywords '(("lstnewenvironment" "{[[{{")) 'function)
     (font-latex-add-keywords '(("lstinputlisting" "[{")) 'reference)
     (font-latex-add-keywords '(("lstinline" "[{") ; The second argument should
						   ; actually be verbatim.
				("lstlistoflistings" ""))
			      'textual)
     (font-latex-add-keywords '(("lstalias" "{{")
				("lstdefinestyle" "{{")
				("lstset" "{"))
			      'variable)
     ;; For syntactic fontification, e.g. verbatim constructs.
     (font-latex-set-syntactic-keywords)
     ;; Tell font-lock about the update.
     (setq font-lock-set-defaults nil)
     (font-lock-set-defaults))))

(defvar LaTeX-listings-package-options '("draft" "final" "savemem" 
					 "noaspects")
  "Package options for the listings package.")

;;; listings.el ends here
