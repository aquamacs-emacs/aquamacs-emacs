;;; url.el --- AUCTeX style for `url.sty'

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-10-13
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

;; This file adds support for `url.sty'.

;;; Code:

(TeX-add-style-hook
 "url"
 (lambda ()
   ;; New symbols
   (TeX-add-symbols
    "Url"
    "UrlBigBreakPenalty"
    "UrlBigBreaks"
    "UrlBreakPenalty"
    "UrlBreaks"
    "UrlFont"
    "UrlLeft"
    "UrlNoBreaks"
    "UrlOrds"
    "UrlRight"
    "UrlSpecials"
    "url"
    "urldef"
    '("urlstyle" TeX-arg-urlstyle))
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (add-to-list 'font-latex-match-reference-keywords-local "url")
     (font-latex-match-reference-make)
     (mapcar (lambda (keyword)
	       (add-to-list 'font-latex-match-variable-keywords-local keyword))
	     '("Url"
	       "UrlBigBreakPenalty"
	       "UrlBigBreaks"
	       "UrlBreakPenalty"
	       "UrlBreaks"
	       "UrlFont"
	       "UrlLeft"
	       "UrlNoBreaks"
	       "UrlOrds"
	       "UrlRight"
	       "UrlSpecials"
	       "urldef"
	       "urlstyle"))
     (font-latex-match-variable-make)
     (add-to-list 'font-latex-verb-like-commands-local "url")
     (add-to-list 'font-latex-verbatim-macros-local "url")
     (font-latex-set-syntactic-keywords)
     ;; Tell font-lock about the update.
     (setq font-lock-set-defaults nil)
     (font-lock-set-defaults))))

(defun TeX-arg-urlstyle (optional &optional prompt)
  "Prompt for style used in \\urlstyle with completion."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Style")
		    (mapcar 'list '("rm" "same" "sf" "tt"))
		    nil t)
   optional))

;;; url.el ends here
