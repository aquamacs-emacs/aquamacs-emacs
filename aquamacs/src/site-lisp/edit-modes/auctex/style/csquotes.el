;;; csquotes.el --- AUCTeX style for `csquotes.sty'

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-11-29
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

;; This file adds support for `csquotes.sty', version 3.0.


;;; Code:

;; FIXME: It would be nice to be able to dump this function in favor
;; of a generalized handling of additional arguments for environments
;; specified via `LaTeX-add-environments'.  `TeX-parse-arguments' and
;; friends would be the functions to be used for that, but those
;; functions currently insert text directly into the buffer.  There
;; would either have to be a way of preventing this and letting them
;; return a string, or the insertion could happen in a temporary buffer
;; and the buffer content be returned.
(defun LaTeX-csquotes-insert-environment (env &rest args)
  "Insert environment ENV considering optional arguments ARGS.

This is basically a variant of `LaTeX-environment-menu'
specialized for csquotes.el.  ARGS can be made up of strings and
vectors containing single strings.  Plain strings will be used as
prompts for mandatory arguments and strings in vectors as prompts
for optional arguments of the environment to be inserted.

That means, in contrast to `LaTeX-environment-menu' it supports
the insertion of optional arguments."
  (let (env-extra prompt optional user-input)
    (dolist (elt args)
      (if (vectorp elt)
	  (setq prompt (aref elt 0)
		optional t)
	(setq optional nil))
      (setq user-input (read-string (TeX-argument-prompt optional prompt nil)))
      (unless (and optional (zerop (length user-input)))
	(setq env-extra (concat env-extra
				(if optional LaTeX-optop TeX-grop)
				user-input
				(if optional LaTeX-optcl TeX-grcl)))))
    (LaTeX-insert-environment env env-extra)))

(TeX-add-style-hook
 "csquotes"
 (lambda ()
   (let ((quote-style-variant-list '(("quotes") ("guillemets") ("american")
				     ("british") ("oldstyle") ("imprimerie")
				     ("swiss")))
	 (quote-style-name-list '(("danish") ("dutch") ("english") ("finnish")
				  ("french") ("german") ("italian")
				  ("norwegian") ("swedish"))))
     ;; New symbols
     (TeX-add-symbols
      '("enquote" 1)
      '("enquote*" 1)
      '("foreignquote" 2)
      '("foreignquote*" 2)
      '("hyphenquote" 2)
      '("hyphenquote*" 2)
      '("blockquote" ["Citation"] ["Punctuation"] t)
      '("foreignblockquote" t ["Citation"] ["Punctuation"] nil)
      '("hyphenblockquote" t ["Citation"] ["Punctuation"] nil)
      `("setquotestyle"
	[ (TeX-arg-eval completing-read "Quote style variant: "
			',quote-style-variant-list) ]
	(TeX-arg-eval completing-read "Quote style name or alias: "
		      ',quote-style-name-list))
      "setquotestyle*"
      '("MakeInnerQuote" "Character")
      '("MakeOuterQuote" "Character")
      '("MakeAutoQuote" "Opening quotation mark" "Closing quotation mark")
      '("MakeForeignQuote" "Babel's language name"
	"Opening quotation mark" "Closing quotation mark")
      '("MakeHyphenQuote" "Babel's language name"
	"Opening quotation mark" "Closing quotation mark")
      '("MakeBlockQuote" "Opening quotation mark" "Delimiter for citation"
	"Closing quotation mark")
      '("MakeForeignBlockQuote" "Language" "Opening quotation mark"
	"Delimiter for citation" "Closing quotation mark")
      '("MakeHyphenBlockQuote" "Language" "Opening quotation mark"
	"Delimiter for citation" "Closing quotation mark")
      "DisableQuotes"
      "RestoreQuotes"
      '("cquote" ["Pre-note"] ["Post-note"] "Key" t)
      '("cquote*" ["Pre-note"] ["Post-note"] "Key" t)
      '("foreigncquote" "Language" ["Pre-note"] ["Post-note"] "Key" t)
      '("foreigncquote*" "Language" ["Pre-note"] ["Post-note"] "Key" t)
      '("hyphencquote" "Language" ["Pre-note"] ["Post-note"] "Key" t)
      '("hyphencquote*" "Language" ["Pre-note"] ["Post-note"] "Key" t)
      '("blockcquote" ["Pre-note"] ["Post-note"] "Key" ["Punctuation"] t)
      '("foreignblockcquote" "Language" ["Pre-note"] ["Post-note"] "Key"
	["Punctuation"] t)
      '("hyphenblockcquote" "Language" ["Pre-note"] ["Post-note"] "Key"
	["Punctuation"] t)
      `("DeclareQuoteStyle"
	[ (TeX-arg-eval completing-read "Quote style variant: "
			',quote-style-variant-list) ]
	(TeX-arg-eval completing-read "Quote style name: "
		      ',quote-style-name-list)
	["Outer quote initialization"] ["Inner quote initialization"]
	"Opening outer quotation mark" ["Middle outer quotation mark"]
	"Closing outer quotation mark" ["Kerning between adjoining marks"]
	"Opening inner quotation mark" ["Middle inner quotation mark"]
	"Closing inner quotation mark")
      `("DeclareQuoteAlias"
	[ (TeX-arg-eval completing-read "Quote style variant: "
			',quote-style-variant-list) ]
	(TeX-arg-eval completing-read "Quote style name: "
		      ',quote-style-name-list)
	"Alias name")
    '("DeclareQuoteOption" 1)
    '("DeclarePlainStyle" "Opening outer quotation mark"
      "Closing outer quotation mark" "Opening inner quotation mark"
      "Closing inner quotation mark")
    '("SetBlockThreshold" "Number of lines")
    '("SetBlockEnvironment" "Environment")
    '("SetCiteCommand" "Command")
    "mkcitation"
    "mkccitation"
    "mkmidblockpunct"
    "mkfinblockpunct"
    '("ifblockquote" 2)
    '("ifquotepunct" 2)
    '("ifquoteterm" 2)
    '("ifquoteperiod" 2)
    '("ifquotecomma" 2)
    '("ifquotesemicolon" 2)
    '("ifquotecolon" 2)
    '("ifquoteexclam" 2)
    '("ifquotequestion" 2)
    '("ifstringblank" 2))
   ;; New environments
   (LaTeX-add-environments
    "quoteblock"
    '("displayquote" "Citation")
    '("foreigndisplayquote" "Language" "Citation")
    '("hyphendisplayquote" "Language" "Citation")
    '("displaycquote" LaTeX-csquotes-insert-environment
      ["Pre-note"] ["Post-note"] "Key")
    '("foreigndisplaycquote" LaTeX-csquotes-insert-environment
      "Language" ["Pre-note"] ["Post-note"] "Key")
    '("hyphendisplaycquote" LaTeX-csquotes-insert-environment
      "Language" ["Pre-note"] ["Post-note"] "Key"))
   ;; Quotation marks
   (when (and (> (length LaTeX-csquotes-open-quote) 0)
	      (> (length LaTeX-csquotes-close-quote) 0))
     (setq TeX-quote-language
	   `(override ,LaTeX-csquotes-open-quote ,LaTeX-csquotes-close-quote
		      ,LaTeX-csquotes-quote-after-quote)))
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (add-to-list 'font-latex-match-function-keywords-local "DisableQuotes")
     (add-to-list 'font-latex-match-function-keywords-local "RestoreQuotes")
     (font-latex-match-function-make)
     (add-to-list 'font-latex-match-reference-keywords-local "blockcite")
     (font-latex-match-reference-make)
     (mapcar (lambda (keyword)
	       (add-to-list 'font-latex-match-textual-keywords-local keyword))
	     '("enquote"
	       "foreignquote"
	       "hyphenquote"
	       "cquote"
	       "cquote*"
	       "foreigncquote"
	       "foreigncquote*"
	       "hyphencquote"
	       "hyphencquote*"
	       "blockquote"
	       "foreignblockquote"
	       "hyphenblockquote"
	       "blockcquote"
	       "foreignblockcquote"
	       "hyphenblockcquote"))
     (font-latex-match-textual-make)
     (mapcar (lambda (keyword)
	       (add-to-list 'font-latex-match-variable-keywords-local keyword))
	     '("setquotestyle"
	       "setquotestyle"
	       "MakeOuterQuote"
	       "MakeInnerQuote"
	       "MakeAutoQuote"
	       "MakeForeignQuote"
	       "MakeHyphenQuote"
	       "MakeBlockQuote"
	       "MakeForeignBlockQuote"
	       "MakeHyphenBlockQuote"
	       "DeclareQuoteStyle"
	       "DeclareQuoteAlias"
	       "DeclareQuoteOption"
	       "DeclarePlainStyle"
	       "SetBlockThreshold"
	       "SetBlockEnvironment"
	       "SetCiteCommand"))
     (font-latex-match-variable-make)
     ;; Tell font-lock about the update.
     (setq font-lock-set-defaults nil)
     (font-lock-set-defaults)))))

;;; csquotes.el ends here
