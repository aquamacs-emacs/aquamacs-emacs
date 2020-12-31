;;; csquotes.el --- AUCTeX style for `csquotes.sty' (v5.2j)

;; Copyright (C) 2004, 2005, 2006, 2014, 2018, 2020 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-11-29
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

;; This file adds support for `csquotes.sty', version 5.2j from
;; 2019/12/06.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))

(defun LaTeX-csquotes-read-language (optional &optional prompt)
  "Read and return a language for csquotes macros.
If OPTIONAL is non-nil, indicate it in minibuffer while reading
user input.  PROMPT replaces the standard one \"Language\".  This
function checks if the functions `LaTeX-babel-active-languages'
or `LaTeX-polyglossia-active-languages' are bound and use them to
retrieve the active languages.  If none available, user is
requested to enter a language."
  (cond ((and (fboundp 'LaTeX-babel-active-languages)
	      (LaTeX-babel-active-languages))
	 (completing-read
	  (TeX-argument-prompt optional prompt "Language")
	  (LaTeX-babel-active-languages)))
	((and (fboundp 'LaTeX-polyglossia-active-languages)
	      (LaTeX-polyglossia-active-languages))
	 (completing-read
	  (TeX-argument-prompt optional prompt "Language")
	  (LaTeX-polyglossia-active-languages)))
	(t
	 (TeX-read-string
	  (TeX-argument-prompt optional prompt "Language")))))

(defun LaTeX-arg-csquotes-language (optional &optional prompt)
  "Insert a language for csquotes macros.
If OPTIONAL is non-nil, insert the language in square brackets.
PROMPT replaces the standard one \"Language\"."
  (TeX-argument-insert
   (LaTeX-csquotes-read-language optional prompt)
   optional))

(TeX-add-style-hook
 "csquotes"
 (lambda ()
   (let ((quote-style-variant-list '(("american")   ("brazilian")
				     ("british")    ("german")
				     ("guillemets") ("guillemets*")
				     ("mexican")    ("portuguese")
				     ("quotes")     ("quotes*")
				     ("spanish")    ("swiss")))
	 (quote-style-name-list '(("austrian")   ("croatian") ("czech")
				  ("danish")     ("dutch")    ("english")
				  ("finnish")    ("french")   ("german")
				  ("greek")      ("italian")  ("norwegian")
				  ("portuguese") ("russian")  ("serbian")
				  ("spanish")    ("swedish"))))
     ;; New symbols
     (TeX-add-symbols

      ;; 3.1 Quoting Regular Text
      '("enquote" 1)
      '("enquote*" 1)

      ;; 3.2 Quoting Text in a Foreign Language
      '("foreignquote"  LaTeX-arg-csquotes-language 1)
      '("foreignquote*" LaTeX-arg-csquotes-language 1)
      '("hyphenquote"   LaTeX-arg-csquotes-language 1)
      '("hyphenquote*"  LaTeX-arg-csquotes-language 1)

      ;; 3.3 Formal Quoting of Regular Text
      '("textquote"  ["Citation"] ["Punctuation"] t)
      '("textquote*" ["Citation"] ["Punctuation"] t)

      ;; 3.4 Formal Quoting of Text in a Foreign Language
      '("foreigntextquote"
	LaTeX-arg-csquotes-language ["Citation"] ["Punctuation"] t)
      '("foreigntextquote*"
	LaTeX-arg-csquotes-language ["Citation"] ["Punctuation"] t)
      '("hyphentextquote"
	LaTeX-arg-csquotes-language ["Citation"] ["Punctuation"] t)
      '("hyphentextquote*"
	LaTeX-arg-csquotes-language ["Citation"] ["Punctuation"] t)

      ;; 3.5 Block Quoting of Regular Text
      '("blockquote" ["Citation"] ["Punctuation"] t)

      ;; 3.6 Block Quoting of Text in a Foreign Language
      '("foreignblockquote"
	LaTeX-arg-csquotes-language ["Citation"] ["Punctuation"] t)
      '("hyphenblockquote"
	LaTeX-arg-csquotes-language ["Citation"] ["Punctuation"] t)
      '("hybridblockquote"
	LaTeX-arg-csquotes-language ["Citation"] ["Punctuation"] t)

      ;; 3.7 Selecting Quote Styles
      `("setquotestyle"
	[ (TeX-arg-eval completing-read "Quote style variant: "
			',quote-style-variant-list) ]
	(TeX-arg-eval completing-read "Quote style name or alias: "
		      ',quote-style-name-list))
      "setquotestyle*"

      ;; 4.1 Quoting Regular Text
      '("MakeInnerQuote" "Character")
      '("MakeOuterQuote" "Character")
      '("MakeAutoQuote"  "Opening quotation mark" "Closing quotation mark")
      '("MakeAutoQuote*" "Opening quotation mark" "Closing quotation mark")

      ;; 4.2 Quoting Text in a Foreign Language
      '("MakeForeignQuote" LaTeX-arg-csquotes-language
	"Opening quotation mark" "Closing quotation mark")
      '("MakeForeignQuote*" LaTeX-arg-csquotes-language
	"Opening quotation mark" "Closing quotation mark")

      '("MakeHyphenQuote" LaTeX-arg-csquotes-language
	"Opening quotation mark" "Closing quotation mark")
      '("MakeHyphenQuote" LaTeX-arg-csquotes-language
	"Opening quotation mark" "Closing quotation mark")

      ;; 4.3 Block Quoting of Regular Text
      '("MakeBlockQuote" "Opening quotation mark" "Delimiter for citation"
	"Closing quotation mark")

      ;; 4.4 Block Quoting of Text in a Foreign Language
      '("MakeForeignBlockQuote" LaTeX-arg-csquotes-language
	"Opening quotation mark" "Delimiter for citation" "Closing quotation mark")
      '("MakeHyphenBlockQuote" LaTeX-arg-csquotes-language
	"Opening quotation mark" "Delimiter for citation" "Closing quotation mark")
      '("MakeHybridBlockQuote" LaTeX-arg-csquotes-language
	"Opening quotation mark" "Delimiter for citation" "Closing quotation mark")

      ;; 4.5 Controlling Active Quotes
      "EnableQuotes"
      "DisableQuotes"
      "VerbatimQuotes"
      "DeleteQuotes"

      ;; 5.1 Formal Quoting of Regular Text
      '("textcquote"  ["Pre-note"] ["Post-note"] "Key" ["Punctuation"] t)
      '("textcquote*" ["Pre-note"] ["Post-note"] "Key" ["Punctuation"] t)

      ;; 5.2 Formal Quoting of Text in a Foreign Language
      '("foreigntextcquote" LaTeX-arg-csquotes-language
	["Pre-note"] ["Post-note"] "Key" ["Punctuation"] t)
      '("foreigntextcquote*" LaTeX-arg-csquotes-language
	["Pre-note"] ["Post-note"] "Key" ["Punctuation"] t)
      '("hyphentextcquote" LaTeX-arg-csquotes-language
	["Pre-note"] ["Post-note"] "Key" ["Punctuation"] t)
      '("hyphentextcquote*" LaTeX-arg-csquotes-language
	["Pre-note"] ["Post-note"] "Key" ["Punctuation"] t)

      ;; 5.3 Block Quoting of Regular Text
      '("blockcquote" ["Pre-note"] ["Post-note"] "Key" ["Punctuation"] t)

      ;; 5.4 Block Quoting of Text in a Foreign Language
      '("foreignblockcquote" LaTeX-arg-csquotes-language
	["Pre-note"] ["Post-note"] "Key" ["Punctuation"] t)
      '("hyphenblockcquote" LaTeX-arg-csquotes-language
	["Pre-note"] ["Post-note"] "Key" ["Punctuation"] t)
      '("hybridblockcquote" LaTeX-arg-csquotes-language
	["Pre-note"] ["Post-note"] "Key" ["Punctuation"] t)

      ;; 7 Auxiliary Commands
      '("textelp" 1)
      '("textelp*" 1)
      '("textins" 1)
      '("textins*" 1)
      '("textdel" 1)

      ;; 8.1 Defining Quote Styles
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
      '("ExecuteQuoteOptions" 1)
      '("DeclarePlainStyle" "Opening outer quotation mark"
	"Closing outer quotation mark" "Opening inner quotation mark"
	"Closing inner quotation mark")
      '("SetBlockThreshold" "Number of lines")
      '("SetBlockEnvironment" "Environment")
      '("SetCiteCommand" "Command")

      ;; 8.7 Hooks for Quotations and Citations
      "mkcitation"
      "mkccitation"
      "mktextquote"
      "mkblockquote"
      "mkbegdispquote"
      "mkenddispquote"

      ;; 8.8 Additional Tests in Quotation Hooks
      '("ifpunctmark" "Character" 2)
      '("ifpunct" 2)
      '("ifterm" 2)
      '("iftextpunctmark" 4)
      '("iftextpunct" 3)
      '("iftextterm" 3)
      '("ifblockquote" 2)
      '("ifblank" 3)
      "unspace"

      ;; 8.9 Configuring Punctuation Look-Ahead
      '("DeclareAutoPunct" "Characters"))

     ;; New environments
     (LaTeX-add-environments

      ;; 6.1 Basic Display Environments
      '("displayquote" LaTeX-env-args
	["Citation"] ["Punctuation"])

      '("foreigndisplayquote" LaTeX-env-args
	LaTeX-arg-csquotes-language ["Citation"] ["Punctuation"])

      '("hyphendisplayquote" LaTeX-env-args
	LaTeX-arg-csquotes-language ["Citation"] ["Punctuation"])

      ;; 6.2 Integrated Display Environments
      '("displaycquote" LaTeX-env-args
	["Pre-note"] ["Post-note"] "Key" ["Punctuation"])

      '("foreigndisplaycquote" LaTeX-env-args
	LaTeX-arg-csquotes-language["Pre-note"] ["Post-note"] "Key" ["Punctuation"])

      '("hyphendisplaycquote" LaTeX-env-args
	LaTeX-arg-csquotes-language["Pre-note"] ["Post-note"] "Key" ["Punctuation"]))

     ;; Quotation marks
     (when (and (> (length LaTeX-csquotes-open-quote) 0)
		(> (length LaTeX-csquotes-close-quote) 0))
       (setq TeX-quote-language
	     `(override ,LaTeX-csquotes-open-quote ,LaTeX-csquotes-close-quote
			,LaTeX-csquotes-quote-after-quote)))
     ;; Fontification
     (when (and (featurep 'font-latex)
		(eq TeX-install-font-lock 'font-latex-setup))
       (font-latex-add-keywords '(("EnableQuotes"   "")
				  ("DisableQuotes"  "")
				  ("VerbatimQuotes" "")
				  ("DeleteQuotes"   ""))
				'function)
       (font-latex-add-keywords '(("enquote"            "*{")
				  ("foreignquote"       "*{{")
				  ("hyphenquote"        "*{{")
				  ("textquote"          "*[[{")
				  ("foreigntextquote"   "*{[[{")
				  ("hyphentextquote"    "*{[[{")
				  ("blockquote"         "[[{")
				  ("foreignblockquote"  "{[[{")
				  ("hyphenblockquote"   "{[[{")
				  ("hybridblockquote"   "{[[{")
				  ("textcquote"         "*[[{[{")
				  ("foreigntextcquote"  "*{[[{[{")
				  ("hyphentextcquote"   "*{[[{[{")
				  ("blockcquote"        "[[{[{")
				  ("foreignblockcquote" "{[[{[{")
				  ("hyphenblockcquote"  "{[[{[{")
				  ("hybridblockcquote"  "{[[{[{"))
				'textual)
       (font-latex-add-keywords '(("setquotestyle"         "[{")
				  ("MakeOuterQuote"        "{")
				  ("MakeInnerQuote"        "{")
				  ("MakeAutoQuote"         "*{{")
				  ("MakeForeignQuote"      "*{{{")
				  ("MakeHyphenQuote"       "*{{{")
				  ("MakeBlockQuote"        "{{{")
				  ("MakeForeignBlockQuote" "{{{{")
				  ("MakeHyphenBlockQuote"  "{{{{")
				  ("DeclareQuoteStyle"     "[{[[{[{[{[{")
				  ("DeclareQuoteAlias"     "[{{")
				  ("DeclareQuoteOption"    "{")
				  ("DeclarePlainStyle"     "{{{{")
				  ("SetBlockThreshold"     "{")
				  ("SetBlockEnvironment"   "{")
				  ("SetCiteCommand"        "{"))
				'variable))))
 LaTeX-dialect)

(defun LaTeX-csquotes-package-options ()
  "Prompt for package options for the csquotes package."
  (TeX-read-key-val t '(("strict"     ("true" "false"))
			("style"      ("american"
				       "australian"
				       "austrian"
				       "brazil"
				       "brazilian"
				       "british"
				       "canadian"
				       "croatian"
				       "czech"
				       "danish"
				       "dutch"
				       "english"
				       "finnish"
				       "french"
				       "german"
				       "greek"
				       "italian"
				       "mexican"
				       "naustrian"
				       "newzealand"
				       "ngerman"
				       "norsk"
				       "norwegian"
				       "nswissgerman"
				       "nynorsk"
				       "portuges"
				       "portuguese"
				       "russian"
				       "serbian"
				       "spanish"
				       "swedish"
				       "swiss"
				       "swissgerman"
				       "UKenglish"
				       "USenglish"))
			("autostyle"  ("true" "false" "try" "once" "tryonce"))
			("austrian"   ("quotes" "guillemets"))
			("croatian"   ("quotes" "guillemets" "guillemets*"))
			("czech"      ("quotes" "guillemets"))
			("danish"     ("quotes" "guillemets" "topquotes"))
			("english"    ("american" "british"))
			("estonian")
			("french"     ("quotes" "quotes*" "guillemets" "guillemets*"))
			("galician"   ("quotes" "guillemets"))
			("german"     ("quotes" "guillemets" "swiss"))
			("hungarian")
			("italian"    ("guillemets" "quotes"))
			("latvian")
			("norwegian"  ("guillemets" "quotes"))
			("polish"     ("guillemets" "guillemets*"))
			("portuguese" ("portuguese" "brazilian"))
			("serbian"    ("quotes" "guillemets" "german"))
			("spanish"    ("spanish" "mexican"))
			("swedish"    ("quotes" "guillemets" "guillemets*"))
			("maxlevel")
			("autopunct"     ("true" "false"))
			("threshold")
			("thresholdtype" ("lines" "words"))
			("parthreshold"  ("true" "false"))
			("splitcomp"     ("true" "false"))
			("csdisplay"     ("true" "false"))
			("debug"         ("true" "false"))
			;; "babel" key is deprecated, replaced by "autostyle":
			;; ("babel" ("true" "false" "try" "once" "tryonce"))
			("version"       ("4.4" "3.6" "3.0")))))

;;; csquotes.el ends here
