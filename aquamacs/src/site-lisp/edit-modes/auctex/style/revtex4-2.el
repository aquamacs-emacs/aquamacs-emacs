;;; revtex4-2.el --- AUCTeX style for `revtex4-2.cls' (v4.2c)

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2019-12-29
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

;; This file adds support for `revtex4-2.cls' (v4.2c) from 2019/01/18.
;; `revtex4-2.cls' is part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))
(defvar LaTeX-natbib-package-options)
(defvar LaTeX-url-package-options)

(defun LaTeX-env-revtex4-2-video (environment)
  "Create ENVIRONMENT with \\caption and \\label commands.
This function is meant to be used for \"video\" environment
provided by REVTeX class."
  (let* ((float (and LaTeX-float ; LaTeX-float can be nil, i.e. do not prompt
		     (TeX-read-string
		      (TeX-argument-prompt t nil "Float position")
		      LaTeX-float)))
	 (caption (TeX-read-string
		   (TeX-argument-prompt nil nil "Caption")))
	 (short-caption (when (>= (length caption) LaTeX-short-caption-prompt-length)
			  (TeX-read-string
			   (TeX-argument-prompt t nil "Short caption")))))
    (setq LaTeX-float float)
    (LaTeX-insert-environment environment
			      (unless (zerop (length float))
				(concat LaTeX-optop float LaTeX-optcl)))
    ;; Save the place where we've started:
    (save-excursion
      ;; Add a new line and add the \setfloatlink macro:
      (LaTeX-newline)
      (indent-according-to-mode)
      (TeX-insert-macro "setfloatlink")
      ;; Insert caption and ask for a label, do nothing if user skips
      ;; caption:
      (when (and caption (not (string= caption "")))
	(LaTeX-newline)
	(indent-according-to-mode)
	(insert (LaTeX-compose-caption-macro caption short-caption))
	;; If `auto-fill-mode' is active, fill the caption.
	(when auto-fill-function (LaTeX-fill-paragraph))
	(LaTeX-newline)
	(indent-according-to-mode)
	;; Ask for a label and indent only if it is inserted:
	(when (LaTeX-label environment 'environment)
	  (indent-according-to-mode))))))

(TeX-add-style-hook
 "revtex4-2"
 (lambda ()

   ;; Add standard stuff taken from `article.el':
   (LaTeX-largest-level-set "section")
   (LaTeX-add-counters "part" "section" "subsection" "subsubsection"
		       "paragraph" "subparagraph" "figure" "table")
   (LaTeX-add-environments "abstract")

   ;; Run style hooks for packages loaded by default:
   (TeX-run-style-hooks "url" "natbib")

   ;; Check for other class options and load AUCTeX style
   ;; respectively.  car of the cons is the REVTeX class option, cdr
   ;; the name of AUCTeX style:
   (let ((opt-style '(("amsfonts" . "amsfonts")
		      ("amsmath" . "amsmath")
		      ("linenumbers" . "lineno")))
	 (opt-cls (cdar LaTeX-provided-class-options)))
     (dolist (opt opt-style)
       (when (member (car opt) opt-cls)
	 (TeX-run-style-hooks (cdr opt)))))

   (TeX-add-symbols
    ;; IV.3. Specifying authors and affiliations
    '("affiliation" "Affliation")
    '("noaffiliation")
    ;; Because collaborations don't normally have affiiations, one
    ;; needs to follow the \collaboration with \noaffiliation:
    '("collaboration" "Collaboration"
      (TeX-arg-literal "\n")
      (TeX-arg-literal "\\noaffiliation"))
    '("email"    ["Text"] "E-Mail Address")
    '("homepage" ["Text"] "URL")
    '("altaffiliation" ["Text"] "Affliation")
    ;; Specifying first names and surnames
    '("surname" "Surname")

    ;; IV.6. Keywords
    '("keywords" t)

    ;; IV.7. Institutional report numbers
    '("preprint" 0)

    ;; V.3. One-column vs. two-column layouts
    '("onecolumngrid"  0)
    '("twocolumngrid"  0)

    ;; V.6. Appendices
    '("appendix*" 0)

    ;; VIII.1. Citing a reference
    '("onlinecite"
      (TeX-arg-conditional TeX-arg-cite-note-p ([LaTeX-arg-natbib-notes]) nil)
      TeX-arg-cite)
    '("textcite"
      (TeX-arg-conditional TeX-arg-cite-note-p ([LaTeX-arg-natbib-notes]) nil)
      TeX-arg-cite)

    ;; IX.2 video environment
    '("setfloatlink" "URL")

    ;; X.3. Dealing with Long Tables
    '("squeezetable" 0)

    ;; XI. Placement of Figures, Tables, and other floats
    '("printtables"   0)
    '("printtables*"  0)
    '("printfigures"  0)
    '("printfigures*" 0))

   ;; V.3. One-column vs. two-column layouts
   (LaTeX-add-environments
    '("widetext")
    '("acknowledgments")

    ;; IX.2. video environment
    '("video" LaTeX-env-revtex4-2-video)

    ;; X. Tables
    '("ruledtabular")

    ;; XII. Rotating Floats
    '("turnpage"))

   ;; Append entry for `video' to `LaTeX-label-alist':
   (add-to-list 'LaTeX-label-alist
		(cons "video" 'LaTeX-revtex4-2-video-label)
		t)

   ;; Tell RefTeX about `video' environment:
   (when (fboundp 'reftex-add-label-environments)
     (reftex-add-label-environments
      `(("video"
	 ,LaTeX-revtex4-2-video-reftex-quick-id-key
	 ,LaTeX-revtex4-2-video-label
	 "~\\ref{%s}" caption))))

   ;; X.3. Dealing with Long Tables
   (when (member "longtable" (TeX-style-list))
     (LaTeX-add-environments
      '("longtable*" LaTeX-env-longtable)))

   ;; Verbatim-like macros with braces as delimiters:
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "email")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "homepage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "setfloatlink")

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("fbox"          "*[{")
				("keywords"      "{")
				("preprint"      "")
				("onecolumngrid" "")
				("twocolumngrid" "")
				("squeezetable"  "")
				("printtables"   "*")
				("printfigures"  "*"))
			      'function)
     (font-latex-add-keywords '(("affiliation"    "{")
				("noaffiliation"  "")
				("collaboration"  "{")
				("altaffiliation" "[{"))
			      'textual)
     (font-latex-add-keywords '(("email"        "[")
				("homepage"     "[")
				("onlinecite"   "[[{")
				("textcite"     "[[{")
				("setfloatlink" ""))
			      'reference)
     (font-latex-add-keywords '(("appendix*" ""))
			      'warning)))
 LaTeX-dialect)

(defvar LaTeX-revtex4-2-class-options
  (progn
    (TeX-load-style "natbib")
    (TeX-load-style "url")
    (append
     LaTeX-natbib-package-options
     LaTeX-url-package-options
     '("aps"   "aip"   "aapm" "sor"
       "prl"   "pra"   "prb"  "prc" "prd" "pre"
       "prab"  "prper" "prx"  "prapplied" "prfluids"
       "prmaterials"   "physrev"    "rmp"
       "apl"   "bmf"   "cha"  "jap" "jcp" "jmp" "rse"
       "pof"   "pop"   "rsi"  "apm" "adv" "sd"
       "mph"   "jor"
       "10pt"  "11pt"  "12pt"
       "groupedaddress"  "superscriptaddress"
       "draft"           "linenumbers" "longbibliography"
       "amsfonts"        "noamsfonts"
       "amssymb"         "noamssymb"
       "amsmath"         "noamsmath"
       "preprintnumbers" "nopreprintnumbers"
       "floatfix"
       "bibnotes"        "nobibnotes"
       "footinbib"       "nofootinbib"
       "eprint"          "noeprint"
       "altaffilletter"  "altaffillsymbol"
       "unsortedaddress"
       "runinaddress"
       "showkeys"        "noshowkeyws"
       "tightenlines"
       "floats"
       "endfloats"
       "endfloats*"
       "titlepage"       "notitlepage"
       "final"
       "letterpaper"     "a4paper" "a5paper"
       "oneside"         "twoside"
       "fleqn"
       "eqsecnum"
       "balancelastpage" "nobalancelastpage"
       "raggedbottom"    "flushbottom"
       "raggedfooter"    "noraggedfooter"
       "byrevtex"
       "citeautoscript" "galley" "nomerge")))
  "Options for the revtex4-2 document class.")

;;; revtex4-2.el ends here
