;;; tex-site.el - Site specific variables.  Don't edit.

;; Copyright (C) 2005 Free Software Foundation, Inc.
;;
;; completely rewritten.

;; Author: David Kastrup <dak@gnu.org>
;; Maintainer: auctex-devel@gnu.org
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

;; This file contains startup code, autoloads and variables adapted to
;; the local site configuration.  It is generated and placed by the
;; installation procedure and should not be edited by hand, nor moved
;; to a different place, as some settings may be established relative
;; to the file.

;; All user customization should be done with
;; M-x customize-variable RET

;;; Code:

(if (< emacs-major-version 21)
  (error "AUCTeX requires Emacs 21 or later"))

(defvar TeX-lisp-directory
  (expand-file-name "auctex" (file-name-directory load-file-name))
  "The directory where most of the AUCTeX lisp files are located.
For the location of lisp files associated with
styles, see the variables TeX-style-* (hand-generated lisp) and
TeX-auto-* (automatically generated lisp).")

(add-to-list 'load-path TeX-lisp-directory)

(defvar TeX-data-directory
  (expand-file-name "auctex" (file-name-directory load-file-name))
  "The directory where the AUCTeX non-Lisp data is located.")

(defcustom TeX-auto-global
    (expand-file-name "var/auctex" (file-name-directory load-file-name))
  "*Directory containing automatically generated information.
Must end with a directory separator.

For storing automatic extracted information about the TeX macros
shared by all users of a site."
  :group 'TeX-file
  :type 'directory)

(defconst TeX-mode-alist
  '((tex-mode . tex-mode)
    (plain-tex-mode . tex-mode)
    (texinfo-mode . texinfo)
    (latex-mode . tex-mode)
    (doctex-mode . tex-mode))
  "Alist of built-in TeX modes and their load files.")

(defalias 'TeX-load-hack 'ignore)

(add-hook 'tex-site-unload-hook
	  (lambda ()
	    (let ((list after-load-alist))
	      (while list
		;; Adapted copy of the definition of `assq-delete-all'
		;; from Emacs 21 as substitute for
		;; `(assq-delete-all'TeX-modes-set (car list))' which
		;; fails on non-list elements in Emacs 21.
		(let* ((alist (car list))
		       (tail alist)
		       (key 'TeX-modes-set))
		  (while tail
		    (if (and (consp (car tail))
			     (eq (car (car tail)) key))
			(setq alist (delq (car tail) alist)))
		    (setq tail (cdr tail))))
		(setq list (cdr list))))
	    (setq load-path (delq TeX-lisp-directory load-path))))

(defun TeX-modes-set (var value &optional update)
  "Set VAR (which should be `TeX-modes') to VALUE.

This places either the standard or the AUCTeX versions of
functions into the respective function cell of the mode.
If UPDATE is set, a previously saved value for
the non-AUCTeX function gets overwritten with the current
definition."
  (custom-set-default var value)
  (let ((list TeX-mode-alist) elt)
    (while list
      (setq elt (car (pop list)))
      (when (or update (null (get elt 'tex-saved)))
	(when (fboundp elt)
	  (put elt 'tex-saved (symbol-function elt))))
      (defalias elt
	(if (memq elt value)
	    (intern (concat "TeX-" (symbol-name elt)))
	  (get elt 'tex-saved))))))

(defcustom TeX-modes
  (mapcar 'car TeX-mode-alist)
  "List of modes provided by AUCTeX.

This variable can't be set normally; use customize for that, or
set it with `TeX-modes-set'."
  :type (cons 'set
	      (mapcar (lambda(x) (list 'const (car x))) TeX-mode-alist))
  :set 'TeX-modes-set
  :group 'AUCTeX
  :initialize (lambda (var value)
		(custom-initialize-reset var value)
		(let ((list TeX-mode-alist))
		  (while list
		    (eval-after-load (cdar list)
		      `(TeX-modes-set ',var ,var t))
		    (setq list (cdr list))))))

(defconst AUCTeX-version "11.85"
    "AUCTeX version.
If not a regular release, the date of the last change.")

(defconst AUCTeX-date "2008-02-10"
  "AUCTeX release date using the ISO 8601 format, yyyy-mm-dd.")

;;; auto-loads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (turn-on-bib-cite bib-cite-minor-mode) "bib-cite"
;;;;;;  "bib-cite.el" (18341 54637))
;;; Generated autoloads from bib-cite.el

(autoload (quote bib-cite-minor-mode) "bib-cite" "\
Toggle bib-cite mode.
When bib-cite mode is enabled, citations, labels and refs are highlighted
when the mouse is over them.  Clicking on these highlights with [mouse-2]
runs bib-find, and [mouse-3] runs bib-display.

\(fn ARG)" t nil)

(autoload (quote turn-on-bib-cite) "bib-cite" "\
Unconditionally turn on Bib Cite mode.

\(fn)" nil nil)

;;;***

;;;### (autoloads (context-mode) "context" "context.el" (18341 54637))
;;; Generated autoloads from context.el

(defalias (quote ConTeXt-mode) (quote context-mode))

(autoload (quote context-mode) "context" "\
Major mode in AUCTeX for editing ConTeXt files.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of context-mode-hook.

\(fn)" t nil)

;;;***

;;;### (autoloads (context-en-mode) "context-en" "context-en.el"
;;;;;;  (18341 54637))
;;; Generated autoloads from context-en.el

(autoload (quote context-en-mode) "context-en" "\
Major mode for editing files for ConTeXt using its english interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook.

\(fn)" t nil)

;;;***

;;;### (autoloads (context-nl-mode) "context-nl" "context-nl.el"
;;;;;;  (18341 54637))
;;; Generated autoloads from context-nl.el

(autoload (quote context-nl-mode) "context-nl" "\
Major mode for editing files for ConTeXt using its dutch interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook.

\(fn)" t nil)

;;;***

;;;### (autoloads (font-latex-setup) "font-latex" "font-latex.el"
;;;;;;  (18344 55640))
;;; Generated autoloads from font-latex.el

(autoload (quote font-latex-setup) "font-latex" "\
Setup this buffer for LaTeX font-lock.  Usually called from a hook.

\(fn)" nil nil)

;;;***

;;;### (autoloads (docTeX-mode TeX-latex-mode BibTeX-auto-store)
;;;;;;  "latex" "latex.el" (18341 54637))
;;; Generated autoloads from latex.el

(autoload (quote BibTeX-auto-store) "latex" "\
This function should be called from `bibtex-mode-hook'.
It will setup BibTeX to store keys in an auto file.

\(fn)" nil nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.drv\\'" . latex-mode)))

(autoload (quote TeX-latex-mode) "latex" "\
Major mode in AUCTeX for editing LaTeX files.
See info under AUCTeX for full documentation.

Special commands:
\\{LaTeX-mode-map}

Entering LaTeX mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `LaTeX-mode-hook'.

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.dtx\\'" . doctex-mode)))

(autoload (quote docTeX-mode) "latex" "\
Major mode in AUCTeX for editing .dtx files derived from `LaTeX-mode'.
Runs `LaTeX-mode', sets a few variables and
runs the hooks in `docTeX-mode-hook'.

\(fn)" t nil)

(defalias (quote TeX-doctex-mode) (quote docTeX-mode))

;;;***

;;;### (autoloads (multi-prompt) "multi-prompt" "multi-prompt.el"
;;;;;;  (18341 54637))
;;; Generated autoloads from multi-prompt.el

(autoload (quote multi-prompt) "multi-prompt" "\
Completing prompt for a list of strings.  
The first argument SEPARATOR should be the string (of length 1) to
separate the elements in the list.  The second argument UNIQUE should
be non-nil, if each element must be unique.  The remaining elements
are the arguments to `completing-read'.  See that.

\(fn SEPARATOR UNIQUE PROMPT TABLE &optional MP-PREDICATE REQUIRE-MATCH INITIAL HISTORY)" nil nil)

;;;***

;;;### (autoloads (TeX-submit-bug-report ams-tex-mode TeX-auto-generate-global
;;;;;;  TeX-auto-generate TeX-plain-tex-mode TeX-tex-mode) "tex"
;;;;;;  "tex.el" (18349 63029))
;;; Generated autoloads from tex.el

(autoload (quote TeX-tex-mode) "tex" "\
Major mode in AUCTeX for editing TeX or LaTeX files.
Tries to guess whether this file is for plain TeX or LaTeX.

The algorithm is as follows:

   1) if the file is empty or `TeX-force-default-mode' is not set to nil,
      `TeX-default-mode' is chosen
   2) If \\documentstyle or \\begin{, \\section{, \\part{ or \\chapter{ is
      found, `latex-mode' is selected.
   3) Otherwise, use `plain-tex-mode'

\(fn)" t nil)

(autoload (quote TeX-plain-tex-mode) "tex" "\
Major mode in AUCTeX for editing plain TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{plain-TeX-mode-map}

Entering `plain-tex-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of plain-TeX-mode-hook.

\(fn)" t nil)

(autoload (quote TeX-auto-generate) "tex" "\
Generate style file for TEX and store it in AUTO.
If TEX is a directory, generate style files for all files in the directory.

\(fn TEX AUTO)" t nil)

(autoload (quote TeX-auto-generate-global) "tex" "\
Create global auto directory for global TeX macro definitions.

\(fn)" t nil)

(autoload (quote ams-tex-mode) "tex" "\
Major mode in AUCTeX for editing AmS-TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{AmSTeX-mode-map}

Entering AmS-tex-mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `AmS-TeX-mode-hook'.

\(fn)" t nil)

(autoload (quote TeX-submit-bug-report) "tex" "\
Submit a bug report on AUCTeX via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your AUCTeX version and AUCTeX configuration.

\(fn)" t nil)

;;;***

;;;### (autoloads (LaTeX-install-toolbar TeX-install-toolbar) "tex-bar"
;;;;;;  "tex-bar.el" (18348 43827))
;;; Generated autoloads from tex-bar.el

(autoload (quote TeX-install-toolbar) "tex-bar" "\
Install toolbar buttons for TeX mode.

\(fn)" t nil)

(autoload (quote LaTeX-install-toolbar) "tex-bar" "\
Install toolbar buttons for LaTeX mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "tex-fold" "tex-fold.el" (18341 54637))
;;; Generated autoloads from tex-fold.el
 (autoload 'TeX-fold-mode "tex-fold" "Minor mode for hiding and revealing macros and environments." t)

(defalias (quote tex-fold-mode) (quote TeX-fold-mode))

;;;***

;;;### (autoloads (tex-font-setup) "tex-font" "tex-font.el" (18341
;;;;;;  54636))
;;; Generated autoloads from tex-font.el

(autoload (quote tex-font-setup) "tex-font" "\
Setup font lock support for TeX.

\(fn)" nil nil)

;;;***

;;;### (autoloads (TeX-texinfo-mode) "tex-info" "tex-info.el" (18341
;;;;;;  54636))
;;; Generated autoloads from tex-info.el

(defalias (quote Texinfo-mode) (quote texinfo-mode))

(autoload (quote TeX-texinfo-mode) "tex-info" "\
Major mode in AUCTeX for editing Texinfo files.

Special commands:
\\{Texinfo-mode-map}

Entering Texinfo mode calls the value of `text-mode-hook'  and then the
value of `Texinfo-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads (japanese-latex-mode japanese-plain-tex-mode) "tex-jp"
;;;;;;  "tex-jp.el" (18341 54636))
;;; Generated autoloads from tex-jp.el

(autoload (quote japanese-plain-tex-mode) "tex-jp" "\
Major mode in AUCTeX for editing Japanese plain TeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-plain-tex-mode'.

\(fn)" t nil)

(autoload (quote japanese-latex-mode) "tex-jp" "\
Major mode in AUCTeX for editing Japanese LaTeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-latex-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads (texmathp-match-switch texmathp) "texmathp" "texmathp.el"
;;;;;;  (18341 54636))
;;; Generated autoloads from texmathp.el

(autoload (quote texmathp) "texmathp" "\
Determine if point is inside (La)TeX math mode.
Returns t or nil.  Additional info is placed into `texmathp-why'.
The functions assumes that you have (almost) syntactically correct (La)TeX in
the buffer.
See the variable `texmathp-tex-commands' about which commands are checked.

\(fn)" t nil)

(autoload (quote texmathp-match-switch) "texmathp" "\
Search backward for any of the math switches.
Limit searched to BOUND.

\(fn BOUND)" nil nil)

;;;***

;;;### (autoloads nil "toolbar-x" "toolbar-x.el" (18348 43827))
;;; Generated autoloads from toolbar-x.el
 (autoload 'toolbarx-install-toolbar "toolbar-x")

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; auto-loads.el ends here
(provide 'tex-site)
;;; tex-site.el ends here
