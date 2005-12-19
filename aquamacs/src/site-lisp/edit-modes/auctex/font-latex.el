;;; font-latex.el --- LaTeX fontification for Font Lock mode.

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
;;   2004, 2005 Free Software Foundation.

;; Authors:    Peter S. Galbraith <psg@debian.org>
;;             Simon Marshall <Simon.Marshall@esrin.esa.it>
;; Maintainer: auctex-devel@gnu.org
;; Created:    06 July 1996
;; Keywords:   tex, wp, faces

;;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This package enhances font-lock fontification patterns for LaTeX.
;; font-lock mode is a minor mode that causes your comments to be
;; displayed in one face, strings in another, reserved words in
;; another, and so on.
;;
;; ** Infinite loops !? **
;; If you get an infinite loop, send a bug report!
;; Then set the following in your ~/.emacs file to keep on working:
;;   (setq font-latex-do-multi-line nil)

;;; Code:

(require 'font-lock)
(require 'tex)

(eval-when-compile
  (require 'cl))

(defgroup font-latex nil
  "Font-latex text highlighting package."
  :prefix "font-latex-"
  :group 'faces
  :group 'tex
  :group 'AUCTeX)

(defgroup font-latex-keywords nil
  "Keywords for highlighting text in font-latex."
  :prefix "font-latex-"
  :group 'font-latex)

(defgroup font-latex-highlighting-faces nil
  "Faces for highlighting text in font-latex."
  :prefix "font-latex-"
  :group 'font-latex)

(defcustom font-latex-do-multi-line 'try-font-lock
  "Control multi-line fontification.

font-latex has a built-in caching mechanism for fontification of
multi-line constructs.  Besides, Emacs provides its own facilities
for multi-line fontification which can be controlled by the
variable `font-lock-multiline'.

Setting `font-latex-do-multi-line' to t will enable font-latex's
mechanism, setting it to nil will disable it.  Setting it to
'try-font-lock will use font-lock's mechanism if available and
font-latex's method if not.

Setting this variable will only have effect after resetting
buffers controlled by font-latex or restarting Emacs."
  :group 'font-latex
  :type '(choice (const :tag "Enabled (font-lock or font-latex)" try-font-lock)
		 (const :tag "Enabled (force font-latex)" t)
		 (const :tag "Disabled" nil)))

(defvar font-latex-use-cache nil
  "Control cache for multi-line fontification.")
;; `font-lock-multiline' has to be made buffer-local.  Do the same
;; with `font-latex-use-cache'.  This way a change of
;; `font-latex-do-multi-line' will only have effect after restarting
;; Emacs or re-initializing the respective buffers, but there won't be
;; any inconsistencies.
(make-variable-buffer-local 'font-latex-use-cache)

(defvar font-latex-quote-regexp-beg nil
  "Regexp used to find quotes.")

(defvar font-latex-quote-list '(("``" "''") ("<<" ">>" french) ("«" "»" french))
  "List of quote specifiers for quotation fontification.

Each element of the list is either a list consisting of two
strings to be used as opening and closing quotation marks
independently of the value of `font-latex-quotes' or a list with
three elements where the first and second element are strings for
opening and closing quotation marks and the third element being
either the symbol 'german or 'french describing the order of
quotes.

If `font-latex-quotes' specifies a different state, order of the
added quotes will be reversed for fontification.  For example if
'(\"\\\"<\" \"\\\">\" french) is given but `font-latex-quotes'
specifies 'german, quotes will be used like \">foo\"< for
fontification.")

(defvar font-latex-quotes-control nil
  "Internal variable for keeping track if `font-latex-quotes' changed.")

(defcustom font-latex-quotes 'french
  "Whether to fontify << French quotes >> or >>German quotes<<.
Also selects \"<quote\"> versus \">quote\"<."
  :type '(choice (const french) (const german))
  :group 'font-latex)

(defun font-latex-add-quotes (quotes)
  "Add QUOTES to `font-latex-quote-list'.
QUOTES has to be a list adhering to the format of an element of
`font-latex-quote-list'."
  (set (make-local-variable 'font-latex-quotes-control) nil)
  (make-local-variable 'font-latex-quote-list)
  (add-to-list 'font-latex-quote-list quotes))

;; The definitions of the title faces were originally taken from
;; info.el (Copyright (C) 1985, 86, 92, 93, 94, 95, 96, 97, 98, 99,
;; 2000, 2001 Free Software Foundation, Inc.) and adapted to the needs
;; of font-latex.el.

(defconst font-latex-sectioning-max 5
  "Highest number for font-latex-sectioning-N-face")
(defface font-latex-sectioning-5-face
  (if (featurep 'xemacs)
      '((((type tty pc) (class color) (background light))
	 (:foreground "blue4" :bold t))
	(((type tty pc) (class color) (background dark))
	 (:foreground "yellow" :bold t))
	(((class color) (background light))
	 (:bold t :foreground "blue4" :family "helvetica"))
	(((class color) (background dark))
	 (:bold t :foreground "yellow" :family "helvetica"))
	(t (:bold t :family "helvetica")))
    '((((type tty pc) (class color) (background light))
       (:foreground "blue4" :weight bold))
      (((type tty pc) (class color) (background dark))
       (:foreground "yellow" :weight bold))
      (((class color) (background light))
       (:weight bold :inherit variable-pitch :foreground "blue4"))
      (((class color) (background dark))
       (:weight bold :inherit variable-pitch :foreground "yellow"))
      (t (:weight bold :inherit variable-pitch))))
  "Face for sectioning commands at level 5."
  :group 'font-latex-highlighting-faces)

(defun font-latex-update-sectioning-faces (&optional max height-scale)
  "Update sectioning commands faces."
  (unless height-scale
    (setq height-scale (if (numberp font-latex-fontify-sectioning)
			   font-latex-fontify-sectioning
			 1.1)))
  (unless max
    (setq max font-latex-sectioning-max))
  (dotimes (num max)
    (let* (;; reverse for XEmacs:
	   (num (- max (1+ num)))
	   (face-name (intern (format "font-latex-sectioning-%s-face" num))))
      (unless (get face-name 'saved-face) ; Do not touch customized faces.
	(if (featurep 'xemacs)
	    (let ((size
		   ;; Multiply with .9 because `face-height' returns a value
		   ;; slightly larger than the actual font size.
		   ;; `make-face-size' takes numeric points according to Aidan
		   ;; Kehoe in <16989.15536.613916.678965@parhasard.net> (not
		   ;; documented).
		   (round (* 0.9
			     (face-height 'default)
			     (expt height-scale (- max 1 num))))))
	      ;; (message "%s - %s" face-name size)
	      (make-face-size face-name size))
	  (set-face-attribute face-name nil :height  height-scale))))))

(defcustom font-latex-fontify-sectioning 1.1
  "Whether to fontify sectioning macros with varying height or a color face.

If it is a number, use varying height faces.  The number is used
for scaling starting from `font-latex-sectioning-5-face'.  Typically
values from 1.05 to 1.3 give best results, depending on your font
setup.  If it is the symbol `color', use `font-lock-type-face'.

Caveats: Customizing the scaling factor applies to all sectioning
faces unless those face have been saved by customize.  Setting
this variable directly does not take effect unless you call
`font-latex-update-sectioning-faces' or restart Emacs.

Switching from `color' to a number or vice versa does not take
effect unless you call \\[font-lock-fontify-buffer] or restart
Emacs."
  ;; Possibly add some words about XEmacs here. :-(
  :type '(choice (number :tag "Scale factor")
                 (const color))
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (unless (eq value 'color)
	   (font-latex-update-sectioning-faces font-latex-sectioning-max value)))
  :group 'font-latex)

(defun font-latex-make-sectioning-faces (max &optional height-scale)
  "Build the faces used to fontify sectioning commands."
  (unless max (setq max font-latex-sectioning-max))
  (unless height-scale
    (setq height-scale (if (numberp font-latex-fontify-sectioning)
			   font-latex-fontify-sectioning
			 1.1)))
  (dotimes (num max)
    (let* (;; reverse for XEmacs:
	   (num (- max (1+ num)))
	   (face-name (intern (format "font-latex-sectioning-%s-face" num)))
	   (f-inherit (intern (format "font-latex-sectioning-%s-face" (1+ num))))
	   (size (when (featurep 'xemacs)
		   (round (* 0.9 (face-height 'default)
			     (expt height-scale (- max 1 num)))))))
      (eval
       `(defface ,face-name
	  (if (featurep 'xemacs)
	      '((t (:size ,(format "%spt" size))))
	    '((t (:height ,height-scale :inherit ,f-inherit))))
	  (format "Face for sectioning commands at level %s.

Probably you don't want to customize this face directly.  Better
change the base face `font-latex-sectioning-5-face' or customize the
variable `font-latex-fontify-sectioning'." num)
	  :group 'font-latex-highlighting-faces))
      (when (and (featurep 'xemacs)
		 ;; Do not touch customized  faces.
		 (not (get face-name 'saved-face)))
	(set-face-parent face-name f-inherit)))))

(font-latex-make-sectioning-faces font-latex-sectioning-max)


;;; Keywords

(defvar font-latex-keywords-1 nil
  "Subdued level highlighting for LaTeX modes.")

(defvar font-latex-keywords-2 nil
  "High level highlighting for LaTeX modes.")

(defvar font-latex-built-in-keyword-classes
  '(("warning"
     ("nopagebreak" "pagebreak" "newpage" "clearpage"
      "cleardoublepage" "enlargethispage" "nolinebreak" "linebreak"
      "newline" "-" "\\" "\\*" "appendix" "displaybreak"
      "allowdisplaybreaks" "include")
     font-latex-warning-face 1 noarg)
    ("variable"
     ("setlength" "settowidth" "setcounter" "addtolength"
      "addtocounter")
     font-lock-variable-name-face 2 (command 2 nil))
    ("reference"
     ("nocite" "cite" "label" "pageref" "vref" "eqref" "ref"
      "include" "input" "bibliography" "index" "glossary"
      "footnote" "footnotemark" "footnotetext")
     font-lock-constant-face 2 (command 1 nil))
    ("function"
     ("begin" "end" "pagenumbering" "thispagestyle" "pagestyle"
      "nofiles" "includeonly" "bibliographystyle" "documentstyle"
      "documentclass" "newenvironment" "newcommand" "newlength"
      "newtheorem" "newcounter" "renewenvironment" "renewcommand"
      "renewlength" "renewtheorem" "renewcounter" "usepackage"
      "fbox" "mbox" "sbox" "vspace" "hspace" "thinspace"
      "negthinspace" "enspace" "enskip" "quad" "qquad" "nonumber"
      "centering" "TeX" "LaTeX")
     font-lock-function-name-face 2 (command 1 t))
    ("sectioning-0"
     ("part")
     font-latex-sectioning-0-face 2 (sectioning 1 t))
    ("sectioning-1"
     ("chapter")
     font-latex-sectioning-1-face 2 (sectioning 1 t))
    ("sectioning-2"
     ("section")
     font-latex-sectioning-2-face 2 (sectioning 1 t))
    ("sectioning-3"
     ("subsection")
     font-latex-sectioning-3-face 2 (sectioning 1 t))
    ("sectioning-4"
     ("subsubsection")
     font-latex-sectioning-4-face 2 (sectioning 1 t))
    ("sectioning-5"
     ("paragraph" "subparagraph" "subsubparagraph")
     font-latex-sectioning-5-face 2 (sectioning 1 t))
    ("slide-title" () font-latex-slide-title-face 2 (command 1 t))
    ("textual"
     ("item" "title" "author" "date" "thanks" "address" "caption"
      "textsuperscript")
     font-lock-type-face 2 (command 1 t))
    ("bold-command"
     ("textbf" "textsc" "textup" "boldsymbol" "pmb")
     font-latex-bold-face 1 (command 1 nil))
    ("italic-command"
     ("emph" "textit" "textsl")
     font-latex-italic-face 1 (command 1 nil))
    ("math-command"
     ("ensuremath")
     font-latex-math-face 1 (command 1 nil))
    ("type-command"
     ("texttt" "textsf" "textrm" "textmd")
     font-lock-type-face 1 (command 1 nil))
    ("bold-declaration"
     ("bf" "bfseries" "sc" "scshape" "upshape")
     font-latex-bold-face 1 declaration)
    ("italic-declaration"
     ("em" "it" "itshape" "sl" "slshape")
     font-latex-italic-face 1 declaration)
    ("type-declaration"
     ("tt" "ttfamily" "sf" "sffamily" "rm" "rmfamily" "mdseries"
      "tiny" "scriptsize" "footnotesize" "small" "normalsize"
      "large" "Large" "LARGE" "huge" "Huge")
     font-lock-type-face 1 declaration))
  "Built-in keywords and specifications for font locking.

The first element of each item is the name of the keyword class.

The second element is a list of keywords (macros without an
escape character) to highlight.

The third element is the face to be used.

The fourth element is the fontification level.

The fifth element is the type of construct to be matched.  It can
be one of 'noarg which will match simple macros without
arguments (like \"\\foo\"), 'declaration which will match macros
inside a TeX group (like \"{\\bfseries foo}\"), a list of the
form `(command <number of mandatory arguments> <flag determining
if trailing asterisk should be fontified>)' which will match
macros of the form \"\\foo[bar]{baz}\", or a list of the form
`(sectioning <num>)' which is basically the same as the `(command <num>)'
list but puts a conditional into the keyword highlighter which
tests for `font-latex-fontify-sectioning'.")

(defcustom font-latex-deactivated-keyword-classes nil
  "List of strings for built-in keyword classes to be deactivated.

Valid entries are \"warning\", \"variable\", \"reference\",
\"function\" , \"sectioning-0\", \"sectioning-1\", \"sectioning-2\",
\"sectioning-3\", \"sectioning-4\", \"sectioning-5\", \"textual\",
\"bold-command\", \"italic-command\", \"math-command\", \"type-command\",
\"bold-declaration\", \"italic-declaration\", \"type-declaration\".

You have to restart Emacs for a change of this variable to take effect."
  :group 'font-latex-keywords
  :type `(set ,@(mapcar
		 (lambda (spec)
		   `(const :tag ,(concat
				  ;; Name of the keyword class
				  (let ((name (split-string (car spec) "-")))
				    (setcar name (capitalize (car name)))
				    (mapconcat 'identity name " "))
				  " keywords in `"
				  ;; Name of the face
				  (symbol-name (nth 2 spec)) "'.\n"
				  ;; List of keywords
				  (with-temp-buffer
				    (insert "  Keywords: "
					    (mapconcat 'identity
						       (nth 1 spec) ", "))
				    (fill-paragraph nil)
				    (buffer-substring-no-properties
				     (point-min) (point-max))))
			   ,(car spec)))
		 font-latex-built-in-keyword-classes)))

(defun font-latex-make-match-defun (prefix name type)
  "Return a function definition for keyword matching.
The variable holding the keywords to match are determined by the
strings PREFIX and NAME.  The type of matcher is determined by
the symbol or list TYPE.

This is a helper function for `font-latex-make-built-in-keywords'
and `font-latex-make-user-keywords' and not intended for general
use."
  ;; Note: The functions are byte-compiled at the end of font-latex.el.
  ;; FIXME: Is the if-clause possible inside of the defun?
  (if (listp type) ; 'command and 'title are treated likewise
      (eval `(defun ,(intern (concat prefix name)) (limit)
	       ,(concat "Fontify `" prefix name "' up to LIMIT.

Generated by `font-latex-make-match-defun'.")
	       (when ,(intern (concat prefix name))
		 (font-latex-match-command-with-arguments
		  ,(intern (concat prefix name)) limit
		  ,(nth 1 type) ,(nth 2 type)))))
    (cond ((eq type 'declaration)
	   (eval `(defun ,(intern (concat prefix name)) (limit)
		    ,(concat "Fontify `" prefix name "' up to LIMIT.

Generated by `font-latex-make-match-defun'.")
		    (when ,(intern (concat prefix name))
		      (font-latex-match-command-in-braces
		       ,(intern (concat prefix name)) limit)))))
	  ((eq type 'noarg)
	   (eval `(defun ,(intern (concat prefix name)) (limit)
		    ,(concat "Fontify `" prefix name "' up to LIMIT.

Generated by `font-latex-make-match-defun'.")
		    (when ,(intern (concat prefix name))
		      (re-search-forward
		       ,(intern (concat prefix name)) limit t))))))))

(defun font-latex-keyword-matcher (prefix name face type)
  "Return a matcher and highlighter as required by `font-lock-keywords'.
PREFIX and NAME are strings which are concatenated to form the
respective match function.  FACE is a face name or a list of text
properties that will be applied to the respective part of the
match returned by the match function.  TYPE is the type of
construct to be highlighted.  Currently the symbols 'command,
'sectioning, 'declaration and 'noarg are valid.

This is a helper function for `font-latex-make-built-in-keywords'
and `font-latex-make-user-keywords' and not intended for general
use."
  (cond ((eq type 'command)
	 `(,(intern (concat prefix name))
	   (0 'font-lock-keyword-face append t)
	   (1 'font-lock-variable-name-face append t)
	   (2 ',face append t)))
	((eq type 'sectioning)
	 `(,(intern (concat prefix name))
	   (0 'font-lock-keyword-face append t)
	   (1 'font-lock-variable-name-face append t)
	   (2 (if (eq font-latex-fontify-sectioning 'color)
		  'font-lock-type-face
		',face)
	      append t)))
	((eq type 'noarg)
	 `(,(intern (concat prefix name))
	   (0 ',face)))
	((eq type 'declaration)
	 `(,(intern (concat prefix name))
	   (0 'font-lock-keyword-face append t)
	   (1 ',face append t)))))

(defun font-latex-make-built-in-keywords ()
  "Build defuns, defvars and defcustoms for built-in keyword fontification."
  (dolist (item font-latex-built-in-keyword-classes)
    (let ((prefix "font-latex-match-")
	  (name (nth 0 item))
	  (keywords (nth 1 item))
	  (face (nth 2 item))
	  (level (nth 3 item))
	  (type (nth 4 item)))

      ;; defvar font-latex-match-*-keywords-local
      (eval `(defvar ,(intern (concat prefix name "-keywords-local"))
	       ',keywords
	       ,(concat "Buffer-local keywords to add to `"
			prefix name "-keywords'.
This must be a list of keyword strings \(not regular expressions\) omitting
the leading backslash.  It will get transformed into a regexp using
`" prefix name "-make'.  This variable is not for end users; they
should customize `" prefix name "-keywords' instead.  It is for
authors of Lisp files that get loaded when LaTeX style files are used in the
current buffer.  They should add keywords to this list and rebuild the
fontification regexp like so:

 (add-to-list '" prefix name "-keywords-local \"setstuff\")
 (" prefix name "-make)

Generated by `font-latex-make-built-in-keywords'.")))
      (eval `(make-variable-buffer-local
	      ',(intern (concat prefix name "-keywords-local"))))

      ;; defun font-latex-match-*-make
      ;; Note: The functions are byte-compiled at the end of font-latex.el.
      (eval `(defun ,(intern (concat prefix name "-make")) ()
	       ,(concat "Make or remake the variable `" prefix name "'.

Generated by `font-latex-make-built-in-keywords'.")
	       (let ((keywords
		      (append
		       (unless (member ,name
				       font-latex-deactivated-keyword-classes)
			 ,(intern (concat prefix name "-keywords-local")))
		       ,(intern (concat prefix name "-keywords"))))
		     multi-char-macros single-char-macros)
		 (dolist (elt keywords)
		   (if (string-match "^[A-Za-z]" elt)
		       (add-to-list 'multi-char-macros elt)
		     (add-to-list 'single-char-macros elt)))
		 (when (or multi-char-macros single-char-macros)
		   (setq ,(intern (concat prefix name))
			 (concat
			  "\\\\\\("
			  (when multi-char-macros
			    (concat
			     "\\(?:" (regexp-opt multi-char-macros) "\\)\\>"))
			  (when single-char-macros
			    (concat
			     (when multi-char-macros "\\|")
			     "\\(?:" (regexp-opt single-char-macros) "\\)"))
			  "\\)"))))))

      ;; defcustom font-latex-match-*-keywords
      (eval `(defcustom ,(intern (concat prefix name "-keywords")) nil
	       ,(concat "List of keyword strings for " name " face.
Each string has to be the name of a macro omitting the leading backslash.

Setting this variable directly does not take effect;
restart Emacs.

Generated by `font-latex-make-built-in-keywords'.")
	       :type '(repeat (string :tag "Keyword"))
	       :set (lambda (symbol value)
		      (set-default symbol value)
		      (funcall ',(intern (concat prefix name "-make"))))
	       :group 'font-latex-keywords))

      ;; defvar font-latex-match-*
      (eval `(defvar ,(intern (concat prefix name))
	       ,(intern (concat prefix name "-keywords"))))
      (eval `(make-variable-buffer-local
	      ',(intern (concat prefix name))))

      ;; defun font-latex-match-*
      (font-latex-make-match-defun prefix name type)

      ;; Add matchers and highlighters to `font-latex-keywords-{1,2}'.
      (let ((keywords-entry (font-latex-keyword-matcher
			     prefix name face
			     (if (listp type) (car type) type))))
	(add-to-list (intern (concat "font-latex-keywords-"
				     (number-to-string level)))
		     keywords-entry t)
	(when (= level 1)
	  (add-to-list (intern (concat "font-latex-keywords-2"))
		       keywords-entry t))))))
(font-latex-make-built-in-keywords)

(defcustom font-latex-user-keyword-classes nil
  "User-defined keyword classes and specifications for font locking.

When adding new entries, you have to use unique values for the
class names, i.e. they must not clash with names of the built-in
keyword classes or other names given by you.  Additionally the
names must not contain spaces.

The keywords are names of commands you want to match omitting the
leading backslash.

The face argument can either be an existing face or font
specifications made by you.  (The latter option is not available
on XEmacs.)

There are three alternatives for the type of keywords:

\"Command with arguments\" comprises commands with the syntax
\"\\foo[bar]{baz}\".  The mandatory argument in curly braces will
get the face you specified.

\"Declaration inside TeX group\" comprises commands with the
syntax \"{\\foo bar}\".  The content inside the braces, excluding
the command will get the face you specified.  In case the braces
are missing, the face will be applied to the command itself.

\"Command without arguments\" comprises commands with the syntax
\"\\foo\".  The command itself will get the face you specified.

Setting this variable directly does not take effect;
use \\[customize]."
  :group 'font-latex-keywords
  :type `(repeat (list (string :tag "Name")
		       (repeat :tag "Keywords" (string :tag "Keyword"))
		       ,(if (featurep 'xemacs)
			    '(face :tag "Face name")
			  '(choice (custom-face-edit :tag "Face attributes")
				   (face :tag "Face name")))
		       (choice :tag "Type"
			       ;; Maps to
			       ;;`font-latex-match-command-with-arguments'
			       (list :tag "Command with arguments"
				     :value (command 1)
				     (const command)
				     (integer :tag "Number of arguments"))
			       ;; Maps to
			       ;;`font-latex-match-command-in-braces'
			       (const :tag "Declaration inside TeX group"
				      declaration)
			       ;; Maps to `re-search-forward'
			       (const :tag "Command without arguments"
				      noarg))))
  :set (lambda (symbol value)
	 (dolist (item value)
	   (when (string-match " " (car item))
	     (error "No spaces allowed in name"))
	   (when (and (listp (nth 3 item))
		      (< (cadr (nth 3 item)) 1))
	     (error "Number of arguments has to be greater than 0")))
	 (let (names names-uniq)
	   (dolist (item (append font-latex-built-in-keyword-classes value))
	     (setq names (append names (list (car item)))))
	   (setq names (TeX-sort-strings names))
	   (setq names-uniq (TeX-delete-duplicate-strings names))
	   (dotimes (i (safe-length names-uniq))
	     (unless (string= (nth i names) (nth i names-uniq))
	       (error "Name %S already exists" (nth i names)))))
	 (set-default symbol value)
	 (let ((prefix "font-latex-match-"))
	   (dolist (elt value)
	     (unless (boundp (intern (concat prefix (car elt))))
	       ;; defvar font-latex-match-*
	       (eval `(defvar ,(intern (concat prefix (car elt))) nil)))
	     (set (intern (concat prefix (car elt)))
		  (when (and (listp (nth 1 elt))
			     (> (safe-length (nth 1 elt)) 0))
		    (concat "\\\\" (let ((max-specpdl-size 1000))
				     (regexp-opt (nth 1 elt) t)))))))))

(defun font-latex-make-user-keywords ()
  "Build defuns and defvars for user keyword fontification."
  (let ((keyword-specs font-latex-user-keyword-classes))
    (dolist (item keyword-specs)
      (let ((prefix "font-latex-match-")
	    (name (nth 0 item))
	    (keywords (nth 1 item))
	    (face (nth 2 item))
	    (type (nth 3 item)))

	;; defvar font-latex-match-*-keywords
	(eval `(defvar ,(intern (concat prefix name "-keywords")) ',keywords
		 ,(concat "Font-latex keywords for " name " face.

Generated by `font-latex-make-user-keywords'.")))

	;; defun font-latex-match-*
	(font-latex-make-match-defun prefix name type)

	;; Add the matcher to `font-latex-keywords-2'.
	(add-to-list 'font-latex-keywords-2
		     (font-latex-keyword-matcher prefix name face
		      (if (listp type) (car type) type)) t))))

  ;; Add the "fixed" matchers and highlighters.
  (dolist (item
	   '(("\\(^\\|[^\\]\\)\\(&+\\)" 2 'font-latex-warning-face)
	     ("\\$\\$\\([^$]+\\)\\$\\$" 1 'font-latex-math-face)
	     (font-latex-match-quotation (0 'font-latex-string-face append))
	     ;; Hack to remove the verbatim face from the \ in
	     ;; \end{verbatim} and similar.  The same hack is used in
	     ;; tex-mode.el.
	     ("^[ \t]*\\(\\\\\\)end"
	      (1 (get-text-property (match-end 1) 'face) t))))
    (add-to-list 'font-latex-keywords-1 item)
    (add-to-list 'font-latex-keywords-2 item))
  (dolist (item 
	   '((font-latex-match-math-env
	      (0 'font-latex-math-face append t))
	     (font-latex-match-math-envII
	      (0 'font-latex-math-face append t))
	     (font-latex-match-simple-command
	      (0 'font-latex-sedate-face append))
	     (font-latex-match-script
	      (1 (font-latex-script (match-beginning 0)) append))))
    (add-to-list 'font-latex-keywords-2 item t)))
(font-latex-make-user-keywords)

(defvar font-latex-keywords font-latex-keywords-1
  "Default expressions to highlight in TeX mode.")


;;; Subscript and superscript

(defcustom font-latex-fontify-script (not (featurep 'xemacs))
  "If non-nil, fontify subscript and superscript strings.
This feature does not work in XEmacs."
  :type 'boolean
  :group 'font-latex)

(defcustom font-latex-script-display '((raise -0.3) . (raise 0.3))
  "Display specification for subscript and superscript content.
The car is used for subscript, the cdr is used for superscripts."
  :group 'font-latex
  :type '(cons (choice (sexp :tag "Subscript form")
		       (const :tag "No lowering" nil))
	       (choice (sexp :tag "Superscript form")
		       (const :tag "No raising" nil))))


;;; Syntactic keywords

(defun font-latex-set-syntactic-keywords ()
  "Set the variable `font-latex-syntactic-keywords'.
This function can be used to refresh the variable in case other
variables influencing its value, like `LaTeX-verbatim-environments',
have changed."
  ;; Checks for non-emptiness of lists added in order to cater for
  ;; installations where `(regexp-opt-group nil)' would enter a loop.
  (let ((verb-envs (append (and (boundp 'LaTeX-verbatim-environments)
				LaTeX-verbatim-environments)
			   (and (boundp 'LaTeX-verbatim-environments-local)
				LaTeX-verbatim-environments-local)))
	(verb-macros-with-delims
	 (append (and (boundp 'LaTeX-verbatim-macros-with-delims)
		      LaTeX-verbatim-macros-with-delims)
		 (and (boundp 'LaTeX-verbatim-macros-with-delims-local)
		      LaTeX-verbatim-macros-with-delims-local)))
	(verb-macros-with-braces
	 (append (and (boundp 'LaTeX-verbatim-macros-with-braces)
		      LaTeX-verbatim-macros-with-braces)
		 (and (boundp 'LaTeX-verbatim-macros-with-braces-local)
		      LaTeX-verbatim-macros-with-braces-local))))
    (setq verb-envs (and verb-envs (regexp-opt verb-envs))
	  verb-macros-with-delims (and verb-macros-with-delims
				       (regexp-opt verb-macros-with-delims))
	  verb-macros-with-braces (and verb-macros-with-braces
				       (regexp-opt verb-macros-with-braces))
	  font-latex-syntactic-keywords nil)
    (unless (= (length verb-envs) 0)
      (add-to-list 'font-latex-syntactic-keywords
		   `(,(concat "^[ \t]*\\\\begin *{\\(?:" verb-envs
			      "\\)}.*\\(\n\\)")
		     (1 "|" t)))
      (add-to-list 'font-latex-syntactic-keywords
		   ;; Using the newline character for the syntax
		   ;; property often resulted in fontification
		   ;; problems when text was inserted at the end of
		   ;; the verbatim environment.  That's why we now use
		   ;; the starting backslash of \end.  There is a hack
		   ;; in `font-latex-make-user-keywords' to remove the
		   ;; spurious fontification of the backslash.
		   `(,(concat "^[ \t]*\\(\\\\\\)end *{\\(?:" verb-envs "\\)}")
		     (1 "|" t))))
    (unless (= (length verb-macros-with-delims) 0)
      (add-to-list 'font-latex-syntactic-keywords
		   `(,(concat "\\\\\\(?:" verb-macros-with-delims "\\)"
			      ;; An opening curly brace as delimiter
			      ;; is valid, but allowing it might screw
			      ;; up fontification of stuff like
			      ;; "\url{...} foo \textbf{<--!...}".
			      "\\([^a-z@*\n\f{]\\).*?"
			      ;; Give an escape char at the end of the
			      ;; verbatim construct punctuation syntax.
			      ;; Prevents wrong fontification of stuff
			      ;; like "\verb|foo\|".
			      "\\(" (regexp-quote TeX-esc) "*\\)\\(\\1\\)")
		     (1 "\"") (2 ".") (3 "\""))))
    (unless (= (length verb-macros-with-braces) 0)
      (add-to-list 'font-latex-syntactic-keywords
		   `(,(concat "\\\\\\(?:" verb-macros-with-braces "\\)"
			      "\\({\\).*?[^\\]\\(?:\\\\\\\\\\)*\\(}\\)")
		     (1 "|") (2 "|")))))
  ;; Cater for docTeX mode.
  (setq font-latex-doctex-syntactic-keywords
	(append font-latex-syntactic-keywords
		;; For docTeX comment-in-doc.
		`(("\\(\\^\\)\\^A" (1 (font-latex-doctex-^^A)))))))

(defvar font-latex-syntactic-keywords nil
  "Syntactic keywords used by `font-latex'.")
(make-variable-buffer-local 'font-latex-syntactic-keywords)


;;; Syntactic fontification

;; Copy and adaptation of `tex-font-lock-syntactic-face-function' in
;; `tex-mode.el' of CVS Emacs (March 2004)
(defun font-latex-syntactic-face-function (state)
  (let ((char (nth 3 state)))
    (cond
     ((not char) 'font-lock-comment-face)
     ((eq char ?$) 'font-latex-math-face)
     (t
      (when (char-valid-p char)
	;; This is a \verb?...? construct.  Let's find the end and mark it.
	(save-excursion
	  (skip-chars-forward (string ?^ char)) ;; Use `end' ?
	  (when (eq (char-syntax (preceding-char)) ?/)
	    (put-text-property (1- (point)) (point) 'syntax-table '(1)))
	  (unless (eobp)
	    (put-text-property (point) (1+ (point)) 'syntax-table '(7)))))
      'font-latex-verbatim-face))))


;;; Faces

(defface font-latex-bold-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
		    ((assq :weight custom-face-attributes) '(:weight bold))
		    (t '(:bold t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in bold."
  :group 'font-latex-highlighting-faces)

(defface font-latex-italic-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
		    ((assq :slant custom-face-attributes) '(:slant italic))
		    (t '(:italic t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in italic."
  :group 'font-latex-highlighting-faces)

(defface font-latex-math-face
  (let ((font (cond ((assq :inherit custom-face-attributes)
		     '(:inherit underline))
		    (t '(:underline t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown"))
      (((class color) (background dark))
       (:foreground "burlywood"))
      (t (,@font))))
  "Face used to highlight math."
  :group 'font-latex-highlighting-faces)

(defface font-latex-sedate-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark))  (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "DimGray"))
    (((class color) (background dark))  (:foreground "LightGray"))
   ;;;(t (:underline t))
    )
  "Face used to highlight sedate stuff."
  :group 'font-latex-highlighting-faces)

(defface font-latex-string-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
		    ((assq :slant custom-face-attributes) '(:slant italic))
		    (t '(:italic t)))))
    `((((type tty) (class color))
       (:foreground "green"))
      (((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "RosyBrown"))
      (((class color) (background dark))
       (:foreground "LightSalmon"))
      (t (,@font))))
  "Face used to highlight strings."
  :group 'font-latex-highlighting-faces)

(defface font-latex-warning-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
		    ((assq :weight custom-face-attributes) '(:weight bold))
		    (t '(:bold t)))))
    `((((class grayscale)(background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale)(background dark))
       (:foreground "LightGray" ,@font))
      (((class color)(background light))
       (:foreground "red" ,@font))
      (((class color)(background dark))
       (:foreground "red" ,@font))
      (t (,@font))))
  "Face for important keywords."
  :group 'font-latex-highlighting-faces)

(defface font-latex-verbatim-face
  (let ((font (if (and (assq :inherit custom-face-attributes)
		       (if (featurep 'xemacs)
			   (find-face 'fixed-pitch)
			 (facep 'fixed-pitch)))
		  '(:inherit fixed-pitch)
		'(:family "courier"))))
    `((((class grayscale) (background light))
	 (:foreground "DimGray" ,@font))
	(((class grayscale) (background dark))
	 (:foreground "LightGray" ,@font))
	(((class color) (background light))
	 (:foreground "SaddleBrown" ,@font))
	(((class color) (background dark))
	 (:foreground "burlywood" ,@font))
	(t (,@font))))
  "Face used to highlight TeX verbatim environments."
  :group 'font-latex-highlighting-faces)

(defface font-latex-superscript-face
  '((t (:height 0.8)))
  "Face used for superscripts."
  :group 'font-latex-highlighting-faces)

(defface font-latex-subscript-face
  '((t (:height 0.8)))
  "Face used for subscripts."
  :group 'font-latex-highlighting-faces)

(defface font-latex-slide-title-face
  (let* ((scale 1.2)
	 (size (when (featurep 'xemacs)
		 (round (* 0.9 (face-height 'default) scale)))))
    (if (featurep 'xemacs)
	`((t (:bold t :family "helvetica" :size ,size)))
      `((t (:inherit (variable-pitch font-lock-type-face)
		     :weight bold :height ,scale)))))
  "Face for slide titles."
  :group 'font-latex-highlighting-faces)
(when (featurep 'xemacs)
  (set-face-parent 'font-latex-slide-title-face 'font-lock-type-face
		   nil nil 'append))


;;; Setup

(defvar font-lock-comment-start-regexp nil
  "Regexp to match the start of a comment.")

;;;###autoload
(defun font-latex-setup ()
  "Setup this buffer for LaTeX font-lock.  Usually called from a hook."
  (font-latex-set-syntactic-keywords)
  ;; Trickery to make $$ fontification be in `font-latex-math-face' while
  ;; strings get whatever `font-lock-string-face' has been set to.
  (when (fboundp 'built-in-face-specifiers)
    ;; Cool patch from Christoph Wedler...
    (let (instance)
      (mapcar (lambda (property)
		(setq instance
		      (face-property-instance 'font-latex-math-face property
					      nil 0 t))
		(if (numberp instance)
		    (setq instance
			  (face-property-instance 'default property nil 0)))
		(or (numberp instance)
		    (set-face-property 'font-lock-string-face property
				       instance (current-buffer))))
	      (built-in-face-specifiers))))

  ;; Configure multi-line fontification.
  (cond ((eq font-latex-do-multi-line 'try-font-lock)
	 (if (boundp 'font-lock-multiline)
	     (set (make-local-variable 'font-lock-multiline) t)
	   (setq font-latex-use-cache t)))
	((eq font-latex-do-multi-line t)
	 (setq font-latex-use-cache t)))

  ;; Tell Font Lock about the support.
  (make-local-variable 'font-lock-defaults)
  ;; The test for `major-mode' currently only works with docTeX mode
  ;; because `TeX-install-font-lock' is called explicitely in
  ;; `doctex-mode'.  In case other modes have to be distinguished as
  ;; well, remove the call to `TeX-install-font-lock' from
  ;; `VirTeX-common-initialization' and place it in the different
  ;; `xxx-mode' calls instead, but _after_ `major-mode' is set.
  (cond
   ((eq major-mode 'doctex-mode)
    (setq font-lock-defaults
          '((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2
				 font-latex-doctex-keywords)
            nil nil ((?\( . ".") (?\) . ".") (?$ . "\"")) nil
            (font-lock-comment-start-regexp . "%")
            (font-lock-mark-block-function . mark-paragraph)
	    (font-lock-unfontify-region-function
	     . font-latex-unfontify-region)
            (font-lock-syntactic-face-function
             . font-latex-doctex-syntactic-face-function)
            (font-lock-syntactic-keywords
             . font-latex-doctex-syntactic-keywords))))
   (t
    (setq font-lock-defaults
          '((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2)
            nil nil ((?\( . ".") (?\) . ".") (?$ . "\"")) nil
            (font-lock-comment-start-regexp . "%")
            (font-lock-mark-block-function . mark-paragraph)
	    (font-lock-unfontify-region-function
	     . font-latex-unfontify-region)
            (font-lock-syntactic-face-function
             . font-latex-syntactic-face-function)
            (font-lock-syntactic-keywords
             . font-latex-syntactic-keywords))))))

;; Copy and adaption of `tex-font-lock-unfontify-region' from
;; tex-mode.el in GNU Emacs on 2004-08-04.
(defun font-latex-unfontify-region (beg end)
  "Unfontify region from BEG to END."
  (font-lock-default-unfontify-region beg end)
  (while (< beg end)
    (let ((next (next-single-property-change beg 'display nil end))
	  (prop (get-text-property beg 'display)))
      (if (and (eq (car-safe prop) 'raise)
	       (member (car-safe (cdr prop))
		       (list (nth 1 (car font-latex-script-display))
			     (nth 1 (cdr font-latex-script-display))))
	       (null (cddr prop)))
	  (put-text-property beg next 'display nil))
      (setq beg next))))


;;; Utility functions

(defun font-latex-find-matching-close (openchar closechar)
  "Skip over matching pairs of { } or [ ], ignoring comments.
OPENCHAR is the opening character and CLOSECHAR is the closing character."
  (let ((parse-sexp-ignore-comments
	 (not (eq major-mode 'doctex-mode))) ; scan-sexps ignores comments
        (init-point (point))
	(mycount 1)
	(esc-char (or (and (boundp 'TeX-esc) TeX-esc) "\\")))
    (or
     (condition-case nil
	 (progn
	   (goto-char (scan-sexps (point) 1))
	   ;; No error code.  See if closechar is unquoted
	   (save-excursion
	     (backward-char 1)
	     (zerop (mod (skip-chars-backward (regexp-quote esc-char)) 2))))
       (error nil))
     (save-match-data
       (goto-char (1+ init-point))
       (while (and (> mycount 0)
		   (re-search-forward
		    (string ?\[
			    ;; closechar might be ]
			    ;; and therefor must be first in regexp
			    closechar openchar
			    ?\])
		    nil t))
	 (cond
	  ((font-latex-commented-outp)
	   (forward-line 1))
	  ((save-excursion
	     (backward-char 1)
	     (zerop (mod (skip-chars-backward (regexp-quote esc-char))
			 2)))
	   (setq mycount (+ mycount
			    (if (= (preceding-char) openchar) 1 -1)))))))
     (if (= mycount 0)
	 t
       (goto-char init-point)
       nil))))

(defun font-latex-commented-outp ()
  "Return t if comment character is found between bol and point."
  (save-excursion
    (let ((limit (point))
	  (esc-char (if (and (boundp 'TeX-esc) TeX-esc) TeX-esc "\\")))
      (forward-line 0)
      (if (and (eq (char-after) ?\%)
	       (not (font-latex-faces-present-p 'font-latex-verbatim-face)))
	  (not (eq major-mode 'doctex-mode))
	(catch 'found
	  (while (progn (skip-chars-forward "^%" limit)
			(< (point) limit))
	    (when (and (save-excursion
			 (zerop (mod (skip-chars-backward
				      (regexp-quote esc-char)) 2)))
		       (not (font-latex-faces-present-p
			     'font-latex-verbatim-face)))
	      (throw 'found t))
	    (forward-char)))))))

(defun font-latex-faces-present-p (faces &optional pos)
  "Return t if FACES are present at position POS.
FACES may be a single face or a list of faces.
If POS is omitted, the current position of point is used."
  (let* ((faces (if (listp faces) faces (list faces)))
	 (pos (or pos (point)))
	 (prop (get-text-property pos 'face))
	 (prop-list (if (listp prop) prop (list prop))))
    (catch 'member
      (dolist (item prop-list)
	(when (memq item faces)
	  (throw 'member t))))))

(defun font-latex-not-on-same-line-as (cache-start)
  "Return t if point is not on same line as CACHE-START."
  (save-excursion
    (not (= (progn (beginning-of-line) (point))
            (progn (goto-char cache-start) (beginning-of-line) (point))))))

(defun font-latex-forward-comment ()
  "Like `forward-comment' but with special provisions for docTeX mode.
In docTeX mode \"%\" at the start of a line will be treated as whitespace."
  (if (eq major-mode 'doctex-mode)
      ;; XXX: We should probably cater for ^^A as well.
      (progn
	(while (progn (if (bolp) (skip-chars-forward "%"))
		      (> (skip-chars-forward " \t\n") 0)))
	(when (eq (char-after) ?%)
	  (beginning-of-line 2)
	  t))
    (forward-comment 1)))


;;;;------------------
;;;; Cache Method:
;;;
;;; This works:
;;;
;;; (defun font-latex-set-cache (cache-id)
;;;   (let ((cache (intern cache-id)))
;;;     (set cache (list (point) (point-max)))))
;;; (defun font-latex-get-cache (cache-id item)
;;;   (let ((cache (intern cache-id)))
;;;     (nth item (symbol-value cache))))
;;; (font-latex-set-cache "font-latex-match-command-cache")
;;; (font-latex-get-cache "font-latex-match-command-cache" 1)
;;;
;;; but let's use symbols instead:

;;; Hacker's note: I haven't tested extensively using lazy-lock, which
;;; apparently fontifies the entire visble page instead of just the current
;;; line.  This could actually be slower than not using lazy-lock using the
;;; current code.  Perhaps there's an opportunity to take advantage of
;;; lazy-lock with alternate coding.

;;; Hacker's note: If this method leads to infinite loops again, I could
;;; change the cache method to something like:
;;;  - When the pattern is un-finished, simply store the limit in the cache.
;;;    and the regexp to match the termination.
;;;  - When checking the cache, check to see if we're at the limit, and if
;;;    so fontify the text directly like at point limit-1 (instead of
;;;    letting font-lock itself set the font!) until either the regexp match
;;;    is found or set another cache at the new limit
;;;  - the scheme must allow a newline to be correctly fontified, and well
;;;    as new characters on the same line as the first cache.  (How?)

;;; Hacker's note (2001-11-02) : It's possible that the caching system is
;;; no longer needed using font-lock-multiline in Emacs21.  I should
;;; disable it and try.  Also, now that I look at this, I wonder why I
;;; didn't use text-properties to be able to set many unterminated
;;; fontification matches in a given buffer.  Perhaps it was portability to
;;; XEmacs?

(defun font-latex-set-cache (cache-id kbeg kend limit keywords match-list)
  "Set cache for font-latex.
Caches the following info into CACHE-ID:
KBEG and KEND: beginning and end points of the LaTeX keyword (e.g. \"section\")
LIMIT:         up to where fontification is done.
KEYWORDS:      the font-lock regexp that initiated the cache.
MATCH-LIST:    the match list that was returned to font-lock

The INITIAL POINT from which we last moved is stored in the same cache, but
it's done elsewhere.  We will never fontify the same MATCH LIST twice in a
row from same INITIAL POINT."
;debug  (message "Setting cache!")
  (let ((ini-point (nth 5 (symbol-value cache-id)))
        (oldlimit (nth 6 (symbol-value cache-id))))
    (set cache-id
         (list kbeg kend limit keywords match-list ini-point oldlimit))))

(defun font-latex-get-cache (cache-id item)
  "Retrieve info from cache in symbol CACHE-ID.
Then ITEMs are:
 0: kbegin
 1: kend
 2: limit
 3: keywords
 4: match-list from last succesful cache
 5: initial point from which we last moved
 6: limit when we last moved"
  (let ((cache (symbol-value cache-id)))
    (nth item cache)))

(defun font-latex-check-cache (cache-id keywords limit)
  "Check that current parameters are consistent with cache to move point.
If we move point, alter the last entry in the cache to indicate from where
we moved and the current limit.
Return t if we move, false if we don't."
  (let ((the-point (point))
        (kbeg (font-latex-get-cache cache-id 0))
        (inip (or (font-latex-get-cache cache-id 5) 0))
        (oldlimit (or (font-latex-get-cache cache-id 6) 0)))
    (when
        (and
         font-latex-use-cache
         kbeg                           ;; Check that cache is actually set
         (equal keywords (font-latex-get-cache cache-id 3))
;debug   (message "1- cache: %s" (symbol-name cache-id))
;debug   (message "1- keywords are the same; next compare point %s to %s"
;debug            the-point (font-latex-get-cache cache-id 1))
         (not (= the-point (font-latex-get-cache cache-id 1)))
;debug   (message "2- Not on end of keyword %s != %s; next after kbeg %s"
;debug            the-point (font-latex-get-cache cache-id 1) kbeg)
         (< kbeg the-point)
;debug   (message "3- After beginning of keyword at %s; next within limit %s"
;debug            kbeg (font-latex-get-cache cache-id 2))
         (<= the-point (font-latex-get-cache cache-id 2))
;debug   (message "4- Within limit at %s" (font-latex-get-cache cache-id 2))
;debug   (message "5- Same limit as last time?: %s vs %s  Point greater? %s > %s"
;debug            limit oldlimit the-point inip)
         (or (< the-point inip) (not (= limit oldlimit)))
;debug   (message "6- Is %s on same line as %s?" the-point kbeg)
         (font-latex-not-on-same-line-as kbeg))
;debug   (message "7- moving from %s to %s!" the-point kbeg)
      (goto-char kbeg)
      (let* ((cache (symbol-value cache-id))
             (e0 kbeg)
             (e1 (nth 1 cache))
             (e2 (nth 2 cache))
             (e3 (nth 3 cache))
             (e4 (nth 4 cache)))
        (set cache-id (list e0 e1 e2 e3 e4 the-point limit)))
      t)))


;;; Match functions

(defvar font-latex-match-command-cache nil
  "Cache for font-latex-match-command.")
(make-variable-buffer-local 'font-latex-match-command-cache)

;; FIXME - Note to myself
;; In call to font-latex-match-command-with-arguments, I could arrange
;; such that keywords which cannot use [options] have this set to nil.
;; LaTeX code wouldn't fontify if options are used illegally in commands,
;; cuing users in that they are doing something wrong.  (See RCS V1.11 for
;; useopt option)
;;
;; NOTE - Without an override flag, font-lock does not re-fontify the
;;  option `opt' when the `t' is typed-in in "\cite[opt".  The first `o'
;;  was fontified and now has a face, which font-lock-apply-highlight
;;  won't override.  The `p' and `t' get a face as they are typed by
;;  inheriting from left-stickyness on the `o'.
;;  THEREFORE, I cannot rely on font-lock-apply-highlight to continue
;;  multi-line incomplete patterns, because the first character of the
;;  pattern on the first line has a face.  I must use `prepend'.
(defun font-latex-match-command-with-arguments (keywords limit arg-count
							 asterisk)
  "Search for regexp command KEYWORDS[opt]{arg} before LIMIT.
The integer ARG-COUNT specifies the number of mandatory arguments
in curly braces.
If ASTERISK is t, fontify trailing asterisk in command.
Sets `match-data' so that:
 subexpression 0 is the keyword,
 subexpression 1 is the contents of any following [...] forms
 subexpression 2 is the contents of any following {...} forms.
Returns nil if none of KEYWORDS is found."
;;(let ((we-moved (font-latex-check-cache
;;                 'font-latex-match-command-cache keywords limit)))
  (when font-latex-use-cache
    (font-latex-check-cache 'font-latex-match-command-cache keywords limit))
  (catch 'match
    (while (re-search-forward keywords limit t)
      (unless (font-latex-faces-present-p '(font-lock-comment-face
					    font-latex-verbatim-face)
					  (match-beginning 0))
	(let ((kbeg (match-beginning 0))
	      kend sbeg send cbeg cend
	      cache-reset opt-arg
	      (parse-sexp-ignore-comments t)) ; scan-sexps ignores comments
	  (save-restriction
	    ;; Restrict to LIMIT.
	    (narrow-to-region (point-min) limit)
	    (goto-char (match-end 0))
	    (if (and asterisk (eq (following-char) ?\*))
		(forward-char 1))
	    (setq kend (point))
	    (while (and (not (eobp)) (font-latex-forward-comment)))
	    ;; Optional arguments [...]
	    (while (eq (following-char) ?\[)
	      (unless opt-arg (setq sbeg (point)) (setq opt-arg t))
	      (if (font-latex-find-matching-close ?\[ ?\])
		  (progn
		    (setq send (point))
		    (while (and (not (eobp)) (font-latex-forward-comment))))
		(setq cache-reset t)
		(setq send (point-max))
		(goto-char send)))
	    ;; Mandatory arguments {...}
	    (catch 'runaway
	      (dotimes (i arg-count)
		(when (eq (following-char) ?\{)
		  (when (= i 0) (setq cbeg (point)))
		  (if (font-latex-find-matching-close ?\{ ?\})
		      (progn
			(setq cend (point))
			(while (and (not (eobp)) (font-latex-forward-comment))))
		    (setq cache-reset t)
		    (setq cend (point-max))
		    (goto-char cend)
		    (throw 'runaway nil))))))
	  (store-match-data (list kbeg kend sbeg send cbeg cend))

          ;; Handle cache
;          (if (and we-moved
;                   (equal (list kbeg kend sbeg send cbeg cend)
;                          (font-latex-get-cache
;                           'font-latex-match-command-cache 4)))
;              (progn
;                (message "pattern cancelled... twice in a row")
;                nil) ;; Return a nul search (cancel this fontification)

	  (when (and font-latex-use-cache cache-reset)
	    (font-latex-set-cache
	     'font-latex-match-command-cache
	     kbeg kend limit keywords (list kbeg kend sbeg send cbeg cend)))
	  (throw 'match t))))))

(defvar font-latex-match-in-braces-cache nil
  "Cache start of unterminated LaTeX commands to fontify.")
(make-variable-buffer-local 'font-latex-match-in-braces-cache)

(defun font-latex-match-command-in-braces (keywords limit)
  "Search for command like {\\bfseries fubar} before LIMIT.
Sets `match-data' so that:
 subexpression 0 is the keyword.
 subexpression 1 is the rest in the TeX group.
Returns nil if no command is found."
  (when font-latex-use-cache
    (font-latex-check-cache 'font-latex-match-in-braces-cache 'in-braces limit))
  (catch 'match
    (while (re-search-forward keywords limit t)
      (unless (font-latex-faces-present-p '(font-lock-comment-face
					    font-latex-verbatim-face)
					  (match-beginning 0))
	(let ((kbeg (match-beginning 0)) (kend (match-end 1))
	      (beg  (match-end 0))
	      end cbeg cend
	      cache-reset
	      (parse-sexp-ignore-comments t)) ; scan-sexps ignores comments
	  (goto-char kbeg)
	  (cond
	   ((not (eq (preceding-char) ?\{))
	    ;; Fontify only the keyword (no argument found).
	    (setq cbeg kbeg cend kend)
	    (goto-char (match-end 0))
	    (store-match-data (list (point) (point) cbeg cend))
	    (throw 'match t))
	   (t
	    ;; There's an opening bracket
	    (save-restriction
	      ;; Restrict to LIMIT.
	      (narrow-to-region (point-min) limit)
	      (forward-char -1)		;Move on the opening bracket
	      (if (font-latex-find-matching-close ?\{ ?\})
		  (setq end (1- (point)))
		(setq cache-reset t)
		(setq end (point-max))
		(goto-char end))
	      (setq cbeg beg cend end)
	      (store-match-data (list kbeg kend cbeg cend))
	      ;; Cache
	      (when (and font-latex-use-cache cache-reset)
		(goto-char limit)	;Avoid infinite loops?
		(font-latex-set-cache
		 'font-latex-match-in-braces-cache
		 kbeg kend limit 'in-braces
		 (list kbeg kend cbeg cend)))
	      (throw 'match t)))))))))

(defun font-latex-match-simple-command (limit)
  "Search for command like \\foo before LIMIT."
  (TeX-re-search-forward-unescaped "\\\\[@A-Za-z]+" limit t))

;;; FIXME: Add caches for math-env, math-envII and quotations.
(defun font-latex-match-math-env (limit)
  "Match math pattern up to LIMIT.
Used for patterns like:
\\( F = ma \\)
\\ [ F = ma \\] but not \\\\ [len]"
  (catch 'match
    (while (re-search-forward "\\(\\\\(\\)\\|\\(\\\\\\[\\)" limit t)
      (goto-char (match-beginning 0))
      (if (eq (preceding-char) ?\\)	; \\[ is not a math environment
	  (goto-char (match-end 0))
	(let ((beg (point)))
	  (search-forward (cond ((match-beginning 1) "\\)")
				(t                   "\\]"))
			  limit 'move)
	  (store-match-data (list beg (or (match-end 0) (point))))
	  (throw 'match t))))))

(defun font-latex-match-math-envII (limit)
  "Match math patterns up to LIMIT.
Used for patterns like:
\\begin{equation}
 fontified stuff
\\end{equation}
The \\begin{equation} and \\end{equation} are not fontified here."
  (when (re-search-forward
         (eval-when-compile
           (concat "\\\\begin[ \t]*{\\(\\(display\\)?math\\|equation\\|eqnarray"
                   "\\|gather\\|multline\\|align\\|x*alignat"
                   "\\)\\*?}"))
         limit t)
    (let ((beg (match-end 0)) end)
      (if (re-search-forward (concat "\\\\end[ \t]*{"
				     (regexp-quote (buffer-substring
						    (match-beginning 1)
						    (match-end 0))))
			     limit 'move)
          (setq end (match-beginning 0))
        (setq end (point)))
      (store-match-data (list beg end))
      t)))

(defun font-latex-match-quotation (limit)
  "Match quote patterns up to LIMIT.
Used for patterns like:
``this is a normal quote'' and these are multilingual quoted strings:
\"< french \"> and \"`german\"' quotes.
The quotes << french >> and 8-bit french are used if `font-latex-quotes' is
set to french, and >> german << (and 8-bit) are used if set to german."
  ;; Update quotes list and regexp if value of `font-latex-quotes' changed.
  (unless (eq font-latex-quotes-control font-latex-quotes)
    (setq font-latex-quotes-control font-latex-quotes)
    ;; Set order of each entry in `font-latex-quote-list' according to
    ;; setting of `font-latex-quotes'.
    (let ((tail font-latex-quote-list)
	  elt)
      (while tail
	(setq elt (car tail))
	(when (and (> (safe-length elt) 2)
		   (not (eq (nth 2 elt) font-latex-quotes)))
	  (setcar tail (list (nth 1 elt) (nth 0 elt) font-latex-quotes)))
	(setq tail (cdr tail))))
    (setq font-latex-quote-regexp-beg
	  (regexp-opt (mapcar 'car font-latex-quote-list) t)))
  ;; Search for matches.
  (catch 'match
    (while (re-search-forward font-latex-quote-regexp-beg limit t)
      (unless (font-latex-faces-present-p '(font-lock-comment-face
					    font-latex-verbatim-face)
					  (match-beginning 0))
	(let ((beg (match-beginning 0)))
	  (search-forward (nth 1 (assoc
				  (if (fboundp 'string-make-multibyte)
				      (string-make-multibyte (match-string 0))
				    (match-string 0))
				  font-latex-quote-list)) limit 'move)
	  (store-match-data (list beg (point)))
	  (throw 'match t))))))

(defun font-latex-match-script (limit)
  "Match subscript and superscript patterns up to LIMIT."
  (when font-latex-fontify-script
    (re-search-forward
     (eval-when-compile
       ;; Regexp taken from `tex-font-lock-keywords-3' from
       ;; tex-mode.el in GNU Emacs on 2004-07-07.
       (concat "[_^] *\\([^\n\\{}]\\|" "\\\\"
	       "\\([a-zA-Z@]+\\|[^ \t\n]\\)" "\\|"
	       "{\\(?:[^{}\\]\\|\\\\.\\|{[^}]*}\\)*" "}\\)"))
     limit t)))

;; Copy and adaption of `tex-font-lock-suscript' from tex-mode.el in
;; GNU Emacs on 2004-07-07.
(defun font-latex-script (pos)
  "Return face and display spec for subscript and superscript content."
  (when (and (font-latex-faces-present-p 'font-latex-math-face pos)
	     (not (font-latex-faces-present-p '(font-lock-constant-face
						font-lock-builtin-face
						font-lock-comment-face
						font-latex-verbatim-face) pos))
	     ;; Check for backslash quoting
	     (not (let ((odd nil)
			(pos pos))
		    (while (eq (char-before pos) ?\\)
		      (setq pos (1- pos) odd (not odd)))
		    odd)))
    ;; Adding other text properties than `face' is supported by
    ;; `font-lock-apply-highlight' in CVS Emacsen since 2001-10-28.
    ;; With the introduction of this feature the variable
    ;; `font-lock-extra-managed-props' was introduced and serves here
    ;; for feature checking.  XEmacs (CVS and 21.4.15) currently
    ;; (2004-08-18) does not support this feature.
    (let ((extra-props-flag (boundp 'font-lock-extra-managed-props)))
      (if (eq (char-after pos) ?_)
	  (if extra-props-flag
	      `(face font-latex-subscript-face display
		     ,(car font-latex-script-display))
	    'font-latex-subscript-face)
	(if extra-props-flag
	    `(face font-latex-superscript-face display
		   ,(cdr font-latex-script-display))
	  'font-latex-superscript-face)))))


;;; docTeX

(defvar font-latex-doctex-preprocessor-face
  'font-latex-doctex-preprocessor-face
  "Face used to highlight preprocessor directives in docTeX mode.")

(defface font-latex-doctex-preprocessor-face
  '((t (:inherit (font-latex-doctex-documentation-face
		  font-lock-preprocessor-face))))
  "Face used to highlight preprocessor directives in docTeX mode."
  :group 'font-latex-highlighting-faces)

(defvar font-latex-doctex-documentation-face
  'font-latex-doctex-documentation-face
  "Face used to highlight the documentation in docTeX mode.")

(defface font-latex-doctex-documentation-face
  '((((class mono)) (:inverse-video t))
    (((class grayscale) (background dark)) (:background "#333"))
    (((class color) (background dark)) (:background "#333"))
    (t (:background "#eeeeee")))
  "Face used to highlight the documentation parts in docTeX mode."
  :group 'font-latex-highlighting-faces)

(defvar font-latex-doctex-keywords
  (append font-latex-keywords-2
	  '(("^%<[^>]*>" (0 font-latex-doctex-preprocessor-face t)))))

;; Set and updated in `font-latex-set-syntactic-keywords'.
(defvar font-latex-doctex-syntactic-keywords nil)

;; Copy and adaptation of `doctex-font-lock-^^A' in `tex-mode.el' of
;; CVS Emacs (March 2004)
(defun font-latex-doctex-^^A ()
  (if (eq (char-after (line-beginning-position)) ?\%)
      (progn
	(put-text-property
	 (1- (match-beginning 1)) (match-beginning 1) 'syntax-table
	 (if (= (1+ (line-beginning-position)) (match-beginning 1))
	     ;; The `%' is a single-char comment, which Emacs
	     ;; syntax-table can't deal with.  We could turn it
	     ;; into a non-comment, or use `\n%' or `%^' as the comment.
	     ;; Instead, we include it in the ^^A comment.
	     ;; COMPATIBILITY for Emacs 20 and XEmacs
	     (eval-when-compile (if (fboundp 'string-to-syntax)
				    (string-to-syntax "< b")
				  '(2097163)))
	   ;; COMPATIBILITY for Emacs 20 and XEmacs
	   (eval-when-compile (if (fboundp 'string-to-syntax)
				  (string-to-syntax ">")
				'(12)))))
	(let ((end (line-end-position)))
	  (if (< end (point-max))
	      (put-text-property end (1+ end) 'syntax-table
				    ;; COMPATIBILITY for Emacs 20 and XEmacs
				    (eval-when-compile
				      (if (fboundp 'string-to-syntax)
					  (string-to-syntax "> b")
					'(2097164))))))
	;; COMPATIBILITY for Emacs 20 and XEmacs
	(eval-when-compile (if (fboundp 'string-to-syntax)
			       (string-to-syntax "< b")
			     '(2097163))))))

;; Copy and adaptation of `doctex-font-lock-syntactic-face-function'
;; in `tex-mode.el' of CVS Emacs (March 2004)
(defun font-latex-doctex-syntactic-face-function (state)
  ;; Mark docTeX documentation, which is parsed as a style A comment
  ;; starting in column 0.
  (if (or (nth 3 state) (nth 7 state)
	  (not (memq (char-before (nth 8 state))
		     '(?\n nil))))
      ;; Anything else is just as for LaTeX.
      (font-latex-syntactic-face-function state)
    font-latex-doctex-documentation-face))


;;; Installation in non-AUCTeX LaTeX mode

(add-hook 'latex-mode-hook 'font-latex-setup)
;; If font-latex is loaded using a latex-mode-hook, then the add-hook above
;; won't be called this time around.  Check for this now:
(if (eq major-mode 'latex-mode)
    (font-latex-setup))


;;; Byte-compilation of generated functions

(when (byte-code-function-p
       (symbol-function 'font-latex-make-built-in-keywords))
  (dolist (elt font-latex-built-in-keyword-classes)
    (let ((name (nth 0 elt)))
      (byte-compile (intern (concat "font-latex-" name)))
      (byte-compile (intern (concat "font-latex-" name "-make"))))))


;; Provide ourselves:
(provide 'font-latex)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; font-latex.el ends here
