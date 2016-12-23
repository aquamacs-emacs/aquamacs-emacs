;;; thingatpt+.el --- Extensions to `thingatpt.el'.
;;
;; Filename: thingatpt+.el
;; Description: Extensions to `thingatpt.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2016, Drew Adams, all rights reserved.
;; Created: Tue Feb 13 16:47:45 1996
;; Version: 0
;; Last-Updated: Mon Nov 21 15:25:52 2016 (-0800)
;;           By: dradams
;;     Update #: 2325
;; URL: http://www.emacswiki.org/thingatpt%2b.el
;; Doc URL: http://www.emacswiki.org/ThingAtPointPlus#ThingAtPoint%2b
;; Keywords: extensions, matching, mouse
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `thingatpt'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `thingatpt.el'.
;;
;;
;;  Commands defined here:
;;
;;    `find-fn-or-var-nearest-point', `forward-char-same-line',
;;    `forward-whitespace-&-newlines', `tap-put-thing-at-point-props',
;;    `tap-redefine-std-fns'.
;;
;;  User options defined here:
;;
;;    `tap-near-point-x-distance', `tap-near-point-y-distance'.
;;
;;  Non-interactive functions defined here:
;;
;;    `tap-bounds-of-color-at-point', `tap-bounds-of-form-at-point',
;;    `tap-bounds-of-form-nearest-point',
;;    `tap-bounds-of-list-at-point',
;;    `tap-bounds-of-list-contents-at-point',
;;    `tap-bounds-of-list-nearest-point',
;;    `tap-bounds-of-number-at-point',
;;    `tap-bounds-of-number-at-point-decimal',
;;    `tap-bounds-of-number-at-point-hex',
;;    `tap-bounds-of-sexp-at-point',
;;    `tap-bounds-of-sexp-nearest-point',
;;    `tap-bounds-of-string-at-point',
;;    `tap-bounds-of-string-contents-at-point',
;;    `tap-bounds-of-symbol-at-point',
;;    `tap-bounds-of-symbol-nearest-point',
;;    `tap-bounds-of-thing-nearest-point', `tap-color-at-point',
;;    `tap-color-nearest-point',
;;    `tap-color-nearest-point-with-bounds',
;;    `tap-define-aliases-wo-prefix', `tap-form-at-point-with-bounds',
;;    `tap-form-nearest-point', `tap-form-nearest-point-with-bounds',
;;    `tap-list-at/nearest-point-with-bounds',
;;    `tap-list-at-point-with-bounds', `tap-list-contents-at-point',
;;    `tap-list-contents-nearest-point', `tap-list-nearest-point',
;;    `tap-list-nearest-point-with-bounds',
;;    `tap-list-nearest-point-as-string', `tap-looking-at-p',
;;    `tap-looking-back-p', `tap-non-nil-symbol-name-at-point',
;;    `tap-non-nil-symbol-name-nearest-point',
;;    `tap-non-nil-symbol-nearest-point',
;;    `tap-number-at-point-decimal', `tap-number-at-point-hex',
;;    `tap-number-nearest-point', `tap-read-from-whole-string',
;;    `tap-region-or-word-at-point',
;;    `tap-region-or-word-nearest-point',
;;    `tap-region-or-non-nil-symbol-name-nearest-point',
;;    `tap-sentence-nearest-point', `tap-sexp-at-point-with-bounds',
;;    `tap-sexp-nearest-point', `tap-sexp-nearest-point-with-bounds',
;;    `tap-string-at-point', `tap-string-contents-at-point',
;;    `tap-string-contents-nearest-point', `tap-string-match-p',
;;    `tap-string-nearest-point', `tap-symbol-at-point-with-bounds',
;;    `tap-symbol-name-at-point', `tap-symbol-name-nearest-point',
;;    `tap-symbol-nearest-point',
;;    `tap-symbol-nearest-point-with-bounds', `tap-thing-at-point',
;;    `tap-thing-at-point-as-string',
;;    `tap-thing-at-point-with-bounds',
;;    `tap-thing/form-nearest-point-with-bounds',
;;    `tap-thing-nearest-point',
;;    `tap-thing-nearest-point-with-bounds',
;;    `tap-unquoted-list-at-point', `tap-unquoted-list-nearest-point',
;;    `tap-unquoted-list-nearest-point-as-string',
;;    `tap-word-nearest-point',
;;
;;    plus the same functions without the prefix `tap-', if you invoke
;;    `tap-redefine-std-fns'.
;;
;;
;;  A REMINDER (the doc strings are not so great):
;;
;;    These functions, defined in `thingatpt.el', all move point:
;;      `beginning-of-thing', `end-of-sexp', `end-of-thing',
;;      `forward-symbol', `forward-thing'.
;;
;;  For older Emacs releases that do not have the following functions,
;;  they are defined here as no-ops:
;;
;;  `constrain-to-field', `field-beginning', `field-end'.
;;
;;
;;  How To Use This Library
;;  =======================
;;
;;  End Users
;;  ---------
;;
;;  Load this library after loading the standard GNU file
;;  `thingatpt.el'.  You can put this in your init file (`~/.emacs'):
;;
;;    (eval-after-load "thingatpt" '(require 'thingatpt+))
;;
;;  That defines new functions and improved versions of some of the
;;  standard thing-at-point functions.  All such functions have the
;;  prefix `tap-', so they are not used by default in any way.
;;
;;  Requiring library `thingatpt+.el' does not, however, make Emacs
;;  use the improved functions.  Merely loading it does not change the
;;  behavior of thing-at-point features.
;;
;;  If you want functions defined here to be used for calls to
;;  standard Emacs functions that make use of the `thing-at-point' and
;;  `bounds-of-thing-at-point' symbol properties for standard thing
;;  types (e.g. `list'), then put this in your init file, instead:
;;
;;    (eval-after-load "thingatpt"
;;      '(when (require 'thingatpt+)
;;         (tap-put-thing-at-point-props))
;;
;;  Note that some of my other libraries, including Icicles,
;;  Bookmark+, `grep+.el', `replace+.el', and `strings.el', do exactly
;;  that.  Note too that `tap-put-thing-at-point-props' improves the
;;  behavior of (thing-at-point 'list) - see below.
;;
;;  A further step, which I recommend, is to use the `tap-' versions
;;  of standard functions, defined here, everywhere in place of those
;;  standard functions.  In other words, redefine the standard
;;  functions as the `tap-' versions defined here.  For example,
;;  redefine `bounds-of-thing-at-point' to do what
;;  `tap-bounds-of-thing-at-point' does.
;;
;;  (If you do that then you need not invoke
;;  `tap-put-thing-at-point-props' to pick up the versions defined
;;  here of standard functions.  The property values set by vanilla
;;  library `thingatpt.el' will be OK because the functions themselves
;;  will have been redefined in that case.)
;;
;;  To get the most out of this library, I recommend that you put
;;  (only) the following in your init file:
;;
;;    (eval-after-load "thingatpt"
;;      '(when (require 'thingatpt+)
;;         (tap-redefine-std-fns))
;;
;;  That makes all Emacs code that uses the following standard
;;  functions use the their versions that are defined here, not the
;;  vanilla versions defined in `thingatpt.el'.
;;
;;  `bounds-of-thing-at-point' - Better behavior.
;;                               Accept optional arg SYNTAX-TABLE.
;;  `form-at-point'            - Accept optional arg SYNTAX-TABLE.
;;  `list-at-point'            - Better behavior.
;;  `symbol-at-point'          - Use `emacs-lisp-mode-syntax-table'.
;;  `thing-at-point'           - Ensure it returns a string or nil.
;;                               Accept optional arg SYNTAX-TABLE.
;;  `thing-at-point-bounds-of-list-at-point'
;;                             - Better behavior.  Accept optional
;;                               args UP and UNQUOTEDP.
;;
;;
;;  Lisp Programmers
;;  ----------------
;;
;;  If you write code that uses some of the functions defined here,
;;  this section is for you.
;;
;;  You can use the functions defined in `thingatpt+.el' that have
;;  prefix `tap-' to obtain, for your code, the improvements they
;;  provide.  Doing only that has no effect on any code that calls
;;  vanilla thing-at-point functions (which have no prefix `tap-').
;;
;;  For convenience you can invoke `tap-define-aliases-wo-prefix' to
;;  provide alias functions that have the same names but without the
;;  prefix `tap-'.  This affects only functions defined here that have
;;  no vanilla counterpart, so the aliases do not collide with any
;;  standard Emacs functions.  This is just a naming convenience.
;;
;;  For example, you might do this:
;;
;;    (when (require 'thingatpt+ nil t)  ; (no error if not found)
;;      (tap-define-aliases-wo-prefix))
;;
;;  You can optionally enable the improvements defined here to have
;;  wider application, so that code that does not directly invoke the
;;  functions defined here nevertheless uses them indirectly.
;;
;;  You can, for example, put `tap-' functions on THING-type symbols
;;  as property `thing-at-point' or property
;;  `bounds-of-thing-at-point'.  That has the effect of using those
;;  `tap-' functions for those THING types only.
;;
;;  For example, to get the improvements for lists offered by
;;  `tap-list-at-point', you can do this:
;;
;;    (put 'list 'bounds-of-thing-at-point
;;         'tap-bounds-of-list-at-point)
;;    (put 'list 'thing-at-point 'tap-list-at-point)
;;
;;  That causes the vanilla thing-at-point functions to invoke those
;;  `tap-' functions when handling lists.  It has an effect only on
;;  lists, not on other THINGs.  This behavior happens because the
;;  generic vanilla functions `thing-at-point' and
;;  `bounds-of-thing-at-point' use those standard symbol properties.
;;
;;  For even wider application, that is, if you want all of the
;;  improvements defined here to be available generally, then you will
;;  also need to do ONE of the following (#1 or #2):
;;
;;  1. Call `tap-redefine-std-fns', to redefine standard functions.
;;
;;  2. Do BOTH of these things:
;;
;;    a. Call `tap-put-thing-at-point-props', to substitute `tap-'
;;       functions for standard functions as the values of symbol
;;       properties `thing-at-point' and `bounds-of-thing-at-point'.
;;
;;    b. Call the individual `tap-*' functions explicitly for each of
;;       the standard functions that would be redefined by
;;       `tap-redefine-std-fns'.  Or call standard functions that make
;;       use of property `thing-at-point' or
;;       `bounds-of-thing-at-point'.
;;
;;    This (#2) changes (improves) the behavior of things like
;;    (thing-at-point 'list), even though it does not redefine any
;;    standard functions.  Again, this is because functions
;;    `thing-at-point' and `bounds-of-thing-at-point' use symbol
;;    properties `thing-at-point' and `bounds-of-thing-at-point', and
;;    `tap-put-thing-at-point-props' changes those property values.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2016/11/21 dadams
;;     Rename tap-thing-at-point to tap-thing-at-point-as-string and add new def of former.
;;     The new def follows vanilla Emacs in letting property thing-at-point return non-string.
;;     tap-form-at-point: Use tap-thing-at-point-as-string.
;;     tap-region-or-word-at-point: Use args STRICT REALLY-WORD with current-word.
;; 2016/11/04 dadams
;;     tap-thing-at-point: Added optional arg NO-PROPERTIES, per Emacs 24.4+.
;;     tap-form-at-point, tap-non-nil-symbol-name-at-point:
;;       Use nil for optional arg NO-PROPERTIES in call to tap-thing-at-point.
;; 2016/09/06 dadams
;;     Added: tap-read-from-whole-string.
;;     tap-form-at-point(-with-bounds): Use tap-read-from-whole-string.
;; 2016/08/30 dadams
;;     tap-string-match-p: Do NOT alias string-match-p, because that is a defsubst.
;; 2016/06/20 dadams
;;     tap-thing-at-point:
;;       Typo: convert result of funcall, not original THING, to string.  Thx to Tino Calancha.
;; 2015/08/23 dadams
;;     tap-list-at/nearest-point-with-bounds:
;;       Use (nth 3 (syntax-ppss)) for Emacs 25+ - see Emacs bug #20732.
;; 2014/08/22 dadams
;;     tap-looking-at-p: Do not defalias to Emacs looking-at-p because that is a defsubst.
;; 2014/06/07 dadams
;;     Added: tap-bounds-of-list-contents-at-point, tap-list-contents-at-point,
;;            tap-list-contents-nearest-point.
;;     Put tap-bounds-of-(string|list)-contents-at-point as bounds-of-thing-at-point property.
;; 2013/09/20 dadams
;;     Added: tap-bounds-of-string-contents-at-point, tap-string-contents-at-point,
;;            tap-string-contents-nearest-point.
;;     tap-define-aliases-wo-prefix: Updated for new functions.
;;     tap-bounds-of-string-at-point (incompatible change):
;;       Include " chars in string returned.  Return the string also when point on ending ".
;; 2013/09/13 dadams
;;     tap-thing/form-nearest-point-with-bounds:
;;       Do not skip looping over chars in same line when eobp.
;; 2012/11/10 dadams
;;     Added: tap(-bounds-of)-color-at-point, tap-color-nearest-point(-with-bounds).
;;     tap-word-nearest-point: Corrected doc string: returns nil if none found.
;; 2012/08/24 dadams
;;     Added: tap-string-match-p, tap-looking-at-p, tap-looking-back-p.
;;     tap-list-at/nearest-point-with-bounds: Handle point inside a string.
;;     tap-number-at-point-(hex|decimal): Use tap-string-match-p.
;; 2012/08/22 dadams
;;     Added: tap-bounds-of-number-at-point(-decimal|-hex).
;;     tap-thing-at-point, tap-bounds-of-thing-at-point-1:  Check first the tap-* property.
;;     For things (unquoted-)list, (non-nil-)symbol-name, region-or-word,
;;      (decimal-|hex-)number, string:
;;         put tap-* properties also.
;;     tap-put-thing-at-point-props: Use tap-bounds-of-number-at-point, not lambda.
;; 2012/08/21 dadams
;;     Added: tap-put-thing-at-point-props.
;;     Moved puts for list and number to tap-put-thing-at-point-props.
;;     tap-define-aliases-wo-prefix: Return non-nil so can use in Boolean test.
;; 2012/08/19 dadams
;;     Added: tap-symbol-name-at-point.
;;     tap(-bounds-of)-symbol-at-point(-with-bounds):
;;       Removed useless arg NON-NIL.  Adjust calls to them accordingly.
;;     tap-thing-at-point: Ensure it returns a string (or nil).
;;     tap-non-nil-symbol-name-at-point:
;;       Redefine, using tap-thing-at-point with emacs-lisp-mode-syntax-table.
;;     tap(-non-nil)-symbol-name-nearest-point: Return nil, not "", if none found.
;;     tap-sexp-nearest-point: Corrected: pass nil PREDICATE arg.
;;     Doc string improvements.
;; 2012/08/18 dadams
;;     tap-define-aliases-wo-prefix: Return non-nil so can use in Boolean guards.
;;     word-nearest-point -> tap-word-nearest-point (typo).
;; 2012/08/17 dadams
;;     Added: tap-define-aliases-wo-prefix, tap-redefine-std-fns.
;;     Added group thing-at-point-plus.  Use for defcustoms.
;;     Renamed fns & vars, adding prefix tap-.
;;     Do not redefine std stuff, except in tap-define-aliases-wo-prefix, tap-redefine-std-fns.
;; 2012/02/18 dadams
;;     thing/form-nearest-point-with-bounds:
;;       Fixed infloop: set [be]obp when finished sole line in both directions.
;; 2011/09/06 dadams
;;     thing/form-nearest-point-with-bounds: If only one line then do not try to access others.
;;     bounds-of-thing-at-point-1, thing-at-point, thing/form-nearest-point-with-bounds:
;;       Respect field boundaries.
;;     Define constrain-to-field, field-(beginning|end) as no-ops for older Emacs releases.
;; 2011/08/30 dadams
;;     Added: region-or-non-nil-symbol-name-nearest-point.
;;     region-or-*: Use region only if transient-mark-mode, non-empty (and active).
;; 2011/08/17 dadams
;;     list-at/nearest-point-with-bounds:
;;       Don't count `foo or 'foo as a list, i.e., (` foo) or (quote foo).
;; 2011/08/14 dadams
;;     bounds-of-thing-at-point-1:
;;       Tests for end need to use <, not <=.  If past the THING then should return nil.
;; 2011/07/08 dadams
;;     Removed: list-at/nearest-point.
;;     Added: (list|sexp)-(at|nearest)-point-with-bounds,
;;            bounds-of-(list|sexp)-(at|nearest)-point, list-at/nearest-point-with-bounds.
;;     (unquoted-)list-(at|nearest)-point(-as-string):
;;       Redefined using list-(at|nearest)-point-with-bounds.
;;     (put 'list 'bounds-of-thing-at-point 'bounds-of-list-at-point) - not nil.
;; 2011/05/24 dadams
;;     Added: (bounds-of-)string-at-point, string-nearest-point.
;; 2011/05/21 dadams
;;     bounds-of-thing-at-point-1: Synchronized with vanilla Emacs fix for bug #8667.
;; 2011/05/13 dadams
;;     Added redefinition of bounds-of-thing-at-point - fixed bug #8667.
;;       Removed old-bounds-of-thing-at-point.  Added: bounds-of-thing-at-point-1.
;;     Added: forward-whitespace-&-newlines.
;;     Added (put 'thing-at-point *) for unquoted-list, non-nil-symbol-name.
;;     Removed old eval-when-compile for Emacs before Emacs 20.
;; 2011/05/07 dadams
;;     Added: number-at-point-(decimal|hex) and aliases.
;;     Put (bounds-of-)thing-at-point properties: (hex-|decimal-)number-at-point.
;; 2011/05/05 dadams
;;     (put 'list 'bounds-of-thing-at-point nil)  See Emacs bug #8628.
;;     (put 'list 'thing-at-point 'list-at-point) - not really needed, though.
;;     bounds-of-thing-at-point: Mention in doc string that pre-Emacs 23 is buggy.
;; 2011/01/20 dadams
;;     *list-*-point: Improved doc strings.
;; 2011/01/04 dadams
;;     Removed autoload cookies from non def* sexps and non-interactive fns.
;;     Added autoload cookies for defcustom.
;; 2010/12/17 dadams
;;     Added: (unquoted-)list-(at|nearest)-point, list-at/nearest-point,
;;            unquoted-list-nearest-point-as-string.
;;     list-nearest-point: Redefined using list-at/nearest-point.
;; 2010/12/10 dadams
;;     form-at-point-with-bounds:
;;       Moved condition-case to around whole.  Let sexp be any format of nil.
;; 2010/01/24 dadams
;;     Added: region-or-word-nearest-point.
;; 2008/10/22 dadams
;;     Added: region-or-word-at-point.  Thx to Richard Riley.
;; 2007/07/15 dadams
;;     Added: thing/form-nearest-point-with-bounds,
;;            non-nil-symbol(-name)-(at|nearest)-point, near-point-(x|y)-distance.
;;     (thing|form)-nearest-point-with-bounds:
;;       Use thing/form-nearest-point-with-bounds, which: (1) accepts PRED arg,
;;         (2) respects near-point-(x|y)-distance, (3) fixed some logic.
;;     form-at-point-with-bounds:
;;       Distinguish between nil (no find) and "nil" object found.
;;     (bounds-of-)symbol-(at|nearest)-point(-with-bounds), :
;;       Added optional non-nil arg.
;;     Added beginning-op, end-op, and forward-op for defun type.
;; 2006/12/08 dadams
;;     Added: find-fn-or-var-nearest-point.
;; 2006/05/16 dadams
;;     Only require cl (at compile time) for Emacs < 20.
;;     Replace incf by setq...1+.
;; 2005/12/17 dadams
;;     symbol-name-nearest-point, form-at-point-with-bounds:
;;       Treat nil as legitimate symbol.
;; 1996/06/11 dadams
;;     bounds-of-symbol-at-point, bounds-of-symbol-nearest-point,
;;       symbol-at-point, symbol-at-point-with-bounds,
;;       symbol-name-nearest-point, symbol-nearest-point,
;;       symbol-nearest-point-with-bounds: No longer use a syntax-table
;;       arg.  Always dealing with elisp symbols, so use
;;       emacs-lisp-mode-syntax-table.
;; 1996/03/20 dadams
;;     1. Added redefinitions of thing-at-point, form-at-point, with optional
;;        syntax table arg.
;;     2. Added: thing-nearest-point-with-bounds,
;;        bounds-of-thing-nearest-point, thing-nearest-point,
;;        form-nearest-point-with-bounds,
;;        bounds-of-form-nearest-point, form-nearest-point,
;;        word-nearest-point, sentence-nearest-point,
;;        sexp-nearest-point, number-nearest-point,
;;        list-nearest-point.
;;     3. symbol-at-point: Added optional syntax table arg.
;;     4. symbol-nearest-point-with-bounds: Now defined in terms of
;;        form-nearest-point-with-bounds.
;;     5. bounds-of-form-at-point: Added args THING and PRED.
;; 1996/03/20 dadams
;;     1. Added redefinition of bounds-of-thing-at-point: New arg SYNTAX-TABLE.
;;     2. thing-at-point-with-bounds, form-at-point-with-bounds,
;;        bounds-of-form-at-point, symbol-at-point-with-bounds,
;;        bounds-of-symbol-at-point, symbol-nearest-point-with-bounds,
;;        bounds-of-symbol-nearest-point, symbol-nearest-point,
;;        symbol-name-nearest-point: New arg SYNTAX-TABLE.
;; 1996/03/08 dadams
;;     1. Added: thing-at-point-with-bounds, form-at-point-with-bounds,
;;        bounds-of-form-at-point, symbol-at-point-with-bounds,
;;        bounds-of-symbol-at-point
;;     2. symbol-at-point: 2nd arg ('symbolp) to form-at-point to ensure interned.
;;     3. Added: symbol-nearest-point-with-bounds, symbol-name-nearest-point,
;;        bounds-of-symbol-nearest-point, symbol-nearest-point.
;;     4. symbol-nearest-point-with-bounds: Use symbol-at-point-with-bounds, not
;;        bounds-of-thing-at-point.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'thingatpt)

;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defgroup thing-at-point-plus nil
  "Enhancements to `thingatpt.el'."
  :prefix "tap-" :group 'applications :group 'development :group 'help
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
Thingatpt+ bug: \
&body=Describe bug here, starting with `emacs -Q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "http://www.emacswiki.org/thingatpt+.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/ThingAtPointPlus")
  :link '(emacs-commentary-link :tag "Commentary" "thingatpt+"))

;;;###autoload
(defcustom tap-near-point-x-distance 50
  "*Maximum number of characters from point to search, left and right.
Used typically by functions that invoke
`tap-thing/form-nearest-point-with-bounds', and which provide default
text for minibuffer input.  Such functions can also ignore or override
this setting temporarily.

See `tap-thing-nearest-point' for an explanation of the determination
of \"nearness\"."
  :type 'integer :group 'thing-at-point-plus :group 'minibuffer)

;;;###autoload
(defcustom tap-near-point-y-distance 5
  "*Maximum number of lines from point to search, up and down.
To constrain search to the same line as point, set this to zero.
Used typically by functions that invoke
`tap-thing/form-nearest-point-with-bounds', and which provide default
text for minibuffer input.  Such functions can also ignore or override
this setting temporarily.

See `tap-thing-nearest-point' for an explanation of the determination
of \"nearness\"."
  :type 'integer :group 'thing-at-point-plus :group 'minibuffer)


;; Make these no-ops for Emacs versions that don't have it.  Easier than `fboundp' everywhere.
(unless (fboundp 'constrain-to-field) (defun constrain-to-field (&rest _ignore) (point)))
(unless (fboundp 'field-beginning)    (defalias 'field-beginning (symbol-function 'ignore)))
(unless (fboundp 'field-end)          (defalias 'field-end (symbol-function 'ignore)))
 
;;; Utility Functions ------------------------------------------------


;; Same as `read-from-whole-string' (called `thingatpt--read-from-whole-string' starting
;; with Emacs 25) in library `thingatpt.el'.
;;
(defun tap-read-from-whole-string (string)
  "Read a Lisp expression from STRING.
Raise an error if the entire string was not used."
  (let* ((read-data  (read-from-string string))
	 (more-left (condition-case nil
                        ;; The call to `ignore' suppresses a compiler warning.
                        (progn (ignore (read-from-string (substring string (cdr read-data))))
                               t)
                      (end-of-file nil))))
    (if more-left (error "Can't read whole string") (car read-data))))

;; Same as `icicle-string-match-p' in `icicles-fn.el'.
;; Do NOT alias `string-match-p', because that is a `defsubst'.
;;
(defun tap-string-match-p (regexp string &optional start)
  "Like `string-match', but this saves and restores the match data."
  (save-match-data (string-match regexp string start)))

;; Same as `bmkp-looking-at-p' (`bookmark+-bmu.el'), `icicle-looking-at-p' (`icicles-mcmd.el').
;; Do not `defalias' to Emacs `looking-at-p' because that is a `defsubst'.
(defun tap-looking-at-p (regexp)
  "Like `looking-at', but this saves and restores the match data."
  (save-match-data (looking-at regexp)))

(defun tap-looking-back-p (regexp &optional limit greedy)
  "Like `looking-back', but this does not change the match data."
  (save-match-data
    (let ((start  (point))
          (pos    (save-excursion
                    (and (re-search-backward
                          (if (> emacs-major-version 21)
                              (concat "\\(?:" regexp "\\)\\=")
                            (concat "\\(" regexp "\\)\\="))
                          limit t)
                         (point)))))
      (if (and greedy  pos)
          (save-restriction
            (narrow-to-region (point-min) start)
            (while (and (> pos (point-min))
                        (save-excursion (goto-char pos)
                                        (backward-char 1)
                                        (tap-looking-at-p
                                         (if (> emacs-major-version 21)
                                             (concat "\\(?:" regexp "\\)\\'")
                                           (concat "\\(" regexp "\\)\\'")))))
              (setq pos  (1- pos)))
            (save-excursion (goto-char pos)
                            (tap-looking-at-p
                             (if (> emacs-major-version 21)
                                 (concat "\\(?:" regexp "\\)\\'")
                               (concat "\\(" regexp "\\)\\'"))))))
      (not (null pos)))))
 
;;; THINGS -----------------------------------------------------------


;; If you invoke `tap-redefine-std-fns', this def replaces the original in `thingatpt.el'.
;;
;; 1. Fix Emacs bug #8667 (do not return an empty thing).
;; 2. Add optional argument SYNTAX-TABLE.
;; 3. Check first the property `tap-bounds-of-thing-at-point'.
;;
;; NOTE: Most of the other functions here are based on this function.
;;
(defun tap-bounds-of-thing-at-point (thing &optional syntax-table)
  "Return the start and end locations for the THING at point.
Return a consp (START . END), where START /= END.
Return nil if no THING is found.

THING is an Emacs Lisp symbol that specifies a type of syntactic
entity.  THING examples include `word', `sentence', `defun'.  See the
commentary of library `thingatpt.el' for how to define a symbol as a
valid THING.

Optional arg SYNTAX-TABLE is a syntax table to use."
  (if syntax-table
      (let ((buffer-syntax  (syntax-table)))
        (unwind-protect
             (progn (set-syntax-table syntax-table)
                    (tap-bounds-of-thing-at-point-1 thing))
          (set-syntax-table buffer-syntax)))
    (tap-bounds-of-thing-at-point-1 thing)))

;; This is the vanilla `bounds-of-thing-at-point', but with Emacs bugs #8667 and #9300 fixed.
(defun tap-bounds-of-thing-at-point-1 (thing)
  "Helper for `tap-bounds-of-thing-at-point'.
Do everything except handle the optional SYNTAX-TABLE arg."
  (let ((bounds-fn  (or (get thing 'tap-bounds-of-thing-at-point)
                        (get thing 'bounds-of-thing-at-point))))
    (if bounds-fn
        (funcall bounds-fn)
      (let ((orig  (point)))
        (condition-case nil
            (save-excursion
              ;; Try moving forward, then back.
              (funcall (or (get thing 'end-op) ; Move to end.
                           (lambda () (forward-thing thing 1))))
              (constrain-to-field nil orig)
              (funcall (or (get thing 'beginning-op) ; Move to beg.
                           (lambda () (forward-thing thing -1))))
              (constrain-to-field nil orig)
              (let ((beg  (point)))
                (if (<= beg orig)
                    ;; If that brings us all the way back to ORIG,
                    ;; it worked.  But END may not be the real end.
                    ;; So find the real end that corresponds to BEG.
                    ;; FIXME: in which cases can `real-end' differ from `end'?
                    (let ((real-end  (progn (funcall
                                             (or (get thing 'end-op)
                                                 (lambda () (forward-thing thing 1))))
                                            (constrain-to-field nil orig)
                                            (point))))
                      (and (< orig real-end)  (< beg real-end)
                           (cons beg real-end)))
                  (goto-char orig)
                  ;; Try a second time, moving first backward and then forward,
                  ;; so that we can find a thing that ends at ORIG.
                  (funcall (or (get thing 'beginning-op) ; Move to beg.
                               (lambda () (forward-thing thing -1))))
                  (constrain-to-field nil orig)
                  (funcall (or (get thing 'end-op) ; Move to end.
                               (lambda () (forward-thing thing 1))))
                  (constrain-to-field nil orig)
                  (let ((end       (point))
                        (real-beg  (progn (funcall
                                           (or (get thing 'beginning-op)
                                               (lambda () (forward-thing thing -1))))
                                          (constrain-to-field nil orig)
                                          (point))))
                    (and (<= real-beg orig)  (< orig end)  (< real-beg end)
                         (cons real-beg end))))))
          (error nil))))))

(defun tap-thing-at-point-with-bounds (thing &optional syntax-table)
  "Return the thing of type THING at point, plus its bounds.
Return nil if no such thing is found.

If found, return a cons (THE-THING START . END), where THE-THING is
the `tap-thing-at-point'.  START and END are the buffer positions of
THE-THING.

See `tap-bounds-of-thing-at-point'.

Optional arg SYNTAX-TABLE is a syntax table to use."
  (let ((bounds  (tap-bounds-of-thing-at-point thing syntax-table)))
    (and bounds  (cons (buffer-substring (car bounds) (cdr bounds)) bounds))))

;; If you invoke `tap-redefine-std-fns', this def replaces the original in `thingatpt.el'.
;;
;; 1. Add optional argument SYNTAX-TABLE.
;; 2. Check first the symbol property `tap-thing-at-point'.
;;
(defun tap-thing-at-point (thing &optional no-properties syntax-table)
  "Return the THING at point.
If no THING is present at point then return nil.

THING is an Emacs Lisp symbol that specifies a type of syntactic
entity.  THING examples include `symbol', `list', `sexp', `defun',
`filename', `url', `email', `word', `sentence', `whitespace', `line',
`number', and `page'.  See the commentary of library `thingatpt.el'
for how to define a symbol as a valid THING.

If THING has property `thing-at-point' then the property value should
be a function.  Call the function with no arguments, and return the
result.

Otherwise, try to get the bounds of THING.  If successful, return the
bounded string.

Optional arg NO-PROPERTIES means that if a string is to be returned
then it is first stripped of any text properties.

Optional arg SYNTAX-TABLE is a syntax table to use."
  (let* ((thing-fn   (or (get thing 'tap-thing-at-point)  (get thing 'thing-at-point)))
         (something  (if thing-fn
                         (let* ((opoint  (point))
                                (thg     (prog1 (funcall thing-fn)
                                           (constrain-to-field nil opoint))))
                           thg)
                       (let ((bounds  (tap-bounds-of-thing-at-point thing syntax-table)))
                         (and bounds  (buffer-substring (car bounds) (cdr bounds)))))))
    (when (and (stringp something)  no-properties)
      (set-text-properties 0 (length something) nil something))
    something))

(defun tap-thing-at-point-as-string (thing &optional no-properties syntax-table)
  "Return the THING at point as a string.
If no THING is present at point then return nil.

THING is an Emacs Lisp symbol that specifies a type of syntactic
entity.  THING examples include `symbol', `list', `sexp', `defun',
`filename', `url', `email', `word', `sentence', `whitespace', `line',
`number', and `page'.  See the commentary of library `thingatpt.el'
for how to define a symbol as a valid THING.

If THING has property `thing-at-point' then the property value should
be a function.  The function is called with no arguments.  If the
return value of that function is a string or nil then that value is
returned by this function also.  Otherwise, that value is converted to
a string and returned.

Optional arg NO-PROPERTIES means that if a string is to be returned
then it is first stripped of any text properties.

Optional arg SYNTAX-TABLE is a syntax table to use."
  (let* ((thing-fn  (or (get thing 'tap-thing-at-point)  (get thing 'thing-at-point)))
         (text      (if thing-fn
                        (let* ((opoint  (point))
                               (thg     (prog1 (funcall thing-fn)
                                          (constrain-to-field nil opoint))))
                          (if (stringp thg)
                              thg
                            (and thg  (format "%s" thg))))
                      (let ((bounds  (tap-bounds-of-thing-at-point thing syntax-table)))
                        (and bounds  (buffer-substring (car bounds) (cdr bounds)))))))
    (when (and text  no-properties) (set-text-properties 0 (length text) nil text))
    text))

(defun tap-thing-nearest-point-with-bounds (thing &optional syntax-table)
  "Return the THING nearest point, plus its bounds: (THING START . END).
Return nil if no such THING is found.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

THING is the `tap-thing-nearest-point', a string.
START and END are the buffer positions of THING - see
`tap-bounds-of-thing-nearest-point'.

Optional arg SYNTAX-TABLE is a syntax table to use."
  (tap-thing/form-nearest-point-with-bounds #'tap-thing-at-point-with-bounds
                                            thing nil syntax-table))

(defun tap-thing/form-nearest-point-with-bounds (fn thing predicate syntax-table)
  "Thing or form nearest point, plus bounds: (THING-OR-FORM START . END).
Helper for `tap-thing-nearest-point-with-bounds'
and `tap-form-nearest-point-with-bounds'.

\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

FN is a function returning a thing or a form at point, and its bounds.
Other args are as for `tap-thing-nearest-point-with-bounds'."
  (let ((opoint  (point)))
    (let ((f-or-t+bds  (prog1 (if predicate
                                  (funcall fn thing predicate syntax-table)
                                (funcall fn thing syntax-table))
                         (constrain-to-field nil opoint)))
          (ind1        0)
          (ind2        0)
          (updown      1)
          (bobp        (or (eq (field-beginning nil) (point))  (bobp)))
          (eobp        (or (eq (field-end nil) (point))        (eobp)))
          (bolp        (or (eq (field-beginning nil) (point))  (bolp)))
          (eolp        (or (eq (field-end nil) (point))        (eolp)))
          (max-x       (abs tap-near-point-x-distance))
          (max-y       (if (zerop (save-excursion
                                    (goto-char (point-max))
                                    (prog1 (forward-line -1)
                                      (constrain-to-field nil opoint))))
                           (abs tap-near-point-y-distance)
                         1)))           ; Only one line.
      ;; IND2: Loop over lines (alternately up and down).
      (while (and (<= ind2 max-y)  (not f-or-t+bds)  (not (and bobp  eobp)))
        (setq updown  (- updown))       ; Switch directions up/down (1/-1).
        (save-excursion
          (when (> max-y 1)             ; Skip this if only one line.
            (condition-case ()
                (prog1 (previous-line (* updown ind2)) ; 0, 1, -1, 2, -2, ...
                  (constrain-to-field nil opoint))
              (beginning-of-buffer (setq bobp  t))
              (end-of-buffer (setq eobp  t))
              (error nil)))
          ;; Do not try to go beyond buffer or field limit.
          (unless (or (and bobp  (> (* updown ind2) 0)) ; But do it for ind2=0.
                      nil)              ; $$$$$$ (and eobp  (< updown 0))) ; No, loop OK here.
            (setq f-or-t+bds  (prog1 (if predicate
                                         (funcall fn thing predicate syntax-table)
                                       (funcall fn thing syntax-table))
                                (constrain-to-field nil opoint))
                  bolp        (or (eq (field-beginning nil) (point))  (bolp))
                  eolp        (or (eq (field-end nil) (point))        (eolp))
                  ind1        0)
            (save-excursion
              ;; IND1: Loop over chars in same line (alternately left and right),
              ;; until either found thing/form or both line limits reached.
              (while (and (not (and bolp eolp))  (<= ind1 max-x)  (not f-or-t+bds))
                (unless bolp (save-excursion ; Left.
                               (setq bolp        (prog1 (forward-char-same-line (- ind1))
                                                   (constrain-to-field nil opoint))
                                     f-or-t+bds  (if predicate
                                                     (funcall fn thing predicate syntax-table)
                                                   (funcall fn thing syntax-table)))
                               (constrain-to-field nil opoint)))
                (unless (or f-or-t+bds  eolp) ; Right.
                  (save-excursion
                    (setq eolp        (prog1 (forward-char-same-line ind1)
                                        (constrain-to-field nil opoint))
                          f-or-t+bds  (if predicate
                                          (funcall fn thing predicate syntax-table)
                                        (funcall fn thing syntax-table)))
                    (constrain-to-field nil opoint)))
                (setq ind1  (1+ ind1)))
              (setq bobp  (or (eq (field-beginning nil) (point))
                              (bobp)
                              (< max-y 2)) ; If only one line, fake `bobp'.
                    eobp  (or (eq (field-end nil) (point))
                              (eobp)
                              (< max-y 2)))))) ; If only one line, fake `eobp'.
        ;; Increase search line distance every second time (once up, once down).
        (when (and (> max-y 1)  (or (< updown 0)  (zerop ind2))) ; 0,1,1,2,2...
          (setq ind2  (1+ ind2))))
      f-or-t+bds)))

(defun tap-bounds-of-thing-nearest-point (thing &optional syntax-table)
  "Return the start and end locations for the THING nearest point.
Return a consp (START . END), where START /= END.
Return nil if no such THING is found.

\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

Optional arg SYNTAX-TABLE is a syntax table to use."
  (let ((thing+bds  (tap-thing-nearest-point-with-bounds thing syntax-table)))
    (and thing+bds
         (cdr thing+bds))))

(defun tap-thing-nearest-point (thing &optional syntax-table)
  "Return the THING nearest point, if any, else nil.
The search for the THING is bounded by options
`tap-near-point-x-distance' and `tap-near-point-y-distance'.

\"Nearest\" to point is determined as follows:

  The nearest THING on the same line is returned, if there is any.
      Between two THINGs equidistant from point on the same line, the
      leftmost is considered nearer.
  Otherwise, neighboring lines are tried in sequence:
  previous, next, 2nd previous, 2nd next, 3rd previous, 3rd next, etc.
      This means that between two THINGs equidistant from point in
      lines above and below it, the THING in the line above point
      (previous Nth) is considered nearer to it.

However, for some THINGs whitespace between THINGs might be considered
insignificant, as well as the amount of it.  This means that if point
is between two THINGs and surrounded by whitespace then the \"nearest\"
THING returned might not be the one that is absolutely closest.

See also `tap-thing-at-point'.

Optional arg SYNTAX-TABLE is a syntax table to use."
  (let ((thing+bds  (tap-thing-nearest-point-with-bounds thing syntax-table)))
    (and thing+bds
         (car thing+bds))))
 
;;; FORMS, SEXPS -----------------------------------------------------

(defun tap-form-at-point-with-bounds (&optional thing predicate syntax-table)
  "Return the Lisp THING at point, plus its bounds: (FORM START . END).
Return nil if no form is found.
THING must be readable as a Lisp entity, to give FORM - see
 `tap-form-at-point'.
START and END are the buffer positions of FORM.
Optional args:
  THING is the kind of form desired (default: `sexp').
  PREDICATE is a predicate that the form must satisfy to qualify.
  SYNTAX-TABLE is a syntax table to use."
  (condition-case nil                   ; E.g. error if tries to read a dot (`.').
      (let* ((thing+bds  (tap-thing-at-point-with-bounds (or thing  'sexp) syntax-table))
             (bounds     (cdr thing+bds))
             (sexp       (and bounds  (tap-read-from-whole-string (car thing+bds)))))
        (and bounds  (or (not predicate)  (funcall predicate sexp))
             (cons sexp bounds)))
    (error nil)))

;; Essentially an alias for the default case.
(defun tap-sexp-at-point-with-bounds (&optional predicate syntax-table)
  "Return the sexp at point, plus its bounds: (SEXP START . END)
Return nil if no sexp is found.
SEXP is an Emacs Lisp entity, the result of reading the textual sexp
at point.  See `sexp-at-point'.

START and END are the buffer positions of SEXP.
Optional args are the same as for `tap-form-at-point-with-bounds'."
  (tap-form-at-point-with-bounds 'sexp predicate syntax-table))

(defun tap-bounds-of-form-at-point (&optional thing predicate syntax-table)
  "Return the start and end locations for the THING at point.
THING must be readable as a Lisp entity - see `tap-form-at-point'.
Return a consp (START . END), where START /= END.
Return nil if no such THING is found.
Optional args:
  THING is the kind of form desired (default: `sexp').
  PREDICATE is a predicate that the form must satisfy to qualify.
  SYNTAX-TABLE is a syntax table to use."
  (let ((form+bds  (tap-form-at-point-with-bounds thing predicate syntax-table)))
    (and form+bds
         (cdr form+bds))))

;; Essentially an alias for the default case.
(defun tap-bounds-of-sexp-at-point (&optional predicate syntax-table)
  "Return the start and end locations for the sexp at point.
SEXP is an Emacs Lisp entity, the result of reading the textual sexp
at point.  See `sexp-at-point'.
Return a consp (START . END), where START /= END.
Return nil if no sexp is found.
Optional args are the same as for `tap-bounds-of-form-at-point'."
  (tap-bounds-of-form-at-point 'sexp predicate syntax-table))


;; If you invoke `tap-redefine-std-fns', this def replaces the original in `thingatpt.el'.
;;
;; Add optional argument SYNTAX-TABLE.
;;
(defun tap-form-at-point (&optional thing predicate syntax-table)
  "Return the form at point, if any, else nil.
This is an Emacs Lisp entity, not necessarily a string.  THING must be
readable as a Lisp entity, or else nil is returned.

Reading THING and returning the resulting Lisp entity is the main
difference between this function and `tap-thing-at-point-as-string'.
The other difference is the use of PREDICATE.

Optional args:
  THING is the kind of form desired (default: `sexp').
  PREDICATE is a predicate that the form must satisfy to qualify.
  SYNTAX-TABLE is a syntax table to use."
  (let ((form  (condition-case nil
                   (tap-read-from-whole-string
                    (tap-thing-at-point-as-string (or thing  'sexp) nil syntax-table))
                 (error nil))))
    (and (or (not predicate)  (funcall predicate form))
         form)))

(defun tap-form-nearest-point-with-bounds (&optional thing predicate syntax-table)
  "Return the form nearest point, plus its bounds: (FORM START . END).
Return nil if no form is found.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

FORM is from `tap-form-nearest-point'.
START and END are the buffer positions of FORM.
Optional args:
  THING is the kind of form desired (default: `sexp').
  PREDICATE is a predicate that the form must satisfy to qualify.
  SYNTAX-TABLE is a syntax table to use."
  (tap-thing/form-nearest-point-with-bounds #'tap-form-at-point-with-bounds
                                            thing predicate syntax-table))

;; Essentially an alias for the default case.
(defun tap-sexp-nearest-point-with-bounds (&optional predicate syntax-table)
  "Return the sexp nearest point, plus its bounds: (SEXP START . END).
Return nil if no sexp is found.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

SEXP is an Emacs Lisp entity, the result of reading a textual sexp
 near point.  See `tap-sexp-nearest-point'.
START and END are the buffer positions of SEXP.
Optional args are the same as for
`tap-form-nearest-point-with-bounds'."
  (tap-form-nearest-point-with-bounds 'sexp predicate syntax-table))

(defun tap-bounds-of-form-nearest-point (&optional thing predicate syntax-table)
  "Return the start and end locations for the form nearest point.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.
See `tap-form-nearest-point'.

Return a consp (START . END), where START /= END.
Return nil if no form is found.
Arguments are the same as for `tap-form-nearest-point-with-bounds'."
  (let ((form+bds  (tap-form-nearest-point-with-bounds thing predicate syntax-table)))
    (and form+bds
         (cdr form+bds))))

;; Essentially an alias for the default case.
(defun tap-bounds-of-sexp-nearest-point (&optional predicate syntax-table)
  "Return the start and end locations for the sexp nearest point.
See `tap-sexp-nearest-point'.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

Return a consp (START . END), where START /= END.
Return nil if no form is found.
Optional args are the same as for `tap-bounds-of-sexp-nearest-point'."
  (tap-bounds-of-form-nearest-point 'sexp predicate syntax-table))

(defun tap-form-nearest-point (&optional thing predicate syntax-table)
  "Return the form nearest point, if any, else nil.
This is an Emacs Lisp entity, not necessarily a string.  THING must be
readable as a Lisp entity, or else nil is returned.

\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

Reading THING and returning the resulting Lisp entity is the main
difference between this function and `tap-thing-nearest-point'.  The
other difference is the use of PREDICATE.

Optional args:
  THING is the kind of form desired (default: `sexp').
  PREDICATE is a predicate that the form must satisfy to qualify.
  SYNTAX-TABLE is a syntax table to use."
  (let ((form+bds  (tap-form-nearest-point-with-bounds thing predicate syntax-table)))
    (and form+bds
         (car form+bds))))
 
;;; SYMBOLS ----------------------------------------------------------

(defun tap-symbol-at-point-with-bounds ()
  "Return the Emacs Lisp symbol nearest point, plus its bounds.
Return (SYMBOL START . END), or nil if no symbol is found.
Note that nil is also returned if the symbol at point is `nil'.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

SYMBOL is the symbol from `tap-symbol-at-point'.
START and END are the buffer positions of SYMBOL."
  (tap-form-at-point-with-bounds 'symbol 'symbolp emacs-lisp-mode-syntax-table))

(defun tap-bounds-of-symbol-at-point ()
  "Return the start and end locations for the Emacs Lisp symbol at point.
See `tap-symbol-at-point'.
Return a consp (START . END), where START /= END.
Return nil if no symbol is found or if the symbol at point is `nil'."
  (let ((symb+bds  (tap-symbol-at-point-with-bounds)))
    (and symb+bds
         (cdr symb+bds))))


;; If you invoke `tap-redefine-std-fns', this def replaces the original in `thingatpt.el'.
;;
;; Use `tap-form-at-point', passing `emacs-lisp-mode-syntax-table'.
;; (Vanilla `symbol-at-point' interns (thing-at-point 'symbol).)
;;
(defun tap-symbol-at-point ()
  "Return the Emacs Lisp symbol at point, or nil if none.
Note that nil is also returned if the symbol at point is `nil'.

Some related functions:
 `tap-symbol-nearest-point' returns the symbol nearest point,
   or nil if none.
 `tap-symbol-name-nearest-point' returns the name of
   `tap-symbol-nearest-point' as a string,
   or nil if none.
 `symbol-name-before-point' returns the string naming the symbol at or
   before point (even if it is on a previous line).
 `word-at-point' returns the word at point,
   or nil if none.
 `word-before-point' returns the word (a string) at or before cursor.
 `tap-word-nearest-point' returns the word nearest point,
   or nil if none.
All but the first return strings, not (non-nil) symbols."
  ;; Needs to satisfy both: (1) symbol syntax in Emacs Lisp mode, and (2) be interned.
  (tap-form-at-point 'symbol 'symbolp emacs-lisp-mode-syntax-table))

(defun tap-symbol-nearest-point-with-bounds (&optional non-nil)
  "Return (SYMBOL START . END) with START and END of SYMBOL.
SYMBOL is the `tap-symbol-nearest-point'.
If optional arg NON-NIL is non-nil, then the nearest symbol other
  than `nil' is sought.
Return nil if no such Emacs Lisp symbol is found.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'."
  (tap-form-nearest-point-with-bounds 'symbol
                                      (if non-nil
                                          (lambda (sym) (and sym  (symbolp sym)))
                                        'symbolp)
                                      emacs-lisp-mode-syntax-table))

(defun tap-bounds-of-symbol-nearest-point (&optional non-nil)
  "Return the start and end locations for the Lisp symbol nearest point.
See `tap-symbol-nearest-point'.
Return a consp (START . END), where START /= END.
Return nil if no such Emacs Lisp symbol is found.
If optional arg NON-NIL is non-nil, then seek the nearest symbol other
than `nil'.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'."
  (let ((symb+bds  (tap-symbol-nearest-point-with-bounds non-nil)))
    (and symb+bds
         (cdr symb+bds))))

(defun tap-symbol-nearest-point (&optional non-nil)
  "Return the Emacs Lisp symbol nearest point, or nil if none.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

If optional arg NON-NIL is non-nil, then seek the nearest symbol other
  than `nil'.

Some related functions:
 `tap-symbol-at-point' returns the symbol under the cursor,
   or nil if none.
 `tap-symbol-name-nearest-point' returns the name of
   `tap-symbol-nearest-point' as a string,
   or nil if none.
 `symbol-name-before-point' returns the string naming the symbol at or
   before the cursor (even if it is on a previous line).
 `word-at-point' returns the word at point,
   or nil if none.
 `word-before-point' returns the word at or before point as a string.
 `tap-word-nearest-point' returns the word nearest point,
   or nil if none.
All but the first return strings, not (non-nil) symbols."
  (let ((symb+bds  (tap-symbol-nearest-point-with-bounds non-nil)))
    (and symb+bds
         (car symb+bds))))

(defun tap-non-nil-symbol-nearest-point ()
  "Return the Emacs Lisp symbol other than `nil' nearest point.
Return nil if none is found.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

Some related functions:
 `tap-symbol-at-point' returns the symbol under the cursor,
   or nil if none.
 `tap-symbol-name-nearest-point' returns the name of
   `tap-symbol-nearest-point' as a string,
   or nil if none.
 `symbol-name-before-point' returns the string naming the symbol at or
   before the cursor (even if it is on a previous line).
 `word-at-point' returns the word at point,
   or nil if none.
 `tap-word-nearest-point' returns the word nearest point,
   or nil if none.
 `word-before-point' returns the word at or before point as a string.
All but the first return strings, not (non-nil) symbols."
  (let ((symb+bds  (tap-symbol-nearest-point-with-bounds t)))
    (and symb+bds
         (car symb+bds))))
 
;;; LISTS ------------------------------------------------------------

(defun tap-list-at/nearest-point-with-bounds (at/near &optional up unquotedp)
  "Helper for `tap-list-at-point-with-bounds' and similar functions.
AT/NEAR is a function called to grab the initial list and its bounds.
UP (default: 0) is the number of list levels to go up to start with.
Non-nil UNQUOTEDP means remove the car if it is `quote' or
 `backquote-backquote-symbol'.

Return (LIST START . END) with START and END of the non-empty LIST.
Return nil if no non-empty list is found.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'."
  (save-excursion
    (unless (eq at/near 'tap-sexp-at-point-with-bounds)
      ;; Skip over whitespace, including newlines.
      ;; "Nearest" here treats all contiguous whitespace as if it were a single char.
      (cond ((tap-looking-at-p   "\\(\\s-*\\|[\n]*\\)\\s(") (skip-syntax-forward "->"))
            ((tap-looking-back-p "\\s)\\(\\s-*\\|[\n]*\\)") (skip-syntax-backward "->"))))
    (let (strg-end)
      (while (setq strg-end  (if (> emacs-major-version 24)
                                 (nth 3 (syntax-ppss)) ; Emacs bug #20732
                               (in-string-p)))
        (skip-syntax-forward "^\"")     ; Skip past string element of list.
        (skip-syntax-forward "\"")))    ; Skip past new string opening, `"', into next string.
    (let ((sexp+bnds  (funcall at/near)))
      (condition-case nil               ; Handle an `up-list' error.
          (progn
            (when up
              (up-list (- up))
              (setq sexp+bnds  (tap-sexp-at-point-with-bounds)))
            (while (or (not (consp (car sexp+bnds)))
                       (and (memq (caar sexp+bnds) (list backquote-backquote-symbol 'quote))
                            (not (listp (cadr (car sexp+bnds))))))
              (up-list -1)
              (setq sexp+bnds  (tap-sexp-at-point-with-bounds)))
            (when (and unquotedp  (consp (car sexp+bnds))
                       (memq (caar sexp+bnds) (list backquote-backquote-symbol 'quote)))
              (cond ((eq 'quote (caar sexp+bnds))
                     (setq sexp+bnds  (cons (cadr (car sexp+bnds))
                                            (cons (+ 5 (cadr sexp+bnds)) (cddr sexp+bnds)))))
                    ((eq backquote-backquote-symbol (caar sexp+bnds))
                     (setq sexp+bnds  (cons (cadr (car sexp+bnds))
                                            (cons (+ 1 (cadr sexp+bnds)) (cddr sexp+bnds)))))))
            (while (not (consp (car sexp+bnds)))
              (up-list -1)
              (setq sexp+bnds  (tap-sexp-at-point-with-bounds))))
        (error (setq sexp+bnds  nil)))
      sexp+bnds)))

(defun tap-list-at-point-with-bounds (&optional up unquotedp)
  "Return (LIST START . END), boundaries of the `tap-list-at-point'.
Return nil if no non-empty list is found.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

UP (default: 0) is the number of list levels to go up to start with.
Non-nil UNQUOTEDP means remove the car if it is `quote' or
 `backquote-backquote-symbol'."
  (tap-list-at/nearest-point-with-bounds 'tap-sexp-at-point-with-bounds up unquotedp))

(defun tap-list-nearest-point-with-bounds (&optional up unquotedp)
  "Return (LIST START . END), boundaries of the `tap-list-nearest-point'.
Return nil if no non-empty list is found.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

UP (default: 0) is the number of list levels to go up to start with.
Non-nil UNQUOTEDP means remove the car if it is `quote' or
 `backquote-backquote-symbol'."
  (tap-list-at/nearest-point-with-bounds 'tap-sexp-nearest-point-with-bounds up unquotedp))

(defun tap-bounds-of-list-at-point (&optional up unquotedp)
  "Return the start and end locations for the non-empty list at point.
See `tap-list-at-point'.
Return a consp (START . END), where START /= END.
Return nil if no non-empty list is found.
Optional args:
  UP (default: 0) is the number of list levels to go up to start with.
  Non-nil UNQUOTEDP means remove the car if it is `quote' or
 `backquote-backquote-symbol'."
  (let ((thing+bds  (tap-list-at-point-with-bounds up unquotedp)))
    (and thing+bds
         (cdr thing+bds))))

(defun tap-bounds-of-list-nearest-point (&optional up unquotedp)
  "Return start and end locations for the non-empty list nearest point.
See `tap-list-nearest-point'.
Return a consp (START . END), where START /= END.
Return nil if no non-empty list is found.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

Optional args:
  UP (default: 0) is the number of list levels to go up to start with.
  Non-nil UNQUOTEDP means remove the car if it is `quote' or
 `backquote-backquote-symbol'."
  (let ((thing+bds  (tap-list-nearest-point-with-bounds up unquotedp)))
    (and thing+bds
         (cdr thing+bds))))


(put 'list 'tap-thing-at-point 'tap-list-at-point)
(put 'list 'tap-bounds-of-thing-at-point 'tap-bounds-of-list-at-point)


;; If you invoke `tap-redefine-std-fns' or `tap-put-thing-at-point-props', this definition
;; replaces the original in `thingatpt.el'.
;;
;; 1. Added optional arg UP.
;; 2. Better, consistent behavior.
;; 3. Let `(tap-)bounds-of-thing-at-point' do its job.
;;
(defun tap-list-at-point (&optional up)
  "Return the non-nil list at point, or nil if none.
If inside a list, return the enclosing list.

UP (default: 0) is the number of list levels to go up to start with.

Note: If point is inside a string that is inside a list:
 This can sometimes return nil.
 This can sometimes return an incorrect list value if the string or
 nearby strings contain parens.
 (These are limitations of function `up-list'.)"
  (let ((list+bds  (tap-list-at-point-with-bounds up)))
    (and list+bds  (car list+bds))))


(put 'unquoted-list 'thing-at-point     'tap-unquoted-list-at-point)
(put 'unquoted-list 'tap-thing-at-point 'tap-unquoted-list-at-point)

(defun tap-unquoted-list-at-point (&optional up)
  "Return the non-nil list at point, or nil if none.
Same as `tap-list-at-point', but removes the car if it is `quote' or
 `backquote-backquote-symbol' (\`).
UP (default: 0) is the number of list levels to go up to start with."
  (let ((list+bds  (tap-list-at-point-with-bounds up 'UNQUOTED)))
    (and list+bds  (car list+bds))))

;;; This simple definition is nowhere near as good as the one below.
;;;
;;; (defun tap-list-nearest-point (&optional syntax-table)
;;;   "Return the list nearest to point, if any, else nil.
;;; This does not distinguish between finding no list and finding
;;; the empty list.  \"Nearest\" to point is determined as for
;;; `tap-thing-nearest-point'.
;;; Optional arg SYNTAX-TABLE is a syntax table to use."
;;;   (tap-form-nearest-point 'list 'listp syntax-table))

(defun tap-list-nearest-point (&optional up)
  "Return the non-nil list nearest point, or nil if none.
Same as `tap-list-at-point', but returns the nearest list.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

UP (default: 0) is the number of list levels to go up to start with."
  (let ((list+bds  (tap-list-nearest-point-with-bounds up)))
    (and list+bds  (car list+bds))))

(defun tap-unquoted-list-nearest-point (&optional up)
  "Return the non-nil list nearest point, or nil if none.
UP (default: 0) is the number of list levels to go up to start with.
Same as `tap-list-nearest-point', but removes the car if it is
`quote' or `backquote-backquote-symbol' (\`)."
  (let ((list+bds  (tap-list-nearest-point-with-bounds up 'UNQUOTED)))
    (and list+bds
         (car list+bds))))


;; The following functions return a string, not a list.
;; They can be useful to pull a sexp into minibuffer.

(defun tap-list-nearest-point-as-string (&optional up)
  "Return a string of the non-nil list nearest point, or \"\" if none.
If not \"\", the list in the string is what is returned by
 `tap-list-nearest-point'.
UP (default: 0) is the number of list levels to go up to start with."
  (let ((list+bds  (tap-list-nearest-point-with-bounds up)))
    (if list+bds (format "%s" (car list+bds)) "")))

(defun tap-unquoted-list-nearest-point-as-string (&optional up)
  "Return a string of the non-nil list nearest point, or \"\" if none.
If not \"\", the list in the string is what is returned by
 `tap-unquoted-list-nearest-point'.
UP (default: 0) is the number of list levels to go up to start with."
  (let ((list+bds  (tap-list-nearest-point-with-bounds up 'UNQUOTED)))
    (if list+bds (format "%s" (car list+bds)) "")))

(defun tap-bounds-of-list-contents-at-point ()
  "Return the start and end locations for the list contents at point.
Same as `tap-bounds-of-list-at-point', except that this does not
include the enclosing `(' and `)' characters."
  (let ((full  (tap-bounds-of-list-at-point)))
    (and full  (cons (1+ (car full)) (1- (cdr full))))))

;; Add this so that, for example, `thgcmd-defined-thing-p' in
;; `thing-cmds.el' recognizes `list-contents' as a THING.
(put 'list-contents 'bounds-of-thing-at-point 'tap-bounds-of-list-contents-at-point)

(defun tap-list-contents-at-point ()
  "Return the contents of the list at point as a string, or nil if none.
Same as `tap-list-at-point-as-string', except that this does not
include the enclosing `(' and `)' characters."
  (let ((bounds  (tap-bounds-of-list-contents-at-point)))
    (and bounds  (buffer-substring (car bounds) (cdr bounds)))))

(defun tap-list-contents-nearest-point ()
  "Return the contents of the list nearest point as a string, or nil.
See `tap-list-contents-at-point'.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'."
  (let ((full  (tap-bounds-of-thing-nearest-point 'list)))
    (and full  (buffer-substring (1+ (car full)) (1- (cdr full))))))
 
;;; SYMBOL NAMES, WORDS, SENTENCES, etc. -----------------------


(put 'non-nil-symbol-name 'thing-at-point     'tap-non-nil-symbol-name-at-point)
(put 'non-nil-symbol-name 'tap-thing-at-point 'tap-non-nil-symbol-name-at-point)

(defun tap-non-nil-symbol-name-at-point ()
  "String naming a non-nil Emacs Lisp symbol at point, or nil if none."
  (let ((name  (tap-thing-at-point 'symbol nil emacs-lisp-mode-syntax-table)))
    (and (not (equal "nil" name))  name)))


(put 'symbol-name 'thing-at-point     'tap-symbol-name-at-point)
(put 'symbol-name 'tap-thing-at-point 'tap-symbol-name-at-point)

(defun tap-symbol-name-at-point ()
  "String naming the Emacs Lisp symbol at point, or nil if none.
The symbol might be `nil', so that \"nil\" is returned.
See also `tap-non-nil-symbol-name-at-point'."
  (tap-thing-at-point 'symbol emacs-lisp-mode-syntax-table))

(defun tap-symbol-name-nearest-point ()
  "String naming the Emacs Lisp symbol nearest point, or nil if none.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'."
  ;; We do it this way to be able to pick symbol `nil' (name "nil").
  (let ((symb+bds  (tap-symbol-nearest-point-with-bounds nil)))
    (and symb+bds (symbol-name (car symb+bds)))))

(defun tap-non-nil-symbol-name-nearest-point ()
  "String naming the Emacs Lisp symbol nearest point, or nil if none.
Returns the name of the nearest symbol other than `nil'.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'."
  (let ((symb+bds  (tap-symbol-nearest-point-with-bounds t)))
    (and symb+bds (symbol-name (car symb+bds)))))

(defun tap-region-or-non-nil-symbol-name-nearest-point (&optional quote-it-p)
  "Return non-empty active region or symbol nearest point, or nil if none.
Non-nil QUOTE-IT-P means wrap the region text in double-quotes (\").
The name of the nearest symbol other than `nil' is used.
See `tap-non-nil-symbol-name-nearest-point'."
  (if (and transient-mark-mode mark-active
           (not (eq (region-beginning) (region-end))))
      (let ((region-text  (buffer-substring-no-properties (region-beginning) (region-end))))
        (if quote-it-p
            (concat "\"" region-text "\"")
          region-text))
    (tap-non-nil-symbol-name-nearest-point)))

(defun tap-word-nearest-point (&optional syntax-table)
  "Return the word (a string) nearest to point, if any, else nil.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

Optional arg SYNTAX-TABLE is a syntax table to use."
  (tap-thing-nearest-point 'word syntax-table))

(defun tap-region-or-word-nearest-point (&optional syntax-table)
  "Return non-empty active region or word nearest point.
See `tap-word-nearest-point'."
  (if (and transient-mark-mode mark-active
           (not (eq (region-beginning) (region-end))))
      (buffer-substring-no-properties (region-beginning) (region-end))
    (tap-word-nearest-point syntax-table)))


(put 'region-or-word 'thing-at-point     'tap-region-or-word-at-point)
(put 'region-or-word 'tap-thing-at-point 'tap-region-or-word-at-point)

(defun tap-region-or-word-at-point ()
  "Return non-empty active region or word at or adjacent to point."
  (if (and transient-mark-mode mark-active
           (not (eq (region-beginning) (region-end))))
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if (> emacs-major-version 21)
        (current-word 'STRICT 'REALLY-WORD)
      (current-word 'STRICT))))


(when (fboundp 'color-defined-p)        ; Emacs 21+
  (put 'color 'thing-at-point               'tap-color-at-point)
  (put 'color 'tap-thing-at-point           'tap-color-at-point)
  (put 'color 'bounds-of-thing-at-point     'tap-bounds-of-color-at-point)
  (put 'color 'tap-bounds-of-thing-at-point 'tap-bounds-of-color-at-point)

  (defun tap-color-at-point ()
    "Return the color name or RGB code (with prefix `#') at point."
    (let ((word  (with-syntax-table (copy-syntax-table (syntax-table))
                   (modify-syntax-entry ?# "w") ; Make `#' a word constituent.
                   (word-at-point))))
      (and word  (color-defined-p word)  word)))

  (defun tap-bounds-of-color-at-point ()
    "Return the bounds of the color name at point.
The color name can also be an RGB code (with prefix `#').
Return nil if no color name is found."
    (let ((word+bnds  (with-syntax-table (copy-syntax-table (syntax-table))
                        (modify-syntax-entry ?# "w") ; Make `#' a word constituent.
                        (thing-at-point-with-bounds 'word))))
      (and word+bnds  (color-defined-p (car word+bnds))  (cdr word+bnds))))

  (defun tap-color-nearest-point-with-bounds ()
    "Return the color name nearest point, plus its bounds: (COLOR START . END).
Return nil if no color name is found.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

COLOR is a color name or RGB code (with prefix `#').
 See `tap-color-nearest-point'.
START and END are the buffer positions of COLOR."
    (tap-thing-nearest-point-with-bounds 'color))

  (defun tap-color-nearest-point ()
    "Return the color name or RGB code (with prefix `#') nearest point.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'."
    (let ((color+bds  (tap-color-nearest-point-with-bounds)))
      (and color+bds  (car color+bds)))))

(defun tap-sentence-nearest-point (&optional syntax-table)
  "Return the sentence (a string) nearest to point, if any, else \"\".
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

Optional arg SYNTAX-TABLE is a syntax table to use."
  (tap-thing-nearest-point 'sentence syntax-table))

(defun tap-sexp-nearest-point (&optional syntax-table)
  "Return the sexp nearest point, if any, else \"\".
This is an Emacs Lisp entity, the result of reading a textual sexp
near point.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

Optional arg SYNTAX-TABLE is a syntax table to use."
  (tap-form-nearest-point 'sexp nil syntax-table))

(defun tap-number-nearest-point (&optional syntax-table)
  "Return the number nearest to point, if any, else nil.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.

Optional arg SYNTAX-TABLE is a syntax table to use."
  (tap-form-nearest-point 'sexp 'numberp syntax-table))


;; `defun' type.  These are defined in recent `thingatpt.el', but not for older versions.
(unless (get 'defun 'beginning-op) (put 'defun 'beginning-op 'beginning-of-defun))
(unless (get 'defun 'end-op)       (put 'defun 'end-op       'end-of-defun))
(unless (get 'defun 'forward-op)   (put 'defun 'forward-op   'end-of-defun))


(put 'number 'tap-bounds-of-thing-at-point 'tap-bounds-of-number-at-point)

(defun tap-bounds-of-number-at-point ()
  "Return the bounds of the number represented by the numeral at point.
Return nil if none is found."
  (and (number-at-point)  (tap-bounds-of-thing-at-point 'sexp)))


;;; `number-at-point' returns the char value when point is on char syntax.
;;; E.g., when on ?A it returns 65 (not nil); when on ?\A-\^@ it returns 4194304.
;;; So we add these functions, which do what you would normally expect.


(put 'decimal-number 'bounds-of-thing-at-point     'tap-bounds-of-number-at-point-decimal)
(put 'decimal-number 'tap-bounds-of-thing-at-point 'tap-bounds-of-number-at-point-decimal)

(defun tap-bounds-of-number-at-point-decimal ()
  "Return bounds of number represented by the decimal numeral at point.
Return nil if none is found."
  (and (tap-number-at-point-decimal)  (tap-bounds-of-thing-at-point 'sexp)))


(put 'decimal-number 'thing-at-point     'tap-number-at-point-decimal)
(put 'decimal-number 'tap-thing-at-point 'tap-number-at-point-decimal)

(defalias 'decimal-number-at-point 'tap-number-at-point-decimal)
(defun tap-number-at-point-decimal ()
  "Return the number represented by the decimal numeral at point.
Return nil if none is found."
  (let ((strg  (tap-thing-at-point 'sexp)))
    (and (stringp strg)
         (tap-string-match-p "\\`[0-9]+\\'" strg)
         (string-to-number strg))))


(put 'hex-number 'bounds-of-thing-at-point     'tap-bounds-of-number-at-point-hex)
(put 'hex-number 'tap-bounds-of-thing-at-point 'tap-bounds-of-number-at-point-hex)

(defun tap-bounds-of-number-at-point-hex ()
  "Return bounds of number represented by the hexadecimal numeral at point.
Return nil if none is found."
  (and (tap-number-at-point-hex)  (tap-bounds-of-thing-at-point 'sexp)))


(put 'hex-number 'thing-at-point     'tap-number-at-point-hex)
(put 'hex-number 'tap-thing-at-point 'tap-number-at-point-hex)

(defalias 'hex-number-at-point 'tap-number-at-point-hex)
(defun tap-number-at-point-hex ()
  "Return the number represented by the hex numeral at point.
Return nil if none is found."
  (let ((strg  (tap-thing-at-point 'sexp)))
    (and (stringp strg)
         (tap-string-match-p "\\`[0-9a-fA-F]+\\'" strg)
         (string-to-number strg 16))))


(when (fboundp 'syntax-ppss)            ; Based loosely on `comint-extract-string'.

  (put 'string 'bounds-of-thing-at-point     'tap-bounds-of-string-at-point)
  (put 'string 'tap-bounds-of-thing-at-point 'tap-bounds-of-string-at-point)

  (defun tap-bounds-of-string-at-point ()
    "Return the start and end locations for the string at point.
Return a consp (START . END), where START /= END.
Return nil if no string is found at point.

NOTE: The ENCLOSING `\"' CHARACTERS ARE COUNTED - they are part of the
string syntax.  See `tap-string-at-point'."
    (save-excursion
      (let (syntax beg end)
        (if (not (eq ?\" (char-after)))
            (setq syntax  (syntax-ppss))
          (or (progn (forward-char)  (setq syntax  (syntax-ppss))  (nth 3 syntax))
              (progn (backward-char) (setq syntax  (syntax-ppss))  (nth 3 syntax))))
        (and (nth 3 syntax)
             (condition-case ()
                 (setq beg  (nth 8 syntax)
                       end  (progn (goto-char (nth 8 syntax)) (forward-sexp) (point)))
               (error nil))
             (cons beg end)))))

  (put 'string 'thing-at-point     'tap-string-at-point)
  (put 'string 'tap-thing-at-point 'tap-string-at-point)

  (defun tap-string-at-point ()
    "Return the string at point, or nil if there is no string at point.
Put roughly, there is a string at point if point is on or after a \"
and on or before a second \".

NOTE: The text returned as a string INCLUDES THE ENCLOSING `\"' chars,
which are part of the string syntax (the string THING).  If the result
is read, then the string contents, i.e., the text without the
delimiting `\"' chars, is returned from that reading.  You can obtain
those string contents directly using `tap-string-contents-at-point'."
    (let ((bounds  (tap-bounds-of-string-at-point)))
      (and bounds  (buffer-substring (car bounds) (cdr bounds)))))

  (defun tap-string-nearest-point ()
    "Return the string nearest point, or nil if there is none.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.
See `tap-string-at-point'."
    (tap-thing-nearest-point 'string))

  (defun tap-bounds-of-string-contents-at-point ()
    "Return the start and end locations for the string contents at point.
Same as `tap-bounds-of-string-at-point', except that this does not
include the enclosing `\"' characters."
    (let ((full  (tap-bounds-of-string-at-point)))
      (and full  (cons (1+ (car full)) (1- (cdr full))))))

  ;; Add this so that, for example, `thgcmd-defined-thing-p' in
  ;; `thing-cmds.el' recognizes `string-contents' as a THING.
  (put 'string-contents 'bounds-of-thing-at-point 'tap-bounds-of-string-contents-at-point)

  (defun tap-string-contents-at-point ()
    "Return the contents of the string at point, or nil if none.
Same as `tap-string-at-point', except that this does not include the
enclosing `\"' characters."
    (let ((bounds  (tap-bounds-of-string-contents-at-point)))
      (and bounds  (buffer-substring (car bounds) (cdr bounds)))))

  (defun tap-string-contents-nearest-point ()
    "Return the contents of the string nearest point, or nil if none.
\"Nearest\" to point is determined as for `tap-thing-nearest-point'.
See `tap-string-contents-at-point'."
    (let ((full  (tap-bounds-of-thing-nearest-point 'string)))
      (and full  (buffer-substring (1+ (car full)) (1- (cdr full)))))))
 
;;; COMMANDS ---------------------------------------------------------


;; This REPLACES ORIGINAL definitions in `thingatpt.el'.
;;
;;;###autoload
(defun tap-put-thing-at-point-props ()
  "Change `(bounds-of-)thing-at-point' properties for standard things.
This makes some things normally handled by `thingatpt.el' be handled
instead by functions defined in `thingatpt+.el'.

This also affects some things that are handled by `thingatpt.el' in
another way, not by setting these properties."
  (interactive)

  ;; This one is set in `thingatpt.el'.
  (put 'list   'bounds-of-thing-at-point 'tap-bounds-of-list-at-point)

  ;; These are not set in `thingatpt.el', but a function for the THING is defined there.
  (put 'list   'thing-at-point           'tap-list-at-point)
  (put 'number 'thing-at-point           'number-at-point) ; In vanilla after 2012-10-12.
  (put 'number 'bounds-of-thing-at-point 'tap-bounds-of-number-at-point)
  t)                                    ; Return non-nil so can use with `and' etc.

;;;###autoload
(defun tap-redefine-std-fns ()
  "Redefine some standard `thingatpt.el' functions, to fix them.
The standard functions replaced are these:
 `bounds-of-thing-at-point' - Better behavior.
                              Accept optional arg SYNTAX-TABLE.
 `form-at-point'            - Accept optional arg SYNTAX-TABLE.
 `list-at-point'            - Better behavior.
                              Accept optional arg SYNTAX-TABLE.
 `symbol-at-point'          - Use `emacs-lisp-mode-syntax-table'.
 `thing-at-point'           - Ensure it returns a string or nil.
                              Accept optional arg SYNTAX-TABLE.
 `thing-at-point-bounds-of-list-at-point'
                            - Better behavior.  Accept optional
                              args UP and UNQUOTEDP."
  (interactive)

  ;; REPLACE ORIGINAL in `thingatpt.el'.
  ;;
  ;; 1. Fix Emacs bug #8667 (do not return an empty thing).
  ;; 2. Add optional argument SYNTAX-TABLE.
  ;;
  ;; NOTE: All of the other functions here are based on this function.
  ;;
  (defalias 'bounds-of-thing-at-point 'tap-bounds-of-thing-at-point)

  ;; REPLACE ORIGINAL in `thingatpt.el'.
  ;;
  ;; Add optional argument SYNTAX-TABLE.
  ;;
  (defalias 'form-at-point 'tap-form-at-point)

  ;; REPLACE ORIGINAL defined in `thingatpt.el'.
  ;;
  ;; 1. Added optional arg UP.
  ;; 2. Better, consistent behavior.
  ;; 3. Let `tap-bounds-of-thing-at-point' do its job.
  ;;
  (defalias 'list-at-point 'tap-list-at-point)

  ;; REPLACE ORIGINAL in `thingatpt.el':
  ;;
  ;; Original defn: (defun symbol-at-point () (form-at-point 'sexp 'symbolp))
  ;; With point on toto in "`toto'" (in Emacs Lisp mode), that definition
  ;; returned `toto, not toto.  With point on toto in "`toto'," (note comma),
  ;; that definition returned nil.  The definition here returns toto in both
  ;; of these cases.
  ;;
  ;; Note also that (form-at-point 'symbol) would not be a satisfactory
  ;; definition either, because it does not ensure that the symbol syntax
  ;; really represents an interned symbol.
  ;;
  (defalias 'symbol-at-point 'tap-symbol-at-point)

  ;; REPLACE ORIGINAL in `thingatpt.el'.
  ;;
  ;; Add optional argument SYNTAX-TABLE.
  ;;
  (defalias 'thing-at-point 'tap-thing-at-point)

  ;; REPLACE ORIGINAL in `thingatpt.el'.
  ;;
  ;; Better behavior.
  ;; Accept optional args UP and UNQUOTEDP.
  ;;
  (when (fboundp 'thing-at-point-bounds-of-list-at-point)
    (defalias 'thing-at-point-bounds-of-list-at-point 'tap-bounds-of-list-at-point))
  t)                                    ; Return non-nil so can use with `and' etc.

(defun tap-define-aliases-wo-prefix ()
  "Provide aliases for `tap-' functions and variables, without prefix."
  (defalias 'bounds-of-form-at-point 'tap-bounds-of-form-at-point)
  (defalias 'bounds-of-form-nearest-point 'tap-bounds-of-form-nearest-point)
  (defalias 'bounds-of-list-nearest-point 'tap-bounds-of-list-nearest-point)
  (defalias 'bounds-of-sexp-at-point 'tap-bounds-of-sexp-at-point)
  (defalias 'bounds-of-sexp-nearest-point 'tap-bounds-of-sexp-nearest-point)
  (when (fboundp 'tap-bounds-of-string-at-point)
    (defalias 'bounds-of-string-at-point 'tap-bounds-of-string-at-point)
    (defalias 'bounds-of-string-contents-at-point 'tap-bounds-of-string-contents-at-point))
  (defalias 'bounds-of-symbol-at-point 'tap-bounds-of-symbol-at-point)
  (defalias 'bounds-of-symbol-nearest-point 'tap-bounds-of-symbol-nearest-point)
  (defalias 'bounds-of-thing-nearest-point 'tap-bounds-of-thing-nearest-point)
  (defalias 'form-at-point-with-bounds 'tap-form-at-point-with-bounds)
  (defalias 'form-nearest-point 'tap-form-nearest-point)
  (defalias 'form-nearest-point-with-bounds 'tap-form-nearest-point-with-bounds)
  (defalias 'list-at/nearest-point-with-bounds 'tap-list-at/nearest-point-with-bounds)
  (defalias 'list-at-point-with-bounds 'tap-list-at-point-with-bounds)
  (defalias 'list-nearest-point 'tap-list-nearest-point)
  (defalias 'list-nearest-point-with-bounds 'tap-list-nearest-point-with-bounds)
  (defalias 'list-nearest-point-as-string 'tap-list-nearest-point-as-string)
  (defalias 'non-nil-symbol-name-at-point 'tap-non-nil-symbol-name-at-point)
  (defalias 'non-nil-symbol-name-nearest-point 'tap-non-nil-symbol-name-nearest-point)
  (defalias 'non-nil-symbol-nearest-point 'tap-non-nil-symbol-nearest-point)
  (defalias 'number-at-point-decimal 'tap-number-at-point-decimal)
  (defalias 'number-at-point-hex 'tap-number-at-point-hex)
  (defalias 'number-nearest-point 'tap-number-nearest-point)
  (defalias 'region-or-word-at-point 'tap-region-or-word-at-point)
  (defalias 'region-or-word-nearest-point 'tap-region-or-word-nearest-point)
  (defalias 'region-or-non-nil-symbol-name-nearest-point
      'tap-region-or-non-nil-symbol-name-nearest-point)
  (defalias 'sentence-nearest-point 'tap-sentence-nearest-point)
  (defalias 'sexp-at-point-with-bounds 'tap-sexp-at-point-with-bounds)
  (defalias 'sexp-nearest-point 'tap-sexp-nearest-point)
  (defalias 'sexp-nearest-point-with-bounds 'tap-sexp-nearest-point-with-bounds)
  (when (fboundp 'tap-string-at-point)
    (defalias 'string-at-point 'tap-string-at-point)
    (defalias 'string-contents-at-point 'tap-string-contents-at-point))
  (when (fboundp 'tap-string-nearest-point)
    (defalias 'string-nearest-point 'tap-string-nearest-point)
    (defalias 'string-contents-nearest-point 'tap-string-contents-nearest-point))
  (defalias 'symbol-at-point-with-bounds 'tap-symbol-at-point-with-bounds)
  (defalias 'symbol-name-at-point 'tap-symbol-name-at-point)
  (defalias 'symbol-name-nearest-point 'tap-symbol-name-nearest-point)
  (defalias 'symbol-nearest-point 'tap-symbol-nearest-point)
  (defalias 'symbol-nearest-point-with-bounds 'tap-symbol-nearest-point-with-bounds)
  (defalias 'thing-at-point-with-bounds 'tap-thing-at-point-with-bounds)
  (defalias 'thing/form-nearest-point-with-bounds 'tap-thing/form-nearest-point-with-bounds)
  (defalias 'thing-nearest-point 'tap-thing-nearest-point)
  (defalias 'thing-nearest-point-with-bounds 'tap-thing-nearest-point-with-bounds)
  (defalias 'unquoted-list-at-point 'tap-unquoted-list-at-point)
  (defalias 'unquoted-list-nearest-point 'tap-unquoted-list-nearest-point)
  (defalias 'unquoted-list-nearest-point-as-string 'tap-unquoted-list-nearest-point-as-string)
  (defalias 'word-nearest-point 'tap-word-nearest-point)
  (when (fboundp 'tap-color-at-point)
    (defalias 'color-at-point                  'tap-color-at-point)
    (defalias 'bounds-of-color-at-point        'tap-bounds-of-color-at-point)
    (defalias 'color-nearest-point             'tap-color-nearest-point)
    (defalias 'color-nearest-point-with-bounds 'tap-color-nearest-point-with-bounds))

  (when (fboundp 'defvaralias)          ; Emacs 22+
    (defvaralias 'near-point-x-distance 'tap-near-point-x-distance)
    (defvaralias 'near-point-y-distance 'tap-near-point-y-distance))
  t)                                    ; Return non-nil so can use with `and' etc.

;;;###autoload
;; This `intern' is in order to have the symbol, e.g., for `thing-types' in `thing-cmds.el'.
(intern "whitespace-&-newlines")
(defun forward-whitespace-&-newlines (arg)
  "Move forward over contiguous whitespace to non-whitespace.
Unlike `forward-whitespace', this moves over multiple contiguous
newlines."
  (interactive "p")
  (if (natnump arg)
      (re-search-forward "[ \t]+\\|\n+" nil 'move arg)
    (while (< arg 0)
      (when (re-search-backward "[ \t]+\\|\n+" nil 'move)  (skip-chars-backward " \t\n"))
      (setq arg  (1+ arg)))))

;; Copied from `misc-cmds.el'.
(intern "char-same-line") ; To have the symbol, e.g., for `thing-types' in `thing-cmds.el'.
(unless (fboundp 'forward-char-same-line)
  (defun forward-char-same-line (&optional arg)
    "Move forward a max of ARG chars on the same line, or backward if ARG < 0.
Return the signed number of chars moved if /= ARG, else return nil."
    (interactive "p")
    (let* ((start                      (point))
           (fwd-p                      (natnump arg))
           (inhibit-field-text-motion  t) ; Just to be sure, for end-of-line.
           (max                        (save-excursion
                                         (if fwd-p (end-of-line) (beginning-of-line))
                                         (- (point) start))))
      (forward-char (if fwd-p (min max arg) (max max arg)))
      (and (< (abs max) (abs arg))
           max))))

;; Inspired by `find-thing-at-point' at `http://www.emacswiki.org/SeanO'.
;;;###autoload
(defun find-fn-or-var-nearest-point (&optional confirmp)
  "Go to the definition of the function or variable nearest the cursor.
With a prefix arg, or if no function or variable is near the cursor,
prompt for the function or variable to find, instead.

\"Nearest\" is determined as for `tap-thing-nearest-point'.
The search is bounded by options `tap-near-point-x-distance' and
`tap-near-point-y-distance'."
  (interactive "P")
  (let* ((symb  (tap-symbol-nearest-point))
         (var   (and (boundp symb)  symb))
         (fn    (or (and (fboundp symb)  symb)
                    (function-called-at-point))))
    (condition-case nil
        (progn (push-mark nil t)
               (cond ((or confirmp  (not (or var fn)))
                      (when (not (or var  fn))
                        (message "Symbol nearest cursor is not a function or variable")
                        (sit-for 1))
                      (call-interactively
                       (if (y-or-n-p "Find function? (n means find variable) ")
                           'find-function
                         'find-variable)))
                     (var (find-variable var))
                     ((and (fboundp 'help-C-file-name) ; Emacs 22
                           fn  (subrp (symbol-function fn)))
                      (let ((buf+pos  (find-function-search-for-symbol
                                       fn nil (help-C-file-name (symbol-function fn) 'subr))))
                        (when (car buf+pos) (pop-to-buffer (car buf+pos)))))
                     (fn (find-function fn))
                     (t (call-interactively 'find-function))))
      (quit (pop-mark)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'thingatpt+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; thingatpt+.el ends here
