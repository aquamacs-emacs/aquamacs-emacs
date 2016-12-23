;;; icomplete+.el --- Extensions to `icomplete.el'.
;;
;; Filename: icomplete+.el
;; Description: Extensions to `icomplete.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2016, Drew Adams, all rights reserved.
;; Created: Mon Oct 16 13:33:18 1995
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Mon Jul  4 19:57:13 2016 (-0700)
;;           By: dradams
;;     Update #: 1700
;; URL: http://www.emacswiki.org/icomplete+.el
;; Doc URL: http://emacswiki.org/IcompleteMode
;; Keywords: help, abbrev, internal, extensions, local, completion, matching
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `icomplete'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to `icomplete.el'.
;;
;;  * Better display of candidates, including highlighting them and showing how many there are.
;;  * Shows key bindings for command candidates, optionally including menu bindings.
;;  * Does not bind keys for cycling in `icomplete-mode'.  Defines a separate mode for that, so you
;;    can use Icomplete with or without those key bindings.  (Emacs 24.4+)
;;  * Support for Icicles:
;;    . Respect current Icicles sort order, which you can cycle using `C-,'.
;;    . When you change direction cycling candidates, show the number of other cycle candidates.
;;
;;
;;  Macros defined here (but identical to those in Emacs 23):
;;
;;    `with-local-quit', `with-no-input'.
;;
;;  Commands define here:
;;
;;    `icompletep-cycling-mode'.
;;
;;  Faces defined here:
;;
;;    `icompletep-choices', `icompletep-determined',
;;    `icompletep-keys', `icompletep-nb-candidates'.
;;
;;  User options defined here:
;;
;;    `icompletep-exact-separator',
;;    `icompletep-include-menu-items-flag' (Emacs 23+),
;;    `icompletep-prospects-length' (Emacs < 23),
;;    `icomplete-show-key-bindings'.
;;
;;  Non-interactive functions defined here:
;;
;;    `icomplete-get-keys' (Emacs > 24.2),
;;    `icompletep-completion-all-sorted-completions',
;;    `icompletep-remove-if'.
;;
;;
;;  ***** NOTE: The following functions defined in `icomplete.el'
;;              have been REDEFINED OR ADVISED HERE:
;;
;;    `icomplete-get-keys' (Emacs < 24.3) -
;;       1. Respect `icompletep-include-menu-items-flag'.
;;       2. Do not wrap with `<...>'.
;;       3. If string of keys would be too long then shorten it.
;;
;;    `icomplete-completions' -
;;       1. Prepend the total number of candidates.
;;       2. For file-name completion, respect `completion-ignored-extensions'.
;;       3. With Icicles, sort candidates using `icicle-reversible-sort' and show number of
;;          remaining cycle candidates.  You can cycle the sort order using `C-,'.
;;       4. Show candidates in a different face.
;;       5. Optionally show and highlight key bindings, truncating if too long.
;;
;;    `icomplete-exhibit' -
;;       1. Save match-data.
;;       2. Do not insert if input begins with `(' (e.g. `repeat-complex-command').
;;       3. Ensure that the insertion does not deactivate mark.
;;
;;    `icomplete-mode' - Advised to provide Icomplete+ doc.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `icomplete.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "icomplete" '(progn (require 'icomplete+)))
;;
;;  Usage notes:
;;
;;  * Starting with Emacs 23 you can get icompletion of things like
;;    file names also.  See variable (non-option)
;;    `icomplete-with-completion-tables'.  If you set it to the
;;    (undocumented) value `t' then icompletion is available anytime
;;    the completion COLLECTION parameter is a function, which
;;    includes file-name completion.
;;
;;  * Starting with Emacs 24 you can specify the kinds of completion
;;    you want by customizing option `completion-category-overrides'
;;    for file names, buffer names, bookmark names, and so on.
;;
;;  * Starting with Emacs 24.4, Icomplete mode automatically binds
;;    keys that are otherwise useful in the minibuffer (for Isearch,
;;    symbol completion, etc.) to its own keys for cycling among
;;    icompletion candidates.  This is a BAD idea - see Emacs bug
;;    #13602.  Icomplete+ fixes this by having a separate mode that
;;    binds Icomplete keys, making that optional.  This is analogous
;;    to the difference between `cua-selection-mode' and `cua-mode'.
;;
;;    So with Icomplete+, just turning on Icomplete mode does not
;;    co-opt those keys taking them away from you for use in the
;;    minibuffer.  If you really want to do that then turn on
;;    `icomplete-cycling-mode' in addition to `icomplete-mode'.  And
;;    in that case, consider also choosing different keys to bind in
;;    `icomplete-minibuffer-map' from those that are bound by default.
;;
;;  (The first two features above are not particular to Icomplete+ -
;;  they are available also for vanilla Icomplete.)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2016/07/04 dadams
;;     icomplete-exhibit (Emacs 24.4+): Call icomplete--field-(beg|end) only once.
;; 2015/06/18 dadams
;;     icomplete-completions: Use boundp for icicle-mode, not fboundp.
;; 2014/04/15 dadams
;;     icomplete-exhibit: Update version tests, for Emacs 24.4 pretest (23.90.1).
;; 2014/04/13 dadams
;;     icomplete-exhibit, fix Emacs bug #17165: Ensure no error because (icomplete--field-*) is nil.
;; 2014/03/10 dadams
;;     icomplete-exhibit: Reverted fix from 2014-03-05 for Emacs < 23 (broke progressive completion).
;; 2014/03/05 dadams
;;     icomplete-exhibit: Wrap body with with-current-buffer, to prevent using *Completions* buffer.
;; 2014/01/11 dadams
;;     Soft, not hard require cl-lib.el.  Only 24.3+ has it.
;;     icompletep-completion-all-sorted-completions for Emacs 24.1-3:
;;       Use icompletep-remove-if if cl-delete-if is not defined (i.e., for Emacs 24.1-2).
;; 2013/12/31 dadams
;;     icomplete-exhibit, for Emacs 24.3.50: Protect icomplete-show-matches-on-no-input with boundp.
;; 2013/12/27 dadams
;;     icomplete-exhibit, icomplete-completions, icompletep-completion-all-sorted-completions:
;;       Added 24.4+ version.
;;     icompletep-completion-all-sorted-completions:
;;       Use cl-delete-if, not delete-if.  And require cl-lib.el.
;;       Take regexp-opt calculation out of the cl-delete-if loop, for performance.
;; 2013/09/21 dadams
;;     icompletep-completion-all-sorted-completions:
;;       Temporary hack for 24.4: completion--cache-all-sorted-completions takes 3 args now.
;; 2013/01/31 dadams
;;     Added: icompletep-cycling-mode.  Turn it off by default.
;; 2013/01/01 dadams
;;     Added: icompletep-completion-all-sorted-completions.
;;     icomplete-completions (Emacs 24 version):
;;       Rewrote to support Icicles sorting and Emacs 24 candidate metadata:
;;         Use icompletep-completion-all-sorted-completions instead of all-completions or
;;           completion-pcm--filename-try-filter.
;;         Use completion--field-metadata, last, and base-size.
;;         Use completion-try-completion, not try-completion.
;;         Do not sort using string-lessp (sorted by icompletep-completion-all-sorted-completions).
;; 2012/12/19 dadams
;;     icomplete-completions: Use width of icomplete-separator, not 1 or 2.  Use string-width more.
;;     icomplete-separator: Use same default value as vanilla Emacs.
;; 2012/12/08 dadams
;;     icompletep-exact-separator: Different default value - Unicode star for Emacs 23+, * otherwise.
;; 2012/12/05 dadams
;;     Updated wrt Emacs 24:
;;      Added: icomplete-show-key-bindings (removed from vanilla Emacs), icomplete-separator,
;;             icompletep-exact-separator.
;;      icomplete-exhibit: Added completion-all-sorted-completions as delay inhibition.
;;      icomplete-completions (all versions): Use icomplete-separator, icompletep-exact-separator.
;; 2012/11/19 dadams
;;     icomplete-completions:
;;       Exclude file names with extensions in completion-ignored-extensions.
;;       Use same MOST sexp as in Emacs <23 versions, since use try-completion.
;; 2012/08/06 dadams
;;     Removed old, commented code at end of file.
;; 2012/07/21 dadams
;;     icomplete-completions: Fixed typo for Emacs 23 version: COLLECTION everywhere, not CANDIDATES.
;; 2012/06/23 dadams
;;     icomplete-completions: Added new version for Emacs 24.  Remove 24 stuff from Emacs 23 version.
;;                            For Emacs 23 & 24: Use try-completion, not completion-try-completion.
;; 2012/06/16 dadams
;;     icomplete-completions (Emacs 23+):
;;       Use actual length of nb-cands-string, not min length from %7d format spec.
;;       Include length of icicle-completion-prompt-overlay, if Icicles and completing.
;; 2012/03/09 dadams
;;     icomplete-completions: Updated for Emacs 24: Bind non-essential and handle bug #10850.
;; 2011/10/13 dadams
;;     icomplete-get-keys: Added optional arg EXACTP.  Better spacing counts.
;;     icomplete-completions: Use MOST-TRY to determine exact match, pass to icomplete-get-keys.
;; 2011/10/12 dadams
;;     Added: icompletep-include-menu-items-flag, icomplete-get-keys, icompletep-remove-if.
;;     icomplete-completions: Truncate key-binding text if too long.
;; 2011/08/24 dadams
;;     Added top-level puts for common-lisp-indent-function.
;;     with-local-quit, while-no-input:
;;       Define only if not defined.  Use put for indentation.  Remove declare declaration.
;; 2011/06/05 dadams
;;     icomplete-completions: Handle Emacs 24's new METADATA arg for completion-try-completion.
;; 2011/01/04 dadams
;;     Removed autoload cookies from non def* sexps.  Added them for defgroup, defface.
;; 2010/07/29 dadams
;;     with-local-quit, with-no-input: Protect declare with fboundp.
;; 2009/08/06 dadams
;;     icomplete-completions (Emacs < 23): Bind, don't set, to initialize nb-candidates.
;; 2008/06/01 dadams
;;     icomplete-completions (Emacs 23): Set candidates to nil if ((nil)).
;;     Commented out vanilla Emacs code that's not used (last, base-size).
;; 2008/05/30 dadams
;;     Updated for Emacs 23 - complete rewrite.
;;       Added: macros with-local-quit and with-no-input.
;;       Added and adapted icomplete-exhibit and icomplete-completions for Emacs 23.
;;     icompletep-prospects-length: Use only for Emacs < 23.
;;     icomplete-exhibit: Removed vestigial test of icicle-apropos-completing-p.
;; 2006/07/30 dadams
;;     icomplete-exhibit: Save match-data.
;; 2006/07/16 dadams
;;     Added dark-background face suggestions from Le Wang - thx.
;; 2006/06/18 dadams
;;      icomplete-exhibit: Don't insert if Icicles apropos-completing.
;; 2006/01/07 dadams
;;      Added :link for sending bug report.
;; 2006/01/06 dadams
;;      Added defgroup.  Added :link.
;;      Renamed: prefix icomplete- to icompletep-.
;; 2005/12/18 dadams
;;     Renamed faces without "-face".
;;     Use defface.  Removed require of def-face-const.el.
;;     icomplete-prospects-length: defvar -> defcustom.
;; 2005/09/30 dadams
;;     Commented out redefinitions of primitives, so no longer reset
;;       minibuffer-completion-table to nil. Leaving the commented code in for now.
;; 2005/08/16 dadams
;;     icomplete-completions: If icicles.el is loaded, change no-match message slightly.
;; 2005/07/24 dadams
;;     icomplete-exhibit: Set deactivate-mark to nil at end.
;;     Remove commented Emacs 19 code at end.
;; 2005/07/19 dadams
;;     Added: icomplete-nb-candidates-face.
;;     icomplete-completions: Add number of icomplete candidates.
;;                            Append number of other cycle candidates (icicle).
;; 2005/05/29 dadams
;;     read-from-minibuffer: Updated to deal with new arg in Emacs 22.
;; 2004/12/02 dadams
;;     Highlight keys (icomplete-completions).
;; 2004/09/21 dadams
;;     Removed (icomplete-mode 99) at end.
;; 2004/04/13 dadams
;;     I'm not sure that some of the "enhancements" here are still
;;     needed.  This code was written long ago.  In particular, I'm not
;;     sure that the changes to `icomplete-exhibit' and the
;;     redefinitions of the Emacs primitives are needed.  Even if they
;;     are not needed, I'm leaving them in, as they are benign :).
;; 1995/12/15 dadams
;;     Defined replacements that reset minibuffer-completion-table to avoid
;;     icompletion: read-string, read-from-minibuffer, read-no-blanks-input.
;; 1995/11/30 dadams
;;     Added redefinition of yes-or-no-p.
;; 1995/10/17 dadams
;;     1) Added icomplete-choices-face and icomplete-determined-face.
;;     2) Redefined icomplete-exhibit: Doesn't insert if input
;;        begins with `('  (e.g. repeat-complex-command).
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

(require 'icomplete)

(when (> emacs-major-version 23)        ; Emacs 24.3+ actually, so soft-require.
  (require 'cl-lib nil t)) ; cl-delete-if

;; Quiet the byte-compiler.
(defvar completion-all-sorted-completions)
(defvar icomplete-eoinput)
(defvar icomplete-hide-common-prefix)
(defvar icomplete-minibuffer-map)
(defvar icomplete-show-matches-on-no-input)
(defvar icomplete-with-completion-tables)
(defvar icompletep-include-menu-items-flag)
(defvar icompletep-ORIG-icomplete-minibuffer-map)
(defvar icompletep-prospects-length)

(defvar icicle-nb-of-other-cycle-candidates)

;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defgroup Icomplete-Plus nil
  "Icomplete Enhancements."
  :prefix "icompletep-"
  :group 'completion :group 'convenience :group 'matching :group 'minibuffer
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icomplete+.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/icomplete+.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/IcompleteMode#IcompleteModePlus")
  :link '(emacs-commentary-link :tag "Commentary" "icomplete+"))

;;;###autoload
(defcustom icompletep-exact-separator (if (> emacs-major-version 22)
                                          (string ?\u2605 ?\  ) ; star
                                        ;; (string ?\u2714 ?\  ) ; check mark
                                        ;; (string ?\u29eb ?\  ) ; diamond
                                        ;; (string ?\u2205 ?\  ) ; empty set
                                        "* ")
  "String used by to separate exact match from other alternatives."
  :type 'string :group 'Icomplete-Plus)

(unless (boundp 'icomplete-show-key-bindings) ; Emacs 24.3+
  (defcustom icomplete-show-key-bindings t
    "*Non-nil means show key bindings as well as completion for sole match."
    :type 'boolean :group 'icomplete))

(when (boundp 'icomplete-show-key-bindings) ; Emacs 20-24.2
  (defcustom icomplete-separator " | "
    "String used to separate completion alternatives."
    :type 'string :group 'icomplete :version "24.4"))

(when (< emacs-major-version 23)
  (defcustom icompletep-prospects-length 100 ; Default was 80
    "*Length of string displaying icompletion candidates."
    :type 'integer :group 'Icomplete-Plus))

(when (> emacs-major-version 22)
  (defcustom icompletep-include-menu-items-flag  t
    "*Non-nil means include menu bindings in the list of keys for a command."
    :type 'boolean :group 'Icomplete-Plus))

;;;###autoload
(defface icompletep-choices
    '((((background dark)) (:foreground "Snow4"))
      (t (:foreground "DarkBlue")))
  "*Face for minibuffer reminder of possible completion suffixes."
  :group 'Icomplete-Plus)

;;;###autoload
(defface icompletep-determined
    '((t (:foreground "SeaGreen")))
  "*Face for minibuffer reminder of possible completion prefix."
  :group 'Icomplete-Plus)

;;;###autoload
(defface icompletep-nb-candidates
  '((((background dark)) (:foreground "SpringGreen"))
    (t (:foreground "DarkMagenta")))
  "*Face for minibuffer reminder of number of completion candidates.
This has no effect unless library `icicles.el' is being used."
  :group 'Icomplete-Plus)

;;;###autoload
(defface icompletep-keys
    '((t (:foreground "Red")))
  "*Face for minibuffer reminder of possible completion key bindings."
  :group 'Icomplete-Plus)

;;; Quiet the byte-compiler.
(defvar icomplete-overlay)
(defvar icomplete-prospects-height)



;; REPLACES ORIGINAL defined in `icomplete.el':
;;
;; Save match-data.
;; Don't insert if input begins with `(' (e.g. `repeat-complex-command').
;;
(when (< emacs-major-version 23)        ; Emacs 20, 21, 22.
  (defun icomplete-exhibit ()
    "Insert icomplete completions display.
Should be run via minibuffer `post-command-hook'.
See `icomplete-mode' and `minibuffer-setup-hook'."
    (when (icomplete-simple-completing-p)
      (save-match-data
        (let* ((minibuf-begin     (if (< emacs-major-version 21)
                                      (point-min)
                                    (minibuffer-prompt-end)))
               (contents          (buffer-substring minibuf-begin (point-max)))
               (buffer-undo-list  t))
          (save-excursion
            (goto-char (point-max))
            ;; Register the end of input, so we know where the extra stuff
            ;; (match-status info) begins:
            (unless (boundp 'icomplete-eoinput)
              ;; In case it got wiped out by major mode business:
              (make-local-variable 'icomplete-eoinput))
            (setq icomplete-eoinput  (point))
            ;; Insert the match-status information:
            (when (and (> (point-max) minibuf-begin)
                       (save-excursion  ; Do nothing if looking at a list, string, etc.
                         (goto-char minibuf-begin)
                         (not (looking-at ; No (, ", ', 9 etc. at start.
                               "\\(\\s-+$\\|\\s-*\\(\\s(\\|\\s\"\\|\\s'\\|\\s<\\|[0-9]\\)\\)")))
                       (or
                        ;; Do not bother with delay after certain number of chars:
                        (> (point-max) icomplete-max-delay-chars)
                        ;; Do not delay if alternatives number is small enough:
                        (if minibuffer-completion-table
                            (cond ((numberp minibuffer-completion-table)
                                   (< minibuffer-completion-table
                                      icomplete-delay-completions-threshold))
                                  ((sequencep minibuffer-completion-table)
                                   (< (length minibuffer-completion-table)
                                      icomplete-delay-completions-threshold))))
                        ;; Delay - give some grace time for next keystroke, before
                        ;; embarking on computing completions:
                        (sit-for icomplete-compute-delay)))
              (insert
               (icomplete-completions contents minibuffer-completion-table
                                      minibuffer-completion-predicate
                                      (not minibuffer-completion-confirm)))))
          (setq deactivate-mark  nil)))))) ; Don't let the insert deactivate the mark.


;;; These two macros are defined in `subr.el' for Emacs 23+.
;;; They are included here only so you can, if needed, byte-compile this file using Emacs < 23
;;; and still use the byte-compiled file in Emacs 23+.
(unless (fboundp 'with-local-quit)
  (defmacro with-local-quit (&rest body)
    "Execute BODY, allowing quits to terminate BODY but not escape further.
When a quit terminates BODY, `with-local-quit' returns nil but
requests another quit.  That quit will be processed as soon as quitting
is allowed once again.  (Immediately, if `inhibit-quit' is nil.)"
    `(condition-case nil
      (let ((inhibit-quit  nil))
        ,@body)
      (quit (setq quit-flag  t)
       ;; This call is to give a chance to handle quit-flag
       ;; in case inhibit-quit is nil.
       ;; Without this, it will not be handled until the next function
       ;; call, and that might allow it to exit thru a condition-case
       ;; that intends to handle the quit signal next time.
       (eval '(ignore nil)))))
  (put 'with-local-quit 'common-lisp-indent-function '(&body)))

(unless (fboundp 'while-no-input)
  (defmacro while-no-input (&rest body) ; Defined in `subr.el'.
    "Execute BODY only as long as there's no pending input.
If input arrives, that ends the execution of BODY,
and `while-no-input' returns t.  Quitting makes it return nil.
If BODY finishes, `while-no-input' returns whatever value BODY produced."
    (let ((catch-sym  (make-symbol "input")))
      `(with-local-quit
        (catch ',catch-sym
          (let ((throw-on-input  ',catch-sym))
            (or (input-pending-p)  (progn ,@body)))))))
  (put 'while-no-input 'common-lisp-indent-function '(&body)))



;; REPLACES ORIGINAL defined in `icomplete.el':
;;
;; 1. Do not include menu items unless `icompletep-include-menu-items-flag'.
;; 2. Do not wrap with `<...>', since:
;;    (a) not needed because we do not include `Matched;', so [...] suffices
;;    (b) `<...>' is also used for function keys, so it would be confusing
;; 3. If string of keys would be too long then shorten it using `...',
;;    or if even `[ ... ]' would be too long then return `TOO-LONG' so the
;;    brackets can be removed altogether.
;;
(defun icomplete-get-keys (func-name &optional exactp)
  "Return strings naming keys bound to FUNC-NAME, or nil if none.
Examines the prior, not current, buffer, presuming that current buffer
is minibuffer.

Non-nil optional arg EXACTP means FUNC-NAME is an exact match, as
determined by `try-completion' or `completion-try-completion.

If option `icompletep-include-menu-items-flag' is non-nil then include
menu-bar bindings in the l of keys (Emacs 23+ only)."
  (when (commandp func-name)
    (save-excursion
      (let* ((sym      (intern func-name))
             (buf      (other-buffer nil t))
             (keys     (with-current-buffer buf (where-is-internal sym)))
             (max-len  (max 0 (-  (window-width)
				  (if (fboundp 'minibuffer-prompt-end)
                                      (minibuffer-prompt-end)
                                    (point-max))
				  (if (fboundp 'minibuffer-prompt-end) (length func-name) 0)
                                  (if exactp 7 9) ; 6 SPC, () around FUNC-NAME, 1 SPC after )
                                  5)))) ; 1 space + 2 each for `[ ' and ` ]'
	(when keys
          (unless (and (boundp 'icompletep-include-menu-items-flag)
                       icompletep-include-menu-items-flag)
            (setq keys  (icompletep-remove-if (lambda (ky)
                                                (and (vectorp ky)  (eq (aref ky 0) 'menu-bar)))
                                              keys)))
	  (setq keys  (mapconcat 'key-description
                                 (sort keys #'(lambda (x y) (< (length x) (length y))))
                                 ", "))
          (cond ((zerop max-len) (setq keys  'TOO-LONG))
                ((> (length keys) max-len)
                 (setq keys  (concat (substring keys 0 (max 0 (- max-len 5))) "...")))))
        keys))))

;; Same as `icicle-remove-if' in `icicles-fn.el'.
(defun icompletep-remove-if (pred xs)
  "A copy of list XS with no elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x  xs)  (unless (funcall pred x) (push x result)))
    (nreverse result)))


;; REPLACES ORIGINAL defined in `icomplete.el':
;;
;; Save match-data.
;; Don't insert if input begins with `(' (e.g. `repeat-complex-command').
;;
(when (and (> emacs-major-version 22)   ; Emacs 23 through Emacs 24.3.
           (or (< emacs-major-version 24)
               (and (= emacs-major-version 24)  (< emacs-minor-version 4)
                    (version< emacs-version "24.3.50"))))
  (defun icomplete-exhibit ()
    "Insert icomplete completions display.
Should be run via minibuffer `post-command-hook'.  See `icomplete-mode'
and `minibuffer-setup-hook'."
    (when (and icomplete-mode  (icomplete-simple-completing-p))
      (with-current-buffer (window-buffer (active-minibuffer-window))
        (save-excursion
          (goto-char (point-max))
          ;; Insert the match-status information.
          (when (and (> (point-max) (minibuffer-prompt-end))
                     buffer-undo-list   ; Wait for some user input.
                     (save-excursion    ; Do nothing if looking at a list, string, etc.
                       (goto-char (minibuffer-prompt-end))
                       (save-match-data
                         (not (looking-at ; No (, ", ', 9 etc. at start.
                               "\\(\\s-+$\\|\\s-*\\(\\s(\\|\\s\"\\|\\s'\\|\\s<\\|[0-9]\\)\\)"))))
                     (or
                      ;; Do not bother with delay after certain number of chars:
                      (> (- (point) (field-beginning)) icomplete-max-delay-chars)
                      ;; Do not delay if completions are known.
                      completion-all-sorted-completions
                      ;; Do not delay if alternatives number is small enough:
                      (and (sequencep minibuffer-completion-table)
                           (< (length minibuffer-completion-table)
                              icomplete-delay-completions-threshold))
                      ;; Delay - give some grace time for next keystroke, before
                      ;; embarking on computing completions:
                      (sit-for icomplete-compute-delay)))
            (let ((text              (while-no-input (icomplete-completions
                                                      (field-string)
                                                      minibuffer-completion-table
                                                      minibuffer-completion-predicate
                                                      (not minibuffer-completion-confirm))))
                  (buffer-undo-list  t)
                  deactivate-mark)
              ;; Do nothing if `while-no-input' was aborted.
              (when (stringp text)
                (move-overlay icomplete-overlay (point) (point) (current-buffer))
                ;; The current C cursor code doesn't know to use the overlay's
                ;; marker's stickiness to figure out whether to place the cursor
                ;; before or after the string, so let's spoon-feed it the pos.
                (put-text-property 0 1 'cursor t text)
                (overlay-put icomplete-overlay 'after-string text)))))))))


;; REPLACES ORIGINAL defined in `icomplete.el':
;;
;; Save match-data.
;; Don't insert if input begins with `(' (e.g. `repeat-complex-command').
;;
(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3))
          (and (= emacs-major-version 24)  (not (version< emacs-version "24.3.50")))) ;@@@@ TO REMOVE
  (defun icomplete-exhibit ()
    "Insert icomplete completions display.
Should be run via minibuffer `post-command-hook'.  See `icomplete-mode'
and `minibuffer-setup-hook'."
    (when (and icomplete-mode  (icomplete-simple-completing-p)) ;Shouldn't be necessary.
      (with-current-buffer (window-buffer (active-minibuffer-window))
        (save-excursion
          (goto-char (point-max))
          (let ((field-end  (icomplete--field-end))
                (field-beg  (icomplete--field-beg)))
            ;; Insert the match-status information.
            (when (and (or (and (boundp 'icomplete-show-matches-on-no-input)
                                icomplete-show-matches-on-no-input)
                           (and (numberp field-end)  (numberp field-beg)
                                (> field-end field-beg)))
                       (save-excursion  ; Do nothing if looking at a list, string, etc.
                         (when (numberp field-end) (goto-char field-end))
                         (save-match-data
                           (not (looking-at ; No (, ", ', 9 etc. at start.
                                 "\\(\\s-+$\\|\\s-*\\(\\s(\\|\\s\"\\|\\s'\\|\\s<\\|[0-9]\\)\\)"))))
                       (or
                        ;; Do not bother with delay after certain number of chars:
                        (> (- (point) field-beg) icomplete-max-delay-chars)
                        ;; Do not delay if completions are known.
                        completion-all-sorted-completions
                        ;; Do not delay if alternatives number is small enough:
                        (and (sequencep (icomplete--completion-table))
                             (< (length (icomplete--completion-table))
                                icomplete-delay-completions-threshold))
                        ;; Delay - give some grace time for next keystroke, before
                        ;; embarking on computing completions:
                        (sit-for icomplete-compute-delay)))
              (let* ((field-string      (icomplete--field-string))
                     (text              (while-no-input (icomplete-completions
                                                         field-string
                                                         (icomplete--completion-table)
                                                         (icomplete--completion-predicate)
                                                         (and (window-minibuffer-p)
                                                              (not minibuffer-completion-confirm)))))
                     (buffer-undo-list  t)
                     deactivate-mark)
                ;; Do nothing if `while-no-input' was aborted.
                (when (stringp text)
                  (move-overlay icomplete-overlay (point) (point) (current-buffer))
                  ;; The current C cursor code doesn't know to use the overlay's
                  ;; marker's stickiness to figure out whether to place the cursor
                  ;; before or after the string, so let's spoon-feed it the pos.
                  (put-text-property 0 1 'cursor t text)
                  (overlay-put icomplete-overlay 'after-string text))))))))))


;; REPLACES ORIGINAL defined in `icomplete.el':
;;
;; 1. Prepends total number of candidates.
;; 2. Sorts alternatives, puts them in a different face, and separates them more.
;; 3. Highlights key-binding text, truncating it if too long.
;; 4. Appends number of remaining cycle candidates (for Icicles).
;;
(when (< emacs-major-version 23)        ; Emacs 20, 21, 22.
  (defun icomplete-completions (name candidates predicate require-match)
    "Identify prospective candidates for minibuffer completion.
NAME is the name to complete.
CANDIDATES are the candidates to match.
PREDICATE filters matches: they succeed only if it returns non-nil.
REQUIRE-MATCH non-nil means the input must match a candidate.

The display is updated with each minibuffer keystroke during
minibuffer completion.

Prospective completion suffixes (if any) are displayed, bracketed by
\"()\", \"[]\", or \"{}\".  The choice of brackets is as follows:

  \(...) - A single prospect is identified, and matching is enforced.
  \[...] - A single prospect is identified, and matching is optional.
  \{...} - Multiple prospects are indicated, and further input is
          needed to distinguish a single one.

The displays for unambiguous matches have ` [ Matched ]' appended
\(whether complete or not), or ` \[ No matches ]', if no eligible
matches exist.  Keybindings for uniquely matched commands are
shown within brackets, [] (without the word \"Matched\"), if there is
room.

When more than one completion is available, the total number precedes
the suffixes display, like this:
  M-x forw    14 (ard-) { char line list...}

If library `icicles.el' is also loaded, then you can cycle
completions.  When you change cycling direction, the number of
additional cycle candidates, besides the current one, is displayed
following the rest of the icomplete info:
  M-x forward-line   [Matched]  (13 more)."
    ;; `all-completions' doesn't like empty `minibuffer-completion-table's (ie: (nil))
    (when (and (listp candidates)  (null (car candidates))) (setq candidates  ()))
    (let* ((comps                     (all-completions name candidates predicate))
           (open-bracket-determined   (if require-match "("   " ["))
           (close-bracket-determined  (if require-match ") "  "] "))
           (keys                      ())
           (nb-candidates             (length comps))
           nb-candidates-string)
      ;; `concat'/`mapconcat' is the slow part.  With the introduction of
      ;; `icompletep-prospects-length', there is no need for `catch'/`throw'.
      (if (null comps) (format (if (fboundp 'icicle-apropos-complete)
                                   "\t%sNo prefix matches%s"
                                 "\t%sNo matches%s")
                               open-bracket-determined
                               close-bracket-determined)
        (let* ((most-try                 (try-completion name (mapcar #'list comps)))
               (most                     (if (stringp most-try) most-try (car comps)))
               (most-len                 (length most))
               (determ                   (and (> most-len (length name))
                                              (concat open-bracket-determined
                                                      (substring most (length name))
                                                      close-bracket-determined)))
               (open-bracket-prospects   "{ ")
               (close-bracket-prospects  " }")
               (prospects-len            0)
               prompt prompt-rest prospects most-is-exact comp)
          (when determ
            (put-text-property 0 (length determ) 'face 'icompletep-determined determ))
          (if (eq most-try t)
              (setq prospects  ())
            (while (and comps  (< prospects-len icompletep-prospects-length))
              (setq comp   (substring (car comps) most-len)
                    comps  (cdr comps))
              (cond ((string= comp "") (setq most-is-exact  t))
                    ((member comp prospects))
                    (t (setq prospects      (cons comp prospects)
                             prospects-len  (+ prospects-len
                                               (string-width comp)
                                               (string-width icomplete-separator)))))))
          (setq prompt-rest
                (if prospects
                    (concat open-bracket-prospects
                            (and most-is-exact  icompletep-exact-separator)
                            (mapconcat 'identity (sort prospects (function string-lessp))
                                       icomplete-separator)
                            (and comps  "...")
                            close-bracket-prospects)
                  (setq keys  (and icomplete-show-key-bindings
                                   (commandp (intern-soft most))
                                   (icomplete-get-keys most (eq t most-try))))
                  (if (eq keys 'TOO-LONG) ; No room even for ` [ ... ]'.
                      ""
                    (concat " [ " (and (not keys)  "Matched") keys " ]"))))
          (unless (string= "" prompt-rest)
            (put-text-property 0 (length prompt-rest) 'face 'icompletep-choices prompt-rest))
          (cond ((< nb-candidates 2)
                 (setq prompt  (concat "      " determ prompt-rest)) ; 6 spaces.
                 (when (eq last-command this-command)
                   (setq icicle-nb-of-other-cycle-candidates  0))) ; We know now, so reset it.
                (t
                 (setq nb-candidates-string  (format "%7d " nb-candidates))
                 (put-text-property (string-match "\\S-" nb-candidates-string)
                                    (1- (length nb-candidates-string))
                                    'face 'icompletep-nb-candidates nb-candidates-string)
                 (setq prompt  (concat nb-candidates-string determ prompt-rest))))
          ;; Highlight keys.
          (when (stringp keys)
            (put-text-property (+ 9 (length determ)) (1- (length prompt))
                               'face 'icompletep-keys prompt))
          ;; Append mention of number of other cycle candidates (from `icicles.el').
          (when (and (boundp 'icicle-last-completion-candidate)
                     (> icicle-nb-of-other-cycle-candidates 0)
                     (= 1 nb-candidates)
                     icicle-last-completion-candidate
                     (not (eq last-command this-command)))
            (setq nb-candidates-string  ; Reuse the string.
                  (format "  (%d more)" icicle-nb-of-other-cycle-candidates))
            (put-text-property (string-match "\\S-" nb-candidates-string)
                               (length nb-candidates-string)
                               'face 'icompletep-nb-candidates nb-candidates-string)
            (setq prompt  (concat prompt nb-candidates-string)))
          prompt)))))



;; REPLACES ORIGINAL defined in `icomplete.el':
;;
;; 1. Prepends total number of candidates.
;; 2. Sorts alternatives alphabetically, puts them in a different face, and separates them more.
;; 3. Highlights key-binding text, truncating it if too long.
;; 4. Appends number of remaining cycle candidates (for Icicles).
;;
(when (= emacs-major-version 23)
  (defun icomplete-completions (name collection predicate require-match)
    "Identify prospective candidates for minibuffer completion.
NAME is the name to complete.
COLLECTION is the collection of candidates to match.
PREDICATE filters matches: they succeed only if it returns non-nil.
REQUIRE-MATCH non-nil means the input must match a candidate.

The display is updated with each minibuffer keystroke during
minibuffer completion.

Prospective completion suffixes (if any) are displayed, bracketed by
\"()\", \"[]\", or \"{}\".  The choice of brackets is as follows:

  \(...) - A single prospect is identified, and matching is enforced.
  \[...] - A single prospect is identified, and matching is optional.
  \{...} - Multiple prospects are indicated, and further input is
          needed to distinguish a single one.

The displays for unambiguous matches have ` [ Matched ]' appended
\(whether complete or not), or ` \[ No matches ]', if no eligible
matches exist.  Keybindings for uniquely matched commands are
shown within brackets, [] (without the word \"Matched\"), if there is
room.

When more than one completion is available, the total number precedes
the suffixes display, like this:
  M-x forw    14 (ard-) { char line list...}

If library `icicles.el' is also loaded, then you can cycle
completions.  When you change cycling direction, the number of
additional cycle candidates, besides the current one, is displayed
following the rest of the icomplete info:
  M-x forward-line   [Matched]  (13 more)."
    ;; `all-completions' doesn't like empty `minibuffer-completion-table's (ie: (nil))
    (when (and (listp collection)  (null (car collection))) (setq collection  ()))
    (let* (;; Do not use `completion-all-sorted-completions' as in vanilla Emacs.
           ;; We need the number of comps, and we do not need that sort order.
           ;; (comps (completion-all-sorted-completions))

           ;; Exclude file names with extensions in `completion-ignored-extensions'.
           (comps            (if (and minibuffer-completing-file-name
                                      icomplete-with-completion-tables)
                                 (completion-pcm--filename-try-filter
                                  (all-completions name collection predicate))
                               (all-completions name collection predicate)))
           (nb-candidates    (length comps))
           (nb-cands-string  (if (< nb-candidates 2) "" (format "%7d " nb-candidates)))
;;; We do not use `completion-all-sorted-completions', so we do not need `last' or `base-size'.
;;; $$$$$      (last          (if (consp comps) (last comps)))
;;;            (base-size     (cdr last))
           (open-bracket     (if require-match "("   " ["))
           (close-bracket    (if require-match ") "  "] ")))
      ;; `concat'/`mapconcat' is the slow part.
      (if (not (consp comps))
          (format (if (fboundp 'icicle-apropos-complete)
                      "\t%sNo prefix matches%s"
                    "\t%sNo matches%s")
                  open-bracket close-bracket)
;;; $$$$$   (if last (setcdr last ()))
        (let* ((keys           ())
               (most-try
                ;; We do not use BASE-SIZE.  So we just use `try-completion'.
;;;             (if (and base-size (> base-size 0))
;;;                 (completion-try-completion name collection predicate (length name))
;;;               ;; If the COMPS are 0-based, the result should be the same with COMPS.
;;;               (completion-try-completion name comps nil (length name))))
                (try-completion name collection predicate))
               ;; Since we use `try-completion', MOST-TRY will not be a cons.
               ;; (most        (if (consp most-try) (car most-try) (if most-try (car comps) "")))
               (most           (if (stringp most-try) most-try (car comps)))
               ;; Compare NAME and MOST, so we can determine if NAME is
               ;; a prefix of MOST, or something else.
               (compare        (compare-strings name nil nil most nil nil
                                                completion-ignore-case))
               (determ         (and (not (or (eq t compare)  (eq t most-try)
                                             (= (setq compare  (1- (abs compare))) (length most))))
                                    (concat open-bracket
                                            (cond ((= compare (length name)) ; NAME is a prefix
                                                   (substring most compare))
                                                  ((< compare 5) most)
                                                  (t (concat "..." (substring most compare))))
                                            close-bracket)))
               (prospects-len  (+ (if (and (boundp 'icicle-mode)  icicle-mode  (icicle-completing-p))
				      2	; for `icicle-completion-prompt-overlay'
				    0)
                                  (string-width (buffer-string)) ; for prompt
				  (string-width nb-cands-string)
				  (length determ) ; for determined part
				  2     ; for "{ "
				  -2    ; for missing last "  " after last candidate
				  5))	; for "... }"
               (prospects-max
                ;; Max total length to use, including the minibuffer content.
                (* (+ icomplete-prospects-height
                      ;; If the minibuffer content already uses up more than
                      ;; one line, increase the allowable space accordingly.
                      (/ prospects-len (window-width)))
                   (window-width)))
               (prefix-len
                ;; Find the common prefix among `comps'.
                ;; Cannot use the optimization below because its assumptions
                ;; are not always true, e.g. when completion-cycling (Emacs bug #10850):
                ;; (if (eq t (compare-strings (car comps) nil (length most)
                ;; 			 most nil nil completion-ignore-case))
                ;;     ;; Common case.
                ;;     (length most)
                ;; Else, use try-completion.
                (let ((comps-prefix  (try-completion "" comps)))
                  (and (stringp comps-prefix)  (string-width comps-prefix))))
               prompt prompt-rest prospects most-is-exact comp limit)
          (when determ
            (put-text-property 0 (length determ) 'face 'icompletep-determined determ))
          (if (eq most-try t)           ; (or (null (cdr comps))
              (setq prospects  ())
            (while (and comps  (not limit))
              (setq comp   (if prefix-len (substring (car comps) prefix-len) (car comps))
                    comps  (cdr comps))
              (cond ((string= comp "") (setq most-is-exact  t))
                    ((member comp prospects))
                    (t (setq prospects-len  (+ (string-width comp)
                                               (string-width icomplete-separator)
                                               prospects-len))
                       (if (< prospects-len prospects-max)
                           (push comp prospects)
                         (setq limit  t))))))
;;; $$$$$    ;; Restore the base-size info, since `completion-all-sorted-completions' is cached.
;;;          (when last (setcdr last base-size))
          (setq prompt-rest
                (if prospects
                    (concat "{ " (and most-is-exact  icompletep-exact-separator)
                            (mapconcat 'identity (sort prospects (function string-lessp))
                                       icomplete-separator)
                            (and limit  "...")
                            " }")
                  (setq keys  (and icomplete-show-key-bindings
                                   (commandp (intern-soft most))
                                   (icomplete-get-keys most (eq t most-try))))
                  (if (eq keys 'TOO-LONG) ; No room even for `[ ... ]'.
                      ""
                    (concat " [ " (and (not keys)  "Matched") keys " ]"))))
          (unless (string= "" prompt-rest)
            (put-text-property 0 (length prompt-rest) 'face 'icompletep-choices prompt-rest))
          (cond ((< nb-candidates 2)
                 (setq prompt  (concat "      " determ prompt-rest)) ; 6 spaces.
                 (when (eq last-command this-command)
                   (setq icicle-nb-of-other-cycle-candidates  0))) ; We know now, so reset it.
                (t
                 (put-text-property (string-match "\\S-" nb-cands-string)
                                    (1- (length nb-cands-string))
                                    'face 'icompletep-nb-candidates nb-cands-string)
                 (setq prompt  (concat nb-cands-string determ prompt-rest))))
          ;; Highlight keys.
          (when (stringp keys)
            (put-text-property (+ 9 (length determ)) (1- (length prompt))
                               'face 'icompletep-keys prompt))
          ;; Append mention of number of other cycle candidates (from `icicles.el').
          (when (and (boundp 'icicle-last-completion-candidate)
                     (> icicle-nb-of-other-cycle-candidates 0)
                     (= 1 nb-candidates)
                     icicle-last-completion-candidate
                     (not (eq last-command this-command)))
            (setq nb-cands-string       ; Reuse the string.
                  (format "  (%d more)" icicle-nb-of-other-cycle-candidates))
            (put-text-property (string-match "\\S-" nb-cands-string)
                               (length nb-cands-string)
                               'face 'icompletep-nb-candidates nb-cands-string)
            (setq prompt  (concat prompt nb-cands-string)))
          prompt)))))


;; REPLACES ORIGINAL defined in `icomplete.el':
;;
;; 1. Prepend total number of candidates.
;; 2. With Icicles, sort candidates using `icicle-reversible-sort' and show number of remaining
;;    cycle candidates.  You can cycle the sort order using `C-,'.
;; 3. Show candidates in a different face.
;; 4. Optionally show and highlight key bindings, truncating if too long.
;;
(when (and (= emacs-major-version 24)  (< emacs-minor-version 4)) ; Emacs 24.1 through Emacs 24.3.

  (defun icomplete-completions (name candidates predicate require-match)
    "Identify prospective candidates for minibuffer completion.
NAME is the name to complete.
CANDIDATES is the collection of candidates to match.  See
`completing-read' for its possible values.
PREDICATE filters matches: they succeed only if it returns non-nil.
REQUIRE-MATCH non-nil means the input must match a candidate.

The display is updated with each minibuffer keystroke during
minibuffer completion.

Prospective completion suffixes (if any) are displayed, bracketed by
\"()\", \"[]\", or \"{}\".  The choice of brackets is as follows:

  \(...) - A single prospect is identified, and matching is enforced.
  \[...] - A single prospect is identified, and matching is optional.
  \{...} - Multiple prospects are indicated, and further input is
          needed to distinguish a single one.

The displays for unambiguous matches have ` [ Matched ]' appended
\(whether complete or not), or ` \[ No matches ]', if no eligible
matches exist.  Keybindings for uniquely matched commands are
shown within brackets, [] (without the word \"Matched\"), if there is
room.

When more than one completion is available, the total number precedes
the suffixes display, like this:

  M-x forw    14 (ard-) { char line list...}

In Icicle mode:
 * The current Icicles sort order is used.
 * When cycling candidates and you change direction, the number of
   other cycling candidates is shown, as `(N more)'.  For example:

   M-x forward-line   [Matched]  (13 more)."
    ;; `concat'/`mapconcat' is the slow part.
    (let* ((non-essential  t)
           (icyp           (and (boundp 'icicle-mode)  icicle-mode))
           (open-bracket   (if require-match "("   " ["))
           (close-bracket  (if require-match ") "  "] "))
           (md             (completion--field-metadata (field-beginning)))
           (comps          (icompletep-completion-all-sorted-completions
                            (and icyp  #'icicle-reversible-sort)))
           (last           (and (consp comps)  (last comps)))
           (base-size      (cdr last))
           nb-candidates  nb-cands-string)
      (if (not (consp comps))
          (format "\t%sNo matches%s" open-bracket close-bracket)
        (when last (setcdr last ()))
        (setq nb-candidates    (length comps)
              nb-cands-string  (if (< nb-candidates 2) "" (format "%7d " nb-candidates)))
        (let* ((most-try       (if (and base-size  (> base-size 0))
                                   (completion-try-completion name candidates predicate
                                                              (length name) md)
                                 ;; If COMPS are 0-based, the result should be the same with COMPS.
                                 (completion-try-completion name comps nil (length name) md)))
               (most           (if (consp most-try) (car most-try) (if most-try (car comps) "")))
               ;; Compare NAME & MOST, to determine if NAME is a prefix of MOST, or something else.
               (compare        (compare-strings name nil nil most nil nil completion-ignore-case))
               (determ         (and (not (or (eq t compare)  (eq t most-try)
                                             (= (setq compare  (1- (abs compare))) (length most))))
                                    (concat open-bracket
                                            (cond ((= compare (length name)) ; NAME is a prefix
                                                   (substring most compare))
                                                  ((< compare 5) most)
                                                  (t (concat "..." (substring most compare))))
                                            close-bracket)))
               (prospects-len  (+ (if (and icyp  (icicle-completing-p))
                                      2 ; for `icicle-completion-prompt-overlay'
                                    0)
                                  (string-width (buffer-string)) ; for prompt
                                  (string-width nb-cands-string)
                                  (length determ) ; for determined part
                                  2     ; for "{ "
                                  -2    ; for missing last "  " after last candidate
                                  5))	; for "... }"
               ;; Max total length to use, including the minibuffer content.
               (prospects-max  (* (+ icomplete-prospects-height
                                     ;; If the minibuffer content already uses up more than
                                     ;; one line, increase the allowable space accordingly.
                                     (/ prospects-len (window-width)))
                                  (window-width)))
               ;; Find the common prefix among COMPS.
               ;; Cannot use the optimization below because its assumptions are not always true,
               ;; e.g. when completion-cycling (Emacs bug #10850):
               ;; (if (eq t (compare-strings (car comps) nil (length most) most nil nil
               ;;                            completion-ignore-case))
               ;;     (length most) ; Common case.
               ;; Else, use try-completion.
               (prefix-len     (let ((comps-prefix  (try-completion "" comps)))
                                 (and (stringp comps-prefix)  (string-width comps-prefix))))
               (prospects      ())
               (keys           ())
               (limit          nil)
               (most-is-exact  nil)
               prompt  prompt-rest  comp)

          (when determ (put-text-property 0 (length determ) 'face 'icompletep-determined determ))
          (if (eq most-try t)           ; (or (null (cdr comps))
              (setq prospects  ())
            (while (and comps  (not limit))
              (setq comp   (if prefix-len (substring (car comps) prefix-len) (car comps))
                    comps  (cdr comps))
              (cond ((string= comp "") (setq most-is-exact  t))
                    ((member comp prospects))
                    (t (setq prospects-len  (+ (string-width comp)
                                               (string-width icomplete-separator)
                                               prospects-len))
                       (if (< prospects-len prospects-max) (push comp prospects) (setq limit  t))))))
          ;; Restore BASE-SIZE info, since `completion-all-sorted-completions' is cached.
          (when last (setcdr last base-size))
          (setq prompt-rest
                (if prospects
                    (concat "{ " (and most-is-exact  icompletep-exact-separator)
                            (mapconcat 'identity (nreverse prospects) icomplete-separator)
                            (and limit  "...")
                            " }")
                  (setq keys  (and icomplete-show-key-bindings
                                   (commandp (intern-soft most))
                                   (icomplete-get-keys most (eq t most-try))))
                  (if (eq keys 'TOO-LONG) ; No room even for `[ ... ]'.
                      ""
                    (concat " [ " (and (not keys)  "Matched") keys " ]"))))
          (unless (string= "" prompt-rest)
            (put-text-property 0 (length prompt-rest) 'face 'icompletep-choices prompt-rest))
          (cond ((< nb-candidates 2)
                 (setq prompt  (concat "      " determ prompt-rest)) ; 6 spaces.
                 (when (eq last-command this-command)
                   (setq icicle-nb-of-other-cycle-candidates  0))) ; We know now, so reset it.
                (t
                 (put-text-property (string-match "\\S-" nb-cands-string)
                                    (1- (length nb-cands-string))
                                    'face 'icompletep-nb-candidates nb-cands-string)
                 (setq prompt  (concat nb-cands-string determ prompt-rest))))
          (when (stringp keys)          ; Highlight keys.
            (put-text-property (+ 9 (length determ)) (1- (length prompt))
                               'face 'icompletep-keys prompt))
          ;; Append mention of number of other cycle candidates (from `icicles.el').
          (when (and (boundp 'icicle-last-completion-candidate)
                     (> icicle-nb-of-other-cycle-candidates 0)
                     (= 1 nb-candidates)
                     icicle-last-completion-candidate
                     (not (eq last-command this-command)))
            (setq nb-cands-string       ; Reuse the string.
                  (format "  (%d more)" icicle-nb-of-other-cycle-candidates))
            (put-text-property (string-match "\\S-" nb-cands-string) (length nb-cands-string)
                               'face 'icompletep-nb-candidates nb-cands-string)
            (setq prompt  (concat prompt nb-cands-string)))
          prompt))))

  ;; I sent a simplified version of this suggestion as Emacs bug #13322.
  ;; This version handles `completion-ignored-extensions', including for Icicles absolute file names.
  (defun icompletep-completion-all-sorted-completions (&optional sort-function dont-remove-dups)
    "Like `completion-all-sorted-completions', but with added optional args.
If SORT-FUNCTION is nil, sort per `completion-all-sorted-completions':
 * per property `cycle-sort-function', if defined
 * else by shorter length, then by recent use."
    (or completion-all-sorted-completions
        (let* ((start      (field-beginning))
               (end        (field-end))
               (string     (buffer-substring start end))
               (md         (completion--field-metadata start))
               (all        (completion-all-completions string  minibuffer-completion-table
                                                       minibuffer-completion-predicate
                                                       (- (point) start)  md))
               (last       (last all))
               (base-size  (or (cdr last)  0))
               (all-md     (completion--metadata (buffer-substring-no-properties start (point))
                                                 base-size md minibuffer-completion-table
                                                 minibuffer-completion-predicate))
               (sort-fun   (or sort-function
                               (completion-metadata-get all-md 'cycle-sort-function))))
          (when last
            (setcdr last ())
            ;; Do the rest after resetting LAST's cdr to (), since we need a proper list.
            ;; Exclude file names with extensions in `completion-ignored-extensions'.
            (when (or minibuffer-completing-file-name
                      (and (boundp 'icicle-abs-file-candidates)  icicle-abs-file-candidates))
              (let ((ignore-regexp  (regexp-opt completion-ignored-extensions))
                    ;; Only Emacs 24.3+ has library `cl-lib.el', with `cl-delete-if'.
                    (fun            (if (fboundp 'cl-delete-if)
                                        #'cl-delete-if
                                      #'icompletep-remove-if)))
                (setq all  (funcall fun (lambda (fl) (string-match-p ignore-regexp fl)) all))))
            (unless dont-remove-dups (setq all  (delete-dups all))) ; Delete duplicates.
            (setq last  (last all)      ; Reset LAST, since it may be a different cons-cell.
                  all   (if sort-fun
                            (funcall sort-fun all)
                          (sort all (lambda (c1 c2) (< (length c1) (length c2)))))) ; Shorter first.
            (when (and (minibufferp)  (not sort-fun))
              (let ((hist  (symbol-value minibuffer-history-variable)))
                (setq all  (sort all (lambda (c1 c2) (> (length (member c1 hist)) ; Recent first.
                                                        (length (member c2 hist))))))))
            ;; Cache result.  Not just for speed, but also so repeated calls to
            ;; `minibuffer-force-complete' can cycle through all possibilities.

            ;; $$$$$$ Temporary fix - will need to change more when `icomplete.el' in Emacs 24.4
            ;;        becomes more stable.  They changed the number of args here from 1 to 3.
            (if (fboundp 'icomplete--field-beg) ; Emacs 24.4+
                (completion--cache-all-sorted-completions
                 (icomplete--field-beg) (icomplete--field-end) (nconc all base-size))
              (completion--cache-all-sorted-completions (nconc all base-size)))))))
  )


(when (or (> emacs-major-version 24)    ; Emacs 24.4+
          (and (= emacs-major-version 24)  (> emacs-minor-version 3))
          (and (= emacs-major-version 24)  (string-match-p "24.3.50" emacs-version))) ;@@@@ TO REMOVE


  ;; REPLACES ORIGINAL defined in `icomplete.el':
  ;;
  ;; 1. Prepend total number of candidates.
  ;; 2. With Icicles, sort candidates using `icicle-reversible-sort' and show number of remaining
  ;;    cycle candidates.  You can cycle the sort order using `C-,'.
  ;; 3. Show candidates in a different face.
  ;; 4. Optionally show and highlight key bindings, truncating if too long.
  ;;
  (defun icomplete-completions (name candidates predicate require-match)
    "Identify prospective candidates for minibuffer completion.
NAME is the name to complete.
CANDIDATES is the collection of candidates to match.  See
`completing-read' for its possible values.
PREDICATE filters matches: they succeed only if it returns non-nil.
REQUIRE-MATCH non-nil means the input must match a candidate.

The display is updated with each minibuffer keystroke during
minibuffer completion.

Prospective completion suffixes (if any) are displayed, bracketed by
\"()\", \"[]\", or \"{}\".  The choice of brackets is as follows:

  \(...) - A single prospect is identified, and matching is enforced.
  \[...] - A single prospect is identified, and matching is optional.
  \{...} - Multiple prospects are indicated, and further input is
          needed to distinguish a single one.

The displays for unambiguous matches have ` [ Matched ]' appended
\(whether complete or not), or ` \[ No matches ]', if no eligible
matches exist.  Keybindings for uniquely matched commands are
shown within brackets, [] (without the word \"Matched\"), if there is
room.

When more than one completion is available, the total number precedes
the suffixes display, like this:

  M-x forw    14 (ard-) { char line list...}

In Icicle mode:
 * The current Icicles sort order is used.
 * When cycling candidates and you change direction, the number of
   other cycling candidates is shown, as `(N more)'.  For example:

   M-x forward-line   [Matched]  (13 more)."
    ;; `concat'/`mapconcat' is the slow part.
    (let* ((non-essential                    t)
	   (minibuffer-completion-table      candidates)
	   (minibuffer-completion-predicate  predicate)
	   (icyp                             (and (boundp 'icicle-mode)  icicle-mode))
           (open-bracket                     (if require-match "("   " ["))
           (close-bracket                    (if require-match ") "  "] "))
           (md                               (completion--field-metadata (icomplete--field-beg)))
           (comps                            (icompletep-completion-all-sorted-completions
                                              (icomplete--field-beg) (icomplete--field-end)
                                              (and icyp  #'icicle-reversible-sort)))
           (last                             (and (consp comps)  (last comps)))
           (base-size                        (cdr last))
           nb-candidates  nb-cands-string)
      (if (not (consp comps))
          (format "\t%sNo matches%s" open-bracket close-bracket)
        (when last (setcdr last ()))
        (setq nb-candidates    (length comps)
              nb-cands-string  (if (< nb-candidates 2) "" (format "%7d " nb-candidates)))
        (let* ((most-try       (if (and base-size  (> base-size 0))
                                   (completion-try-completion
                                    name candidates predicate (length name) md)
                                 ;; If COMPS are 0-based, the result should be the same with COMPS.
                                 (completion-try-completion name comps nil (length name) md)))
               (most           (if (consp most-try) (car most-try) (if most-try (car comps) "")))
               ;; Compare NAME & MOST, to determine if NAME is a prefix of MOST, or something else.
               (compare        (compare-strings name nil nil most nil nil completion-ignore-case))
               (determ         (and (not (or (eq t compare)  (eq t most-try)
                                             (= (setq compare  (1- (abs compare))) (length most))))
                                    (concat open-bracket
                                            (cond ((= compare (length name)) ; NAME is a prefix
                                                   (substring most compare))
                                                  ((< compare 5) most)
                                                  (t (concat "..." (substring most compare))))
                                            close-bracket)))
               (prospects-len  (+ (if (and icyp  (icicle-completing-p))
                                      2 ; for `icicle-completion-prompt-overlay'
                                    0)
                                  (string-width (buffer-string)) ; for prompt
                                  (string-width nb-cands-string)
                                  (length determ) ; for determined part
                                  2     ; for "{ "
                                  -2    ; for missing last "  " after last candidate
                                  5))	; for "... }"
               ;; Max total length to use, including the minibuffer content.
               (prospects-max  (* (+ icomplete-prospects-height
                                     ;; If the minibuffer content already uses up more than
                                     ;; one line, increase the allowable space accordingly.
                                     (/ prospects-len (window-width)))
                                  (window-width)))
               ;; Find the common prefix among COMPS.
               ;; Cannot use the optimization below because its assumptions are not always true,
               ;; e.g. when completion-cycling (Emacs bug #10850):
               ;; (if (eq t (compare-strings (car comps) nil (length most) most nil nil
               ;;                            completion-ignore-case))
               ;;     (length most) ; Common case.
               ;; Else, use try-completion.
               (prefix         (and icomplete-hide-common-prefix  (try-completion "" comps)))
               (prefix-len     (and (stringp prefix)
                                    ;; Hide prefix only if the info is already displayed via `most'.
                                    (string-prefix-p prefix most 'IGNORE-CASE)
                                    (string-width prefix)))
               (prospects      ())
               (keys           ())
               (limit          nil)
               (most-is-exact  nil)
               prompt  prompt-rest  comp)

          (when determ (put-text-property 0 (length determ) 'face 'icompletep-determined determ))
          (if (or (eq most-try t)  (not (consp (cdr comps))))
              (setq prospects  ())

            ;; @@@@@@@@ ???????????
            (when (member name comps)
              ;; NAME is complete but not unique.  This scenario poses following UI issues:
              ;; - When `icomplete-hide-common-prefix' is non-nil, NAME is stripped empty.
              ;;   This would make the entry inconspicuous.
              ;; - Due to sorting of completions, NAME may not be the first of the prospects
              ;;   and could be hidden deep in the displayed string.
              ;; - Because of `icomplete-prospects-height', NAME may not even be displayed.
              ;;
              ;; So provide a visual cue to user via an "empty string" in the try completion field.
              (setq determ  (concat open-bracket "" close-bracket)))
            (while (and comps  (not limit))
              (setq comp   (if prefix-len (substring (car comps) prefix-len) (car comps))
                    comps  (cdr comps))
              (cond ((string= comp "") (setq most-is-exact  t))
                    ((member comp prospects))
                    (t (setq prospects-len  (+ (string-width comp)
                                               (string-width icomplete-separator)
                                               prospects-len))
                       (if (< prospects-len prospects-max) (push comp prospects) (setq limit  t))))))
          ;; Restore BASE-SIZE info, since `completion-all-sorted-completions' is cached.
          (when last (setcdr last base-size))
          (setq prompt-rest
                (if prospects
                    (concat "{ " (and most-is-exact  icompletep-exact-separator)
                            (mapconcat 'identity (nreverse prospects) icomplete-separator)
                            (and limit  "...")
                            " }")
                  (setq keys  (and icomplete-show-key-bindings
                                   (commandp (intern-soft most))
                                   (icomplete-get-keys most (eq t most-try))))
                  (if (eq keys 'TOO-LONG) ; No room even for `[ ... ]'.
                      ""
                    (concat " [ " (and (not keys)  "Matched") keys " ]"))))
          (unless (string= "" prompt-rest)
            (put-text-property 0 (length prompt-rest) 'face 'icompletep-choices prompt-rest))
          (cond ((< nb-candidates 2)
                 (setq prompt  (concat "      " determ prompt-rest)) ; 6 spaces.
                 (when (eq last-command this-command)
                   (setq icicle-nb-of-other-cycle-candidates  0))) ; We know now, so reset it.
                (t
                 (put-text-property (string-match "\\S-" nb-cands-string)
                                    (1- (length nb-cands-string))
                                    'face 'icompletep-nb-candidates nb-cands-string)
                 (setq prompt  (concat nb-cands-string determ prompt-rest))))
          (when (stringp keys)          ; Highlight keys.
            (put-text-property (+ 9 (length determ)) (1- (length prompt))
                               'face 'icompletep-keys prompt))
          ;; Append mention of number of other cycle candidates (from `icicles.el').
          (when (and (boundp 'icicle-last-completion-candidate)
                     (> icicle-nb-of-other-cycle-candidates 0)
                     (= 1 nb-candidates)
                     icicle-last-completion-candidate
                     (not (eq last-command this-command)))
            (setq nb-cands-string       ; Reuse the string.
                  (format "  (%d more)" icicle-nb-of-other-cycle-candidates))
            (put-text-property (string-match "\\S-" nb-cands-string) (length nb-cands-string)
                               'face 'icompletep-nb-candidates nb-cands-string)
            (setq prompt  (concat prompt nb-cands-string)))
          prompt))))


  (defun icompletep-completion-all-sorted-completions (&optional start end
                                                       sort-function dont-remove-dups)
    "Like `completion-all-sorted-completions', but with added optional args.
Non-nil SORT-FUNCTION is a function that sorts the candidates.
If SORT-FUNCTION is nil, sort per `completion-all-sorted-completions':
 * per property `cycle-sort-function', if defined
 * else by shorter length, then by recent use.
Non-nil DONT-REMOVE-DUPS means do not remove duplicates."
    (or completion-all-sorted-completions
        (let* ((start      (or start  (minibuffer-prompt-end)))
               (end        (or end  (point-max)))
               (string     (buffer-substring start end))
               (md         (completion--field-metadata start))
               (all        (completion-all-completions string  minibuffer-completion-table
                                                       minibuffer-completion-predicate
                                                       (- (point) start)  md))
               (last       (last all))
               (base-size  (or (cdr last)  0))
               (all-md     (completion--metadata (buffer-substring-no-properties start (point))
                                                 base-size md minibuffer-completion-table
                                                 minibuffer-completion-predicate))
               (sort-fun   (or sort-function
                               (completion-metadata-get all-md 'cycle-sort-function))))
          (when last
            (setcdr last ())
            ;; Do the rest after resetting LAST's cdr to (), since we need a proper list.
            ;; Exclude file names with extensions in `completion-ignored-extensions'.
            (when (or minibuffer-completing-file-name
                      (and (boundp 'icicle-abs-file-candidates)  icicle-abs-file-candidates))
              (let ((ignore-regexp  (regexp-opt completion-ignored-extensions)))
                (setq all  (cl-delete-if (lambda (fl) (string-match-p ignore-regexp fl)) all))))
            (unless dont-remove-dups (setq all  (delete-dups all))) ; Delete duplicates.
            (setq last  (last all)      ; Reset LAST, since it may be a different cons-cell.
                  all   (if sort-fun
                            (funcall sort-fun all)
                          (sort all (lambda (c1 c2) (< (length c1) (length c2)))))) ; Shorter first.
            (when (and (minibufferp)  (not sort-fun))
              (let ((hist  (symbol-value minibuffer-history-variable)))
                (setq all  (sort all (lambda (c1 c2) (> (length (member c1 hist)) ; Recent first.
                                                        (length (member c2 hist))))))))
            ;; Cache result.  Not just for speed, but also so repeated calls to
            ;; `minibuffer-force-complete' can cycle through all possibilities.
            (completion--cache-all-sorted-completions start end (nconc all base-size)))))))


(defvar icompletep-cycling-mode nil)
(when (boundp 'icomplete-minibuffer-map) ; Emacs 24.4+

  (setq icompletep-ORIG-icomplete-minibuffer-map  icomplete-minibuffer-map ; Save it and wipe it out.
        icomplete-minibuffer-map                  nil)

  ;; Eval this so that even if the library is byte-compiled with Emacs 20,
  ;; loading it into Emacs 21+ will define variable `icompletep-cycling-mode'.
  (eval '(define-minor-mode icompletep-cycling-mode
          "Icomplete mode, with its cycling keys enabled if on, disabled if off.
If off then the cycling keys of Icomplete mode are free for their
normal minibuffer uses.  The default Icomplete-mode cycling keys are:

M-TAB\t- complete to exact match, repeat to cycle
C-j\t- complete to first match and exit minibuffer
C-s\t- cycle first entry to end
C-r\t- cycle last entry to start

If you want Icomplete cycling behavior but do not want to use these
keys for it, customize the bindings in `icomplete-minibuffer-map'.

Turning this mode on also turns on Icomplete mode.
Turning it off does not turn Icomplete mode on or off."
          nil nil nil
          (when icompletep-cycling-mode (icomplete-mode 99)) ; Turn on Icomplete if cycling is on.
          (setq icomplete-minibuffer-map  (and icompletep-cycling-mode
                                           icompletep-ORIG-icomplete-minibuffer-map))))
  (icompletep-cycling-mode -99)         ; Turn it off by default.

  (defadvice icomplete-mode (before icompletep-doc activate)
    "
Icomplete+ enhances vanilla Icomplete mode in these ways:
 * Better display of candidates - highlighting and showing how many.
 * Shows key bindings for command, optionally including menu bindings.
 * Does not bind keys for cycling in `icomplete-mode', so you can use
   those keys normally in the minibuffer.  If you want cycling then
   enable minor mode `icompletep-cycling-mode'.
 * Provides support for Icicles:
   . Respects Icicles sort order, which you can cycle using `C-,'.
   . When you change direction cycling candidates with Icicles, shows
     the number of other cycle candidates.")

  )

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icomplete+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icomplete+.el ends here
