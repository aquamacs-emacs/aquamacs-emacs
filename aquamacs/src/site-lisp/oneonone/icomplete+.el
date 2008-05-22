;;; icomplete+.el --- Extensions to `icomplete.el'.
;;
;; Filename: icomplete+.el
;; Description: Extensions to `icomplete.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2008, Drew Adams, all rights reserved.
;; Created: Mon Oct 16 13:33:18 1995
;; Version: 21.0
;; Last-Updated: Tue Jan 01 14:05:56 2008 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 639
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icomplete+.el
;; Keywords: help, abbrev, internal, extensions, local
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `icomplete'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `icomplete.el'.
;;
;;  Faces defined here:
;;
;;    `icompletep-choices', `icompletep-determined',
;;    `icompletep-keys', `icompletep-nb-candidates'.
;;
;;  User option defined here:
;;
;;    `icompletep-prospects-length'.
;;
;;
;;  ***** NOTE: The following functions defined in `icomplete.el'
;;              have been REDEFINED HERE:
;;
;;  `icomplete-completions' -
;;     Sorts alternatives and puts them in a different face.
;;  `icomplete-exhibit' -
;;     Saves match-data.
;;     Doesn't insert if input begins with `(' (e.g.
;;       `repeat-complex-command').
;;     Set `deactivate-mark' to nil at end, so the insertion doesn't
;;       deactivate mark.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `icomplete.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "icomplete" '(progn (require 'icomplete+)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
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
(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, unless

;;;;;;;;;;;;;;;;;;;

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
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icomplete+.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/IcompleteMode#IcompleteModePlus")
  :link '(emacs-commentary-link :tag "Commentary" "icomplete+")
  )

(defface icompletep-choices
    '((((background dark)) (:foreground "Snow4"))
      (t (:foreground "DarkBlue")))
  "*Face for minibuffer reminder of possible completion suffixes."
  :group 'Icomplete-Plus)

(defface icompletep-determined
    '((t (:foreground "SeaGreen")))
  "*Face for minibuffer reminder of possible completion prefix."
  :group 'Icomplete-Plus)

(defface icompletep-nb-candidates
  '((((background dark)) (:foreground "SpringGreen"))
    (t (:foreground "DarkMagenta")))
  "*Face for minibuffer reminder of number of completion candidates.
This has no effect unless library `icicles.el' is being used."
  :group 'Icomplete-Plus)

(defface icompletep-keys
    '((t (:foreground "Red")))
  "*Face for minibuffer reminder of possible completion key bindings."
  :group 'Icomplete-Plus)

(defcustom icompletep-prospects-length 100 ; Default was 80
  "*Length of string displaying icompletion candidates."
  :type 'integer :group 'Icomplete-Plus)



;; REPLACES ORIGINAL defined in `icomplete.el':
;;
;; Save match-data.
;; Don't insert if Icicles apropos-completing.
;; Don't insert if input begins with `(' (e.g. `repeat-complex-command').
;;
;;;###autoload
(defun icomplete-exhibit ()
  "Insert icomplete completions display.
Should be run via minibuffer `post-command-hook'.
See `icomplete-mode' and `minibuffer-setup-hook'."
  (when (icomplete-simple-completing-p)
    (save-match-data
      (let* ((minibuf-begin (if (< emacs-major-version 21)
                                (point-min)
                              (minibuffer-prompt-end)))
             (contents (buffer-substring minibuf-begin (point-max)))
             (buffer-undo-list t))
        (save-excursion
          (goto-char (point-max))
          ;; Register the end of input, so we know where the extra stuff
          ;; (match-status info) begins:
          (unless (boundp 'icomplete-eoinput)
            ;; In case it got wiped out by major mode business:
            (make-local-variable 'icomplete-eoinput))
          (setq icomplete-eoinput (point))
          ;; Insert the match-status information:
          (when (and (or(not (boundp 'icicle-apropos-completing-p))
                        (not icicle-apropos-completing-p))
                     (> (point-max) minibuf-begin)
                     (save-excursion
                       (goto-char minibuf-begin)
                       (not (looking-at ; No (, ", ', 9 etc. at start.
                             "\\(\\s-+$\\|\\s-*\\(\\s(\\|\\s\"\\|\\s'\\|\\s<\\|[0-9]\\)\\)")))
                     (or
                      ;; Don't bother with delay after certain number of chars:
                      (> (point-max) icomplete-max-delay-chars)
                      ;; Don't delay if alternatives number is small enough:
                      (if minibuffer-completion-table
                          (cond ((numberp minibuffer-completion-table)
                                 (< minibuffer-completion-table
                                    icomplete-delay-completions-threshold))
                                ((sequencep minibuffer-completion-table)
                                 (< (length minibuffer-completion-table)
                                    icomplete-delay-completions-threshold))
                                ))
                      ;; Delay - give some grace time for next keystroke, before
                      ;; embarking on computing completions:
                      (sit-for icomplete-compute-delay)))
            (insert
             (icomplete-completions contents minibuffer-completion-table
                                    minibuffer-completion-predicate
                                    (not minibuffer-completion-confirm)))))
        (setq deactivate-mark nil))))) ; Don't let the insert deactivate the mark.



;; REPLACES ORIGINAL defined in `icomplete.el':
;; Sorts alternatives and puts them in a different face.
;;
;;;###autoload
(defun icomplete-completions (name candidates predicate require-match)
  "Identify prospective candidates for minibuffer completion.
NAME is the name to complete.
CANDIDATES are the candidates to match.
PREDICATE filters matches: they succeed only if this returns non-nil.
REQUIRE-MATCH non-nil means the input must match a candidate.

The display is updated with each minibuffer keystroke during
minibuffer completion.

Prospective completion suffixes (if any) are displayed, bracketed by
\"()\", \"[]\", or \"{}\".  The choice of brackets is as follows:

  \(...) - A single prospect is identified and matching is enforced.
  \[...] - A single prospect is identified and matching is optional.
  \{...} - Multiple prospects, separated by commas, are indicated,
           and further input is required to distinguish a single one.

The displays for unambiguous matches have \" [ Matched ]\" appended
\(whether complete or not), or \" \[ No match ]\", if no eligible
matches exist.
Keybindings for uniquely matched commands are displayed within the [].

When more than one completion is available, the total number precedes
the suffixes display, like so:
  M-x forw    14 (ard-) { char line list...}

If library `icicles.el' is also loaded, then you can cycle
completions.  When you change cycling direction, the number of
additional cycle candidates, besides the current one, is displayed
following the rest of the icomplete info:
  M-x forward-line   [Matched]  (13 more)."
  ;; 'all-completions' doesn't like empty
  ;; minibuffer-completion-table's (ie: (nil))
  (when (and (listp candidates) (null (car candidates))) (setq candidates nil))
  (let ((comps (all-completions name candidates predicate))
        ;; "-determined" - only one candidate
        (open-bracket-determined (if require-match "(" " ["))
        (close-bracket-determined (if require-match ") " "] "))
        (keys nil)
        nb-candidates nb-candidates-string)
    (setq nb-candidates (length comps))
    ;; `concat'/`mapconcat' is the slow part.  With the introduction of
    ;; `icompletep-prospects-length', there is no need for `catch'/`throw'.
    (if (null comps) (format (if (fboundp 'icicle-apropos-complete)
                                 "\t%sNo prefix matches%s"
                               "\t%sNo matches%s")
                             open-bracket-determined
                             close-bracket-determined)
      (let* ((most-try (try-completion name (mapcar #'list comps)))
             (most (if (stringp most-try) most-try (car comps)))
             (most-len (length most))
             (determ (and (> most-len (length name))
                          (concat open-bracket-determined
                                  (substring most (length name))
                                  close-bracket-determined)))
             (open-bracket-prospects "{ ")
             (close-bracket-prospects " }")
             ;; "-prospects" - more than one candidate
             (prospects-len 0)
             prompt prompt-rest prospects most-is-exact comp)
        (when determ
          (put-text-property 0 (length determ) 'face 'icompletep-determined determ))
        (if (eq most-try t)
            (setq prospects nil)
          (while (and comps (< prospects-len icompletep-prospects-length))
            (setq comp (substring (car comps) most-len)
                  comps (cdr comps))
            (cond ((string-equal comp "") (setq most-is-exact t))
                  ((member comp prospects))
                  (t (setq prospects (cons comp prospects)
                           prospects-len (+ (length comp) 1 prospects-len))))))
        (setq prompt-rest
              (if prospects
                  (concat open-bracket-prospects
                          (and most-is-exact ", ")
                          (mapconcat 'identity
                                     (sort prospects (function string-lessp))
                                     "  ")
                          (and comps "...")
                          close-bracket-prospects)
                (concat "\t[ Matched"
                        (if (setq keys (and icomplete-show-key-bindings
                                            (commandp (intern-soft most))
                                            (icomplete-get-keys most)))
                            (concat "; " keys)
                          (setq keys nil))
                        " ]")))
        (put-text-property 0 (length prompt-rest)
                           'face 'icompletep-choices prompt-rest)
        (cond ((< nb-candidates 2)
               (setq prompt (concat "      " determ prompt-rest))
               (when (eq last-command this-command)
                 (setq icicle-nb-of-other-cycle-candidates 0))) ; We know now, so reset it.
              (t
               (setq nb-candidates-string (format "%7d " nb-candidates))
               (put-text-property (string-match "\\S-" nb-candidates-string)
                                  (1- (length nb-candidates-string))
                                  'face 'icompletep-nb-candidates nb-candidates-string)
               (setq prompt (concat nb-candidates-string determ prompt-rest))))
        ;; Highlight keys, after "Matched; " (18 chars).
        (when keys (put-text-property (+ 18 (length determ)) (1- (length prompt))
                                      'face 'icompletep-keys prompt))
        ;; Append mention of number of other cycle candidates (from `icicles.el').
        (when (and (boundp 'icicle-last-completion-candidate)
                   (> icicle-nb-of-other-cycle-candidates 0)
                   (= 1 nb-candidates)
                   icicle-last-completion-candidate
                   (not (eq last-command this-command)))
          (setq nb-candidates-string    ; Reuse the string.
                (format "  (%d more)" icicle-nb-of-other-cycle-candidates))
          (put-text-property (string-match "\\S-" nb-candidates-string)
                             (length nb-candidates-string)
                             'face 'icompletep-nb-candidates nb-candidates-string)
          (setq prompt (concat prompt nb-candidates-string)))
        prompt))))




;;; The following functions have been REDEFINED to reset the
;;; `minibuffer-completion-table' in order to avoid icompletion.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Note:  The function `read-input' is an alias for `read-string'.

;; (or (fboundp 'old-read-string)
;; (fset 'old-read-string (symbol-function 'read-string)))

;; ;; REPLACES ORIGINAL:
;; ;; Resets `minibuffer-completion-table' to avoid icompletion.
;; (defsubst read-string
;;   (prompt &optional initial-input history default-value inherit-input-method)
;;   "Read a string from the minibuffer, prompting with string PROMPT.
;; If non-nil, second arg INITIAL-INPUT is a string to insert before
;;     reading.  This argument has been superseded by DEFAULT-VALUE and
;;     should normally be `nil' in new code.  It behaves as in
;;     `read-from-minibuffer'.  See the documentation for that function.
;; The third arg HISTORY, if non-nil, specifies a history list and
;;     optionally the initial position in that list.
;;     See `read-from-minibuffer' for details of argument HISTORY.
;; Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
;;     for history commands and as the value to return if the user enters
;;     an empty string.
;; Fifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer
;;     inherits the current input method and setting of
;;     `enable-multibyte-characters'."
;;   (setq minibuffer-completion-table nil) ; So won't icomplete by default.
;;   (old-read-string prompt initial-input history default-value inherit-input-method))


;; (or (fboundp 'old-read-from-minibuffer)
;; (fset 'old-read-from-minibuffer (symbol-function 'read-from-minibuffer)))

;; ;; REPLACES ORIGINAL:
;; ;; Resets `minibuffer-completion-table' to avoid icompletion.
;; (defsubst read-from-minibuffer
;;   (prompt &optional initial-contents keymap read hist default-value
;;           inherit-input-method keep-all)
;;   "Read a string from the minibuffer, prompting with string PROMPT.
;; The optional second arg INITIAL-CONTENTS is an obsolete alternative to
;;   DEFAULT-VALUE.  It normally should be nil in new code, except when
;;   HIST is a cons.  It is discussed in more detail below.
;; Third arg KEYMAP is a keymap to use while reading;
;;   if omitted or nil, the default is `minibuffer-local-map'.
;; If fourth arg READ is non-nil, then interpret the result as a Lisp
;;   object and return that object.  In other words, do this:
;;       `(car (read-from-string INPUT-STRING))'
;; Fifth arg HIST, if non-nil, specifies a history list and optionally
;;   the initial position in the list.
;;   It can be a symbol, which is the history list variable to use,
;;   or it can be a cons cell (HISTVAR . HISTPOS).
;;   In that case, HISTVAR is the history-list variable to use,
;;   and HISTPOS is the initial position for use by the minibuffer
;;   history commands.  For consistency, you should also specify that
;;   element of the history as the value of INITIAL-CONTENTS.
;;   Positions are counted starting from 1 at the beginning of the list.
;; Sixth arg DEFAULT-VALUE is the default value.  If non-nil, it is
;;   available for history commands; but, unless READ is non-nil,
;;   `read-from-minibuffer' does NOT return DEFAULT-VALUE if the user
;;   enters empty input!  It returns the empty string.
;; Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer
;;   inherits the current input method and the setting of
;;   `enable-multibyte-characters'.
;; Eighth arg KEEP-ALL, if non-nil, says to put all inputs in the history
;;  list, even empty or duplicate inputs. (This argument is not available
;;  in Emacs versions prior to Emacs 22.)
;; If variable `minibuffer-allow-text-properties' is non-nil, then the
;;   string returned includes whatever text properties were present in
;;   the minibuffer.  Otherwise the value has no text properties.

;; The remainder of this documentation describes INITIAL-CONTENTS in more
;; detail.  It is relevant only when studying existing code, or when HIST
;; is a cons.  If non-nil, INITIAL-CONTENTS is a string to be inserted
;; into the minibuffer before reading input.  Normally, point is put at
;; the end of that string.  However, if INITIAL-CONTENTS is (STRING .
;; POSITION), the initial input is STRING, but point is placed at
;; _one-indexed_ position POSITION in the minibuffer.  Any integer value
;; less than or equal to one puts point at the beginning of the string.
;; *Note* that this behavior differs from the way such arguments are used
;; in `completing-read' and some related functions, which use
;; zero-indexing for POSITION."
;;   (setq minibuffer-completion-table nil) ; So won't icomplete by default.
;;   (if (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
;;       (old-read-from-minibuffer prompt initial-contents keymap read hist
;;                                 default-value inherit-input-method keep-all)
;;     (old-read-from-minibuffer prompt initial-contents keymap read hist
;;                               default-value inherit-input-method))) ; No KEEP-ALL


;; (or (fboundp 'old-read-no-blanks-input)
;; (fset 'old-read-no-blanks-input (symbol-function 'read-no-blanks-input)))

;; ;; REPLACES ORIGINAL:
;; ;; Resets `minibuffer-completion-table' to avoid icompletion.
;; (defsubst read-no-blanks-input (prompt &optional initial-contents inherit-input-method)
;;   "Read a string from the minibuffer, not allowing blanks.
;; Arg PROMPT is a prompt string.  Whitespace terminates the input.

;; If optional second arg INITIAL-CONTENTS is non-nil, it should be a
;; string, which is used as initial input, with point positioned at the
;; end, so that a SPACE will accept the input.  INITIAL-CONTENTS can
;; alternatively be a cons of a string and an integer.  Such values are
;; treated as in `read-from-minibuffer', but are normally not useful in
;; this function.

;; Third arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer
;; inherits the current input method and the setting of
;; `enable-multibyte-characters'."
;;   (setq minibuffer-completion-table nil) ; So won't icomplete by default.
;;   (old-read-no-blanks-input prompt initial-contents inherit-input-method))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icomplete+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icomplete+.el ends here

