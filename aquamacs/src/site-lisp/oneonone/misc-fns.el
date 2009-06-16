;;; misc-fns.el --- Miscellaneous non-interactive functions.
;;
;; Filename: misc-fns.el
;; Description: Miscellaneous non-interactive functions.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2008, Drew Adams, all rights reserved.
;; Created: Tue Mar  5 17:21:28 1996
;; Version: 21.0
;; Last-Updated: Tue Jan 01 13:36:50 2008 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 462
;; URL: http://www.emacswiki.org/cgi-bin/wiki/misc-fns.el
;; Keywords: internal, unix, lisp, extensions, local
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `misc-fns'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Miscellaneous non-interactive functions.
;;
;;  Code here is organized into sections by area affected.
;;
;;    Sections are separated by `;;;$ ... ---------------'.
;;
;;  You may want to put this in your `~/.emacs' file, to erase the
;;  minibuffer when it is inactive and `minibuffer-empty-p':
;;
;;   (require 'misc-fns)
;;   (add-hook '<mode>-hook 'notify-user-of-mode), for each <mode>.
;;
;;
;;  Face defined here: `notifying-user-of-mode'.
;;
;;  User options (variables) defined here:
;;
;;    `buffer-modifying-cmds', `mode-line-reminder-duration',
;;    `notifying-user-of-mode-flag'.
;;
;;  Functions defined here:
;;
;;    `another-buffer', `current-line', `display-in-mode-line',
;;    `do-files', `flatten', `fontify-buffer', `force-time-redisplay',
;;    `interesting-buffer-p', `live-buffer-name',
;;    `make-transient-mark-mode-buffer-local', `mod-signed',
;;    `notify-user-of-mode', `region-or-buffer-limits', `signum',
;;    `simple-set-difference', `simple-set-intersection',
;;    `simple-set-union', `undefine-keys-bound-to',
;;    `undefine-killer-commands'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2007/09/25 dadams
;;     buffer-modifying-cmds: Respect kill-read-only-ok.
;; 2007/09/22 dadams
;;     NOTE: If you upgrade this library, and you use any of these libraries, then
;;           you MUST upgrade them also: buff-menu+.el, compile+.el, dired+.el,
;;           start-opt.el. 
;;     undefine-keys-bound-to, undefine-keys-bound-to: Removed optional OLD-MAP arg.
;;     undefine-keys-bound-to: Redefined using where-is-internal and lookup-key.
;;     buffer-modifying-cmds: Added lots more, some from Emacs 22.
;; 2007/04/02 dadams
;;     Added: region-or-buffer-limits.
;; 2006/12/11 dadams
;;     undefine-*: Don't bind to undefined if command is already bound in keymap.
;; 2006/03/31 dadams
;;     No longer use display-in-minibuffer.
;; 2006-02-20 dadams
;;     Added signum.
;; 2005/12/30 dadams
;;     Removed stray require of def-face-const.el.
;; 2005/12/18 dadams
;;     buffer-modifying-cmds, mode-line-reminder-duration: defvar -> defcustom.
;;     notify-user-of-mode-face -> notify-user-of-mode.
;;       Use defface.  Removed require of def-face-const.el.
;;     notify-user-of-mode (variable) -> notify-user-of-mode-flag.
;;       defvar -> defcustom.
;;     undefine-keys-bound-to: defsubst -> defun.
;; 2005/10/28 dadams
;;     notify-user-of-mode: Don't notify if minibuffer is active.
;; 2005/09/30 dadams
;;     Renamed simple-intersection to simple-set-intersection.
;;     Added: simple-set-union.
;; 2005/09/26 dadams
;;     Added simple-set-difference.
;; 2005/07/21 dadams
;;     Added simple-intersection.
;; 2005/01/25 dadams
;;     Removed ###autoload on defvars.
;; 2004/12/30 dadams
;;     Added flatten.
;; 2004/11/28 dadams
;;     Added mod-signed.
;; 2004/10/13 dadams
;;     Removed special-display-buffer-p (just use special-display-p)
;; 2004/03/xx dadams
;;     Added fontify-buffer.
;; 1999/03/17 dadams
;;     1. Added: live-buffer-name.
;;     2. interesting-buffer-p: buffer-live-p -> live-buffer-name.
;;     3. notify-user-of-mode: message if display-in-minibuffer not defined;
;;        protect with fboundp.
;; 1996/04/23 dadams
;;     Added: undefine-keys-bound-to, undefine-killer-commands,
;;            buffer-modifying-cmds.
;; 1996/04/04 dadams
;;     Added special-display-buffer-p.
;; 1996/03/18 dadams
;;     Simplified display-in-mode-line.
;; 1996/03/18 dadams
;;     notify-user-of-mode: message -> display-in-minibuffer.
;; 1996/03/08 dadams
;;     Redefined another-buffer in terms of other-buffer.
;; 1996/02/06 dadams
;;     Put variable-interactive property on appropriate user option vars.
;; 1995/11/10 dadams
;;     make-transient-mark-mode-buffer-local: Added arg and set new default value
;;         from existing default value, not from existing (possibly local) value.
;; 1995/11/09 dadams
;;     Added make-transient-mark-mode-buffer-local.
;; 1995/10/25 dadams
;;     force-time-redisplay: Use update-mode-line macro.
;; 1995/08/10 dadams
;;     Added: display-in-mode-line, force-time-redisplay,
;;     mode-line-reminder-duration.
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

(and (< emacs-major-version 21)         ;; dolist, push, pop
     (eval-when-compile (require 'cl))) ;; (plus, for Emacs <20: when, unless)

;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-fns)
(require 'misc-fns)                 ; Ensure loaded before compile.

;;;;;;;;;;;;;;;;;;;;;


;;;$ MODE-LINE ----------------------------------------------------------------

(defcustom mode-line-reminder-duration 10
  "*Maximum number of seconds to display a reminder in the mode-line."
  :type 'integer)

;; From `show-bind.el'.
;;;###autoload
(defun display-in-mode-line (text)
  "Display TEXT in mode line for `mode-line-reminder-duration' seconds."
  (let ((mode-line-format (list text)))
    (force-mode-line-update)
    (sit-for mode-line-reminder-duration))
  (force-mode-line-update))

;;;###autoload
(defun force-time-redisplay ()
  "Force a redisplay.
This is probably obsolete now.  Use `force-mode-line-update'."
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))



;;;$ BUFFERS ------------------------------------------------------------------

;;;###autoload
(defun another-buffer (&optional buffer visible-ok)
  "First buffer in `buffer-list' whose name does not start with a space.
This is the first buffer in the list.
Arg BUFFER is excepted if another can be found, besides \"*scratch*\".
Arg VISIBLE-OK is as for `other-buffer'.

Differs from `other-buffer' in:
 1) If BUFFER is nil, `current-buffer' is used as the excepted BUFFER.
 2) BUFFER is used, not *scratch* buffer, if no other buffer exists
    (unless BUFFER starts with a space).  That is, BUFFER is excepted
    only if another, besides *scratch*, can be found."
  (setq buffer (or buffer (current-buffer))) ; Default.
  (let ((buf (other-buffer buffer visible-ok)))
    (if (and (eq (get-buffer-create "*scratch*") buf)
             (not (eq 0 (string-match  " " (buffer-name buffer)))))
        buffer ;; This buffer is better than *scratch*.
      buf))) ;; `other-buffer' was good enough.


;; This differs from the standard Emacs function `buffer-live-p' in that:
;; 1. The BUFFER arg may be a buffer or its name.
;; 2. This returns the buffer name, not `t', if the buffer is live.
(defun live-buffer-name (buffer)
  "Return BUFFER's name if a buffer that has not been deleted, else nil.
BUFFER may be either a buffer or its name (a string)."
  (setq buffer (and buffer (get-buffer buffer))) ; Convert to buffer, if any.
  (and buffer (buffer-name buffer)))    ; Return buffer's name.

;;;###autoload
(defun interesting-buffer-p (buffer)
  "Non-nil if BUFFER is a live buffer whose name does not start with SPC."
  (and buffer (setq buffer (live-buffer-name buffer)) ; Live buffer's name.
       (or (zerop (length buffer))      ; Not an empty name.
           (not (char-equal ?\  (aref buffer 0)))))) ; Starts with non-blank.

;; Stolen from file `intes.el.2'
;;;###autoload
(defun current-line ()
  "Current line number of cursor."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)))

(defun fontify-buffer (buffer &rest ignore)
  "Fontify buffer BUFFER.
Usable as a candidate for `compilation-finish-functions'.
Any arguments besides BUFFER (IGNORE list) are ignored."
  (save-excursion (set-buffer buffer) (font-lock-fontify-buffer)))



;;;$ REGION -------------------------------------------------------------------

;;;###autoload
(defun make-transient-mark-mode-buffer-local (&optional default)
  "Make variable `transient-mark-mode' permanent-local everywhere.
Set default value to arg DEFAULT, if non-nil, else `default-value' of
`transient-mark-mode'.  This means that if already buffer-local, its
default value is not changed."
  (make-variable-buffer-local 'transient-mark-mode)
  (put 'transient-mark-mode 'permanent-local t)
  (setq-default transient-mark-mode (or default
                                        (default-value 'transient-mark-mode))))

(defun region-or-buffer-limits ()
    "Return the start and end of the region as a list, smallest first.
If the region is not active or empty, then bob and eob are used."
  (if (or (not mark-active) (null (mark)) (= (point) (mark)))
      (list (point-min) (point-max))
    (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))



;;;$ MODE ---------------------------------------------------------------------

(defcustom notifying-user-of-mode-flag t
  "*Non-nil means to display messages notifying user of mode changes.
See function `notify-user-of-mode'."
  :type 'boolean)

(defface notify-user-of-mode '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
  "*Face used for notifying user of current major mode.
See function `notify-user-of-mode'.")

;;;###autoload
(defun notify-user-of-mode (&optional buffer anyway)
  "Display msg naming major mode of BUFFER (default: current buffer).
A message is never displayed if the minibuffer is active.  Otherwise:
  No msg is displayed if not `notifying-user-of-mode-flag' or BUFFER
  is internal, unless optional 2nd arg ANYWAY is non-nil.
  In that case, msg is displayed anyway.
Useful as a mode hook.  For example:
\(add-hook 'c-mode-hook 'notify-user-of-mode)"
  (setq buffer (buffer-name (and buffer (get-buffer buffer)))) ; Default curr.
  (when (and buffer
             (not (active-minibuffer-window))
             (or (and notifying-user-of-mode-flag ; Global var controls display.
                      (interesting-buffer-p buffer)) ; Not internal buffer.
                 anyway))               ; Override.
    (message "Buffer `%s' is in %s mode.   For info on the mode: `%s'."
             buffer mode-name
             (substitute-command-keys "\\[describe-mode]"))))


;;;$ FILES --------------------------------------------------------------------

;;;###autoload
(defun do-files (files fn &optional kill-buf-after)
  "Visit each file in list FILES, executing function FN once in each.
Optional arg KILL-BUF-AFTER non-nil means kill buffer after saving it."
  (let ((notifying-user-of-mode-flag nil)) ; No msg on mode.
    (dolist (file files)
      (set-buffer (find-file-noselect file))
      (funcall fn)
      (setq buffer-backed-up t)           ; Do not back it up.
      (save-buffer)                       ; Just save new version.
      (when kill-buf-after
        (kill-buffer (current-buffer))))))


;;;$ KEYS ---------------------------------------------------------------------

(defcustom buffer-modifying-cmds
  (append
   (and (or (not (boundp 'kill-read-only-ok)) kill-read-only-ok)
        '(backward-kill-paragraph backward-kill-sentence backward-kill-sexp
          backward-kill-word clipboard-kill-region comint-kill-input comment-kill 
          kill-backward-up-list kill-comment kill-line kill-paragraph
          kill-rectangle kill-region kill-region-wimpy kill-sentence kill-sexp
          kill-whole-line kill-word mouse-kill))
   '(align-newline-and-indent backward-delete-char backward-delete-char-untabify
     bookmark-insert bookmark-insert-location canonically-space-region
     capitalize-region capitalize-word c-backslash-region
     c-context-line-break center-line center-paragraph center-region
     c-fill-paragraph c-hungry-delete-backwards c-hungry-delete-forward
     c-indent-command c-indent-defun c-indent-exp clear-rectangle
     comint-truncate-buffer comment-dwim comment-indent-new-line comment-region
     comment-or-uncomment-region complete-symbol compose-last-chars
     compose-region dabbrev-completion dabbrev-expand decompose-region
     delete-backward-char delete-blank-lines delete-char
     delete-horizontal-space delete-indentation delete-matching-lines
     delete-non-matching-lines delete-pair delete-rectangle delete-region
     delete-trailing-whitespace delete-whitespace-rectangle delimit-columns-region
     downcase-region downcase-word edit-picture expand-abbrev expand-region-abbrevs
     fill-individual-paragraphs fill-nonuniform-paragraphs fill-paragraph
     fill-region fill-region-as-paragraph format-insert-file flush-lines
     ido-insert-buffer ido-insert-file increase-left-margin increase-right-margin
     indent-code-rigidly indent-for-comment indent-for-tab-command
     indent-line-function indent-new-comment-line indent-pp-sexp indent-region
     indent-rigidly insert-abbrevs insert-buffer insert-file insert-file-literally
     insert-kbd-macro insert-pair insert-parentheses insert-register
     insert-zippyism join-line justify-current-line just-one-space keep-lines
     lisp-complete-symbol lisp-fill-paragraph lisp-indent-line morse-region 
     newline newline-and-indent open-line open-rectangle query-replace
     query-replace-regexp quoted-insert reindent-then-newline-and-indent
     replace-regexp replace-string repunctuate-sentences reverse-region rot13-region
     self-insert-command set-justification-center set-justification-full
     set-justification-left set-justification-none set-justification-right
     set-left-margin set-right-margin skeleton-pair-insert-maybe smiley-region
     sort-columns sort-fields sort-lines sort-numeric-fields sort-pages
     sort-paragraphs split-line string-insert-rectangle string-rectangle
     studlify-region table-delete-column table-delete-row table-heighten-cell
     table-insert table-insert-column table-insert-row table-insert-sequence
     table-justify table-shorten-cell table-span-cell table-split-cell
     table-split-cell-horizontally table-split-cell-vertically table-widen-cell
     tab-to-tab-stop tabify texinfo-format-region tildify-region time-stamp
     todo-insert-item translate-region transpose-chars transpose-lines
     transpose-paragraphs transpose-sentences transpose-sexps transpose-words
     ucs-insert uncomment-region unmorse-region untabify upcase-region
     upcase-word vc-insert-headers whitespace-cleanup
     whitespace-cleanup-region yank yank-pop yank-rectangle zap-to-char))
  "*Buffer-modifying commands used in `undefine-killer-commands'."
  :type '(repeat symbol))

(defun undefine-keys-bound-to (command keymap)
  "Undefine all keys bound only by inheritance to COMMAND in KEYMAP.
If a key is bound to COMMAND in KEYMAP, but it is not bound directly
in KEYMAP, then bind it to `undefined' in KEYMAP."
  (dolist (key (where-is-internal command keymap))
    (when (and key (not (lookup-key keymap key)))
      (define-key keymap key 'undefined))))

(defun undefine-killer-commands (keymap)
  "Undefine KEYMAP keys that are bound to buffer-modifying commands.
For each key in KEYMAP that is indirectly bound to one of the commands in
`buffer-modifying-cmds', rebind it to `undefined'."
  (mapcar (lambda (cmd) (undefine-keys-bound-to cmd keymap)) buffer-modifying-cmds))

;;;; ;;;###autoload
;;;; (defun name+key (cmd)
;;;;   "Returns string naming command CMD (a symbol), with its current bindings."
;;;;   (let ((keys (mapconcat 'key-description
;;;;                          (where-is-internal cmd (current-local-map))
;;;;                          ", ")))
;;;;     (format "%s%s" cmd (if keys (concat " (" keys ")") ""))))

;;;; ;; Swap two keys.  Stolen from Emacs FAQ.
;;;; ;; When Emacs receives a character, you can make Emacs behave as though it
;;;; ;; received another character by setting the value of keyboard-translate-table.

;;;; ;; WARNING: the value of C-g (7) is still hard coded in one place in the
;;;; ;; minibuffer code.  Thus, swapping C-g with another key may cause a minor
;;;; ;; problem.  (Fixed in Emacs 18.58.)
;;;; ;;;###autoload
;;;; (defun swap-keys (key1 key2)
;;;;   "Swap keys KEY1 and KEY2 using function map-key."
;;;;   (map-key key1 key2)
;;;;   (map-key key2 key1))

;;;; (defun map-key (from to)
;;;;   "Make key FROM behave as though key TO was typed instead."
;;;;   (setq keyboard-translate-table
;;;;         (concat keyboard-translate-table
;;;;                 (let* ((i (length keyboard-translate-table))
;;;;                        (j from)
;;;;                        (k i)
;;;;                        (str (make-string (max 0 (- j (1- i))) ?X)))
;;;;                   (while (<= k j)
;;;;                     (aset str (- k i) k)
;;;;                     (setq k (1+ k)))
;;;;                   str)))
;;;;   (aset keyboard-translate-table from to)
;;;;   (let ((i (1- (length keyboard-translate-table))))
;;;;     (while (and (>= i 0) (eq (aref keyboard-translate-table i) i))
;;;;       (setq i (1- i)))
;;;;     (setq keyboard-translate-table
;;;;           (if (eq i -1)
;;;;               nil
;;;;             (substring keyboard-translate-table 0 (1+ i))))))


;;;$ MISCELLANEOUS ------------------------------------------------------------

(defun mod-signed (num base)
  "Return NUM modulo BASE, irrespective of the sign of NUM.
BASE is always non-negative.
Examples: (mod-signed  5 3) =>  2
          (mod-signed -5 3) => -2."
  (if (natnump num)
      (mod num base)
    (- (mod (- num) base))))

;; This is standard Lisp code stolen from tradition, not original with me.
(defun flatten (list)
  "Flatten LIST, returning a list with the atoms in LIST at any level.
Also works for a consp whose cdr is non-nil."
  (cond ((null list) nil)
        ((atom list) list)
        (t
         (let ((old list)
               (new ())
               item)
           (while old
             (if (atom old)             ; From consp with non-nil cdr.
                 (setq item old
                       old nil)
               (setq item (car old)
                     old (cdr old)))
             ;; Make item atomic.
             (while (consp item)
               (if (cdr item)
                   (setq old (cons (cdr item) old)))
               (setq item (car item)))
             (setq new (cons item new)))
           (reverse new)))))

;; From `cl-seq.el', function `union', without keyword treatment.
(defun simple-set-union (list1 list2)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or
LIST2.  This is a non-destructive function; it copies the data if
necessary."
  (cond ((null list1) list2)
        ((null list2) list1)
        ((equal list1 list2) list1)
        (t
         (or (>= (length list1) (length list2))
             (setq list1 (prog1 list2 (setq list2 list1)))) ; Swap them.
         (while list2
           (unless (member (car list2) list1)
               (setq list1 (cons (car list2) list1)))
           (setq list2 (cdr list2)))
         list1)))

;; From `cl-seq.el', function `intersection', without keyword treatment.
(defun simple-set-intersection (list1 list2)
  "Set intersection of lists LIST1 and LIST2.
This is a non-destructive operation: it copies the data if necessary."
  (and list1 list2
       (if (equal list1 list2)
           list1
         (let ((result nil))
           (unless (>= (length list1) (length list2))
             (setq list1 (prog1 list2 (setq list2 list1)))) ; Swap them.
           (while list2
             (when (member (car list2) list1)
               (setq result (cons (car list2) result)))
             (setq list2 (cdr list2)))
           result))))

;; From `cl-seq.el', function `set-difference', without keyword treatment.
(defun simple-set-difference (list1 list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-difference operation.
The result list contains all items that appear in LIST1 but not LIST2.
This is a non-destructive function; it copies the data if necessary."
  (if (or (null list1) (null list2))
      list1
    (let ((result nil))
      (while list1
        (unless (member (car list1) list2) (setq result (cons (car list1) result)))
        (setq list1 (cdr list1)))
      result)))

;; from `cl-extra.el'.
(defun signum (num)
  "Return 1 if NUM is positive, -1 if negative, 0 if zero."
  (cond ((< num 0) -1) ((> num 0) 1) (t 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc-fns.el ends here
