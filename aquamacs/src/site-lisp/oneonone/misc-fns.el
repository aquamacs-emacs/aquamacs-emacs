;;; misc-fns.el --- Miscellaneous non-interactive functions.
;;
;; Filename: misc-fns.el
;; Description: Miscellaneous non-interactive functions.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2016, Drew Adams, all rights reserved.
;; Created: Tue Mar  5 17:21:28 1996
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Oct 16 15:19:51 2016 (-0700)
;;           By: dradams
;;     Update #: 666
;; URL: http://www.emacswiki.org/misc-fns.el
;; Keywords: internal, unix, lisp, extensions, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
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
;;  You might want to put this in your `~/.emacs' file, to erase the
;;  minibuffer when it is inactive and `minibuffer-empty-p':
;;
;;   (require 'misc-fns)
;;   (add-hook '<MODE>-hook 'notify-user-of-mode), for each <MODE>.
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
;;    `all-apply-p', `another-buffer', `color-named-at',
;;    `current-line', `display-in-mode-line', `do-files', `flatten',
;;    `fontify-buffer', `interesting-buffer-p', `live-buffer-name',
;;    `make-transient-mark-mode-buffer-local', `mode-ancestors',
;;    `mode-symbol-p', `mod-signed', `notify-user-of-mode',
;;    `read-mode-name', `region-or-buffer-limits', `signum',
;;    `some-apply-p' `string-after-p', `string-before-p',
;;    `undefine-keys-bound-to', `undefine-killer-commands',
;;    `unique-name'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2016/10/16 dadams
;;     Added: all-apply-p, some-apply-p.
;; 2016/05/19 dadams
;;     Added: mode-symbol-p, read-mode-name.
;; 2015/12/31 dadams
;;     Renamed: chars-(after|before) to string-(after|before)-p.
;;     chars-(after|before): Use version from Martin Rudalics in Emacs bug #17284.
;; 2015/10/02 dadams
;;     chars-before: Use version from Tassilo Horn (help-gnu-emacs@gnu.org).
;; 2015/04/03 dadams
;;     Added: chars-after, chars-before.
;; 2014/10/14 dadams
;;     Added :group for defcustom and defface.
;; 2013/09/30 dadams
;;     Removed force-time-redisplay.
;; 2012/11/10 dadams
;;     Added: color-named-at.
;; 2012/06/18 dadams
;;     notify-user-of-mode: Use format-mode-line if available.
;; 2012/04/21 dadams
;;     Added mode-ancestors.
;; 2012/02/29 dadams
;;     Removed: simple-set-(intersection|union|difference).
;; 2011/01/04 dadams
;;     Removed autoload cookies from non-interactive fns.  Added for defcustom.
;; 2010/05/25 dadams
;;     Added: unique-name.
;; 2010/01/12 dadams
;;     fontify-buffer: save-excursion + set-buffer -> with-current-buffer.
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

(eval-when-compile (when (< emacs-major-version 21) (require 'cl))) ;; dolist

;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-fns)
(require 'misc-fns)                 ; Ensure loaded before compile.

;;;;;;;;;;;;;;;;;;;;;


;;;$ MODE-LINE ----------------------------------------------------------------

;;;###autoload
(defcustom mode-line-reminder-duration 10
  "*Maximum number of seconds to display a reminder in the mode-line."
  :group 'help :type 'integer)

(defun display-in-mode-line (text)
  "Display TEXT in mode line for `mode-line-reminder-duration' seconds."
  (let ((mode-line-format (list text)))
    (force-mode-line-update)
    (sit-for mode-line-reminder-duration))
  (force-mode-line-update))


;;;$ BUFFERS ------------------------------------------------------------------

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

(defun interesting-buffer-p (buffer)
  "Non-nil if BUFFER is a live buffer whose name does not start with SPC."
  (and buffer (setq buffer (live-buffer-name buffer)) ; Live buffer's name.
       (or (zerop (length buffer))      ; Not an empty name.
           (not (char-equal ?\  (aref buffer 0)))))) ; Starts with non-blank.

(defun unique-name (name existing-names &optional min use-base-p maxp)
  "Return NAME or NAME<N>, a name that is not in EXISTING-NAMES.
Return NAME if NAME is not a member of EXISTING-NAMES.
Otherwise, return NAME or its base name, suffixed by `<N>', where N is
an integer.

The optional args are used only when NAME is in EXISTING-NAMES.

MIN is the minimum integer N to use in the new suffix.  Default: 1.

Non-nil USE-BASE-P means use only the base name of NAME.  The value
returned is of the form `BASENAME<N>' (only a single suffix).
BASENAME is NAME truncated at the right starting with the first suffix
`<M>'.  The base name of `a<2>' and `a<2><3>' is `a'.

For example, if NAME is `a<2>', then with nil USE-BASE-P we might
return `a<2><1>' (depending on MIN, MAX etc.).  With non-nil
USE-BASE-P we might return `a<3>', since the base name `a' gets
suffixed, not the full NAME `a<2>'.

Optional arg MAXP is used only if USE-BASE-P is non-nil.

If MAXP is nil then N is the smallest integer greater than or equal to
MIN such that `BASENAME<N>' is not in EXISTING-NAMES.

If MAXP is non-nil then N is the smallest integer greater than or
equal to MIN and greater than the largest integer M used in a suffix
`<M>' that immediately follows BASENAME in a name in EXISTING-NAMES.

As an example, `generate-new-buffer-name' could be defined this way:

 (defun generate-new-buffer-name (buf)
   (let ((buffs  (mapcar #'buffer-name (buffer-list))))
     (unique-name buf buffs 2)))"
  (unless min  (setq min  1))
  (if (and (not (member name existing-names)) (not maxp))
      name
    (let ((indx     min)
          (baselen  (string-match "\<\\(-?[0-9]+\\)\>" name))
          try)
      (when (and use-base-p baselen)
        (setq name  (substring name 0 baselen)))
      (if maxp
          (format
           "%s<%d>" name
           (1+ (apply
                #'max
                (mapcar (lambda (nn)
                          (if (string-match "\<\\(-?[0-9]+\\)\>" nn)
                              (string-to-number (match-string 1 nn))
                            min))
                        existing-names))))
        (catch 'unique-name
          (while t
            (unless (member (setq try  (concat name "<" indx ">"))
                            existing-names)
              (throw 'unique-name try))
            (setq indx  (max min (1+ indx)))))))))

;; Stolen from file `intes.el.2'
(defun current-line ()
  "Current line number of cursor."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)))

(defun fontify-buffer (buffer &rest ignore)
  "Fontify buffer BUFFER.
Usable as a candidate for `compilation-finish-functions'.
Any arguments besides BUFFER (IGNORE list) are ignored."
  (with-current-buffer buffer (font-lock-fontify-buffer)))



;;;$ REGION -------------------------------------------------------------------

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
If the region is not active or is empty, then bob and eob are used."
  (if (or (not mark-active) (null (mark)) (= (point) (mark)))
      (list (point-min) (point-max))
    (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))



;;;$ MODES ---------------------------------------------------------------------

(defcustom notifying-user-of-mode-flag t
  "*Non-nil means to display messages notifying user of mode changes.
See function `notify-user-of-mode'."
  :group 'help :type 'boolean)

(defface notify-user-of-mode '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
  "*Face used for notifying user of current major mode.
See function `notify-user-of-mode'."
  :group 'help)

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
    (message "Buffer `%s' is in mode `%s'.   For info on the mode: `%s'."
             buffer (if (fboundp 'format-mode-line)
                        (format-mode-line mode-name)
                      mode-name)
             (substitute-command-keys "\\[describe-mode]"))))

(defun mode-ancestors (mode)
  "Return the ancestor modes, a list of symbols, for symbol MODE.
Uses symbol property `derived-mode-parent' to trace backwards."
  (let ((parent  (get mode 'derived-mode-parent))
        (modes   ()))
    (while parent
      (push parent modes)
      (setq parent  (get parent 'derived-mode-parent)))
    modes))

(defun mode-symbol-p (symbol)
  "Return non-nil if SYMBOL is a major-mode or minor-mode symbol.
Note: This might falsely return nil in some exceptional cases."
  (or (get symbol 'derived-mode-parent) ; Most modes.
      (get symbol 'custom-mode-group) ; Some modes, such as `ada-mode'.
      ;; Use `FOO-mode' as candidate if `FOO' has a custom group.
      (and (string-match "-mode\\'" (symbol-name symbol))
           (get (setq symbol  (intern (substring (symbol-name symbol) 0
                                                 (match-beginning 0))))
                'custom-group))
      (eq 'fundamental-mode symbol)))

(defun read-mode-name (&optional prompt predicate require-match initial-input
                         history def inherit-input-method keymap)
  "Read the name of a major or minor mode symbol, with completion.
Optional args are as for `completing-read', but without COLLECTION.
Optional arg PREDICATE is applied only to mode symbols."
  (let ((emacs-23+  (fboundp 'completion-table-with-predicate)) ; Emacs 23+
        (pred       (if (not predicate)
                        'mode-symbol-p
                      `(lambda (symb) (and (funcall 'mode-symbol-p symb)
                                      (funcall ',predicate symb))))))
    (completing-read (or prompt  "Mode: ")
                     (if emacs-23+
                         (apply-partially #'completion-table-with-predicate
                                          obarray pred t)
                       obarray)
                     (and (not emacs-23+)  pred)
                     require-match
                     (or initial-input  (symbol-name major-mode))
                     history
                     def
                     inherit-input-method
                     keymap)))


;;;$ FILES --------------------------------------------------------------------

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
  :group 'editing :type '(repeat symbol))

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

;; Same as `tap-color-at-point' in `thingatpt+.el', except that this accepts an arg.
(when (fboundp 'color-defined-p)
  (defun color-named-at (&optional position)
    "Return the color named at POSITION (default: point), as a string.
The name is anything recognized by `color-defined-p', which includes
an RGB color code prefixed by `#'.
Return nil if no color is named at point."
    (unless position (setq position  (point)))
    (let ((word  (with-syntax-table (copy-syntax-table (syntax-table))
                   (modify-syntax-entry ?# "w") ; Make `#' a word constituent.
                   (word-at-point))))
      (and word  (color-defined-p word)  word))))

;;; (defun chars-after (chars)
;;;   "Return non-nil if the literal string CHARS is right after point."
;;;   (let* ((len  (length chars))
;;;          (idx  (1- len))
;;;          (pt   (point)))
;;;     (catch 'chars-after
;;;       (dolist (char  (nreverse (append chars ())))
;;;         (unless (condition-case nil
;;;                     (eq char (char-after (+ pt idx)))
;;;                   (error nil))          ; e.g. `eobp'
;;;           (throw 'chars-after nil))
;;;         (setq idx  (1- idx)))
;;;       t)))

;; Version similar to `chars-before' by Martin Rudalics in bug #17284.
;; And renamed from `chars-after'.
;;
(defun string-after-p (chars)
  "Return non-nil if the literal string CHARS is right after point."
  (let ((end  (+ (point) (length chars))))
    (and (<= end (point-max))
         (string= chars (buffer-substring-no-properties (point) end)))))

;;; (defun chars-before (chars)
;;;   "Return non-nil if the literal string CHARS is right before point.
;;; This is more efficient that `looking-back' for this use case."
;;;   (let* ((len  (length chars))
;;;          (idx  (1- len))
;;;          (pt   (point)))
;;;     (catch 'chars-before
;;;       (dolist (char  (append chars ()))
;;;         (unless (condition-case nil
;;;                     (eq char (char-before (- pt idx)))
;;;                   (error nil))          ; e.g. `bobp'
;;;           (throw 'chars-before nil))
;;;         (setq idx  (1- idx)))
;;;       t)))

;; Version from Tassilo Horn [tsdh@gnu.org], in help-gnu-emacs@gnu.org,
;; 2015-10-02, Subject "`looking-back' strange warning".
;;
;;; (defun chars-before (chars)
;;;   "Return non-nil if the literal string CHARS is right before point.
;;; This is more efficient that `looking-back' for this use case."
;;;   (let ((beg  (- (point) (length chars))))
;;;     (unless (< beg 0)
;;;       (save-excursion
;;; 	(goto-char beg)
;;; 	(looking-at (regexp-quote chars))))))

;; Version from Martin Rudalics in thread of Emacs bug #17284.
;; And renamed from `chars-before'.
;;
(defun string-before-p (chars)
  "Return non-nil if the literal string CHARS is right before point.
This is more efficient that `looking-back' for this use case."
  (let ((start  (- (point) (length chars))))
    (and (>= start (point-min))
         (string= chars (buffer-substring-no-properties start (point))))))

(defun all-apply-p (predicates &optional arguments)
  "Invoke PREDICATES in order, passing ARGUMENTS, until one returns nil.
If none returns nil then return the value returned by the last one.

PREDICATES can also be a single predicate, in which case it acts the
same as a singleton list of that predicate.

This is like `run-hook-with-args-until-failure', except that that
function accepts a hook variable whose value is PREDICATES as its
first argument."
  (when (functionp predicates) (setq predicates  (list predicates)))
  (let ((ret  t))
    (catch 'all-apply-p
      (dolist (pred  predicates)
        (setq ret  (apply pred arguments))
        (unless ret (throw 'all-apply-p nil)))
      ret)))

(defun some-apply-p (predicates &optional arguments)
  "Invoke PREDICATES in order, passing ARGUMENTS, until one returns non-nil.
If none returns non-nil then return the value returned by last one.

PREDICATES can also be a single predicate, in which case it acts the
same as a singleton list of that predicate.

This is like `run-hook-with-args-until-success', except that that
function accepts a hook variable whose value is PREDICATES as its
first argument."
  (when (functionp predicates) (setq predicates  (list predicates)))
  (let ((ret  nil))
    (catch 'some-apply-p
      (dolist (pred  predicates)
        (setq ret  (apply pred arguments))
        (when ret (throw 'some-apply-p ret)))
      ret)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc-fns.el ends here
