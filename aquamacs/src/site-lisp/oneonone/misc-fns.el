;;; misc-fns.el --- Miscellaneous non-interactive functions.
;;
;; Filename: misc-fns.el
;; Description: Miscellaneous non-interactive functions.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2005, Drew Adams, all rights reserved.
;; Created: Tue Mar  5 17:21:28 1996
;; Version: 21.0
;; Last-Updated: Mon Jul 04 09:22:19 2005
;;           By: dradams
;;     Update #: 204
;; Keywords: internal, unix, lisp, extensions, local
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
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
;;  Main new functions defined here:
;;
;;    `another-buffer', `current-line', `display-in-mode-line',
;;    `do-files', `force-time-redisplay', `interesting-buffer-p',
;;    `live-buffer-name', `make-transient-mark-mode-buffer-local',
;;    `mod-signed', `notify-user-of-mode', `undefine-keys-bound-to',
;;    `undefine-killer-commands'.
;;
;;  Main new user options (variables) defined here:
;;
;;    `buffer-modifying-cmds', `mode-line-reminder-duration',
;;    `notify-user-of-mode-face', `notifying-user-of-mode'.
;;
;;  Library `misc-fns' requires these libraries:
;;
;;    `misc-fns'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(and (< emacs-major-version 21)         ;; dolist, push, pop
     (eval-when-compile (require 'cl))) ;; (plus, for Emacs <20: when, unless)

;; Get macro `define-face-const' when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile (require 'def-face-const))

 ;; (autoload 'display-in-minibuffer "strings")

;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-fns)
(require 'misc-fns)                 ; Ensure loaded before compile.

;;;;;;;;;;;;;;;;;;;;;


;;;$ MODE-LINE ----------------------------------------------------------------

;; From `show-bind.el'.
(defvar mode-line-reminder-duration 10
  "*Maximum number of seconds to display a reminder in the mode-line.")
(put 'mode-line-reminder-duration 'variable-interactive
     "nMax seconds to display key reminders in mode-line: ")

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
This is probably obsolete now.  Use `force-mode-line-update."
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



;;;$ MODE ---------------------------------------------------------------------

(defvar notifying-user-of-mode t
  "*Non-nil <=> Displaying messages notifying user of mode changes.
See function `notify-user-of-mode'.")

(unless (boundp 'blue-foreground-face) (define-face-const "Blue" nil))
(defvar notify-user-of-mode-face blue-foreground-face
  "*Face used for notifying user of current major mode.
See function `notify-user-of-mode'.")

;;;###autoload
(defun notify-user-of-mode (&optional buffer anyway)
  "Display msg naming major mode of BUFFER (default: current buffer).
No msg is displayed if not `notifying-user-of-mode' or BUFFER is
internal, unless optional 2nd arg ANYWAY is non-nil.
In that case, msg is displayed anyway.
Useful as a mode hook.  For example:
\(add-hook 'c-mode-hook 'notify-user-of-mode)"
  (setq buffer (buffer-name (and buffer (get-buffer buffer)))) ; Default curr.
  (when (and buffer
             (or (and notifying-user-of-mode ; Global var controls display.
                      (interesting-buffer-p buffer)) ; Not internal buffer.
                 anyway))               ; Override.
    (if (fboundp 'display-in-minibuffer)
	(display-in-minibuffer
         'new "Buffer `" (list notify-user-of-mode-face buffer) "' is in "
         (list notify-user-of-mode-face mode-name)
         " mode.   For info on the mode: `"
         (list notify-user-of-mode-face
               (substitute-command-keys "\\[describe-mode]")) "'.")
      (message
       "Buffer `%s' is in %s mode.   For info on the mode: `%s'."
       buffer mode-name
       (substitute-command-keys "\\[describe-mode]")))))


;;;$ FILES --------------------------------------------------------------------

;;;###autoload
(defun do-files (files fn &optional kill-buf-after)
  "Visit each file in list FILES, executing function FN once in each.
Optional arg KILL-BUF-AFTER non-nil means kill buffer after saving it."
  (let ((notifying-user-of-mode nil))    ; No msg on mode.
    (dolist (file files)
      (set-buffer (find-file-noselect file))
      (funcall fn)
      (setq buffer-backed-up t)           ; Do not back it up.
      (save-buffer)                       ; Just save new version.
      (when kill-buf-after
        (kill-buffer (current-buffer))))))


;;;$ KEYS ---------------------------------------------------------------------

(defsubst undefine-keys-bound-to (command keymap &optional oldmap)
  "Bind to `undefined' all keys currently bound to COMMAND in KEYMAP.
If optional argument OLDMAP is specified, rebinds in KEYMAP as
`undefined' all keys that are currently bound to COMMAND in OLDMAP."
  (substitute-key-definition command 'undefined keymap oldmap))

(defvar buffer-modifying-cmds
  '(delete-char quoted-insert transpose-chars kill-region yank kill-word
                indent-new-comment-line kill-sentence fill-paragraph
                transpose-words yank-pop zap-to-char just-one-space
                indent-for-comment delete-indentation kill-sexp split-line
                transpose-sexps backward-kill-sentence)
  "*Buffer-modifying commands used in `undefine-killer-commands'.")

(defsubst undefine-killer-commands (keymap &optional oldmap)
  "Bind `undefined' to KEYMAP keys bound to buffer-modifying commands.
If optional arg OLDMAP is specified, rebinds in KEYMAP as `undefined'
the keys that are currently bound to buffer-modifying commands in
OLDMAP.  The buffer-modifying commands used: `buffer-modifying-cmds'."
  (mapcar (function (lambda (cmd) (undefine-keys-bound-to cmd keymap oldmap)))
          buffer-modifying-cmds))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc-fns.el ends here
