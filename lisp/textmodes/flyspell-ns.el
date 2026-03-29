;;; flyspell-ns.el --- Flyspell using Mac built-in  -*- lexical-binding:t -*-

;; Copyright (C) 2000-2022

;; Author: Nathaniel Cunningham & David Reitter
;; Maintainer: aquamacs-bugs@aquamacs.org
;; Keywords: convenience

;; This file is part of Aquamacs Emacs.

;; Aquamacs Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Aquamacs Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Aquamacs Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file provides functions for using the native Mac OS
;; spellchecker with flyspell inside Aquamacs.
;; Flyspell is a minor Emacs mode performing on-the-fly spelling
;; checking.
;;
;; To enable Flyspell minor mode, type M-x flyspell-mode.
;; This applies only to the current buffer.
;;
;; To enable Flyspell in text representing computer programs, type
;; M-x flyspell-prog-mode.
;; In that mode only text inside comments and strings is checked.
;;
;; Some user variables control the behavior of flyspell.  They are
;; those defined under the `User configuration' comment.

;;; Code:

(require 'ispell)
;; Note: we expect this file to be loaded by flyspell in
;; Aquamacs. This file certainly depends on it, but we can't require
;; it here because that would make a circular dependency.

(eval-when-compile (require 'cl-lib))
(require 'thingatpt) ;; use (word-at-point) in ns-spellchecking functions
(require 'osxkeys) ;; flyspell inherit regular Aquamacs context menu

;; Forward declaration: this variable is defined in flyspell.el.
;; Declaring it here ensures the byte compiler treats it as a
;; dynamic (special) variable, not a free lexical variable.
(defvar flyspell-ns-session-words)

(defcustom flyspell-mode-auto-on t
  "Non-nil means that executing `flyspell-region' or `flyspell-buffer'
will automatically turn on `flyspell-mode' for that buffer."
  :group 'flyspell
  :type 'boolean)

;; **********************************************************************
;; functions that use NSSpellChecker as the spellchecking
;; engine, instead of ispell or aspell

(defun ns-spellcheck-and-flyspell-word (beg end)
  "Use NSSpellChecker to locate misspelled words within range
BEG to END in current buffer.  Run flyspell-word on the misspelling,
and repeat search if word is not considered a misspelling by flyspell.
Returns buffer location of misspelled word if found, or nil.  As a side
effect, marks the misspelled word (if found) with face flyspell-incorrect."
  (let ((pos beg)
	misspell-location
	misspell-end
	done)
    ;; update dictionary if needed
    (unless (string= ispell-current-dictionary
		     (ns-spellchecker-current-language))
      (ispell-change-dictionary (ns-spellchecker-current-language)))
    ;; loop through until we find a misspelling or end of string
    (while (not done)
      (setq misspell-location
	    (ns-spellchecker-check-spelling (buffer-substring pos end)
					    (current-buffer)))
      (if misspell-location
	  (save-excursion
	    (setq misspell-end
		  (+ pos (car misspell-location) (cdr misspell-location)))
	    (let ((misspelled-word
		   (substring (buffer-substring pos end)
			      (car misspell-location)
			      (+ (car misspell-location)
				 (cdr misspell-location)))))
	      (if (member misspelled-word flyspell-ns-session-words)
		  ;; word accepted for session; skip it
		  (setq misspell-location nil)
		(goto-char misspell-end)
		(if (flyspell-word) ;; returns t if not misspelled
		    ;; ignore misspelling if flyspell-word says it's OK,
		    ;;  but continue checking
		    (setq misspell-location nil)
		  ;; if flyspell-word concurs, we've found a misspelling & are done
		  (setq done t)))))
	;; no misspellings in string; finish.
	(setq done t))
      (unless done
	;; check remainder of string
	(setq pos misspell-end)))
    ;; if a misspelling has been found, report is location (otherwise nil)
    (when misspell-location
      (cons (+ pos (car misspell-location)) misspell-end))
    ))

(defun ns-find-next-misspelling (&optional noskip)
  "Move forward in buffer to next misspelling; set region to the word,
and apply flyspell-incorrect face.   If NOSKIP is non-nil, don't skip
the current highlighted word (if any)."
  ;; search forward for a spelling error according to NSSpellChecker
  ;; do flyspell-word or equivalent to see if it is really misspelled
  ;; (e.g. not TeX or other filtered expression)
  ;; if it is, then also highlight it, and put it in the spelling panel
  (interactive)
  (unless (string= ispell-current-dictionary
		   (ns-spellchecker-current-language))
    (ispell-change-dictionary (ns-spellchecker-current-language)))
  (let* ((pos (if mark-active
		  ;; use beginning of region as start point for spellchecking,
		  ;; if there is an active region
		  (min (mark) (point))
		(point)))
	 (beg (point-min))
	 (end (point-max))
	 ;; (chunk-begin pos)
	 misspell-location
	 ;; chunk-end
	 ;; approx-end
	 )
    (save-excursion ;retain point & region if no misspelling found
      (goto-char pos)
      ;; this bit is needed to unhighlight the previous word, if it
      ;;   is ignored or learned in the spelling panel
      (let ((word (word-at-point)))
	(if (and word
		 (not (ns-spellchecker-check-spelling word (current-buffer))))
	    (flyspell-unhighlight-at (point))))
      ;; If midway through a word, start at search at next word;
      ;;   but don't skip an entire word
      (if (backward-word)
	  (forward-word))
      ;; When a selection is active, always skip the first word or
      ;;   partial word (as TextEdit does), so we don't spellcheck
      ;;   the same word again
      (if (and mark-active (not noskip)) (forward-word))
      (flyspell-word)
      (setq pos (point))
      ;; if region from point to end is larger than 1.5x
      ;; NS-SPELLCHECKER-CHUNK-SIZE chars, then check text in smaller chunks
      ;; (if (< (- end pos) (* 1.5 ns-spellchecker-chunk-size))
      ;; 	  (setq chunk-end end)
      ;; 	(setq chunk-end (+ pos ns-spellchecker-chunk-size))
      ;; 	;; try not to spellcheck across a sentence boundary, to keep
      ;; 	;;   grammar checking happy; make sure we at least don't split words.
      ;; 	(save-excursion
      ;; 	  (goto-char approx-end)
      ;; 	  (forward-sentence)
      ;; 	  (setq chunk-end (point))
      ;; 	  ;; If sentence boundary not found within 10% after
      ;; 	  ;; specified chunk size, look for first sentence
      ;; 	  ;; boundary up to 10% before desired chunk size.
      ;; 	  ;; If not found, use first word boundary after desired chunk size
      ;; 	  (if (> (abs (- chunk-end approx-end))
      ;; 		 (* 0.1 ns-spellchecker-chunk-size))
      ;; 	      (progn
      ;; 		;; find nearest previous sentence boundary
      ;; 		(backward-sentence)
      ;; 		(setq chunk-end (point))
      ;; 		;; check whether it's within 10% of specified value
      ;; 		(if (> (abs (- approx-end chunk-end))
      ;; 		       (* 0.1 ns-spellchecker-chunk-size))
      ;; 		    (progn
      ;; 		      ;; if not, use next word boundary
      ;; 		      (goto-char approx-end)
      ;; 		      (forward-word)
      ;; 		      (setq chunk-end (point))))))))
      ;; (while (and
      ;; 	      (eq (car misspelled-location) -1)
      ;; 	      (not overlap))
      ;; 	(setq misspell-location
      ;; 	      (ns-spellchecker-check-spelling
      ;; 	       (buffer-substring chunk-begin chunk-end)))
      ;; 	;; )
      ;; (setq misspell-location
      ;; 	    (ns-spellchecker-check-spelling (buffer-substring pos end)))
      ;; (if (= (car misspell-location) -1)
      ;; 	  nil
      ;; 	(setq misspell-beg
      ;; 	      (+ pos (car misspell-location))
      ;; 	      misspell-end
      ;; 	      (+ pos (car misspell-location) (cdr misspell-location)))
      ;; 	  (goto-char misspell-end)
      ;; 	  (setq misspell-found-p (not (flyspell-word))))
      ;; (unless misspell-found-p
      ;; 	(setq misspell-location
      ;; 	      (ns-spellchecker-check-spelling (buffer-substring beg pos)))
      ;; 	(if (= (car misspell-location) -1)
      ;; 	    nil
      ;; 	  (setq misspell-beg
      ;; 		(+ beg (car misspell-location))
      ;; 		misspell-end
      ;; 		(+ beg (car misspell-location) (cdr misspell-location)))
      ;; 	  (goto-char misspell-end)
      ;; 	  (setq misspell-found-p (not (flyspell-word)))))
      ;; (if misspell-found-p
      ;; return start and end of the misspelled word, or nil if none found
      ;; (cons misspell-beg misspell-end))
      (setq misspell-location
	    (ns-spellcheck-and-flyspell-word pos end))
      (if (not misspell-location)
	  (setq misspell-location (ns-spellcheck-and-flyspell-word beg pos)))
      ;; returns nil if we haven't found a misspelling
      misspell-location
      )))

(defun ns-highlight-misspelling-and-suggest (&optional noskip)
  "Search forward in current buffer for first misspelling, looping if end
is reached.  If found, set region to the misspelling, apply face
flyspell-incorrect, and show word in OS X spelling panel.  If
NOSKIP is non-nil, don't skip the current highlighted word (if any)."
  (interactive)
  (let* ((misspell-region (ns-find-next-misspelling noskip))
	 (misspell-beg (car misspell-region))
	 (misspell-end (cdr misspell-region))
	 word)
    (if (not misspell-region)
	;; no misspelling found; blank and beep the spelling panel
	(progn
	  (ns-spellchecker-show-word "")
	  (message "Spell checker: found no errors"))
      ;; misspelling found; set region to mispelled word, and show
      ;;   in spelling panel
      (goto-char misspell-end)
      (push-mark misspell-beg 'no-msg 'activate)
      (setq word (buffer-substring misspell-beg misspell-end))
      (ns-spellchecker-show-word word)))
  (if (and flyspell-mode-auto-on (not flyspell-mode))
      (turn-on-flyspell)))

(defun ns-start-spellchecker ()
  "Show NSSpellChecker spellingPanel, and call
ns-highlight-misspelling-and-suggest, which see."
  (interactive)
  (ns-popup-spellchecker-panel)
  (ns-highlight-misspelling-and-suggest))

(defun ns-toggle-spellchecker-panel ()
  "Show NSSpellChecker spellingPanel, and call
ns-highlight-misspelling-and-suggest.  If panel
is already visible, close it instead."
  (interactive)
  (if (ns-spellchecker-panel-visible-p)
      (ns-close-spellchecker-panel)
    (ns-popup-spellchecker-panel)
    ;; panel shouldn't skip past currently selected word, if there is one
    (ns-highlight-misspelling-and-suggest 'noskip)))

;;;###autoload
(defun spellcheck-now ()
  "Start spellchecking, using OS X spellchecker
(`ns-highlight-misspelling-and-suggest') or `ispell-buffer' (which see),
depending on value of `ispell-program-name'."
  (interactive)
  (if (string= ispell-program-name "NSSpellChecker")
      (ns-highlight-misspelling-and-suggest)
    (ispell-buffer)))

;;;###autoload
(defun spellchecker-panel-or-ispell ()
  "Calls `ns-toggle-spellchecker-panel' or `ispell' (which see), depending
on current value of `ispell-program-name'."
  (interactive)
  (if (string= ispell-program-name "NSSpellChecker")
      (ns-toggle-spellchecker-panel)
    (ispell)))

(defun ns-flyspell-region (beg end)
  "Flyspell text between BEG and END using ns-spellchecker-check-spelling."
  (interactive "r")
  (unless (string= ispell-current-dictionary
		   (ns-spellchecker-current-language))
    (ispell-change-dictionary (ns-spellchecker-current-language)))
  (save-excursion
    (let ((spellcheck-position beg)
	  (count 0)
	  ;; if called by ns-flyspell-large-region, then report progress
	  ;; relative to "large region" extents instead of simply extents of
	  ;; the current sub-region
	  (progress-beg
	   (if (boundp 'ns-flyspell-large-region-beg)
	       ns-flyspell-large-region-beg
	     beg))
	  (progress-end
	   (if (boundp 'ns-flyspell-large-region-end)
	       ns-flyspell-large-region-end
	     end))
	  spellcheck-text
	  ns-spellcheck-output
	  misspelled-location
	  misspelled-length)
      ;; remove any existing flyspell overlays before checking
      (flyspell-delete-region-overlays beg end)
      (while (< spellcheck-position end)
	;; report progress
        (if flyspell-issue-message-flag
	    (message "Spell Checking...%d%% [%s]"
		     (* 100 (/ (float (- spellcheck-position progress-beg))
			       (- progress-end progress-beg)))
		     (word-at-point)))
	;; extract text from last checked word to end
	(setq spellcheck-text (buffer-substring spellcheck-position end))
	;; find (position . length) of first misspelled word in extracted text
	(setq ns-spellcheck-output
	      ;; returns nil if no misspellings found
	      (ns-spellchecker-check-spelling spellcheck-text (current-buffer)))
	(if ns-spellcheck-output
	    ;; found misspelled word
	    (let ((misspelled-word
		   (substring spellcheck-text
			      (car ns-spellcheck-output)
			      (+ (car ns-spellcheck-output)
				 (cdr ns-spellcheck-output)))))
	      (setq misspelled-location
		    (+ (car ns-spellcheck-output) spellcheck-position))
	      (setq misspelled-length (cdr ns-spellcheck-output))
	      ;; start next check after current found word
	      (setq spellcheck-position
		    (+ misspelled-location misspelled-length))
	      ;; skip words accepted for the session
	      (unless (member misspelled-word flyspell-ns-session-words)
		;; use flyspell-word to filter and mark misspellings
		(goto-char spellcheck-position)
		(flyspell-word)))
	  ;; no misspellings found; we've reached the end of chunk
	  (setq spellcheck-position end))
	)
      (if (and (not (boundp 'ns-flyspell-large-region-end))
	       flyspell-issue-message-flag)
	  (message "Spell Checking completed."))
      ;; function returns end of this chunk
      end)))

(defun ns-flyspell-large-region (beg end)
  "Flyspell text between BEG and END using ns-spellchecker-check-spelling.
Break long text into chunks of approximate size NS-SPELLCHECKER-CHUNK-SIZE,
dividing at sentence boundaries where possible, or at word boundaries if
sentence boundaries are too far between."
  (interactive "r")
  (save-excursion
    (let ((inhibit-redisplay t)
	  (chunk-beg beg)
	  (ns-flyspell-large-region-beg beg)
	  (ns-flyspell-large-region-end end)
	  chunk-end
	  approx-end)
      (while
	  (<
	   (progn
	     ;; if length from chunk start to overall end is less
	     ;; than 1.5x chunk size, set chunk end to overall end
	     (if (< (- end chunk-beg) (* 1.5 ns-spellchecker-chunk-size))
		 (setq chunk-end end)
	       ;; otherwise, set end to sentence boundary near desired chunk size
	       (save-excursion
		 (setq approx-end (+ chunk-beg ns-spellchecker-chunk-size))
		 (goto-char approx-end)
		 (forward-sentence)
		 (setq chunk-end (point))
		 ;; If sentence boundary not found within 10% after
		 ;; specified chunk size, look for first sentence
		 ;; boundary up to 10% before desired chunk size.
		 ;; If not found, use first word boundary after desired chunk size
		 (if (> (- chunk-end approx-end)
			(* 0.1 ns-spellchecker-chunk-size))
		     (progn
		       ;; find nearest previous sentence boundary
		       (backward-sentence)
		       (setq chunk-end (point))
		       ;; check whether it's within 10% of specified value
		       (if (> (- approx-end chunk-end)
			      (* 0.1 ns-spellchecker-chunk-size))
			   (progn
			     ;; if not, use next word boundary
			     (goto-char approx-end)
			     (forward-word)
			     (setq chunk-end (point))))))))
	     ;; make sure we haven't extended the chunk beyond the region
	     (if (> chunk-end end)
		 (setq chunk-end end))
	     ;; check spelling of chunk, returning chunk end location
	     (ns-flyspell-region chunk-beg chunk-end))
	   end)
	;; if not done, start new chunk at previous chunk end
	(setq chunk-beg chunk-end))
      ;; when done, report completion
      (if flyspell-issue-message-flag (message "Spell Checking completed."))
      )))

;; ----------------------------------------------------------------------
;; Flyspell context menu inherits Aquamacs's generic context menu (as
;; a copy, so the inheritance can be disabled without affecting the
;; original)
(defvar aquamacs-context-menu-map-copy nil
  "Copy of Aquamacs general context menu keymap.  Used by
flyspell to incorporate general context menu items into menu for
misspelled words.")

;; **********************************************************************
;; global-flyspell-mode and automatic text-mode flyspelling

(defun maybe-turn-on-flyspell ()
  "Run `turn-on-flyspell' for current buffer, unless one of
`global-flyspell-inhibit-functions' returns t"
  (unless (run-hook-with-args-until-success
	   'global-flyspell-inhibit-functions)
    (turn-on-flyspell)))

;; turn on flyspell-mode for all buffers, with exception of new fundamental-mode
;; buffers and those returning t for any function listed in
;; `global-flyspell-inhibit-functions'
;;;###autoload
(define-globalized-minor-mode global-flyspell-mode flyspell-mode
  maybe-turn-on-flyspell)

(defcustom global-flyspell-inhibit-functions
  '(global-flyspell-default-inhibit-function)
  "List of functions to be called before `global-flyspell-mode' activates
flyspell in a buffer.  Those functions are called one by one, with no
arguments, until one of them returns a non-nil value, and thus, prevents
activation of `flyspell-mode' (but only via `global-flyspell-mode')."
  :group 'flyspell
  :type 'hook)

(defcustom global-flyspell-inhibit-buffer-names
  nil
  "List of buffer names in which `global-flyspell-mode' will not
  activate `flyspell-mode'"
  :group 'flyspell
  :type '(repeat (string)))

(defun global-flyspell-default-inhibit-function ()
  "Return non-nil if current buffer: has a major mode with a
property named mode-class with value special; has a name starting
with a space and is not visiting a file; is designated by its
name as a buffer to be displayed specially; or is listed by name
in `global-flyspell-inhibit-buffer-names'.  This is used to
indicate that the buffer should not have `flyspell-mode' turned on
by `global-flyspell-mode'."
  (or
   ;; buffers whose major-mode is marked as "special"
   (eq (get major-mode 'mode-class) 'special)
   ;; buffers whose name starts with space, if not visiting a file
   (and (char-equal ?\  (aref (buffer-name) 0))
	(not (buffer-file-name)))
   ;; buffers designated for special display (e.g. dedicated frame)
   (special-display-p (buffer-name))
   ;; buffer with name listed in global-flyspell-inhibit-buffer-names
   (member (buffer-name) global-flyspell-inhibit-buffer-names)
   ))

;;;###autoload
(defun flyspell-text-modes ()
  "Use Flyspell in Text mode and related modes.
Applies to all buffers that use modes related to Text mode,
both existing buffers and buffers that you subsequently create.
Turns off `flyspell-all-modes' if on."
  (interactive)
  (if global-flyspell-mode
      (global-flyspell-mode -1))
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (customize-mark-as-set 'text-mode-hook)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (let ((textual (or (derived-mode-p 'text-mode) text-mode-variant)))
	(if textual
	    (turn-on-flyspell)
	  (if flyspell-mode (turn-off-flyspell))))))
  (message "Flyspell activated in Text modes"))

;;;###autoload
(defun flyspell-all-modes ()
  "Use Flyspell in all major modes.
Applies both to existing buffers and buffers that you subsequently
create. Turns off `flyspell-text-modes' if on."
  (interactive)
  (let ((disable-for-text (memq 'turn-on-flyspell
				text-mode-hook)))
    (if disable-for-text
	(progn
	  (remove-hook 'text-mode-hook 'turn-on-flyspell)
	  (customize-mark-as-set 'text-mode-hook)))
    (global-flyspell-mode 1)
    (message "Flyspell activated in all modes")))

;;;###autoload
(defun flyspell-no-modes ()
  "Turn off `flyspell-text-modes' or `flyspell-all-modes' if on.
Also turns off flyspell-mode in all existing buffers."
  (interactive)
  (let ((disable-for-text (memq 'turn-on-flyspell
				text-mode-hook)))
    (if disable-for-text
	(progn
	  (remove-hook 'text-mode-hook 'turn-on-flyspell)
	  (customize-mark-as-set 'text-mode-hook)))
    ;; this turns off flyspell everywhere, whether or not the global mode was
    ;; previously on:
    (global-flyspell-mode -1)
    (message "Flyspell deactivated in all modes")))

(provide 'flyspell-ns)

;;; flyspell-ns.el ends here
