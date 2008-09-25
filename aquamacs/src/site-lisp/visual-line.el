;;; visual-line.el
;; Copyright (C) 2008 Free Software Foundation

;; Maintainer: David Reitter <david.reitter@gmail.com>
;; Authors: David Reitter 
;; Keywords: mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Overview:
;;
;; `visual-line-mode' and `global-visual-line-mode' enable
;; navigation by visual lines.  Vertical movement commands such as
;; `next-line' and `previous-line' (normally bound to up/down arrow
;; keys) will move the point to the next line as shown on the
;; screen, even if that is the same line in the underlying buffer.
;; The point is moved to a position that is located (on the screen)
;; horizontally close (pixel-wise), rather than to an equivalent
;; by-character column.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Notable changes:
;;
;; Initial version:
;; This file was adapted from Aquamacs Emacs.
;; Lennart Borgmann contributed the code that creates a minor mode
;; for this.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Code Comments:
 
;; Note that `visual-line-up' and friends use two different methods to
;; figure out the best position to move to because of a slowness with
;; outline-(minor-)mode. One of the methods (basically binary search) is
;; much faster when a lot of hidden text is present, but a bit slower in
;; all other cases.


(defun visual-col-at-point ()
  "Returns the visual column at point.
The visual column is relative to the left window edge, not
to the beginning of the (unwrapped) line."
  (- (point)
     (save-excursion
       (vertical-motion 0)
       (point))))
;; seems slower (in situations with very long lines)
;;(or (car (nth 6 (posn-at-point))) 0))

(defun visual-pixel-col-at-point ()
  "Returns the pixel column at point.
This is the distance from the left edge of the window 
to the character at point."
  (or (car-safe 
       (pos-visible-in-window-p (point) nil 'partial))
      0))

(defvar visual-movement-temporary-goal-column nil)
(make-variable-buffer-local 'visual-movement-temporary-goal-column)

(defvar visual-previous-scroll-margin 'none)
(defun visual-restore-scroll-margin ()
  "Restore the scroll margin."
  (if (integerp visual-previous-scroll-margin)
      (setq scroll-margin visual-previous-scroll-margin))
  (remove-hook 'pre-command-hook 'visual-restore-scroll-margin))

(defcustom visual-scroll-margin nil
 "Number of lines of margin at top and bottom of a window.
For visual scrolling with up and down keys, this value
applies instead of `scroll-margin' if it is non-nil.

The reason this variable exists is that clicks in the first and last
line of a window will set the cursor within the standard scroll-margin,
causing the buffer to scroll immediately. This is usually undesired.
In this case, set `scroll-margin' to zero and `visual-scroll-margin'
to the desired margin."
 :group 'Windows)


(defun visual-line-up (num-lines)
  (interactive "p")
  (if (bobp) (signal 'beginning-of-buffer nil))
  (let ((to-set-point)
	(old-point (point)))
    (let ((inhibit-point-motion-hooks t))
      (let ((visual-pixel-col (visual-pixel-col-at-point))
	    (end-of-old-line))


	;; temporary binding of scroll-margin
	;; cannot do this with a temporary let binding
	(setq visual-previous-scroll-margin scroll-margin)
	(if visual-scroll-margin
	    (setq scroll-margin visual-scroll-margin))
	(add-hook 'pre-command-hook 'visual-restore-scroll-margin)

	(let ((x-char) ;; jump to this char position (x). takes precedence
	      (x (car (posn-x-y (posn-at-point))))) ;; jump to this pixel pos
	  (save-excursion
	    (vertical-motion 1)	;; trying going one down, to left
	    (setq end-of-old-line (point)))

	  (vertical-motion 0)

	  (let* ((beg-of-old-line
		  ;; move right, but not further than to end of line
		  (prog1 (point)
		    (vertical-motion (- num-lines)))) ;; one up again
		 (beg-of-new-line (point))
		 (rel-beg-of-old-line  (- beg-of-old-line (point) 1)))

	    ;; handle track-eol...
	    (if (and track-eol (= old-point (1- end-of-old-line))
		     ;; Don't count beg of empty line as end of line
		     ;; unless we just did explicit end-of-line.
		     (or (not (= old-point beg-of-old-line))
			 (eq last-command 'end-of-line)))
		(setq visual-movement-temporary-goal-column 9999))

	    ;; approximate positioning
	    (if (and (or goal-column visual-movement-temporary-goal-column)
		     (memq last-command '(visual-line-up
					  visual-line-down))
		     ;;(= old-point (1- end-of-old-line))
		     )
		(if goal-column
		    (setq x-char goal-column)
		  (setq x visual-movement-temporary-goal-column))
	      ;; else, do complete positioning
	      ;; save original position
	      (setq visual-movement-temporary-goal-column visual-pixel-col))

	    (if x-char
		(forward-char (min x-char rel-beg-of-old-line))
	      (unless (pos-visible-in-window-p (point) nil 'partial)
		(redisplay t))
	      (let ((y (cdr (posn-x-y (posn-at-point)))))
		(and y (setq to-set-point (posn-at-x-y x y))))))
	  )))
    ;; point motion hooks aren't inhibited any longer
    (and to-set-point (posn-set-point to-set-point))
    (if (eq (point) old-point)
	;; got stuck, perhaps at the end of
	;; several visual lines of intangible text?
	(beginning-of-line))))

(defun visual-line-down (num-lines)
  (interactive "p")
  (if (and next-line-add-newlines (= num-lines 1))
      (if (save-excursion (end-of-line) (eobp))
	  ;; When adding a newline, don't expand an abbrev.
	  (let ((abbrev-mode nil))
	    (end-of-line)
	    (insert hard-newline)))
    (if (eobp) (signal 'end-of-buffer nil)))
  (let ((to-set-point))
    (let ((inhibit-point-motion-hooks t))
      (let ((old-point (point))
	    (visual-pixel-col (visual-pixel-col-at-point))
	    (beg-of-line)
	    (next-line-start)
	    (rel-next-line-start))


	;; temporary binding of scroll-margin
	;; cannot do this with a temporary let binding
	(setq visual-previous-scroll-margin scroll-margin)
	(if visual-scroll-margin
	    (setq scroll-margin visual-scroll-margin))
	(add-hook 'pre-command-hook 'visual-restore-scroll-margin)

	(let ((x-char) ;; jump to this char position (x). takes precedence
	      (x (car (posn-x-y (posn-at-point))))) ;; jump to this pixel pos
	  (vertical-motion num-lines) ;; down
	  (save-excursion
	    (setq beg-of-line (point))
	    (vertical-motion +1) ;; down
	    (setq next-line-start (point))
	    (setq rel-next-line-start  (- (point) beg-of-line 1)))
	    (unless (= beg-of-line (point-max))
	      ;; handle track-eol...
	      ;; handle track-eol...
	      (if (and track-eol (= old-point (1- next-line-start))
		       ;; Don't count beg of empty line as end of line
		       ;; unless we just did explicit end-of-line.
		       (or (not (= 0 visual-col))
			   (eq last-command 'end-of-line)))
		  (setq visual-movement-temporary-goal-column 9999))
	      
	      ;; approximate positioning
	      (if (and (or goal-column visual-movement-temporary-goal-column)
		       (memq last-command '(visual-line-up
					    visual-line-down))
		       ;(= old-point (- beg-of-line 1)) 
		       ;; do not re-set temp column but jump to the old one
		       ;; in case of repeated movement commands
		       )
		  (if goal-column
		      (setq x-char goal-column)
		    (setq x visual-movement-temporary-goal-column))
		;; else, do complete positioning
		;; save original position
		(setq visual-movement-temporary-goal-column visual-pixel-col))
	      
	      (if x-char
		  (forward-char (min x-char rel-next-line-start))
		(unless (pos-visible-in-window-p (point) nil 'partial)
		  (redisplay t))
		(let ((y (cdr (posn-x-y (posn-at-point)))))
		  (and y (setq to-set-point (posn-at-x-y x y))))))
	    )))
      ;; point motion hooks aren't inhibited any longer
      (and to-set-point (posn-set-point to-set-point))))


(defun beginning-of-visual-line ()
  "Move point to the beginning of the current line.
If `word-wrap' is nil, we move to the beginning of the buffer
line (as in `beginning-of-line'); otherwise, point is moved to
the beginning of the visual line."
  (interactive)
  (if (bobp)
      (signal 'beginning-of-buffer nil))
  (if word-wrap
      (vertical-motion 0)
    (beginning-of-line)))

(defun end-of-visual-line ()
  "Move point to the end of the current line.
If `word-wrap' is nil, we move to the end of the line (as in
`beginning-of-line'); otherwise, point is moved to the end of the
visual line."
  (interactive)
  (if (eobp)
      (signal 'end-of-buffer nil))
  (if word-wrap
      (progn
	(vertical-motion 1)
	(skip-chars-backward "\r\n" (- (point) 1)))
    (end-of-line)))

;; this code based on simple.el
(defun kill-visual-line (&optional arg)
  "Kill the rest of the visual line; if no nonblanks there, kill thru
newline.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.
With zero argument, kills the text before point on hthe current line.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

To kill a whole line, when point is not at the beginning, type \
\\[beginning-of-line] \\[kill-line] \\[kill-line].

If `kill-whole-line' is non-nil, then this command kills the whole line
including its terminating newline, when used at the beginning of a line
with no argument.  As a consequence, you can always kill a whole line
by typing \\[beginning-of-line] \\[kill-line].

If you want to append the killed line to the last killed text,
use \\[append-next-kill] before \\[kill-line].

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)

``Line'' is defined as visual line, from the leftmost to the
rightmost position of a single visual line, if `word-wrap' is
non-nil.  Otherwise, this function behaves exactly like
`kill-line'."
  (interactive "P")
  (if word-wrap
      (kill-region 
       (point)
       ;; It is better to move point to the other end of the
       ;; kill before killing.  That way, in a read-only
       ;; buffer, point moves across the text that is copied
       ;; to the kill ring.  The choice has no ef
       ;; now that undo records the value of point from before
       ;; the command was run.
       (progn
	 (if arg
	     (vertical-motion (prefix-numeric-value arg))
	   (if (eobp)
	       (signal 'end-of-buffer nil))
	   (let ((end
		  (save-excursion
		    (vertical-motion 1) 
					; we're possibly one too far
		    (skip-chars-backward "\r\n" (- (point) 1))
		    (point))))
	     (if (or (save-excursion
		       ;; If trailing whitespace is visible,
		       ;; don't treat it as nothing.
		       (unless show-trailing-whitespace
			 (skip-chars-forward " \t" end))
		       (= (point) end))
		     (and kill-whole-line (bolp)))
		 (vertical-motion 1)
	       (goto-char end))))
	 (point)))
    (kill-line arg)))

;; to do: we should really delete everything
;; that is not read-only, rather than just
;; exclude a prompt

(defun skip-read-only-prompt (&optional max)
  (while (and (get-char-property (point) 'read-only)
	      (< (point) (or max (point-max)))
    (forward-char))))

(defun kill-whole-visual-line (&optional arg)
  "Kill current visual line.
With prefix arg, kill that many lines starting from the current line.
If arg is negative, kill backward.  Also kill the preceding newline.
\(This is meant to make \\[repeat] work well with negative arguments.\)
If arg is zero, kill current line but exclude the trailing newline.

``Line'' is defined as visual line, from the leftmost to the
rightmost position of a single visual line, if `word-wrap' is
non-nil.  Otherwise, this function behaves exactly like
`kill-line'."
  (interactive "p")

  (if (not word-wrap)
      (let ((kill-whole-line t))
	(kill-line arg))
    (if (and (> arg 0) (eobp) (save-excursion (vertical-motion 0) (eobp)))
	(signal 'end-of-buffer nil))
    (if (and (< arg 0) (bobp) (save-excursion (vertical-motion 1) (bobp)))
	(signal 'beginning-of-buffer nil))
    (unless (eq last-command 'kill-region)
      (kill-new "")
      (setq last-command 'kill-region))
    (cond ((zerop arg)
	   ;; We need to kill in two steps, because the previous command
	   ;; could have been a kill command, in which case the text
	   ;; before point needs to be prepended to the current kill
	   ;; ring entry and the text after point appended.  Also, we
	   ;; neehd to use save-excursion to avoid copying the same text
	   ;; twice to the kill ring in read-only buffers.
	   (save-excursion
	     ;; delete in one go
	     (kill-region (progn (vertical-motion 0) 
				 (skip-read-only-prompt) (point))
			  (progn (vertical-motion 1) (point)))
	     ))
	  ((< arg 0)
	   (save-excursion
	     (kill-region (point) (progn (end-of-visual-line) (point))))
	   (kill-region (point)
			(progn
			  (vertical-motion (1+ arg))
			  (unless (bobp) (backward-char))
			  (point))))
	  (t
	   (save-excursion
	     (kill-region (let ((ep (point)))
			    (vertical-motion 0) 
			    (skip-read-only-prompt ep)
			    (point))
			  (progn
			    (vertical-motion arg) 
			    (point))))))))

;; mark functions for CUA
(dolist (cmd
	 '( beginning-of-visual-line
	    end-of-visual-line
	    visual-line-down visual-line-up))
 (put cmd 'CUA 'move))

(defalias 'original-kill-line 'kill-line)
(defalias 'original-next-line 'next-line)
(defalias 'original-previous-line 'previous-line)
(defalias 'original-move-beginning-of-line 'move-beginning-of-line)
(defalias 'original-move-end-of-line 'move-end-of-line)

(defun line-wrapped-p ()
  "Return non-nil if the current line is wrapped."
  (let ((here (point))
        result)
    (vertical-motion 0)
    (setq result (/= (line-beginning-position) (point)))
    (unless result
      (let ((line-end-pos (line-end-position)))
        (vertical-motion 1)
        (setq result (/= line-end-pos (- (point) 1)))))
    (goto-char here)
    result))

(defvar visual-line-map
 (let ((map (make-sparse-keymap)))
   (define-key map [remap next-line] 'visual-line-down)
   (define-key map [remap previous-line] 'visual-line-up)
   (define-key map [remap kill-line] 'kill-visual-line)
   (define-key map [(control shift ?k)] 'original-kill-line)
   (define-key map [remap move-beginning-of-line] 'beginning-of-visual-line)
   (define-key map [remap move-end-of-line]  'end-of-visual-line)
   map))

(define-minor-mode visual-line-mode
 "Define key binding for visual line moves."
 :keymap visual-line-map
 :group 'convenience)

(defun turn-on-visual-line-mode ()
 (visual-line-mode 1))

(define-globalized-minor-mode global-visual-line-mode
 visual-line-mode turn-on-visual-line-mode)

(defface blank-newline
  '((((class color) (background dark))
     (:foreground "lightgrey" :bold nil))
    (((class color) (background light))
     ( :foreground "lightgrey" :bold nil))
    (t (:bold nil :underline t)))
  "Face used to visualize NEWLINE char mapping.

See `blank-display-mappings'."
  :group 'blank)


 ;; 2230 = \x8B6
(defvar show-newlines-newline-code (vector (make-glyph-code 2230 'blank-newline) 10))
(define-minor-mode show-newlines-mode
  "Mark newlines in current buffer"
 :group 'convenience

 (unless buffer-display-table
   (setq buffer-display-table (or standard-display-table (make-display-table))))
 (if show-newlines-mode
     (aset buffer-display-table 10 show-newlines-newline-code)
   (aset buffer-display-table 10 nil)))

(define-minor-mode global-show-newlines-mode
  "Mark newlines in all buffers"
  :group 'convenience
  :global t

 (unless standard-display-table
   (setq standard-display-table (make-display-table)))
 (if global-show-newlines-mode
     (aset standard-display-table 10 show-newlines-newline-code)
   (aset standard-display-table 10 nil))
 (dolist (buffer (buffer-list))
   (with-current-buffer buffer
     (if buffer-display-table
	 (show-newlines-mode (if global-show-newlines-mode 1 -1))))))
       
;;(setq  show-newlines-newline-code (vector (make-glyph-code 2230 'blank-newline) 10))
;;(setf (aref show-newlines-newline-code 0) (make-glyph-code 34 'blank-newline))

(define-key-after menu-bar-showhide-menu [show-newlines-mode]
  (menu-bar-make-mm-toggle global-show-newlines-mode
			   "Show Newlines"
			   "Show hard newlines") 'highlight-paren-mode)


(provide 'visual-line)
