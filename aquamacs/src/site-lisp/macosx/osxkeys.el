;; osxkeys.el
;; Mac Style Keyboard Shortcuts 
;; provides osx-key-mode


;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: osxkeys.el,v 1.79 2007/12/15 19:29:34 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/

;; Attribution: Leave this header intact in case you redistribute this file.
;; Attribution must be given in application About dialog or similar,
;; "Contains Aquamacs osx-key-mode by D Reitter" does the job.
;; Apart from that, released under the GPL:
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

 
;; Copyright (C) 2005, 2006 David Reitter


;; Unit test  / check requirements
(require 'aquamacs-tools)
(aquamacs-require  '(boundp 'mac-command-modifier))
(aquamacs-require  '(boundp 'mac-control-modifier))
(aquamacs-require  '(boundp 'mac-option-modifier))


;; To do: this should only happen when the mode is switched on

(aquamacs-set-defaults '(
			 (mac-option-modifier meta) 
;;(setq mac-control-modifier nil) ;; use default
			 (mac-command-modifier alt)
			 (mac-pass-command-to-system t) 
;; let system handle Apple-H and the like
;; (this is default anyways)
))

;; mac-command-is-meta won't work any more at this point!
;; it's deprecated

;;; MacOS X specific stuff

(defvar osxkeys-command-key mac-command-modifier
"Command key used for `osx-key-mode'.
Defaults to the value of `mac-command-modifier'.
You will need to run

(setq  osx-key-mode-map (make-osx-key-mode-map))

after updating this variable.")

;; Define the return key to avoid problems on MacOS X
(define-key function-key-map [return] [13])

;; Option (alt) will act like in other Mac programs
;; if it is used without a standard modifier, it is interpreted as 'Meta'
;; use Esc instead of Option (alt) if you need Meta for a reserved combination.
  
;; allow selection of secondary buffer
   
 
(require 'aquamacs-tools)
 
 

(require 'filladapt)

(require 'mac-extra-functions)
 
(require 'aquamacs-redo)
  
;; remove existing bindings that don't exist on the mac
(global-unset-key [cut])
(global-unset-key [copy])
(global-unset-key [paste])
(global-unset-key [f20])
(global-unset-key [f16])
(global-unset-key [f18])
  

(defvar cua--explicit-region-start) ;; in case CUA isn't loaded

(defun aquamacs-backward-char ()
  "Move point to the left or the beginning of the region.
 Like `backward-char', but moves point to the beginning of the region
provided `cua-mode' and the mark are active."
  (interactive)
  (let ((left (min (point) (or (mark t) 0))))

    (if (and cua-mode transient-mark-mode 
	     mark-active
	     (not cua--explicit-region-start)
	     (not (aquamacs--shift-key-for-command-p)))
	(goto-char left)
      (let ((this-command 'backward-car)) ;; maintain compatibility
	(call-interactively 'backward-char)))))


(defun aquamacs-forward-char (&rest args)
  "Move point to the right or the end of the region.
 Like `forward-char', but moves point to the end of the region
provided `cua-mode' and the mark are active."
  (interactive)
  (let ((right (max (point) (or (mark t) 0))))

    (if (and cua-mode transient-mark-mode 
	     mark-active
	     (not cua--explicit-region-start)
	     (not (aquamacs--shift-key-for-command-p)))
	(goto-char right)
       (let ((this-command 'forward-car)) ;; maintain compatibility
	 (call-interactively 'forward-char)))))

(dolist (cmd
	 '(aquamacs-backward-char aquamacs-forward-char))
  (put cmd 'CUA 'move))

(defun aquamacs--shift-key-for-command-p ()
;; code from cua-base.el
(if window-system
	(memq 'shift (event-modifiers
		      (aref (this-single-command-raw-keys) 0)))
      (or
       (memq 'shift (event-modifiers
		     (aref (this-single-command-keys) 0)))
       ;; See if raw escape sequence maps to a shifted event, e.g. S-up or C-S-home.
       (and (boundp 'function-key-map)
	    function-key-map
	    (let ((ev (lookup-key function-key-map
				  (this-single-command-raw-keys))))
	      (and (vector ev)
		   (symbolp (setq ev (aref ev 0)))
		   (string-match "S-" (symbol-name ev))))))))



;; respects goal-column
;; does not respect (yet)  track-eol 
;; unchecked: line-move-ignore-invisible
(defun buffer-line-at-point ()
 (or (cdr (nth 2 (posn-at-point))) 0))

(defun visual-line-at-point ()
 (or (cdr (nth 6 (posn-at-point))) 0))

(defun visual-col-at-point ()
(- (point)
   (save-excursion
     (vertical-motion 0)
     (point))))
  ;; seems slower (in situations with very long lines)
  ;;(or (car (nth 6 (posn-at-point))) 0))

(defun visual-pixel-col-at-point ()
 (or 
  (car-safe (pos-visible-in-window-p (point) nil 'partial))
  0))

(defvar visual-movement-temporary-goal-column nil)
(make-variable-buffer-local 'visual-movement-temporary-goal-column)

(defvar visual-previous-scroll-margin 'none)
(defun visual-restore-scroll-margin ()
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
:group 'Aquamacs)

;; (setq scroll-margin 0)
;; (setq visual-scroll-margin 5) 
(defun visual-line-up (num-lines)
"Move cursor vertically up NUM-LINES lines.
Interactively, vscroll tall lines if `auto-window-vscroll' is
enabled.  If there is no character in the target line exactly
over the current horizontal pixel position, the cursor is
positioned close to the character in that line at the same position, 
or at the end of the line if it is not long enough.

The command C-x C-n can be used to create
a semipermanent goal column for this command.
Then instead of trying to move exactly vertically (or as close as possible),
this command moves to the specified goal column (or as close as possible).
The goal column is stored in the variable `goal-column', which is nil
when there is no goal column.

This function differs from `previous-line' as it moves vertically 
in the visual sense. The result differs when variable-width font is
used or when characters of non-standard width (e.g. TABs) are used.

If you are thinking of using this in a Lisp program, consider using
`forward-line' with a negative argument instead.  It is usually easier
to use and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (if (bobp) (signal 'beginning-of-buffer nil))
  (let ((pixel-col (visual-pixel-col-at-point))
	(visual-col (visual-col-at-point))
	(old-point (point))
	(end-of-old-line))

    ;; temporary binding of scroll-margin
    ;; cannot do this with a temporary let binding
    (setq visual-previous-scroll-margin scroll-margin)
    (if visual-scroll-margin
	(setq scroll-margin visual-scroll-margin))
    (add-hook 'pre-command-hook 'visual-restore-scroll-margin)

    

    (save-excursion
      (vertical-motion 1)	;; trying going one down, to left
      (setq end-of-old-line (point)))
 
      (vertical-motion 0)

    (let* ((beg-of-old-line 
		;; move right, but not further than to end of line
	    (prog1 (point)
	      (vertical-motion (- num-lines))))	    ;; one up again
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
				    visual-line-down
				    osxkeys-visual-line-up-in-buffers
				    osxkeys-visual-line-down-in-buffers))
	       (= old-point (1- end-of-old-line)))	
	  ;; jumping from end of line
	      
	   (forward-char (min (or goal-column 
				  visual-movement-temporary-goal-column) 
			      rel-beg-of-old-line))
	;; else, do complete positioning
	;; save original position  
	(setq visual-movement-temporary-goal-column visual-col)

	;; approximate positioning
	(forward-char (min visual-col rel-beg-of-old-line)) 
	;; correct position
	  (if (>= (visual-pixel-col-at-point) pixel-col)
	      (progn 
		(while (and 
			(> (visual-pixel-col-at-point) pixel-col)
			(> (point) beg-of-new-line)) ;; do not cross line
		  (forward-char -1)))
	    (progn 
	      (while (and 
		      (< (visual-pixel-col-at-point) pixel-col)
		      (< (point) (1- beg-of-old-line))) ;; do not cross line
		(forward-char +1))))))))

(defun visual-line-down (num-lines)
"Move cursor vertically down NUM-LINES lines.
Interactively, vscroll tall lines if `auto-window-vscroll' is enabled.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one, behavior depends on the
value of `next-line-add-newlines'.  If non-nil, it inserts a newline character
to create a line, and moves the cursor to that line.  Otherwise it moves the
cursor to the end of the buffer.

The command C-x C-n can be used to create
a semipermanent goal column for this command.
Then instead of trying to move exactly vertically (or as close as possible),
this command moves to the specified goal column (or as close as possible).
The goal column is stored in the variable `goal-column', which is nil
when there is no goal column.

This function differs from `next-line' as it moves vertically 
in the visual sense. The result differs when variable-width font is
used or when characters of non-standard width (e.g. TABs) are used.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
 (interactive "p")

 (if (and next-line-add-newlines (= num-lines 1))
     (if (save-excursion (end-of-line) (eobp))
	 ;; When adding a newline, don't expand an abbrev.
	 (let ((abbrev-mode nil))
	   (end-of-line)
	   (insert hard-newline)))
   (if (eobp) (signal 'end-of-buffer nil)))
 (let ((pixel-col (visual-pixel-col-at-point))
	(visual-col (visual-col-at-point))
	(old-point (point))
	(beg-of-line)
	(next-line-start)
	(rel-next-line-start))

    ;; temporary binding of scroll-margin
    ;; cannot do this with a temporary let binding
    (setq visual-previous-scroll-margin scroll-margin)
    (if visual-scroll-margin
	(setq scroll-margin visual-scroll-margin))
    (add-hook 'pre-command-hook 'visual-restore-scroll-margin)

    (vertical-motion num-lines) ;; down
    (save-excursion
      (setq beg-of-line (point))
      (vertical-motion +1) ;; down
      (setq next-line-start (point))
      (setq rel-next-line-start  (- (point) beg-of-line 1)))
    (unless (= beg-of-line (point-max))
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
					visual-line-down
					osxkeys-visual-line-up-in-buffers
					osxkeys-visual-line-down-in-buffers))
		   (= old-point (- beg-of-line 1))) ;; jumping from end of line
	      
	      (forward-char (min (or goal-column 
				     visual-movement-temporary-goal-column) 
				 rel-next-line-start))
	    ;; else, do complete positioning
	    ;; save original position  
	    (setq visual-movement-temporary-goal-column visual-col)
	    (forward-char (min visual-col rel-next-line-start))
	    ;; correct position
	    (if (> (visual-pixel-col-at-point) pixel-col)
		(progn 
		  (while (and 
			  (> (visual-pixel-col-at-point) pixel-col)
			  (> (point) beg-of-line)) ;; do not cross line
		    (forward-char -1)))
	      (progn 
		(while (and 
			(< (visual-pixel-col-at-point) pixel-col)
			(< (point) (1- next-line-start))) ;; do not cross line
		  (forward-char +1))))))))

	    
(defun beginning-of-visual-line ()
  (interactive)
  (if (bobp)
      (signal 'beginning-of-buffer nil))
  (vertical-motion 0))

(defun end-of-visual-line ()
  (interactive)
  (if (eobp)
      (signal 'end-of-buffer nil))
  (let ((end-of-line (line-end-position)))
    (vertical-motion 1)
     (unless (or (eobp)
		 (< (point) end-of-line)) ;; jumping over wrapped text
      (backward-char 1))))

;; this code based on simple.el
(defun kill-visual-line (&optional arg)
  "Kill the rest of the visual line; if no nonblanks there, kill thru newline.
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
even beep.)"
  (interactive "P")
  (kill-region (point)
	       ;; It is better to move point to the other end of the kill
	       ;; before killing.  That way, in a read-only buffer, point
	       ;; moves across the text that is copied to the kill ring.
	       ;; The choice has no effect on undo now that undo records
	       ;; the value of point from before the command was run.
	       (progn
		 (if arg
		     (vertical-motion (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (let ((end
			  (save-excursion
			    (end-of-visual-line) (point))))
		     (if (or (save-excursion
			       ;; If trailing whitespace is visible,
			       ;; don't treat it as nothing.
			       (unless show-trailing-whitespace
				 (skip-chars-forward " \t" end))
			       (= (point) end))
			     (and kill-whole-line (bolp)))
			 (visual-line-down 1)
		       (goto-char end))))
		 (point))))


(defun kill-whole-visual-line (&optional arg)
  "Kill current visual line.
With prefix arg, kill that many lines starting from the current line.
If arg is negative, kill backward.  Also kill the preceding newline.
\(This is meant to make \\[repeat] work well with negative arguments.\)
If arg is zero, kill current line but exclude the trailing newline."
  (interactive "p")
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
	 ;; need to use save-excursion to avoid copying the same text
	 ;; twice to the kill ring in read-only buffers.
	 (save-excursion
	   ;; delete in one go
	   (kill-region (progn (vertical-motion 0) (point))
			(progn (vertical-motion 1) (point)))
	 ))
	((< arg 0)
	 (save-excursion
	   (kill-region (point) (progn (end-of-visual-line) (point))))
	 (kill-region (point)
		      (progn (vertical-motion (1+ arg))
			     (unless (bobp) (backward-char))
			     (point))))
	(t
	 (save-excursion
	   (kill-region (progn (vertical-motion 0) (point))
			(progn (vertical-motion arg) (point)))))))


(defun osxkeys-visual-line-up-in-buffers ()
"Moves the cursor up one (visual) line.
If the `up' key would normally be bound to something else than
`previous-line' (as it is the case in minibuffers), the other binding
is called."
  (interactive)
  (let* (osx-key-mode  ;; turn off mode temporarily
	 (binding (key-binding [up])))
    (if (eq binding 'previous-line)
	(call-interactively (function visual-line-up))
      (call-interactively binding))))


(defun osxkeys-visual-line-down-in-buffers ()
"Moves the cursor down one (visual) line.
If the `down' key would normally be bound to something else than
`next-line' (as it is the case in minibuffers), the other binding
is called."
  (interactive)
  (let* (osx-key-mode  ;; turn off mode temporarily
	 (binding (key-binding [down])))
    (if (eq binding 'next-line)
	(call-interactively (function visual-line-down))
      (call-interactively binding))))

;; mark functions for CUA
(dolist (cmd
	 '( beginning-of-visual-line 
	    end-of-visual-line
	    visual-line-down visual-line-up
	    osxkeys-visual-line-up-in-buffers
	    osxkeys-visual-line-down-in-buffers))
  (put cmd 'CUA 'move))


(defun aquamacs-clipboard-kill-ring-save-secondary ()
  "Copy secondary selection to kill ring, and save in the X clipboard."
(interactive)
  (if (and mouse-secondary-overlay
	   (overlay-start mouse-secondary-overlay)
	   (overlay-end mouse-secondary-overlay))
  (let ((x-select-enable-clipboard t)
	(cua-keep-region-after-copy t))
    (clipboard-kill-ring-save 
     (overlay-start mouse-secondary-overlay) 
     (overlay-end mouse-secondary-overlay) )
    (message "Secondary selection saved to clipboard and kill-ring.")
    )
  ; else
  (message "The secondary selection is not set.")
  ))

(defun aquamacs-clipboard-kill-secondary ()
  "Kill the secondary selection, and save it in the X clipboard."
   (interactive)
   (if mouse-secondary-overlay
       (let ((x-select-enable-clipboard t))
	 (clipboard-kill-region 
	  (overlay-start mouse-secondary-overlay)
	  (overlay-end mouse-secondary-overlay))
	 (message "Secondary selection saved to clipboard and kill-ring, then killed.")    
	 )
					; else
     (message "The secondary selection is not set.")))

(defmacro allow-line-as-region-for-function (orig-function)
`(defun ,(intern (concat (symbol-name orig-function) "-or-line")) 
   ()
   ,(format "Like `%s', but acts on the current line if mark is not active." orig-function)
   (interactive)
   (if mark-active
       (call-interactively (function ,orig-function))
     (save-excursion 
       ;; define a region (temporarily) -- so any C-u prefixes etc. are preserved.
       (beginning-of-line)
       (set-mark (point))
       (end-of-line)
       (call-interactively (function ,orig-function))))))

(allow-line-as-region-for-function comment-region)
(allow-line-as-region-for-function uncomment-region)
(allow-line-as-region-for-function comment-or-uncomment-region)



(defun aquamacs-use-selection-for-find (beg end)
  (interactive "r")  
  (if mark-active
      (setq isearch-string (buffer-substring beg end))))

;; (add-hook 'after-init-hook 
;; 	  (lambda () 
	    
;; 	    ;; make a copy of it as a workaround attempt
;; 	   ;;  (use-global-map (copy-keymap (current-global-map)))
;; 	    (setq global-map-backup global-map)
;; 	    )
;; 	  )

;; 

;;  aquamacs context menu


(defun aquamacs-mouse-get-word ()
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if last-nonmenu-event ;; mouse used
	(save-excursion
	  (mouse-set-point last-nonmenu-event)
	  (mouse-skip-word -1)
	  (set-mark (point))
	  (mouse-set-point last-nonmenu-event)
	  (mouse-skip-word 1)
	  (buffer-substring-no-properties (region-beginning) (region-end)))
      (thing-at-point 'word))))

    

;; doubles in mailclient.el
(defun aquamacs-encode-string-as-url (string)
  "Convert STRING to a URL, using utf-8 as encoding."
  (apply (function concat)
	 (mapcar
	  (lambda (char)
	    (cond
	     ((eq char ?\x20) "%20")   ;; space
	     ((eq char ?\n) "%0D%0A")  ;; newline 
	     ((string-match "[-a-zA-Z0-9_:/.@]" (char-to-string char))
	      (char-to-string char))   ;; printable
	     (t                        ;; everything else
	      (format "%%%02x" char))))	;; escape
	  ;; Convert string to list of chars
	  (append (encode-coding-string string 'utf-8)))))

(defun aquamacs-google-lookup ()
  (interactive)
  (let ((word (aquamacs-mouse-get-word)))
    (if word
	(browse-url  
	 (concat "http://www.google.com/search?q="  
		 (aquamacs-encode-string-as-url 
		  (substring word 0 (min (length word) 128))))))))

(defun aquamacs-dictionary-lookup ()
  (interactive)
  (let ((word (aquamacs-mouse-get-word)))
    (if word
	(do-applescript (concat 
			 "tell application \"Dictionary\" to activate
tell application \"System Events\"
	tell process \"Dictionary\"
		tell text field 1 of group 1 of tool bar 1 of window \"Dictionary and Thesaurus\"
			keystroke \"" 
			 (replace-regexp-in-string 
			  "[\\\\\"]" ""
			  (substring word 0 (min (length word) 32))) "\"
			keystroke return
		end tell
	end tell
end tell")))))

(defun aquamacs-make-mouse-buffer-menu ( )
  "Return a menu keymap of buffers for selection with the mouse.
This switches buffers in the window that you clicked on,
and selects that window."
  (let ((buffers (buffer-list))  alist menu split-by-major-mode sum-of-squares)
    ;; Make an alist of elements that look like (MENU-ITEM . BUFFER).
    (let ((tail buffers))
      (while tail
	;; Divide all buffers into buckets for various major modes.
	;; Each bucket looks like (MODE NAMESTRING BUFFERS...).
	(with-current-buffer (car tail)
	  (let* ((adjusted-major-mode major-mode) elt)
	    (let ((tail mouse-buffer-menu-mode-groups))
	      (while tail
		(if (string-match (car (car tail)) mode-name)
		    (setq adjusted-major-mode (cdr (car tail))))
		(setq tail (cdr tail))))
	    (setq elt (assoc adjusted-major-mode split-by-major-mode))
	    (if (null elt)
		(setq elt (list adjusted-major-mode
				(if (stringp adjusted-major-mode)
				    adjusted-major-mode
				  mode-name))
		      split-by-major-mode (cons elt split-by-major-mode)))
	    (or (memq (car tail) (cdr (cdr elt)))
		(setcdr (cdr elt) (cons (car tail) (cdr (cdr elt)))))))
	(setq tail (cdr tail))))
    ;; Compute the sum of squares of sizes of the major-mode buckets.
    (let ((tail split-by-major-mode))
      (setq sum-of-squares 0)
      (while tail
	(setq sum-of-squares
	      (+ sum-of-squares
		 (let ((len (length (cdr (cdr (car tail)))))) (* len len))))
	(setq tail (cdr tail))))
    (if (< (* sum-of-squares mouse-buffer-menu-mode-mult)
	   (* (length buffers) (length buffers)))
	;; Subdividing by major modes really helps, so let's do it.
	(let (subdivided-menus (buffers-left (length buffers)))
	  ;; Sort the list to put the most popular major modes first.
	  (setq split-by-major-mode
		(sort split-by-major-mode
		      (function (lambda (elt1 elt2)
				  (> (length elt1) (length elt2))))))
	  ;; Make a separate submenu for each major mode
	  ;; that has more than one buffer,
	  ;; unless all the remaining buffers are less than 1/10 of them.
	  (while (and split-by-major-mode
		      (and (> (length (car split-by-major-mode)) 3)
			   (> (* buffers-left 10) (length buffers))))
	    (let ((this-mode-list (mouse-buffer-menu-alist
				   (cdr (cdr (car split-by-major-mode))))))
	      (and this-mode-list
		   (setq subdivided-menus
			 (cons (cons
				(nth 1 (car split-by-major-mode))
				this-mode-list)
			       subdivided-menus))))
	    (setq buffers-left
		  (- buffers-left (length (cdr (car split-by-major-mode)))))
	    (setq split-by-major-mode (cdr split-by-major-mode)))
	  ;; If any major modes are left over,
	  ;; make a single submenu for them.
	  (if split-by-major-mode
	      (let ((others-list
		     (mouse-buffer-menu-alist
		      ;; we don't need split-by-major-mode any more,
		      ;; so we can ditch it with nconc.
		      (apply 'nconc (mapcar 'cddr split-by-major-mode)))))
		(and others-list
		     (setq subdivided-menus
			   (cons (cons "Others" others-list)
				 subdivided-menus)))))
	  (setq menu (cons "Buffer Menu" (nreverse subdivided-menus))))
      (progn
	(setq alist (mouse-buffer-menu-alist buffers))
	(setq menu (cons "Buffer Menu"
			 (mouse-buffer-menu-split "Select Buffer" alist)))))
  
    (let ((km (make-sparse-keymap)))
      (mapc (lambda (pair)
	      (define-key km (vector (intern (car pair)))
		`(menu-item ,(car pair) 
			,(eval
			  (list 'lambda () 
				'(interactive)
				`(let ((one-buffer-one-frame nil))
				   (switch-to-buffer ,(cdr pair)))))
	      ))) alist)
      km
	 )  )
  )
 

(defun aquamacs-get-mouse-major-mode-menu ()
  "Pop up a mode-specific menu of mouse commands.
Defaults to nil if the major mode doesn't define a menu."
  ;; Switch to the window clicked on, because otherwise
  ;; the mode's commands may not make sense.
  (interactive "@e\nP")
  ;; Let the mode update its menus first.
  (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (let* (;; This is where mouse-major-mode-menu-prefix
	 ;; returns the prefix we should use (after menu-bar).
	 ;; It is either nil or (SOME-SYMBOL).
	 (mouse-major-mode-menu-prefix nil)
	 ;; Keymap from which to inherit; may be null.
	 (ancestor (mouse-major-mode-menu-1
		    (and (current-local-map)
			 (local-key-binding [menu-bar]))))
	 ;; Make a keymap in which our last command leads to a menu or
	 ;; default to the edit menu.
	 (newmap (if ancestor
		     (make-sparse-keymap (concat mode-name " Mode"))
		   nil)))
    (if ancestor
	;; Make our menu inherit from the desired keymap which we want
	;; to display as the menu now.
	(set-keymap-parent newmap ancestor))
    newmap))


(defun raise-next-frame ()
"Raise the next frame.
See also `raise-previous-frame' and `other-frame'.
An Aquamacs-only function."
  (interactive)
  (other-frame 1))

(defun raise-previous-frame ()
"Raise the previous frame.
See also `raise-next-frame' and `other-frame'.
An Aquamacs-only function."
  (interactive)
  (other-frame -1))


(defvar aquamacs-context-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [paste] (cons "Paste" 'clipboard-yank))
    (define-key map [copy] (cons "Copy" 'clipboard-kill-ring-save))
    (define-key map [cut] (cons "Cut" 'clipboard-kill-region))
    (define-key map [aq-cm-sep] '(menu-item "--"))
    (define-key map [dictionary] (cons "Look Up in Dictionary" 
				   'aquamacs-dictionary-lookup))
    (define-key map [google] (cons "Search in Google" 
				   'aquamacs-google-lookup))
    (define-key map [aq-cm-sep3] '(menu-item "--"))
    (define-key map [switch-buffer] nil)
    (define-key map [change-mode] nil)
    (define-key map [mode-menu] nil)
    (define-key map [aq-cm-sep4] '(menu-item "--"))
    (define-key map [yank-here] '(menu-item "Yank here" 
				     mouse-yank-at-click
				     :enable kill-ring))
    ;; (define-key map [spotlight] (cons "Search in Spotlight" 
    ;;				   'aquamacs-spotlight-lookup))

   map) "Keymap for the Aquamacs context menu.")

(defvar aquamacs-popup-context-menu-buffers-state nil)
(require 'aquamacs-menu) ;; for pretty mode name function
(defun aquamacs-popup-context-menu  (event &optional  prefix)
  "Popup a context menu. 
Its content is specified in the keymap `aquamacs-context-menu-map'."
  (interactive "@e \nP")
  ;; Let the mode update its menus first.
  ;; (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (when (frame-or-buffer-changed-p 'aquamacs-popup-context-menu-buffers-state)
    (let ((mode-menu (aquamacs-get-mouse-major-mode-menu)))
      (if mode-menu
	  ;; TO DO major mode might not work unless we switch buffer
	  (define-key aquamacs-context-menu-map [mode-menu] 
	    `(menu-item ,(aquamacs-pretty-mode-name major-mode) ,mode-menu :visible t))
	(define-key aquamacs-context-menu-map [mode-menu] 
	  '(menu-item nil :visible nil))))

    (define-key aquamacs-context-menu-map [switch-buffer] 
      `(menu-item "Switch to Buffer "   
		  ,(aquamacs-make-mouse-buffer-menu)
		  :help "Show a different buffer in this frame"))
    (define-key aquamacs-context-menu-map [change-mode] 
      `(menu-item "Change Major Mode "   
		  ,menu-bar-change-mode-menu
		  :help "Show a different buffer in this frame")))
  
  ;; move popup menu a little so mouse pointer is over first entry
  ;; not needed
  ;; ((pos
  ;; 	 (if (not (eq (event-basic-type event) 'mouse-3))
  ;; 	     event
  ;; 	   (list (lispost (- (car (nth 2 (car (cdr event)))) 0)
  ;; 		       (- (cdr (nth 2 (car (cdr event)))) 0))
  ;; 		 (car (car (cdr event)))))))
  
    (popup-menu aquamacs-context-menu-map event prefix))
  
(defcustom osx-key-mode-mouse-3-behavior #'aquamacs-popup-context-menu
  "Determine behavior of mouse-3 in osx-key-mode.
When set to `aquamacs-popup-context-menu' or nil,  mouse-3
(usually: clicking the right mouse button) will bring up a
context menu.  When set to `mouse-save-then-kill', mouse-3 will
extend the region with `mouse-save-then-kill' (traditional Emacs
behavior)."
  :group 'Aquamacs
  :type '(radio 
	  (function-item :tag "Save or kill text between point and mouse"  
			 mouse-save-then-kill) 
	  (function-item :tag "Show context menu" 
			 aquamacs-popup-context-menu)))
 
(defun osx-key-mode-mouse-3 (event &optional  prefix)
  "Popup a context menu or extend the region.
 Behavior depends on setting of `osx-key-mode-mouse-3-behavior'." 
  (interactive "@e \nP")
  ;; we need to bind last-command to the target command
  ;; so mouse-save-then-kill is not confused and recognizes
  ;; a double click.
  (let* ((cmd (or osx-key-mode-mouse-3-behavior 
	     (function aquamacs-popup-context-menu)))
	(last-command (if (eq last-command this-command)
			  cmd
			last-command)))
    
  (apply cmd 
	 event prefix)))

(defun make-osx-key-low-priority-map (&optional command-key)

(if command-key
      (setq osxkeys-command-key command-key)
    (if mac-command-modifier
	(setq osxkeys-command-key mac-command-modifier)
      )
    )
  (let ((map (make-sparse-keymap)))

    (define-key map `[(meta q)] 'fill-paragraph-or-region)
    (define-key map `[(meta shift q)] 'unfill-paragraph-or-region)
    (define-key map '[(up)] 'visual-line-up)
    (define-key map '[(down)] 'visual-line-down)
    (define-key map `[(,osxkeys-command-key left)] 'beginning-of-visual-line)
    (define-key map `[(,osxkeys-command-key right)] 'end-of-visual-line)

    (define-key map `[(,osxkeys-command-key delete)] 'kill-visual-line)
    (define-key map `[(,osxkeys-command-key backspace)] 'kill-whole-line)
    (define-key map `[(,osxkeys-command-key shift backspace)] 'kill-whole-visual-line)

    (define-key map `[(meta up)] 'cua-scroll-down)
    (define-key map `[(meta down)] 'cua-scroll-up)
    ;; left / right (for transient-mark-mode)
    ;; could be moved into transient-mark-mode-map?
    (define-key map '[(left)] 'aquamacs-backward-char)
    (define-key map '[(right)] 'aquamacs-forward-char)
   (define-key map `[(,osxkeys-command-key up)] 'beginning-of-buffer)
    (define-key map `[(,osxkeys-command-key down)] 'end-of-buffer)
    (define-key map `[(,osxkeys-command-key left)] 'beginning-of-line)
    (define-key map `[(,osxkeys-command-key right)] 'end-of-line)

    (define-key map [(home)] 'beginning-of-buffer)
    (define-key map [(end)] 'end-of-buffer)
    map))

(defvar osx-key-low-priority-key-map
  (make-osx-key-low-priority-map)
  "Low-priority keymap for `osx-key-mode'.
These bindings will be added to the global key map when the mode is
turned on. Toggle mode in order to update the global map.")
;; (setq  osx-key-low-priority-key-map (make-osx-key-low-priority-map))


(defun make-osx-key-mode-map (&optional command-key)
  "Create a mode map for OSX key mode. COMMAND-KEY specifies
which key is mapped to command. mac-command-modifier is the
default."
  (if command-key
      (setq osxkeys-command-key command-key)
    (if mac-command-modifier
	(setq osxkeys-command-key mac-command-modifier)
      )
    )
  (let ((map (make-sparse-keymap)))

    ;; debug log

    (define-key map [mouse-3] 'osx-key-mode-mouse-3)


    (define-key map `[(,osxkeys-command-key \?)] 'aquamacs-user-help)
    (define-key map `[(,osxkeys-command-key shift \?)] 'aquamacs-emacs-manual)

    (define-key map `[(,osxkeys-command-key n)] 'new-frame-with-new-scratch) 
					;open new frame empty
    (define-key map `[(,osxkeys-command-key o)] 'mac-key-open-file) 
					;open new frame with a file

    (define-key map `[(,osxkeys-command-key shift s)] 'mac-key-save-file-as)
    (define-key map `[(,osxkeys-command-key shift o)] 'mac-key-open-file-other-frame) 
					;open new frame with a file
    (define-key map `[(,osxkeys-command-key a)] 'mark-whole-buffer)
    (define-key map `[(,osxkeys-command-key v)] 'clipboard-yank) 
    (define-key map `[(,osxkeys-command-key c)] 'clipboard-kill-ring-save)
    (define-key map `[(meta ,osxkeys-command-key c)] 
      'aquamacs-clipboard-kill-ring-save-secondary)
    ;; this because the combination control-space usually activates Spotlight
    (define-key map `[(control ,osxkeys-command-key space)] 'set-mark)
    (define-key map `[(,osxkeys-command-key x)] 'clipboard-kill-region)
    (define-key map `[(meta ,osxkeys-command-key x)] 'aquamacs-clipboard-kill-secondary)
    (define-key map `[(,osxkeys-command-key s)] 'mac-key-save-file)
    (define-key map `[(,osxkeys-command-key p)] 'aquamacs-print)
    (define-key map `[(,osxkeys-command-key l)] 'goto-line)
    (define-key map `[(,osxkeys-command-key f)] 'isearch-forward)
    (define-key map `[(,osxkeys-command-key g)] 'isearch-repeat-forward)  
    (define-key map `[(,osxkeys-command-key shift g)] 'isearch-repeat-backward)
    (define-key map `[(,osxkeys-command-key e)] 'aquamacs-use-selection-for-find)
    (define-key map `[(,osxkeys-command-key w)] 'close-current-window-asktosave)
    (define-key map `[(,osxkeys-command-key m)] 'iconify-or-deiconify-frame) 
    (define-key map `[(,osxkeys-command-key .)] 'keyboard-quit)
    (define-key map `[(,osxkeys-command-key 49)] 'delete-other-windows) ; 49='1'
    (define-key map `[(,osxkeys-command-key 50)] 'split-window-vertically) ; 50='2'
    
    (define-key map `[(,osxkeys-command-key escape)] 'keyboard-escape-quit)
 
    (define-key map `[(,osxkeys-command-key :)] 'ispell-buffer)


    (define-key global-map `[(,osxkeys-command-key {)] 'comment-region-or-line)
    (define-key global-map `[(,osxkeys-command-key })] 'uncomment-region-or-line)
    (define-key global-map `[(,osxkeys-command-key \')] 'comment-or-uncomment-region-or-line)



    ;; only those keys - C-n and C-p stay Emacs-like
    
    (define-key map `[(,osxkeys-command-key \;)] 'toggle-mac-option-modifier)

    (define-key map `[(control ,osxkeys-command-key q)] 'kill-emacs)
    (define-key map `[(,osxkeys-command-key q)] 'save-buffers-kill-emacs)
    (define-key map (vector (list osxkeys-command-key '\,) ) 'customize)

    (define-key map `[(,osxkeys-command-key z)] 'aquamacs-undo)
    (define-key map `[(,osxkeys-command-key shift z)] 'aquamacs-redo)
    (define-key map `[(,osxkeys-command-key \`)] 'raise-next-frame)
    (define-key map `[(,osxkeys-command-key \<)] 'raise-next-frame)
    (define-key map `[(,osxkeys-command-key \>)] 'raise-previous-frame)
    (if (fboundp 'mac-font-panel-mode)
	(define-key map `[(,osxkeys-command-key t)] 'mac-font-panel-mode))

    map)
  )
 

(defvar osx-key-mode-map
  (make-osx-key-mode-map)
  "Keymap for `osx-key-mode'.")
;;  (setq  osx-key-mode-map (make-osx-key-mode-map))


(defun osx-key-mode-command-key-warning ()
  (and osx-key-mode
       (not (eq mac-command-modifier osxkeys-command-key))
       (message (format
"Warning: You have set `mac-command-modifier' to %s in your 
customizations or init file. The Mac-like keyboard shortcuts
provided by `osx-key-mode' won't work with this setting.
The mode uses `osxkeys-command-key' als Command, which is
currently set to %s. You should change one of those two
variables or turn off `osx-key-mode'.
See the description of `osxkeys-command-key'." 
mac-command-modifier osxkeys-command-key))))

(defun aquamacs-install-low-priority-global-key-map (keymap &optional target)
  "Install keys from keymap keymap into the target (or global) map."
  (let ((target (or target (current-global-map)))
	(overwritten (make-sparse-keymap)))
    (map-keymap
   (lambda (key command)

     (let ((old (lookup-key  target `[,key])))
	   

       (if (keymapp command)  ; key is a prefix key
	   (if (keymapp old)
	       ;; recurse
	       (setq old (aquamacs-install-low-priority-global-key-map
			  command old)))
	 (define-key target `[,key] command))
	 ;; also save "nil" entries for unassigned keys
       (define-key overwritten `[,key] old))) 
   keymap)
    overwritten))

;(aquamacs-install-low-priority-global-key-map osx-key-low-priority-key-map)

 

(defvar osx-key--saved-low-priority-map (make-sparse-keymap)
"Bindings in the global map overwritten when osx-key-mode was turned on.")

(define-minor-mode osx-key-mode
  "Toggle Mac Key mode.
With arg, turn Mac Key mode on iff arg is positive.
When Mac Key mode is enabled, mac-style key bindings are provided.
Setting the `osx-key-mode' variable has limited effect - call
the `osx-key-mode' function to switch mode on or off.
`osx-key-mode-map' and `osx-key-low-priority-key-map' contain the
keymaps used by this mode. They may be modified where necessary."
  :global t
  :group 'osx-key-mode 
  :keymap 'osx-key-mode-map  


;; Change encoding so you can use alt-e and alt-u accents (and others) 
  (set-terminal-coding-system 'iso-8859-1) 
  (set-keyboard-coding-system 'mac-roman) ;; keyboard
  (set-selection-coding-system 'mac-roman) ;; copy'n'paste
 
  (setq mac-emulate-three-button-mouse (if osx-key-mode 'control
					   nil))


  ;; use right mouse click as mouse-3
  (setq mac-wheel-button-is-mouse-2 osx-key-mode)

  (if osx-key-mode
      ;; install low priority map
      (setq osx-key--saved-low-priority-map 
	    (aquamacs-install-low-priority-global-key-map
	    osx-key-low-priority-key-map))
    ;; restore old map
    (when osx-key--saved-low-priority-map
      (aquamacs-install-low-priority-global-key-map
       osx-key--saved-low-priority-map)
      (setq osx-key--saved-low-priority-map (make-sparse-keymap)))
    )

  (osx-key-mode-command-key-warning))(define-minor-mode osx-key-mode
  "Toggle Mac Key mode.
With arg, turn Mac Key mode on iff arg is positive.
When Mac Key mode is enabled, mac-style key bindings are provided.
Setting the `osx-key-mode' variable has limited effect - call
the `osx-key-mode' function to switch mode on or off.
`osx-key-mode-map' and `osx-key-low-priority-key-map' contain the
keymaps used by this mode. They may be modified where necessary."
  :global t
  :group 'osx-key-mode 
  :keymap 'osx-key-mode-map  


;; Change encoding so you can use alt-e and alt-u accents (and others) 
  (set-terminal-coding-system 'iso-8859-1) 
  (set-keyboard-coding-system 'mac-roman) ;; keyboard
  (set-selection-coding-system 'mac-roman) ;; copy'n'paste
 
  (setq mac-emulate-three-button-mouse (if osx-key-mode 'control
					   nil))


  ;; use right mouse click as mouse-3
  (setq mac-wheel-button-is-mouse-2 osx-key-mode)

  (if osx-key-mode
      ;; install low priority map
      (setq osx-key--saved-low-priority-map 
	    (aquamacs-install-low-priority-global-key-map
	    osx-key-low-priority-key-map))
    ;; restore old map
    (when osx-key--saved-low-priority-map
      (aquamacs-install-low-priority-global-key-map
       osx-key--saved-low-priority-map)
      (setq osx-key--saved-low-priority-map (make-sparse-keymap)))
    )

  (osx-key-mode-command-key-warning))

(add-hook 'after-init-hook 'osx-key-mode-command-key-warning)

;; (osx-key-mode 1)
(provide 'osxkeys)
