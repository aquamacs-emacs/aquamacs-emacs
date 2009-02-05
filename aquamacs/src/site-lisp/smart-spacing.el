;; Smart spacing

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; This file is part of Aquamacs Emacs
;; http://aquamacs.org/

;; Aquamacs Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Aquamacs Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANLITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Gen Public License for more details.

;; You should have received a  of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;; Copyright (C) 2009: David Reitter


;; (defcustom smart-spacing-when-killing-words nil
;;   "Delete extra spaces when killing words.
;; Affects commands `aquamacs-kill-word' and `aquamacs-backwards-kill-word'."
;;   :group 'convenience
;;   :group 'Aquamacs
;;   :type '(choice (const nil) (const t)))

(define-minor-mode smart-spacing-mode
 "Smart spacing: word-wise kill&yank.
When this mode is enabled, kill and yank operations support
word-wise editing.  Afer killing (copying) a word or several
words, the text will be inserted as a full phrase when
yanking. That means that spaces around the word may be inserted
during yanking, and spaces and other word delimiters are removed
during killing as necessary to leave only one space between
words.

During killing, smart-spacing-mode behaves conservatively.  It
will never delete more than one extra space at a time.

This feature is part of Aquamacs."
 :group 'convenience
 :lighter " Spc")

(defun turn-on-smart-spacing-mode ()
  (interactive)
  (smart-spacing-mode 1))

(defun turn-off-smart-spacing-mode ()
  (interactive)
  (smart-spacing-mode 0))

(define-globalized-minor-mode 
  global-smart-spacing-mode smart-spacing-mode
  turn-on-smart-spacing-mode)

(defvar smart-spacing-rules
  '(("  " . (bidi . 1))
    ("--" . 1)
    (" ." . -1)
    (" )" . -1)
    ("( " . 1)
    (" :" . -1)
    (" ," . -1)
    (" ;" . -1)
    (" \"" . -1)
    ("\" " . 1) 
    (" '" . -1)
    ("\n " . 1)
    (" " . 1) ; buffer boundary
    ;; ("\n\n" . "\n")
    )
  "Assoc list for smart spacing.
If key is at point after killing text, delete |value| chars to
the left or the right.  Negative value indicates deletion to the
left.  If value is a cons (xxx . num), then num characters will
be deleted either to the left or to the right, depending on where
the point is when the command is called.")

(defmacro user-buffer-p (buf)
  "Evaluate to t if buffer BUF is not an internal buffer."
  `(not (string= (substring (buffer-name ,buf) 0 1) " ")))

(defun smart-spacing-filter-buffer-substring (beg end &optional delete noprops )   
 "Like `filter-buffer-substring', but add spaces around content if region is a phrase."
 (let* ((from (min beg end)) (to (max beg end))
	;; (move-point (memq (point) (list beg end))) 
	(point-at-end (eq (point) end))
	(use-smart-string 
	 (and
	  smart-spacing-mode
	  (user-buffer-p (current-buffer))
	  (smart-spacing-char-is-word-boundary (1- from) from)
	  (smart-spacing-char-is-word-boundary to (1+ to))))
	;; the following is destructive (side-effect).  
	;; do after checking for word boundaries.
	(string (filter-buffer-substring beg end delete noprops)))
   (when use-smart-string
     (put-text-property 0 (length string)
			'yank-handler 
			'(smart-spacing-yank-handler nil nil nil) 
			string)
     (when delete (smart-remove-remaining-spaces from point-at-end)))
    string))

(defun smart-delete-region (from to)
  (if (and smart-spacing-mode (memq this-command '(cua-delete-region mouse-save-then-kill)))
      (let* ((from (min from to)) 
	     (to (max from to))
	     ;; (move-point (memq (point) (list beg end))) 
	     (point-at-end (eq (point) to))) 
	     
	     (delete-region from to)
	     (smart-remove-remaining-spaces from point-at-end))
    (delete-region from to)))

(defun smart-remove-remaining-spaces (pos point-at-end)
  "Remove remaining spaces.
Adheres to `smart-spacing-rules'.
If POINT-AT-END, behaves as if point was at then end of
a previously deleted region (now at POS)."
  (let ((del (assoc (buffer-substring-no-properties
		     (max (point-min) (- pos 1)) 
		     (min (1- (point-max)) (1+ pos)))
		    smart-spacing-rules)))
    (when del
      (setq del (cdr del))
      ;; in some cases we want point to end up 
      ;; further to the left or to the right,
      ;; depending on whether it was on the left or the right
      ;; edge of the region
      (when (consp del)
	(if point-at-end
	    (setq del (cdr del))
	  (setq del (- (cdr del)))))
      ;; delete either to the left or to the right
      ;; this deletion will keep point in the right place.
      (delete-region pos (+ del pos)))))

(defun smart-spacing-char-is-word-boundary (pos &optional side)
  (or (< pos (point-min))
      (>= pos (point-max))
      (not (let ((str (buffer-substring-no-properties pos (1+ pos))))
	     (or (string-match "\\w" str)
		 (if (eq side 'left) (or (equal str ".") (equal str ")")))
		 (if (eq side 'right) (equal str "(")))))))


(defun smart-spacing-yank-handler (string)
      (when  (and smart-spacing-mode  
		  major-mode ; paranoia
		  (user-buffer-p (current-buffer)))
	(or (smart-spacing-char-is-word-boundary opoint 'right) ; to the right
	     (setq string (concat string " ")))
	(or (smart-spacing-char-is-word-boundary (1- opoint) 'left) ; to the left
	    (setq string (concat " " string))
	     ))
      (insert string))



;; overwrite two functions from simple.el
(defun copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it.
In Transient Mark mode, deactivate the mark.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste."
  (interactive "r")
  (if (eq last-command 'kill-region)
      (kill-append (smart-spacing-filter-buffer-substring beg end) (< end beg))
    (kill-new (smart-spacing-filter-buffer-substring beg end)))
  (if transient-mark-mode
      (setq deactivate-mark t))
  nil)

(defun kill-region (beg end &optional yank-handler)
  "Kill (\"cut\") text between point and mark.
This deletes the text from the buffer and saves it in the kill ring.
The command \\[yank] can retrieve it from there.
\(If you want to kill and then yank immediately, use \\[kill-ring-save].)

If you want to append the killed region to the last killed text,
use \\[append-next-kill] before \\[kill-region].

If the buffer is read-only, Emacs will beep and refrain from deleting
the text, but put the text in the kill ring anyway.  This means that
you can use the killing commands to copy text from a read-only buffer.

This is the primitive for programs to kill text (as opposed to deleting it).
Supply two arguments, character positions indicating the stretch of text
 to be killed.
Any command that calls this function is a \"kill command\".
If the previous command was also a kill command,
the text killed this time appends to the text killed last time
to make one entry in the kill ring.

In Lisp code, optional third arg YANK-HANDLER, if non-nil,
specifies the yank-handler text property to be set on the killed
text.  See `insert-for-yank'."
  ;; Pass point first, then mark, because the order matters
  ;; when calling kill-append.
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (condition-case nil
      (let ((string (smart-spacing-filter-buffer-substring beg end t)))
	(when string			;STRING is nil if BEG = END
	  ;; Add that string to the kill ring, one way or another.
	  (if (eq last-command 'kill-region)
	      (kill-append string (< end beg) yank-handler)
	    (kill-new string nil yank-handler)))
	(when (or string (eq last-command 'kill-region))
	  (setq this-command 'kill-region))
	nil)
    ((buffer-read-only text-read-only)
     ;; The code above failed because the buffer, or some of the characters
     ;; in the region, are read-only.
     ;; We should beep, in case the user just isn't aware of this.
     ;; However, there's no harm in putting
     ;; the region's text in the kill ring, anyway.
     (copy-region-as-kill beg end)
     ;; Set this-command now, so it will be set even if we get an error.
     (setq this-command 'kill-region)
     ;; This should barf, if appropriate, and give us the correct error.
     (if kill-read-only-ok
	 (progn (message "Read only text copied to kill ring") nil)
       ;; Signal an error if the buffer is read-only.
       (barf-if-buffer-read-only)
       ;; If the buffer isn't read-only, the text is.
       (signal 'text-read-only (list (current-buffer)))))))


;; from cua-base.el


(defun cua-delete-region ()
  "Delete the active region.
Save a copy in register 0 if `cua-delete-copy-to-register-0' is non-nil."
  (interactive)
  (let ((start (mark)) (end (point)))
    (or (<= start end)
	(setq start (prog1 end (setq end start))))
    (setq cua--last-deleted-region-text (filter-buffer-substring start end))
    (if cua-delete-copy-to-register-0
	(set-register ?0 cua--last-deleted-region-text))
    (smart-delete-region start end)
   (setq cua--last-deleted-region-pos
	  (cons (current-buffer)
		(and (consp buffer-undo-list)
		     (car buffer-undo-list))))
    (cua--deactivate)
    (/= start end)))

;; currently not advising backward-delete-char-untabity 
;; or delete-char

;; mouse.el

(defun mouse-save-then-kill (click)
  "Save text to point in kill ring; the second time, kill the text.
If the text between point and the mouse is the same as what's
at the front of the kill ring, this deletes the text.
Otherwise, it adds the text to the kill ring, like \\[kill-ring-save],
which prepares for a second click to delete the text.

If you have selected words or lines, this command extends the
selection through the word or line clicked on.  If you do this
again in a different position, it extends the selection again.
If you do this twice in the same position, the selection is killed."
  (interactive "e")
  (let ((before-scroll
	 (with-current-buffer (window-buffer (posn-window (event-start click)))
	   point-before-scroll)))
    (mouse-minibuffer-check click)
    (let ((click-posn (posn-point (event-start click)))
	  ;; Don't let a subsequent kill command append to this one:
	  ;; prevent setting this-command to kill-region.
	  (this-command this-command))
      (if (and (with-current-buffer
                   (window-buffer (posn-window (event-start click)))
		 (and (mark t) (> (mod mouse-selection-click-count 3) 0)
		      ;; Don't be fooled by a recent click in some other buffer.
		      (eq mouse-selection-click-count-buffer
			  (current-buffer)))))
	  (if (not (and (eq last-command 'mouse-save-then-kill)
			(equal click-posn
			       (car (cdr-safe (cdr-safe mouse-save-then-kill-posn))))))
	      ;; Find both ends of the object selected by this click.
	      (let* ((range
		      (mouse-start-end click-posn click-posn
				       mouse-selection-click-count)))
		;; Move whichever end is closer to the click.
		;; That's what xterm does, and it seems reasonable.
		(if (< (abs (- click-posn (mark t)))
		       (abs (- click-posn (point))))
		    (set-mark (car range))
		  (goto-char (nth 1 range)))
		;; We have already put the old region in the kill ring.
		;; Replace it with the extended region.
		;; (It would be annoying to make a separate entry.)
		(kill-new (smart-spacing-filter-buffer-substring (point) (mark t)) t)
		(mouse-set-region-1)
		;; Arrange for a repeated mouse-3 to kill this region.
		(setq mouse-save-then-kill-posn
		      (list (car kill-ring) (point) click-posn))
		(mouse-show-mark))
	    ;; If we click this button again without moving it,
	    ;; that time kill.
	    (smart-delete-region (mark) (point))
	    (setq mouse-selection-click-count 0)
	    (setq mouse-save-then-kill-posn nil))
	(if (and (eq last-command 'mouse-save-then-kill)
		 mouse-save-then-kill-posn
		 (eq (car mouse-save-then-kill-posn) (car kill-ring))
		 (equal (cdr mouse-save-then-kill-posn) (list (point) click-posn)))
	    ;; If this is the second time we've called
	    ;; mouse-save-then-kill, delete the text from the buffer.
	    (progn
	      (smart-delete-region (point) (mark))
	      ;; After we kill, another click counts as "the first time".
	      (setq mouse-save-then-kill-posn nil))
	  ;; This is not a repetition.
	  ;; We are adjusting an old selection or creating a new one.
	  (if (or (and (eq last-command 'mouse-save-then-kill)
		       mouse-save-then-kill-posn)
		  (and mark-active transient-mark-mode)
		  (and (memq last-command
			     '(mouse-drag-region mouse-set-region))
		       (or mark-even-if-inactive
			   (not transient-mark-mode))))
	      ;; We have a selection or suitable region, so adjust it.
	      (let* ((posn (event-start click))
		     (new (posn-point posn)))
		(select-window (posn-window posn))
		(if (numberp new)
		    (progn
		      ;; Move whichever end of the region is closer to the click.
		      ;; That is what xterm does, and it seems reasonable.
		      (if (<= (abs (- new (point))) (abs (- new (mark t))))
			  (goto-char new)
			(set-mark new))
		      (setq deactivate-mark nil)))
		(kill-new (smart-spacing-filter-buffer-substring (point) (mark t)) t))
	    ;; Set the mark where point is, then move where clicked.
	    (mouse-set-mark-fast click)
	    (if before-scroll
		(goto-char before-scroll))
	    (exchange-point-and-mark)   ;Why??? --Stef
	    (kill-new (smart-spacing-filter-buffer-substring (point) (mark t))))
          (mouse-show-mark)
	  (mouse-set-region-1)
	  (setq mouse-save-then-kill-posn
		(list (car kill-ring) (point) click-posn)))))))


; (global-set-key [(met mouse-3)] 'mouse-save-then-kill)


(provide 'smart-spacing)
