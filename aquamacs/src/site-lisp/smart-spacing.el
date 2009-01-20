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
;; but WIT ANY WARRANTY; without even the implied warranty of
;; MERCHANLITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Gen Public License for more details.

;; You should have received a  of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;; Copyright (C) 2009: David Reitter


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

(defun turn-on-smart-spacing-mode ()
  (interactive)
  (smart-spacing-mode 0))

(define-globalized-minor-mode 
  global-smart-spacing-mode smart-spacing-mode
  turn-on-smart-spacing-mode)

(defvar smart-spacing-rules
  '(("  " . 1)
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
    ;; ("\n\n" . "\n")
    )
  "Assoc list for smart spacing.
If key is at point after killing text, delete |value| chars to the left or the right.
Negative value indicates deletion to the left.")

(defun smart-spacing-filter-buffer-substring (beg end &optional delete noprops)   
 "Like `filter-buffer-substring', but add spaces around content if region is a phrase."
 (let* ((from (min beg end)) (to (max beg end))
	;; (move-point (memq (point) (list beg end)))  
	(use-smart-string 
	 (and
	  smart-spacing-mode
	  (smart-spacing-char-is-word-boundary (1- from) from)
	  (smart-spacing-char-is-word-boundary to (1+ to))))
	;; the following is destructive (side-effect).  
	;; do after checking for word boundaries.
	(string (filter-buffer-substring beg end delete noprops)))
   (if use-smart-string
       (progn
	 (put-text-property 0 (length string)
			    'yank-handler '(smart-spacing-yank-handler nil nil nil) string)

	 (when (and delete (> from (point-min)) (< (1+ from) (point-max)))
	   ;; remove  space
	   (let ((del (assoc (buffer-substring-no-properties (- from 1) (1+ from))
			     smart-spacing-rules)))
	     (if del
		 ;; delete either to the left or to the right
		 ;; this deletion will keep point in the right place.
		 (delete-region from (+ (cdr del) from)))))))
     string))


(defun smart-spacing-char-is-word-boundary (pos &optional side)
  (not (and
	(>= pos (point-min))
	(< pos (point-max))
	(let ((str (buffer-substring-no-properties pos (1+ pos))))
	  (or (string-match "\\w" str)
	    (if (eq side 'left) (or (equal str ".") (equal str ")")))
	    (if (eq side 'right) (equal str "(")))))))

(defun smart-spacing-yank-handler (string)
      (when  (and smart-spacing-mode  
		  major-mode ; paranoia
		  (not (eq major-mode 'fundamental-mode)))
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

 

(provide 'smart-spacing)