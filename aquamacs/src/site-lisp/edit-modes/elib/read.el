;;;; $Id: read.el,v 1.1 2006/12/01 23:31:41 davidswelt Exp $
;;;; This file contains a number of functions for reading from the
;;;; minibuffer.

;; Copyright (C) 1991-1995 Free Software Foundation

;; Author: Inge Wallin <inge@lysator.liu.se>
;;	Lars Willför <willfor@lysator.liu.se>
;; Maintainer: elib-maintainers@lysator.liu.se
;; Created: before 11 May 1991
;; Keywords: extensions, lisp

;;;;
;;;; This file is part of the GNU Emacs lisp library, Elib.
;;;;
;;;; GNU Elib is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; GNU Elib is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Elib; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA 02111-1307, USA
;;;;
;;;; Authors: Inge Wallin, Lars Willf|r
;;;;


(provide 'read)

;;; Code:

(defun read-number (&optional prompt default)
  "Read a number from the minibuffer. 
Optional arguments: PROMPT DEFAULT.

If DEFAULT is non-nil, it is written within parenthesis after the prompt.
DEFAULT can be either a number, or of the type which `(interactive P)'
generates." 
  (let ((numdefault (cond ((null default) nil)
			  ((integerp default) default)
			  ((listp default) (car default))
			  (t nil)))
	(number nil)
	(numstr nil))

    (while (not number)
      (setq numstr (read-string
		    (concat (if prompt 
				prompt
			      "Enter a number: ")
			    (if numdefault 
				(format "(%d) " numdefault)))))
      (cond ((and (string= numstr "") 
		  numdefault)
	     (setq number numdefault))
	    ((string-match "\\`[0-9]+\\'" numstr)
	     (setq number (string-to-int numstr)))
	    (t (beep))))
    number))


(defun read-num-range (low high &optional prompt show-range)
  "Read a number within an interval from the minibuffer.
Args: LOW HIGH &optional PROMPT SHOW-RANGE.

The number read must be within the range [LOW HIGH].
If SHOW-RANGE is non-nil, the prompt will include the range for information
to the user."
  (let ((number (1- low)))
    (while (or (< number low)
	       (> number high))
      (setq number (read-number 
		    (concat (if prompt
				prompt
			      "Enter a number: ")
			    (if show-range
				(format "(%d-%d) " low high)
			      "")))))
    number))


(defun read-silent (prompt &optional outchar)
  "Read a string in the minibuffer without echoing.
Args: PROMPT &optional OUTCHAR.

For each character the user writes, one OUTCHAR is displayed."
  (message prompt)
  (let ((input-string "")
	(input-char)
	(cursor-in-echo-area t)
	(showstring ""))
    (while (not (or (eq (setq input-char (read-char)) ?\r)
		    (eq input-char ?\n)))
      (cond
       ((eq input-char ?\C-?)
	(if (equal (length input-string)
		   0)
	    nil
	  (setq showstring (substring showstring 0 -1))
	  (setq input-string (substring input-string 0 -1))))
       ((eq input-char ?\C-u)
	(setq showstring "")
	(setq input-string ""))
       (t
	(if outchar
	    (setq showstring 
		  (concat showstring (char-to-string outchar))))
	(setq input-string
	      (concat input-string (char-to-string input-char)))))
      (message (concat prompt (if outchar
				  showstring
				nil))))
    (message "")
    input-string))

;; read.el ends here
