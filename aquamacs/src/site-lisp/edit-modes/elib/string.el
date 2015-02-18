;;;; $Id: string.el,v 1.1 2006/12/01 23:31:42 davidswelt Exp $
;;;; This file contains some miscellaneous string functions

;; Copyright (C) 1991-1995 Free Software Foundation

;; Author: Sebastian Kremer <sk@thp.Uni-Koeln.DE>
;;	Per Cederqvist <ceder@lysator.liu.se>
;;	Inge Wallin <inge@lysator.liu.se>
;; Maintainer: elib-maintainers@lysator.liu.se
;; Created: before 9 May 1991
;; Keywords: extensions, lisp

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
;;;; Author: Sebastian Kremer
;;;;         sk@thp.Uni-Koeln.DE
;;;; 


;;; Commentary:

;;;
;;; This file is part of the elisp library Elib.
;;; It implements simple generic string functions for use in other 
;;; elisp code: replace regexps in strings, split strings on regexps.
;;; 


;;; Code:

(provide 'string)


;; This function is a near-equivalent of the elisp function replace-match
;; which work on strings instead of a buffer.  The FIXEDCASE parameter
;; of replace-match is not implemented.

(defun string-replace-match (regexp string newtext &optional literal global)
  "Replace first match of REGEXP in STRING with NEWTEXT.
If no match is found, nil is returned instead of the new string.

Optional arg LITERAL non-nil means to take NEWTEXT literally. If LITERAL is 
nil, character `\\' is the start of one of the following sequences:
  \\\\   will be replaced by a single \\.
  \\&   will be replaced by the text which matched the regexp.
  \\N   where N is a number and 1 <= N <= 9, will be replaced
       by the Nth subexpression in REGEXP. Subexpressions are grouped
       inside \\( \\).

Optional arg GLOBAL means to replace all matches instead of only the first."

  (let ((data (match-data)))
    (unwind-protect

	(if global
	    (let ((result "") 
		  (start 0)
		  matchbeginning
		  matchend)
	      (while (string-match regexp string start)
		(setq matchbeginning (match-beginning 0)
		      matchend (match-end 0)
		      result (concat result
				     (substring string start matchbeginning)
				     (if literal
					 newtext
				       (elib-string-expand-newtext)))
		      start matchend))

	      (if matchbeginning	; matched at least once
		  (concat result (substring string start))
		nil))

	  ;; not GLOBAL
	  (if (not (string-match regexp string 0))
	      nil
	    (concat (substring string 0 (match-beginning 0))
		    (if literal newtext (elib-string-expand-newtext))
		    (substring string (match-end 0)))))
      (store-match-data data))))


(defun elib-string-expand-newtext ()
  ;; Expand \& and \1..\9 (referring to STRING) in NEWTEXT.
  ;; Uses match data and fluid vars `newtext', `string'.
  ;; Note that in Emacs 18 match data are clipped to current buffer
  ;; size...so the buffer should better not be smaller than STRING.
  (let ((pos 0)
	(len (length newtext))
	(expanded-newtext ""))
    (while (< pos len)
      (setq expanded-newtext
	    (concat expanded-newtext
		    (let ((c (aref newtext pos)))
		      (if (= ?\\ c)
			  (cond ((= ?\& (setq c (aref newtext
						      (setq pos (1+ pos)))))
				 (substring string
					    (match-beginning 0)
					    (match-end 0)))
				((and (>= c ?1) 
				      (<= c ?9))
				 ;; return empty string if N'th
				 ;; sub-regexp did not match:
				 (let ((n (- c ?0)))
				   (if (match-beginning n)
				       (substring string
						  (match-beginning n)
						  (match-end n))
				     "")))
				(t (char-to-string c)))
			(char-to-string c)))))
      (setq pos (1+ pos)))
    expanded-newtext))


(defun string-split (pattern string &optional limit)
  "Splitting on regexp PATTERN, turn string STRING into a list of substrings.
Optional third arg LIMIT (>= 1) is a limit to the length of the
resulting list."

  (let ((data (match-data)))
    (unwind-protect
	(let* ((start (string-match pattern string))
	       (result (list (substring string 0 start)))
	       (count 1)
	       (end (if start (match-end 0))))
	  (if end			; else nothing left
	      (while (and (or (not (integerp limit))
			      (< count limit))
			  (string-match pattern string end))
		(setq start (match-beginning 0)
		      count (1+ count)
		      result (cons (substring string end start) result)
		      end (match-end 0)
		      start end)))
	  (if (and (or (not (integerp limit))
		       (< count limit))
		   end)			; else nothing left
	      (setq result
		    (cons (substring string end) result)))
	  (nreverse result))
      (store-match-data data))))

;;; string.el ends here
