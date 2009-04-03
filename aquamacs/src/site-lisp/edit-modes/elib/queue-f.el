;;;; $Id: queue-f.el,v 1.1 2006/12/01 23:31:41 davidswelt Exp $
;;;; This file implements a simple FIFO queue.

;; Copyright (C) 1991-1995 Free Software Foundation

;; Author: Inge Wallin <inge@lysator.liu.se>
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
;;;; Author: Inge Wallin
;;;; 

;;; Commentary:

;;; The queue is implemented as a two cons cell list, the first 
;;; containing the tag 'QUEUE.  The car of the the second cons
;;; cell points at the first element of the queue and the cdr points
;;; at the last.  All entries and removals are done using destructive
;;; functions.
;;;


;;; Code:

;; Provide the function version and remove the macro version
(provide 'queue-f)
(setq features (delq 'queue-m features))


;;; ================================================================


(defun queue-create ()
  "Create an empty fifo queue."
  (cons 'QUEUE (cons nil nil)))


(defun queue-p (queue)
  "Return t if QUEUE is a queue, otherwise return nil."
  (eq (car-safe queue) 'QUEUE))


(defun queue-enqueue (queue element)
  "Enter an element into a queue.
Args: QUEUE ELEMENT"
  (let ((elementcell (cons element nil)))
    (if (null (car (cdr queue)))
	;; QUEUE is empty
	(setcar (cdr queue)
		(setcdr (cdr queue) 
			elementcell))
      (setcdr (cdr (cdr queue))
	      elementcell)
      (setcdr (cdr queue)
	      elementcell))))


(defun queue-dequeue (queue)
  "Remove the first element of QUEUE and return it.
If QUEUE is empty, return nil and do nothing."
  (if (not (null (car (cdr queue))))
      (prog1
	  (car (car (cdr queue)))
	(setcar (cdr queue)
		(cdr (car (cdr queue))))
	(if (null (car (cdr queue)))
	    (setcdr (cdr queue) nil)))))


(defun queue-empty (queue)
  "Return t if QUEUE is empty, otherwise return nil."
  (null (car (cdr queue))))


(defun queue-first (queue)
  "Return the first element of QUEUE or nil if it is empty.
The element is not removed."
  (car-safe (car (cdr queue))))


(defun queue-nth (queue n)
  "Return the nth element of a queue, but don't remove it.
Args: QUEUE N
If the length of the queue is less than N, return nil.

The oldest element (the first one) has number 0."
  (nth n (car (cdr queue))))


(defun queue-last (queue)
  "Return the last element of QUEUE or nil if it is empty."
  (car-safe (cdr (cdr queue))))


(defun queue-all (queue)
  "Return a list of all elements of QUEUE or nil if it is empty.
The oldest element in the queue is the first in the list."
  (car (cdr queue)))


(defun queue-copy (queue)
  "Return a copy of QUEUE.  All entries in QUEUE are also copied."
  (let* ((first  (copy-sequence (car (cdr queue))))
	 (last first))
    (while (cdr last)
      (setq last (cdr last)))
    (cons 'QUEUE (cons first last))))


(defun queue-length (queue)
  "Return the number of elements in QUEUE."
  (length (car (cdr queue))))


(defun queue-clear (queue)
  "Remove all elements from QUEUE."
  (setcdr queue (cons nil nil)))

;;; queue-f.el ends here
