;;; aquamacs-redo.el -- Redo/undo system for Aquamacs

;; Copyright (C) 1985, 1986, 1987, 1993-1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1997 Kyle E. Jones
;; Copyright (C) 2007 David Reitter

;; Author: Kyle E. Jones, February 1997
;; Author/Maintainer: David Reitter, February 2007
;; Keywords: lisp, extensions

;; This file is part of Aquamacs.

;; Aquamacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Aquamacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Aquamacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.
 
;;; Commentary:

;; Derived partly from lisp/prim/simple.el in XEmacs.

;; Emacs' normal undo system allows you to undo an arbitrary
;; number of buffer changes.  These undos are recorded as ordinary
;; buffer changes themselves.  So when you break the chain of
;; undos by issuing some other command, you can then undo all
;; the undos.  The chain of recorded buffer modifications
;; therefore grows without bound, truncated only at garbage
;; collection time.
;;
;; The redo/undo system is different in two ways:
;;   1. The undo/redo command chain is only broken by a buffer
;;      modification.  You can move around the buffer or switch
;;      buffers and still come back and do more undos or redos.
;;   2. The `aquamacs-redo' command rescinds the most recent undo
;;      without recording the change as a _new_ buffer change.  It
;;      completely reverses the effect of the undo, which includes
;;      making the chain of buffer modification records shorter by
;;      one, to counteract the effect of the undo command making the
;;      record list longer by one.  
;;   3. `aquamacs-undo' will restore point and mark.
;;
 
;; Changes:

;; 02/2007:
;; renamed undo -> aquamacs-undo, and redo -> aquamacs-redo
;; so that old undo/redo commands are still available (C-_)
;; restore mark and point

;;; Code:

(defvar aquamacs-redo-version "2.0aq"
  "Version number for the Aquamcas-Redo package.")


(defvar after-undo-hook nil "Functions called after undo or redo operations.")

(defvar last-buffer-undo-list nil
  "The head of buffer-undo-list at the last time an undo or redo was done.")

(make-variable-buffer-local 'last-buffer-undo-list)
(make-variable-buffer-local 'pending-undo-list)

;; Emacs 20 variable
(defvar undo-in-progress)


(defun aquamacs-can-redo-p ()
  "Return non-nil if `aquamacs-redo' can redo anything."
  (and (not buffer-read-only)
       (not (eq buffer-undo-list t))
       (not (eq last-buffer-undo-list nil))
       (or (eq last-buffer-undo-list buffer-undo-list)
	   (let ((p buffer-undo-list))
	     (and (null (car-safe p)) (setq p (cdr-safe p)))
	     (while (and p (integerp (car-safe p)))
	       (setq p (cdr-safe p)))
	     (eq last-buffer-undo-list p)))
       (not (or (eq buffer-undo-list pending-undo-list)
		(eq (cdr buffer-undo-list) pending-undo-list)))))

(defun aquamacs-redo (&optional count)
  "Redo the the most recent undo.
Prefix arg COUNT means redo the COUNT most recent undos.
If you have modified the buffer since the last redo or undo,
then you cannot redo any undos before then.
See also `aquamacs-undo'."
  (interactive "*p")
  (if (eq buffer-undo-list t)
      (error "No undo information in this buffer"))
  (if (eq last-buffer-undo-list nil)
      (error "No undos to redo"))
  (or (eq last-buffer-undo-list buffer-undo-list)
      ;; skip one undo boundary and all point setting commands up
      ;; until the next undo boundary and try again.
      (let ((p buffer-undo-list))
	(and (null (car-safe p)) (setq p (cdr-safe p)))
	(while (and p (integerp (car-safe p)))
	  (setq p (cdr-safe p)))
	(eq last-buffer-undo-list p))
      (error "Buffer modified since last undo/redo, cannot redo"))
  (and (or (eq buffer-undo-list pending-undo-list)
	   (eq (cdr buffer-undo-list) pending-undo-list))
       (error "No further undos to redo in this buffer"))
  (or (eq (selected-window) (minibuffer-window))
      (message "Redo..."))
  (let ((modified (buffer-modified-p))
	(undo-in-progress t)
	(recent-save (recent-auto-save-p))
	(old-undo-list buffer-undo-list)
	(p (cdr buffer-undo-list))
	(records-between 0))
    ;; count the number of undo records between the head of the
    ;; undo chain and the pointer to the next change.  Note that
    ;; by `record' we mean clumps of change records, not the
    ;; boundary records.  The number of records will always be a
    ;; multiple of 2, because an undo moves the pending pointer
    ;; forward one record and prepend a record to the head of the
    ;; chain.  Thus the separation always increases by two.  When
    ;; we decrease it we will decrease it by a multiple of 2
    ;; also.
    (while p
      (cond ((eq p pending-undo-list)
	     (setq p nil))
	    ((null (car p))
	     (setq records-between (1+ records-between))
	     (setq p (cdr p)))
	    (t
	     (setq p (cdr p)))))
    ;; we're off by one if pending pointer is nil, because there
    ;; was no boundary record in front of it to count.
    (and (null pending-undo-list)
	 (setq records-between (1+ records-between)))
    ;; don't allow the user to redo more undos than exist.
    ;; only half the records between the list head and the pending
    ;; pointer are undos that are a part of this command chain.
    (setq count (min (/ records-between 2) count)
	  p (primitive-undo (1+ count) buffer-undo-list))
    (if (eq p old-undo-list)
	nil ;; nothing happened
      ;; set buffer-undo-list to the new undo list.  if has been
      ;; shortened by `count' records.
      (setq buffer-undo-list p)
      ;; primitive-undo returns a list without a leading undo
      ;; boundary.  add one.
      (undo-boundary)
      ;; now move the pending pointer backward in the undo list
      ;; to reflect the redo.  sure would be nice if this list
      ;; were doubly linked, but no... so we have to run down the
      ;; list from the head and stop at the right place.
      (let ((n (- records-between count)))
	(setq p (cdr old-undo-list))
	(while (and p (> n 0))
	  (if (null (car p))
	      (setq n (1- n)))
	  (setq p (cdr p)))
	(setq pending-undo-list p)))
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary recent-save))
    (or (eq (selected-window) (minibuffer-window))
	(message "Redo!"))
    (setq last-buffer-undo-list buffer-undo-list))
  (run-hooks 'after-undo-hook))

(defalias 'redo 'aquamacs-redo) ;; backwards compatibility and convenience

(defun aquamacs-can-undo-p ()
  "Return non-nil if `aquamacs-undo' has something to undo."
  (and (not buffer-read-only)
       (if (or (and buffer-undo-list (eq last-buffer-undo-list buffer-undo-list))
	       (let ((p buffer-undo-list))
		 (and (null (car-safe p)) (setq p (cdr-safe p)))
		 (while (and p (integerp (car-safe p)))
		   (setq p (cdr-safe p)))
		 (eq last-buffer-undo-list p)))
	   (and pending-undo-list (listp pending-undo-list))
	 (or (not (eq buffer-undo-list t))))))

(defun aquamacs-undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count.
Unlike `undo', this will also restore the positions of
mark and point, and it will collaborate with `aquamacs-redo'."
  (interactive "*p")
  (let ((modified (buffer-modified-p))
	(recent-save (recent-auto-save-p)))
    (or (eq (selected-window) (minibuffer-window))
	(message "Undo..."))
    (or (eq last-buffer-undo-list buffer-undo-list)
	;; skip one undo boundary and all point setting commands up
	;; until the next undo boundary and try again.
	(let ((p buffer-undo-list))
	  (and (null (car-safe p)) (setq p (cdr-safe p)))
	  (while (and p (integerp (car-safe p)))
	    (setq p (cdr-safe p)))
	  (eq last-buffer-undo-list p))
	(progn (undo-start)
	       (undo-more 1)))
    (undo-more (or arg 1))
    ;; Don't specify a position in the undo record for the undo command.
    ;; Instead, undoing this should move point to where the change is.
    ;;
    ;;;; The old code for this was mad!  It deleted all set-point
    ;;;; references to the position from the whole undo list,
    ;;;; instead of just the cells from the beginning to the next
    ;;;; undo boundary.  This does what I think the other code
    ;;;; meant to do.
    (when (listp buffer-undo-list) ;; safety, inserted DR 02/2007

      (let ((list buffer-undo-list)
	    (prev nil))
	(while (and list (not (null (car list))))
	  (if (integerp (car list))
	      (if prev
		  (setcdr prev (cdr list))
		;; impossible now, but maybe not in the future 
		(setq buffer-undo-list (cdr list))))
	  (setq prev list
		list (cdr list))))
      (and modified (not (buffer-modified-p))
	   (delete-auto-save-file-if-necessary recent-save))))
  (or (eq (selected-window) (minibuffer-window))
      (message "Undo!"))
  (setq last-buffer-undo-list buffer-undo-list)
  (run-hooks 'after-undo-hook))



;; Save and restore point and mark
 
(make-variable-buffer-local 'aquamacs-undo--before-command-point)
(make-variable-buffer-local 'aquamacs-undo--before-command-mark) 

(defun aquamacs-undo--rec-region  ()
;; this is necessary to have in pre-command-hook (delete-selection-mode!)
  (unless (eq this-command 'aquamacs-undo)
       (setq aquamacs-undo--before-command-point (point))
      (setq aquamacs-undo--before-command-mark (if mark-active (mark)))))

;; fore before-change
(defun aquamacs-undo--rec-region-when-buffer-changes  (beg end &optional oldlen)
  (or (eq this-command 'aquamacs-undo) 
      (eq this-command 'undo)
      (not aquamacs-undo--before-command-mark) 
;      (not mark-active)
      (eq buffer-undo-list t)
      ;; storing add'l point-setting op's would overwrite the effect
      (setq buffer-undo-list  
	    (cons `(apply aquamacs-undo--restore-mark-and-point 
		;	  ,(point) ,(mark))
			  ,aquamacs-undo--before-command-point 
			  ,aquamacs-undo--before-command-mark) 
		  buffer-undo-list))))

(defun aquamacs-undo--restore-mark-and-point (point mark)
  (when (eq this-command 'aquamacs-undo)
    (goto-char point)
    (if mark (set-mark mark))
    (setq deactivate-mark nil)))


;; need to make sure this is before delete-selection-pre-hook

(add-hook 'pre-command-hook 'aquamacs-undo--rec-region)

(add-hook 'before-change-functions 
	  'aquamacs-undo--rec-region-when-buffer-changes) 


;; this is a workaround so that selecting a region by mouse
;; and pressing DEL / backspace works with Undo
;; (otherwise, pre-command-hooks are not run due to the hackiness of
;; mouse.el)

(aquamacs-set-defaults '((mouse-region-delete-keys nil)))

(provide 'aquamacs-redo)
