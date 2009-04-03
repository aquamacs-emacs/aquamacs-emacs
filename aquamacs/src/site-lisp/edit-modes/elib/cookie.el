;;; $Id: cookie.el,v 1.1 2006/12/01 23:31:41 davidswelt Exp $
;;; cookie.el -- Utility to display cookies in buffers

;; Copyright (C) 1991-1995   Free Software Foundation

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;;	Inge Wallin <inge@lysator.liu.se>
;; Maintainer: elib-maintainers@lysator.liu.se
;; Created: 3 Aug 1992
;; Keywords: extensions, lisp

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Elib; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA
;;;


;;; Commentary:

;;;     Introduction
;;;     ============
;;;
;;; Cookie is a package that implements a connection between an
;;; dll (a doubly linked list) and the contents of a buffer.
;;; Possible uses are dired (have all files in a list, and show them),
;;; buffer-list, kom-prioritize (in the LysKOM elisp client) and
;;; others.  pcl-cvs.el uses cookie.el.
;;;
;;; A `cookie' can be any lisp object.  When you use the cookie
;;; package you specify a pretty-printer, a function that inserts
;;; a printable representation of the cookie in the buffer.  (The
;;; pretty-printer should use "insert" and not
;;; "insert-before-markers").
;;;
;;; A `collection' consists of a doubly linked list of cookies, a
;;; header, a footer and a pretty-printer.  It is displayed at a
;;; certain point in a certain buffer.  (The buffer and point are
;;; fixed when the collection is created).  The header and the footer
;;; are constant strings.  They appear before and after the cookies.
;;; (Currently, once set, they can not be changed).
;;;
;;; Cookie does not affect the mode of the buffer in any way. It
;;; merely makes it easy to connect an underlying data representation
;;; to the buffer contents.
;;;
;;; A `tin' is an object that contains one cookie.  There are
;;; functions in this package that given a tin extracts the cookie, or
;;; gives the next or previous tin.  (All tins are linked together in
;;; a doubly linked list.  The 'previous' tin is the one that appears
;;; before the other in the buffer.)  You should not do anything with
;;; a tin except pass it to the functions in this package.
;;;
;;; A collection is a very dynamic thing.  You can easily add or
;;; delete cookies.  You can sort all cookies in a collection (you
;;; have to supply a function that compares two cookies).  You can
;;; apply a function to all cookies in a collection, et c, et c.
;;;
;;; Remember that a cookie can be anything.  Your imagination is the
;;; limit!  It is even possible to have another collection as a
;;; cookie.  In that way some kind of tree hierarchy can be created.
;;;
;;; Full documentation will, God willing, soon be available in a
;;; Texinfo manual.



;;;     Coding conventions
;;;     ==================
;;;
;;; All functions that are intended for external use begin with one of
;;; the prefixes "cookie-", "collection-" or "tin-".  The prefix
;;; "elib-" is used for internal functions and macros.  There are
;;; currently no global or buffer-local variables used.
;;;
;;; Many function operate on `tins' instead of `cookies'.  To avoid
;;; confusion most of the function names include the string "cookie"
;;; or "tin" to show this.
;;;
;;; Most doc-strings contains an "Args:" line that lists the
;;; arguments.
;;;
;;; The internal functions don't contain any doc-strings.  RMS thinks
;;; this is a good way to save space.



;;; INTERNAL DOCUMENTATION (Your understanding of this package might
;;; increase if you read it, but you should not exploit the knowledge
;;; you gain. The internal details might change without notice).
;;;
;;; A collection is implemented as an dll (a doubly linked list).
;;; The first and last element on the list are always the header and
;;; footer (as strings). Any remaining entries are `wrappers'.
;;;
;;; At the implementation level a `tin' is really an elib-node that
;;; consists of
;;;      left        Pointer to previous tin
;;;      right       Pointer to next tin
;;;      data        Holder of a `wrapper'.
;;; These internals of an elib-node are in fact unknown to cookie.el.
;;; It uses dll.el to handle everything that deals with the
;;; doubly linked list.
;;;
;;; The wrapper data type contains
;;;      start-marker    Position of the printed representation of the
;;;                      cookie in the buffer. 
;;;      cookie          The user-supplied cookie.
;;;
;;; The wrapper is not accessible to the user of this package.

;;; Code:

(require 'dll)
(provide 'cookie)


;;; ================================================================
;;;      Internal   macros   for use in the cookie package


(put 'elib-set-buffer-bind-dll 'lisp-indent-hook 1)

(defmacro elib-set-buffer-bind-dll (collection &rest forms)

  ;; Execute FORMS with collection->buffer selected as current buffer,
  ;; and dll bound to collection->dll.
  ;; Return value of last form in FORMS.  INTERNAL USE ONLY.

  (let ((old-buffer (make-symbol "old-buffer"))
	(hnd (make-symbol "collection")))
    (` (let* (((, old-buffer) (current-buffer))
	      ((, hnd) (, collection))
	      (dll (elib-collection->dll (, hnd))))
	 (set-buffer (elib-collection->buffer (, hnd)))
	 (unwind-protect
	     (progn (,@ forms))
	   (set-buffer (, old-buffer)))))))


(put 'elib-set-buffer-bind-dll-let* 'lisp-indent-hook 2)

(defmacro elib-set-buffer-bind-dll-let* (collection varlist &rest forms)

  ;; Execute FORMS with collection->buffer selected as current buffer,
  ;; dll bound to collection->dll, and VARLIST bound as in a let*.
  ;; dll will be bound when VARLIST is initialized, but the current
  ;; buffer will *not* have been changed.
  ;; Return value of last form in FORMS.  INTERNAL USE ONLY.

  (let ((old-buffer (make-symbol "old-buffer"))
	(hnd (make-symbol "collection")))
    (` (let* (((, old-buffer) (current-buffer))
	      ((, hnd) (, collection))
	      (dll (elib-collection->dll (, hnd)))
	      (,@ varlist))
	 (set-buffer (elib-collection->buffer (, hnd)))
	 (unwind-protect
	     (progn (,@ forms))
	   (set-buffer (, old-buffer)))))))


(defmacro elib-filter-hf (collection tin)

  ;; Evaluate TIN once and return it. BUT if it is
  ;; the header or the footer in COLLECTION return nil instead.
  ;; Args: COLLECTION TIN
  ;; INTERNAL USE ONLY.

  (let ((tempvar (make-symbol "tin"))
	(tmpcoll (make-symbol "tmpcollection")))
    (` (let (((, tempvar) (, tin))
	     ((, tmpcoll) (, collection)))
	 (if (or (eq (, tempvar) (elib-collection->header (, tmpcoll)))
		 (eq (, tempvar) (elib-collection->footer (, tmpcoll))))
	     nil
	   (, tempvar))))))



;;; ================================================================
;;;      Internal   data types   for use in the cookie package

;;; Yes, I know about cl.el, but I don't like it.   /ceder

;;; The wrapper data type.

(defun elib-create-wrapper (start-marker cookie)
  ;; Create a wrapper.   INTERNAL USE ONLY.
  (cons 'WRAPPER (vector start-marker cookie)))

(defun elib-wrapper->start-marker (wrapper)
  ;; Get start-marker from wrapper.    INTERNAL USE ONLY.
  (elt (cdr wrapper) 0))

(defun elib-wrapper->cookie-safe (wrapper)
  ;; Get cookie from wrapper.   INTERNAL USE ONLY.
  ;; Returns nil if given nil as input.
  ;; Since (elt nil 1) returns nil in emacs version 18.57 and 18.58
  ;; this can be defined in this way. The documentation in the info
  ;; file says that elt should signal an error in that case. I think
  ;; it is the documentation that is buggy. (The bug is reported).
  (elt (cdr wrapper) 1))

(defun elib-wrapper->cookie (wrapper)
  ;; Get cookie from wrapper.   INTERNAL USE ONLY.
  (elt (cdr wrapper) 1))



;;; The collection data type

(defun elib-create-collection (buffer pretty-printer 
					 header-wrapper footer-wrapper
					 dll)
  ;; Create a collection. INTERNAL USE ONLY.
  (cons 'COLLECTION
	;; The last element is a pointer to the last tin
	;; the cursor was at, or nil if that is unknown.  
	(vector buffer
		pretty-printer 
		header-wrapper footer-wrapper
		dll nil)))


(defun elib-collection->buffer (collection)
  ;; Get buffer from COLLECTION.
  (elt (cdr collection) 0))

(defun elib-collection->pretty-printer (collection)
  ;; Get pretty-printer from COLLECTION.
  (elt (cdr collection) 1))

(defun elib-collection->header (collection)
  ;; Get header from COLLECTION.
  (elt (cdr collection) 2))

(defun elib-collection->footer (collection)
  ;; Get footer from COLLECTION.
  (elt (cdr collection) 3))

(defun elib-collection->dll (collection)
  ;; Get dll from COLLECTION.
  (elt (cdr collection) 4))

(defun elib-collection->last-tin (collection)
  ;; Get last-tin from COLLECTION.
  (elt (cdr collection) 5))



(defun elib-set-collection->buffer (collection buffer)
  ;; Change the buffer. Args: COLLECTION BUFFER.
  (aset (cdr collection) 0 buffer))

(defun elib-set-collection->pretty-printer (collection pretty-printer)
  ;; Change the pretty-printer. Args: COLLECTION PRETTY-PRINTER.
  (aset (cdr collection) 1 pretty-printer))

(defun elib-set-collection->header (collection header)
  ;; Change the header. Args: COLLECTION HEADER.
  (aset (cdr collection) 2 header))

(defun elib-set-collection->footer (collection footer)
  ;; Change the footer. Args: COLLECTION FOOTER.
  (aset (cdr collection) 3 footer))

(defun elib-set-collection->dll (collection dll)
  ;; Change the dll. Args: COLLECTION DLL.
  (aset (cdr collection) 4 dll))

(defun elib-set-collection->last-tin (collection last-tin)
  ;; Change the last-tin. Args: COLLECTION LAST-TIN.
  (aset (cdr collection) 5 last-tin))


;;; ================================================================
;;;      Internal   functions   for use in the cookie package

(defun elib-abs (x)
  ;; Return the absolute value of x
  (max x (- x)))

(defun elib-create-wrapper-and-insert (cookie string pos)
  ;; Insert STRING at POS in current buffer. Remember the start
  ;; position. Create a wrapper containing that start position and the
  ;; COOKIE.
  ;;    INTERNAL USE ONLY.

  (save-excursion
    (goto-char pos)
    ;; Remember the position as a number so that it doesn't move
    ;; when we insert the string.
    (let ((start (if (markerp pos)
		     (marker-position pos)
		   pos))
	  (buffer-read-only nil))
      ;; Use insert-before-markers so that the marker for the
      ;; next cookie is updated.
      (insert-before-markers string)

      ;; Always insert a newline. You want invisible cookies? You
      ;; lose. (At least in this version). FIXME-someday. (It is
      ;; harder to fix than it might seem. All markers have to point
      ;; to the right place all the time...)
      (insert-before-markers ?\n)
      (elib-create-wrapper (copy-marker start) cookie))))


(defun elib-create-wrapper-and-pretty-print (cookie
						pretty-printer pos)
  ;; Call PRETTY-PRINTER with point set at POS in current buffer.
  ;; Remember the start position. Create a wrapper containing that
  ;; start position and the COOKIE.
  ;;    INTERNAL USE ONLY.

  (save-excursion
    (goto-char pos)
    ;; Remember the position as a number so that it doesn't move
    ;; when we insert the string.
    (let ((start (if (markerp pos)
		     (marker-position pos)
		   pos))
	  (buffer-read-only nil))
      ;; Insert the trailing newline using insert-before-markers
      ;; so that the start position for the next cookie is updated.
      (insert-before-markers ?\n)
      ;; Move back, and call the pretty-printer.
      (backward-char 1)
      (funcall pretty-printer cookie)
      (elib-create-wrapper (copy-marker start) cookie))))


(defun elib-delete-tin-internal (collection tin)
  ;; Delete a cookie string from COLLECTION.  INTERNAL USE ONLY.
  ;; Can not be used on the footer. Returns the wrapper that is deleted.
  ;; The start-marker in the wrapper is set to nil, so that it doesn't
  ;; consume any more resources.
  (let ((dll (elib-collection->dll collection))
	(buffer-read-only nil))
    ;; If we are about to delete the tin pointed at by last-tin,
    ;; set last-tin to nil.
    (if (eq (elib-collection->last-tin collection) tin)
	(elib-set-collection->last-tin collection nil))

    (delete-region (elib-wrapper->start-marker (dll-element dll tin))
		   (elib-wrapper->start-marker
		    (dll-element dll (dll-next dll tin))))
    (set-marker (elib-wrapper->start-marker (dll-element dll tin)) nil)
    ;; Delete the tin, and return the wrapper.
    (dll-delete dll tin)))

(defun elib-refresh-tin (collection tin)
  ;; Redisplay the cookie represented by TIN. INTERNAL USE ONLY.
  ;; Args: COLLECTION TIN
  ;; Can not be used on the footer. dll *must* be bound to
  ;; (elib-collection->dll collection).

  (let ((buffer-read-only nil))
    (save-excursion
      ;; First, remove the string from the buffer:
      (delete-region (elib-wrapper->start-marker (dll-element dll tin))
		     (1- (marker-position
			  (elib-wrapper->start-marker
			   (dll-element dll (dll-next dll tin))))))

      ;; Calculate and insert the string.

      (goto-char (elib-wrapper->start-marker (dll-element dll tin)))
      (funcall (elib-collection->pretty-printer collection)
	       (elib-wrapper->cookie (dll-element dll tin))))))


(defun elib-pos-before-middle-p (collection pos tin1 tin2)

  ;; Return true if for the cookies in COLLECTION, POS is in the first
  ;; half of the region defined by TIN1 and TIN2.

  (let ((dll (elib-collection->dll collection)))
    (< pos (/ (+ (elib-wrapper->start-marker (dll-element dll tin1))
		 (elib-wrapper->start-marker (dll-element dll tin2)))
	      2))))


;;; ===========================================================================
;;;                  Public members of the cookie package


(defun collection-create (buffer pretty-printer 
			     &optional header footer pos)
  "Create an empty collection of cookies.
Args: BUFFER PRETTY-PRINTER &optional HEADER FOOTER POS.

The collection will be inserted in BUFFER. BUFFER may be a
buffer or a buffer name. It is created if it does not exist.

PRETTY-PRINTER should be a function that takes one argument, a
cookie, and inserts a string representing it in the buffer (at
point). The string PRETTY-PRINTER inserts may be empty or span
several linse. A trailing newline will always be inserted
automatically. The PRETTY-PRINTER should use insert, and not
insert-before-markers.

Optional third argument HEADER is a string that will always be
present at the top of the collection. HEADER should end with a
newline.  Optionaly fourth argument FOOTER is similar, and will
always be inserted at the bottom of the collection.

Optional fifth argument POS is a buffer position, specifying
where the collection will be inserted. It defaults to the
begining of the buffer."

  (let ((new-collection
	 (elib-create-collection (get-buffer-create buffer)
				    pretty-printer nil nil (dll-create))))

    (elib-set-buffer-bind-dll new-collection
      ;; Set default values
      (if (not header)
	  (setq header ""))
      (if (not footer)
	  (setq footer ""))
      (if (not pos)
	  (setq pos (point-min))
	(if (markerp pos)
	    (set pos (marker-position pos)))) ;Force header to be above footer.

      (let ((foot (elib-create-wrapper-and-insert footer footer pos))
	    (head (elib-create-wrapper-and-insert header header pos)))

	(dll-enter-first dll head)
	(dll-enter-last  dll foot)
	(elib-set-collection->header new-collection (dll-nth dll 0))
	(elib-set-collection->footer new-collection (dll-nth dll -1))))

    ;; Return the collection
    new-collection))


(defun tin-cookie (collection tin)
  "Get the cookie from a TIN. Args: COLLECTION TIN."
  (elib-wrapper->cookie (dll-element (cookie->dll collection) tin)))

(defun cookie-enter-first (collection cookie)
  "Enter a COOKIE first in the cookie collection COLLECTION.
Args: COLLECTION COOKIE."

  (elib-set-buffer-bind-dll collection

    ;; It is always safe to insert an element after the first element,
    ;; because the header is always present. (dll-nth dll 0) should
    ;; therefore never return nil.

    (dll-enter-after
     dll
     (dll-nth dll 0)
     (elib-create-wrapper-and-pretty-print
      cookie
      (elib-collection->pretty-printer collection)
      (elib-wrapper->start-marker
       (dll-element dll (dll-nth dll 1)))))))


(defun cookie-enter-last (collection cookie)
  "Enter a COOKIE last in the cookie-collection COLLECTION.
Args: COLLECTION COOKIE."

  (elib-set-buffer-bind-dll collection

    ;; Remember that the header and footer are always present. There
    ;; is no need to check if (dll-nth dll -1) returns nil - it never
    ;; does.

    (dll-enter-before
     dll
     (dll-nth dll -1)
     (elib-create-wrapper-and-pretty-print
      cookie
      (elib-collection->pretty-printer collection)
      (elib-wrapper->start-marker (dll-last dll))))))


(defun cookie-enter-after-tin (collection tin cookie)
  "Enter a new COOKIE after TIN.
Args: COLLECTION TIN COOKIE."
  (elib-set-buffer-bind-dll collection
    (dll-enter-after
     dll tin
     (elib-create-wrapper-and-pretty-print
      cookie
      (elib-collection->pretty-printer collection)
      (elib-wrapper->start-marker (dll-element dll (dll-next dll tin)))))))


(defun cookie-enter-before-tin (collection tin cookie)
  "Enter a new COOKIE before TIN.
Args: COLLECTION TIN COOKIE."
  (elib-set-buffer-bind-dll collection
    (dll-enter-before
     dll tin
     (elib-create-wrapper-and-pretty-print
      cookie
      (elib-collection->pretty-printer collection)
      (elib-wrapper->start-marker (dll-element dll tin))))))


(defun tin-next (collection tin)
  "Get the next tin. Args: COLLECTION TIN.
Returns nil if TIN is nil or the last cookie."
  (if tin
      (elib-filter-hf
       collection (dll-next (elib-collection->dll collection) tin))
    nil))

(defun tin-previous (collection tin)
  "Get the previous tin. Args: COLLECTION TIN.
Returns nil if TIN is nil or the first cookie."
  (if tin
      (elib-filter-hf
       collection
       (dll-previous (elib-collection->dll collection) tin))
    nil))


(defun tin-nth (collection n)
  "Return the Nth tin. Args: COLLECTION N.
N counts from zero. Nil is returned if there is less than N cookies.
If N is negative, return the -(N+1)th last element.
Thus, (tin-nth dll 0) returns the first node,
and (tin-nth dll -1) returns the last node.

Use tin-cookie to extract the cookie from the tin (or use
cookie-nth instead)."

    ;; Skip the header (or footer, if n is negative).
    (if (< n 0)
	(setq n (1- n))
      (setq n (1+ n)))

    (elib-filter-hf collection
		       (dll-nth (elib-collection->dll collection) n)))

(defun cookie-nth (collection n)
  "Return the Nth cookie. Args: COLLECTION N.
N counts from zero. Nil is returned if there is less than N cookies.
If N is negative, return the -(N+1)th last element.
Thus, (cookie-nth dll 0) returns the first cookie,
and (cookie-nth dll -1) returns the last cookie."

    ;; Skip the header (or footer, if n is negative).
    (if (< n 0)
	(setq n (1- n))
      (setq n (1+ n)))

    (let* ((dll (elib-collection->dll collection))
	   (tin (elib-filter-hf collection (dll-nth dll n))))
      (if tin
	  (elib-wrapper->cookie (dll-element dll tin))
	nil)))

(defun tin-delete (collection tin)
  "Delete a tin from a collection. Args: COLLECTION TIN.
The cookie in the tin is returned."

  (elib-set-buffer-bind-dll collection
    (elib-wrapper->cookie
     (elib-delete-tin-internal collection tin))))


(defun cookie-delete-first (collection)
  "Delete first cookie and return it. Args: COLLECTION.
Returns nil if there are no cookies left in the collection."

  (elib-set-buffer-bind-dll-let* collection
      ((tin (dll-nth dll 1)))         ;Skip the header.

    ;; We have to check that we do not try to delete the footer.
    (if (eq tin (elib-collection->footer collection))
	nil
      (elib-wrapper->cookie (elib-delete-tin-internal collection tin)))))


(defun cookie-delete-last (collection)
  "Delete last cookie and return it. Args: COLLECTION.
Returns nil if there is no cookie left in the collection."

  (elib-set-buffer-bind-dll-let* collection
      ((tin (dll-nth dll -2)))		;Skip the footer.
    ;; We have to check that we do not try to delete the header.
    (if (eq tin (elib-collection->header collection))
	nil
      (elib-wrapper->cookie (elib-delete-tin-internal collection tin)))))

(defun cookie-first (collection)
  "Return the first cookie in COLLECTION. The cookie is not removed."

  (let* ((dll (elib-collection->dll collection))
	 (tin (elib-filter-hf collection (dll-nth dll -1))))
    (if tin
	(elib-wrapper->cookie (dll-element dll tin)))))



(defun cookie-last (collection)
  "Return the last cookie in COLLECTION. The cookie is not removed."

  (let* ((dll (elib-collection->dll collection))
	 (tin (elib-filter-hf collection (dll-nth dll -2))))
      (if tin
	  (elib-wrapper->cookie (dll-element dll tin)))))


(defun collection-empty (collection)
  "Return true if there are no cookies in COLLECTION."

  (eq (dll-nth (elib-collection->dll collection) 1) 
      (elib-collection->footer collection)))


(defun collection-length (collection)
  "Return the number of cookies in COLLECTION."

  ;; Don't count the footer and header.
  (- (dll-length (elib-collection->dll collection)) 2))


(defun collection-list-cookies (collection)
  "Return a list of all cookies in COLLECTION."

  (elib-set-buffer-bind-dll-let* collection
      ((result nil)
       (header (elib-collection->header collection))
       (tin (dll-nth dll -2)))
    (while (not (eq tin header))
      (setq result (cons (elib-wrapper->cookie (dll-element dll tin))
			 result))
      (setq tin (dll-previous dll tin)))
    result))


(defun collection-clear (collection)
  "Remove all cookies in COLLECTION."

  (elib-set-buffer-bind-dll-let* collection
      ((header (elib-collection->header collection))
       (footer (elib-collection->footer collection)))

    ;; We have to bind buffer-read-only separately, so that the
    ;; current buffer is correct.
    (let ((buffer-read-only nil))
      (delete-region (elib-wrapper->start-marker
		      (dll-element dll (dll-nth dll 1)))
		     (elib-wrapper->start-marker
		      (dll-element dll footer))))
    (setq dll (dll-create-from-list (list (dll-element dll header)
					  (dll-element dll footer))))
    (elib-set-collection->dll collection dll)

    ;; Re-set the header and footer, since they are now new objects.
    ;; elib-filter-hf uses eq to compare objects to them...
    (elib-set-collection->header collection (dll-nth dll 0))
    (elib-set-collection->footer collection (dll-nth dll -1))))


(defun cookie-map (map-function collection &rest map-args)
  "Apply MAP-FUNCTION to all cookies in COLLECTION.
MAP-FUNCTION is applied to the first element first.
If MAP-FUNCTION returns non-nil the cookie will be refreshed (its
pretty-printer will be called once again).

Note that the buffer for COLLECTION will be current buffer when MAP-FUNCTION 
is called.  MAP-FUNCTION must restore the current buffer to BUFFER before 
it returns, if it changes it.

If more than two arguments are given to cookie-map, remaining
arguments will be passed to MAP-FUNCTION."

  (elib-set-buffer-bind-dll-let* collection
      ((footer (elib-collection->footer collection))
       (tin (dll-nth dll 1)))

    (while (not (eq tin footer))

      (if (apply map-function
		 (elib-wrapper->cookie (dll-element dll tin))
		 map-args)
	  (elib-refresh-tin collection tin))

      (setq tin (dll-next dll tin)))))



(defun cookie-map-reverse (map-function collection &rest map-args)
  "Apply MAP-FUNCTION to all cookies in COLLECTION.
MAP-FUNCTION is applied to the last cookie first.
If MAP-FUNCTION returns non-nil the cookie will be refreshed.

Note that the buffer for COLLECTION will be current buffer when MAP-FUNCTION 
is called.  MAP-FUNCTION must restore the current buffer to BUFFER before 
it returns, if it changes the current buffer.

If more than two arguments are given to cookie-map, remaining
arguments will be passed to MAP-FUNCTION."

  (elib-set-buffer-bind-dll-let* collection
      ((header (elib-collection->header collection))
       (tin (dll-nth dll -2)))

    (while (not (eq tin header))

      (if (apply map-function
		 (elib-wrapper->cookie (dll-element dll tin))
		 map-args)
	  (elib-refresh-tin collection tin))

      (setq tin (dll-previous dll tin)))))



(defun collection-append-cookies (collection cookie-list)
  "Insert all cookies in the list COOKIE-LIST last in COLLECTION.
Args: COLLECTION COOKIE-LIST."

  (while cookie-list
    (cookie-enter-last collection (car cookie-list))
    (setq cookie-list (cdr cookie-list))))


(defun collection-filter-cookies (collection predicate &rest extra-args)
  "Remove all cookies in COLLECTION for which PREDICATE returns nil.
Args: COLLECTION PREDICATE &rest EXTRA-ARGS.
Note that the buffer for COLLECTION will be current-buffer when PREDICATE 
is called. PREDICATE must restore the current buffer before it returns
if it changes it.

The PREDICATE is called with the cookie as its first argument. If any
EXTRA-ARGS are given to collection-filter-cookies they will be passed to the
PREDICATE."

  (elib-set-buffer-bind-dll-let* collection
      ((tin (dll-nth dll 1))
       (footer (elib-collection->footer collection))
       (next nil))
    (while (not (eq tin footer))
      (setq next (dll-next dll tin))
      (if (apply predicate
		 (elib-wrapper->cookie (dll-element dll tin))
		 extra-args)
	  nil
	(elib-delete-tin-internal collection tin))
      (setq tin next))))


(defun collection-filter-tins (collection predicate &rest extra-args)
  "Remove all cookies in COLLECTION for which PREDICATE returns nil.
Note that the buffer for COLLECTION will be current-buffer when PREDICATE 
is called. PREDICATE must restore the current buffer before it returns
if it changes it.

The PREDICATE is called with one argument, the tin. If any EXTRA-ARGS
are given to collection-filter-cookies they will be passed to the PREDICATE."

  (elib-set-buffer-bind-dll-let* collection
      ((tin (dll-nth dll 1))
       (footer (elib-collection->footer collection))
       (next nil))
    (while (not (eq tin footer))
      (setq next (dll-next dll tin))
      (if (apply predicate tin extra-args)
	  nil
	(elib-delete-tin-internal collection tin))
      (setq tin next))))


(defun tin-locate (collection pos &optional guess)
  "Return the tin that POS (a buffer position) is within.
Args: COLLECTION POS &optional GUESS.
POS may be a marker or an integer.
GUESS should be a tin that it is likely that POS is near.

If POS points before the first cookie, the first cookie is returned.
If POS points after the last cookie, the last cookie is returned.
If the COLLECTION is empty, nil is returned."

  (elib-set-buffer-bind-dll-let* collection
      ((footer (elib-collection->footer collection)))

    (cond
     ;; No cookies present?
     ((eq (dll-nth dll 1) (dll-nth dll -1))
      nil)

     ;; Before first cookie?
     ((< pos (elib-wrapper->start-marker
	      (dll-element dll (dll-nth dll 1))))
      (dll-nth dll 1))

     ;; After last cookie?
     ((>= pos (elib-wrapper->start-marker (dll-last dll)))
      (dll-nth dll -2))

     ;; We now now that pos is within a cookie.
     (t
      ;; Make an educated guess about which of the three known
      ;; cookies (the first, the last, or GUESS) is nearest.
      (let* ((best-guess (dll-nth dll 1))
	     (distance (elib-abs (- pos (elib-wrapper->start-marker
					 (dll-element dll best-guess))))))
	(if guess
	    (let* ((g guess)		;Check the guess, if given.
		   (d (elib-abs
		       (- pos (elib-wrapper->start-marker
			       (dll-element dll g))))))
	      (cond
	       ((< d distance)
		(setq distance d)
		(setq best-guess g)))))

	(let* ((g (dll-nth dll -1))	;Check the last cookie
	       (d (elib-abs
		   (- pos (elib-wrapper->start-marker
			   (dll-element dll g))))))
	  (cond
	   ((< d distance)
	    (setq distance d)
	    (setq best-guess g))))

	(if (elib-collection->last-tin collection) ;Check "previous".
	    (let* ((g (elib-collection->last-tin collection)) 
		   (d (elib-abs
		       (- pos (elib-wrapper->start-marker
			       (dll-element dll g))))))
	      (cond
	       ((< d distance)
		(setq distance d)
		(setq best-guess g)))))

	;; best-guess is now a "best guess".
     
	;; Find the correct cookie. First determine in which direction
	;; it lies, and then move in that direction until it is found.
    
	(cond
	 ;; Is pos after the guess?
	 ((>= pos
	      (elib-wrapper->start-marker (dll-element dll best-guess)))

	  ;; Loop until we are exactly one cookie too far down...
	  (while (>= pos (elib-wrapper->start-marker
			  (dll-element dll best-guess)))
	    (setq best-guess (dll-next dll best-guess)))

	  ;; ...and return the previous cookie.
	  (dll-previous dll best-guess))

	 ;; Pos is before best-guess
	 (t

	  (while (< pos (elib-wrapper->start-marker
			 (dll-element dll best-guess)))
	    (setq best-guess (dll-previous dll best-guess)))

	  best-guess)))))))


;;(defun tin-start-marker (collection tin)
;;  "Return start-position of a cookie in COLLECTION.
;;Args: COLLECTION TIN.
;;The marker that is returned should not be modified in any way,
;;and is only valid until the contents of the cookie buffer changes."
;;
;;  (elib-wrapper->start-marker 
;;   (dll-element (elib-collection->dll collection) tin)))


;;(defun tin-end-marker (collection tin)
;;  "Return end-position of a cookie in COLLECTION.
;;Args: COLLECTION TIN.
;;The marker that is returned should not be modified in any way,
;;and is only valid until the contents of the cookie buffer changes."
;;
;;  (let ((dll (elib-collection->dll collection)))
;;    (elib-wrapper->start-marker
;;     (dll-element dll (dll-next dll tin)))))



(defun collection-refresh (collection)
  "Refresh all cookies in COLLECTION.

The pretty-printer that was specified when the COLLECTION was created
will be called for all cookies in COLLECTION.

Note that tin-invalidate is more efficient if only a small
number of cookies needs to be refreshed."

  (elib-set-buffer-bind-dll-let* collection

      ((header (elib-collection->header collection))
       (footer (elib-collection->footer collection)))

    (let ((buffer-read-only nil))
      (delete-region (elib-wrapper->start-marker
		      (dll-element dll (dll-nth dll 1)))
		     (elib-wrapper->start-marker
		      (dll-element dll footer)))

      (goto-char (elib-wrapper->start-marker
		  (dll-element dll footer)))
    
      (let ((tin (dll-nth dll 1)))
	(while (not (eq tin footer))

	  (set-marker (elib-wrapper->start-marker (dll-element dll tin))
		      (point))
	  (funcall (elib-collection->pretty-printer collection)
		   (elib-wrapper->cookie (dll-element dll tin)))
	  (insert "\n")
	  (setq tin (dll-next dll tin)))))
    
    (set-marker (elib-wrapper->start-marker (dll-element dll footer))
		(point))))


(defun tin-invalidate (collection &rest tins)
  "Refresh some cookies. Args: COLLECTION &rest TINS.
The pretty-printer that for COLLECTION will be called for all TINS."

  (elib-set-buffer-bind-dll collection
    
    (while tins
      (elib-refresh-tin collection (car tins))
      (setq tins (cdr tins)))))


(defun collection-set-goal-column (collection goal)
  "Set goal-column for COLLECTION.
Args: COLLECTION GOAL.
goal-column is made buffer-local.

There will eventually be a better way to specify the cursor position."
  (elib-set-buffer-bind-dll collection 
    (make-local-variable 'goal-column)
    (setq goal-column goal)))


(defun tin-goto-previous (collection pos arg)
  "Move point to the ARGth previous cookie.
Don't move if we are at the first cookie, or if COLLECTION is empty.
Args: COLLECTION POS ARG.
Returns the tin we move to."

  (elib-set-buffer-bind-dll-let* collection
      ((tin (tin-locate
	     collection pos (elib-collection->last-tin collection))))

    (cond
     (tin
      (while (and tin (> arg 0))
	(setq arg (1- arg))
	(setq tin (dll-previous dll tin)))

      ;; Never step above the first cookie.

      (if (null (elib-filter-hf collection tin))
	  (setq tin (dll-nth dll 1)))

      (goto-char
       (elib-wrapper->start-marker
	(dll-element dll tin)))

      (if goal-column
	  (move-to-column goal-column))
      (elib-set-collection->last-tin collection tin)
      tin))))


(defun tin-goto-next (collection pos arg)
  "Move point to the ARGth next cookie.
Don't move if we are at the last cookie.
Args: COLLECTION POS ARG.
Returns the tin."

  ;;Need to do something clever with (current-buffer)...
  ;;Previously, when the buffer was used instead of the collection, this line
  ;;did the trick. No longer so... This is hard to do right! Remember that a
  ;;cookie can contain a collection!
  ;;(interactive (list (current-buffer) (point)
  ;;		     (prefix-numeric-value current-prefix-arg)))

  (elib-set-buffer-bind-dll-let* collection
      ((tin (tin-locate
	     collection pos (elib-collection->last-tin collection))))

    (while (and tin (> arg 0))
      (setq arg (1- arg))
      (setq tin (dll-next dll tin)))

    ;; Never step below the first cookie.

    (if (null (elib-filter-hf collection tin))
	(setq tin (dll-nth dll -2)))

    (goto-char
     (elib-wrapper->start-marker
      (dll-element dll tin)))

    (if goal-column
	(move-to-column goal-column))

    (elib-set-collection->last-tin collection tin)
    tin))


(defun tin-goto (collection tin)
  "Move point to TIN.  Args: COLLECTION TIN."
  (elib-set-buffer-bind-dll collection
    (goto-char
     (elib-wrapper->start-marker
      (dll-element dll tin)))

    (if goal-column
	(move-to-column goal-column))

    (elib-set-collection->last-tin collection tin)))


(defun collection-collect-tin (collection predicate &rest predicate-args)
  "Select cookies from COLLECTION using PREDICATE.
Return a list of all selected tins.

PREDICATE is a function that takes a cookie as its first argument.

The tins on the returned list will appear in the same order as in the
buffer.  You should not rely on in which order PREDICATE is called.

Note that the buffer the COLLECTION is displayed in is current-buffer
when PREDICATE is called.  If PREDICATE must restore current-buffer if
it changes it.

If more than two arguments are given to collection-collect-tin the remaining
arguments will be passed to PREDICATE."

  (elib-set-buffer-bind-dll-let* collection
      ((header (elib-collection->header collection))
       (tin (dll-nth dll -2))
       (result nil))

    ;; Collect the tins, starting at the last one, so that they
    ;; appear in the correct order in the result (which is cons'ed
    ;; together).

    (while (not (eq tin header))

      (if (apply predicate
		 (elib-wrapper->cookie (dll-element dll tin))
		 predicate-args)
	  (setq result (cons tin result)))

      (setq tin (dll-previous dll tin)))
    result))


(defun collection-collect-cookie (collection predicate &rest predicate-args)
  "Select cookies from COLLECTION using PREDICATE.
Return a list of all selected cookies.

PREDICATE is a function that takes a cookie as its first argument.

The cookies on the returned list will appear in the same order as in
the buffer.  You should not rely on in which order PREDICATE is
called.

Note that the buffer the COLLECTION is displayed in is current-buffer
when PREDICATE is called.  If PREDICATE must restore current-buffer if
it changes it.

If more than two arguments are given to collection-collect-cookie the
remaining arguments will be passed to PREDICATE."

  (elib-set-buffer-bind-dll-let* collection
      ((header (elib-collection->header collection))
       (tin (dll-nth dll -2))
       result)

    (while (not (eq tin header))

      (if (apply predicate
		 (elib-wrapper->cookie (dll-element dll tin))
		 predicate-args)
	  (setq result (cons (elib-wrapper->cookie (dll-element dll tin))
			     result)))

      (setq tin (dll-previous dll tin)))
    result))


(defun cookie-sort (collection predicate)
  "Sort the cookies in COLLECTION, stably, comparing elements using PREDICATE.
PREDICATE is called with two cookies, and should return T
if the first cookie is \"less\" than the second.

All cookies will be refreshed when the sort is complete."

  (elib-set-collection->last-tin collection nil)

  (collection-append-cookies
   collection
   (prog1 (sort (collection-list-cookies collection) predicate)
     (collection-clear collection))))


(defun collection-buffer (collection)
  "Return the buffer that is associated with COLLECTION.
Returns nil if the buffer has been deleted."
  (let ((buf (elib-collection->buffer collection)))
    (if (buffer-name buf)
	buf
      nil)))


;;; Local Variables:
;;; eval: (put 'elib-set-buffer-bind-dll 'lisp-indent-hook 1)
;;; eval: (put 'elib-set-buffer-bind-dll-let* 'lisp-indent-hook 2)
;;; End:

;;; cookie.el ends here
