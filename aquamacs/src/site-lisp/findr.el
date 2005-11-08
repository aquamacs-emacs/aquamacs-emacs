;;; findr.el -- Breadth-first file-finding facility for (X)Emacs
;;  Thursday July 30 1999

;;; Aquamacs-Update: http://www.emacswiki.org/cgi-bin/emacs/download/findr.el

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: David Bakhash <cadet@bu.edu>
;; Maintainer: David Bakhash <cadet@bu.edu>
;; Version: 0.7
;; Created: Tue Jul 27 12:49:22 EST 1999
;; Keywords: files

;; This file is not part of emacs or XEmacs.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.


;;; Commentary:

;; This code contains a command, called `findr', which allows you to
;; search for a file breadth-first.  This works on UNIX, Windows, and
;; over the network, using efs and ange-ftp. It's pretty quick, and (at
;; times) is a better and easier alternative to other mechanisms of
;; finding nested files, when you've forgotten where they are.

;; You pass `findr' a regexp, which must match the file you're looking
;; for, and a directory, and then it just does its thing:

;; M-x findr <ENTER> ^my-lib.p[lm]$ <ENTER> c:/ <ENTER>

;; If called interactively, findr will prompt the user for opening the
;; found file(s).  Regardless, it will continue to search, until
;; either the search is complete or the user quits the search.
;; Regardless of the exit (natural or user-invoked), a findr will
;; return a list of found matches.

;; Two other entrypoints let you to act on regexps within the files:
;; `findr-search' to search
;; `findr-query-replace' to replace


;;; Installation:

;; (autoload 'findr "findr" "Find file name." t)
;; (define-key global-map [(meta control S)] 'findr)

;; (autoload 'findr-search "findr" "Find text in files." t)
;; (define-key global-map [(meta control s)] 'findr-search)

;; (autoload 'findr-query-replace "findr" "Replace text in files." t)
;; (define-key global-map [(meta control r)] 'findr-query-replace)


;; Change Log:

;; 0.1: Added prompt to open file, if uses so chooses, following
;;      request and code example from Thomas Plass.
;; 0.2: Made `findr' not stop after the first match, based on the
;;      request by Thomas Plass.
;;      Also, fixed a minor bug where findr was finding additional
;;      files that were not correct matches, based on
;;      `file-relative-name' misuse (I had to add the 2nd arg to it).
;; 0.3: Added a `sit-for' for redisplay reasons.
;;      Modifications as suggested by RMS: e.g. docstring.
;; 0.4  Added `findr-query-replace', courtesy of Dan Nelsen.
;; 0.5  Fixed spelling and minor bug in `findr-query-replace' when
;;      non-byte-compiled.
;; 0.6  http://groups.google.com/groups?selm=cxjhfml4b2c.fsf_-_%40acs5.bu.edu :
;; From: David Bakhash (cadet@bu.edu)
;; Subject: Re: latest version of findr.el (5)
;; This is the only article in this thread
;; View: Original Format
;; Newsgroups: gnu.emacs.sources
;; Date: 1999/07/31
;; Courtesy of Dan Nelsen, this version has search-and-replace capabilities.
;; it's still a bit experimental, so I wouldn't expect too much of it.  But it
;; hasn't been tested yet for friendly behavior.

;; The function `findr-query-replace' wasn't working unless you byte-compile the
;; file.  But, since findr is really designed for speed, that's not a bad thing
;; (i.e. it forces you to byte-compile it).  It's as simple as:

;; M-x byte-compile-file <ENTER> /path/to/findr.el <ENTER>

;; anyhow, I think it should work now.

;; dave

;; ---- the next few versions are from Patrick Anderson
;; 0.7 Added `findr-search', broke `findr'

;; 0.8 fixed 0.7 breakage.


;; Code:

(eval-when-compile
  (require 'cl)
  )

(provide 'findr)

;;;; breadth-first file finder...

(defun* findr (name dir &key (prompt-p (interactive-p)))
  "Search directory DIR breadth-first for files matching regexp NAME.
If PROMPT-P is non-nil, or if called interactively, Prompts for visiting
search result\(s\)."
  (interactive "sFile name \(regexp\): \nDDirectory: ")
  (let ((*dirs* (findr-make-queue))
	*found-files*)
    (labels ((findr-1 
	      (dir)
	      (message "searching %s ..." dir)
	      (let ((files (directory-files dir t "\\w")))
		(loop
		 for file in files
		 for fname = (file-relative-name file dir)
		 when (file-directory-p file)
		 do (findr-enqueue file *dirs*)

		 when (string-match name fname)
		 do

		 ;; Don't return directory names when
		 ;; building list for `tags-query-replace' or `tags-search'
		 ;; 							   (if (and (file-regular-p file)
		 ;; 										  (not prompt-p))
		 ;; 								(push file *found-files*))

		 ;; _never_ return directory names
		 (if (file-regular-p file)
		     (push file *found-files*))

		 (message file)

		 (when
		     (and prompt-p
			  (y-or-n-p (format "Find file %s? " file)))
		   (find-file file)
		   (sit-for 0)		; redisplay hack
		   )))))
      (unwind-protect
	  (progn
	    (findr-enqueue dir *dirs*)
	    (while (findr-queue-contents *dirs*)
	      (findr-1 (findr-dequeue *dirs*)))
	    (message "searching ... done"))
	;; protected:
	(return-from findr (nreverse *found-files*))))))

(defun findr-query-replace (from to name dir)
  "Do `query-replace-regexp' of FROM with TO, on each file found by findr.
If you exit (\\[keyboard-quit] or ESC), you can resume the query replace
with the command \\[tags-loop-continue]."
  (interactive
   ;; I'd love to have the #. reader macro here, and use concat to
   ;; break up this string, but that was problematic.  Hopefully, GNU
   ;; Emacs will make `cl-read.el' a standard package.
   "sReplace text through files: (text regexp): \nsReplace %s by: \nsLook in these files: (files regexp): \nDStart in directory: ")
  (tags-query-replace from to nil '(findr name dir)))


(defun findr-search (regexp files dir)
  "Search through all files listed in tags table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue]."
  (interactive
   "sSearch through files for: (text regexp): \nsLook in these files: (files regexp): \nDStart in directory: ")
  (tags-search regexp '(findr files dir)))


(defun findr-find (files dir)
  "Same as `findr' except file names are put in a compilation buffer."
  (interactive "sFile name \(regexp\): \nDDirectory: ")
  (mapcar 'message (findr files dir))
)

;;;; Queues

(defun findr-make-queue ()
  "Build a new queue, with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)
    q))

(defun findr-enqueue (item q)
  "Insert item at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun findr-dequeue (q)
  "Remove an item from the front of the queue."
  (prog1 (pop (cdr q))
    (when (null (cdr q))
      (setf (car q) q))))

(defsubst findr-queue-contents (q)
  (cdr q))

;;; findr.el ends here
