;;; semanticdb-matlab.el --- Semantic database extensions for MATLAB

;;; Copyright (C) 2008 David Engster

;; Author: David Engster <dengste@eml.cc>
;; based heavily on semanticdb-skel.el (C) Eric Ludlam

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


(require 'semanticdb-search)
(require 'working)
(eval-when-compile
  ;; For generic function searching.
  (require 'eieio)
  (require 'eieio-opt)
  )
;;; Code:

;; Put all directories which should be recursively scanned for your
;; personal MATLAB files here.

(defvar semanticdb-matlab-include-paths
  (if (file-exists-p (expand-file-name "~/matlab"))
      (list (expand-file-name "~/matlab") ;; Default location for extra code.
	    )
    ;; Else, no default path.
    nil)
  "Directories which should be scanned for m-files.")

;;; Classes:
(defclass semanticdb-table-matlab (semanticdb-search-results-table)
  ((major-mode :initform matlab-mode)
   )
  "A table for returning search results from MATLAB path.")

(defclass semanticdb-project-database-matlab
  (semanticdb-project-database
   ;; Use SINGLETON if there should be only one copy of this database.
   ;; Do not use this if you need a different copy for different projects.
   ;; eieio-singleton
   )
  ((new-table-class :initform semanticdb-table-matlab
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   )
  "Database representing MATLAB path.")

;; Create the database, and add it to searchable databases for matlab mode.
(defvar-mode-local matlab-mode semanticdb-project-system-databases
  (list
   (semanticdb-project-database-matlab "Matlab"))
  "Search MATLAB path for symbols.")

;; NOTE: Be sure to modify this to the best advantage of your
;;       language.
(defvar-mode-local matlab-mode semanticdb-find-default-throttle
  '(project omniscience)
  "Search project files, then search this omniscience database.
It is not necessary to to system or recursive searching because of
the omniscience database.")

;;; Filename based methods
;;
(defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-matlab))
  "For a MATLAB database, there are no explicit tables.
Create one of our special tables that can act as an intermediary."
  ;; NOTE: This method overrides an accessor for the `tables' slot in
  ;;       a database.  You can either construct your own (like tmp here
  ;;       or you can manage any number of tables.

  ;; We need to return something since there is always the "master table"
  ;; The table can then answer file name type questions.
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (semanticdb-table-matlab "MATLAB system table")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)
      ))
  (call-next-method))

(defmethod semanticdb-file-table ((obj semanticdb-project-database-matlab) filename)
  "From OBJ, return FILENAME's associated table object."
  ;; NOTE: See not for `semanticdb-get-database-tables'.
  (car (semanticdb-get-database-tables obj))
  )

(defmethod semanticdb-get-tags ((table semanticdb-table-matlab ))
  "Return the list of tags belonging to TABLE."
  ;; NOTE: Omniscient databases probably don't want to keep large tabes
  ;;       lolly-gagging about.  Keep internal Emacs tables empty and
  ;;       refer to alternate databases when you need something.
  nil)

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-matlab) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by by `semantic-equivalent-major-modes'
local variable."
  (save-excursion
    (set-buffer buffer)
    (eq (or mode-local-active-mode major-mode) 'matlab-mode)))

(defmethod semanticdb-full-filename ((obj semanticdb-table-matlab))
  "Fetch the full filename that OBJ refers to.
This function is currently a stub."
;; FIXME
;; return filename for object - what should we do with builtin functions?
nil)


;;; Usage
;;
;; Unlike other tables, an omniscent database does not need to
;; be associated with a path.  Use this routine to always add ourselves
;; to a search list.
(define-mode-local-override semanticdb-find-translate-path matlab-mode
  (path brutish)
  "Return a list of semanticdb tables asociated with PATH.
If brutish, do the default action.
If not brutish, do the default action, and append the system
database (if available.)"
  (let ((default
	  ;; When we recurse, disable searching of system databases
	  ;; so that our MATLAB database only shows up once when
	  ;; we append it in this iteration.
	  (let ((semanticdb-search-system-databases nil)
		)
	    (semanticdb-find-translate-path-default path brutish))))
    ;; Don't add anything if BRUTISH is on (it will be added in that fcn)
    ;; or if we aren't supposed to search the system.
    (if (or brutish (not semanticdb-search-system-databases))
	default
      (let ((tables (apply #'append
			   (mapcar
			    (lambda (db) (semanticdb-get-database-tables db))
			    semanticdb-project-system-databases))))
	(append default tables)))))

;;; Search Overrides
;;
;; NOTE WHEN IMPLEMENTING: Be sure to add doc-string updates explaining
;; how your new search routines are implemented.
;;


(defvar semanticdb-matlab-system-files-cache '(nil)
  "Internal cache for system M files.
This variable caches all M files in the directories listed in
`semantic-matlab-system-paths-include' under MATLAB root
directory.  Users can reset this cache using
`semanticdb-matlab-reset-files-cache'")

(defvar semanticdb-matlab-user-files-cache '(nil)
  "Internal cache for user M files.
This variable caches all M files in the directories listed in
`semanticdb-matlab-include-paths'.  Users can reset this cache
using `semanticdb-matlab-reset-files-cache'.")

(defvar semanticdb-matlab-user-class-cache nil
  "Internal cache for user classes.")

(defun semanticdb-matlab-reset-files-cache ()
  "Reset semanticdb-matlab file cache."
  (interactive)
  (setq semanticdb-matlab-user-files-cache '(nil))
  (setq semanticdb-matlab-system-files-cache '(nil)))

(defun semanticdb-matlab-possibly-add-buffer-to-cache ()
  "Add current buffer file name to cache.
This function will add the current buffer file name to
`semanticdb-matlab-user-files-cache' if not already there.  Meant
to be called in local `after-save-hook'."
  (unless (and semanticdb-matlab-user-files-cache
	       (member (buffer-file-name)
		       (cdr semanticdb-matlab-user-files-cache)))
    (setcdr semanticdb-matlab-user-files-cache
	  (append (cdr semanticdb-matlab-user-files-cache)
		  (list (buffer-file-name))))))

;; Make sure newly created MATLAB files get in the user-files-cache
(add-hook 'matlab-mode-hook
	  (lambda ()
	    ;; add buffer-local after-save-hook
	    (add-hook
	     'after-save-hook
	     'semanticdb-matlab-possibly-add-buffer-to-cache t t)))

;; Helper functions

(defun semanticdb-matlab-scan-directories
  (dirs &optional recursive exclude-classes exclude-private)
  "Get list of all m-files in DIRS.
DIRS is a list of directories.  If RECURSIVE, every subdirectory
will be included in the search.  If EXCLUDE-CLASSES, class
directories (beginning with '@') will be skipped.  If
EXCLUDE-PRIVATE, 'private' directories will be skipped."
  (if dirs
      (let ((working-status-dynamic-type 'working-spinner-display)
	    files)
	(dolist (dir dirs)
	  (when (and (boundp 'working-message)
		     working-message)
	    (working-dynamic-status))
	  (let (subdirs)
	    (dolist (cur (directory-files dir t "[^.]" t))
	      (if (file-directory-p cur)
		  (when (and recursive
			     (not (and exclude-classes
				       (string-match ".*/@" cur)))
			     (not (and exclude-private
				       (string-match ".*/private$" cur))))
		    (push cur subdirs))
		(when (string-match "\\.m$" cur)
		  (push cur files))))
	    (when subdirs
	      (setq files
		    (append files
			    (semanticdb-matlab-scan-directories
			     subdirs recursive exclude-classes exclude-private))))))
	files)
    nil))

(defun semanticdb-matlab-cache-files ()
  "Cache user and system MATLAB files if necessary."
  ;; car of *-file-cache variables is used as flag
  (unless (car semanticdb-matlab-system-files-cache)
    (working-status-forms
     "Searching system MATLAB files" "done"
     (setq semanticdb-matlab-system-files-cache
	   (cons t
		 (semanticdb-matlab-scan-directories
		  semantic-matlab-dependency-system-include-path t t t)))))
  (unless (car semanticdb-matlab-user-files-cache)
    (working-status-forms
     "Searching user MATLAB files" "done"
     (setq semanticdb-matlab-user-files-cache
	   (cons t
		 (semanticdb-matlab-scan-directories
		  semanticdb-matlab-include-paths t nil nil))))
    ;; cache user defined old-style classes
    (setq semanticdb-matlab-user-class-cache
	  (semantic-matlab-find-oldstyle-classes
	   (cdr semanticdb-matlab-user-files-cache)))))

(defun semanticdb-matlab-find-name (name &optional type)
  "Find NAME in matlab file names.
If TYPE is 'regex, NAME is a regular expression.
If TYPE is 'prefix, NAME is a prefix."
  (semanticdb-matlab-cache-files)
  (let ((files (append (cdr semanticdb-matlab-system-files-cache)
		       (cdr semanticdb-matlab-user-files-cache)))
	regexp results)
    (cond
     ((eq type 'prefix)
      (setq regexp (format "^%s.*\\.m$" name)))
     ((eq type 'regex)
      (setq regexp (format "%s\\.m$" name)))
     (t
      (setq regexp (format "^%s\\.m" name))))
    (dolist (cur files)
      (when (string-match regexp (file-name-nondirectory cur))
	(push cur results)))
    results))

(define-mode-local-override semantic-ctxt-current-class-list
  matlab-mode (point)
  "Return a list of tag classes that are allowed at point.
If point is nil, the current buffer location is used."
  (cond
   ((looking-at ".+=")
    '(variable type))
   ((looking-back "\\(get\\|set\\)([a-zA-Z_0-9]*")
    '(variable type))
   ((looking-back "\\(get\\|set\\)([a-zA-Z_0-9]+,'[a-zA-Z_0-9]*")
    '(variable))
   ((looking-back "\\.[a-zA-Z_0-9]*")
    '(variable))
   ((looking-at "\\s-*([a-zA-Z_0-9]+,")
    '(function))
   (t
    '(function variable type))))

;; Search functions

(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-matlab) name &optional tags)
  "Find all tags named NAME in TABLE.
Return a list of tags."
  ;; If we have tags, go up.
  (if tags (call-next-method)
    (let (where)
      ;; If MATLAB shell is active, use it.
      (when (and (matlab-shell-active-p)
		 (setq where (matlab-shell-which-fcn name)))
	(when (and (not (file-exists-p (car where)))
		   ;; Sometimes MATLAB builtin functions lie.
		   (string-match "@" (car where)))
	  (setq where
		(list
		 (concat
		  (substring (car where) 0 (match-beginning 0))
		  name ".m")))))
      (unless (car where)
	;; Fall back to home-made database.
	(setq where 
	      (list (car (semanticdb-matlab-find-name name)))))
      (if (car where)
	  (list (car (semanticdb-file-stream (car where))))
	nil))))
      
(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-matlab) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    (let ((files (semanticdb-matlab-find-name regex 'regex)))
      (delq nil
	    (mapcar '(lambda (x)
		       (let ((matlab-vers-on-startup nil))
			 (car (semanticdb-file-stream x))))
		    files)))))

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-matlab) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  ;; If we have tags, go up.
  (if tags (call-next-method)
    ;; first, get completions from home-made database...
    (let ((compdb (semanticdb-matlab-find-name prefix 'prefix))
	  compshell)
      ;; ...and from MATLAB shell, if available
      (when (matlab-shell-active-p)
	(setq compshell
	      (mapcar 
	       (lambda (x)
		 (let ((where (matlab-shell-which-fcn (car x))))
		   ;; correct name for builtin functions
		   (when (and (cdr where)
			      (string-match 
			       "\\(.*\\)/@.*\\(/[A-Za-z_0-9]+\\.m\\)" 
			       (car where)))
		     (setq where
			   (list 
			    (concat (match-string 1 (car where)) 
				    (match-string 2 (car where))))))
		   (list (car where))))
	       (matlab-shell-completion-list prefix)))
	;; combine results
	(mapc
	 (lambda (x)
	   (unless (member x compdb)
	     (setq compdb (append compdb x))))
	 compshell))
      ;; generate tags
      (delq nil
	    (mapcar '(lambda (x)
		       (let ((matlab-vers-on-startup nil))
			 (car (semanticdb-file-stream x))))
		    compdb)))))

(provide 'semanticdb-matlab)

;;; semanticdb-matlab.el ends here
