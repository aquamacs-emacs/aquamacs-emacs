;; Aquamacs Editing Helper
;; some editing functions for Aquamacs
 
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-editing.el,v 1.10 2008/10/14 03:48:00 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/


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
 
;; Copyright (C) 2005, 2007 David Reitter





;; these are taken from wikipedia-mode.el by Chong Yidong, Uwe Brauer
;; license: GPL2, 2007-02-13

;; slightly modified by David Reitter for Aquamacs

(defun unfill-region () 
"Undo filling, deleting stand-alone newlines.
Newlines that do not end paragraphs, list entries, etc. in the region
are deleted."
  (interactive)
  (save-excursion
	(narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (re-search-forward ".\\(\n\\)\\([^\n]\\|----\\)" nil t)
      (replace-match " " nil nil nil 1))
    (goto-char (point-min))
    (while (re-search-forward "[^\\.]\\( +\\)" nil t)
      ;; todo: use a negated (sentence-end) here instead.
      (replace-match " " nil nil nil 1)))
  (message "Region unfilled. Stand-alone newlines deleted")
  (widen))
 
(defun fill-paragraph-or-region ()  
  "Fill paragraph or region (if any).
When no region is defined (mark is not active) or
`transient-mark-mode' is off, call `fill-paragraph'.
Otherwise, call `fill-region'."
  (interactive)

  (if (and mark-active transient-mark-mode)
      (call-interactively 'fill-region)
    (call-interactively 'fill-paragraph)))

(defun unfill-paragraph-or-region () ; Version:1.7dr
  "Unfill paragraph or region (if any).
When no region is defined (mark is not active) or
`transient-mark-mode' is off, puts a paragraph (separated by
empty lines) in one (long line). If a region is defined, acts
like `unfill-region'."
  (interactive)

  (if (and mark-active transient-mark-mode)
    (unfill-region)
  (when use-hard-newlines
	;; 	(backward-paragraph 1)
	;; 	(next-line 1)
	(beginning-of-line 1)
	(set-fill-prefix)
	(set (make-local-variable 'use-hard-newlines) nil)
	(set (make-local-variable 'sentence-end-double-space) t)
	(set (make-local-variable 'paragraph-start)
		 "[ ’¡¡	\n]")
	(when  (featurep 'xemacs)	
	  (let ((fill-column (point-max)))
		(fill-paragraph-or-region nil)))
	(unless  (featurep 'xemacs)	
	  (let ((fill-column (point-max)))
		(fill-paragraph nil)))
	(set (make-local-variable 'use-hard-newlines) t)
	(set (make-local-variable 'sentence-end-double-space) nil)
	(set (make-local-variable 'paragraph-start)
		 "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$"))

  (unless use-hard-newlines
	;; 	(backward-paragraph 1)
	;; 	(next-line 1)
	(beginning-of-line 1)
	(set-fill-prefix)
	(set (make-local-variable 'sentence-end-double-space) t)
	(set (make-local-variable 'paragraph-start)
		 "[ ’¡¡	\n]")
	(when  (featurep 'xemacs)	
	  (let ((fill-column (point-max)))
		(fill-paragraph-or-region nil)))
	(unless  (featurep 'xemacs)	
	  (let ((fill-column (point-max)))
		(fill-paragraph nil)))
	(set (make-local-variable 'sentence-end-double-space) nil)
	(set (make-local-variable 'paragraph-start)
		 "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$"))))
 
(defcustom auto-word-wrap-default-function nil
  "Function to call if auto-detection of word wrapping failed.
This serves as the default for word wrapping detection.
Defaults to `turn-on-auto-fill' if nil."
  :group 'Aquamacs
  :group 'fill
  :type '(choice (const nil)  (const turn-on-auto-fill) (const turn-on-word-wrap))
  :version "22.0")

(defalias 'auto-detect-longlines 'auto-detect-wrap)
(defun auto-detect-wrap ()
  "Automatically enable word-wrap or autofill.
The buffer contents are examined to determine whether to use hard
word wrap (autofill) or soft word wrap (word-wrap).  The variable
`auto-word-wrap-default-function' is used to determine the
default in case there is not enough text."
  (interactive)
  ;; (longlines-mode -1) ;; turn it off
  (setq word-wrap nil)
  (toggle-truncate-lines -1)
  ;; calc mean length of lines
  (save-excursion
    (beginning-of-buffer)
    (let ((start-point (point))
	  (count 0)
	  (empty-lines 0)
	  (longlines-count 0)
	  (last-point (point)))
      (while (and (< (point) (point-max)) (< count 200))
	(search-forward "\n" nil 'noerror)
	(let ((ll (- (point) last-point)))
	(if (< ll 2) ;; empty line?
	    (incf empty-lines)
	  (incf count)
	  (if (> ll fill-column)
	      (incf longlines-count)))
	(setq last-point (point))))
      (if (> count 0)
	  (let ((mean-line-length 
		 (/ (- (point) start-point empty-lines) count)))
	    (if (and (< longlines-count 6)
		     (< mean-line-length (* 1.3 fill-column)))
		(auto-fill-mode 1)
	      ;; long lines on average
	      ;;(longlines-mode 1) ;; turn on longlines mode
	      (setq word-wrap t)
	      (if (interactive-p)
		  (message "Soft word wrap auto-enabled."))))
	    (funcall (or auto-word-wrap-default-function 'turn-on-auto-fill))))))
 
(provide 'aquamacs-editing)