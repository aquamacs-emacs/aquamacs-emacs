;; a better buffer menu.
;; DOES NOT WORK YET.
;; maybe a patch to msb

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: better-buffer-menu.el,v 1.2 2005/06/09 19:52:49 davidswelt Exp $

;; This file is NOT YET part of Aquamacs Emacs
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
 
;; Copyright (C) 2005, David Reitter


(defun menu-bar-update-buffers (&optional force)
  ;; If user discards the Buffers item, play along.
  (and (lookup-key (current-global-map) [menu-bar buffer])
       (or force (frame-or-buffer-changed-p))
       (let ((buffers (buffer-list))
	     (frames (frame-list))
	     buffers-menu frames-menu)
	 ;; If requested, list only the N most recently selected buffers.
	 (if (and (integerp buffers-menu-max-size)
		  (> buffers-menu-max-size 1))
	     (if (> (length buffers) buffers-menu-max-size)
		 (setcdr (nthcdr buffers-menu-max-size buffers) nil)))

	 ;; Make the menu of buffers proper.
	 (setq buffers-menu
	       (let* ((buffer-list
		       (mapcar 'list buffers))
		      (menu-bar-update-buffers-maxbuf 0)
		      alist)
		 ;; Put into each element of buffer-list
		 ;; the name for actual display,
		 ;; perhaps truncated in the middle.
		 (dolist (buf buffer-list)
		   (let ((name (buffer-name (car buf))))
		     (setcdr buf
			     (if (> (length name) 27)
				 (concat (substring name 0 12)
					 "..."
					 (substring name -12))
			       name))))
		 ;; Compute the maximum length of any name.
		 (dolist (buf buffer-list)
		   (unless (eq ?\  (aref (cdr buf) 0))
		     (setq menu-bar-update-buffers-maxbuf
			   (max menu-bar-update-buffers-maxbuf
				(length (cdr buf))))))
		 ;; Set ALIST to an alist of the form
		 ;; ITEM-STRING . BUFFER
		 (dolist (buf buffer-list)
		   (unless (eq ?\  (aref (cdr buf) 0))
		     (push (menu-bar-update-buffers-1 buf) alist)))
		 ;; Now make the actual list of items, and add
		 ;; some miscellaneous buffer commands to the end.
		 (mapcar (lambda (pair)
			   ;; This is somewhat risque, to use
			   ;; the buffer name itself as the event
			   ;; type to define, but it works.
			   ;; It would not work to use the buffer
			   ;; since a buffer as an event has its
			   ;; own meaning.
			   (nconc (list (buffer-name (cdr pair))
					(car pair)
					(cons nil nil))
				  'menu-bar-select-buffer))
			 (nreverse alist))))

	 ;; Make a Frames menu if we have more than one frame.
	 (when (cdr frames)
	   (let ((frames-menu
		  (cons 'keymap
			(cons "Select Frame"
			      (mapcar
			       (lambda (frame)
				 (nconc
				  (list (frame-parameter frame 'name)
					(frame-parameter frame 'name)
					(cons nil nil))
				  'menu-bar-select-frame))
			       frames)))))
	     ;; Put it after the normal buffers
	     (setq buffers-menu
		   (nconc buffers-menu
			  `((frames-separator "--")
			    (frames menu-item "Frames" ,frames-menu))))))

	 ;; Add in some normal commands at the end of the menu.  We use
	 ;; the copy cached in `menu-bar-buffers-menu-command-entries'
	 ;; if it's been set already.  Note that we can't use constant
	 ;; lists for the menu-entries, because the low-level menu-code
	 ;; modifies them.
	 (unless menu-bar-buffers-menu-command-entries
	   (setq menu-bar-buffers-menu-command-entries
		 (list '(command-separator "--")
		       (list 'next-buffer
			     'menu-item
			     "Next Buffer"
			     'next-buffer
			     :help "Switch to the \"next\" buffer in a cyclic order")
		       (list 'prev-buffer
			     'menu-item
			     "Previous Buffer"
			     'prev-buffer
			     :help "Switch to the \"previous\" buffer in a cyclic order")
		       (list 'select-named-buffer
			     'menu-item
			     "Select Named Buffer..."
			     'switch-to-buffer
			     :help "Prompt for a buffer name, and select that buffer in the current window")
		       (list 'list-all-buffers
			     'menu-item
			     "List All Buffers"
			     'list-buffers
			     :help "Pop up a window listing all emacs buffers"
			     ))))
	 (setq buffers-menu
	       (nconc  menu-bar-buffers-menu-command-entries buffers-menu))

	 (print buffers-menu)
	(setq buffers-menu	    (apply 'append
			   (mapcar (lambda (x)
				     (if (consp x)
					 ;; Add a separator between each
					 ;; part of the unified menu.
					 (cons '(--- "---") (cdr x))))
				   buffers-menu)))


	 (setq buffers-menu (cons 'keymap (cons "Select Buffer" buffers-menu)))
	 (define-key (current-global-map) [menu-bar buffer]
	   ;; Call copy-sequence so the string is not pure.
	   (cons (copy-sequence "Buffers") buffers-menu))))

(print buffers-menu)
)

(provide 'better-buffer-menu)

(msb-mode 1)
 

(menu-bar-update-buffers)