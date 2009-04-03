;; Run `aquamacs-setup'

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-aux.el,v 1.3 2008/02/11 08:48:59 davidswelt Exp $ 

;; This file is part of Aquamacs Emacs
;; http://aquamacs.org/

;; Aquamacs Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Aquamacs Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;; Copyright (C) 2008: David Reitter


(require 'aquamacs) ;; usually pre-loaded
(aquamacs-setup)
(defvar aquamacs--aquamacs-aux-file load-file-name)

(defun aquamacs-cleanup-load-history ()
  "Fix functions associations in Aquamacs.
This fixes the links to the source code of functions defined
in `aquamacs-setup'."
;; assoc (as opposed to assoc-string) works fine in this case

  (add-to-list 'load-history 
	       (cons (concat (directory-file-name 
			      (file-name-directory
			       (file-truename aquamacs--aquamacs-aux-file)))
			     "/aquamacs.el")
		     (cdr (assoc aquamacs--aquamacs-aux-file load-history))))
  (setq load-history (assq-delete-all-equal aquamacs--aquamacs-aux-file 
					    load-history)))

(provide 'aquamacs-aux)
