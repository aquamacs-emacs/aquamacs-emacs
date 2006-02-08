;; Aquamacs-mac-fontsets

;; specify some default fontsets that should work on the Mac
;; some fonts might not be found - a warning is issues
;; this should eventually be replaced by a complete font selection
;; dialog

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs fonts
 
;; Last change: $Id: aquamacs-mac-fontsets.el,v 1.10 2006/02/08 20:47:49 davidswelt Exp $

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
 
;; Copyright (C) 2005, David Reitter

;;; FONT DEFAULTS 

(setq ignore-font-errors t)

(setq aquamacs-ring-bell-on-error-saved 
      (if (boundp 'aquamacs-ring-bell-on-error-flag)
	  aquamacs-ring-bell-on-error-flag
	nil))
(setq aquamacs-ring-bell-on-error-flag nil)

(defun signal-font-error (arg)
  (unless ignore-font-errors
    (print arg)
    )
  )
;; this requires fontset-add-font-fast
;; as defined by an aquamacs patch to fontset.c
  
(defun create-aquamacs-fontset (maker name weight variety style sizes  &optional fontsetname)
  "Create a mac fontset with the given properties (leave nil to under-specify).
SIZES is a list of integers, indicating the desired font sizes in points.
Errors are signalled with ``signal-font-error'', unless ''ignore-font-errors'' 
is non-nil. (This function is part of Aquamacs and subject to change.)"
  (message (concat "Defining fontset: " (or fontsetname name)))
  (condition-case e
      (dolist (size sizes)
	  
	(create-fontset-from-mac-roman-font
	 (format "-%s-%s-%s-%s-%s-*-%s-*-*-*-*-*-mac-roman"
		 (or maker "*")
		 (or name "*")
		 (or weight "*")
		 (or variety "*")
		 (or style "*")
		 size 
		 )
	 nil
	 (concat (or fontsetname name) (int-to-string size))
	 )
	) 
    (error (signal-font-error e)))

)


;; don't do this at the moment
;; carbon-font uses the somewhat dysfunctional
;; create-fontset-from-fontset-spec
;; (and also overwrites any monaco12 definitions)
;(if (string= "mac" window-system)
;    (require 'carbon-font)
;  )


(create-aquamacs-fontset
 "apple" "monaco*" "medium" "r" "normal" '(9 10 11 12 13 14 16 18) "monaco" )
(create-aquamacs-fontset
 "apple" "lucida grande*" "medium" "r" "normal" '(9 10 11 12 13 14 16 18) "lucida" )

(create-aquamacs-fontset
 "apple" "lucida sans typewrite*" "medium" "r" "normal" '(9 10   12   14) "lucida_typewriter" )

(create-aquamacs-fontset
 "apple" "lucida console*" "medium" "r" nil '(11) "lucida_console" )

(create-aquamacs-fontset
 nil "courier*" "medium" "r" nil '(11 13) "courier" )
 
(create-aquamacs-fontset
 nil "bitstream vera sans mono" "medium" "r" "normal" '(10 12 14) "vera_mono" )
 
  
;; want more fonts? 
;; (print-elements-of-list (x-list-fonts "*arial*"))


 
;; interesting thread about fonts:
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2004-01/msg00398.html
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2003-03/msg00436.html



(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (insert (car list)) (insert "\n")
    (setq list (cdr list))))
         
         

;; delete all the other fonts from the menu
;; - because they're not present on the Mac

;; default gets put in autom.
(setq x-fixed-font-alist
      '("--- Font menu" ("Misc" () ))) 

(setq aquamacs-ring-bell-on-error-flag aquamacs-ring-bell-on-error-saved)
(provide 'aquamacs-mac-fontsets)