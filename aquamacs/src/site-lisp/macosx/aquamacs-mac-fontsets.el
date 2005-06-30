;; Aquamacs-mac-fontsets

;; specify some default fontsets that should work on the Mac
;; some fonts might not be found - a warning is issues
;; this should eventually be replaced by a complete font selection
;; dialog

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs fonts
 
;; Last change: $Id: aquamacs-mac-fontsets.el,v 1.6 2005/06/30 09:43:52 davidswelt Exp $

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
(setq aquamacs-ring-bell-on-error-saved aquamacs-ring-bell-on-error)
(setq aquamacs-ring-bell-on-error nil)

(defun signal-font-error (arg)
  (unless ignore-font-errors
    (print arg)
    )
  )
;; this requires fontset-add-font-fast
;; as defined by an aquamacs patch to fontset.c

(defun fontset-add-mac-fonts-fast (fontset &optional base-family)
  (if base-family
      (setq base-family (downcase base-family))
    (let ( 
	   (ascii-font
	   (downcase (x-resolve-font-name
		      (fontset-font fontset (charset-id 'ascii))))))
      (setq base-family (aref (x-decompose-font-name ascii-font)
			      xlfd-regexp-family-subnum))))
;;  (if (not (string-match "^fontset-" fontset))
;;      (setq fontset
;;	    (concat "fontset-" (aref (x-decompose-font-name fontset)
;;				     xlfd-regexp-encoding-subnum))))
  (dolist
      (font-encoder
       (nreverse
	(mapcar (lambda (lst)
		  (cons (cons (format (nth 3 lst) base-family) (nth 0 lst))
			(nth 1 lst)))
		mac-font-encoder-list)))
    (let ((font (car font-encoder))
	  (encoder (cdr font-encoder))

	  )
      (map-char-table
       (lambda (key val)
	 (or (null val)
	     (generic-char-p key)
	     (memq (char-charset key)
		   '(ascii eight-bit-control eight-bit-graphic))
	     (set-fontset-font-fast fontset key font)
	     (setq last-key key)
	     (setq last-font font)
	     ))
       (get encoder 'translation-table))))
)
(defun create-fontset-from-mac-roman-font-fast (font &optional resolved-font
						fontset-name)
  "Create a fontset from a Mac roman font FONT.

Optional 1st arg RESOLVED-FONT is a resolved name of FONT.  If
omitted, `x-resolve-font-name' is called to get the resolved name.  At
this time, if FONT is not available, error is signaled.

Optional 2nd arg FONTSET-NAME is a string to be used in
`<CHARSET_ENCODING>' fields of a new fontset name.  If it is omitted,
an appropriate name is generated automatically.

It returns a name of the created fontset.

This is an optimized (fast) and possibly incompatible version for Aquamacs."
  (let ((fontset
	 (create-fontset-from-ascii-font font resolved-font fontset-name)))
    (fontset-add-mac-fonts-fast fontset)
    fontset))

 
(defun create-aquamacs-fontset (maker name weight variety style sizes  &optional fontsetname)
  (message (concat "Defining " (or fontsetname name)))
  (condition-case e
      (dolist (size sizes)
	 
	(create-fontset-from-mac-roman-font-fast
	 (format "-%s-%s*-%s-%s-%s-*-%s-*-*-*-*-*-mac-roman"
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



(create-aquamacs-fontset
 "apple" "monaco" "medium" "r" "normal" '(9 10 11 12 13 14 16 18) "monaco" )
(create-aquamacs-fontset
 "apple" "lucida grande" "medium" "r" "normal" '(9 10 11 12 13 14 16 18) "lucida" )

(create-aquamacs-fontset
 "apple" "lucida sans typewrite" "medium" "r" "normal" '(9 10   12   14) "lucida_typewriter" )

(create-aquamacs-fontset
 "apple" "lucida console" "medium" "r" nil '(11) "lucida_console" )

(create-aquamacs-fontset
 nil "courier" "medium" "r" nil '(11 13) "courier" )
 
(create-aquamacs-fontset
 nil "bitstream vera sans mono" "medium" "r" "normal" '(10 12 14) "vera_mono" )
 

;; (create-fontset-from-mac-roman-font-in-size
  
;;  "-apple-monaco*-medium-r-normal--%s-*-*-*-*-*-mac-roman" 
;;  '(9 10 11 12 13 14 16 18)
;;  "monaco%s")

 

;; (create-fontset-from-mac-roman-font-in-sizes 
;;  "-apple-lucida grande*-medium-r-*-*-%s-*-*-*-*-*-mac-roman" 
;;  '(9 10 11 12 13 14 16 18)
;;  "lucida%s")
 

;; (create-fontset-from-mac-roman-font-in-sizes 
;;  "-apple-lucida sans typewrite*-medium-r-normal-*-%s-*-*-*-*-*-mac-roman" 
;;  '(9 10 12 14)
;;  "lucida_typewrite%s") 


;; (create-fontset-from-mac-roman-font-in-sizes 
      
;;  "-apple-lucida console*-medium-r-*-*-%s-*-*-*-*-*-mac-roman" 
;;  '(11)
;;  "lucida_console%s") 

;; (create-fontset-from-mac-roman-font-in-sizes 
;;  "-*-courier*-medium-r-*-*-%s-*-*-*-*-*-mac-roman" 
   
 ;;  '(11 13)
;;  "courier%s") 

;; (create-fontset-from-mac-roman-font-in-sizes 
;;  "-*-bitstream vera sans mono-medium-r-normal-*-%s-*-*-*-*-*-mac-roman" 
;;  '(10 12 14)
;;  "vera_mono%s") 

   
 
;; want more fonts? 

 
;; want more fonts? 
;; (print-elements-of-list (x-list-fonts "*lucida grande*")) 

 
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

(if (string= "mac" window-system)
    (require 'carbon-font)
  )

(setq aquamacs-ring-bell-on-error aquamacs-ring-bell-on-error-saved)
(provide 'aquamacs-mac-fontsets)