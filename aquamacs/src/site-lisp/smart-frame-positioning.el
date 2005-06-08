;;; Smart-frame-positioning.el 
;; 
;; Filename: smart-frame-positioning.el
;; Description: Position frames so that they don't overlap
;; Author: David Reitter
;; Maintainer: David Reitter, david.reitter@gmail.com
;; Copyright (C) 2005, David Reitter, all rights reserved.
;; Emacs Version: 22.0
;; Last-Updated: 29-May-2005
;;           By: dr
;;     Update #:
;; Keywords: frames position
;; Compatibility: GNU Emacs 22.x
;; 
;; This file is part of Aquamacs Emacs.
;;
;; to activate:
;; (require 'smart-frame-positioning)
;; (smart-frame-positioning-mode t)
;;
;; To Do:
;; The origin of the display is not necessarily 0. 
;; How to check?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; version 2.
;;
;; In addition, I ask that you acknowledge the original author, as 
;; done in the header of the present file.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smart-position-and-create-frame (&optional parameters) 
 
  (let* ((hasbeenfitted)
	 
	 (oldframe (selected-frame))
	 (newparms (append (list '(visibility . nil)) 
			   (get-mode-specific-theme major-mode)
			   parameters
			   )
		   )
	 (f
	  (funcall smart-frame-positioning-old-frame-creation-function 
		   newparms
		   )
	  )
	 )
     			; first position the frame  
    (if (cdr (assq 'fit-frame newparms)) 
	(when (fboundp 'fit-frame-single-window-forced) 
	  (progn (fit-frame-single-window-forced f)
		 (setq hasbeenfitted t)
		 )
	  )
      )
  
    (setq newpos (find-good-frame-position oldframe f)) 
    (if hasbeenfitted
	(setq newpos (assq-delete-all 'height (assq-delete-all 'width newpos)))
      )
    ; make sure we don't make it visible prematurely
    (setq newpos (assq-delete-all 'visibility newpos))
    (modify-frame-parameters f newpos)
    
    (make-frame-visible f)
    f					; return the frame
    )
  
  )
; modified fit-frame -- don't call show-frame
; make sure the frame is not shown prematurely

; (setq inhibit-fit-frame nil)
 
(defcustom smart-frame-positioning-enforce nil
  "If true and if in smart-frame-positioning-mode, ignore any user-supplied
position in ``default-frame-alist''." 
  :type 'boolean
  :require 'smart-frame-positioning
  :version 22.0
)

(defcustom smart-frame-positioning-margin 20
  "In smart-frame-positioning-mode, place the frames this many
pixels apart if possible." 
  :type 'integer
  :require 'smart-frame-positioning
  :version 22.0
)

(defun frame-total-pixel-height (f)

  (+ 0 (* (frame-char-height f) (tool-bar-lines-needed f))
     (frame-pixel-height f)
     )
)

;(setq  smart-frame-positioning-enforce nil)
; (find-good-frame-position default-frame-alist)
(defun find-good-frame-position ( old-frame new-frame )
					; next-frame-alist is optional
					; we assume default-frame-alist if it is not given
 

  (let ((new-frame-parameters))
    (if (and (not smart-frame-positioning-enforce) 
	     (cdr (assq 'user-position (frame-parameters new-frame))))
	nil ;; just leave the parameters unmodified, if user has set a position
      
      (let ((preassigned (get-frame-position-assigned-to-buffer-name)))
	
	(or
	 ;; ( progn 
	 ;; OS X ensures that frames are not opened outside the visible area of the screen
	 ;; untested for other systems - the following might have to be enabled
	 ;; to guard cases when the available screen size gets smaller
	 ;; (if (> (cdr (assq 'top preassigned)) (- (display-pixel-width) 40)
	 ;;(dolist (pair preassigned)
	 ;;	(assq-set (car pair) (cdr pair) 'new-frame-parameters))
	 ;;new-frame-parameters
	 preassigned ; if preassigned, return it
	 ;;  )
	 ;;else
	  (let
	      ( 
	       ;; eval is necessary, because left can be (+ -1000)
	       ;; which is not an integer!
	       ( y (eval (frame-parameter old-frame 'top)) )
	       ( x (eval (frame-parameter old-frame 'left)) )
	       ( w (frame-pixel-width old-frame) )
	       ( h (frame-total-pixel-height old-frame) )
        
	       (next-w (frame-pixel-width new-frame) )
	       (next-h (frame-total-pixel-height new-frame) )
	       (margin smart-frame-positioning-margin)
       
	       ) 
	    ;; return:
 
	    (unless (frame-visible-p old-frame)
	      ;; if we're given an invisible frame (probably no
	      ;; frame visible then!), assume a sensible standard
	      ;; 3 * margin for y because of menu bar (on OS X)
	      (setq x margin y (* 3 margin) w 0 h 0))
	     
	    (let (
		  (next-x 
		   (if (> (- x margin next-w) 0)
		       (- x margin next-w)
					; if it doesn't fit to the left
		     (if (> (display-pixel-width)  (+ x w margin next-w))
		   
			 (+ x w margin)
		 
		       ; if it doesn't fit to the right
		       ; then position on the "other side" (where current frame is not)
		       (if (or (equal w 0) (equal h 0)  ; invisible?
			       (> (+ x (/ w 2)) (/ (display-pixel-width) 2)))
			   margin ;; left edeg
			 
		       ; or on the right edge 
			 (- (display-pixel-width) next-w margin)
		        )
		       )
	      
		     ) )
	  

					; now determine y position	
		  (next-y nil)
		  )
   
					; we'll try to position the frame somewhere near the original one
	      (dolist (ny (list y (+ y margin) (+ y (* 3 margin)) (+ y (* 5 margin)) 
				(+ y (* 6 margin)) (+ y (* 4 margin))  (+ y (* 2 margin)) 
				(- y margin) (- y (* 3 margin)) (- y (* 5 margin)) 
				(- y (* 6 margin)) (- y (* 4 margin)) (+ y (* 2 margin))))
	      
		(if next-y
		    nil	;; no operation if next-y already found
		  (let ((samerow t))
		    (dolist (f (visible-frame-list) )
		    
		      (if (or (> (abs (- (frame-parameter f 'top) ny)) 10) ; different height
			      (or (> next-x (+ (eval (frame-parameter f 'left)) (frame-parameter f 'width))) ;or no overlap
				  (< (+ next-x next-w) (eval (frame-parameter f 'left))))
			      )
			  nil		; fine
			(setq samerow nil)
			)
		     
		      )
		    (if samerow
			(setq next-y ny)
		      )

		    )
		  )
		)
	      (if next-y
		  ; make sure it's not too low
		  (setq next-y (min next-y (- (display-pixel-height) next-h (* 2 margin) )))
		 (setq next-y margin)) ;; if all else fails

	      (assq-set 'left next-x 'new-frame-parameters)
	      (assq-set 'top next-y 'new-frame-parameters)
	      
	      new-frame-parameters	; return this
	      )
	    )
	  )
	)
      )
    )
  )

(defvar smart-frame-positioning-old-frame-creation-function 
  (setq smart-frame-positioning-old-frame-creation-function 
	frame-creation-function))

(define-minor-mode smart-frame-positioning-mode
  "If enabled, new frames are opened in a convenient position. 
The algorithm tries to avoid overlapping of frames on the display,
and it tries to position them so that they don't leave the screen.
Nota bene: This is not an exact science."
  :init-value nil
  :global t
  :version "22.0"
  :group 'frames
  
  (if smart-frame-positioning-mode
				
					; else
					; turn on
    (progn 
 
      (unless (eq frame-creation-function
		  'smart-position-and-create-frame)
	(setq smart-frame-positioning-old-frame-creation-function frame-creation-function)
	)

      (setq frame-creation-function 'smart-position-and-create-frame)
      )
    
	;else
 
      (setq frame-creation-function smart-frame-positioning-old-frame-creation-function)
      )
  smart-frame-positioning-mode 
  )
        
(defvar smart-frame-prior-positions '()
  "Association list with buffer names and frame positions / sizes, so these
can be remembered. This is part of Aquamacs Emacs."
)
 
(defun store-frame-position-for-buffer (f)
 "Store position of frame F associated with current buffer for later retrieval. (Part of Aquamacs)"

; don't store too many entries here
 (if (> (length smart-frame-prior-positions) 50)
     (setcdr (nthcdr 49 smart-frame-prior-positions) nil)
   ) 

  (assq-set-equal (buffer-name) 
	     ( list 
		    (cons 'left (eval (frame-parameter f 'left)))
		    (cons 'top (eval (frame-parameter f 'top)))
		    (cons 'width (frame-parameter f 'width))
		    (cons 'height (frame-parameter f 'height))
		    ) 
		  
	    'smart-frame-prior-positions)

  )
(defun get-frame-position-assigned-to-buffer-name ()
  (cdr (assq-string-equal (buffer-name) smart-frame-prior-positions))
)


(add-hook 'delete-frame-functions
	  'store-frame-position-for-buffer)


(provide 'smart-frame-positioning)

; (smart-frame-positioning-mode nil)    

