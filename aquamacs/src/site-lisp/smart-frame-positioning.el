;;; Smart-frame-positioning.el 

;; Smart Frame Positioning Mode: Aquamacs frames (because there are
;;  usually many) are opened in useful positions on the screen, so they
;;  don't overlap. Aquamacs associates positions with buffer names, so
;;  that frames displaying the same buffer (and file name) are always
;;  opened in the same position.

;; to activate:
;; (require 'smart-frame-positioning)
;; (smart-frame-positioning-mode t)
;;
;; To Do:
;; The origin of the display is not necessarily 0. 
;; How to check?
;; 
 
;; Emacs Version: 22.0

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: smart-frame-positioning.el,v 1.14 2005/09/19 19:03:51 davidswelt Exp $

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
  :group 'frames
)

(defcustom smart-frame-positioning-margin 20
  "In smart-frame-positioning-mode, place the frames this many
pixels apart if possible." 
  :type 'integer
  :require 'smart-frame-positioning
  :version 22.0
  :group 'frames
)

(defun frame-total-pixel-height (f)

  (+ 0 (* (frame-char-height f) (tool-bar-lines-needed f))
     (frame-pixel-height f)
     )
)
(defun smart-fp--char-to-pixel-width (chars frame)
       (* chars (frame-char-width frame)))
(defun smart-fp--char-to-pixel-height (chars frame)
       (* chars (frame-char-height frame)))
(defun smart-fp--pixel-to-char-width (pixels frame)
       (round (- (/ pixels (frame-char-width frame)) .5)))
(defun smart-fp--pixel-to-char-height (pixels frame)
       (round (- (/ pixels (frame-char-height frame)) .5)))


;; Unit test  / check requirements
(require 'aquamacs-tools)
(aquamacs-require 
 '(fboundp 'mac-display-available-pixel-bounds)
)

;(setq  smart-frame-positioning-enforce nil)
; (find-good-frame-position default-frame-alist)
(defun find-good-frame-position ( old-frame new-frame )
  ;; next-frame-alist is optional
  ;; we assume default-frame-alist if it is not given
 

  (let ((new-frame-parameters))
    (if (and (not smart-frame-positioning-enforce) 
	     (cdr (assq 'user-position (frame-parameters new-frame))))
	nil ;; just leave the parameters unmodified, if user has set a position
      
      (let* (
	     ;; on some systems, we can retrieve the available pixel width with
	     ;; non-standard methods.
	     ;; on OS X, e.g. mac-display-available-pixel-bounds (patch!!) returns
	     ;; available screen region, excluding the Dock.
	       (rect (if (fboundp 'mac-display-available-pixel-bounds)
			 (mac-display-available-pixel-bounds)
		       (list 0 0 
			     (display-pixel-width) (display-pixel-height))))
	       (min-x (+ 5 (nth 0 rect)))
	       (min-y (+ 5 (nth 1 rect)))
	       (max-x (nth 2 rect))
	       (max-y (nth 3 rect))
	       (preassigned (get-frame-position-assigned-to-buffer-name)))
	
	(if preassigned
	 ;; ( progn 
	 ;; OS X ensures that frames are not opened outside the visible 
	    ;; area of the screen
	 ;; untested for other systems - the following might have to be enabled
	 ;; to guard cases when the available screen size gets smaller
	 ;; (if (> (cdr (assq 'top preassigned)) (- (display-pixel-width) 40)
	 ;;(dolist (pair preassigned)
	 ;;	(assq-set (car pair) (cdr pair) 'new-frame-parameters))
	 ;;new-frame-parameters

	 ;; if preassigned, the return it
	    (progn  
	       
	      `(
		(left .
		      ,(max min-x (cdr (assq 'left preassigned))))
		(top 
		 ,(max min-y (cdr (assq 'top preassigned))))
		(width .
		       (smart-fp--pixel-to-char-width
			(min (- max-x (cdr (assq 'left preassigned)))
			     (smart-fp--char-to-pixel-width
				(cdr (assq 'width preassigned))
				new-frame)) 
			new-frame))   
		(height .
			(smart-fp--pixel-to-char-height
			 (min (- max-y  (cdr (assq 'top preassigned)))
			      (smart-fp--char-to-pixel-height
				 (cdr (assq 'height preassigned))
				 new-frame))
			 new-frame))
	    
		)
	      )
 
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
	      (setq x (+ min-x margin)  y (+ min-y margin) w 0 h 0))
	    (let (
		  (next-x 
		   (if (> (- x margin next-w) min-x)
		       (- x margin next-w)
					; if it doesn't fit to the left
		     (if (> max-x  (+ x w margin next-w))
		   
			 (+ x w margin)
		 
		       ;; if it doesn't fit to the right
		       ;; then position on the "other side" 
		       ;; (where current frame is not)
		       (if (or (equal w 0) (equal h 0)  ; invisible?
			       (> (+ x (/ w 2)) (/ max-x 2)))
			   min-x ;; left edeg
			 
		       ; or on the right edge 
			 (- max-x next-w)
		        )
		       )
	      
		     ) )
	  

		  ;; now determine y position	
		  (next-y nil)
		  )
   
	      ;; we'll try to position the frame somewhere near the original one
	      (mapc  
	       (lambda (ny)
		 (if next-y
		     nil ;; no operation if next-y already found
		   (let ((samerow t))
		     (mapc  
		      (lambda (f)  
			(if (or (> (abs (- (eval (frame-parameter f 'top)) ny)) 10) 
				;; different height
				(or (> next-x (+ (eval 
						  (frame-parameter f 'left)) 
						 (frame-parameter f 'width))) 
					;or no overlap
				    (< (+ next-x next-w) 
				       (eval (frame-parameter f 'left))))
				)
			    nil		; fine
			 (setq samerow nil)  
			  ) )
		      ;; list:
		      (visible-frame-list)
		      )
		     (if samerow
			 
			 (setq next-y ny)
		       ) 
		     )
		   ) )
	       ;; list:
	       (list y (+ y margin) (+ y (* 3 margin)) (+ y (* 5 margin)) 
		     (+ y (* 6 margin)) (+ y (* 4 margin))  (+ y (* 2 margin)) 
		     (- y margin) (- y (* 3 margin)) (- y (* 5 margin)) 
		     (- y (* 6 margin)) (- y (* 4 margin)) (+ y (* 2 margin)))
	       )
	      (setq next-x (max next-x min-x ))

	      
	      (if next-y
		  ;; make sure it's not too low
		  ;; the 20 seem to be necessary because of a bug in Emacs
		  (setq next-y (max min-y 
				    (min next-y (- max-y next-h 20))))
		   
		 (setq next-y min-y)) ;; if all else fails
 

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
	(setq smart-frame-positioning-old-frame-creation-function 
	      frame-creation-function)
	)

      (setq frame-creation-function 'smart-position-and-create-frame)

      (add-hook 'delete-frame-functions
		'store-frame-position-for-buffer)

      ;; the first frame should be in a good position

      (let* (
	     ;; on some systems, we can retrieve the available pixel width with
	     ;; non-standard methods.
	     ;; on OS X, e.g. mac-display-available-pixel-bounds (patch!!) returns
	     ;; available screen region, excluding the Dock.
	       (rect (if (fboundp 'mac-display-available-pixel-bounds)
			 (mac-display-available-pixel-bounds)
		       (list 0 0 
			     (display-pixel-width) (display-pixel-height))))
	       (min-x (+ 5 (nth 0 rect)))
	       (min-y (+ 5 (nth 1 rect))))
	(setq initial-frame-alist
	      (append
	       `((top . ,(+ smart-frame-positioning-margin min-y))
		 (left . ,(+ smart-frame-positioning-margin min-x))
		 )
	       initial-frame-alist)))
	)
   
	;else
 
      (setq frame-creation-function 
	    smart-frame-positioning-old-frame-creation-function)
      (remove-hook 'delete-frame-functions
		   'store-frame-position-for-buffer)
      )
  smart-frame-positioning-mode 
  )
        
(defvar smart-frame-prior-positions '()
  "Association list with buffer names and frame positions / sizes, so these
can be remembered. This is part of Aquamacs Emacs."
)
 
(defun store-frame-position-for-buffer (f)
  "Store position of frame F associated with current buffer for later retrieval. (Part of Aquamacs)"
  ;; (setq smart-frame-prior-positions nil)
  ;; don't store too many entries here
  (when buffer-file-number ;;; don't save position if 'untitled'
    (if (> (length smart-frame-prior-positions) 50)
	(setcdr (nthcdr 49 smart-frame-prior-positions) nil)
      ) 

    (assq-set-equal (buffer-name) 
		    ( list 
		      (cons 'left (eval (frame-parameter f 'left)))
		      (cons 'top (eval (frame-parameter f 'top)))
		      (cons 'width (frame-parameter f 'width))
		      (cons 'height (frame-parameter f 'height))) 
		    'smart-frame-prior-positions)))


(defun get-frame-position-assigned-to-buffer-name ()
      (cdr (assq-string-equal (buffer-name) smart-frame-prior-positions)))




(provide 'smart-frame-positioning)

; (smart-frame-positioning-mode nil)    

