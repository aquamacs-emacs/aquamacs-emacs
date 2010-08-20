;;; Smart-frame-positioning.el 
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs frames
 
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
 
;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010 David Reitter

;; Smart Frame Positioning Mode: In environments where many frames are
;;  opened, this mode shows them in useful positions on the screen so
;;  they don't overlap. The mode also associates positions with buffer
;;  names, so that frames displaying the same buffer (and file name)
;;  are always opened in the same position.

;; To activate:
;; (require 'smart-frame-positioning)
;; (smart-frame-positioning-mode t)
;;
;; To Do:
;; Multiple screens are currently handled in a simplified fashion.
;; This should be well-behaved in dual-screen setups, with the exception
;; that all screens are assumed to be of the same size.
;; May produce undesirable results on certain triple-screen setups or
;; when screens with very different resolutions are used.
;;  
;; In the Carbon port, the function
;; `mac-display-available-pixel-bounds' returns the available screen
;; coordinates for a screen, and the position of the current
;; window in relation to that one is only guessed.  Via GetDeviceList
;; and GetNextDevice (Quickdraw), all screens can be retrieved.  A
;; better implementation could be done once the Cocoa port is
;; available, so we're not going to invest much time in the Carbon
;; portion here.
;;
;; `mac-display-available-pixel-bounds' could be implemented for different
;; platforms. It should return a list of four ordinates (x y w h), giving the
;; available screen real estate for the main screen.
;; An optional parameter (currently not used) could identify the screen.


(eval-when-compile (require 'aquamacs-macros))


(defcustom save-frame-position-file 
  (convert-standard-filename
   "~/Library/Preferences/Aquamacs Emacs/frame-positions.el")
  "Name of the file that records `smart-frame-prior-positions' value."
 :type 'file
 :require 'smart-frame-positioning
 :version 22.0
 :group 'frames
 :group 'Aquamacs)
 
(defcustom smart-frame-positioning-hook nil
  "Functions to be run before frame creation.
These functions are run in `smart-frame-positioning-mode after
a frame is created and before it is made visible. 
The functions should take one argument, that is, the new frame.
The functions may alter the frame parameters. 
After return from these functions, the above mode will adapt
the frame position of the frame. Height and width, if set 
by any of the hook functions, will normally be preserved." 
  :type 'hook
  :require 'smart-frame-positioning
  :version 22.0
  :group 'frames)


(unless (fboundp 'display-available-pixel-bounds)

  (cond ((eq initial-window-system 'ns)
	 (if (fboundp 'display-usable-bounds)
	     (fset 'display-available-pixel-bounds 
		   'display-usable-bounds)))
	((eq initial-window-system 'mac)
	 (if (fboundp 'mac-display-available-pixel-bounds)
	     (fset 'display-available-pixel-bounds 
		   'mac-display-available-pixel-bounds)))
	(t
	 (if (fboundp 'x-display-usable-bounds)
	     (fset 'display-available-pixel-bounds 
		   'x-display-usable-bounds)))))

(unless (fboundp 'display-available-pixel-bounds)
  (defun display-available-pixel-bounds (&optional frame)
    (list 0 0 
	  (display-pixel-width) (display-pixel-height))))

; (display-available-pixel-bounds)

(defun smart-tool-bar-pixel-height (&optional frame) 
  (if (> (or (frame-parameter frame 'tool-bar-lines) 0) 0)
      (let* ((mode (cond ((boundp 'mac-tool-bar-display-mode)
			 mac-tool-bar-display-mode)
			((boundp 'ns-tool-bar-display-mode)
			 ns-tool-bar-display-mode)
			(t t)))
	     (size (cond ((eq mode 'icons) 40)
			 ((eq mode 'both) 56)
			 ((eq mode 'labels) 20)
			 (t 0))))
	(if (and (boundp 'ns-tool-bar-size-mode)
		 (eq ns-tool-bar-size-mode 'small)
		 (not (eq mode 'labels)))
	    (- size 10)
	  size))
    0))
; (smart-tool-bar-pixel-height nil)
; (frame-total-pixel-height nil)

(defun smart-position-and-create-frame (&optional parameters) 
 "Create a frame in a useful screen position.
May be used in `frame-creation-function' or 
`frame-creation-function-alist'. `smart-frame-positioning-mode' 
should be used as the interface to this function."
 (if smart-frame-positioning-mode
  (let* ((newpos)
	 (oldframe (if (frame-visible-p (selected-frame)) (selected-frame)))
 	 ;; create the frame
	 (f (funcall smart-frame-positioning-old-frame-creation-function
		     (append parameters '((visibility . nil))))))
    (run-hook-with-args 'smart-frame-positioning-hook f)

    (setq newpos (or (smart-fp--get-frame-position-assigned-to-buffer-name (current-buffer))
		     (if oldframe (find-good-frame-position oldframe f))
		     (smart-fp--get-frame-position-assigned-to-buffer-type (current-buffer))))
    (let ((overriding-parms (append parameters default-frame-alist)))
      (mapc (lambda (key)
	      (if (assq key overriding-parms)
		  (assq-set key 
			    (cdr-safe (assq key overriding-parms))
			    'newpos)))
	    '(left top width height)))
    (when (frame-parameter f 'fit-frame)
	;; delete height and width - these parameters
	;; are preserved and will stay untouched
	;; in case the hook changed them.
	;; (unless exceeding screen dimensions)
	(setq newpos 
		(assq-delete-all 
		 'height 
		 (assq-delete-all 'width newpos))))
    ; make sure we don't make it visible prematurely
    (setq newpos (assq-delete-all 'visibility newpos))
    (modify-frame-parameters f newpos)

    ;; stay within the available screen
    (smart-move-frame-inside-screen f)
    (when window-configuration-change-hook
      ;; this is so that longlines-mode recognizes the new window width
      (save-excursion
	(select-window (frame-first-window f))
	(set-buffer (window-buffer (selected-window)))
	  (run-hooks 'window-configuration-change-hook)))
    (unless  (and (assq 'visibility parameters)
		  (eq (cdr (assq 'visibility parameters)) nil))
      (make-frame-visible f))
    f)	; return the frame
  ;; not in s-m-p-mode
  (funcall smart-frame-positioning-old-frame-creation-function
	   parameters)))


 
(defcustom smart-frame-positioning-enforce nil
  "If true and if in smart-frame-positioning-mode, ignore any user-supplied
position in ``default-frame-alist''." 
  :type 'boolean
  :require 'smart-frame-positioning
  :version 22.0
  :group 'frames)

(defcustom smart-frame-positioning-margin 20
  "In smart-frame-positioning-mode, place the frames this many
pixels apart if possible." 
  :type 'integer
  :require 'smart-frame-positioning
  :version 22.0
  :group 'frames)

(defvar smart-fp--frame-title-bar-height 22) ;; how to determine this?



(defun frame-total-pixel-height (f) ;;(frame-pixel-height f))
;; this should use the correct faces for minibuffer/modeline
;; and check for presence of those elements etc etc.
;; it's just an approximation right now.
  (+ smart-fp--frame-title-bar-height ;; this is for the title bar of the window
     (smart-tool-bar-pixel-height f)
     (frame-pixel-height f)))
 ; (frame-total-pixel-height (selected-frame))
; 

(defun smart-fp--char-to-pixel-width (chars frame)
       (* chars (frame-char-width frame)))
(defun smart-fp--char-to-pixel-height (chars frame)
        (* chars (frame-char-height frame)))
(defun smart-fp--pixel-to-char-width (pixels frame &optional round-to-lower)
       (round (- (/ (float pixels) (frame-char-width frame)) 
		 (if round-to-lower .499999 0))))
(defun smart-fp--pixel-to-char-height (pixels frame &optional round-to-lower)
       (round (- (/ (float pixels) (frame-char-height frame)) 
		 (if round-to-lower .499999 0))))

(defvar smart-fp-window-system (or initial-window-system 'ns))
(defun smart-fp--get-frame-creation-function ()
  (if (boundp 'frame-creation-function)
      frame-creation-function
    (if (boundp 'frame-creation-function-alist)
	(cdr (assq smart-fp-window-system frame-creation-function-alist))
      nil)))
(require 'aquamacs-tools)
(defun smart-fp--set-frame-creation-function (fun)
  (if (boundp 'frame-creation-function)
      (setq frame-creation-function fun)
    (if (boundp 'frame-creation-function-alist)
	(assq-set smart-fp-window-system fun 'frame-creation-function-alist))
    nil))

 
(defvar smart-fp--current-direction nil)

(defun find-good-frame-position (old-frame new-frame)
  "Finds a good frame position for a new frame based on the old one's position."
 
  (let ((new-frame-parameters))
    (if (and (not smart-frame-positioning-enforce) 
	     (cdr (assq 'user-position (frame-parameters new-frame))))
	nil ;; just leave the parameters unmodified, if user has set a position
      
      (let* (
	     ;; on some systems, we can retrieve the available pixel width.
	     (rect (or (if (fboundp 'display-available-pixel-bounds)
			   ;; may return nil:
			   (display-available-pixel-bounds old-frame))
		       (list 0 0 
			     (display-pixel-width) (display-pixel-height))))
	     (min-x (+ 5 (nth 0 rect)))
	     (min-y (+ 5 (nth 1 rect)))
	     (max-x (- (+ (nth 0 rect) (nth 2 rect)) 5))
	     (max-y (- (+ (nth 1 rect) (nth 3 rect)) 5)))
	(smart-fp--convert-negative-ordinates
	    (let* ( ;; eval is necessary, because left can be (+ -1000)
		  ;; which is not an integer!
		  ( y (eval (frame-parameter old-frame 'top)) )
		  ( x (eval (frame-parameter old-frame 'left)) )
		  (size-reference-frame 
		   (if (frame-parameter old-frame 'fit-frame)
		       new-frame
		     old-frame))
		  ( w (frame-pixel-width size-reference-frame))
		  ( h (frame-total-pixel-height size-reference-frame) )
        
		  (next-w w ) ;;(frame-pixel-width new-frame) )
		  (next-h h ) ;; (frame-total-pixel-height new-frame) )
		  (margin smart-frame-positioning-margin))

	      (if (frame-full-screen-p old-frame)
		  (let ((ss (frame-parameter old-frame 'fullscreen-saved-state)))
		    (if ss
			(setq x (nth 2 ss) ; left
			      y (nth 1 ss) ; top
			      w (- (nth 4 ss) (nth 2 ss))
			      h (- (nth 3 ss) (nth 1 ss)))
		      ;; backup solution  - shouldn't occur
		      (setq x (or (cdr-safe (assq 'top default-frame-alist)) 40)
			    y (or (cdr-safe (assq 'left default-frame-alist)) 40)
			    w 400 h 600 ;; hack
			    ))
		    (setq next-w w next-h h)))

	      ;; in case the frame is obviously created
	      ;; on another screen
	      ;; these ought to use the full screen dimensions, not
	      ;; the available ones. However, since we don't know the dimensions
	      ;; of the other screen (we only know the main ones), these aren't
	      ;; quite clear
	      (when (< (+ x w) min-x)	; to the left
		(let ((new-max-x min-x))
		  (setq min-x (- min-x max-x))
		  (setq max-x new-max-x)))
	      (when (> x max-x)	;; to the right
		;; there seems to be a screen right to the Dock screen
		;; try to guess the size
		(setq min-x max-x) 
		(setq max-x (* 2 max-x))) ;; crude assumption - 
					; screen size unknown
	      (when (< (+ y h) min-y)	  ; above
		(let ((new-max-y min-y))
		  (setq min-y (- min-y max-y))
		  (setq max-y new-max-y)))
	      (when (> y max-y)	;; below
		(setq min-y max-y)
		(setq max-y (* 2 max-y)))
	      ;; return:
	      (unless (frame-visible-p old-frame)
		;; if we're given an invisible frame (probably no
		;; frame visible then!), assume a sensible standard
		(setq x (+ min-x margin)  y (+ min-y margin) w 0 h 0))
	      (let ((next-y nil)
		    (next-x 
		     (or
		      (let ((n-x nil) (next-direction nil) (count 2))
			(while (and (> count 0) (not next-direction))
			  (setq count (1- count))
			  (cond 
			   ((not (eq 'right smart-fp--current-direction ))
			    (if (> (- x margin next-w) min-x)
				(progn (setq n-x (- x margin next-w))
				       (setq next-direction 'left))
			      (setq smart-fp--current-direction 'right)))
			   ((not (eq 'left smart-fp--current-direction ))
			    (if (> max-x  (+ x w margin next-w))
				(progn (setq n-x (+ x w margin))
				       (setq next-direction 'right))
			      (setq smart-fp--current-direction 'right)))))
			(setq smart-fp--current-direction next-direction)
			n-x)
		      ;; if it doesn't fit to the right or left
		      ;; then position on the "other side" 
		      ;; (where current frame is not)
		      (if (or (equal w 0) (equal h 0) ; invisible?
			      (> (+ x (/ w 2)) (/ max-x 2)))
			  min-x ;; left edge
			;; or on the right edge 
			(- max-x next-w)))))
	    
		;; we'll try to position the frame somewhere near the
		;; original one
		(mapc  
		 (lambda (ny)
		   (if next-y
		       nil ;; no operation if next-y already found
		     (if (< (+ 0 ny 
				 ;smart-fp--frame-title-bar-height
				 (smart-tool-bar-pixel-height new-frame)
				 next-h) max-y)
		     (let ((samerow t))
		       (mapc  
			(lambda (f)  
			  ;; avoid placing frame at similar y as any visible frame
			  (if (or (> (abs (- (eval 
					      (frame-parameter f 'top)) 
					     ny)) 10) 
				  ;; different height
				  (> next-x (+ (eval 
						(frame-parameter f 'left)) 
					       (frame-parameter f 'width))) 
				  ;; or no overlap
				  (< (+ next-x next-w) 
				     (eval (frame-parameter f 'left))))
			      nil	; fine
			    (setq samerow nil)))  
			;; list:
			(visible-frame-list))
		       (if samerow
			   (setq next-y ny))))))
		 ;; list:
		 (list y (+ y margin) (+ y (* 3 margin)) (+ y (* 5 margin)) 
		       (+ y (* 6 margin)) (+ y (* 4 margin))  
		       (+ y (* 2 margin)) 
		       (- y margin) (- y (* 3 margin)) (- y (* 5 margin)) 
		       (- y (* 6 margin)) (- y (* 4 margin)) 
		       (+ y (* 2 margin))))
		(setq next-x (max next-x min-x ))
		(if next-y
		    ;; that hasn't been fixed yet.
		    (setq next-y (max min-y 
				      (min next-y (- max-y next-h))))
		  (setq next-y min-y)) ;; if all else fails
		;; do we need to change the height as well?
		(when (and next-y 
			   (> (+ 0 next-y 
				 ;smart-fp--frame-title-bar-height
				 (smart-tool-bar-pixel-height new-frame)
				 next-h) max-y))
		  (setq next-h (- max-y 
				  next-y 
				  (smart-tool-bar-pixel-height new-frame) 
				 ; smart-fp--frame-title-bar-height
				  0)))
		(assq-set 'left next-x 'new-frame-parameters)
		(assq-set 'top next-y 'new-frame-parameters)
		(assq-set 'width (smart-fp--pixel-to-char-width 
				  (- next-w 32) ;; why needed??
				  new-frame 'round-to-lower)
			  'new-frame-parameters)
		(assq-set 'height  (smart-fp--pixel-to-char-height
				   ;; not sure why this works:
				    (- next-h smart-fp--frame-title-bar-height  
				       (smart-tool-bar-pixel-height new-frame))
				   new-frame 'round-to-lower)
			  'new-frame-parameters)
		;; return this 
		new-frame-parameters)))))))

(defvar smart-frame-positioning-old-frame-creation-function 
	(smart-fp--get-frame-creation-function))

(define-minor-mode smart-frame-positioning-mode
  "If enabled, new frames are opened in a convenient position. 
The algorithm tries to avoid overlapping of frames on the display,
and it tries to position them so that they don't leave the screen.
Nota bene: This is not an exact science.

When frames are deleted, their position (associated with the name of
the buffer shown in their first window) is stored. Frames will be 
re-created at that position later on.

`smart-frame-positioning-enforce', `smart-frame-positioning-margin',
`smart-frame-positioning-hook' and `save-frame-position-file' 
can be customized to configure this mode."

  :init-value nil
  :global t
  :version "22.0"
  :group 'frames
  
  (if smart-frame-positioning-mode
      ;; turn on
    (progn 
 
      (unless (eq (smart-fp--get-frame-creation-function)
		  'smart-position-and-create-frame)
	(setq smart-frame-positioning-old-frame-creation-function 
	      (smart-fp--get-frame-creation-function)))

      (smart-fp--set-frame-creation-function
       'smart-position-and-create-frame)

      (add-hook 'delete-frame-functions
		'smart-fp--store-frame-position-for-buffer)

      (add-hook 'minibuffer-setup-hook 'smart-move-minibuffer-inside-screen)

      ;; the first frame should be in a good position

      ;; (let* ((rect (if (fboundp 'display-available-pixel-bounds)
;; 		       (display-available-pixel-bounds)
;; 		     (list 0 0 
;; 			   (display-pixel-width) (display-pixel-height))))
;; 	     (min-x (+ 5 (nth 0 rect)))
;; 	     (min-y (+ 5 (nth 1 rect))))
;; 	(setq initial-frame-alist
;; 	      (append
;; 	       `((top . ,(+ smart-frame-positioning-margin min-y))
;; 		 (left . ,(+ smart-frame-positioning-margin min-x)))
;; 	       initial-frame-alist)))
      )
   
    ;; else (turning off)
    (smart-fp--set-frame-creation-function
     smart-frame-positioning-old-frame-creation-function)
    (remove-hook 'minibuffer-setup-hook 'smart-move-minibuffer-inside-screen)
    (remove-hook 'delete-frame-functions
		 'smart-fp--store-frame-position-for-buffer))
  smart-frame-positioning-mode)
        
(defun frame-full-screen-p (&optional frame)
  (eq (frame-parameter nil 'fullscreen) 'fullboth))
 

(defvar smart-frame-prior-positions '()
  "Association list with buffer names and frame positions / sizes,
so these can be remembered. This is part of Aquamacs Emacs.")
 
(defvar smart-frame--initial-frame (selected-frame))
(defun smart-fp--store-frame-position-for-buffer (f)
  "Store position of frame F associated with current buffer."

  ;; if this is the first-ever frame (default frame), then
  ;; save it as default for initial-frame-alist which is persistent
  ;; other frames may be preferred in certain situations.

  (when (eq f smart-frame--initial-frame)
    (set-default 'initial-frame-alist
		 (smart-fp--convert-negative-ordinates
		  (if (frame-full-screen-p f)
		      (list 
		       (cons 'fullscreen (frame-parameter f 'fullscreen))
		       (cons 'fullscreen-saved-state 
			     (frame-parameter f 'fullscreen-saved-state)))
		    (list 
		     (cons 'left (eval (frame-parameter f 'left)))
		     (cons 'top (eval (frame-parameter f 'top)))
		     (cons 'width (frame-parameter f 'width))
		     (cons 'height (frame-parameter f 'height))
;		     (cons 'pixel-width (frame-pixel-width f))
;		     (cons 'pixel-height (frame-pixel-height f))
		     )))))
  
  ;; (setq smart-frame-prior-positions nil)
  ;; don't store too many entries here
  (when (and (not (frame-full-screen-p f))
	     (or buffer-file-number 
		 (not (string-match "untitled.*" (buffer-name)))))
    ;; don't save position if 'untitled'
    ;; but do save buffers like *Messages* and *Help*
    (if (> (length smart-frame-prior-positions) 50)
	(setcdr (nthcdr 49 smart-frame-prior-positions) nil))
    (assq-set-equal (buffer-name) 
		    ( list 
		      (cons 'left (eval (frame-parameter f 'left)))
		      (cons 'top (eval (frame-parameter f 'top)))
		      (cons 'width (frame-parameter f 'width))
		      (cons 'height (frame-parameter f 'height))
		      ;;(cons 'pixel-width (frame-pixel-width f))
		      ;;(cons 'pixel-height (frame-pixel-height f))
		      ) 
		    'smart-frame-prior-positions)))

;  (smart-fp--get-initial-frame-position)
(defun smart-fp--get-initial-frame-position (&optional f)
  "Store position of frame F as initial frame position."

  ;; choose the right frame
  (let ((the-f))
    (setq f
	  (cond
	   (f)
	   ;; prefer the size of the original frame
	   ((frame-live-p smart-frame--initial-frame) ;; may be iconified
	    smart-frame--initial-frame)
	   ;; otherwise: exactly one frame visible
	   ((eq (let ((c 0))
		  (mapc (lambda (fr)
			  (when (and  (frame-live-p fr) (eq (frame-visible-p fr) t))
			    (incf c) (setq the-f fr))) (frame-list)) c) 1)
	    the-f)
	   ;; otherwise: take original frame at deletion time
	   ((default-value 'initial-frame-alist))
	   (the-f) ;; the first visible frame
	   (nil)))) ;; give up
  (if f
      (if (listp f)
	  f
	(smart-fp--convert-negative-ordinates
	 (if (frame-full-screen-p f)
	     (list 
	      (cons 'fullscreen (frame-parameter f 'fullscreen))
	      (cons 'fullscreen-saved-state 
		    (frame-parameter f 'fullscreen-saved-state)))
	   (list 
	    (cons 'left (eval (frame-parameter f 'left)))
	    (cons 'top (eval (frame-parameter f 'top)))
	    (cons 'width (frame-parameter f 'width))
	    (cons 'height (frame-parameter f 'height))
	    ;; (cons 'pixel-width (frame-pixel-width f))
	    ;; (cons 'pixel-height (frame-pixel-height f))
	    ))))))

(defvar smart-frame-keep-initial-frame-alist t
"* Initialize `intial-frame-list' after startup from old frame position.
If non-nil, the `intial-frame-list' will default to contain position, height
and width of the frame (or the first frame showing a normal buffer) when
Aquamacs was last terminated.")

;; (smart-frame-set-initial-frame-alist '((width . 100)))
(defun smart-frame-set-initial-frame-alist (frame-parameters)
  "Set values from FRAME-PARAMETERS in `initial-frame-alist' unless already set"
;; this is called after the user's customizations have been read.
;; so we need to take care not to override their customizations.
  (when smart-frame-keep-initial-frame-alist
    (let ((new-initial-frame-alist (if (< emacs-major-version 23) '((visibility . nil))))) ;;  - not in Emacs 23
  
      ;; convert frame parameters
      ;; the new frame probably doesn't have the right 
      ;; font at this time, or tool-bar is unclear, etc.
;;       (when smart-frame--initial-frame
;; 	(when (assq 'pixel-width frame-parameters)
;; 	  (assq-set 'width (smart-fp--pixel-to-char-width 
;; 			    (cdr (assq 'pixel-width frame-parameters))
;; 			    smart-frame--initial-frame)
;; 		    'frame-parameters)
;; 	  (assq-set 'height (smart-fp--pixel-to-char-height 
;; 			    (cdr (assq 'pixel-height frame-parameters))
;; 			    smart-frame--initial-frame)
;; 		    'frame-parameters)))
      ;; if dimensions are set 
      (when (not (or (assq 'height initial-frame-alist) 
		     (assq 'width initial-frame-alist)))
	;; can't use after-init-hook
	(defadvice frame-notice-user-settings (after keep-inside-screen activate)
	  (smart-move-frame-inside-screen smart-frame--initial-frame)))
      (mapc
       (lambda (item)
	 (unless (assq (car item)  initial-frame-alist)
	   (setq new-initial-frame-alist (cons item new-initial-frame-alist))))
       (reverse frame-parameters))
      (setq initial-frame-alist 
	    (append new-initial-frame-alist initial-frame-alist))
      ;; Emacs (or the system?) prevents frames that are off-screen. 
      ;; thus, we don't have to check this here. 
      ;; visibility: ugly workaround for stupid emacs bug 166 ;; FIXME
      ;; set the standard value (so it is customizable correctly)
      (put 'initial-frame-alist 'standard-value `((quote ,frame-parameters))))))

;; modelled after `save-place-alist-to-file'
;; but we're saving a (setq ...) so we can just load the file
;; (smart-fp--save-frame-positions-to-file)
(defun smart-fp--load-frame-positions-from-file ()
  (protect
   (load (expand-file-name save-frame-position-file)
	 'noerror nil 'nosuffix )))

;; (smart-fp--save-frame-positions-to-file)
(defun smart-fp--save-frame-positions-to-file ()
  "Save `smart-frame-prior-positions' to a file.
The file is specified in `smart-frame-position-file'."
  (protect
   (let ((file (expand-file-name save-frame-position-file)))
    (save-excursion
      
      (set-buffer (get-buffer-create " *Saved Positions*"))
      (setq buffer-file-coding-system 'utf-8) ;; avoid asking questions
      (delete-region (point-min) (point-max))
      (princ ";; Saved Frame Positions\n\n" (current-buffer))
      (let ((print-length nil)
            (print-level nil))
	;; for compatibility with older Aquamacs versions,
	;; check (fboundp 'smart-frame-set-initial-frame-alist)
        (print `(if (fboundp 'smart-frame-set-initial-frame-alist)
		    (smart-frame-set-initial-frame-alist 
		     ',(smart-fp--get-initial-frame-position)))
	       (current-buffer))
	(princ "\n\n" (current-buffer))
	(print `(setq smart-frame-prior-positions
		      ',smart-frame-prior-positions)
	       (current-buffer)))
      (condition-case nil
	  ;; Don't use write-file; we don't want this buffer to visit it.
	  (write-region (point-min) (point-max) file)
	(file-error (message "Saving frame positions: Can't write %s" file)))
      (kill-buffer (current-buffer))
      ))))

;; load this after the custom-file
(when (or init-file-user user-init-file)
  (add-hook 'after-init-hook 'smart-fp--load-frame-positions-from-file 'append)
  (add-hook 'kill-emacs-hook 'smart-fp--save-frame-positions-to-file))

;; could assoc be used instead?
(defun assq-string-equal (key alist)
  (catch 'break
    (mapc 
     (lambda (element)
       (if (string-equal (car-safe element) key)
	   (throw 'break element)))
     alist)
    nil ;; not found
    ))

(defun smart-fp--get-frame-position-assigned-to-buffer-name (&optional buffer)
  (smart-fp--convert-negative-ordinates
   (cdr (assq-string-equal (buffer-name buffer) smart-frame-prior-positions))))

(defun smart-fp--get-frame-position-assigned-to-buffer-type (&optional buffer)
  (smart-fp--convert-negative-ordinates
   (let ((type
	  (if (equal " *empty*" (buffer-name buffer)) nil
	    (special-display-p (buffer-name buffer))))
	 (pp smart-frame-prior-positions))
     (while (and pp (not (eq type (special-display-p (caar pp)))))
       (setq pp (cdr pp)))
     (cdr (car pp)))))

(defun smart-fp--convert-negative-ordinates (parms)
  "Converts screen ordinates of the form -x to a list (+ -x).
Returns nil of parms is nil."
  (mapcar (lambda (o)
	    (if (and (integerp (cdr-safe o))
		     (< (cdr o) 0))
		`(,(car o) . (+ ,(cdr o)))
		; else
		o))
	  parms))

(defun smart-minibuffer-inside-screen-p (&optional frame)
  (let* ((frame (or frame (selected-frame)))
	       ;; on some systems, we can retrieve the available pixel width with
	       ;; non-standard methods.
	       ;; on OS X, e.g. display-available-pixel-bounds (patch!!) returns
	       ;; available screen region, excluding the Dock.
	       (minib-window (frame-parameter frame 'minibuffer))
	       (edges (if (windowp minib-window) (window-inside-pixel-edges minib-window)
			(list 0 (- (frame-pixel-height frame) (frame-char-height frame))
			      (frame-pixel-width) (frame-pixel-height frame)))
		      )
	       (left (+ (nth 0 edges) (eval (frame-parameter frame 'left))))
	       (top (+ (nth 1 edges) (eval (frame-parameter frame 'top))
		       smart-fp--frame-title-bar-height (smart-tool-bar-pixel-height frame)))
	       (right (+ (* (nth 2 edges) 0.666) (eval (frame-parameter frame 'left)))) ;; just half the width
	       (bottom (+ (eval (frame-parameter frame 'top)) smart-fp--frame-title-bar-height
			  (smart-tool-bar-pixel-height frame)
			  (nth 3 edges) 
			  ))
	       (bounds  (display-available-pixel-bounds frame)))
	  ;; is the area visible? 
	  ;; we cut a corner here and only check the display that shows the majority of the frame
	  (and bounds
	       (>= left (- (nth 0 bounds) 4))
	       (>= top (nth 1 bounds))
	       (<= right (+ (nth 0 bounds) (nth 2 bounds)))
	       (<= bottom (+ (nth 1 bounds) (nth 3 bounds) 4)))))

; (display-available-pixel-bounds nil)
; (setq frame (selected-frame))
; (smart-move-minibuffer-inside-screen)
(defun smart-move-minibuffer-inside-screen (&optional frame)
  (when (and (frame-parameter (or frame (selected-frame)) 'window-system)
	     (not (frame-parameter frame 'fullscreen)))
    (unless
	(smart-minibuffer-inside-screen-p frame)
      (smart-move-frame-inside-screen frame))))

; (display-available-pixel-bounds (selected-frame))
;; this is a lisp implementation of Carbon's ConstrainWindowToScreen
; (smart-move-frame-inside-screen)
; (setq frame nil)
(defun smart-move-frame-inside-screen (&optional frame vertical-only)
  "Move a frame inside the available screen boundaries. 
The frame specified in FRAME is moved so it is entirely visible on
the screen. The function tries to avoid leaving frames on screen
boundaries.
The function will fail to do its job when the Dock is not displayed
on the main screen, i.e. where the menu is."
  (interactive)
  (when (frame-parameter (or frame (selected-frame)) 'window-system)
    (let* ((frame (or frame (selected-frame)))
	   ;; on some systems, we can retrieve the available pixel width with
	   ;; non-standard methods.
	   ;; on OS X, e.g. display-available-pixel-bounds (patch!!) returns
	   ;; available screen region, excluding the Dock.
	   (rect (or (display-available-pixel-bounds frame)
		     (display-available-pixel-bounds (selected-frame))
		     ; may return nil (e.g. window is entirely off-screen)
		     ; request main screen extent:
		     (display-available-pixel-bounds t)))
	   (min-x (nth 0 rect))
	   (min-y (nth 1 rect))
	   (max-x (+ min-x (nth 2 rect)))
	   (max-y (+ min-y (nth 3 rect)))
	   (next-x (eval (frame-parameter frame 'left)))
	   (next-y (eval (frame-parameter frame 'top)))
	   (next-wc (eval (frame-parameter frame 'width)))
	   (next-hc (eval (frame-parameter frame 'height)))
	   (next-w (frame-pixel-width frame)
					;(smart-fp--char-to-pixel-width next-wc frame)
		   )
	   (next-h (frame-pixel-height frame))
	   (next-h-total (frame-total-pixel-height frame))
	   (w-offset (- next-w (smart-fp--char-to-pixel-width next-wc frame)))
	   (h-offset (- next-h (smart-fp--char-to-pixel-height next-hc frame))))
      (when rect
	(unless vertical-only
	  (modify-frame-parameters 
	   frame
	   (let* ((next-x (max min-x 
			       (min
				(- max-x next-w )
				next-x)))
		  
		  (next-wc  (if (<= next-w (- max-x next-x))
				next-wc
			      (smart-fp--pixel-to-char-width (- max-x next-x) 
							     frame 'round-lower))))
	     (smart-fp--convert-negative-ordinates `((left .
							   ,next-x)
						     
						     (width .
							    ,next-wc)   
						     )))))
	(modify-frame-parameters 
	 frame
	 (let* ((next-y (max min-y 
			     (min 
			      (- max-y next-h-total)	
			      next-y)))
		
		(next-hc (if (<= next-h-total (- max-y next-y ))
			     next-hc
			   (smart-fp--pixel-to-char-height 
			    (- max-y next-y 
			       (smart-tool-bar-pixel-height frame)
			       smart-fp--frame-title-bar-height)
			    frame 'round-lower))))
	   (smart-fp--convert-negative-ordinates 
	    `((top . ,next-y)
	      (height . ,next-hc)))))))))

(require 'fit-frame)
(defun scatter-frames ()
  (interactive)
  (let ((frames (visible-frame-list)))
    (dolist (frame frames)
      (let ((smart-frame-positioning-enforce t) (newpos)
	    (fit-frame-max-width-percent (if (> (length frames) 1) 45 90)))
	(fit-frame frame )
	(setq newpos (find-good-frame-position (selected-frame) frame))
	(when (frame-parameter frame 'fit-frame)
	  ;; delete height and width - these parameters
	  ;; are preserved and will stay untouched
	  ;; in case the hook changed them.
	  ;; (unless exceeding screen dimensions)
	  (setq newpos
		(assq-delete-all 
		 'height 
		 (assq-delete-all 'width newpos))))
	(modify-frame-parameters frame newpos)
	(smart-move-frame-inside-screen frame))
      (select-frame frame))))

;; bug workaround:
;; if the toolbar hasn't been filled,
;; COcoa does not know its height, so expanding it will move
;; the echo area out of the screen
(defadvice ns-toggle-toolbar (around toolbar-enlarge-ensure-inside-screen activate)
  (let ((inside (smart-minibuffer-inside-screen-p)))
    ad-do-it
    (and smart-frame-positioning-mode
	 inside
	 (> (frame-parameter nil 'tool-bar-lines) 0)
	 (smart-move-frame-inside-screen nil t))))


(provide 'smart-frame-positioning) 