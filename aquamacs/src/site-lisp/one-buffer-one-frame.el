;; one-buffer-one-frame.el
;; Functions to open buffers in their own frames
;;
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: one-buffer-one-frame.el,v 1.1 2005/07/08 21:53:16 davidswelt Exp $
;; This file is part of Aquamacs Emacs
;; http://aquamacs.org/

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

 ;;
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: one-buffer-one-frame.el,v 1.1 2005/07/08 21:53:16 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://aquamacs.org/

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

 


(defun open-in-other-frame-p (buf)
  
 (or one-buffer-one-frame-force	;; set by color-theme
     (let ( (bufname (get-bufname buf))
	    )
   
       (if (and one-buffer-one-frame 
		(> (buffer-size (window-buffer)) 0)
		)
	   (if 
	       (member bufname
		       '(
			 "\*Completions\*" 
			 "\*Apropos\*" 
			 " SPEEDBAR" ; speedbar package opens its own frame
			 "\*Article\*"	; gnus
		      
			 )
		       )
	       nil
	     t 				
   
	     )
					; else --> not one-buffer-one-frame
	 (special-display-p (get-bufname (car args))) ; return nil if not special-display buffer 
	 )
       )
     )
 )

(defun killable-buffer-p (buf)
  
  (let ( (bufname (get-bufname buf))
	 )
 
    (if one-buffer-one-frame
	(if (or (equal "\*Messages\*" bufname) 
	      
		(equal  "\*scratch\*" bufname) 
		(equal  "\*Help\*" bufname) 
	      
		)
	    nil
      
	  t
	  ) nil 
	    )
    )
  )


; init
(setq aquamacs-newly-opened-frames '() )

;; only for certain special buffers

 

(if (string= "mac" window-system)
(defadvice switch-to-buffer (around sw-force-other-frame (&rest args) activate)
    
  
					; is buffer shown in a frame?
  (if (and one-buffer-one-frame
	   (walk-windows
	    (lambda (w)
	      (if (eq (window-buffer w) (get-bufobj (car args)))
	     
		   (make-frame-visible (select-frame (window-frame w)
						    )
				      ) 
		  
		)
	      ) t) ;; include hidden frames
	   )
      t
  
    (if (or (not (visible-frame-list))
	    (not (frame-visible-p (selected-frame)))
	    (open-in-other-frame-p (car args))
	     
	    )
	(if (equal (car args) (buffer-name (current-buffer)))  ; is buffer already current? then make sure it's visible.
	    (raise-frame (selected-frame) )  ; bring to front
	     (progn
	        
	       (apply #'switch-to-buffer-other-frame args)
	  
	       (add-to-list 'aquamacs-newly-opened-frames (cons (selected-window) (current-buffer))) ;; store the frame/buffer information
	       )
	     )
	; else : show in same frame
      (if (window-dedicated-p (selected-window))
        (apply #'switch-to-buffer-other-window args)
					; else: show in same frame
	ad-do-it
	)
	
      )
    )
 
  (set-mode-specific-theme)
 
  )
)


;; make sure that when a minibuffer is ready to take input, 
;; the appropriate frame is raised (made visible)
;; using minibuffer-auto-raise globally has unpleasant results,
;; with frames losing focus all the time. speedbar doesn't work either.
(defun auto-raise-if-all-hidden ()
  (setq minibuffer-auto-raise (not (visible-frame-list))) 
)
(add-hook 'menu-bar-update-hook 'auto-raise-if-all-hidden)


;; we'd like to open new frames for some stuff
   
; one could make h-W just kill the buffer and then handle things here
; however, kill-buffer is called a lot for buffers that are not associated
; with a frame and we would need to make sure that only buffers for
; which a new frame was created will take their dedicated frame with
; them when they are killed!
; maybe the previous force-other-frame should keep track of
; newly opened frames!
 



; quit-window is usually called by some modes when the user enters 'q'
; e.g. in dired. we want to delete the window then.        
 (defadvice quit-window (around always-dedicated (&rest args) activate)
   (interactive)
   (if one-buffer-one-frame
       (let (save (window-dedicated-p (selected-window)))
	 (set-window-dedicated-p (selected-window) t)
	 ad-do-it
	 (set-window-dedicated-p (selected-window) save)
	 )
; else
     ad-do-it
     )
   )


 
;; delete window when buffer is killed
;; but only do so if aquamacs opened a new frame&window for
;; this buffer (e.g. during switch-to-buffer)

(defun delete-window-if-created-for-buffer ()

   (let (
	 (buf (current-buffer))
	 )
     
     (let ((winlist (find-all-windows-internal buf))
	   
	   )
        
       (dolist (win winlist)
	 ; force deletion if buffer is not killable
	 (delete-window-if-created-for-this-buffer win buf t)
	 ; (not (killable-buffer-p buf)))

	 
	
       )
     )
				
   ) 
)
    


(setq pop-up-frames nil)
(setq pop-up-windows t)
(setq display-buffer-reuse-frames t)

(defadvice pop-to-buffer (around always-dedicated (buf &rest args) protect activate) 

  (if one-buffer-one-frame
      (let ((puf pop-up-frames)
	    (sw (selected-window))
	    (wd (window-dedicated-p (selected-window)))
	    )
 
	(setq pop-up-frames (not 
			     (string-match "[ ]*\*(Completions|Apropos)\*" 
					   (get-bufname buf))
				 )
	      )
 
	(set-window-dedicated-p sw nil) 
	ad-do-it
	(set-window-dedicated-p sw wd)
	(setq pop-up-frames puf)

	)
    ;; else
    ad-do-it

    )
  )

; make sure that push-button does not lead to reusing of 

(defun delete-window-if-created-for-this-buffer (win buf force)
  ;; used by osxkeys, too
  ;; as of now, we're always forcing the deletion of a window if the user requests it.
  ;; 
 
  (let ((elt (car (member (cons win buf) aquamacs-newly-opened-frames))))
    (if (or force elt (window-dedicated-p win) )
	(progn
	  ;; remove entry from windows list
	  (if elt
	      (setq aquamacs-newly-opened-frames (delq elt aquamacs-newly-opened-frames))
	    )

	  ;; delete the window (or make the frame invisible)
	  (condition-case nil 
	      (if (window-live-p win)
		  (delete-window win) ;; only get rid of that current window
		)
	    (error   
	     

	     (make-frame-invisible (selected-frame) t) 
	     (if (find-all-frames-internal (get-buffer "*Messages*"))
		 (select-frame (car (find-all-frames-internal (get-buffer "*Messages*"))) 
			       )
	       )

	     ) 
	    )
)
      ;; else:
      ;; decide not to delete / make invisible
      ;; then switch buffer
	  (next-buffer)
	   
	  )
      )
    
  )
(defun delete-window-if-one-buffer-one-frame ()
  (if one-buffer-one-frame
      (delete-window-if-created-for-buffer)
    )
  )
(if (string= "mac" window-system)
    (add-hook 'kill-buffer-hook 'delete-window-if-one-buffer-one-frame t)
  )
 


(defun handle-delete-frame (event)
  "Handle delete-frame events from the X server."
  (interactive "e")
  (let ((frame (posn-window (event-start event)))
	(i 0)
	(delw nil)
	)
    (select-frame frame)
     

    (while 
	(and (frame-first-window frame) 
	(window-live-p (frame-first-window frame))
	(select-window (frame-first-window frame))
	(setq delw (cons (frame-first-window frame) delw))
	(close-current-window-asktosave)
	(frame-live-p frame)
	(next-window (selected-window) 'nominibuf frame)
	(not (memq  (frame-first-window frame) delw))
	)
      ) 
    )
  )

  

;; pressing q in a view should delete the frame
(aquamacs-set-defaults
 '((view-remove-frame-by-deleting t)))




;; make sure that C-mouse-1 menu acts locally
(defadvice mouse-buffer-menu (around select-buffer-same-frame (&rest args) activate) 
 (let ((one-buffer-one-frame nil))
   ad-do-it
) 
)
  


;; as a bugfix, we're redefining this
;; in order to create a new frame if all frames are invisible
(defun fancy-splash-frame ()
  "Return the frame to use for the fancy splash screen.
Returning non-nil does not mean we should necessarily
use the fancy splash screen, but if we do use it,
we put it on this frame."
  (let (chosen-frame)
   
    (dolist (frame (append (frame-list) (list (selected-frame))))
      (if (and (frame-visible-p frame)
	       (not (window-minibuffer-p (frame-selected-window frame))))
	  (setq chosen-frame frame))) 
    (if chosen-frame
	chosen-frame
      
      (or
       ;; make visible
       (select-frame (car (frame-list))) 
       ;; or create a new one
       (make-frame)
       )
      )
    )
)
; no tool-bars, always white 
(defadvice fancy-splash-screens (around modify-frame (&rest args) activate)

  (let ( (default-frame-alist '( (tool-bar-lines . 0) (minibuffer . nil ) ) ) )
    ad-do-it
    )
  (message "") ;; workaround ("wrong argument")
)






(provide 'one-buffer-one-frame)