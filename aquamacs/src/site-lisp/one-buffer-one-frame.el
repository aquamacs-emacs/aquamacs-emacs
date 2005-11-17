;; one-buffer-one-frame.el
;; Functions to open buffers in their own frames
;;
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: one-buffer-one-frame.el,v 1.16 2005/11/17 23:29:59 davidswelt Exp $
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
 
;; Last change: $Id: one-buffer-one-frame.el,v 1.16 2005/11/17 23:29:59 davidswelt Exp $

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
 
;; DESCRIPTION:
;; 
;; Always open new frames for each new buffer and switch to their frame.
;; In this minor mode, buffers are usually displayed in their own frames.
;; All buffer switching functions available to users will raise the
;; frame that shows the buffer. Deletion of the frame (or window) usually
;; kills the associated buffer, unless the buffer is still shown in another
;; window or it is a special-purpose buffer such as `*Messages*'.
;;
;; To enable, add this to your init file:
;; (one-buffer-one-frame-mode t)




;; CODE


(defvar one-buffer-one-frame-mode-map (make-sparse-keymap))


(defvar obof-backups-initialized nil)

;; temporary definition (workaround for old defcustom in preloaded func)
;; (defun one-buffer-one-frame-mode (&rest a))


(define-minor-mode one-buffer-one-frame-mode
  "Always open new frames for each new buffer and switch to their frame.
In this minor mode, buffers are usually displayed in their own frames.
All buffer switching functions available to users will raise the
frame that shows the buffer. Deletion of the frame (or window) usually
kills the associated buffer, unless the buffer is still shown in another
window or it is a special-purpose buffer such as `*Messages*'. 

This minor mode provides some additional key bindings:

C-x B         `switch-to-buffer-here'
C-x S-left    `prev-buffer-here'
C-x S-right   `next-buffer-here'
C-x C-B       `list-buffers-here'

The mode sets `pop-up-frames', `pop-up-windows', 
`display-buffer-reuse-frames'.

This mode is part of Aquamacs Emacs, http://aquamacs.org."

;; the condition case is because otherwise this won't
;; do it's job. don't know why.
  (condition-case nil
  (if one-buffer-one-frame-mode
      (setq obofm-old-pop-up-frames  pop-up-frames
	    pop-up-frames nil
	    ;; if pop-up-frames is t, even *Completions* buffers
	    ;; will spawn their own frames
	    obofm-old-pop-up-windows pop-up-windows
	    pop-up-windows t
	    obofm-old-display-buffer-reuse-frames display-buffer-reuse-frames
	    display-buffer-reuse-frames t
	    obof-backups-initialized t)
					; else (turning off)
    ;; restore settings
    (if obof-backups-initialized
	(setq    pop-up-frames obofm-old-pop-up-frames
		 pop-up-windows obofm-old-pop-up-windows
		 display-buffer-reuse-frames 
		 obofm-old-display-buffer-reuse-frames)))
  (error nil))

  :group 'Aquamacs
  :global t
  :keymap 'one-buffer-one-frame-mode-map
  :require 'aquamacs-frame-setup)

;; because of the following alias, setting the mode variable will
;; enable most of the mode, but not the keymap.
(defvaralias 'one-buffer-one-frame 'one-buffer-one-frame-mode)

(defun enable-one-buffer-one-frame-mode ()
  "Turn on `one-buffer-one-frame-mode' if `one-buffer-one-frame' is non-nil.
This ensures that the mode is active even if one-buffer-one-frame is set
with `setq' in the user's init file.

This is just a kludge.
To enable `one-buffer-one-frame-mode', call 
(one-buffer-one-frame-mode 1)
To disable `one-buffer-one-frame-mode', call 
(one-buffer-one-frame-mode 0)"
  (one-buffer-one-frame-mode (if one-buffer-one-frame 1 0)))


(add-hook 'after-init-hook 'enable-one-buffer-one-frame-mode 'append)

  
(defvar one-buffer-one-frame-force nil 
  "Enforce one-buffer-one-frame - should be set only temporarily.")
 

(defun open-in-other-frame-p (buf)
  
  (or one-buffer-one-frame-force ;; set by color-theme
      (let ( (bufname (get-bufname buf)))
	(and one-buffer-one-frame 
		(if 
		    (member bufname
			    '(
			      "\*Completions\*" 
			      "\*Apropos\*" 
			      " SPEEDBAR" ; speedbar package opens its own frame
			      "\*Choices\*" ; for ispell
			      "\*Article\*" ; gnus
			      ))
		    nil
		  (or	
		   ;; return t if there is already text in window
		   (> (buffer-size (window-buffer)) 0)
		   ;; return nil if not special-display buffer 
		   (special-display-p (get-bufname (car args)))))))))
 
(defun killable-buffer-p (buf)
  
  (let ( (bufname (get-bufname buf))
	 )
 
   ; (if one-buffer-one-frame
	(if (or (equal "\*Messages\*" bufname) 
	      
		(equal  "\*scratch\*" bufname) 
		(equal  "\*Help\*" bufname) 
	      
		)
	    nil
      
	  t
	  )
      ;; if not one-buffer-one-frame
   ;   t ;;  used to be nil!
;	    )
    )
  )


; init
(setq aquamacs-newly-opened-frames '() )
;; this won't work very well - the buffer names stored
;; here could change in the mean time.
;; storing objects won't work either, because
;; comparing them doesn't work once the buffer is killed

;; only for certain special buffers

(defun switch-to-buffer-here (buffer &optional norecord)
  "Switch to another buffer to be shown in the same window.
Like `switch-to-buffer', except that even in `one-buffer-one-frame-mode'
the current window is switched to the new buffer."
  (interactive "BSwitch to buffer here: ")
  (let ((one-buffer-one-frame nil))
    (switch-to-buffer buffer norecord)))
(defun list-buffers-here (&optional files-only)
  "Display a list of names of existing buffers.
Like `list-buffers', except that even in `one-buffer-one-frame-mode'
the current window is switched to the new buffer."
  (interactive "P")
  (let ((one-buffer-one-frame nil))
    (list-buffers files-only)))

(defun next-buffer-here ()
  "Switch to the next buffer in cyclic order in the same window.
Like `next-buffer', except that even in `one-buffer-one-frame-mode'
the current window is switched to the new buffer."
  (interactive)
  (let ((one-buffer-one-frame nil))
    (next-buffer)))
(defun prev-buffer-here ()
  "Switch to the previous buffer in cyclic order in the same window.
Like `prev-buffer', except that even in `one-buffer-one-frame-mode'
the current window is switched to the new buffer."
  (interactive)
  (let ((one-buffer-one-frame nil))
    (prev-buffer)))

(define-key one-buffer-one-frame-mode-map [(control x) (shift b)] 
  'switch-buffer-here)
(define-key one-buffer-one-frame-mode-map [(control x) (control shift b)] 
  'list-buffers-here)
(define-key one-buffer-one-frame-mode-map [(control x) (shift right)] 
  'next-buffer-here)
(define-key one-buffer-one-frame-mode-map [(control x) (shift left)] 
  'prev-buffer-here)
(define-key one-buffer-one-frame-mode-map [(control x) (control shift right)] 
  'next-buffer-here)
(define-key one-buffer-one-frame-mode-map [(control x) (control shift left)] 
  'prev-buffer-here)


(if window-system
(defadvice switch-to-buffer (around sw-force-other-frame (&rest args) activate compile)
  ;; is buffer shown in a frame?
  (let ((switch t)
	(window-to-select))
    (if one-buffer-one-frame
	(walk-windows
	 (lambda (w)
	   (when (equal (window-buffer w) (get-bufobj (car args)))
	     (setq switch nil)
	     (setq window-to-select w)
	     
	     )
	   ) t t)) ;; t = include-hidden-frame (must be t) 
      
    (if switch
	(if (or (not (visible-frame-list))
		(not (frame-visible-p (selected-frame)))
		(open-in-other-frame-p (car args)))
 
	    (progn
	      (apply #'switch-to-buffer-other-frame args)
	      (setq ad-return-value (current-buffer))
	      ;; store the frame/buffer information
	      (add-to-list 'aquamacs-newly-opened-frames 
			   (cons (selected-window) (buffer-name)))) 
	  ;; else : show in same frame
	  (if (window-dedicated-p (selected-window))
	      (progn 
		(apply #'switch-to-buffer-other-window args)
		(setq ad-return-value (current-buffer)))
	    ;; else: show in same frame
	    ad-do-it))
      ;; else (don't switch, just activate another frame)
      ;; we need to do it here, because raise-frame / select frame are
      ;; ineffective from within walk-windows
      (raise-frame (select-frame (window-frame window-to-select)))
      (select-window window-to-select)
      (setq ad-return-value (current-buffer)))
  (set-mode-specific-theme))))

;; some exception for the speedbar
;; this doesn't work, unfortunately
;; (add-hook 'speedbar-load-hook 
;; 	  (lambda ()
;; 	    (make-local-variable 'one-buffer-one-frame)
;; 	    (setq one-buffer-one-frame nil)
;; 	    )
;; )

;; less elegant, but it works:
(add-hook 'speedbar-load-hook (lambda ()
(defadvice speedbar-find-file 
  (around same-frame (&rest args) protect activate)
  
  (if one-buffer-one-frame 
      (progn
	(setq one-buffer-one-frame nil)
	(unwind-protect
	    ad-do-it
	  (setq one-buffer-one-frame t)
    
	  ))
    ad-do-it))))

 

;; make sure that when a minibuffer is ready to take input, 
;; the appropriate frame is raised (made visible)
;; using minibuffer-auto-raise globally has unpleasant results,
;; with frames losing focus all the time. speedbar doesn't work either.

(if window-system
(add-hook 'minibuffer-setup-hook 
	  (lambda () 
	    (if one-buffer-one-frame
		(raise-frame)))
)
)

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
(if window-system
 (defadvice quit-window (around always-dedicated (&optional kill window) 
				activate)
   (interactive "P")
   (if one-buffer-one-frame
       (let* ((one-buffer-one-frame nil)
	     (win (selected-window))
	     (save (window-dedicated-p win)))
	 (set-window-dedicated-p win t)
	 (ad-set-arg 1 win)
	 ad-do-it
	 (if (window-live-p win)
	     (set-window-dedicated-p win save))
	 )
     ;; else 
     ad-do-it 
     ) ))

 

;; (if window-system
;; (defadvice pop-to-buffer (around always-dedicated (buf &rest args) 
;; 				 protect activate)
;;   "Temporarily make selected window dedicated, "
;;   (if one-buffer-one-frame
;;       (let* ((pop-up-frames t)
;; 	     (pop-up-windows nil)
;; 	   (win (selected-window))
;; 	   (wd (window-dedicated-p win))
;; 	    ) 
;; 	(set-window-dedicated-p win nil)  
;; 	ad-do-it  
;; 	(set-window-dedicated-p win wd)  
;; 	)
;;     ;; else
;;     ad-do-it

;;     )
;;   )
;;  )


(defun aquamacs-display-buffer (&rest args)

       
       (let ((display-buffer-function nil))
	 (if (and
	      one-buffer-one-frame
	      (open-in-other-frame-p (car args))
	      )
	     (let ((pop-up-frames t)
		   ;;(sframe (selected-frame))
		   ;;(swin (selected-window))
		   )
	       (apply (function display-buffer) args)
	       ;; make sure the old frame stays the selected one
	       ;; this would have a more general effect 
	       ;;(select-frame sframe)
	       ;;(select-window swin)

	       )
	   (apply (function display-buffer) args)
	   )
       )
)

(aquamacs-set-defaults 
 '((display-buffer-function aquamacs-display-buffer)))

(defun aquamacs-delete-window (&optional window)
  "Remove WINDOW from the display.  Default is `selected-window'.
If WINDOW is the only one in its frame, then `delete-frame' too,
even if it's the only visible frame."
  (interactive)
  (setq window (or window (selected-window)))
  (select-window window)
  (if (one-window-p t)
      (aquamacs-delete-frame)
    (old-delete-window (selected-window))))
;; old-delete-window is the original emacs delete-window.


(defun delete-window-if-one-buffer-one-frame ()
  (if one-buffer-one-frame
      (delete-window-if-created-for-buffer)))

(defun aquamacs-delete-frame (&optional frame)
  (condition-case nil 
      (delete-frame)
    (error   
	     
     (let ((f (or frame (selected-frame))))
       (make-frame-invisible f t)
       ;; select messages to it gets any input
       (if (find-all-frames-internal (get-buffer "*Messages*"))
	   (select-frame (car (find-all-frames-internal 
			       (get-buffer "*Messages*"))))))))) 

;; delete window when buffer is killed
;; but only do so if aquamacs opened a new frame&window for
;; this buffer (e.g. during switch-to-buffer)

(defun delete-window-if-created-for-buffer (&optional buffer)
  (let ((buf (or buffer (current-buffer))))
    (let ((winlist (find-all-windows-internal buf)))
      (mapc  
       (lambda (win)
	 ;;force deletion if buffer is not killable
	 (delete-window-if-created-for-this-buffer win (buffer-name buf) t)
					; (not (killable-buffer-p buf)))
	 )
       winlist))))
     
(defun delete-window-if-created-for-this-buffer (win buf-name skip-check)
  ;; used by osxkeys, too
  ;; as of now, we're always forcing the deletion of a window if the user requests it.
  ;; 
 
  (let ((elt (car (member (cons win buf-name)
			  aquamacs-newly-opened-frames))))
    (if (and (or (not buf-name) (not (same-window-p buf-name)))
	     (or skip-check elt (window-dedicated-p win) ))
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
	     
	     (let ((f (selected-frame)))
	       (make-frame-invisible f t)
	        
	       (if (find-all-frames-internal (get-buffer "*Messages*"))
		   (select-frame (car (find-all-frames-internal 
				       (get-buffer "*Messages*")))))))))
      ;; else:
      ;; decide not to delete / make invisible
      ;; then switch buffer
      (if (and one-buffer-one-frame (get-buffer "*scratch*"))
	  (let ((one-buffer-one-frame))
	    (switch-to-buffer "*scratch*")
	    )
	  (next-buffer)))))


(if window-system
    (add-hook 'kill-buffer-hook 'delete-window-if-one-buffer-one-frame t)
  )
 
(defun close-current-window-asktosave (&optional force-delete-frame)
  "Delete current buffer, close selected window (and its frame
if `one-buffer-one-frame'. Beforehand, ask to save file if necessary."
  (interactive) 

  (select-frame-set-input-focus (selected-frame))
 
  (let ((wind (selected-window))
	(killable (and (killable-buffer-p (window-buffer))
		       ;; theoretically, we should check if, in case
		       ;; of force-delete-frame all windows display
		       ;; the same buffer, in which case it is
		       ;; killable again.  practically, this situation
		       ;; shouldn't occur to often, so we skip that
		       ;; someone tedious check.

		       (eq (length (find-all-windows-internal 
				    (window-buffer) 
				    'only_visible_ones)) 
			   1))))
					; ask before killing
    (cond ( (and (eq (current-buffer) (window-buffer)) 
					; only if a document is shown
		 killable
		 (eq   (string-match "\\*.*\\*" (buffer-name)) nil)
		 (eq   (string-match " SPEEDBAR" (buffer-name)) nil) 
					; has no minibuffer!
		 )
	    (cond ((buffer-modified-p)
		   (if (progn
			 (unless (minibuffer-window)
			   (setq last-nonmenu-event nil)
			   )
			 (y-or-n-p "Save this buffer to file before closing window? ")
			 )
		       (progn
			 (save-buffer)
			 (message "File saved.")
			 )
		     ;; mark as not modified, so it will be killed for sure
		     (set-buffer-modified-p nil)
		     ))
		  ((message "")))))
  
  
	
    ;; only if not a *special* buffer
    ;; if the buffer is shown in another window , just delete the current win
    (if one-buffer-one-frame
       (let* ((this-buf (window-buffer))
	     (this-buf-name (buffer-name this-buf)))
	(if
	 
	    (if killable 
		(kill-buffer this-buf)    
	      t
	      )
	    ;; else
	    ;; always delete 
	    ;; unless user said "no"
	    (progn
	      (message "") 
	      ;; we don't want a message in the echo area of the next window!
	      (delete-window-if-created-for-this-buffer 
	       wind this-buf-name t) 
	      ))
	  )	
      ;; else not one-buffer=one-frame
      (progn
	(if killable  
	    (kill-buffer (window-buffer wind))   
	  )
	(when (window-live-p wind)
	  (if (or force-delete-frame ;; called via frame closer button
		  (window-dedicated-p wind)
		  )
	      (aquamacs-delete-frame (window-frame wind) ) 
					; delete window/frame, hide if
					; necessary
	    ;; else
	    (progn
	   
	      (select-window wind)
	      (if (one-window-p 'nomini 'only_selected_frame)
		  (if (not killable)
		      ;; if it's not killable, we need to jump to the
		      ;; next buffer
		      (next-buffer)
		    )
		(aquamacs-delete-window wind) ) ) ) ) ) ) ) )

(if window-system
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
	
	(close-current-window-asktosave 'force-delete-window)
	 
	(frame-live-p frame)
	(next-window (selected-window) 'nominibuf frame)
	(not (memq  (frame-first-window frame) delw))
	)
      ) 
    )
  )
)
  

;; pressing q in a view should delete the frame
(aquamacs-set-defaults
 '((view-remove-frame-by-deleting t)))




;; make sure that C-mouse-1 menu acts locally
(if window-system
(defadvice mouse-buffer-menu (around select-buffer-same-frame (&rest args) activate) 
 (let ((one-buffer-one-frame nil))
   ad-do-it
) 
)
)
  


;; as a bugfix, we're redefining this
;; in order to create a new frame if all frames are invisible
(if window-system
(defun fancy-splash-frame ()
  "Return the frame to use for the fancy splash screen.
Returning non-nil does not mean we should necessarily
use the fancy splash screen, but if we do use it,
we put it on this frame."
  (let (chosen-frame)
   
    (mapc  
     (lambda (frame) (if (and (frame-visible-p frame)
			      (not (window-minibuffer-p 
				    (frame-selected-window frame))))
			 (setq chosen-frame frame)))
     ;; list:
     (append (frame-list) (list (selected-frame)))
     ) 
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
))

(if window-system
(defadvice fancy-splash-screens (around modify-frame (&rest args) activate)

  (let ( (default-frame-alist '( (tool-bar-lines . 0) (minibuffer . nil ) ) ) )
    ad-do-it
    )
  (message "") ;; workaround ("wrong argument")
))


;; (defcustom one-buffer-one-frame t
;;   "When non-nil, new buffers open in new frames and are killed with them.
;; When non-nil, open a new frame for each new buffer and switch to that frame
;; when buffer is selected from Buffers menu. When nil, regular buffers are displayed
;; in the same frame and window.

;; This variable is controlled by `one-buffer-one-frame-mode'.
;; Switch on the mode interactively, and only temporarily set this variable
;; to nil from lisp functions to inhibit its functionality.

;; If set during startup (e.g. in `user-init-file'), the mode will turn on."
;;   :type '(radio 
;; 		(const :tag "Open new frames for buffers" t)
;; 		(const :tag "standard Emacs behavior (nil)" nil))
;;   :group 'Aquamacs
;;   :set 'one-buffer-one-frame-mode
;;   :require 'aquamacs-frame-setup)


(provide 'one-buffer-one-frame)