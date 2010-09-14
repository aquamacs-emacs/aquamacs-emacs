;; one-buffer-one-frame.el
;; Frame/buffer/window management in Aquamacs including `one-buffer-one-frame-mode'
;;
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; This file is part of Aquamacs Emacs
;; http://aquamacs.org/

;; Aquamacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Aquamacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Aquamacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010 David Reitter
 
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



;; KNOWN ISSUES

;; save-window-excursion does not unwind newly created frames.
;; because we advise switch-to-buffer to open stuff in new frames,
;; we get undesirable behavior, for example in viper-mode, where
;; an extra frame is opened.

;; it might make sense to extend save-window-excursion...

;; doesn't play well with ediff-directories
;; conjecture: ediff divides the frame (into two windows), then calls switch-to-buffer
;; instead of using pop-to-buffer



(require 'aquamacs-tools)  

(defvar one-buffer-one-frame-mode-map (make-sparse-keymap))


(defvar obof-backups-initialized nil)

(define-minor-mode one-buffer-one-frame-mode
  "Always open new frames for each new buffer and switch to their frame.
In this minor mode, buffers are usually displayed in their own frames.
All buffer switching functions available to users will raise the
frame that shows the buffer. Deletion of the frame (or window) usually
kills the associated buffer, unless the buffer is still shown in another
window or it is a special-purpose buffer such as `*Messages*'. 

This minor mode provides some additional key bindings:

C-x B         `switch-to-buffer-here'
C-x S-left    `previous-buffer-here'
C-x S-right   `next-buffer-here'
C-x C-B       `list-buffers-here'

Decisions about showing buffers in separate frames can be influenced
in a number of customization variables. Buffers are, by default,
shown in new frames, unless they are switched to with `switch-to-buffer'
and match an entry in `obof-same-frame-switching-regexps', or they are
popped up with `pop-to-buffer' and match an entry in `obof-same-frame-regexps'
\(typically Emacs always shows these in a new window). 
A matching entry in `obof-other-frame-regexps' will always force buffers to 
be shown in a new frame.

The mode sets `pop-up-frames', `pop-up-windows', 
`display-buffer-reuse-frames'.

This mode is part of Aquamacs Emacs, http://aquamacs.org."

  :group 'Aquamacs
  :global t
  :keymap 'one-buffer-one-frame-mode-map
  :require 'aquamacs-frame-setup

  (if (running-on-a-mac-p)
      (progn
	(if one-buffer-one-frame-mode
	  (setq obofm-old-pop-up-frames  pop-up-frames
		pop-up-frames nil
		;; if pop-up-frames is t, even *Completions* buffers
		;; will spawn their own frames
		obofm-old-pop-up-windows pop-up-windows
		
		;; if this is set to t, we ignore the user's preferenes
		;; and it doesn't lead to good decisions (by default)
		;;  pop-up-windows t
		obofm-old-display-buffer-reuse-frames 
		display-buffer-reuse-frames
		display-buffer-reuse-frames t
		obof-backups-initialized t
		;; pressing q in a view should delete the frame
		view-remove-frame-by-deleting t)
					; else (turning off)
	  ;; restore settings
	  (if obof-backups-initialized
	    (setq    pop-up-frames obofm-old-pop-up-frames
		     ;; pop-up-windows obofm-old-pop-up-windows
		     display-buffer-reuse-frames 
		     obofm-old-display-buffer-reuse-frames)))
	(message "one-buffer-one-frame-mode %sabled." (if one-buffer-one-frame-mode "en" "dis")))
    ;; no window-system available
    (message "one-buffer-one-frame mode won't work without frames.")))


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


  
(defvar one-buffer-one-frame-force nil 
  "Enforce one-buffer-one-frame - should be set only temporarily.")
 
(defvar one-buffer-one-frame-inhibit nil
  "Enforce one-buffer-one-frame - should be set only temporarily.")
 
(defcustom obof-same-window-buffer-names nil
  "Like `same-window-buffer-names', but for `one-buffer-one-frame-mode'.
This may be a list of names of buffers to be shown in the same window,
as in `same-window-buffer-names'.
It may also be set to the symbol `same-window-buffer-names', in which
case the value of `same-window-buffer-names' is used."
  :group 'Aquamacs
  :group 'frames)

(defcustom obof-same-window-regexps nil
  "Like `same-window-regexps', but for `one-buffer-one-frame-mode'.
This may be a list of regexps applying to names of buffers to 
be shown in the same window, as in `same-window-regexps'.
It may also be set to the symbol `same-window-regexps', in which
case the value of `same-window-regexps' is used."
  :group 'Aquamacs
  :group 'frames)

(defcustom obof-same-frame-regexps
  '(
    " SPEEDBAR"
    "\\*.*\\*")
  "Buffers popped up in a new window in `one-buffer-one-frame-mode'.
In `one-buffer-one-frame-mode', if the name of a buffer to be shown matches
one of the regular expressions in this list, it is shown in the same frame,
when shown with `pop-to-buffer' (or `display-buffer' in general). A new window
inside the frame may be opened to show the buffer.
Exceptions are listed in `obof-other-frame-regexps'."
  :group 'Aquamacs
  :group 'frames)
 
(defcustom obof-same-frame-switching-regexps
  '(
    " SPEEDBAR"
    "*About GNU Emacs*")
  "Buffers to switch to in the same frame in `one-buffer-one-frame-mode'.
In `one-buffer-one-frame-mode', if the name of a buffer to be shown matches
one of the regular expressions in this list, it is shown in the same frame 
when switched to with `switch-to-buffer'. 
Exceptions are listed in `obof-other-frame-regexps'."
  :group 'Aquamacs
  :group 'frames)

(defcustom obof-other-frame-regexps
  '(
    "\\*Messages\\*" 
    "\\*scratch\\*" 
    "\\*Help\\*"
    "\\*Custom.*\\*"
    ".*output\\*"
    "\\*mail\\*"
    "\\*grep\\*"  
    "\\*shell\\*"   
    "\\*Faces\\*"
    "\\*Colors\\*")
  "Buffers always shown in a separate frame in `one-buffer-one-frame-mode'.
In `one-buffer-one-frame-mode', if the name of a buffer to be shown matches
one of the regular expressions in this list, it is shown in a separate frame.
This overrides entries in `obof-same-frame-regexps'."  
  :group 'Aquamacs
  :group 'frames)

(defun obof-same-frame-p (buf &optional switching)
  (let ((from-buf 
	 (and last-command-event
	      (listp (event-start last-command-event))
	      (let ((clicked-in-window (posn-window 
					(event-start last-command-event))))
		(and (window-live-p clicked-in-window)
		     (window-buffer clicked-in-window))))))
    (with-current-buffer (or from-buf (current-buffer))
	  (or (not one-buffer-one-frame-mode)
	      (let ( (bufname (get-bufname buf)))
		(if one-buffer-one-frame-force ;; set by color-theme
		    nil
		  (or
		   ;; when called, the current buffer is already the new buffer
		   ;; so check the event
		   one-buffer-one-frame-inhibit
		   (and
		    (or (= (buffer-size (window-buffer)) 0)
		    (let ((same-window-buffer-names nil)
			  (same-window-regexps 
			   (if switching
			       obof-same-frame-switching-regexps
			     obof-same-frame-regexps)))
		      ;; this is a fast solution
		      (same-window-p bufname)))
		    (not (let ((same-window-buffer-names nil)
			       (same-window-regexps obof-other-frame-regexps))
			   ;; this is a fast solution
			   (same-window-p bufname)))))))))))

(defun obof-inhibit-frame-creation () 
  "Inhibit creation of extra frames resulting from clicks here."
  (when one-buffer-one-frame-mode
      (set (make-local-variable 'one-buffer-one-frame-inhibit)
	   t)))

(defun obof-inhibit-pop-up-windows ()
  (when one-buffer-one-frame-mode
    ;;(set (make-local-variable 'one-buffer-one-frame-inhibit)	   t) ;; no need?
    (set (make-local-variable 'obof-same-frame-regexps)	   `("\\`\\*Customiz.*\\*\\'" . ,obof-same-frame-regexps ))
    (set (make-local-variable 'same-window-regexps) `("\\`\\*Customiz.*\\*\\'" . ,same-window-regexps))
    (set (make-local-variable 'obof-same-window-regexps) 'same-window-regexps)
    ;;     (set (make-local-variable 'display-buffer-function) 'display-buffer-in-same-window) ;; strong. not needed.
    (set (make-local-variable 'pop-up-windows ) nil) ;; this works for dired
    ;; this doesn't work very well
    ;; because it isn't called from the target frame!
    ;; and because it is too sticky
    ;;     (make-variable-frame-local 'pop-up-windows)
    ;;     (set-frame-parameter nil  'pop-up-windows nil)
    ))

;; (assq ' pop-up-windows (frame-parameters nil))
;; pop-up-windows one-buffer-one-frame-inhibit
;; one-buffer-one-frame
;; Todo:
;; make this a patch
;; (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)

(defun dired-mouse-find-file (event)
  "In Dired, visit the file or directory name you click on."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
	    pos (posn-point (event-end event)))
      (if (not (windowp window))
	  (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    (if (file-directory-p file)
	(or (and (cdr dired-subdir-alist)
		 (dired-goto-subdir file))
	    (progn
	      (select-window window)
	      (dired file)))
      (select-window window)
      (find-file (file-name-sans-versions file t)))))

 ;; this will cause newly opened files to show up in the dired buffer
;(defvar dired-mode-hook nil)
; to do: this should really only apply to files opened
; from within the dired buffer.
 (add-hook 'dired-mode-hook 'obof-inhibit-frame-creation)
 (add-hook 'dired-mode-hook 'obof-inhibit-pop-up-windows)

; customization buffers
;; (setq custom-mode-hook nil)
;; don't do this - would apply to newly opened buffers, too
;; (add-hook 'custom-mode-hook 'obof-inhibit-frame-creation)
;; (add-hook 'custom-mode-hook 'obof-inhibit-pop-up-windows)
(aquamacs-set-defaults
 '((custom-buffer-done-kill t)))


(defun open-in-other-frame-p (buf &optional switching)
  (not (obof-same-frame-p buf switching)))
 
(defun killable-buffer-p (buf)
  "Returns non-nil if buffer BUF can be killed."
  (let ( (bufname (get-bufname buf)))
	(if (or (equal "\*Messages\*" bufname) 
		(equal  "\*scratch\*" bufname) 
		(equal  "\*Help\*" bufname) 
		)
	    nil
	  t)))


; init
(setq aquamacs-newly-opened-windows '() )
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

(defun previous-buffer-here ()
  "Switch to the previous buffer in cyclic order in the same window.
Like `previous-buffer', except that even in `one-buffer-one-frame-mode'
the current window is switched to the new buffer."
  (interactive)
  (let ((one-buffer-one-frame nil))
    (previous-buffer)))

(define-key one-buffer-one-frame-mode-map [(control x) (shift b)] 
  'switch-to-buffer-here)

(define-key one-buffer-one-frame-mode-map [?\C-x B] 
  'switch-to-buffer-here)



(define-key one-buffer-one-frame-mode-map [(control x) (control shift b)] 
  'list-buffers-here)
(define-key one-buffer-one-frame-mode-map [(control x) (shift right)] 
  'next-buffer-here)
(define-key one-buffer-one-frame-mode-map [(control x) (shift left)] 
  'previous-buffer-here)
(define-key one-buffer-one-frame-mode-map [(control x) (control shift right)] 
  'next-buffer-here)
(define-key one-buffer-one-frame-mode-map [(control x) (control shift left)] 
  'previous-buffer-here)


(if (running-on-a-mac-p)
    (defadvice switch-to-buffer (around sw-force-other-frame (&rest args) 
					activate compile)
      (clear-image-cache t) ;; ugly workaround for image cache corruption problem 
                            ;; (where a toolbar icon is shown in place of every image)
      (if one-buffer-one-frame  
	  ;; technically, code below should work even without this
	  ;; "if", because it does mostly the same things as switch-to-buffer.
	  ;; however, we want to be on the safe side, and also not
	  ;; honor stuff like `obof-same-window-regexps' when obof is off.
	  (let ((norecord (nth 1 args))
		(switch t)
		(window-to-select))
	    (walk-windows
	     (lambda (w)
	       (when (equal (window-buffer w) (get-bufobj (car args)))
		 (setq switch nil)
		 (setq window-to-select w))) t t) ;) ;; t = include-hidden-frame (must be t) 
	    (if switch
		(let ((same-window-regexps 
		       (if (eq obof-same-window-regexps 'same-window-regexps)
			   same-window-regexps
			 obof-same-window-regexps))
		      (same-window-buffer-names 
		       (if (eq obof-same-window-buffer-names 'same-window-buffer-names)
			   same-window-buffer-names
			 obof-same-window-buffer-names)))
		  (if (or (not (visible-frame-list))
			  (not (frame-visible-p (selected-frame)))
			  (open-in-other-frame-p (car args) t))
		      (progn
			(setq ad-return-value (apply #'switch-to-buffer-other-frame args))
			;; store the frame/buffer information
			(add-to-list 'aquamacs-newly-opened-windows 
				     (cons (selected-window) (buffer-name)))) 
		    ;; else : show in same frame
		    (if (window-dedicated-p (selected-window))
			(setq ad-return-value   (apply #'switch-to-buffer-other-window args))
		      ;; else: show in same frame
		      ad-do-it)))
	      ;; else (don't switch, just activcate another frame)
	      ;; we need to do it here, because raise-frame / select frame are
	      ;; ineffective from within walk-windows
	      (when window-to-select
		(select-frame-set-input-focus (window-frame window-to-select))
		;; raise-frame doesn't select it  
		(select-window window-to-select norecord)
		;; normally, the following would only happen in 
		;; the next top-level event loop (assumption)
		;; but because the normal switch-to-buffer does it right away
		;; we should do it manually.
		(set-buffer (window-buffer window-to-select)))
	      (unless ad-return-value (setq ad-return-value (current-buffer)))))
	;; else: not one-buffer-one-frame   
   	(setq ad-return-value 
	      ad-do-it)
	(unless (frame-visible-p (selected-frame))
	  ;; make sure we don't make *empty* visible
	  (if (not (string= (substring (get-bufname (car args)) 0 1) " "))
	      (make-frame-visible (selected-frame)))))))

;; (select-window wts)

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

(if (running-on-a-mac-p)
    (add-hook 'minibuffer-setup-hook 
	      (defun maybe-raise-frame () 
		(unless one-buffer-one-frame
		  (raise-frame)))))

;; we'd like to open new frames for some stuff
   
; one could make h-W just kill the buffer and then handle things here
; however, kill-buffer is called a lot for buffers that are not associated
; with a frame and we would need to make sure that only buffers for
; which a new frame was created will take their dedicated frame with
; them when they are killed!
; maybe the previous force-other-frame should keep track of
; newly opened frames!
  
;; ; quit-window is usually called by some modes when the user enters 'q'
;; ; e.g. in dired. we want to delete the window then.  
;; advising quit-window like this will cause some things to fail, e.g.
;; SLIME will switch buffers back in the wrong window after 'q' deletes
;; the window
;; (defadvice quit-window (around always-dedicated (&optional kill window) 
;; 				activate protect)
;;    (interactive "P")
;;    (if (and one-buffer-one-frame
;; 	    (or (not tabbar-mode)
;; 		(< (length (tabbar-tabs (tabbar-current-tabset))) 2)))
;;        (let* ((one-buffer-one-frame nil)
;; 	     (win (or window (selected-window)))
;; 	     (save (window-dedicated-p win)))
;; 	 (set-window-dedicated-p win t)
;; 	 (ad-set-arg 1 win)
;; 	 ad-do-it
;; 	 (if (window-live-p win)
;; 	     (set-window-dedicated-p win save))
;; 	 )
;;      ;; else 
;;      ad-do-it 
;;      ))

 (when (running-on-a-mac-p)

; quit-window is usually called by some modes when the user enters 'q'
; e.g. in dired. we want to delete the window then.  
; (ad-disable-advice 'bury-buffer 'around 'always-dedicated)
 (defadvice bury-buffer (around maybe-delete-window (&optional buffer) 
				activate protect)
   (let ((the-buffer (current-buffer)))

     ad-do-it

   ;; from the documentation of bury-buffer:
   ;; Also, if buffer is nil or omitted, remove the current buffer from the
   ;; selected window if it is displayed there.

     (if (and one-buffer-one-frame
	      (null buffer)) ;; only if nil
       ;; delete the frame if necessary
       ;; only delete a whole frame with only the window in it
       ;; because extra windows are usually created with pop-to-buffer etc.
       ;; so a package expects them to exist to do something with them.
       ;; if a frame was created, however, this heuristic doesn't work out
       ;; seems to work with SLIME like this...

	 (delete-window-if-created-for-buffer the-buffer 'only-frame)))))

(defun aquamacs-display-buffer (buffer &optional not-this-window frame)
  (if one-buffer-one-frame-mode
      (let ((same-window-regexps 
	     (if (eq obof-same-window-regexps 'same-window-regexps)
		 same-window-regexps
	       obof-same-window-regexps))
	    (same-window-buffer-names 
	     (if (eq obof-same-window-buffer-names 'same-window-buffer-names)
		 same-window-buffer-names
	       obof-same-window-buffer-names))
	    (display-buffer-function nil))
	(if (and
	     one-buffer-one-frame
	     (open-in-other-frame-p buffer))
	    (let ((pop-up-frames t) ;; open in a new frame!
		  (sframe (selected-frame))
		  (swin (selected-window)))
	      ;; (message "Pop-up-frames is %s" pop-up-frames)
	      (let ((ret 
		     (display-buffer buffer not-this-window frame)))
		;; make sure the old frame stays the selected one
		;; this is to maintain compatibility with opening
		;; a new window inside the frame, where the input focus
		;; stays in the original window.
		 
		;; dr 12/2006 - we'll try this again, because
		;; that's the way display-buffer is supposed to work.
		  
		(unless (eq display-buffer-reuse-frames 'select)
		  ;; we can't use select-frame-set-input-focus because
		  ;; that would raise the (main) frame over the newly
		  ;; opened one, and we don't want that.
		  (select-frame sframe)
		  (cond ((memq window-system '(x mac ns))
			 (x-focus-frame sframe))
			((eq window-system 'w32)
			 (w32-focus-frame sframe)))
		  (select-window swin)) 
		ret))
	  (display-buffer buffer not-this-window frame)))
    ;;    (if (and nil tabbar-mode (not not-this-window))
    ;; display-buffer: "Make buffer appear in some window but don't select it."
    ;; so perhaps this is incompatible, because we need to guarantee that the buffer is visible, but not selected
    ;; that's not really possible with tabs.

    ;;	(switch-to-buffer-in-tab buffer) ;; to do: evaluate the "frame" argument properly and only do this if there's an existing tab.
    ;; otherwise, we'll never pop up a separate window.
    (let ((display-buffer-function nil))
      (display-buffer buffer not-this-window frame))))

(defun display-buffer-in-same-window (&rest args)

  (let (pop-up-windows pop-up-frames (same-window-regexps '(".*Custom.*"))
		       (display-buffer-function nil))
    (apply #'display-buffer args)))

;; (setq display-buffer-reuse-frames 'select)
(if (running-on-a-mac-p)
    (aquamacs-set-defaults 
     '((display-buffer-reuse-frames t)
       (display-buffer-function aquamacs-display-buffer))))

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
  ;; only delete window when tabbar-mode is not on!
  (with-current-buffer (current-buffer)
    (if (and one-buffer-one-frame (not (and (boundp 'tabbar-mode) tabbar-mode))
	     (not (memq '(lambda ()
			   (condition-case nil
			       (delete-window)
			     (error nil))) kill-buffer-hook)))
	
	(delete-window-if-created-for-buffer))))

(defvar aquamacs-last-frame-empty-buffer nil)
(defun init-aquamacs-last-frame-empty-buffer ()
  (unless (buffer-live-p aquamacs-last-frame-empty-buffer)
    (setq aquamacs-last-frame-empty-buffer (generate-new-buffer " *empty*"))
    (with-current-buffer aquamacs-last-frame-empty-buffer
      (setq buffer-read-only t)
      (setq header-line-format nil)))
  aquamacs-last-frame-empty-buffer)

(defvar aquamacs-last-frame-empty-frame nil)
(defun aquamacs-make-empty-frame (parms)
  (let ((all-parms
	 (append
	  '((visibility . nil))
	  parms)))
    (if (and aquamacs-last-frame-empty-frame
	     (frame-live-p aquamacs-last-frame-empty-frame)
	     (not (frame-iconified-p aquamacs-last-frame-empty-frame)))
	(modify-frame-parameters aquamacs-last-frame-empty-frame
				 parms)
      (setq aquamacs-last-frame-empty-frame (make-frame all-parms))))
  (select-frame aquamacs-last-frame-empty-frame)
  (raise-frame aquamacs-last-frame-empty-frame)
  aquamacs-last-frame-empty-frame)    

(defvar aquamacs-deleted-frame-position nil)
(defun aquamacs-delete-frame (&optional frame)
  ;; this function takes are to create a hidden frame that can receive
  ;; keyboard input while all other frames are iconified or deleted
  ;; The hidden frame shows a special, read-only *empty* buffer.

  ;; are we deleting a non-special frame?
  (let ((f (or frame (selected-frame))))
    ;; HACK: the buffer has been killed already at this point
    ;; so we must consider the frame title instead, which has not been updated yet
    (unless (or (special-display-p (frame-parameter f 'name))
		(if (> (length (frame-parameter f 'name)) 0) (string= (substring (frame-parameter f 'name) 0 1) " ")))
      ;; store frame position for reuse
      (setq aquamacs-deleted-frame-position
	  `((top . ,(frame-parameter f 'top))
	    (left . ,(frame-parameter f 'left))
	    (width . ,(frame-parameter f 'width))
	    (height . ,(frame-parameter f 'height)))))

    (with-current-buffer (current-buffer) ; delete-frame changes current buffer
      ;; current buffer must be preserved (e.g., for other kill-buffer-hook functions)
      (condition-case nil
	;; do not delete the last visible frame if there are others hidden:
	;; doing so prevents Aquamacs from receiving keyboard input (NS problem?)
	(progn 
	  (delete-frame (or frame (selected-frame)))
	  (unless (visible-frame-list) ;; delete-frame may succeed if iconified frames are around
	    (error)))
      (error
       ;; we're doing delete-frame later
       ;;(run-hook-with-args 'delete-frame-functions f)
       (let ((confirm-nonexistent-file-or-buffer)
	     (one-buffer-one-frame-mode nil)
	     (pop-up-frames nil)
	     (smart-frame-positioning-mode nil))
	 (delete-other-windows)
	 (set-window-dedicated-p (selected-window) nil)
	 ;; select read-only special buffer in case it gets any input
	 (let ((hb (init-aquamacs-last-frame-empty-buffer)))

	   (with-current-buffer hb
	     ;; to do: we should re-use a hidden frame if it exists.
	     (let ((hf (aquamacs-make-empty-frame aquamacs-deleted-frame-position)))
	       (if (and (not (eq f hf)) (frame-live-p f))
		   (delete-frame f t))
	       (select-window (frame-first-window hf))
	       (switch-to-buffer hb  'norecord)
	       (make-frame-visible hf) ; HACK: must do this first, presumably to convince NS to make it key.
	       (make-frame-invisible hf t))))))))))

(defun aquamacs-handle-frame-iconified (&optional frame)
  (interactive)
  (when (or (null (visible-frame-list))
	  (equal (visible-frame-list) (list (or frame (selected-frame)))))
    ;; if no other frame visible, create hidden backup frame to receive keyboard input
    (let ((bup-frame (aquamacs-make-empty-frame 
		      (mapcar (lambda (x) (cons x (frame-parameter frame x)))
			      '(top left width height)) 
		      )))
      (let ((confirm-nonexistent-file-or-buffer)
	    (one-buffer-one-frame nil)
	    (pop-up-frames nil)
	    (tabbar-mode nil))
      (switch-to-buffer (init-aquamacs-last-frame-empty-buffer))
      (make-frame-visible (selected-frame)) ; HACK: must do this first, presumably to convince NS to make it key.
      (make-frame-invisible (selected-frame))))))

;; usually, iconify-frame is bound to 'ignore
(define-key special-event-map [iconify-frame] 'aquamacs-handle-frame-iconified)

;; other Lisp code doesn't cause iconify-frame to be sent:

(defadvice iconify-frame (after leave-hidden-frame
				(&rest args) activate compile)
  (aquamacs-handle-frame-iconified (car args)))





;; delete window when buffer is killed
;; but only do so if aquamacs opened a new frame&window for
;; this buffer (e.g. during switch-to-buffer)
(defun delete-window-if-created-for-buffer (&optional buffer whole-frame-only)
  (with-current-buffer (current-buffer)
    (let ((buf (or buffer (current-buffer))))
    (let ((winlist (find-all-windows-internal buf)))
      (mapc  
       (lambda (win)
	 ;;force deletion if buffer is not killable
	 (if (or (not whole-frame-only)
		 (equal 1 (length (window-list 
				   (window-frame win) 'no-minibuf))))
	     (delete-window-if-created-for-this-buffer win 
						       (buffer-name buf) t)))
					; (not (killable-buffer-p buf)))
       winlist))
    ;; some functions like find-alternate-file (via kill-buffer-hook)
    ;; rely on the buffer still being current.  we're not killing it after all!
   )))
     
(defun delete-window-if-created-for-this-buffer (win buf-name skip-check)
  "Delete the window (sometimes)
Deletes a window WIN if appropriate. 
SKIP-CHECK non-nil causes the window WIN to be deleted even if
there is no record of opening the window just for this purpose."
  ;; used by osxkeys, too
  ;; as of now, we're always forcing the deletion of a window if the user requests it.
  ;; 
  (if (window-live-p win)
      (let ((elt (car (member (cons win buf-name)
			      aquamacs-newly-opened-windows))))
	(if (and (or (not buf-name) (not (same-window-p buf-name)) 
		     ;; always open in same window, so don't close the window
		     (window-dedicated-p win)) ;; dedicated windows should be closed
		 (or skip-check elt)) ;; only affect frames opened for this purpose
	    (progn
	      ;; remove entry from windows list
	      (if elt
		  (setq aquamacs-newly-opened-windows (delq elt aquamacs-newly-opened-windows)))
	      
	      ;; delete the window (or make the frame invisible)	  
	      (condition-case nil 
		  (if (window-live-p win)
		      (delete-window win)) ;; only get rid of that current window
		(error   
		 
		 (let ((f (window-frame win))) ;;(selected-frame)))
		   ;; hook can contain smart-frame-pos call
		   (run-hook-with-args 'delete-frame-functions f)
		   (make-frame-invisible f t)
		   
	       (if (find-all-frames-internal (get-buffer "*Messages*"))
		   (select-frame (car (find-all-frames-internal 
				       (get-buffer "*Messages*")))))))))
	  ;; else:
	  ;; decide not to delete / make invisible
	  ;; then switch buffer
	  ;; to whatever was shown previously (does this work well???)
	  (with-selected-window win
	    (previous-buffer-here))))))


(if (running-on-a-mac-p)
    (add-hook 'kill-buffer-hook 'delete-window-if-one-buffer-one-frame t))
 
;; this is what's bound to Apple-W
;; and what can called programmatically (instead of bury-buffer, etc.)
(defun close-window ()
  "Deletes the tab, window or frame and maybe kills buffer.
Deletes the selected tab, window or frame showing the current
buffer.  In `tabbar-mode' or in `one-buffer-one-frame-mode', and if
the tab, window or frame is the only one showing the buffer, kill
the buffer, too.  Ask user whether to kill it if appropriate."
  (interactive)
  ;; quit current command if in minibuffer
  (when (minibuffer-window-active-p 
       (minibuffer-window (selected-frame)))
      (abort-recursive-edit))
  ;; else, close tab or window+frame

  (cond 
   ((and (boundp tabbar-mode) tabbar-mode)
    (let ((tabbar-retain-windows-when-tab-deleted nil))
      (tabbar-close-tab)))
   (one-buffer-one-frame-mode
    (close-current-window-asktosave))
   (t ;; if neither OBOF nor tabs, then we don't kill the buffer
    (aquamacs-delete-window))))

(defun close-current-window-asktosave (&optional force-delete-frame)
  "Delete current buffer, close selected window (and its frame
if `one-buffer-one-frame'. Beforehand, ask to save file if necessary."
  (interactive) 
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
    (when (frame-visible-p (window-frame wind))

      (select-frame-set-input-focus (selected-frame))
					; ask before killing
      (with-current-buffer (window-buffer)
	(when (and ;(eq (current-buffer) (window-buffer)) 
					; only if a document is shown
	       killable
	       (eq   (string-match "\\*.*\\*" (buffer-name)) nil)
	       (eq   (string-match " SPEEDBAR" (buffer-name)) nil)) 
					; has no minibuffer!
	  (if (and
	       (or buffer-file-name buffer-offer-save)
	       (buffer-modified-p))
	      ;; a lot of buffers (e.g. dired) may be modified,
	      ;; but have no file name
	      (if (aquamacs-ask-for-confirmation
		 (format "Save buffer %s to file before closing window? 
The buffer contains unsaved changes which will be lost if you discard them now." (buffer-name)) 
		 nil (format "Save%s" (if buffer-file-name "" "...")) "Don't Save" t)
		(progn 
		    (if (listp last-nonmenu-event)
			(mac-key-save-file)
		      (save-buffer))
		    (if (buffer-modified-p)
			(keyboard-quit)
		      (message "File saved.")))
	      ;; mark as not modified, so it will be killed for sure
	      (set-buffer-modified-p nil))
	    (message ""))))
	
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
	(if killable (kill-buffer (window-buffer wind)))
	(when (window-live-p wind)
	  (if (or force-delete-frame ;; still needed?
		  (window-dedicated-p wind))
	      (aquamacs-delete-frame (window-frame wind) ) 
					; delete window/frame, hide if
					; necessary
	    ;; else
	    (progn
	      (select-window wind)
	      (aquamacs-delete-window wind) ) ) ) ) ) ) ) )   

(when (running-on-a-mac-p)
(defun handle-delete-frame (event)
  "Handle delete-frame events from the X server."
  (interactive "e")
  (let ((frame (posn-window (event-start event)))
	(i 0)
	(delw nil))
    (select-frame frame)
    (while 
	(and (frame-first-window frame) 
	     (window-live-p (frame-first-window frame))
	     (select-window (frame-first-window frame))
	     (setq delw (cons (frame-first-window frame) delw)
		   delb (window-buffer))
	     (or (let ((last-nonmenu-event))
		   (close-window)
		   ;; (close-current-window-asktosave nil)
		   ) t)
	     (frame-live-p frame)
	     ;; the above closing action will have deleted the window, so
	     ;; we have moved on.
	     ;; ensure that this is the case (i.e. no cancels)
	     (not (and (memq  (frame-first-window frame) delw)
		       (eq (window-buffer) delb)))))) 
  ;; needed due to bug in main event loop (first mouse event
  ;; is interpreted with wrong current buffer)
  (set-buffer (window-buffer (selected-window))))

;; (defun handle-ns-application-reopen ()
;;   (interactive)
;;   (if (visible-frame-list)
;;       ;; did it just bring up the *empty* frame?
;;       (let (last-iconified-frame)
;; 	(and (= 1 (length (visible-frame-list)))
;; 	     (equal (buffer-name) " *empty*")
;; 	     (progn (mapcar (lambda (f) (if (and (not last-iconified-frame) 
;; 						 (frame-live-p f) (frame-iconified-p f)) 
;; 					    (setq last-iconified-frame f))) (frame-list))
;; 		    last-iconified-frame)
;; 	     (let ((f (selected-frame)))
	       
;; 	       (raise-frame last-iconified-frame)
;; 	       (make-frame-invisible f)
;; 	       )))
;;     (let ((list (frame-list))) 
;;       (while list      
;; 	(when (frame-iconified-p (car list))
;; 	  (make-frame-visible (car list))
;; 	  (select-frame-set-input-focus (car list))
;; 	  (setq list))
;; 	(setq list (cdr list))))))
(define-key special-event-map [ns-application-reopen] 'ignore)
(define-key special-event-map [ns-application-activated] 'ignore)
)


  


;; FIXES IN VARIOUS PLACES


;; make sure that C-mouse-1 menu acts locally
(if (running-on-a-mac-p)
    (defadvice mouse-buffer-menu (around select-buffer-same-frame 
					 (&rest args) activate protect) 
      (let ((one-buffer-one-frame nil))
	ad-do-it)))
  
;; ediff-directories, e.g. uses split-window to create a new window
;; in a frame, and then `switch-to-buffer', which should simply show
;; another buffer in the newly created window. Problem is, in this
;; mode, this will open a new buffer.


(defadvice split-window (before inhibit-one-buffer-one-frame 
				(&rest args) activate compile)
  (when one-buffer-one-frame-mode
    (setq one-buffer-one-frame-inhibit t)
    ;; clear flag as soon as command has finished (or similar)
    (run-with-idle-timer 
     0 nil 
     (lambda () (setq one-buffer-one-frame-inhibit nil)))))


;; (defadvice delete-window (before inhibit-one-buffer-one-frame 
;; 				(&rest args) activate compile)
;;   (when one-buffer-one-frame-mode
;;     (setq one-buffer-one-frame-inhibit t)
;;     ;; clear flag as soon as command has finished (or similar)
;;     (run-with-idle-timer 
;;      0 nil 
;;      (lambda () (setq one-buffer-one-frame-inhibit nil))))) 


(defadvice desktop-read (around inhibit-one-buffer-one-frame 
				(&rest args) activate compile)
  (let ((one-buffer-one-frame-inhibit t))
    ad-do-it))


;; Commands

(defun aquamacs-join-windows ()
  "Join windows in selected or all frames.
If selected frame contains more than one window, join the windows together.
Otherwise, combine all other frames into selected frame."
  (interactive)
  (if tabbar-mode
      (tabbar-window-merge-windows-in-frame)
    (delete-other-windows)))

(defun aquamacs-split-window-vertically ()
  "Split window, select new window.
Like `split-window-vertically' without further arguments, 
but select the newly created window."
  (interactive)
  (select-window (split-window-vertically)))

(defun raise-next-frame ()
"Raise the next frame.
See also `raise-previous-frame' and `other-frame'.
An Aquamacs-only function."
  (interactive)
  (other-frame 1))

(defun raise-previous-frame ()
"Raise the previous frame.
See also `raise-next-frame' and `other-frame'.
An Aquamacs-only function."
  (interactive)
  (other-frame -1))

(defun other-previous-window ()
  "Like `next-window', but in the opposite direction."
  (interactive)
  (other-window -1))

(provide 'one-buffer-one-frame)