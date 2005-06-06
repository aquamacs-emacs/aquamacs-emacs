;;; frame+.el --- Extensions to `frame.el'.
;; 
;; Filename: frame+.el
;; Description: Extensions to `frame.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2005, Drew Adams, all rights reserved.
;; Created: Fri Apr 12 16:42:12 1996
;; Version: 21.0
;; Last-Updated: Tue Feb 08 16:37:20 2005
;;           By: dradams
;;     Update #: 131
;; Keywords: frames
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;    Extensions to `frame.el'.
;;
;;
;;  New functions defined here:
;;
;;    `enlarge-frame', `enlarge-frame-horizontally',
;;    `fit-frame-or-mouse-drag-vertical-line', `shrink-frame',
;;    `shrink-frame-horizontally'.
;;
;;
;;  ***** NOTE: The following function defined in `frame.el' has been
;;              REDEFINED HERE:
;;
;;  `special-display-popup-frame' - 1. Calls `make-frame' while BUFFER
;;                                     is current, so frame hooks use
;;                                     BUFFER.
;;                                  2. Calls `fit-frame'.
;;
;;
;;  Suggested key bindings:
;;  
;;  (global-set-key [(control meta down)] 'enlarge-frame)
;;  (global-set-key [(control meta right)] 'enlarge-frame-horizontally)
;;  (global-set-key [(control meta up)] 'shrink-frame)
;;  (global-set-key [(control meta left)] 'shrink-frame-horizontally)
;;  (when (< emacs-major-version 21)
;;    (global-set-key [vertical-line down-mouse-1]
;;       'fit-frame-or-mouse-drag-vertical-line))
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `frame.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "frame" '(require 'frame+))
;;
;;  Library `frame+' requires these libraries:
;;
;;    `avoid', `fit-frame', `frame', `frame-cmds', `frame-fns',
;;    `icomplete', `icomplete+', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 2004/10/11 dadams
;;     Moved here from fit-frame.el: enlarge-frame*, shrink-frame*,
;;       fit-frame-or-mouse-drag-vertical-line.
;; 2004/10/01 dadams
;;     special-display-popup-frame: Updated for Emacs 21 also.
;; 2000/09/27 dadams
;;     special-display-popup-frame:
;;       1. does fit-frame.
;;       2. doesn't make-frame-visible (done by raise-frame).
;; 1999/03/17 dadams
;;     Updated to corrspond with version Emacs 34.1.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

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
;; 
;;; Code:

(require 'frame)

(require 'fit-frame nil t) ;; (no error if not found): fit-frame

;;;;;;;;;;;;;;;;;;;



;; REPLACES ORIGINAL in `frame.el':
;; 1. Calls `make-frame' while BUFFER is current, so that any frame hooks
;;    (e.g. `after-make-frame-functions') will use BUFFER, not the previously
;; current buffer.
;; 2. Calls `fit-frame'.
;;;###autoload
(defun special-display-popup-frame (buffer &optional args)
  "Display BUFFER in its own frame, reusing an existing window if any.
Return the window chosen. The window is not selected within its frame.

If a new frame is needed, then `make-frame' is called to create it,
with BUFFER as the current buffer (temporarily).

If ARGS is an alist, use it as a list of frame parameter specs.
If ARGS is a list whose car is a symbol, use (car ARGS) as a function
to do the work.  Pass it BUFFER as first arg, and (cdr ARGS) gives the
rest of the args to the function."
  (if (and args (symbolp (car args)))
      (let* ((window (apply (car args) buffer (cdr args)))
             (frame (window-frame window)))
					;(print "asdhkasdkjahsdkjh fitting frame!")
					;  (when (fboundp 'fit-frame) (fit-frame (window-frame window)))
        (raise-frame frame)
        window)                         ; Return the window.
					;(print (get-buffer-window buffer 0))
  
    (let ((window (get-buffer-window buffer 0)))
      (or
       ;; If we have a window already, make it visible.
       (when window
         (let ((frame (window-frame window)))
	   (save-excursion
	     (set-buffer buffer)
	     (fit-frame frame)
	     )
           (make-frame-visible frame)
           (raise-frame frame)
           (when (fboundp 'fit-frame) (fit-frame frame))
           window))                     ; Return the window.
       ;; Reuse the current window if the user requested it.
       (when (cdr (assq 'same-window args))
         (condition-case nil
             (progn (switch-to-buffer buffer) (selected-window))
           (error nil)))
       ;; Stay on the same frame if requested.
       (when (or (cdr (assq 'same-frame args)) (cdr (assq 'same-window args)))
         (let* ((pop-up-frames nil) (pop-up-windows t)
                special-display-regexps special-display-buffer-names
                (window (display-buffer buffer)))
           ;; Only do it if this is a new window:
           ;; (set-window-dedicated-p window t)
           window))                     ; Return the window.
       
       ;; If no window yet, make one in a new frame.
       (let (frame)
	     
		   (save-excursion	  ; Make frame while BUFFER is
                      (set-buffer buffer) ; current => frame hooks OK.
		       
		      ; create invisible frame
                      (setq frame (make-frame (append '((visibility . nil)) 
						      args special-display-frame-alist)))
	  
		      (unless (memq 'fit-frame after-make-frame-functions)
			(when (fboundp 'fit-frame) 
	     
			  (fit-frame frame)
			  ) 
			)

		      ; now show frame, after fitting it
		      (make-frame-visible frame)
		      
		      )
		    
	     
	 
         (set-window-buffer (frame-selected-window frame) buffer)
	 
         (set-window-dedicated-p (frame-selected-window frame) t)
         (frame-selected-window frame)))))) ; Return the window.


;; Inspired by `sk-grow-frame' from Sarir Khamsi [sarir.khamsi@raytheon.com]
;;;###autoload
(defun enlarge-frame (&optional increment frame)
  "Increase the height of FRAME (default: selected-frame) by INCREMENT.
Interactively, INCREMENT is given by the prefix argument."
  (interactive "p")
  (set-frame-height frame (+ (frame-height frame) increment)))

;;;###autoload
(defun enlarge-frame-horizontally (&optional increment frame)
  "Increase the width of FRAME (default: selected-frame) by INCREMENT.
Interactively, INCREMENT is given by the prefix argument."
  (interactive "p")
  (set-frame-width frame (+ (frame-width frame) increment)))

;;;###autoload
(defun shrink-frame (&optional increment frame)
  "Decrease the height of FRAME (default: selected-frame) by INCREMENT.
Interactively, INCREMENT is given by the prefix argument."
  (interactive "p")
  (set-frame-height frame (- (frame-height frame) increment)))

;;;###autoload
(defun shrink-frame-horizontally (&optional increment frame)
  "Decrease the width of FRAME (default: selected-frame) by INCREMENT.
Interactively, INCREMENT is given by the prefix argument."
  (interactive "p")
  (set-frame-width frame (- (frame-width frame) increment)))

;;;###autoload
(when (and (< emacs-major-version 21) (fboundp 'fit-frame))
  (defun fit-frame-or-mouse-drag-vertical-line (start-event)
    "If only window in frame, `fit-frame'; else `mouse-drag-vertical-line'"
    (interactive "e")
    (if (one-window-p t) (fit-frame) (mouse-drag-vertical-line start-event))))


;;;;;;;;;;;;;;;;;;;;;;;

(provide 'frame+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; frame+.el ends here
