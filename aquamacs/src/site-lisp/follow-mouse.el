;;; follow-mouse.el --- Automatically select the window under the mouse -*-unibyte: t; coding: iso-8859-1;-*-

;; Copyright ? 1998,2000,2003 Kevin Rodgers
;; Copyright 2008 David Reitter

;; Author: Kevin Rodgers <ihs_4664@yahoo.com>
;; Created: 12 May 1998
;; Version: $Revision: 1.2 $
;; Keywords: mouse

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; By default, a window within an Emacs frame must be selected by
;; typing `C-x o' (other-window) or by clicking [mouse-1] on the mode
;; line or the buffer itself (mouse-set-point); this corresponds to a
;; "click to type" window manager policy.  follow-mouse.el implements a
;; "focus follows mouse" window manager policy, so that a window is
;; selected when the mouse moves over it.
;; 
;; To enable follow-mouse, put this in your ~/.emacs file:
;; 	(turn-on-follow-mouse)
;; 
;; follow-mouse can be enabled or disabled interactively with the
;; `M-x turn-on-follow-mouse', `M-x turn-off-follow-mouse', and
;; `M-x toggle-follow-mouse' commands.
;; 
;; By default, follow-mouse will deselect an active minibuffer window;
;; to prevent that, just unset the
;; `follow-mouse-deselect-active-minibuffer' option.
;; 
;; By default, follow-mouse also raises the frame whose window is
;; selected; to disable that, just unset the
;; `follow-mouse-auto-raise-frame' option.

;; (turn-on-follow-mouse)
;; (setq follow-mouse-auto-raise-frame t)
;; (setq follow-mouse-auto-raise-frame nil)
;; (turn-off-follow-mouse)


;; Revisions

;; 1.17a.
;;   set input focus to the frame containing the selected window.
;;   select the frame after raising it.

;;; Code:

(provide 'follow-mouse)

(require 'custom)			; defgroup, defcustom

(defgroup follow-mouse nil
  "Automatically select the window under the mouse."
  :group 'mouse)

(defvar follow-mouse nil
  "If non-nil, `\\<special-event-map>\\[follow-mouse-select-window]' \
selects the window under the mouse.
Don't set this variable directly; use `\\[toggle-follow-mouse]' instead.")

(defcustom follow-mouse-deselect-active-minibuffer t
  "*If non-nil, `\\<special-event-map>\\[follow-mouse-select-window]' \
deselects an active minibuffer window."
  :group 'follow-mouse
  :type '(boolean))
(put 'follow-mouse-deselect-active-minibuffer 'variable-interactive
     "XLeave active minibuffer window? (t or nil): ")

(defcustom follow-mouse-auto-raise-frame t
  "*If non-nil, `\\<special-event-map>\\[follow-mouse-select-window]' \
raises the frame as well."
  :group 'follow-mouse
  :type '(boolean))
(put 'follow-mouse-auto-raise-frame 'variable-interactive
     "XAutomatically raise the selected window's frame? (t or nil): ")

;;;###autoload
(defun turn-on-follow-mouse ()
  "Moving the mouse will automatically select the window under it."
  (interactive)
  (toggle-follow-mouse 1 (interactive-p)))

;;;###autoload
(defun turn-off-follow-mouse ()
  "Moving the mouse will not automatically select the window under it."
  (interactive)
  (toggle-follow-mouse 0 (interactive-p)))

;;;###autoload
(defun toggle-follow-mouse (&optional arg verbose)
  "Toggle whether moving the mouse automatically selects the window under it.
If the optional prefix ARG is specified, follow-mouse is enabled if it is
positive, and disabled otherwise.  If called interactively, or the optional
VERBOSE argument is non-nil, display a confirmation message."
  (interactive (list current-prefix-arg t))
  (if (or (null arg)
	  (if (> (prefix-numeric-value arg) 0)
	      (not follow-mouse)
	    follow-mouse))
      ;; Toggle it:
      (progn
	(cond ((setq follow-mouse (not follow-mouse))
	       ;; Save the current value of track-mouse before (re)setting it:
	       (put 'follow-mouse 'track-mouse track-mouse)
	       (setq track-mouse t)
	       ;; Save the current binding of [mouse-movement] before
	       ;; (re)binding it:
	       (put 'follow-mouse 'mouse-movement
		    (lookup-key special-event-map [mouse-movement]))
	       (define-key special-event-map [mouse-movement]
		 'follow-mouse-select-window))
	      (t			; disable
	       ;; Restore the previous value of track-mouse:
	       (setq track-mouse (get 'follow-mouse 'track-mouse))
	       ;; Restore the previous binding of [mouse-movement]:
	       (define-key special-event-map [mouse-movement]
		 (get 'follow-mouse 'mouse-movement))))
	(if (or (interactive-p) verbose)
	    (message "Follow mouse is %s"
		     (if follow-mouse "enabled" "disabled"))))
    (if (or (interactive-p) verbose)
	(message "Follow mouse is already %s"
		 (if follow-mouse "enabled" "disabled"))))    
  ;; Return the result:
  follow-mouse)

(defun follow-mouse-select-window (event)
  "*Like `mouse-select-window', if `follow-mouse' is set.
Otherwise, do nothing; in particular, don't generate an error if EVENT
occurs outside a window or in an inactive minibuffer window.
See `follow-mouse-deselect-active-minibuffer' and
`follow-mouse-auto-raise-frame'."
  (interactive "e")
  (prog1 (if follow-mouse
	     (let ((current-window (get-buffer-window (current-buffer)))
		   (event-window (posn-window (event-start event))))
	       (if (and (or (not (window-minibuffer-p current-window))
			    (not (minibuffer-window-active-p current-window))
			    follow-mouse-deselect-active-minibuffer)
			(windowp event-window)
			(or (not (window-minibuffer-p event-window))
			    (minibuffer-window-active-p event-window)))
		   (progn
		     (or (eq (window-buffer current-window)
			     (window-buffer event-window))
			 (run-hooks 'mouse-leave-buffer-hook))
		     (if follow-mouse-auto-raise-frame
			 (progn 
			   (mouse-select-window event)
			   (select-frame (window-frame event-window)))
		       (select-window event-window))
		     (x-focus-frame (window-frame event-window))))))
    ;; Enable dragging:
    (setq unread-command-events
	  (nconc unread-command-events (list event)))))

;;; follow-mouse.el ends here
