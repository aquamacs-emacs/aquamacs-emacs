;;; pager.el --- windows-scroll commands
;;; Version 2.0 - 97-10-06
;;; Copyright (C) 1992-1997 Mikael Sjödin (mic@docs.uu.se)
;;;
;;; Author: Mikael Sjödin  --  mic@docs.uu.se
;;;
;;; This file is NOT part of GNU Emacs.
;;; You may however redistribute it and/or modify it under the terms of the GNU
;;; General Public License as published by the Free Software Foundation; either
;;; version 2, or (at your option) any later version.
;;;
;;; pager.el is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; ----------------------------------------------------------------------
;;; Description:
;;;
;;; pager.el defines alternative commands to the Emacs builtins: scroll-down
;;; and scroll-up.  It also contains commands to scroll the screen one row at
;;; the time.
;;;
;;; The Emacs builtins for scrolling are worthless!  The commands in pager.el
;;; works the way the builtins should have done from the beginning.  For
;;; instance, doing a pg-up followed by a pg-down (when using pager.el) will
;;; return point to the original place.  
;;;
;;; This file has been tested under Emacs 19.34 and 20.2 but I belive it should
;;; work on most Emacs versions and Emacs derivatives.
;;;
;;; This file can be obtained from http://www.docs.uu.se/~mic/emacs.html

;;; ----------------------------------------------------------------------
;;; Installation:
;;;
;;; o Place this file in a directory in your load-path.
;;; o Put the following in your .emacs file:
;;;     (require 'pager)
;;;     (global-set-key "\C-v"	   'pager-page-down)
;;;     (global-set-key [next] 	   'pager-page-down)
;;;     (global-set-key "\ev"	   'pager-page-up)
;;;     (global-set-key [prior]	   'pager-page-up)
;;;     (global-set-key '[M-up]    'pager-row-up)
;;;     (global-set-key '[M-kp-8]  'pager-row-up)
;;;     (global-set-key '[M-down]  'pager-row-down)
;;;     (global-set-key '[M-kp-2]  'pager-row-down)
;;; o Restart your Emacs. 
;;; o pager.el is now installed.  Use the normal keys to scroll a full page and
;;;   M-up resp. M-down to scroll just one row up or down.

;;; ----------------------------------------------------------------------
;;; Versions:
;;; 2.0 Renamed interface functions (kept old-ones as aliases)
;;;     Complete reimplementation, old version where not working well in Emacs
;;;     20.
;;;
;;; 1.0 Initial Release 

;;; ======================================================================
;;; Internal variables

(defvar pager-temporary-goal-column 0
  "Similat to temporary-goal-column byt used by the pager.el functions")
;(make-variable-buffer-local 'pager-temporary-goal-column)

(defconst pager-keep-column-commands
  '(pager-row-down pager-row-up row-dn row-up
		   pager-page-down pager-page-up pg-dn pg-up)
  "Commands which when called without any other intervening command should
keep the `pager-temporary-goal-column'")

;;; ======================================================================
;;; Commands

;;; pager 1.0 compatibility
(defalias 'pg-dn 'pager-page-down)
(defalias 'pg-up 'pager-page-up)
(defalias 'row-dn 'pager-row-down)
(defalias 'row-up 'pager-row-up)

;; ----------------------------------------------------------------------

(defun pager-page-down ()
  "Like scroll-up, but moves a fixed amount of lines (fixed relative the
`window-height') so that pager-page-up moves back to the same line."
  (interactive)
  (if (not (pos-visible-in-window-p (point-max)))
      (pager-scroll-screen (- (1- (window-height))
			      next-screen-context-lines))))
    
(defun pager-page-up ()
  "Like scroll-down, but moves a fixed amount of lines (fixed relative the
`window-height') so that pager-page-down moves back to the same line."
  (interactive)
  (if (not (pos-visible-in-window-p (point-min)))
      (pager-scroll-screen (- next-screen-context-lines 
			      (1- (window-height))))))

;; ------------------------------

(defun pager-scroll-screen (lines)
  "Scroll screen LINES, but keep the cursors position on screen."
  (if (not (memq last-command pager-keep-column-commands))
      (setq pager-temporary-goal-column (current-column)))
  (save-excursion
    (goto-char (window-start))
    (forward-line lines)
    (set-window-start (selected-window) (point)))
  (forward-line lines)
  (move-to-column pager-temporary-goal-column))


;; ----------------------------------------------------------------------

(defun pager-row-up ()
  "Move point to previous line while scrolling screen down one line.
The effect is that the cursor stays in the same position on the screen."
  (interactive)
  (if (not (memq last-command pager-keep-column-commands))
      (setq pager-temporary-goal-column (current-column)))
  (if (not (pos-visible-in-window-p (point-min)))
      (scroll-down 1))
  (forward-line -1)
  (move-to-column pager-temporary-goal-column)
  )

(defun pager-row-down ()
  "Move point to next line while scrolling screen up one line.
The effect is that the cursor stays in the same position on the screen."
  (interactive)
  (if (not (memq last-command pager-keep-column-commands))
      (setq pager-temporary-goal-column (current-column)))
  (if (not (pos-visible-in-window-p (point-max)))
      (scroll-up 1))
  (if (<= (point) (point-max))
      (forward-line 1))
  (move-to-column pager-temporary-goal-column)
  )

;; ----------------------------------------------------------------------

(provide 'pager)


