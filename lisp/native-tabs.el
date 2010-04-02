;;; native-tabs.el --- Support tabs in frames

;; Copyright (C) 2010 Free Software Foundation, Inc.

;; Author: Jan Djärv <jan.h.d@swipnet.se>
;; Maintainer: FSF
;; Keywords: tabs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides the lisp part of Gtk+ native tabs.

;;; Code:

;;; Customizable variables

(declare-function tab-new "xfns.c" ())
(declare-function tab-delete "xfns.c" ())
(declare-function tab-delete-other "xfns.c" ())
(declare-function tab-next "xfns.c" ())
(declare-function tab-previous "xfns.c" ())
(declare-function tab-nr-of-tabs "xfns.c" ())
(declare-function tab-configuration "xfns.c" ())
(declare-function tab-current "xfns.c" ())
(declare-function tab-show "xfns.c" ())

(defun find-file-new-tab (filename &optional wildcards)
  "Edit file FILENAME, in a new tab.

Like \\[find-file] (which see), but creates a new tab.

Interactively, the default if you just type RET is the current directory,
but the visited file name is available through the minibuffer history:
type M-n to pull it into the minibuffer.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files."
  (interactive
   (find-file-read-args "Find file in new tab: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (not (null (tab-new)))
	(progn
	  (delete-other-windows)
	  (if (listp value)
	      (progn
		(setq value (nreverse value))
		(cons (switch-to-buffer (car value))
		      (mapcar 'switch-to-buffer (cdr value))))
	    (switch-to-buffer value))))))

(defun switch-to-buffer-other-tab (buffer-or-name &optional norecord)
  "Switch to buffer BUFFER-OR-NAME in another tab.
BUFFER-OR-NAME may be a buffer, a string \(a buffer name), or
nil.  Return the buffer switched to.

If called interactively, prompt for the buffer name using the
minibuffer.  The variable `confirm-nonexistent-file-or-buffer'
determines whether to request confirmation before creating a new
buffer.

If BUFFER-OR-NAME is a string and does not identify an existing
buffer, create a new buffer with that name.  If BUFFER-OR-NAME is
nil, switch to the buffer returned by `other-buffer'.

Optional second arg NORECORD non-nil means do not put this
buffer at the front of the list of recently selected ones.

This uses the function `display-buffer' as a subroutine; see its
documentation for additional customization information."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other tab: ")))
  (let ((same-window-buffer-names same-window-regexps))
    (if (not (null (tab-new)))
	(progn
	  (select-window (display-buffer buffer-or-name nil (selected-frame))
			 norecord)
	  (delete-other-windows)))))


(defun display-existing-buffer-in-tab (buffer-or-name &optional frame)
  "Switch to a tab that shows BUFFER-OR-NAME on FRAME.
FRAME nil means selected frame.

Returns the key for the tab switch to, or nil if no tab displays 
BUFFER-OR-NAME."
  (let* ((buffer (if (bufferp buffer-or-name)
		     buffer-or-name
		   (get-buffer buffer-or-name)))
	 (tabs (if buffer (tab-configuration frame) nil))
	 (tab-key))
    (while (and tabs (null tab-key))
      (let* ((elt (car tabs))
	     (winconf (cadr elt))
	     (buffers (buffers-in-window-configuration winconf)))
	(if (memq buffer buffers)
	    (setq tab-key (car elt))
	  (setq tabs (cdr tabs)))))
    (if (and tab-key (not (equal tab-key (tab-current frame))))
	(progn
	  (tab-show tab-key frame)
	  tab-key)
      nil)))

(defun switch-to-buffer-tab (buffer-or-name &optional frame)
  (interactive "BSwitch to buffer:\nP")
  (if (not (display-existing-buffer-in-tab buffer-or-name frame))
      (switch-to-buffer buffer-or-name)))

(defun handle-tab-event (event)
  "Handle tab-event to change tabs on the frame in EVENT."
  (interactive "e")
  (let* ((n1 (nth 0 (cdr event)))
	 (n2 (nth 1 (cdr event)))
	 (type (car n2))
	 (frame (car n1))
	 (x (car (cdr n1)))
	 (y (cdr (cdr n1))))
    (if (eq type 2) 
	(let ((top y)
	      (left x)
	      (width (frame-pixel-width frame))
	      (height (frame-pixel-height frame))
	      (dw (x-display-pixel-width frame))
	      (dh (x-display-pixel-height frame)))
	  (if (< dw (+ left width))
	      (setq left (- dw width)))
	  (if (< dh (+ top height))
	      (setq top (- dh height)))
	  (make-frame 
	   (list (cons 'width (frame-parameter frame 'width))
		 (cons 'height(frame-parameter frame 'height))
		 (cons 'top top)
		 (cons 'left left)))))))

(if (featurep 'tabs)
    (progn
      (define-key special-event-map [tab-event]
	'handle-tab-event)
      (global-set-key "\C-x7\C-f" 'find-file-new-tab)
      (global-set-key "\C-x70" 'tab-delete)
      (global-set-key "\C-x71" 'tab-delete-other)
      (global-set-key "\C-x72" 'tab-new)
      (global-set-key "\C-x73" 'switch-to-buffer-tab)
      (global-set-key "\C-x7b" 'switch-to-buffer-other-tab)
      (global-set-key "\C-x7f" 'find-file-new-tab)
      (global-set-key "\C-x7o" 'tab-next)
      (global-set-key "\C-x7n" 'tab-next)
      (global-set-key "\C-x7p" 'tab-previous)))

