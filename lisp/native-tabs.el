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

(defvar tab-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x7\C-f" 'find-file-new-tab)
    (define-key map "\C-x70" 'tab-delete)
    (define-key map "\C-x71" 'tab-delete-other)
    (define-key map "\C-x72" 'tab-new)
    (define-key map "\C-x73" 'switch-to-buffer-in-tab)
    (define-key map "\C-x7b" 'switch-to-buffer-other-tab)
    (define-key map "\C-x7f" 'find-file-new-tab)
    (define-key map "\C-x7o" 'tab-next)
    (define-key map "\C-x7n" 'tab-next)
    (define-key map "\C-x7p" 'tab-previous)
    map)
  "Keymap for `tab-mode´")

;;;###autoload
(define-minor-mode tab-mode
  "Toggle use of tabs.
This command applies to all frames that exist and frames to be
created in the future.
With numeric ARG, use tabs if and only if ARG is positive.

Keyboard commands for tabs are:
\\{tab-mode-map}."
  :init-value t
  :global t
  :group 'mouse
  :group 'frames
  :keymap tab-mode-map
  (modify-all-frames-parameters (list (cons 'disable-tabs (not tab-mode)))))

(declare-function tab-new "xfns.c" (&optional label frame))
(declare-function tab-delete "xfns.c" (&optional label frame))
(declare-function tab-delete-other "xfns.c" (&optional frame))
(declare-function tab-next "xfns.c" (&optional frame))
(declare-function tab-previous "xfns.c" (&optional frame))
(declare-function tab-nr-of-tabs "xfns.c" (&optional frame))
(declare-function tab-current "xfns.c" (&optional frame))
(declare-function tab-show "xfns.c" (key &optional frame))
(declare-function tab-enable "xfns.c" (enable &optional frame))

(defun current-tab-window-config ()
  (list (current-window-configuration) (point-marker)))

(defun window-tab-config-frame (config)
  (if (and (consp config) (window-configuration-p (car config)))
      (window-configuration-frame (car config))
    nil))

(defun set-tab-window-config (config)
  (and (consp config) (window-configuration-p (car config))
       (set-window-configuration (car config))
       (goto-char (cadr config))))

(defun change-tab-window-config-frame (config frame)
  (if (and (consp config) (window-configuration-p (car config)))
      (list (change-window-configuration-frame (car config) frame)
	    (cadr config))
    config))

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
  (save-window-excursion
    (let* ((value (find-file-noselect filename nil nil wildcards))
	   (newtab (tab-new)))
      (if newtab
	  (progn
	    (delete-other-windows)
	    (if (listp value)
		(progn
		  (setq value (nreverse value))
		  (cons (switch-to-buffer (car value))
			(dolist 'switch-to-buffer (cdr value))))
	      (switch-to-buffer value))
	    (put newtab 'winconfig (current-tab-window-config)))))))


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


(defun find-tab-for-existing-buffer (buffer-or-name &optional frame)
  "Find a tab that shows BUFFER-OR-NAME on FRAME.
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
	     (winconf (get elt 'winconfig))
	     (buffers (buffers-in-window-configuration winconf)))
	(if (memq buffer buffers)
	    (setq tab-key elt)
	  (setq tabs (cdr tabs)))))
    tab-key))

(defun switch-to-buffer-in-tab (buffer-or-name &optional frame)
  (interactive "BSwitch to buffer:\nP")
  (let ((tab (find-tab-for-existing-buffer buffer-or-name frame)))
    (if tab
	(tab-show tab frame)
      (switch-to-buffer buffer-or-name))))

(defun handle-tab-event (event)
  "Handle tab-event to change tabs on the frame in EVENT."
  (interactive "e")
  (let* ((n1 (nth 0 (cdr event)))
	 (n2 (nth 1 (cdr event)))
	 (type (car n2))
	 (frame (car n1))
	 (x (car (cdr n1)))
	 (y (cdr (cdr n1))))

    (cond ((eq type 'tab-new-frame) ;; // A tab is dropped to the background.
	   (let ((tab (car (cdr n2)))
		 (top y)
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
		    (cons 'left left)))))

	  ((eq type 'tab-changed)
	   (let* ((newtab (car (cdr n2)))
		  (newcfg (get newtab 'winconfig))
		  (oldtab (cdr (cdr n2))))
	     (if oldtab (put oldtab 'winconfig (current-tab-window-config)))
	     (if newcfg (set-tab-window-config
			 (if (eq (window-tab-config-frame newcfg) frame)
			     newcfg
			   (put newtab 'winconfig
				(change-tab-window-config-frame newcfg frame))))
	       (delete-other-windows)))))))

(define-key special-event-map [tab-event] 'handle-tab-event)
