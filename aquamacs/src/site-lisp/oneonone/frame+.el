;;; Aquamacs-Update: http://www.emacswiki.org/cgi-bin/wiki/frame+.el/download/frame+.el
;;; frame+.el --- Extensions to `frame.el'.
;;
;; Filename: frame+.el
;; Description: Extensions to `frame.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2005, Drew Adams, all rights reserved.
;; Created: Fri Apr 12 16:42:12 1996
;; Version: 21.0
;; Last-Updated: Wed Dec 07 09:33:25 2005 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 168
;; Keywords: frames
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `fit-frame', `frame', `frame-cmds', `frame-fns',
;;   `misc-fns', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `frame.el'.
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
;;  This file should be loaded after loading the standard GNU file
;;  `frame.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "frame" '(require 'frame+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2005/05/31 dadams
;;     special-display-popup-frame: Put with-current-buffer around fit-frame.
;; 2005/05/29 dadams
;;     Moved to frame-cmds.el: enlarge-frame*, shrink-frame*.
;; 2004/10/11 dadams
;;     Moved here from fit-frame.el: enlarge-frame*, shrink-frame*.
;; 2004/10/01 dadams
;;     special-display-popup-frame: Updated for Emacs 21 also.
;; 2000/09/27 dadams
;;     special-display-popup-frame:
;;       1. Does fit-frame.
;;       2. Doesn't make-frame-visible (done by raise-frame).
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
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
;;    current buffer.  This fix is only needed prior to Emacs 21.
;; 2. Calls `fit-frame'.
;;;###autoload
(defun special-display-popup-frame (buffer &optional args)
  "Display BUFFER in its own frame, reusing an existing window if any.
Return the window chosen.  Window is not selected within its frame.

If a new frame is needed, then `make-frame' is called to create it,
with BUFFER as the current buffer.

If ARGS is an alist, use it as a list of frame parameter specs.
If ARGS is a list whose car is a symbol, use (car ARGS) as a function
to do the work.  Pass it BUFFER as first arg, and (cdr ARGS) as the
rest of its args."
  (if (and args (symbolp (car args)))
      (let* ((window (apply (car args) buffer (cdr args)))
             (frame (window-frame window)))
        (when (fboundp 'fit-frame) (fit-frame (window-frame window)))
        (raise-frame frame)
        window)                         ; Return the window.
    (let ((window (get-buffer-window buffer 0)))
      (or
       ;; If we have a window already, make it visible.
       (when window
         (let ((frame (window-frame window)))
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
       (let ((frame (with-current-buffer buffer
                      (make-frame (append args special-display-frame-alist)))))
         (when (and (fboundp 'fit-frame)
                    (not (memq 'fit-frame after-make-frame-functions)))
           (with-current-buffer buffer (fit-frame frame)))
         (set-window-buffer (frame-selected-window frame) buffer)
         (set-window-dedicated-p (frame-selected-window frame) t)
         (frame-selected-window frame)))))) ; Return the window.

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'frame+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; frame+.el ends here
