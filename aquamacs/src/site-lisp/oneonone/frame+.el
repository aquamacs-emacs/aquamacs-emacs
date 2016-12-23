;;; frame+.el --- Extensions to `frame.el'.
;;
;; Filename: frame+.el
;; Description: Extensions to `frame.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2016, Drew Adams, all rights reserved.
;; Created: Fri Apr 12 16:42:12 1996
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Dec 31 13:26:18 2015 (-0800)
;;           By: dradams
;;     Update #: 264
;; URL: http://www.emacswiki.org/frame+.el
;; Doc URL: http://emacswiki.org/OneOnOneEmacs
;; Keywords: frames
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `fit-frame', `frame'.
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
;;  `special-display-popup-frame' - Call `fit-frame'.
;;
;;  NOTE: Starting with Emacs 24, `special-display-popup-frame' was
;;        moved to `window.el' from `frame.el'.  I have therefore
;;        moved my enhancement of it from `frame+.el' to my library
;;        `window+.el'.  This means that `frame+.el' is now OBSOLETE.
;;        I leave it posted in case someone with an older release does
;;        not want the additional enhancements that are included in
;;        `window+.el'.
;;
;;  This file should be loaded after loading the standard GNU file
;;  `frame.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "frame" '(require 'frame+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/08/25 dadams
;;     special-display-popup-frame:
;;       Put back missing (set-window-buffer window buffer) - removed accidentally.
;;       Call fit-frame with the buffer's WINDOW selected.  Inhibit before then.
;; 2012/08/11 dadams
;;     special-display-popup-frame:
;;       Do not redefine if window+.el was loaded.
;;       Updated to be the same as definition in window+.el:
;;         For latest Emacs 24.
;;         Adapt redefinition for all Emacs versions.
;;         Do not raise or fit frame if (car ARGS) is FUNCTION.  Make it do the work.
;; 2011/06/29 dadams
;;     Restricted special-display-popup-frame definition here to Emacs < 24.
;; 2011/01/04 dadams
;;     Removed autoload cookie from non-interactive function.
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



;; REPLACE ORIGINAL in `frame.el':
;;
;; This is the same definition as in `window+.el'.  Starting with Emacs 24,
;; `special-display-popup-frame' is defined in `window.el', not `frame.el'.
;;
;; 1. (Emacs 20 only) Calls `make-frame' while BUFFER is current, so that
;;    any frame hooks (e.g. `after-make-frame-functions') will use BUFFER,
;;    not the previously current buffer.
;;
;; 2. Call `fit-frame', with BUFFER's window selected.  Inhibit fitting before then.
;;
(unless (featurep 'window+)             ; Same definition is in `window+.el'.
  (defun special-display-popup-frame (buffer &optional args)
    "Pop up a frame displaying BUFFER.  Return its window.
If BUFFER is already displayed in a visible or iconified frame then
raise that frame.  Otherwise, display BUFFER in a new frame.

Optional argument ARGS is a list specifying additional information.

If ARGS is an alist, use it as a list of frame parameters.  If these
parameters contain (same-window . t) then display BUFFER in the
selected window.  If they contain (same-frame . t) then display BUFFER
in a window of the selected frame.

If ARGS is a list whose car is a symbol then use (car ARGS) as a
function to do the work: display the buffer and raise its frame.  Pass
it BUFFER as first argument, and (cdr ARGS) as the rest of the
arguments."
    (if (and args  (symbolp (car args)))
;;;   Should we let/make the FUNCTION that is (car ARGS) do everything, or should we
;;;   ensure that the frame is fit and raised?  For now, make FUNCTION do everything.
;;;   (let* ((window  (apply (car args) buffer (cdr args)))
;;;          (frame   (window-frame window)))
;;;     (when (fboundp 'fit-frame) (fit-frame (window-frame window)))
;;;     (raise-frame frame)
;;;     window)                         ; Return the window.
        (apply (car args) buffer (cdr args))
      (let ((window  (get-buffer-window buffer 0)))
        (or
         ;; If we have a window already, make it visible.
         (and window  (let ((frame  (window-frame window)))
                        (make-frame-visible frame)
                        (raise-frame frame)
                        (when (fboundp 'display-buffer-record-window) ; Emacs 24+
                          (display-buffer-record-window 'reuse window buffer))
                        (when (fboundp 'fit-frame) (fit-frame frame))
                        window))                ; Return the window.
         ;; Reuse the selected window if the caller requested it.
         (and (cdr (assq 'same-window args))
              (condition-case nil       ; Try Emacs 24 `switch-to-buffer' first.
                  (progn (switch-to-buffer buffer nil t) (selected-window))
                (error                  ; Try again, with old `switch-to-buffer'.
                 (condition-case nil
                     (progn (switch-to-buffer buffer) (selected-window))
                   (error nil)))))
         ;; Stay on the same frame if requested.
         (and (or (cdr (assq 'same-frame args))  (cdr (assq 'same-window args)))
              (let ((pop-up-windows                t)
                    (pop-up-frames                 nil)
                    (special-display-buffer-names  ())
                    (special-display-regexps       ()))
                (display-buffer buffer)))
         ;; If no window yet, make one in a new frame.
         ;; `make-frame' creates the frame before the buffer is shown in it, so do not
         ;; call `fit-frame' until we can select the buffer's window.
         (let* ((make-frame-functions  (delq 'fit-frame after-make-frame-functions))
                (frame                 (with-current-buffer buffer
                                         (make-frame
                                          (append args special-display-frame-alist))))
                (window                (frame-selected-window frame)))
           (set-window-buffer window buffer)
           (set-window-dedicated-p window t)
           (when (fboundp 'display-buffer-record-window) ; Emacs 24+
             (display-buffer-record-window 'frame window buffer))
           ;; Now call `fit-frame', with WINDOW selected.
           (save-selected-window (select-window window) (fit-frame))
           window))))))                 ; Return the window.

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'frame+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; frame+.el ends here
