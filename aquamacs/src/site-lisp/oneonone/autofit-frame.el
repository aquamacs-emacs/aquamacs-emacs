;;; autofit-frame.el --- Automatically resize one-window frames to fit
;; 
;; Filename: autofit-frame.el
;; Description: Automatically resize one-window frames to fit.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2000-2005, Drew Adams, all rights reserved.
;; Created: Thu Dec  7 10:06:18 2000
;; Version: 21.0
;; Last-Updated: Sat Jan 08 17:31:37 2005
;;           By: dradams
;;     Update #: 340
;; Keywords: internal, extensions, convenience, local
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;    Automatically resize one-window frames to fit.
;; 
;;  Functions are provided here to automatically resize each frame to
;;  fit its selected window, when there is no other window in the
;;  frame.  Standard Emacs primitive functions are redefined to do
;;  this: `display-buffer' and `switch-to-buffer'.
;; 
;;  Automatic frame resizing is also provided here implicitly for
;;  functions `pop-to-buffer', `switch-to-buffer-other-window', and
;;  `switch-to-buffer-other-frame', since they ultimately use
;;  `display-buffer'. (Command `switch-to-buffer' does not use
;;  `display-buffer', so it is redefined separately here.)
;;
;;  Put the following in your Emacs initialization file (`~/.emacs'),
;;  in order to provide for automatic frame resizing:
;;
;;    (require 'autofit-frame)
;;    (add-hook 'after-make-frame-functions 'fit-frame)
;;
;;  The second line here causes newly created frames to be fitted to
;;  their buffer (window). Even if you load `auto-fit-frames.el', you
;;  will still need to do this, because `display-buffer' and
;;  `switch-to-buffer' are not called when a new frame is created.
;;
;;  To automatically fit frames that show a temporary buffer in their
;;  sole window, add this to your initialization file also:
;;
;;    (add-hook 'temp-buffer-show-hook
;;              'fit-frame-if-one-window 'append)
;;
;;  User option (variable) `autofit-frames-flag' turns on and off the
;;  automatic resizing defined here.  Setting it to nil turns it off:
;;  (setq autofit-frames-flag nil).  You can also bind it to nil to
;;  temporarily inhibit frame resizing in Lisp code:
;;
;;         (let ((autofit-frames-flag nil))...)
;;
;;
;;  New user option (variable) defined here: `autofit-frames-flag'.
;;
;;  New function defined here (useful as a `temp-buffer-show-hook'):
;;
;;    `fit-frame-if-one-window'.
;;
;;
;;  ***** NOTE: The following EMACS PRIMITIVES are REDEFINED HERE:
;;
;;  `display-buffer' - 
;;     1) Uses `read-buffer' in interactive spec.
;;     2) Resizes frame to fit sole window if `autofit-frames-flag'.
;;
;;     ***************************************************************
;;     NOTE: It would be better to rewrite the C code of
;;           `display-buffer' so that the frame is _not_ resized if an
;;           existing frame is simply raised. This is because the user
;;           may have manually resized the frame, and we don't want to
;;           override this. The code here unfortunately does _not_
;;           respect the user in this regard.
;;     ***************************************************************
;;
;;  `switch-to-buffer' - 
;;     1) Uses `read-buffer' in interactive spec.
;;     2) If current window is dedicated, then use another window.
;;     3) Resizes frame to fit sole window if `autofit-frames-flag'
;;        (unless BUFFER is already the `current-buffer').
;;
;;
;;  This file loads file `fit-frame.el', which provides the main
;;  functionality behind the automatic frame resizing.  See it for
;;  user options to do such things as customize default frame sizes:
;;
;;  `create-empty-frame-height', `create-empty-frame-width',
;;  `create-empty-special-display-frame-height',
;;  `create-empty-special-display-frame-width',
;;  `create-frame-max-height', `create-frame-max-height-percent',
;;  `create-frame-max-width', `create-frame-max-width-percent',
;;  `create-frame-min-height', `create-frame-min-width', and
;;  `inhibit-fit-frame-flag'.
;;
;;  The reason for separating the code here from that in
;;  `fit-frame.el' is to let you load that code but not load the code
;;  here, if you do not want to redefine Emacs primitives.
;; 
;;  This file was formerly called `shrink-fit-all.el', then
;;  `auto-resize-frames.el'.
;;
;;
;;  See also these files for other frame commands:
;;
;;     `doremi-frm.el'    - Incrementally adjust frame properties
;;                          using arrow keys and/or mouse wheel.
;;
;;     `frame-cmds.el'    - Miscellaneous frame and window commands.
;;
;;     `thumb-frm.el'     - Shrink frames to a thumbnail size and
;;                          restore them again.
;;
;;     `zoom-frm.el'      - Zoom a frame, so that its font becomes
;;                          larger or smaller.
;;
;;
;;  Library `autofit-frame' requires these libraries:
;;
;;    `avoid', `fit-frame', `frame-cmds', `frame-fns', `icomplete',
;;    `icomplete+', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 2004/10/07 dadams
;;      Renamed resize-* to fit-*, per RMS.
;;      Renamed automatically-resize-frames-p to autofit-frames-flag.
;; 2004/09/21 dadams
;;      Commented-out redefinition of pop-to-buffer. Removed it from intro.
;; 2004/06/01 dadams
;;      1. Replaced fit-frame-when-display-p, fit-frame-when-pop-to-p,
;;         and fit-frame-when-switch-to-p by
;;         automatically-resize-frames-p.
;;      2. Removed pop-to-buffer and
;;         resize-frame-if-one-window-and-cond.
;;      3. Renamed fit-frame-* to resize-frame-*. Renamed file.
;;      4. switch-to-buffer: Don't resize if buffer is already current.
;; 2004/05/07 dadams
;;      Updated to work with Emacs 21:
;;        pop-to-buffer has additional arg and doc-string changes
;;        display-buffer has doc-string changes
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

(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when
(require 'strings nil t) ;; read-buffer
(require 'fit-frame) ;; fit-frame

;;;;;;;;;;;;;;;;;;;;;;;




;;; User option ---------------------------------------------------

;;;###autoload
(defvar autofit-frames-flag t
  "*Non-nil means automatically resize one-window frames to fit buffer.")



;;; Non-interactive functions ---------------------------------

;; This is not used here. It is useful as a `temp-buffer-show-hook':
;;   (add-hook 'temp-buffer-show-hook 'fit-frame-if-one-window 'append)
;;;###autoload
(defun fit-frame-if-one-window ()
  "Resize frame to fit selected window if it is alone in the frame.
Usable in `temp-buffer-show-hook'.
This does nothing if `autofit-frames-flag' is nil."
  (and (one-window-p t) autofit-frames-flag (fit-frame)))


;; ;; REPLACES ORIGINAL (built-in) - prior to Emacs 20 only: 
;; ;; Resizes frame to fit sole window if `autofit-frames-flag'.
;; ;;
;; ;; Prior to Emacs 20, `pop-to-buffer' did not call `display-buffer',
;; ;; and did not have an optional NORECORD arg, so redefine
;; ;; `pop-to-buffer' for Emacs < 20.
;; (when (< emacs-major-version 20)
;;   (or (fboundp 'old-pop-to-buffer)
;;       (fset 'old-pop-to-buffer (symbol-function 'pop-to-buffer)))
;;   (defun pop-to-buffer (bufname &optional other-window)
;;     "Select buffer BUFFER in some window, preferably a different one.
;; If BUFFER is nil, then some other buffer is chosen.
;; If `pop-up-windows' is non-nil, windows can be split to do this.
;; If optional second arg OTHER-WINDOW is non-nil, insist on finding another
;; window even if BUFFER is already visible in the selected window.

;; Resizes frame if `one-window-p' and `autofit-frames-flag'"
;;     (old-pop-to-buffer bufname other-window)
;;     (and (one-window-p t) autofit-frames-flag (fit-frame))))




;;; Commands ---------------------------------------------------


(or (fboundp 'old-display-buffer)
    (fset 'old-display-buffer (symbol-function 'display-buffer)))

;; REPLACES ORIGINAL (built-in):
;; 1) Uses `read-buffer' in interactive spec.
;; 2) Resizes frame to fit sole window if `autofit-frames-flag'.
;;
;; NOTE: It would be better to rewrite the C code, so that the frame
;;       is not resized if the frame is simply _raised_.
;;;###autoload
(defun display-buffer (buffer &optional not-this-window frame)
  "Make BUFFER appear in some window but don't select it.
BUFFER can be a buffer or a buffer name.
If BUFFER is shown already in some window, just use that one,
unless the window is the selected window and the optional second
argument NOT-THIS-WINDOW is non-nil (interactively, with prefix arg).
If `pop-up-frames' is non-nil, make a new frame if no window shows BUFFER.
Returns the window displaying BUFFER.

Emacs 21 only:
  If `display-buffer-reuse-frames' is non-nil, and another frame is
  currently displaying BUFFER, then simply raise that frame.

The variables `special-display-buffer-names', `special-display-regexps',
`same-window-buffer-names', and `same-window-regexps' customize how certain
buffer names are handled.

If optional argument FRAME is `visible', search all visible frames.
If FRAME is 0, search all visible and iconified frames.
If FRAME is t, search all frames.
If FRAME is a frame, search only that frame.
If FRAME is nil, search only the selected frame
 (actually the last nonminibuffer frame),
 unless `pop-up-frames' or `display-buffer-reuse-frames' is non-nil,
 which means search visible and iconified frames.

Emacs 21 only:
  If `even-window-heights' is non-nil, window heights will be evened
  out if displaying the buffer causes two vertically adjacent windows
  to be displayed.

Resizes frame to fit sole window if `autofit-frames-flag'."
  (interactive
   (list (read-buffer "Display buffer: " (current-buffer) 'existing)
         current-prefix-arg))
  (let ((win (old-display-buffer buffer not-this-window frame)))
    (when (window-live-p win)
      (save-selected-window
        (select-window win)
        (and (one-window-p t) autofit-frames-flag (fit-frame))))
    win))                               ; Return the window



(or (fboundp 'old-switch-to-buffer)
    (fset 'old-switch-to-buffer (symbol-function 'switch-to-buffer)))

;; REPLACES ORIGINAL (built-in):
;; 1) Uses `read-buffer' in interactive spec.
;; 2) If current window is dedicated, then use another window.
;;    NOTE: Emacs versions >= 19.34 signal an error if dedicated window,
;;          instead of using another one.  Don't know what the 19.28 version did.
;;          Don't know if this is needed any more.
;; 3) Resizes frame to fit sole window if `autofit-frames-flag'
;;    (unless BUFFER is already the `current-buffer').
;;;###autoload
(defun switch-to-buffer (buffer &optional norecord)
  "Select buffer BUFFER in current window, unless the window is dedicated.
If current window is dedicated (`window-dedicated-p'), then another window
is used.  BUFFER may be a buffer or its name.

Optional second arg NORECORD non-nil =>
   Do not put BUFFER at front of list of recently selected buffers.

*WARNING*: This is NOT the way to work on another buffer temporarily
within a Lisp program!  Use `set-buffer' instead, to avoid messing
with window-buffer correspondences.

Resizes frame to fit sole window if `autofit-frames-flag'
\(unless BUFFER is already the `current-buffer')."
  (interactive
   (list (read-buffer "Switch to buffer: " 
		      (other-buffer (current-buffer)) 'existing)))
  (setq buffer (get-buffer-create buffer)) ; If arg is a string, convert it to a buffer.
  (let ((orig-buf (current-buffer)))
    (if (window-dedicated-p (selected-window))
        (switch-to-buffer-other-window buffer)
      (old-switch-to-buffer buffer norecord))
    (and (one-window-p t)
         (not (eq buffer orig-buf))     ; Don't resize if same buffer.
         autofit-frames-flag
         (fit-frame))))



;;;;;;;;;;;;;;;;;;;;;;;

(provide 'autofit-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autofit-frame.el ends here
