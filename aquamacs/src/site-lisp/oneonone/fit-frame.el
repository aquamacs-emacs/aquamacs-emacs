;;; fit-frame.el --- Resize a frame to fit window, or resize it incrementally
;; 
;; Filename: fit-frame.el
;; Description: Resize a frame to fit its selected window, or resize it incrementally.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2000-2005, Drew Adams, all rights reserved.
;; Created: Thu Dec  7 09:32:12 2000
;; Version: 21.0
;; Last-Updated: Sat May 28 15:19:48 2005
;;           By: dradams
;;     Update #: 443
;; Keywords: internal, extensions, convenience, local
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;    Resize a frame to fit selected window, or resize it incrementally.
;;
;;  Commands and user options (variables) are provided here to resize
;;  (shrink-wrap) a frame to fit its selected window or to resize a
;;  frame incrementally.
;;
;;  The command to fit a frame to its selected window is `fit-frame'.
;;  The main user options for this command are`inhibit-fit-frame-flag'
;;  and `create-frame-max-*[-percent]'.
;;
;;  The commands to incrementally resize frames are `enlarge-frame'
;;  and `enlarge-frame-horizontally'. Sarir Khamsi
;;  [sarir.khamsi@raytheon.com] originally wrote `enlarge-frame',
;;  which he called `sk-grow-frame'.
;; 
;;  To take full advantage of the functionality provided here, load
;;  the companion file `auto-fit-frames.el' to modify primitives
;;  `display-buffer' and `switch-to-buffer' so they automatically fit
;;  all frames that have a single window. File `auto-fit-frames.el'
;;  loads `fit-frame.el'.
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;    (require 'fit-frame)
;;    (add-hook 'after-make-frame-functions 'fit-frame)
;;
;;  The second line here causes newly created frames to be fitted to
;;  their buffer (window). Even if you load `auto-fit-frames.el', you
;;  will still need to do this, because `display-buffer' and
;;  `switch-to-buffer' are not called when a new frame is created.
;;
;;  Suggested key bindings:
;;  
;;  (global-set-key [(control ?x) (control ?_)] 'fit-frame)
;;  (global-set-key [(control meta down)] 'enlarge-frame)
;;  (global-set-key [(control meta right)] 'enlarge-frame-horizontally)
;;  (global-set-key [(control meta up)] 'shrink-frame)
;;  (global-set-key [(control meta left)] 'shrink-frame-horizontally)
;;  (global-set-key [vertical-line down-mouse-1]
;;     'fit-frame-or-mouse-drag-vertical-line)
;;
;;  Customize the menu. Uncomment this to try it out.
;;
;;   (defvar menu-bar-frames-menu (make-sparse-keymap "Frames"))
;;   (define-key global-map [menu-bar frames]
;;     (cons "Frames" menu-bar-frames-menu)))
;;   (define-key menu-bar-frames-menu [fit-frame]
;;     '("Fit This Frame" . fit-frame))
;;
;;
;;  New functions defined here:
;;
;;    `create-frame-max-height', `create-frame-max-width',
;;    `enlarge-frame', `enlarge-frame-horizontally', `fit-frame',
;;    `fit-frame-or-mouse-drag-vertical-line', `maximize-frame',
;;    `minimize-frame', `restore-frame', `shrink-frame',
;;    `shrink-frame-horizontally'.
;;
;;  New user options (variables) defined here:
;;
;;    `create-empty-frame-height', `create-empty-frame-width',
;;    `create-empty-special-display-frame-height',
;;    `create-empty-special-display-frame-width',
;;    `create-frame-max-height', `create-frame-max-height-percent',
;;    `create-frame-max-width', `create-frame-max-width-percent',
;;    `create-frame-min-height', `create-frame-min-width',
;;    `fill-column-frame-margin', `inhibit-fit-frame-flag'.
;;
;;
;;  See also these files for other frame commands:
;;
;;     `auto-fit-frames.el' - See above.
;;
;;     `frame-cmds.el' - Various frame and window commands.
;;
;;     `doremi-frm.el' - Incrementally adjust frame properties
;;                       using arrow keys and/or mouse wheel.
;;
;;  This file was formerly called `shrink-fit.el', then
;;  `resize-frame.el', and command `fit-frame' was formerly called
;;  `shrink-frame-to-fit', then `resize-frame'.
;;
;;  Library `fit-frame' requires these libraries:
;;
;;    `avoid', `frame-cmds', `frame-fns', `icomplete', `icomplete+',
;;    `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 2005/05/25 dadams
;;     string-to-int -> string-to-number everywhere.
;; 2005/03/18 dadams
;;     Added: maximize-frame, restore-frame, minimize-frame.
;; 2004/12/18 dadams
;;     Updated Commentary to clarify use of after-make-frame-functions.
;; 2004/10/13 dadams
;;     Use special-display-p instead of special-display-buffer-p.
;; 2004/10/09 dadams
;;     Per request by RMS:
;;       Removed fit-1-window-frames-on (moved to compile-.el).
;;       Removed ;;;#autoload's.
;;       Renamed resize-* to fit-*.
;;       Changed defvar to defcustom.
;; 2004/10/02 dadams
;;     Per request by RMS:
;;       Renamed grow-frame-height and grow-frame-width to enlarge-frame
;;         and enlarge-frame-horizontally, respectively.
;;       Added shrink-frame and shrink-frame-horizontally.
;; 2004/08/26 dadams
;;     Added enlarge-frame and enlarge-frame-horizontally
;; 2004/06/01 dadams
;;     1. Removed making-frame-msg and making-frame-done-msg.
;;     2. Renamed shrink-* to resize-*. Renamed file.
;;     3. Renamed and reversed enable-* to inhibit-*.
;; 2004/05/07 dadams
;;     Updated to work with Emacs 21 and to work standalone.
;; 2004/04/06 dadams
;;     Removed nframe. make-frame is defined without it in Emacs 20.7.
;; 2001/01/05 dadams
;;     Protected show-frame via fboundp.
;; 2000/12/08 dadams
;;     Clarified doc strings: create-empty-frame-*, create-empty-special-*.
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

(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, unless
(require 'strings nil t)    ;; minibuffer-empty-p, read-number
(require 'frame-cmds nil t) ;; show-frame

;;;;;;;;;;;;;;;;;;;;;;;




;;; User options ---------------------------------------------------

(defgroup fit-frame nil
  "Resize a frame to fit its selected window, or resize it incrementally."
  :version "22.1" :group 'frames :group 'convenience)

(defcustom inhibit-fit-frame-flag nil
  "*If `nil', then command `fit-frame' does nothing.
You can bind this to non-`nil' to temporarily inhibit frame fitting:
    (let ((inhibit-fit-frame-flag t))...)"
  :type 'boolean :group 'fit-frame)

(defcustom create-frame-min-width 20
  "*Minimum width, in characters, for new frames created by `fit-frame'.
The actual minimum is at least the greater of this and `window-min-width'."
  :type 'integer :group 'fit-frame)

(defcustom create-frame-max-width nil
  "*Maximum width, in characters, for new frames created by `fit-frame'.
If `nil', then the function `create-frame-max-width' is used instead."
  :type '(choice (const :tag "Use `create-frame-max-width-percent' instead" nil)
                 integer)
  :group 'fit-frame)

(defcustom create-frame-max-width-percent 94
  "*Maximum percent of display width for a new frame created by `fit-frame'.
See function `create-frame-max-width'.
Not used unless `create-frame-max-width' is `nil'."
  :type 'integer :group 'fit-frame)

(defcustom create-frame-min-height window-min-height
  "*Minimum height, in lines, for new frames created by `fit-frame'.
The actual minimum is at least the greater of this and `window-min-height'."
  :type 'integer :group 'fit-frame)

(defcustom create-frame-max-height nil
  "*Maximum height, in lines, for new frames created by `fit-frame'.
If `nil', then the function `create-frame-max-height' is used instead."
  :type '(choice (const :tag "Use `create-frame-max-height-percent' instead" nil)
                 integer)
  :group 'fit-frame)

(defcustom create-frame-max-height-percent 82
  "*Maximum percent of display height for a new frame created by `fit-frame'.
See function `create-frame-max-height'.
Not used unless `create-frame-max-height' is `nil'."
  :type 'integer :group 'fit-frame)

(defcustom create-empty-frame-width (or (cdr (assq 'width default-frame-alist)) 80)
  "*Width, in characters, for new empty frames created by `fit-frame'."
  :type 'integer :group 'fit-frame)

(defcustom create-empty-frame-height (or (cdr (assq 'height default-frame-alist)) 35)
  "*Height, in lines, for new empty frames created by `fit-frame'."
  :type 'integer :group 'fit-frame)

(defcustom create-empty-special-display-frame-width 80
  "*Width, in chars, for new empty special-display frames created by `fit-frame'.
If this is nil, it is ignored."
  :type 'integer :group 'fit-frame)

(defcustom create-empty-special-display-frame-height 9
  "*Height, in lines, for new empty special-display frames created by `fit-frame'.
If this is nil, it is ignored."
  :type 'integer :group 'fit-frame)

(defcustom fill-column-frame-margin 7
  "*With negative prefix arg, `fit-frame' frame width is this plus `fill-column'.
Depending on the average word length of the language used in selected-window,
you may need different values for this. This variable is buffer-local."
  :type 'integer :group 'fit-frame)

(make-variable-buffer-local 'fill-column-frame-margin)



;;; Non-Interactive Functions -------------------------------------------

(defun create-frame-max-width ()
  "Maximum width, in characters, for new frames
when `fit-frame' is used in `after-make-frame-functions',
and `create-frame-max-width' is nil.

The value is relative to your display size and the frame's character
size, and depends on the value of `create-frame-max-width-percent':

  (/ (* create-frame-max-width-percent (x-display-pixel-width))
     (* 100 (frame-char-width)))"
  (/ (* create-frame-max-width-percent (x-display-pixel-width))
     (* 100 (frame-char-width))))

(defun create-frame-max-height ()
  "Maximum height, in characters, for new frames
when `fit-frame' is used in `after-make-frame-functions',
and `create-frame-max-height' is nil.

The value is relative to your display size and the frame's character
size, and depends on the value of `create-frame-max-height-percent':

  (/ (* create-frame-max-height-percent (x-display-pixel-height))
     (* 100 (frame-char-height)))"
  (/ (* create-frame-max-height-percent (x-display-pixel-height))
     (* 100 (frame-char-height))))




;;; Commands ---------------------------------------------------

(defun fit-frame (&optional frame width height)
  "Resize FRAME to fit its selected window.
Usable in `after-make-frame-functions'.

This does nothing if `inhibit-fit-frame-flag' is non-nil.

FRAME defaults to the current (i.e. selected) frame.  When FRAME arg
is supplied, the FRAME is shrunk to fit the window determined by
`select-frame'.

Interactively, supplying a non-negative prefix arg means you will be
prompted for the new frame width and height.  A negative prefix arg
means to use the current value of `fill-column', plus
`fill-column-frame-margin', for the new frame width; and the frame
height is not changed.

Otherwise, the new frame width and height will be as follows.

With no (or null) args WIDTH & HEIGHT:

  If the frame is empty (i.e. has only one window, with an empty
  buffer), then:

    If the frame's buffer is a special display buffer, then:
      The new width is `create-empty-special-display-frame-width'.
      The new height is `create-empty-special-display-frame-height'.

    Otherwise:
      The new width is `create-empty-frame-width'.
      The new height is `create-empty-frame-height'.

  If the frame is not empty, then:

    The new frame width is the maximum of:
      1) `create-frame-min-width',
      2) `window-min-width', and
      3) the minimum of: `create-frame-max-width' variable or, if nil,
                         `create-frame-max-width' function,
         and the widest line currently in the `selected-window'.

    The new frame height is the maximum of:
      1) `create-frame-min-height',
      2) `window-min-height', and
      3) the minimum of: `create-frame-max-height' variable or, if nil,
                         `create-frame-max-height' function,
         and the number of lines currently in the `selected-window'.

    Note that there are two intended uses of `create-frame-max-*':
      1) Use the variable, if you want to specify an absolute size, in
         characters.
      2) Use the function (variable = nil), if you want to specify a
         relative size, in percent of display size.  Frames will then
         appear the same relative size on different displays.

When used in `after-make-frame-functions', the current `frame-width' and
`frame-height' are those of the newly created frame.

If optional args WIDTH and HEIGHT are `natnump's:

    They are the values to use for the new frame size."
  (interactive
   (let ((option (prefix-numeric-value current-prefix-arg)))
     (list nil
           (and current-prefix-arg
                (if (natnump option)
                    (floor (string-to-number (read-string "New width: ")))
                  (+ fill-column fill-column-frame-margin)))
           (and current-prefix-arg
                (if (natnump option)
                    (floor (string-to-number (read-string "New height: ")))
                  (frame-height))))))
  (setq frame (or frame (selected-frame)))

  (unless inhibit-fit-frame-flag
    (let ((max-width 0)
          (nb-lines 2)                  ; Minimum is 1 for empty + 1 extra.
          empty-buf-p specbuf-p)
      
      ;; Get minimum frame height, `nb-lines'.
      ;;
      ;; *** THIS NO DOUBT NEEDS SOME MORE TWEAKING FOR EMACS 22 ***
      ;;
      ;; Apparently, in Emacs 21 `set-frame-size' includes tool-bar
      ;; and minibuffer.  It doesn't seem to include menu-bar.  This
      ;; `nb-lines' calculation should really use frame's current
      ;; minibuffer height (not just 1).  And perhaps a possible
      ;; horizontal scroll bar also needs to be taken into account.
      (when (>= emacs-major-version 21)
	(let* ((fparams (frame-parameters frame))
               (tool-bar-lines (cdr (assq 'tool-bar-lines fparams)))
               (minibuf        (cdr (assq 'minibuffer     fparams))))
	  (setq nb-lines (+ nb-lines tool-bar-lines (if minibuf 1 0)
                            2))))       ; Tweak factor - not sure why it's needed.
      
 ;   (progn (print "maybe normal buffer!") 0)
	;	      (print (buffer-name))
      (my-my-set-frame-size
       ;; Frame
       frame
       ;; Columns
       (or width
           (save-window-excursion
             (select-frame frame)
             (and (setq empty-buf-p (= (point-min) (point-max)))
                  (one-window-p (selected-window))
                  (if (setq specbuf-p
                            (special-display-p (buffer-name (window-buffer))))
                      create-empty-special-display-frame-width
                    create-empty-frame-width)))
           (max create-frame-min-width window-min-width
		
                (min (or create-frame-max-width (create-frame-max-width))
                     (save-window-excursion
                       (select-frame frame)
		      
                       (save-excursion
                         (goto-char (point-min))
                         (while (not (eobp))
                           (end-of-line)
                           (setq max-width (max (current-column) max-width))
                           (forward-line 1) 
                           (setq nb-lines (1+ nb-lines))))
                       (setq max-width (1+ max-width))))))

       ;; Rows
       (or height
           (and empty-buf-p (if specbuf-p
                                create-empty-special-display-frame-height
                              create-empty-frame-height))
           (max create-frame-min-height window-min-height
                (min (or create-frame-max-height (create-frame-max-height))
                     nb-lines))))

f
;      (when (fboundp 'show-frame) (show-frame frame))
)
)) ; Defined in `frame-cmds.el'.


(defun my-my-set-frame-size (f width height)
  ; (debug)
 ; (print (list "my-my-set-frame-size" f width height))
  (set-frame-size f width height)
; (sleep-for 0.3)
)
  

;; Inspired by `sk-grow-frame' from Sarir Khamsi [sarir.khamsi@raytheon.com]
(defun enlarge-frame (&optional increment frame)
  "Increase the height of FRAME (default: selected-frame) by INCREMENT.
Interactively, INCREMENT is given by the prefix argument."
  (interactive "p")
  (set-frame-height frame (+ (frame-height frame) increment)))

(defun enlarge-frame-horizontally (&optional increment frame)
  "Increase the width of FRAME (default: selected-frame) by INCREMENT.
Interactively, INCREMENT is given by the prefix argument."
  (interactive "p")
  (set-frame-width frame (+ (frame-width frame) increment)))

(defun shrink-frame (&optional increment frame)
  "Decrease the height of FRAME (default: selected-frame) by INCREMENT.
Interactively, INCREMENT is given by the prefix argument."
  (interactive "p")
  (set-frame-height frame (- (frame-height frame) increment)))

(defun shrink-frame-horizontally (&optional increment frame)
  "Decrease the width of FRAME (default: selected-frame) by INCREMENT.
Interactively, INCREMENT is given by the prefix argument."
  (interactive "p")
  (set-frame-width frame (- (frame-width frame) increment)))

(defun fit-frame-or-mouse-drag-vertical-line (start-event)
  "If only window in frame, `fit-frame'; else `mouse-drag-vertical-line'"
  (interactive "e")
  (if (one-window-p t) (fit-frame) (mouse-drag-vertical-line start-event)))

;; Note that in Windows you can also just double-click the title bar
;; of a frame to alternately maximize and restore it.
(when (eq window-system 'w32)
  (defun restore-frame (&optional frame)
    "Restore FRAME to previous size (default: current frame)."
    (interactive)
    (w32-send-sys-command 61728 frame)))

(when (eq window-system 'w32)
  (defun maximize-frame (&optional frame)
    "Maximize FRAME (default: current frame)."
    (interactive)
    (w32-send-sys-command 61488 frame)))

(when (eq window-system 'w32)
  (defalias 'minimize-frame (if (fboundp 'really-iconify-frame)
                                'really-iconify-frame
                              'iconify-frame)))

;;; Helper Functions, to enable this file to work standalone --------------------

;; Defined in `strings.el'.
(or (fboundp 'minibuffer-empty-p)
    (defun minibuffer-empty-p ()
      "Returns non-nil iff minibuffer is empty.
Sets variable `minibuffer-empty-p' to returned value."
      (save-excursion
        (save-window-excursion
          (set-buffer (window-buffer (minibuffer-window)))
          (set-minibuffer-empty-p (= 0 (buffer-size)))))))

;; Defined in `strings.el'.
(or (fboundp 'set-minibuffer-empty-p)
    (defun set-minibuffer-empty-p (flag)
      "Set value of variable `set-minibuffer-empty-p' to FLAG."
      (setq minibuffer-empty-p flag)))

;; Defined in `frame-cmds.el'.
(or (fboundp 'show-frame)
    (defun show-frame (frame)
      "Make FRAME visible and raise it, without selecting it.
FRAME may be a frame or its name."
      (interactive (list (read-frame "Frame to make visible: ")))
      (setq frame (get-a-frame frame))
      (make-frame-visible frame)
      (raise-frame frame)))

;; Defined in `frame-fns.el'.
(or (fboundp 'get-a-frame)
    (defun get-a-frame (frame)
      "Return a frame, if any, named FRAME (a frame or a string).
If none, return nil.
If FRAME is a frame, it is returned."
      (cond ((framep frame) frame)
            ((stringp frame)
             (let ((frs (frame-list))
                   (found-fr nil))
               (while (and frs (not found-fr))
                 (when (string= frame (get-frame-name (car frs)))
                   (setq found-fr (car frs)))
                 (setq frs (cdr frs)))
               found-fr))
            (t (error "GET-A-FRAME:  Arg neither a string nor a frame: %s." frame)))))


;; Defined in `frame-fns.el'.
(or (fboundp 'get-frame-name)
    (defun get-frame-name (&optional frame)
      "Return the string that names FRAME (a frame).  Default is selected frame."
      (or frame (setq frame (selected-frame)))
      (if (framep frame)
          (cdr (assq 'name (frame-parameters frame)))
        (error "GET-FRAME-NAME:  Argument not a frame: %s." frame))))

;;;;;;;;;;

(provide 'fit-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fit-frame.el ends here
