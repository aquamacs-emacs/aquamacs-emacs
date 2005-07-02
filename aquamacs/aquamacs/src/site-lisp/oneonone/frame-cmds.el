;;; frame-cmds.el --- Frame and window commands (interactive functions).
;; 
;; Filename: frame-cmds.el
;; Description: Frame and window commands (interactive functions).
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2005, Drew Adams, all rights reserved.
;; Created: Tue Mar  5 16:30:45 1996
;; Version: 21.0
;; Last-Updated: Sat May 28 14:59:09 2005
;;           By: dradams
;;     Update #: 1952
;; Keywords: internal, extensions, mouse, local, frames, windows, convenience
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;;    Frame and window commands (interactive functions).
;;
;;
;;  Summary:
;; 
;;    Load this library from your init file (~/.emacs or _emacs).
;;    Add the suggested key bindings (below) to  your init file.
;;    Use `C-M-z' or `C-x C-z' to iconify/hide all frames.
;;    Use `C-M-z' in a lone frame to restore all frames.
;;    Use `C-mouse-1' in the minibuffer to restore all frames.
;;    Use `C-mouse-1' in Dired to mark/unmark a file.
;;    Use `C-mouse-3' on the mode line to remove window from frame.
;;    Use `tile-frames-horizontally', `-vertically' to tile frames.
;;
;;  Note on saving changes made with the commands defined here:
;;
;;    Some of the commands defined here change frame properties. You
;;    can save any changes you have made, by using Customize. To visit
;;    a Customize buffer of all unsaved changes you have made, use
;;    command `customize-customized'.
;;
;;    Frame parameter changes, such as background color, can be saved
;;    for future use by all frames or all frames of a certain
;;    kind. For that, you must change the frame parameters of the
;;    correponding frame-alist variable.
;;
;;    There is no single variable for saving changes to parameters of
;;    the current frame. Instead, there are several different
;;    frame-alist variables, which you can use to define different
;;    kinds of frames. These include: `default-frame-alist',
;;    `initial-frame-alist', and `special-display-frame-alist'. The
;;    complete list of such frame alist variables is available using
;;    function `frame-alist-var-names', defined here.
;;
;;    Example: Suppose you change the background color of a frame and
;;    want to make that the default background color for new frames in
;;    the future. You will need to update the value of variable
;;    `default-frame-alist' to use the `background-color' parameter
;;    setting of the changed frame.
;;
;;    You can easily copy one or all parameter values from any given
;;    frame to any frame alist (such as `default-frame-alist'), by
;;    using the commands `set-frame-alist-parameter-from-frame' and
;;    `set-all-frame-alist-parameters-from-frame'. Those commands are
;;    defined here.
;;
;;
;;  New user options defined here:
;;
;;    `frame-config-register', `frame-parameters-to-exclude',
;;    `rename-frame-when-iconify-flag', `show-hide-show-function',
;;    `window-mgr-title-bar-pixel-width'.
;;
;;  New commands defined here:
;;
;;    `delete-1-window-frames-on', `delete/iconify-window',
;;    `delete/iconify-windows-on', `enlarge-font', `hide-everything',
;;    `hide-frame', `iconify-everything', `iconify/map-frame',
;;    `jump-to-frame-config-register', `mouse-iconify/map-frame',
;;    `mouse-remove-window', `mouse-show-hide-mark-unmark',
;;    `remove-window', `remove-windows-on', `rename-frame',
;;    `rename-non-minibuffer-frame', `save-frame-config',
;;    `set-all-frame-alist-parameters-from-frame',
;;    `set-frame-alist-parameter-from-frame', `show-*Help*-buffer',
;;    `show-a-frame-on', `show-buffer-menu', `show-frame',
;;    `show-hide', `tell-customize-var-has-changed', `tile-frames',
;;    `tile-frames-horizontally', `tile-frames-vertically'.
;;
;;  New non-interactive (helper) functions defined here:
;;
;;    `args-for-tile-frames', `frame-alist-var-names',
;;    `frame-iconified-p', `frame-parameter-names'.
;;
;;
;;
;;  ***** NOTE: The following EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `delete-window' - If only one window in frame, `delete-frame'.
;;  `delete-windows-on' - 
;;     1) Uses `read-buffer'.
;;     2) Calls `delete-window', so this also deletes frames where
;;        window showing the BUFFER is the only window.
;;
;;
;;  Suggested key bindings:
;;
;;   (global-set-key [(control ?x) (control ?z)] 'iconify-everything)
;;   (global-set-key [vertical-line S-down-mouse-1] 'iconify-everything)
;;   (global-set-key [(control ?z)] 'iconify/map-frame)
;;   (global-set-key [mode-line mouse-3] 'mouse-iconify/map-frame)
;;   (global-set-key [mode-line C-mouse-3] 'mouse-remove-window)
;;   (global-set-key [(control meta ?z)] 'show-hide)
;;   (global-set-key [vertical-line C-down-mouse-1] 'show-hide)
;;   (global-set-key [C-down-mouse-1] 'mouse-show-hide-mark-unmark)
;;   (substitute-key-definition 'delete-window 'remove-window global-map)
;;   (define-key ctl-x-5-map "h" 'show-*Help*-buffer)
;;   (substitute-key-definition 'delete-window 'remove-window global-map)
;;   (define-key global-map "\C-xt." 'save-frame-config)
;;
;;
;;  See also these files for other frame commands:
;;
;;     `autofit-frame.el' - Automatically fit each frame to its
;;                          selected window. This uses `fit-frame.el'.
;;
;;     `fit-frame.el'     - 1) Fit a frame to its selected window.
;;                          2) Incrementally resize a frame.
;;
;;     `doremi-frm.el'    - Incrementally adjust frame properties
;;                          using arrow keys and/or mouse wheel.
;;
;;     `thumb-frm.el'     - Shrink frames to a thumbnail size and
;;                          restore them again.
;;
;;     `zoom-frm.el'      - Zoom a frame, so that its font becomes
;;                          larger or smaller.
;;
;;  Suggested key bindings:
;;
;;  (defalias 'doremi-prefix (make-sparse-keymap))
;;  (defvar doremi-map (symbol-function 'doremi-prefix)
;;    "Keymap for Do Re Mi commands.")
;;  (define-key global-map "\C-xt" 'doremi-prefix)
;;  (define-key doremi-map "." 'save-frame-config)
;;
;;  Customize the menu. Uncomment this to try it out.
;;
;;   (defvar menu-bar-doremi-menu (make-sparse-keymap "Do Re Mi"))
;;   (define-key global-map [menu-bar doremi]
;;     (cons "Do Re Mi" menu-bar-doremi-menu))
;;   (define-key menu-bar-doremi-menu [doremi-font]
;;     '("Save Frame Configuration" . save-frame-config))
;;
;;   (defvar menu-bar-frames-menu (make-sparse-keymap "Frames"))
;;   (define-key global-map [menu-bar frames]
;;     (cons "Frames" menu-bar-frames-menu)))
;;   (define-key menu-bar-frames-menu [set-all-params-from-frame]
;;     '("Set All Frame Parameters from Frame"
;;       . set-all-frame-alist-parameters-from-frame))
;;   (define-key menu-bar-frames-menu [set-param-from-frame]
;;     '("Set Frame Parameter from Frame"
;;       . set-frame-alist-parameter-from-frame))
;;   (define-key menu-bar-frames-menu [tile-frames-vertically]
;;     '("Tile Frames Vertically" . tile-frames-vertically))
;;   (define-key menu-bar-frames-menu [tile-frames-horizontally]
;;     '("Tile Frames Horizontally" . tile-frames-horizontally))
;;   (define-key menu-bar-frames-menu [iconify-everything]
;;     '("Iconify All Frames" . iconify-everything))
;;   (define-key menu-bar-frames-menu [show-hide]
;;     '("Hide Frames / Show Buffers" . show-hide))
;;
;;  Library `frame-cmds' requires these libraries:
;;
;;    `avoid', `frame-fns', `icomplete', `icomplete+', `strings',
;;    `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 2005/05/28 dadams
;;     show-a-frame-on: Use another-buffer as default for read-buffer, if available.
;; 2005/05/15 dadams
;;     Renamed: minibuffer-frame to 1on1-minibuffer-frame.
;; 2005/05/10 dadams
;;     remove-window: Removed definition; just defalias it to delete-window.
;;     delete-window: (one-window-p) -> (one-window-p t).
;;     set-frame-alist-parameter-from-frame: No longer use destructive fns.
;; 2005/01/19 dadams
;;     set-all-frame-alist-parameters-from-frame:
;;            Added really-all-p and use frame-parameters-to-exclude.
;;     Added: frame-parameters-to-exclude, tell-customize-var-has-changed.
;; 2005/01/18 dadams
;;     Added: set-all-frame-alist-parameters-from-frame, set-frame-alist-parameter-from-frame,
;;            frame-alist-var-names, frame-parameter-names.
;;     Added Note on saving changes.
;; 2005/01/08 dadams
;;     Moved enlarge-font here from doremi-frm.el, where it was called doremi-grow-font.
;; 2005/01/04 dadams
;;     Added rename-frame-when-iconify-flag. 
;;       Use it in iconify-everything, (mouse-)iconify/map-frame.
;;     Added (defgroup frame-cmds).
;; 2004/12/23 dadams
;;     frame-config-register, show-hide-show-function, window-mgr-title-bar-pixel-width:
;;         Changed defvar to defcustom.
;; 2004/12/21 dadams
;;     hide-everything, iconify-everything: bind thumbify-instead-of-iconify-flag to nil.
;; 2004/12/10 dadams
;;     tile-frames: Change 15 to (frame-char-height fr) for scroll-bar-width.
;;     tile-frames-*: Corrected doc strings for non-interactive case.
;; 2004/12/09 dadams
;;     Changed compile-time require of strings to a soft require.
;; 2004/10/11 dadams
;;     args-for-tile-frames: Fixed bug when non-existant frame in name history.
;;     tile-frames: show-frame at end (for case where use prefix arg)
;; 2004/09/11 dadams
;;     Moved to doremi-frm.el: frame-config-ring*, frame-config-wo-parameters,
;;                             push-frame-config.
;; 2004/09/07 dadams
;;     Added: jump-to-frame-config-register, push-frame-config, save-frame-config.
;; 2004/09/01 dadams
;;     Added: frame-config-register, show-hide-show-function,
;;            jump-to-frame-config-register.
;;     Rewrote to record frame config: iconify-everything, hide-everything.
;;     Rewrote to use show-hide-show-function: show-hide.
;; 2004/03/22 dadams
;;     Added: tile-frames, tile-frames-vertically, args-for-tile-frames.
;;     Rewrote tile-frames-horizontally to use tile-frames.
;; 2004/03/19 dadams
;;     Added tile-frames-horizontally.
;; 2000/11/27 dadams
;;     hide-frame: fixed bug: Added get-a-frame for frame name read.
;; 2000/09/27 dadams
;;     1. Added: frame-iconified-p.
;;     2. remove-window: only make-frame-invisible if not iconified (HACK).
;; 1999/10/05 dadams
;;     rename-frame: fixed bug if only 1 frame and old-name was a frame.
;; 1999/08/25 dadams
;;     Added: hide-everything, show-buffer-menu, show-hide.
;; 1999/03/17 dadams
;;     delete-1-window-frames-on: ensure a buffer object (not a name).
;; 1996/04/26 dadams
;;     delete/iconify-windows-on, show-a-frame-on: Do nothing if null buffer.
;; 1996/03/12 dadams
;;     delete/iconify-window: Unless one-window-p, do old-delete-window outside of
;;                            save-window-excursion.
;; 1996/03/08 dadams
;;     1. delete-windows-on: a. Fixed incorrect interactive spec (bad paren).
;;                           b. Second arg FRAME also provided interactively now.
;;     2. Added: delete/iconify-window, delete/iconify-windows-on.
;; 1996/02/27 dadams
;;     show-frame: Call make-frame-visible.
;; 1996/02/09 dadams
;;     Added show-*Help*-buffer.
;; 1996/01/30 dadams
;;     1. show-frame: Don't make-frame-visible.  Done by raise-frame anyway.
;;     2. Added show-a-frame-on.
;; 1996/01/09 dadams
;;     Added delete-windows-on and made it interactive.
;; 1996/01/08 dadams
;;     Added rename-non-minibuffer-frame.  Use in iconify-everything,
;;           iconify/map-frame, mouse-iconify/map-frame.
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

(eval-when-compile (require 'cl)) ;; incf, case, set-difference
                                  ;; (plus, for Emacs 20: dolist and, for Emacs <20: when, unless)
(require 'frame-fns) ;; frames-on, get-frame-name, get-a-frame, read-frame
(require 'strings nil t) ;; (no error if not found) read-buffer
(require 'misc-fns nil t) ;; (no error if not found) another-buffer
(require 'icomplete+ nil t) ;; (no error if not found): read-from-minibuffer


;;;;;;;;;;;;;;;;;;;;;;;




;;; USER OPTIONS (VARIABLES) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup frame-cmds nil
  "Miscellaneous frame and window commands."
  :version "22.1" :group 'frames)

(defcustom rename-frame-when-iconify-flag t
  "*If non-`nil', then frames are renamed when iconified.
The new name is the name of the current buffer."
  :type 'boolean :group 'frame-cmds)

(defcustom frame-config-register ?\C-l  ; Control-L is the name of the register.
  "*Character naming register for saving/restoring frame configuration."
  :type 'character :group 'frame-cmds)

(defcustom show-hide-show-function 'jump-to-frame-config-register
  "*Function to show stuff that is hidden or iconified by `show-hide'.
Candidates include `jump-to-frame-config-register' and `show-buffer-menu'."
  :type '(choice (const :tag "Restore frame configuration" jump-to-frame-config-register)
		 (function :tag "Another function"))
  :group 'frame-cmds) 

(defcustom window-mgr-title-bar-pixel-width 30
  "*Width of frame title bar provided by the window manager, in pixels.
There is no way for Emacs to determine this, so you must set it."
  :type 'integer :group 'frame-cmds)

(defcustom frame-parameters-to-exclude '((window-id) (buffer-list) (name) (title) (icon-name))
  "Parameters to exclude in `set-all-frame-alist-parameters-from-frame'.
An alist of the same form as that returned by `frame-parameters'.
The cdr of each alist element is ignored.
These frame parameters are not copied to the target alist."
  :type '(repeat (cons symbol sexp)) :group 'frame-cmds)



;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defun save-frame-config ()
  "Save current frame configuration. You can restore it with \\[jump-to-frame-config-register]"
  (interactive)
  (frame-configuration-to-register frame-config-register)
  (when (featurep 'doremi-frm) (push-current-frame-config))
  (message
   (substitute-command-keys
    (if (featurep 'doremi-frm)
        (format "Use `\\[jump-to-frame-config-register]' (`C-x r j %c') or \
`\\[doremi-frame-configs]' to restore frames as before (undo)." frame-config-register)
      "Use `\\[jump-to-frame-config-register]' to restore frames as before (undo)."))))

;;;###autoload
(defun jump-to-frame-config-register ()
  "Restore frame configuration saved in `frame-config-register'."
  (interactive)
  (jump-to-register frame-config-register))

;;;###autoload
(defun iconify-everything ()
  "Iconify all frames of session at once.
Remembers frame configuration in register `C-l' (Control-L).
To restore this frame configuration, use `\\[jump-to-register] C-l'."
  (interactive)
  (frame-configuration-to-register frame-config-register)
  (let ((thumbify-instead-of-iconify-flag nil)) ; Defined in `thumb-frm.el'.
    (dolist (frame (visible-frame-list))
      (when rename-frame-when-iconify-flag (rename-non-minibuffer-frame frame))
      (iconify-frame frame))))

;;;###autoload
(defun hide-everything ()
  "Hide all frames of session at once.
Iconify minibuffer frame; make all others invisible.
Remembers frame configuration in register `C-l' (Control-L).
To restore this frame configuration, use `\\[jump-to-register] C-l'."
  (interactive)
  (frame-configuration-to-register frame-config-register)
  (let ((minibuf-frame-name (and (boundp '1on1-minibuffer-frame)
                                 (cdr (assq 'name (frame-parameters 1on1-minibuffer-frame)))))
        (thumbify-instead-of-iconify-flag nil)) ; Defined in `thumb-frm.el'.
    (dolist (frame (frame-list))
      (if (eq minibuf-frame-name (cdr (assq 'name (frame-parameters frame))))
          (iconify-frame frame)         ; minibuffer frame
        (make-frame-invisible frame t))))) ; other frames
    
;;;###autoload
(defun show-hide ()
  "1 frame visible: `show-hide-show-function'; else: `hide-everything'."
  (interactive)
  (if (< (length (visible-frame-list)) 2)
      (funcall show-hide-show-function)
    (hide-everything)))
    
;;;###autoload
(defun show-buffer-menu ()
  "Call `buffer-menu' after making all frames visible.
Useful after using `hide-everything' because of a Windows bug that
doesn't let you display frames that have been made visible after 
being made invisible."
  (interactive)
  (let ((minibuf-frame-name
         (and (boundp '1on1-minibuffer-frame)
              (cdr (assq 'name (frame-parameters 1on1-minibuffer-frame))))))
    (dolist (frame (frame-list))
      (if (eq minibuf-frame-name
              (cdr (assq 'name (frame-parameters frame))))
          (make-frame-visible frame)    ; minibuffer frame
        (iconify-frame frame)))         ; other frames
    (buffer-menu)))

;;;###autoload
(defun mouse-show-hide-mark-unmark (click)
  "In minibuffer: `show-hide'. In dired: mark/unmark. Else: buffer menu."
  (interactive "e")
  (if (window-minibuffer-p (posn-window (event-start click)))
      (show-hide)
    (or (and (memq major-mode '(dired-mode vc-dired-mode))
             (fboundp 'dired-mouse-mark/unmark)
             (dired-mouse-mark/unmark click)) ; Returns nil if not on a file or dir.
        (mouse-buffer-menu click))))
    
;;;###autoload
(defun iconify/map-frame (&optional iconify-all)
  "Iconify selected frame if now mapped.  Map it if now iconified.
With non-nil prefix arg ICONIFY-ALL, iconify all visible frames."
  (interactive "P")
  (if iconify-all
      (iconify-everything)
    (when rename-frame-when-iconify-flag (rename-non-minibuffer-frame))
    (iconify-or-deiconify-frame)))

;;;###autoload
(defun mouse-iconify/map-frame (click)
  "Iconify frame clicked on, if now mapped.  Map it if now iconified."
  (interactive "e")
  (select-window (posn-window (event-start click)))
  (when rename-frame-when-iconify-flag (rename-non-minibuffer-frame))
  (iconify-or-deiconify-frame))



(or (fboundp 'old-delete-window)
    (fset 'old-delete-window (symbol-function 'delete-window)))

;; REPLACES ORIGINAL (built-in):
;; If WINDOW is the only one in its frame, `delete-frame'.
;;;###autoload
(defun delete-window (&optional window)
  "Remove WINDOW from the display.  Default is `selected-window'.
If WINDOW is the only one in its frame, then `delete-frame' too."
  (interactive)
  (setq window (or window (selected-window)))
  (select-window window)
  (if (one-window-p t) (delete-frame) (old-delete-window (selected-window))))


(or (fboundp 'old-delete-windows-on)
    (fset 'old-delete-windows-on (symbol-function 'delete-windows-on)))

;; REPLACES ORIGINAL (built-in):
;; 1) Uses `read-buffer' in interactive spec.
;; 2) Calls `delete-window', so if use my `delete-window' this also deletes
;;    frames where window showing the BUFFER is the only window.
;;;###autoload
(defun delete-windows-on (buffer &optional frame)
  "Delete windows showing BUFFER.

Optional second arg FRAME controls which frames are considered.
  If nil or omitted, delete all windows showing BUFFER in any frame.
  If `t', delete only windows showing BUFFER in the selected frame.
  If `visible', delete all windows showing BUFFER in any visible frame.
  If a frame, delete only windows showing BUFFER in that frame.

Interactively, FRAME depends on the prefix arg, as follows:
  Without a prefix arg (prefix = nil), FRAME is nil (all frames).
  With prefix arg >= 0, FRAME is `t' (this frame only).
  With prefix arg < 0,  FRAME is `visible' (all visible frames)."
  (interactive
   (list (read-buffer "Delete windows on buffer: " (current-buffer) 'existing)
         (and current-prefix-arg
              (or (natnump (prefix-numeric-value current-prefix-arg))
                  'visible))))
  ;; `get-buffer-window' interprets FRAME oppositely for t and nil, so switch.
  (setq frame (if (eq t frame) nil (if (eq nil frame) t frame)))
  (let (win)
    (while (setq win (get-buffer-window buffer frame)) (delete-window win))))

(defsubst frame-iconified-p (frame)
  (and (frame-live-p frame) (eq (frame-visible-p frame) 'icon)))

;; (defun remove-window (&optional window)
;;   "Remove WINDOW from the display.  Default is `selected-window'.
;; If WINDOW is the only one in its frame, then:
;;    If WINDOW is dedicated to its buffer, then make its frame invisible.
;;    Otherwise, delete its frame (as well as the window)."
;;   (interactive)
;;   (setq window (or window (selected-window)))
;;   (select-window window)
;;   (if (and (window-dedicated-p (selected-window))
;;            (one-window-p t))
;;       (let ((fr (selected-frame)))
;;         ;; HACK because of Emacs bug: `raise-frame' won't raise a frame
;;         ;; that was first iconified and then made invisible.
;;         ;; So, here we don't make an iconified frame invisible.
;;         (unless (frame-iconified-p fr)
;;           (make-frame-invisible fr)))
;;     (delete-window)))

;; REMOVED old definition, above, because of problems with invisible
;; *Completions* frame when use completion window with subsequent args
;; to a command.  Just use `delete-window' now, which deletes frame if
;; `one-window-p'. Use a `defalias' because its easier than replacing
;; all my calls to `remove-window' with `delete-window'.
;; 
;;;###autoload
(defalias 'remove-window 'delete-window)

;;;###autoload
(defun remove-windows-on (buffer)
  "Remove all windows showing BUFFER.  This calls `remove-window'
on each window showing BUFFER."
  (interactive
   (list (read-buffer "Remove all windows showing buffer: " (current-buffer) 'existing)))
  (setq buffer (get-buffer buffer))     ; Convert to buffer.
  (when buffer                          ; Do nothing if null BUFFER.
    (dolist (fr (frames-on buffer t))
      (remove-window (get-buffer-window buffer t)))))

;;;###autoload
(defun mouse-remove-window (click)
  "Remove the window you click on.  (This calls `remove-window'.)
This command must be bound to a mouse click."
  (interactive "e")
  (mouse-minibuffer-check click)
  (remove-window (posn-window (event-start click))))

;;;###autoload
(defun delete/iconify-window (&optional window frame-p)
  "Delete or iconify WINDOW (default: `selected-window').
If WINDOW is the only one in its frame (`one-window-p'), then optional
arg FRAME-P determines the behavior regarding the frame, as follows:
  If FRAME-P is nil, then the frame is deleted (with the window).
  If FRAME-P is `t', then the frame is iconified.
  If FRAME-P is a symbol naming a function, the function is applied
             to WINDOW as its only arg.
             If the result is nil, then the frame is deleted.
             If the result is non-nil, then the frame is iconified.
  If FRAME-P is anything else, then behavior is as if FRAME-P were the
             symbol `window-dedicated-p': the frame is iconified if
             WINDOW is dedicated, otherwise the frame is deleted.

Interactively, FRAME-P depends on the prefix arg, as follows:
  Without a prefix arg (prefix = nil), FRAME-P is `window-dedicated-p'.
  With prefix arg < 0, FRAME-P is `t'.  The frame is iconified.
  With prefix arg >= 0, FRAME-P is nil.  The frame is deleted."
  (interactive
   (list nil (if current-prefix-arg
                 (not (natnump (prefix-numeric-value current-prefix-arg)))
               'window-dedicated-p)))
  (setq window (or window (selected-window)))
  (let ((one-win-p t))
    (save-window-excursion
      (select-window window)
      (if (one-window-p)
          (if frame-p
              (if (eq t frame-p)
                  (iconify-frame)
                (unless (and (symbolp frame-p) (fboundp frame-p))
                  (setq frame-p 'window-dedicated-p))
                (if (funcall frame-p window) (iconify-frame) (delete-frame)))
            (delete-frame))             ; Default.
        (setq one-win-p nil)))
    ;; Do this outside `save-window-excursion'.
    (unless one-win-p (old-delete-window window))))

;;;###autoload
(defun delete/iconify-windows-on (buffer &optional frame frame-p)
  "For each window showing BUFFER: delete it or iconify its frame.
\(This calls `delete/iconify-window' on each window showing BUFFER.)

Optional second arg FRAME controls which frames are considered.
  If nil or omitted, treat all windows showing BUFFER in any frame.
  If `t', treat only windows showing BUFFER in the selected frame.
  If `visible', treat all windows showing BUFFER in any visible frame.
  If a frame, treat only windows showing BUFFER in that frame.

Optional third arg FRAME-P controls what to do with one-window frames.
  If FRAME-P is nil, then one-window frames showing BUFFER are deleted.
  If FRAME-P is `t', then one-window frames are iconified.
  If FRAME-P is a symbol naming a function, the function is applied
             to each window showing buffer in a frame by itself.
             If the result is nil, then the frame is deleted.
             If the result is non-nil, then the frame is iconified.
  If FRAME-P is anything else, then behavior is as if FRAME-P were the
             symbol `window-dedicated-p': One-window frames are
             iconified if window is dedicated, else they are deleted.

Interactively, FRAME is nil, and FRAME-P depends on the prefix arg:
  Without a prefix arg (prefix = nil), FRAME-P is `window-dedicated-p'.
  With prefix arg < 0, FRAME-P is `t'.  The frame is iconified.
  With prefix arg >= 0, FRAME-P is nil.  The frame is deleted."
  (interactive
   (list (read-buffer "Delete windows on buffer: " (current-buffer) 'existing)
         nil
         (if current-prefix-arg
             (not (natnump (prefix-numeric-value current-prefix-arg)))
           'window-dedicated-p)))
  (setq buffer (get-buffer buffer))     ; Convert to buffer.
  (when buffer                          ; Do nothing if null BUFFER.
    ;; `get-buffer-window' interprets FRAME oppositely for t and nil,
    ;; so switch.
    (setq frame (if (eq t frame) nil (if (eq nil frame) t frame)))
    (dolist (fr (frames-on buffer frame))
      (delete/iconify-window (get-buffer-window buffer frame) frame-p))))

;;;###autoload
(defun rename-frame (&optional old-name new-name all-named)
  "Rename a frame named OLD-NAME to NEW-NAME.
Prefix arg ALL-NAMED non-nil means rename all frames named FRAME to NEWNAME.

OLD-NAME may be a frame, its name, or nil.  Default is `selected-frame'.

NEW-NAME is a string or nil.  Default NEW-NAME is current `buffer-name'."
  (interactive
   (list (read-frame (concat "Rename " (and current-prefix-arg "all ")
                             "frame" (and current-prefix-arg "s named") ": ")
                     nil t)             ; Default = selected.  Must exist.
         (read-from-minibuffer "Rename to (new name): " (cons (buffer-name) 1))
         current-prefix-arg))
  (setq old-name (or old-name (get-frame-name))) ; Batch default: current.
  (setq new-name (or new-name (buffer-name))) ; Batch default: buffer name.
  ;; Convert to frame if string.
  (let ((fr (get-a-frame old-name)))
    (if all-named
        (while fr
          (modify-frame-parameters fr (list (cons 'name new-name)))
          (setq fr (get-a-frame old-name))) ; Get another.
      (when (string= (get-frame-name fr) (get-frame-name))
        (setq fr (selected-frame)))
      (modify-frame-parameters fr (list (cons 'name new-name))))))

;;;###autoload
(defun rename-non-minibuffer-frame (&optional old-name new-name all-named)
  "Unless OLD-NAME names the minibuffer frame, use `rename-frame'
to rename a frame named OLD-NAME to NEW-NAME.

Prefix arg ALL-NAMED non-nil => Rename all frames named FRAME to NEWNAME.
OLD-NAME may be a frame, its name, or nil.  Default is `selected-frame'.
NEW-NAME is a string or nil.  Default NEW-NAME is current `buffer-name'."
  (interactive
   (list (read-frame (concat "Rename " (and current-prefix-arg "all ")
                             "frame" (and current-prefix-arg "s named") ": ")
                     nil t)             ; Default = selected.  Must exist.
         (read-from-minibuffer "Rename to (new name): " (cons (buffer-name) 1))
         current-prefix-arg))
  (setq old-name (or old-name (get-frame-name))) ; Batch default: current.
  (setq new-name (or new-name (buffer-name))) ; Batch default: buffer name.
  (let ((fr (get-a-frame old-name)))    ; Convert to frame if string.
    (if (and (boundp '1on1-minibuffer-frame)
             (eq (cdr (assq 'name (frame-parameters 1on1-minibuffer-frame)))
                 (cdr (assq 'name (frame-parameters fr)))))
        (and (interactive-p)
             (error
              "Use `rename-frame' if you really want to rename minibuffer."))
      (rename-frame))))

;;;###autoload
(defun show-frame (frame)
  "Make FRAME visible and raise it, without selecting it.
FRAME may be a frame or its name."
  (interactive (list (read-frame "Frame to make visible: ")))
  (setq frame (get-a-frame frame))
  (make-frame-visible frame)
  (raise-frame frame))

;;;###autoload
(defun hide-frame (frame &optional prefix)
  "Make FRAME invisible.  Like `make-frame-invisible', but reads frame name.
Non-nil PREFIX makes it invisible even if all other frames are invisible."  
  (interactive (list (read-frame "Frame to make invisible: ")))
  (make-frame-invisible (get-a-frame frame) prefix))

;;;###autoload
(defun show-a-frame-on (buffer)
  "Make visible and raise a frame showing BUFFER, if there is one.
Neither the frame nor the BUFFER are selected.
BUFFER may be a buffer or its name (a string)."
  (interactive
   (list (read-buffer "Show a frame showing buffer: "
                      (if (fboundp 'another-buffer) ; Defined in `misc-fns.el'.
                          (another-buffer nil t)
                        (other-buffer (current-buffer)))
                      'existing)))
  (when buffer                          ; Do nothing if null BUFFER.
    (let ((fr (car (frames-on buffer)))) (when fr (show-frame fr)))))

;;;###autoload
(defun show-*Help*-buffer ()
  "Raise a frame showing buffer *Help*, without selecting it."
  (interactive) (show-a-frame-on "*Help*"))

;;;###autoload
(defun delete-1-window-frames-on (buffer)
  "Delete all visible 1-window frames showing BUFFER."
  (interactive
   (list (read-buffer "Delete all visible 1-window frames showing buffer: "
                      (current-buffer) 'existing)))
  (setq buffer (get-buffer buffer))
  (save-excursion
    (when (buffer-live-p buffer)        ; Do nothing if dead buffer.
      (dolist (fr (frames-on buffer))   ; Is it better to search through 
        (save-window-excursion          ; `frames-on' or `get-buffer-window-list'?
          (select-frame fr)
          (when (one-window-p t fr) (delete-frame)))))))

;;;###autoload
(defun tile-frames-horizontally (&optional frames)
  "Tile frames horizontally.
Interatively:
  With prefix arg, you are prompted for names of two frames to tile.
  With no prefix arg, all visible frames are tiled, except a
       standalone minibuffer frame, if any.
If called from a program, all frames in list FRAMES are tiled."
  (interactive (and current-prefix-arg (args-for-tile-frames)))
  (tile-frames 'horizontal frames))

;;;###autoload
(defun tile-frames-vertically (&optional frames)
  "Tile frames vertically.
Interatively:
  With prefix arg, you are prompted for names of two frames to tile.
  With no prefix arg, all visible frames are tiled, except a
       standalone minibuffer frame, if any.
If called from a program, all frames in list FRAMES are tiled."
  (interactive (and current-prefix-arg (args-for-tile-frames)))
  (tile-frames 'vertical frames))

;;;###autoload
(defun tile-frames (direction frames &optional no-extending-p)
  "Tile visible frames horizontally or vertically, depending on DIRECTION.
Arg DIRECTION is `horizontal' or `vertical'.
Arg FRAMES is the list of frames to tile. If nil, then tile all visible
frames (except a standalone minibuffer frame, if any)."
  (let ((visible-frames
         (or frames
             (filtered-frame-list       ; Get visible frames, except minibuffer.
              (function
               (lambda (fr)
                 (and (eq t (frame-visible-p fr))
                      (or (not (boundp '1on1-minibuffer-frame))
                          (not (eq (cdr (assq 'name (frame-parameters 1on1-minibuffer-frame)))
                                   (cdr (assq 'name (frame-parameters fr))))))))))))
        ;; Size of a full-display frame, in pixels - but leave room
        ;; for a minibuffer frame at bottom of display.
        (fr-pixel-width (x-display-pixel-width))
        (fr-pixel-height (if (boundp '1on1-minibuffer-frame)
                             (cdr (assq 'top (frame-parameters 1on1-minibuffer-frame)))
                           (x-display-pixel-height)))
        (fr-origin 0))
    (case direction                     ; Size of frame in pixels.
      (horizontal (setq fr-pixel-width  (/ fr-pixel-width  (length visible-frames))))
      (vertical   (setq fr-pixel-height (/ fr-pixel-height (length visible-frames))))
      (otherwise (error "tile-frames: DIRECTION must be `horizontal' or `vertical'.")))
    (dolist (fr visible-frames)
      (let ((borders (* 2 (+ (cdr (assq 'border-width (frame-parameters fr)))
                             (cdr (assq 'internal-border-width (frame-parameters fr))))))
            (scroll-bar-width (or (cdr (assq 'scroll-bar-width (frame-parameters fr)))
                                  (frame-char-height fr)))) ; Tweak - can't know scroll-bar width.
        (set-frame-size
         fr
         (/ (- fr-pixel-width           ; Subtract borders & scroll bars.
               borders
               (if (cdr (assq 'vertical-scroll-bars (frame-parameters fr)))
                   scroll-bar-width
                 0))
            (frame-char-width fr))      ; Divide by # pixels/char.
         (- (/ (- fr-pixel-height       ; Subtract borders, scroll bars, & title bar.
                  borders
                  (if (cdr (assq 'horizontal-scroll-bars (frame-parameters fr)))
                      scroll-bar-width
                    0)
                  window-mgr-title-bar-pixel-width)
               (frame-char-height fr))  ; Divide by # pixels/line.
            (cdr (assq 'menu-bar-lines (frame-parameters fr))))))
      (set-frame-position fr
                          (if (eq direction 'horizontal) fr-origin 0)
                          (if (eq direction 'horizontal) 0 fr-origin))
      (show-frame fr)
      (incf fr-origin (if (eq direction 'horizontal) fr-pixel-width fr-pixel-height)))))

 
(defun args-for-tile-frames ()
  (list
   (list
    ;; Note: `read-frame' puts selected-frame name at front of `frame-name-history'.
    (get-a-frame (read-frame "Tile two frames - First frame: " nil t))
    ;; Get next visible frame. For default (prompt) value:
    ;;   If there is another visible frame in `frame-name-history', use next such.
    ;;   Else if there is another visible frame in internal frame list, use next such.
    ;;   Else use selected frame. (`frame-name-history' is defined in `frame.el'.)
    (get-a-frame 
     (read-frame
      "Second frame: "
      (let ((fr-names (cdr frame-name-history))
            (visible-p nil)
            (fr nil))
        (while (and (not fr) fr-names)  ; While no visible frame found and still fr-names to check.
          (setq fr (car fr-names))      ; Name
          (setq fr (get-a-frame fr))    ; Frame
          (setq fr (and fr (eq t (frame-visible-p fr)) fr)) ; Visible frame
          (setq fr-names (cdr fr-names)))

        ;; If no visible frames in history, besides selected-frame,
        ;; then get next visible frame (not its name) from internal frame list.
        (unless fr
          (setq fr (selected-frame))
          (while (and (not visible-p)
                      (setq fr (next-frame fr))
                      (not (equal fr (selected-frame)))) ; equal => no other found.
            (setq visible-p (eq t (frame-visible-p fr)))))
        fr)
      t)))))

;;;###autoload
(defun enlarge-font (&optional increment frame)
  "Increase size of font in FRAME by INCREMENT.
Interactively, INCREMENT is given by the prefix argument.
Optional FRAME parameter defaults to current frame."
  (interactive "p")
  (setq frame (or frame (selected-frame)))
  (let ((fontname (cdr (assq 'font (frame-parameters frame)))))
    (when (query-fontset fontname)
      (setq fontname (nth 2 (assq 'ascii (aref (fontset-info fontname frame) 2)))))
    (let ((xlfd-fields (x-decompose-font-name fontname))
          new-font-name)
      (unless xlfd-fields (error "Cannot decompose font name"))
      (let ((new-size (+ (string-to-number (aref xlfd-fields xlfd-regexp-pixelsize-subnum))
                         increment)))
        (unless (> new-size 0) (error "New font size is too small: %s" new-size))
        (aset xlfd-fields
              xlfd-regexp-pixelsize-subnum
              (number-to-string new-size)))
      ;; Set point size & width to "*", so frame width will adjust to new font size
      (aset xlfd-fields xlfd-regexp-pointsize-subnum "*")
      (aset xlfd-fields xlfd-regexp-avgwidth-subnum "*")
      (setq new-font-name (x-compose-font-name xlfd-fields))
      (modify-frame-parameters frame (list (cons 'font new-font-name)))
      ;; Update faces that want a bold or italic version of the default font.
      (frame-update-faces frame))))

;;;###autoload
(defun set-frame-alist-parameter-from-frame (alist parameter &optional frame)
  "Set PARAMETER of frame alist ALIST to its current value in FRAME.
FRAME defaults to the selected frame. ALIST is a variable (symbol)
whose value is an alist of frame parameters."
  (interactive
   (let ((symb (or (and (fboundp 'symbol-nearest-point)(symbol-nearest-point))
                   (and (symbolp (variable-at-point)))))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Frame alist to change (variable): "
                                    (frame-alist-var-names) nil t nil nil 'default-frame-alist t))
           (intern (completing-read "Parameter to set:"
                                    (frame-parameter-names) nil t nil nil 'left t))
           (get-a-frame (read-frame "Frame to copy parameter value from: " nil t)))))
  (unless (boundp alist)
    (error "Not a defined Emacs variable: `%s'." alist))
  (set alist (assq-delete-all parameter (copy-alist (eval alist))))
  (set alist (cons (assq parameter (frame-parameters frame)) (eval alist)))
  (tell-customize-var-has-changed alist))

;;;###autoload
(defun set-all-frame-alist-parameters-from-frame (alist &optional frame really-all-p)
  "Set frame parameters of ALIST to their current values in FRAME.
Unless optional argument REALLY-ALL-P (prefix arg) is non-`nil', the
frame parameters in list `frame-parameters-to-exclude' are
excluded: they are not copied from FRAME to ALIST. ALIST is a variable
\(symbol) whose value is an alist of frame parameters. FRAME defaults
to the selected frame."
  (interactive
   (let ((symb (or (and (fboundp 'symbol-nearest-point)(symbol-nearest-point))
                   (and (symbolp (variable-at-point)))))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Frame alist to change (variable): "
                                    (frame-alist-var-names) nil t nil nil 'default-frame-alist t))
           (get-a-frame (read-frame "Frame to copy parameter values from: " nil t))
           current-prefix-arg)))
  (unless (boundp alist)
    (error "Not a defined Emacs variable: `%s'." alist))
  (set alist (set-difference (frame-parameters frame)
                             (and (not really-all-p) frame-parameters-to-exclude)
                             :key 'car))
  (tell-customize-var-has-changed alist))

(defun frame-alist-var-names ()
  "Returns an alist of all variable names that end in \"frame-alist\".
The CAR of each list item is a string variable name.
The CDR is `nil'."
  (let ((vars nil))
    (mapatoms (lambda (sym) (and (boundp sym)
                                 (setq sym (symbol-name sym))
                                 (string-match "frame-alist$" sym)
                                 (push (list sym) vars))))
    vars))

(defun frame-parameter-names ()
  "Returns an alist of all available frame-parameter names.
The CAR of each list item is a string parameter name.
The CDR is `nil'."
  (let ((params '(("display") ("title") ("name") ("left") ("top") ("icon-left") ("icon-top")
                  ("user-position") ("height") ("width") ("window-id") ("minibuffer")
                  ("buffer-predicate") ("buffer-list") ("font") ("auto-raise") ("auto-lower")
                  ("vertical-scroll-bars") ("horizontal-scroll-bars") ("scroll-bar-width")
                  ("icon-type") ("icon-name") ("foreground-color") ("background-color")
                  ("background-mode") ("mouse-color") ("cursor-color") ("border-color")
                  ("display-type") ("cursor-type") ("border-width") ("internal-border-width")
                  ("unsplittable") ("visibility") ("menu-bar-lines"))))
    (when (>= emacs-major-version 21)
      (setq params (nconc params '("fullscreen" "outer-window-id" "tty-color-mode" "left-fringe"
                                   "right-fringe" "tool-bar-lines" "screen-gamma" "line-spacing"
                                   "wait-for-wm" "scroll-bar-foreground" "scroll-bar-foreground"))))
    params))

(defun tell-customize-var-has-changed (variable)
  "Tell Customize to recognize that VARIABLE has been set (changed).
VARIABLE is a symbol that names a user option."
  (interactive "vVariable: ")
  (put variable 'customized-value (list (custom-quote (eval variable)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'frame-cmds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; frame-cmds.el ends here
