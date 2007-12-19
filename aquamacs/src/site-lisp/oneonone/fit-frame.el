;;; fit-frame.el --- Resize a frame.  In particular, fit a frame to its buffers.
;;
;; Filename: fit-frame.el
;; Description: Resize a frame.  In particular, fit a frame to its buffers.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2000-2007, Drew Adams, all rights reserved.
;; Created: Thu Dec  7 09:32:12 2000
;; Version: 21.0
;; Last-Updated: Tue Dec 18 08:03:49 2007 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 1032
;; URL: http://www.emacswiki.org/cgi-bin/wiki/fit-frame.el
;; Keywords: internal, extensions, convenience, local
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Resize a frame.  In particular, fit a frame to its buffers.
;;
;;  Commands and user options (variables) are provided here to resize
;;  (shrink-wrap) a frame to fit its displayed buffers, its selected
;;  buffer, or the `fill-column' width.
;;
;;  The command to fit a frame is `fit-frame'.  The main user options
;;  for this command are `fit-frame-inhibit-fitting-flag' and
;;  `fit-frame-max-*[-percent]'.  You can use a prefix argument to
;;  control the behavior of command `fit-frame'.
;;
;;  To take full advantage of the functionality provided here, load
;;  the companion library `auto-fit-frames.el', to modify primitives
;;  `display-buffer' and `switch-to-buffer' so they automatically fit
;;  all frames that have a single window.  Library
;;  `auto-fit-frames.el' loads library `fit-frame.el'.
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;    (require 'fit-frame)
;;    (add-hook 'after-make-frame-functions 'fit-frame)
;;
;;  The second line here causes newly created frames to be fitted to
;;  their buffer.  Even if you load library `auto-fit-frames.el', you
;;  will still need to do this, because `display-buffer' and
;;  `switch-to-buffer' are not called when a new frame is created.
;;
;;  Command `fit-frame' does *not* take the following into account,
;;  when determining the proper frame size:
;;
;;   - font sizes, other than the default frame font
;;   - characters, such as TAB, that have special widths
;;
;;  Suggested key bindings:
;;
;;   (global-set-key [(control ?x) (control ?_)] 'fit-frame)
;;   (global-set-key [vertical-line down-mouse-1]
;;                   'fit-frame-or-mouse-drag-vertical-line)
;;
;;  Customize the menu-bar.  Uncomment this to try it out.
;;
;;   (defvar menu-bar-frames-menu (make-sparse-keymap "Frames"))
;;   (define-key global-map [menu-bar frames]
;;     (cons "Frames" menu-bar-frames-menu)))
;;   (define-key menu-bar-frames-menu [fit-frame]
;;     '("Fit This Frame" . fit-frame))
;;
;;
;;  Commands defined here:
;;
;;    `fit-frame', `fit-frame-or-mouse-drag-vertical-line',
;;    `fit-frame-maximize-frame', `fit-frame-minimize-frame',
;;    `fit-frame-restore-frame', `maximize-frame', `minimize-frame',
;;    `restore-frame',
;;
;;  User options (variables) defined here:
;;
;;    `fit-frame-empty-height',
;;    `fit-frame-empty-special-display-height',
;;    `fit-frame-empty-special-display-width',
;;    `fit-frame-empty-width', `fit-frame-fill-column-margin',
;;    `fit-frame-inhibit-fitting-flag', `fit-frame-max-height',
;;    `fit-frame-max-height-percent', `fit-frame-max-width',
;;    `fit-frame-max-width-percent', `fit-frame-min-height',
;;    `fit-frame-min-width', `fit-frame-skip-header-lines-alist'.
;;
;;  Non-interactive functions defined here:
;;
;;    `fit-frame-max-frame-size', `fit-frame-max-height',
;;    `fit-frame-max-width', `fit-frame-max-window-size',
;;    `fit-frame-same-column-windows', `fit-frame-same-row-windows',
;;    `fit-frame-thumbnail-factor'.
;;
;;
;;  See also these files for other frame commands:
;;
;;     `auto-fit-frames.el' - See above.
;;
;;     `frame-cmds.el' - Various frame and window commands, including
;;                       incrementally resizing frames.
;;
;;     `doremi-frm.el' - Incrementally adjust frame properties
;;                       using arrow keys and/or mouse wheel.
;;
;;  This file was formerly called `shrink-fit.el', then
;;  `resize-frame.el', and command `fit-frame' was formerly called
;;  `shrink-frame-to-fit', then `resize-frame'.
;;
;;  TO DO:
;;
;;  Emacs needs a command similar to `fit-frame' for windows, that is,
;;  a command that will fit the existing windows of a frame to their
;;  buffers, as well as possible.  That could be then be used in
;;  combination with `fit-frame'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2007/12/18 dadams
;;     fit-frame-max-(width|height): Applied patch from David Reitter for Apple (Mac).
;; 2007/11/27 dadams
;;     fit-frame: Use read-number, if available.
;; 2007/11/21 dadams
;;     fit-frame: extra-lines:
;;       Don't add 1 for standalone minibuffer.
;;       Removed extra 2-line tweak factor for Emacs 21+.
;; 2007/11/01 dadams
;;     RMS request: Simplified fit-frame doc string.
;; 2007/07/22 dadams
;;     RMS requests:
;;       Lowercased group name.
;;       Renamed to use prefix fit-frame-: (maximize|minimize|restore)-frame,
;;         create-empty(-special-display)-frame-(height|width),
;;         fill-column-frame-margin, frame-max-(height|width)(-percent),
;;         frame-min-(height|width), inhibit-fit-frame-flag,  max-(frame|window)-size,
;;         same-(column|row)-windows, thumbnail-factor.
;;     Added aliases: maximize-frame, minimize-frame, restore-frame.
;; 2007/07/21 dadams
;;     fit-frame: Added optional all-windows-p arg.
;;     Added: max-frame-size, max-window-size, same-column-windows, same-row-windows.
;;     Removed: show-frame, get-a-frame, get-frame-name, (set-)minibuffer-empty-p.
;;     Removed require of frame-cmds.el.
;;     Renamed: create-frame-(min|max)-(width|height)(-percent) to
;;              frame-(min|max)-(width|height)(-percent).
;; 2007/03/11 dadams
;;     fit-frame: Deal with header lines that wrap more than once.
;;     fit-frame-skip-header-lines-alist: 2 lines for Dired.
;; 2006/03/13 dadams
;;     fit-frame: Treat nil tool-bar-lines as zero.  (Thx to Sebastian Luque.)
;; 2006/01/07 dadams
;;     Added :link for sending bug report.
;; 2006/01/06 dadams
;;     Added :link.  Capitalized group name.
;;     Added ;;;###autoload.
;; 2005/12/30 dadams
;;     Added: thumbnail-factor.
;;     create-frame-max-height: Use thumbnail-factor.
;; 2005/11/15 dadams
;;     create-frame-max-* functions: Added optional frame arg.
;;     Minor bug fix: Call create-frame-max-* with frame arg, so use correct char size.
;; 2005/07/31 dadams
;;     Removed require of strings.el.
;; 2005/07/04 dadams
;;     fit-frame: Bug fix: Added (set-buffer (window-buffer))
;; 2005/05/30 dadams
;;     Added: fit-frame-skip-header-lines-alist.
;;     fit-frame: Use fit-frame-skip-header-lines-alist to ignore width of header lines.
;; 2005/05/29 dadams
;;     Moved enlarge-frame* and shrink-frame* to frame-cmds.el.
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
;; the Free Software Foundation; either version 3, or (at your option)
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

(when (< emacs-major-version 21) (eval-when-compile (require 'cl))) ;; dolist

;;;;;;;;;;;;;;;;;;;;;;;
 
;;; User options ---------------------------------------------------

;;;###autoload
(defgroup fit-frame nil
  "Resize a frame to fit its selected window, or resize it incrementally."
  :group 'frames :group 'convenience
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
fit-frame.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/fit-frame.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Shrink-Wrapping_Frames")
  :link '(emacs-commentary-link :tag "Commentary" "fit-frame"))

;;;###autoload
(defcustom fit-frame-inhibit-fitting-flag nil
  "*Non-nil means command `fit-frame' does nothing.
You can bind this to non-`nil' to temporarily inhibit frame fitting:
    (let ((fit-frame-inhibit-fitting-flag t))...)"
  :type 'boolean :group 'fit-frame)

;;;###autoload
(defcustom fit-frame-min-width 20
  "*Minimum width, in characters, for frames resized by `fit-frame'.
The actual minimum is at least the greater of this and `window-min-width'."
  :type 'integer :group 'fit-frame)

;;;###autoload
(defcustom fit-frame-max-width nil
  "*Maximum width, in characters, for frames resized by `fit-frame'.
If nil, then the function `fit-frame-max-width' is used instead."
  :type '(choice (const :tag "Use `fit-frame-max-width-percent' instead" nil)
                 integer)
  :group 'fit-frame)

;;;###autoload
(defcustom fit-frame-max-width-percent 94
  "*Maximum percent of display width for a frame resized by `fit-frame'.
See function `fit-frame-max-width'.
Not used unless `fit-frame-max-width' is nil."
  :type 'integer :group 'fit-frame)

;;;###autoload
(defcustom fit-frame-min-height window-min-height
  "*Minimum height, in lines, for frames resized by `fit-frame'.
The actual minimum is at least the greater of this and `window-min-height'."
  :type 'integer :group 'fit-frame)

;;;###autoload
(defcustom fit-frame-max-height nil
  "*Maximum height, in lines, for frames resized by `fit-frame'.
If nil, then the function `fit-frame-max-height' is used instead."
  :type '(choice (const :tag "Use `fit-frame-max-height-percent' instead" nil)
                 integer)
  :group 'fit-frame)

;;;###autoload
(defcustom fit-frame-max-height-percent 82
  "*Maximum percent of display height for a frame resized by `fit-frame'.
See function `fit-frame-max-height'.
Not used unless `fit-frame-max-height' is nil."
  :type 'integer :group 'fit-frame)

;;;###autoload
(defcustom fit-frame-empty-width (or (cdr (assq 'width default-frame-alist)) 80)
  "*Width, in characters, for new empty frames."
  :type 'integer :group 'fit-frame)

;;;###autoload
(defcustom fit-frame-empty-height (or (cdr (assq 'height default-frame-alist)) 35)
  "*Height, in lines, for new empty frames."
  :type 'integer :group 'fit-frame)

;;;###autoload
(defcustom fit-frame-empty-special-display-width 80
  "*Width, in chars, for new empty special-display frames.
If this is nil, it is ignored."
  :type 'integer :group 'fit-frame)

;;;###autoload
(defcustom fit-frame-empty-special-display-height 9
  "*Height, in lines, for new empty special-display frames.
If this is nil, it is ignored."
  :type 'integer :group 'fit-frame)

;;;###autoload
(defcustom fit-frame-fill-column-margin 7
  "*With negative prefix arg, `fit-frame' frame width minus `fill-column'.
That is, `fill-column' + `fit-frame-fill-column-margin' = frame width.
Depending on the average word length of the language used in the
selected window, you may need different values for this.
This variable is buffer-local."
  :type 'integer :group 'fit-frame)

(make-variable-buffer-local 'fit-frame-fill-column-margin)

;;;###autoload
(defcustom fit-frame-skip-header-lines-alist
  '((Info-mode . 1) (dired-mode . 2) (compilation-mode . 2))
  "*Alist of major-modes and header lines to ignore.

When `fit-frame' calculates the width of the current buffer, it can
first skip some lines at the buffer beginning, ignoring their
widths.  For example, Info, Dired, and compilation buffers sometimes
have a long header line at the top.  You can use this alist to tell
`fit-frame' to ignore the width of these header lines.

Each item in the alist is of form (MODE . LINES).
 MODE is a major-mode name.
 LINES is the number of lines to skip at the beginning of the buffer."
  :type '(repeat (cons :format "%v" (symbol :tag "Major Mode")
                       (integer :tag "Header Lines to Ignore")))
  :group 'fit-frame)
 
;;; Commands ---------------------------------------------------

;;;###autoload
(defun fit-frame (&optional frame width height all-windows-p)
  "Resize FRAME.
Does nothing if `fit-frame-inhibit-fitting-flag' is non-nil.

FRAME defaults to the current (i.e. selected) frame.

If non-nil, WIDTH and HEIGHT specify the frame width and height.  To
define them interactively, use a non-negative prefix arg (e.g. `C-9').

To set the width to `fill-column' + `fit-frame-fill-column-margin',
use a negative prefix arg (e.g. `M--').

To fit the frame to all of its displayed buffers, use no prefix arg.
To fit it to just the current buffer, use a plain prefix arg (`C-u').

Fitting a non-empty buffer means using the smallest frame such that:

 * The width is at least `fit-frame-min-width' and `window-min-width'.
   The width is at most `fit-frame-max-width(-percent)' and the
   longest line length.

 * The height is at least `fit-frame-min-height' and
   `window-min-height'.  The height is at most
   `fit-frame-max-height(-percent)' and the number of lines.

You can thus use those user variables to control the maximum and
minimum frame sizes.  The `*-percent' options let you specify the
maximum as a percentage of your display size.

These user options control fitting of an empty frame:

 * `fit-frame-empty-width', `fit-frame-empty-height' (normal buffer)
 * `fit-frame-empty-special-display-width',
   `fit-frame-empty-special-display-height' (special-display buffer)

See also option `fit-frame-skip-header-lines-alist'.

When used in `after-make-frame-functions', the current `frame-width'
and `frame-height' are those of the newly created frame."
  (interactive
   (let ((option (prefix-numeric-value current-prefix-arg)))
     (list nil
           (and current-prefix-arg (atom current-prefix-arg)
                (if (natnump option)
                    (floor (if (fboundp 'read-number)
                               (read-number "New width: ")
                             (string-to-number (read-string "New width: "))))
                  (+ fill-column fit-frame-fill-column-margin)))
           (and current-prefix-arg (atom current-prefix-arg)
                (if (natnump option)
                    (floor (if (fboundp 'read-number)
                               (read-number "New height: ")
                             (string-to-number (read-string "New height: "))))
                  (frame-height)))
           (atom current-prefix-arg))))
  (setq frame (or frame (selected-frame)))
  (unless fit-frame-inhibit-fitting-flag
    (let ((extra-lines 2)               ; Minimum is 1 for empty + 1 extra.
          (computed-max-frame-size nil)
          empty-buf-p specbuf-p)
      ;; `extra-lines' for minimum frame height.  Starting with Emacs 21+,
      ;; `set-frame-size' includes the tool-bar and the minibuffer, but not the
      ;; menu-bar.  Add 1 line for the minibuffer, unless it is standalone.  Perhaps
      ;; we should also take into account a possible horizontal scroll bar, but we
      ;; don't do that.
      (when (>= emacs-major-version 21)
        (let* ((fparams (frame-parameters frame)))
          (setq extra-lines (+ extra-lines
                               (or (cdr (assq 'tool-bar-lines fparams)) 0))) ; Tool bar.
          (when (and (cdr (assq 'minibuffer fparams)) ; Frame has a minibuffer, but
                     (save-window-excursion (select-frame frame) ; it's not standalone.
                                            (not (one-window-p nil 'selected-frame))))
            (setq extra-lines (1+ extra-lines)))))
      (set-frame-size
       ;; Frame
       frame
       ;; Columns
       (or width
           (save-window-excursion
             (select-frame frame)
             (and (setq empty-buf-p (and (= (point-min) (point-max))
                                         (one-window-p (selected-window))))
                  (if (setq specbuf-p (special-display-p (buffer-name (window-buffer))))
                      fit-frame-empty-special-display-width
                    fit-frame-empty-width)))
           (max fit-frame-min-width window-min-width
                (min (or fit-frame-max-width (fit-frame-max-width frame))
                     (1+ (car (setq computed-max-frame-size
                                    (fit-frame-max-frame-size frame all-windows-p)))))))
       ;; Rows
       (or height
           (and empty-buf-p (if specbuf-p
                                fit-frame-empty-special-display-height
                              fit-frame-empty-height))
           (max fit-frame-min-height window-min-height
                (min (or fit-frame-max-height (fit-frame-max-height frame))
                     (+ (cdr (or computed-max-frame-size
                                 (fit-frame-max-frame-size frame all-windows-p)))
                        extra-lines))))))))

;;;###autoload
(defun fit-frame-or-mouse-drag-vertical-line (start-event)
  "If only window in frame, `fit-frame'; else `mouse-drag-vertical-line'."
  (interactive "e")
  (if (one-window-p t) (fit-frame) (mouse-drag-vertical-line start-event)))

;; Note that in Windows you can also just double-click the title bar
;; of a frame to alternately maximize and restore it.
;;;###autoload
(when (eq window-system 'w32)
  (defalias 'restore-frame 'fit-frame-restore-frame)
  (defun fit-frame-restore-frame (&optional frame)
    "Restore FRAME to previous size (default: current frame)."
    (interactive)
    (w32-send-sys-command 61728 frame)))

;;;###autoload
(when (eq window-system 'w32)
  (defalias 'maximize-frame 'fit-frame-maximize-frame)
  (defun fit-frame-maximize-frame (&optional frame)
    "Maximize FRAME (default: current frame)."
    (interactive)
    (w32-send-sys-command 61488 frame)))

(when (eq window-system 'w32)
  (defalias 'minimize-frame 'fit-frame-minimize-frame)
  (defalias 'fit-frame-minimize-frame
      (if (fboundp 'really-iconify-frame)
          'really-iconify-frame
        'iconify-frame)))
 
;;; Non-Interactive Functions -------------------------------------------

(defun fit-frame-max-width (&optional frame)
  "Maximum width, in characters, for new frames
when `fit-frame' is used in `after-make-frame-functions',
and `fit-frame-max-width' is nil.

The value is relative to your display size and FRAME's character
size, and depends on the value of `fit-frame-max-width-percent':

  (/ (* fit-frame-max-width-percent (x-display-pixel-width))
     (* 100 (frame-char-width FRAME)))"
  (setq frame (or frame (selected-frame)))
  (/ (* fit-frame-max-width-percent
        (if (fboundp 'winmgr-display-available-pixel-bounds) ; For MacIntosh.
 	    (nth 2 (winmgr-display-available-pixel-bounds))
          (x-display-pixel-width)))
     (* 100 (frame-char-width frame))))

(defun fit-frame-max-height (&optional frame)
  "Maximum height, in characters, for new frames
when `fit-frame' is used in `after-make-frame-functions',
and `fit-frame-max-height' is nil.

The value is relative to your display size and FRAME's character
size, and depends on the value of `fit-frame-max-height-percent':

  (/ (* fit-frame-max-height-percent (x-display-pixel-height))
     (* 100 (frame-char-height FRAME)))"
  (setq frame (or frame (selected-frame)))
  (/ (* fit-frame-max-height-percent
        (if (fboundp 'winmgr-display-available-pixel-bounds) ; For MacIntosh.
 	    (nth 3 (winmgr-display-available-pixel-bounds))
          (x-display-pixel-height)))
     (* 100 (frame-char-height frame)
        ;; When fitting a thumbnail frame, we don't want the height to use the
        ;; whole display height.  So, we apply a fudge factor:
        ;; `fit-frame-thumbnail-factor'.  We could also use it in
        ;; `fit-frame-max-width', in addition to `fit-frame-max-height',
        ;; but we don't need to.
        (fit-frame-thumbnail-factor frame))))

(defun fit-frame-max-frame-size (frame all-windows-p)
  "Return maximum size of frame FRAME as a cons: (MAX-WIDTH . MAX-HEIGHT).
If ALL-WINDOWS-P is non-nil, then consider all buffers shown in FRAME.
Otherwise, consider only the selected buffer."
  (save-window-excursion
    (select-frame frame)
    (if (not all-windows-p)
        (fit-frame-max-window-size (selected-window))
      (let* ((wins ())
             (marked-wins ())
             (max-width 0)
             (max-height 0))
        (walk-windows (lambda (w) (push w wins)) 'no-mini 'this-frame)
        (setq wins (sort wins (lambda (w1 w2) ; Top to bottom, left to right.
                                (let ((edges1 (window-edges w1))
                                      (edges2 (window-edges w2)))
                                  (or (< (cadr edges1) (cadr edges2)) ; top
                                      (and (= (cadr edges1) (cadr edges2))
                                           (<= (car edges1) (car edges2)))))))) ; left
        (dolist (win wins)
          (unless (memq win marked-wins)
            (let* ((win-edges (window-edges win))
                   (win-top (cadr win-edges))
                   (win-left (car win-edges)))
              ;; Add widths of buffers in the same row.  Max the heights of the buffers.
              (dolist (row-win (fit-frame-same-row-windows frame win marked-wins))
                (setq marked-wins (cons row-win marked-wins))
                (let* ((win-size (fit-frame-max-window-size row-win))
                       (max-win-width (car win-size))
                       (max-win-height (cdr win-size)))
                  (unless (> (cadr (window-edges row-win)) win-top) ; Use only first.
                    (setq max-width (+ max-width max-win-width)))
                  (setq max-height (max max-height max-win-height))))
              ;; Add heights of buffers in the same column.  Max the buffer widths.
              (dolist (col-win (fit-frame-same-column-windows frame win marked-wins))
                (setq marked-wins (cons col-win marked-wins))
                (let* ((win-size (fit-frame-max-window-size col-win))
                       (max-win-width (car win-size))
                       (max-win-height (cdr win-size)))
                  (unless (> (car (window-edges col-win)) win-left) ; Use only first.
                    (setq max-height (+ max-height max-win-height)))
                  (setq max-width (max max-width max-win-width)))))))
        (cons max-width max-height)))))

(defun fit-frame-same-row-windows (frame window exclude)
  "List of windows that are in the same row as window WINDOW.
These are the windows whose top edge is above the bottom edge of
REF-WIN.
Windows that are in list EXCLUDE are excluded from the result."
  (let ((ref-bottom (cadddr (window-edges window)))
        (wins ()))
    (walk-windows (lambda (w) (when (and (not (memq w exclude))
                                         (< (cadr (window-edges w)) ref-bottom))
                                (push w wins)))
                  'nomini 'this-frame)
    wins))
  
(defun fit-frame-same-column-windows (frame window exclude)
  "List of windows that are in the same column as window REF-WIN.
These are the windows whose left edge is to the left of the right edge
of REF-WIN.
Windows that are in list EXCLUDE are excluded from the result."
  (let ((ref-right (caddr (window-edges window)))
        (wins ()))
    (walk-windows (lambda (w) (when (and (not (memq w exclude))
                                         (< (car (window-edges w)) ref-right))
                                (push w wins)))
                  'nomini 'this-frame)
    wins))

(defun fit-frame-max-window-size (window)
  "Maximum size that would be needed to display the buffer in WINDOW.
Returned as a cons: (MAX-WIDTH . MAX-HEIGHT), where:
 MAX-WIDTH is the maximum width, in characters.
 MAX-HEIGHT is the maximum height, in lines."
  (select-window window)
  (let ((hdr-lines (cdr (assq major-mode fit-frame-skip-header-lines-alist)))
        (hdr-widths ())
        (max-win-width 0)
        (max-win-height 0))
    (save-excursion
      (set-buffer (window-buffer))
      (goto-char (point-min))
      ;; Don't count header lines for width calculation.
      (while (and hdr-lines (> hdr-lines 0))
        (end-of-line)
        (setq hdr-widths (cons (current-column) hdr-widths)
              hdr-lines  (1- hdr-lines))
        (forward-line)
        (setq max-win-height (1+ max-win-height)))
      ;; Calculate maximum line width and number of lines.
      (while (not (eobp))
        (end-of-line)
        (setq max-win-width (max (current-column) max-win-width))
        (forward-line 1)
        (setq max-win-height (1+ max-win-height))))          
    ;; Add height for any wrap-around header lines.
    (while hdr-widths
      (when (> (car hdr-widths) max-win-width)
        (if (zerop max-win-width)
            (setq max-win-height (1+ max-win-height))
          (let ((nb-wraps (/ (car hdr-widths) max-win-width))
                (remainder (% (car hdr-widths) max-win-width)))
            (unless (zerop remainder) (setq nb-wraps (1+ nb-wraps)))
            (setq max-win-height (+ max-win-height nb-wraps)))))
      (setq hdr-widths (cdr hdr-widths)))
    (cons max-win-width max-win-height)))
        
(defun fit-frame-thumbnail-factor (frame)
  "Shrink factor for thumbnail frames.  See `thumb-frm.el'.
FRAME is the frame to apply factor to."
  (let ((char-height (frame-char-height frame)))
    (if (and (fboundp 'thumbnail-frame-p) ; Defined in `thumb-frm.el'
             (thumbnail-frame-p frame))
        ;; Need integer result for `set-frame-size'.
        ;; Add one because of integer round-off.
        (1+ (/ (+ char-height frame-thumbnail-font-difference) char-height))
      1)))

;;;;;;;;;;

(provide 'fit-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fit-frame.el ends here
