;;; oneonone.el --- Frame configuration that uses one frame per window.
;;
;; Filename: oneonone.el
;; Description: Frame configuration that uses one frame per window.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1999-2005, Drew Adams, all rights reserved.
;; Created: Fri Apr  2 12:34:20 1999
;; Version: 21.1
;; Last-Updated: Mon Jul 04 10:08:28 2005
;;           By: dradams
;;     Update #: 1480
;; Keywords: local, frames
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Frame configuration that uses one frame per window.
;;
;;  This library is part of One-on-One Emacs, a collection of
;;  libraries that try to make Emacs more frame-oriented and less
;;  window-oriented.
;;
;;  This library sets up Emacs to use multiple frames: individual
;;  frames are used, by default, instead of Emacs windows.  That is,
;;  the default is to use a frame for each Emacs window: one window on
;;  one frame.
;;
;;  You can configure the properties of all frames defined here.
;;
;;  Default properties are defined here for normal frames and
;;  "special" frames, which show "special-display buffers" (see Emacs
;;  manual for info on such frames).
;;
;;  In addition, these user options control the creation of three
;;  separate, specialized frames:
;;
;;    - `1on1-minibuffer-frame-flag' - minibuffer frame
;;    - `1on1-*Help*-frame-flag' - *Help* buffer frame
;;    - `1on1-*Completions*-frame-flag' - *Completions* buffer frame
;;
;;  More precisely, buffers *Help* and *Completions* are always
;;  created in frames that have the special properties defined here.
;;
;;  `1on1-minibuffer-frame' is a standalone minibuffer frame.  It is
;;  the only frame to have a minibuffer.  By default, it is
;;  automatically sized to the full width of your display and placed
;;  at the bottom of the display.
;;
;;  This library is especially useful if used in combination with
;;  One-on-One Emacs libraries `autofit-frame.el', which automatically
;;  fits frames to their sole window, and `fit-frame.el', which lets
;;  you fit a frame to its selected window manually.  Library
;;  `autofit-frame.el' uses library `fit-frame.el'.
;;
;;  Because Emacs is not really designed to be frame-oriented, there
;;  are many built-in and standard functions that produce
;;  less-than-optimal results when frames, instead of windows, are the
;;  default.  In other One-on-One Emacs libraries, I have fixed most
;;  of these built-in functions to play well with frames.
;;
;;  For more information on One-on-One Emacs see
;;  http://www.emacswiki.org/cgi-bin/wiki/OneOnOneEmacs.
;;
;;
;;  To use this library, put the following at the *END* of your init
;;  file, `.emacs'.  In particular, if your init file contains a
;;  `custom-set-variables' expression, then the following must appear
;;  *AFTER* `custom-set-variables', in order for this to take into
;;  account your customizations of any `1on1-' user options.
;;
;;    (require 'oneonone)
;;    (1on1-emacs)
;;
;;  I also recommend that you put the following code in your init
;;  file, so that these buffers will display in their own frames.
;;  Which code to use depends on your version of GNU Emacs.
;;
;;    ;; Change first test to (string-match "22." emacs-version)
;;    ;;   after 22.x release.
;;
;;    (cond ((or (string-match "22." emacs-version)
;;           (string-match "21.3.50" emacs-version))
;;           (remove-hook 'same-window-regexps "\\*info\\*\\(\\|<[0-9]+>\\)")
;;           (remove-hook 'same-window-regexps "\\`\\*Customiz.*\\*\\'"))
;;          ((< emacs-major-version 21)
;;           (remove-hook 'same-window-buffer-names "*info*"))
;;          (t  ;; E.g. Emacs 21.3.1
;;           (remove-hook 'same-window-buffer-names "*info*")
;;           (remove-hook 'same-window-regexps "\\`\\*Customiz.*\\*\\'")))
;;
;;  If you use MS Windows, you might also want to add this, to avoid
;;  giving focus to a frame just because it is raised:
;;
;;    (setq w32-grab-focus-on-raise      nil
;;          win32-grab-focus-on-raise    nil) ; older name
;;
;;  To get an idea what this library does, without modifying your init
;;  file, you can just load it and then use `M-x 1on1-emacs'.
;;
;;  Note on user options defined here:
;;  ---------------------------------
;;
;;    Some user options are used here only as conveniences to define
;;    frame-parameter alists.  They are defined using `defvar', not
;;    `defcustom', because you cannot use Customize to define them
;;    independently of the alist user options they help define.  The
;;    alists themselves are the variables to customize.  If you want
;;    to change the `defvar' variables individually and then use them
;;    to set the alist variables, you must do use `setq' or
;;    `set-variable', not using Customize, to change them, and you
;;    must restart Emacs for their changes to take effect.
;;
;;    Changes to any user options defined here with `defcustom' will
;;    take effect as soon as `1on1-emacs' is executed, so you can do
;;    `M-x 1on1-emacs' to see their changes (no need to restart
;;    Emacs).
;;
;;
;;  New functions and macros defined here (each has prefix `1on1-'):
;;
;;    `color-(in)active-minibuffer-frame',
;;    `color-isearch-minibuffer-frame', `display-*Completions*-frame',
;;    `display-*Help*-frame', `emacs', `flash-ding-minibuffer-frame',
;;    `set-minibuffer-frame-top/bottom', `set-minibuffer-frame-width',
;;    `setup-minibuffer-frame-coloring'.
;;
;;  Customizable user options defined here (each has prefix `1on1-'):
;;
;;    `*Completions*-frame-flag', `*Help*-frame-flag',
;;    `active-minibuffer-frame-background',
;;    `completions-frame-background',
;;    `completions-frame-mouse+cursor-color', `default-frame-alist',
;;    `help-frame-background', `help-frame-mouse+cursor-color',
;;    `inactive-minibuffer-frame-background',
;;    `isearch-minibuffer-frame-background', `minibuffer-frame-alist',
;;    `minibuffer-frame-top/bottom', `minibuffer-frame-width',
;;    `minibuffer-frame-width-percent',
;;    `special-display-frame-alist'.
;;
;;  Non-customizable user options defined here (prefix `1on1-'):
;;
;;    `default-frame-background', `default-frame-cursor-color',
;;    `default-frame-font', `default-frame-foreground',
;;    `default-frame-menu-bar-lines', `default-frame-mouse-color',
;;    `default-frame-size', `default-frame-upper-left-corner',
;;    `default-special-frame-background',
;;    `default-special-frame-cursor-color',
;;    `default-special-frame-font',
;;    `default-special-frame-foreground',
;;    `default-special-frame-menu-bar-lines',
;;    `default-special-frame-mouse-color',
;;    `default-special-frame-size',
;;    `default-special-frame-upper-left-corner',
;;    `minibuffer-frame-background', `minibuffer-frame-cursor-color',
;;    `minibuffer-frame-flag', `minibuffer-frame-font',
;;    `minibuffer-frame-foreground', `minibuffer-frame-height',
;;    `minibuffer-frame-mouse-color'.
;;
;;  Other new variables defined here (each has prefix `1on1-'):
;;
;;    `minibuffer-frame'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `abort-recursive-edit', `top-level' -
;;               Reset color of minibuffer frame to "inactive" color.
;;
;;  `y-or-n-p' - Temporarily color minibuffer frame to "active" color.
;;
;;
;;  Library `oneonone' requires these libraries:
;;
;;    `avoid', `cl', `files+', `frame-cmds', `frame-fns', `icomplete',
;;    `icomplete+', `misc-fns', `oneonone', `strings', `thingatpt',
;;    `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2005/06/01 dadams
;;     Corrected typo that gave minibuffer frame a vertical scroll bar.
;; 2005/05/29 dadams
;;     *-alist: Use values from standard alist variables, if available (that is,
;;       don't override user settings.)
;; 2005/05/28 dadams
;;     Renamed: 1on1-separate-minibuffer-frame-flag -> 1on1-minibuffer-frame-flag,
;;       1on1-separate-*Help*-frame-flag -> 1on1-*Help*-frame-flag,
;;       1on1-separate-*Completions*-frame-flag -> 1on1-*Completions*-frame-flag.
;;     Added: setup-minibuffer-frame-coloring.
;;     Added info in doc strings about use of each variable (restart/1on1-emacs).
;;     Corrected 1on1-minibuffer-frame-alist and 1on1-special-display-frame-alist
;;       for menu-bar-lines (nil).
;;     1on1-set-minibuffer-frame-top/bottom: Rewrote with modify-frame-parameters.
;;     1on1-emacs:
;;       Make sensitive to any changes to 1on1-*[Help|Completions]*-frame-flag.
;;       Move defcustom's, defvar's, and defun's outside 1on1-emacs.
;;       If 1on1-minibuffer-frame already exists, just modify it.
;;       Don't step on other parameters in standard alists; just append new values.
;; 2005/05/23 dadams
;;     Changed some individual frame-parameter variables from defcustom to defvar.
;;       Left them as user options, however, so you can change them with
;;       set-variable before loading oneonone.el.
;;     Renamed:
;;       1on1-upper-left-frame-corner-default ->
;;          1on1-default-frame-upper-left-corner
;;       1on1-default-special-display-frame-size ->
;;          1on1-default-special-frame-size
;;       1on1-upper-left-special-display-frame-corner-default ->
;;          1on1-default-special-frame-upper-left-corner
;;     Split 1on1-menu-bar-lines into: 1on1-default-special-frame-menu-bar-lines,
;;          1on1-default-frame-menu-bar-lines
;; 2005/05/18 dadams
;;     Fixed typo: "oneoneone" -> "oneonone".
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2005/05/09 dadams
;;     Major reorganization/rewrite.  Created, from previous version setup-frames.el.
;;       Added prefix "1on1-".
;;       Encapsulated stuff in new command 1on1-emacs.
;; 2005/01/29 dadams
;;     1on1-default-frame-font: Fixed bug - misplaced parens, so no good if not Windows.
;; 2005/01/19 dadams
;;     Use defcustom now.
;;     Removed (put ... 'variable-interactive...).
;;     1on1-minibuffer-frame-top/bottom: Must be an integer (for set-frame-position).
;; 2004/12/18 dadams
;;     Bind after-make-frame-functions to nil when create 1on1-minibuffer-frame.
;; 2004/11/26 dadams
;;     Removed ;;;###autoload's.
;; 2004/11/20 dadams
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/10/01 dadams
;;     Ensure loaded before compile.
;;     No fringe.
;;     Remove *info* and *Customiz.* buffers from `same-window-regexps'
;; 2004/09/21 dadams
;;     Updated to work with Emacs 21 (and Emacs 20).
;; 2004/03/19 dadams
;;     1on1-minibuffer-frame-width -> 1on1-set-minibuffer-frame-width.
;;     added 1on1-set-minibuffer-frame-top/bottom.
;; 2001/01/05 dadams
;;     1. 1on1-minibuffer-frame-width: Use 1on1-minibuffer-frame arg for frame-char-width.
;;     2. Don't define width when initially set 1on1-minibuffer-frame-alist.  Instead,
;;        use set-frame-width afterward, so 1on1-minibuffer-frame-width uses correct
;;        character size.
;; 2001/01/05 dadams
;;     1. These vars no longer user options (interactively changeable):
;;        1on1-completions-frame-background, 1on1-completions-frame-mouse+cursor-color,
;;        1on1-help-frame-background, 1on1-help-frame-mouse+cursor-color,
;;        1on1-minibuffer-frame-cursor-color, 1on1-minibuffer-frame-font,
;;        1on1-minibuffer-frame-foreground, 1on1-minibuffer-frame-height,
;;        1on1-minibuffer-frame-mouse-color, 1on1-minibuffer-frame-top/bottom,
;;        1on1-minibuffer-frame-width.
;;     2. Added: 1on1-minibuffer-frame-width (function),
;;               1on1-minibuffer-frame-width-percent (var).
;;     3. Changed var 1on1-minibuffer-frame-width to nil default (now use *-percent).
;; 2000/09/27 dadams
;;     1. Added: 1on1-display-*Completions*-frame, 1on1-display-*Help*-frame.
;;     2. *Help* & *Completions* frames not created here.  Instead, use
;;        special-display-buffer-names & display-*-frame fns to define them.
;;     3. Added: top-level, abort-recursive-edit.
;; 1999/08/24 dadams
;;     1. Windows: win32-grab-focus-on-raise = nil.
;;     2. 1on1-default-frame-font different if Windows.
;;     3. Added: 1on1-separate-minibuffer-frame-flag, 1on1-menu-bar-lines,
;;        1on1-upper-left-frame-corner-default, 1on1-default-frame-size,
;;        1on1-upper-left-special-display-frame-corner-default,
;;        1on1-default-special-display-frame-size, 1on1-default-special-frame-foreground,
;;        1on1-default-special-frame-background, 1on1-default-special-frame-font,
;;        1on1-default-special-frame-mouse-color, 1on1-default-special-frame-cursor-color.
;;     4. Use new vars to define default-frame-alist, special-display-frame-alist.
;;     5. Only create built-in frames if 1on1-separate-minibuffer-frame-flag.
;;     6. Protected refs to x-* vars.
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

(eval-and-compile (require 'cl)) ;; pushnew (plus, for Emacs <20: when, unless)

(require 'frame-cmds nil t) ;; (no error if not found):
                            ;; remove-window, remove-windows-on, rename-frame
(require 'files+ nil t) ;; (no error if not found): switch-to-buffer-other-frame


;; Ensure that this is loaded before compiling it.
(provide 'oneonone)
(require 'oneonone)


;; To quiet the byte compiler
;; TEST IS TEMPORARY - change to (string-match "22.x" emacs-version) after 22.x release
(unless (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
  (defvar x-pointer-box-spiral)
  (defvar x-pointer-xterm))

;;;;;;;;;;;;;;;;;;;;;;;;


(defgroup oneonone nil
  "Options to define initial frame configuration."
  :group 'oneonone :group 'frames)


;;; Minibuffer frame: ********************************
;;;
(defvar 1on1-minibuffer-frame nil
  "Minibuffer-only frame used by One-on-One Emacs.
Note: This is not used if `1on1-minibuffer-frame-flag' is nil.")

(defvar 1on1-minibuffer-frame-flag t
  "*Non-nil means use a separate, specialized frame for the minibuffer.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-minibuffer-frame-foreground "Red"
  "*Default foreground color for the minibuffer frame.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

This is used only to define the standard value of
`1on1-minibuffer-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-minibuffer-frame-background "PaleGoldenrod"
  "*Initial color of the `1on1-minibuffer-frame' background.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

This is used only to define the standard value of
`1on1-minibuffer-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defcustom 1on1-active-minibuffer-frame-background "light gray"
  "*The color of the `1on1-minibuffer-frame' when it is active.
See `1on1-color-active-minibuffer-frame'.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil."
  :type 'string :group 'oneonone)

(defcustom 1on1-inactive-minibuffer-frame-background 1on1-minibuffer-frame-background
  "*The color of the `1on1-minibuffer-frame' when it is inactive.
See `1on1-color-inactive-minibuffer-frame'.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil."
  :type 'string :group 'oneonone)

(defcustom 1on1-isearch-minibuffer-frame-background "bisque"
  "*Color of the `1on1-minibuffer-frame' when `isearch' is active.
See `1on1-color-isearch-minibuffer-frame'.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil."
  :type 'string :group 'oneonone)

(defvar 1on1-minibuffer-frame-font
  (if (eq system-type 'windows-nt)
      "-*-Lucida Console-normal-r-*-*-14-112-96-96-c-*-iso8859-1"
      ;;;;;;;"-*-Lucida Console-normal-r-*-*-15-*-*-*-c-*-*-ansi-"
    "-Misc-Fixed-Medium-R-Normal--15-140-75-75-C-90-ISO8859-1")
  "*Default font for the minibuffer frame.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

This is used only to define the standard value of
`1on1-minibuffer-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-minibuffer-frame-mouse-color "Black"
  "*Default mouse color for the minibuffer frame.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

This is used only to define the standard value of
`1on1-minibuffer-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-minibuffer-frame-cursor-color "Black"
  "*Default text cursor color for the minibuffer frame.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

This is used only to define the standard value of
`1on1-minibuffer-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defcustom 1on1-minibuffer-frame-top/bottom nil
  "*Position of top (or bottom) of minibuffer frame, in pixels.
If nil, function `1on1-set-minibuffer-frame-top/bottom' will position
minibuffer at bottom of display.

An integer.  If negative, then the position is that of the frame
bottom relative to the screen bottom.

See `default-frame-alist' for an explanation of frame parameters.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for it to take effect."
  :type '(choice (const :tag "Use function `1on1-set-minibuffer-frame-top/bottom'" nil)
                 (integer :tag "Pixels from top (>= 0) or bottom (< 0)" :value 0))
  :group 'oneoneone)

(defvar 1on1-minibuffer-frame-height 2
  "*Height of minibuffer frame, in characters.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

This is used only to define the standard value of
`1on1-minibuffer-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defcustom 1on1-minibuffer-frame-width nil
  "Width, in characters, for minibuffer frame.
If nil, then function `1on1-set-minibuffer-frame-width' is used instead.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for it to take effect."
  :type '(choice (const :tag "Use function `1on1-set-minibuffer-frame-width'" nil)
                 (integer :tag "Width, in characters, for minibuffer frame" :value 0))
  :group 'oneoneone)

(defcustom 1on1-minibuffer-frame-width-percent 100
  "Max percent of the total display width to give to minibuffer frame.
See function `1on1-set-minibuffer-frame-width'.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for it to take effect."
  :type 'integer :group 'oneonone)

;; Use values from the standard list, when available.  However, we have no way of
;; distinguishing values predefined in vanilla Emacs from user settings.
(defcustom 1on1-minibuffer-frame-alist
  (list
   (or (assq 'foreground-color minibuffer-frame-alist)
       (cons 'foreground-color 1on1-minibuffer-frame-foreground))
   (or (assq 'background-color minibuffer-frame-alist)
       (cons 'background-color 1on1-minibuffer-frame-background))
   (or (assq 'font minibuffer-frame-alist)
       (cons 'font 1on1-minibuffer-frame-font))
   (or (assq 'mouse-color minibuffer-frame-alist)
       (cons 'mouse-color 1on1-minibuffer-frame-mouse-color))
   (or (assq 'cursor-color minibuffer-frame-alist)
       (cons 'cursor-color 1on1-minibuffer-frame-cursor-color))
   (or (assq 'menu-bar-lines minibuffer-frame-alist)
       (cons 'menu-bar-lines nil))
   (or (assq 'height minibuffer-frame-alist)
       (cons 'height 1on1-minibuffer-frame-height))
   (or (assq 'icon-type minibuffer-frame-alist)
       (cons 'icon-type (< emacs-major-version 21))) ; `t' for Emacs 21 too?
   (or (assq 'minibuffer minibuffer-frame-alist)
       (cons 'minibuffer 'only))
   (or (assq 'user-position minibuffer-frame-alist)
       (cons 'user-position t))
   (or (assq 'vertical-scroll-bars minibuffer-frame-alist) ;  No scroll bar.
       (cons 'vertical-scroll-bars nil))
   (or (assq 'name minibuffer-frame-alist)
       (cons 'name "Emacs Minibuffer")))
  "Frame-parameter alist for the standalone minibuffer frame
`1on1-minibuffer-frame'.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for it to take effect."
  ;; If we didn't need Emacs 20 compatibility, this could be:
  ;; :type '(alist :key-type symbol :value-type sexp)
  :type '(repeat (cons :format "%v" (symbol :tag "Frame Parameter") (sexp :tag "Value")))
  :group 'oneonone)


;;; *Help* frame: ********************************
;;;   Display of *Help* buffer in custom frame.
;;;   Background, height, cursor and pointer colors.
;;;
(defcustom 1on1-*Help*-frame-flag t
  "*Non-nil means use a separate, specialized frame for buffer *Help*.

If you customize this variable, you will need to rerun `1on1-emacs'
for it to take effect."
  :type 'boolean :group 'oneonone)

(defcustom 1on1-help-frame-background "Thistle"
  "Default background color for the *Help* buffer's frame.

Note: This is not used if `1on1-*Help*-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for it to take effect."
  :type 'string :group 'oneonone)

(defcustom 1on1-help-frame-mouse+cursor-color "Blue Violet"
  "Default color for cursor & pointer of *Help* frame.

Note: This is not used if `1on1-*Help*-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for it to take effect."
  :type 'string :group 'oneonone)


;;; *Completions* frame: ********************************
;;;   Display of *Completion* buffer in custom frame.
;;;   Background, height, cursor and pointer colors.
;;;
(defcustom 1on1-*Completions*-frame-flag t
  "*Non-nil means use a specialized frame for *Completions*.

If you customize this variable, you will need to rerun `1on1-emacs'
for it to take effect."
  :type 'boolean :group 'oneonone)

(defcustom 1on1-completions-frame-background "LavenderBlush2"
  "Default background color for the *Completions* buffer's frame.

Note: This is not used if `1on1-*Completions*-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for it to take effect."
  :type 'string :group 'oneonone)

(defcustom 1on1-completions-frame-mouse+cursor-color "VioletRed"
  "Default color for cursor & pointer of *Completions* frame.

Note: This is not used if `1on1-*Completions*-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for it to take effect."
  :type 'string :group 'oneonone)


;;; `1on1-default-frame-alist'
;;;
(defvar 1on1-default-frame-foreground "Black"
  "*Default foreground color for non-special frames.
This is used only to define the standard value of
`1on1-default-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-default-frame-background "LightBlue"
  "*Default background color for non-special frames.
This is used only to define the standard value of
`1on1-default-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-default-frame-font
  (if (eq system-type 'windows-nt)
      "-*-Lucida Console-normal-r-*-*-14-112-96-96-c-*-iso8859-1"
      ;;;;;;"-*-Lucida Console-normal-r-*-*-15-*-*-*-c-*-*-ansi-"
    "-Misc-Fixed-Medium-R-Normal--15-140-75-75-C-90-ISO8859-1")
  "*Default font for non-special frames.
This is used only to define the standard value of
`1on1-default-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-default-frame-mouse-color "Red"
  "*Default mouse color for non-special frames.
This is used only to define the standard value of
`1on1-default-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-default-frame-cursor-color "Red"
  "*Default text cursor color for non-special frames.
This is used only to define the standard value of
`1on1-default-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-default-frame-menu-bar-lines 1
  "*Number of lines used for the menu bar in non-special frames.
This is used only to define the standard value of
`1on1-default-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-default-frame-upper-left-corner '(0 . 0)
  "*Position of upper left frame corner.
A cons whose car is the distance from the top in pixels
and whose cdr is the distance from the left in pixels.

This is used only to define the standard value of
`1on1-default-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-default-frame-size '(80 . 35)
  "*Default frame size.
A cons whose car is the frame width in pixels
and whose cdr is the frame height in pixels.

This is used only to define the standard value of
`1on1-default-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

;; Use values from the standard list, when available.  However, we have no way of
;; distinguishing values predefined in vanilla Emacs from user settings.
(defcustom 1on1-default-frame-alist
  (list
   (or (assq 'foreground-color default-frame-alist)
       (cons 'foreground-color 1on1-default-frame-foreground))
   (or (assq 'background-color default-frame-alist)
       (cons 'background-color 1on1-default-frame-background))
   (or (assq 'font default-frame-alist)
       (cons 'font 1on1-default-frame-font))
   (or (assq 'mouse-color default-frame-alist)
       (cons 'mouse-color 1on1-default-frame-mouse-color))
   (or (assq 'cursor-color default-frame-alist)
       (cons 'cursor-color 1on1-default-frame-cursor-color))
   (or (assq 'menu-bar-lines default-frame-alist)
       (cons 'menu-bar-lines 1on1-default-frame-menu-bar-lines))
   (or (assq 'top default-frame-alist)
       (cons 'top (car 1on1-default-frame-upper-left-corner)))
   (or (assq 'left default-frame-alist)
       (cons 'left (cdr 1on1-default-frame-upper-left-corner)))
   (or (assq 'width default-frame-alist)
       (cons 'width (car 1on1-default-frame-size)))
   (or (assq 'height default-frame-alist)
       (cons 'height (cdr 1on1-default-frame-size)))
   (or (assq 'minibuffer default-frame-alist)
       (cons 'minibuffer (not 1on1-minibuffer-frame-flag)))
   (or (assq 'user-position default-frame-alist)
       (cons 'user-position t))
   (or (assq 'vertical-scroll-bars default-frame-alist)
       (cons 'vertical-scroll-bars 'right))
   (or (assq 'icon-type default-frame-alist)
       (cons 'icon-type (< emacs-major-version 21))) ; `t' for Emacs 21 too?
   (or (assq 'tool-bar-lines default-frame-alist)
       (cons 'tool-bar-lines 1))        ; Emacs 21+
   (or (assq 'left-fringe default-frame-alist)
       (cons 'left-fringe 0))           ; Emacs 21+
   (or (assq 'right-fringe default-frame-alist)
       (cons 'right-fringe 0))          ; Emacs 21+
   (or (assq 'fringe default-frame-alist)
       (cons 'fringe 0)))               ; Emacs 21, but not 21.3.50 - REMOVE after 22.x
  "Properties to be used for One-on-One Emacs `default-frame-alist'.
If you customize this variable, you will need to rerun `1on1-emacs'
for it to take effect."
  ;; If we didn't need Emacs 20 compatibility, this could be:
  ;; :type '(alist :key-type symbol :value-type sexp)
  :type '(repeat (cons :format "%v" (symbol :tag "Frame Parameter") (sexp :tag "Value")))
  :group 'oneonone)

;;; `1on1-special-display-frame-alist'
;;;
(defvar 1on1-default-special-frame-foreground "Black"
  "*Default foreground color for special display frames.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

(defvar 1on1-default-special-frame-background "LightSteelBlue"
  "*Default background color for special display frames.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

(defvar 1on1-default-special-frame-font
  (if (eq system-type 'windows-nt)
      "-*-Lucida Console-normal-r-*-*-14-112-96-96-c-*-iso8859-1"
      ;;;;;;;;"-*-Lucida Console-normal-r-*-*-15-*-*-*-c-*-*-ansi-"
    "-Misc-Fixed-Medium-R-Normal--15-140-75-75-C-90-ISO8859-1")
  "*Default font for special display frames.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

(defvar 1on1-default-special-frame-mouse-color "Yellow"
  "*Default mouse color for special display frames.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

(defvar 1on1-default-special-frame-cursor-color "Yellow"
  "*Default text cursor color for special display frames.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

(defvar 1on1-default-special-frame-menu-bar-lines 1
  "*Number of lines used for the menu bar of special display frames.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

(defvar 1on1-default-special-frame-upper-left-corner '(0 . 0)
  "*Position of upper left corner of special display frames.
A cons whose car is the distance from the top in pixels
and whose cdr is the distance from the left in pixels.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

(defvar 1on1-default-special-frame-size '(80 . 20)
  "*Default size of special display frames.
A cons whose car is the frame width in pixels
and whose cdr is the frame height in pixels.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

;; Use values from the standard list, when available.  However, we have no way of
;; distinguishing values predefined in vanilla Emacs from user settings.
(defcustom 1on1-special-display-frame-alist
  (list
   (or (assq 'font special-display-frame-alist)
       (cons 'font 1on1-default-special-frame-font))
   (or (assq 'width special-display-frame-alist)
       (cons 'width (car 1on1-default-special-frame-size)))
   (or (assq 'height special-display-frame-alist)
       (cons 'height (cdr 1on1-default-special-frame-size)))
   (or (assq 'mouse-color special-display-frame-alist)
       (cons 'mouse-color 1on1-default-special-frame-mouse-color))
   (or (assq 'cursor-color special-display-frame-alist)
       (cons 'cursor-color 1on1-default-special-frame-cursor-color))
   (or (assq 'menu-bar-lines special-display-frame-alist)
       (cons 'menu-bar-lines 1on1-default-special-frame-menu-bar-lines))
   (or (assq 'foreground-color special-display-frame-alist)
       (cons 'foreground-color 1on1-default-special-frame-foreground))
   (or (assq 'background-color special-display-frame-alist)
       (cons 'background-color 1on1-default-special-frame-background))
   (or (assq 'top special-display-frame-alist)
       (cons 'top (car 1on1-default-special-frame-upper-left-corner)))
   (or (assq 'left special-display-frame-alist)
       (cons 'left (cdr 1on1-default-special-frame-upper-left-corner)))
   (or (assq 'unsplittable special-display-frame-alist)
       (cons 'unsplittable t))
   (or (assq 'user-position special-display-frame-alist)
       (cons 'user-position t))
   (or (assq 'vertical-scroll-bars special-display-frame-alist)
       (cons 'vertical-scroll-bars 'right)))
  "Properties to be used for One-on-One `special-display-frame-alist'.
If you customize this variable, you will need to rerun `1on1-emacs'
for it to take effect."
  ;; If we didn't need Emacs 20 compatibility, this could be:
  ;; :type '(alist :key-type symbol :value-type sexp)
  :type '(repeat (cons :format "%v" (symbol :tag "Frame Parameter") (sexp :tag "Value")))
  :group 'oneonone)



;;; Main command
;;;
;;;###autoload
(defun 1on1-emacs ()
  "One-on-One Emacs setup.
Use `1on1-default-frame-alist' and `1on1-special-display-frame-alist'.

If `1on1-minibuffer-frame-flag' is non-nil, then create
   minibuffer-only frame, `1on1-minibuffer-frame', using
   `1on1-minibuffer-frame-alist'.

If `1on1-separate-minibuffer-*Help*-flag' is non-nil, then use
   special frame for *Help* buffer.

If `1on1-separate-minibuffer-*Completions*-flag' is non-nil, then
   use special frame for *Completions* buffer."
  (interactive)
  (setq default-frame-alist (append 1on1-default-frame-alist default-frame-alist)
        special-display-frame-alist (append 1on1-special-display-frame-alist
                                            special-display-frame-alist)
        initial-frame-alist default-frame-alist)

  ;; *Help* frame
  (if 1on1-*Help*-frame-flag
      (add-to-list
       'special-display-buffer-names
       (list "*Help*" '1on1-display-*Help*-frame
             (list (cons 'background-color 1on1-help-frame-background)
                   (cons 'mouse-color 1on1-help-frame-mouse+cursor-color)
                   (cons 'cursor-color 1on1-help-frame-mouse+cursor-color)
                   '(height . 40))))
    (setq special-display-buffer-names
          (remove-if (lambda (elt) (equal "*Help*" (car elt)))
                     special-display-buffer-names)))

  ;; *Completions* frame
  (if 1on1-*Completions*-frame-flag
      (add-to-list
       'special-display-buffer-names
       (list "*Completions*" '1on1-display-*Completions*-frame
             (list (cons 'background-color 1on1-completions-frame-background)
                   (cons 'mouse-color 1on1-completions-frame-mouse+cursor-color)
                   (cons 'cursor-color 1on1-completions-frame-mouse+cursor-color))))
    (setq special-display-buffer-names
          (remove-if (lambda (elt) (equal "*Completions*" (car elt)))
                     special-display-buffer-names)))

  ;; Minibuffer frame
  (when 1on1-minibuffer-frame-flag
    ;; `display-buffer' (& `*-other-window' fns) will use separate frames.
    (setq pop-up-frames      t
          pop-up-frame-alist (append default-frame-alist pop-up-frame-alist))

    ;; Set up `1on1-minibuffer-frame'.
    (setq minibuffer-frame-alist (append 1on1-minibuffer-frame-alist
                                         minibuffer-frame-alist))
    (if 1on1-minibuffer-frame
        (modify-frame-parameters 1on1-minibuffer-frame 1on1-minibuffer-frame-alist)
      (setq 1on1-minibuffer-frame
            (let ((after-make-frame-functions nil)) ; E.g. inhibit `fit-frame'.
              (make-frame 1on1-minibuffer-frame-alist))))

    ;; Resize and reposition it.  If variable `1on1-minibuffer-frame-width'
    ;; or `1on1-minibuffer-frame-top/bottom' is nil, calculate automatically.
    (1on1-set-minibuffer-frame-width)
    (1on1-set-minibuffer-frame-top/bottom)

    ;; Rename minibuffer frame. (`rename-frame' is defined in `frame-cmds.el'.)
    (when (fboundp 'rename-frame)
      (rename-frame 1on1-minibuffer-frame "Emacs minibuffer                         \
show/hide: hold CTRL + click in window"))
    (setq minibuffer-auto-raise t)
    ;; Background colors of minibuffer frame: 3 states
    (add-hook 'isearch-mode-hook '1on1-color-isearch-minibuffer-frame)
    (add-hook 'isearch-mode-end-hook '1on1-color-inactive-minibuffer-frame)
    (add-hook 'minibuffer-setup-hook '1on1-color-active-minibuffer-frame)
    (add-hook 'minibuffer-exit-hook '1on1-color-inactive-minibuffer-frame)
    ;; Redefine built-in fns so they color minibuffer frame.
    (1on1-setup-minibuffer-frame-coloring)))


(defun 1on1-display-*Help*-frame (buf &optional args)
  "Display *Help* buffer in its own frame.
`special-display-function' is used to do the actual displaying.
BUF and ARGS are the arguments to `special-display-function'."
  (let ((old-ptr-shape x-pointer-shape)
        return-window)
    (when (boundp 'x-pointer-xterm)
      (setq x-pointer-shape x-pointer-xterm))
    (setq return-window (select-window (funcall special-display-function buf args)))
    (raise-frame)
    (setq x-pointer-shape old-ptr-shape)
    return-window))

(defun 1on1-display-*Completions*-frame (buf &optional args)
  "Display *Completions* buffer in its own frame.
`special-display-function' is used to do the actual displaying.
Completion input events are redirected to `1on1-minibuffer-frame'.
BUF and ARGS are the arguments to `special-display-function'."
  (let ((old-ptr-shape x-pointer-shape)
        return-window)
    (when (boundp 'x-pointer-box-spiral)
      (setq x-pointer-shape x-pointer-box-spiral))
    (setq return-window (select-window (funcall special-display-function buf args)))
    (raise-frame)
    (when (boundp '1on1-minibuffer-frame)
      (redirect-frame-focus (selected-frame) 1on1-minibuffer-frame))
    (setq x-pointer-shape old-ptr-shape)
    return-window))

(defun 1on1-set-minibuffer-frame-top/bottom ()
  "Set position of minibuffer frame.
Use `1on1-minibuffer-frame-top/bottom' if non-nil.
Else, place minibuffer at bottom of display."
  (when (boundp '1on1-minibuffer-frame)
    (modify-frame-parameters
     1on1-minibuffer-frame
     `((top ,@ (or 1on1-minibuffer-frame-top/bottom
                   (- (frame-char-height 1on1-minibuffer-frame))))))))

(defun 1on1-set-minibuffer-frame-width ()
  "Set width of minibuffer frame, in characters.
Use `1on1-minibuffer-frame-width' if not nil.
Else, set width relative to character size of `1on1-minibuffer-frame'
and display size, and depending on
`1on1-minibuffer-frame-width-percent':

  (/ (* 1on1-minibuffer-frame-width-percent (x-display-pixel-width))
     (* 100 (frame-char-width 1on1-minibuffer-frame)))"
  (when (boundp '1on1-minibuffer-frame)
    (set-frame-width
     1on1-minibuffer-frame
     (or 1on1-minibuffer-frame-width
         (/ (* 1on1-minibuffer-frame-width-percent (x-display-pixel-width))
            (* 100 (frame-char-width 1on1-minibuffer-frame)))))))

(defun 1on1-color-active-minibuffer-frame ()
  "Use `1on1-active-minibuffer-frame-background' for minibuffer."
  (and (boundp '1on1-minibuffer-frame)
       (save-window-excursion
         (select-frame 1on1-minibuffer-frame)
         (set-background-color 1on1-active-minibuffer-frame-background))))

(defun 1on1-color-inactive-minibuffer-frame ()
  "Use `1on1-inactive-minibuffer-frame-background' for minibuffer."
  (and (boundp '1on1-minibuffer-frame)
       (save-window-excursion
         (select-frame 1on1-minibuffer-frame)
         (set-background-color 1on1-inactive-minibuffer-frame-background))))

(defun 1on1-color-isearch-minibuffer-frame ()
  "Use `1on1-isearch-minibuffer-frame-background' for minibuffer."
  (and (boundp '1on1-minibuffer-frame)
       (save-window-excursion
         (select-frame 1on1-minibuffer-frame)
         (set-background-color
          ;; Can also try `x-defined-colors', defined in `x-win.el'.
          ;; It contains all colors currently supported by X windows.
          (if (x-color-defined-p 1on1-isearch-minibuffer-frame-background)
              1on1-isearch-minibuffer-frame-background
            "white")))))

(defun 1on1-flash-ding-minibuffer-frame (&optional do-not-terminate)
  "Ring bell (`ding'), after flashing minibuffer frame, if relevant.
Terminates any keyboard macro executing, unless arg DO-NOT-TERMINATE non-nil."
  (flash-ding do-not-terminate (and (boundp '1on1-minibuffer-frame)
                                    1on1-minibuffer-frame)))

(defun 1on1-setup-minibuffer-frame-coloring ()
  "Redefine some built-in functions so they color the minibuffer frame.
Functions redefined: `y-or-n-p', `top-level', `abort-recursive-exit'."

  (or (fboundp 'old-y-or-n-p)
      (fset 'old-y-or-n-p (symbol-function 'y-or-n-p)))

  ;; REPLACES ORIGINAL (built-in function):
  ;; Temporarily colors minibuffer frame to "active" color.
  ;;
  (defun y-or-n-p (prompt)
    "Ask user a \"y or n\" question.  Return t if answer is \"y\".
Takes one argument, which is the string to display to ask the question.
It should end in a space; `y-or-n-p' adds `(y or n) ' to it.
No confirmation of answer is requested; a single character is enough.
Also accepts SPC to mean yes, or DEL to mean no."
    (1on1-color-active-minibuffer-frame)
    (prog1 (old-y-or-n-p prompt) (1on1-color-inactive-minibuffer-frame)))


  (or (fboundp 'old-top-level)
      (fset 'old-top-level (symbol-function 'top-level)))

  ;; REPLACES ORIGINAL (built-in function):
  ;; Resets color of minibuffer frame to "inactive" color.
  ;;
  (defun top-level ()
    "Exit all recursive editing levels."
    (interactive)
    (1on1-color-inactive-minibuffer-frame)
    (old-top-level))


  (or (fboundp 'old-abort-recursive-edit)
      (fset 'old-abort-recursive-edit (symbol-function 'abort-recursive-edit)))

  ;; REPLACES ORIGINAL (built-in function):
  ;; Resets color of minibuffer frame to "inactive" color.
  ;;
  (defun abort-recursive-edit ()
    "Abort command that requested this recursive edit or minibuffer input."
    (interactive)
    (old-abort-recursive-edit)
    (1on1-color-inactive-minibuffer-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; oneonone.el ends here
