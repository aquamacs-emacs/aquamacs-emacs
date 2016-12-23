;;; oneonone.el --- Frame configuration that uses one frame per window.
;;
;; Filename: oneonone.el
;; Description: Frame configuration that uses one frame per window.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1999-2016, Drew Adams, all rights reserved.
;; Created: Fri Apr  2 12:34:20 1999
;; Version: 0
;; Package-Requires: ((hexrgb "0"))
;; Last-Updated: Thu Dec 31 15:46:34 2015 (-0800)
;;           By: dradams
;;     Update #: 3087
;; URL: http://www.emacswiki.org/oneonone.el
;; Doc URL: http://emacswiki.org/OneOnOneEmacs
;; Keywords: local, frames
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `frame-cmds', `frame-fns', `hexrgb', `misc-fns',
;;   `oneonone', `strings', `thingatpt', `thingatpt+', `zoom-frm'.
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
;;  You can configure each of the frames defined here.
;;
;;  Default properties are defined here for normal frames and
;;  "special" frames, which show "special-display buffers" (see Emacs
;;  manual for info on such frames).
;;
;;  In addition, these user options control the creation of three
;;  separate, specialized frames:
;;
;;    - `1on1-*Help*-frame-flag' - `*Help*' buffer frame
;;    - `1on1-*Completions*-frame-flag' - `*Completions*' buffer frame
;;    - `1on1-minibuffer-frame-flag' - minibuffer frame
;;
;;  Buffers `*Help*' and `*Completions*' are always displayed in their
;;  own frames.  In addition, if `1on1-*Help*-frame-flag' or
;;  `1on1-*Completions*-frame-flag' is non-nil, then the `*Help*' or
;;  `*Completions*' frame has a special (customizable) appearance.
;;
;;  If `1on1-minibuffer-frame-flag' is non-nil (the default value),
;;  then the minibuffer is shown in its own frame,
;;  `1on1-minibuffer-frame'; this is the only frame to have a
;;  minibuffer.  If you customize `1on1-minibuffer-frame-flag' to nil,
;;  then each frame will have its own minibuffer, as usual, and there
;;  will be no standalone minibuffer frame.
;;
;;  By default, if you use a standalone minibuffer frame, it is
;;  automatically sized to the full width of your display and placed
;;  at the bottom of the display.
;;
;;  If you use a standalone minibuffer frame then option
;;  `minibuffer-auto-raise' can make a difference.  This library does
;;  not change the option value.  A value of nil can make sense for
;;  some window managers that force the refocusing of a frame whenever
;;  it is raised.  If you use MS Windows then this is not a problem:
;;  command `1on1-emacs' sets the Windows-specific option
;;  `w32-grab-focus-on-raise' to nil, so that frame raising and
;;  focusing are decoupled.  So on MS Windows, at least, a non-nil
;;  value for `minibuffer-auto-raise' can make sense.
;;
;;  If `1on1-fit-minibuffer-frame-flag' is non-nil,
;;  `1on1-minibuffer-frame-flag' is non-nil, and you also use library
;;  `fit-frame.el', then, whenever the minibuffer is active, the
;;  minibuffer frame height is automatically adjusted to fit its
;;  content after each command or user event (e.g. each key press).
;;  Options `1on1-fit-minibuffer-frame-max-height' and
;;  `1on1-fit-minibuffer-frame-max-height-percent' define the maximum
;;  possible height for this behavior.  In addition, if you bind
;;  `1on1-fit-minibuffer-frame' to a key (I use `C-o'), then you can
;;  use that key repeatedly to increase the height by one line, even
;;  beyond the maximum.
;;
;;  To help you perceive changes to different minibuffer recursion
;;  levels, the background color of the minibuffer frame is changed
;;  slightly with each recursion-depth change.
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
;;  http://www.emacswiki.org/OneOnOneEmacs.
;;
;;  To use this library, put the following at the *END* of your init
;;  file, `.emacs' (or `_emacs').  In particular, if your init file
;;  contains a `custom-set-variables' expression, then the following
;;  must appear *AFTER* that expression, in order for this to take
;;  into account your customizations of any `1on1-' user options.
;;
;;    (require 'oneonone)
;;    (1on1-emacs)
;;
;;  Initial frame: By default, the initial Emacs frame is like all
;;  other normal (non-special-display) frames; that is,
;;  `initial-frame-alist' effectively uses the frame properties
;;  defined in `default-frame-alist'.  If you would like the initial
;;  frame to be different, set `default-frame-alist' to nil after
;;  requiring `oneonone.el' but before executing `1on1-emacs':
;;
;;    (require 'oneonone)
;;    (setq default-frame-alist  nil)
;;    (setq initial-frame-alist  '((background-color . "White"))); e.g.
;;    (1on1-emacs)
;;
;;  If you want the text cursor to change to a box when Emacs is idle,
;;  then add this line also to your init file:
;;
;;    (toggle-box-cursor-when-idle 1) ; Turn on box cursor when idle.
;;
;;  Info and Customize frames: I recommend that you put the following
;;  code in your init file, so that Info and Customize buffers will
;;  display in their own frames.  Which code to use depends on your
;;  version of GNU Emacs.
;;
;;    (cond ((< emacs-major-version 21)
;;           (remove-hook 'same-window-buffer-names "*info*"))
;;          ((= emacs-version 21)
;;           (remove-hook 'same-window-buffer-names "*info*")
;;           (remove-hook 'same-window-regexps "\\`\\*Customiz.*\\*\\'"))
;;          (t
;;           (remove-hook 'same-window-regexps "\\*info\\*\\(\\|<[0-9]+>\\)")
;;           (remove-hook 'same-window-regexps "\\`\\*Customiz.*\\*\\'")))
;;
;;  Recommended key bindings (requires library `fit-frame.el'):
;;
;;    (define-key minibuffer-local-map "\C-o"
;;                '1on1-fit-minibuffer-frame)
;;    (define-key minibuffer-local-must-match-map "\C-o"
;;                '1on1-fit-minibuffer-frame)
;;    (define-key minibuffer-local-completion-map "\C-o"
;;                '1on1-fit-minibuffer-frame)
;;
;;  By default, `oneonone.el' sets the width of the bottom and right
;;  dividers, which separate Emacs windows, to 2 instead of 0.  This
;;  lets you more easily notice where to drag with your mouse, to
;;  resize windows.  If you use Emacs 24.4 or later then I also
;;  recommend that you consider customizing face `window-divider', to
;;  further highlight the dividers.
;;
;;
;;  Notes on user options defined here:
;;  ---------------------------------
;;
;;  Some non-option variables are used here only as conveniences to
;;  define frame-parameter alists.  They are defined using `defvar',
;;  not `defcustom', because you cannot use Customize to define them
;;  independently of the alist user options they help to define.  The
;;  alists themselves are the variables to customize.  If you want to
;;  change the `defvar' variables individually and then use them to
;;  set the alist variables, then use `setq', not Customize, to change
;;  them, and restart Emacs for their changes to take effect.
;;
;;  Changes to any user options defined here take effect as soon as
;;  `1on1-emacs' is executed, so you can do `M-x 1on1-emacs' to see
;;  their changes (no need to restart Emacs).
;;
;;  User options `1on1-color-minibuffer-frame-on-setup-increment' and
;;  `1on1-color-minibuffer-frame-on-exit-increment' determine how much
;;  to change the color of the minibuffer frame when the minibuffer is
;;  entered and exited.  They are hue increments, and should be
;;  opposite in sign.
;;
;;  They should cancel each other out, so that the color returns to
;;  what it was initially at any given minibuffer depth.  However,
;;  because of the way HSV and RGB color-component conversion works,
;;  the best cancellation does not necessarily occur when these
;;  options have the same absolute value.  And how much their absolute
;;  values should differ depends on that magnitude.  It is best to
;;  just set one of these to an increment you like, and then fiddle
;;  with the other until they more or less cancel.
;;
;;
;;  Commands defined here:
;;
;;    `1on1-emacs', `1on1-fit-minibuffer-frame',
;;    `1on1-ORIG-abort-recursive-edit', `1on1-ORIG-top-level',
;;    `1on1-ORIG-y-or-n-p', `1on1-other-frame',
;;    `1on1-set-box-cursor-when-idle-interval',
;;    `1on1-set-cursor-type', `1on1-toggle-box-cursor-when-idle'.
;;
;;  User options defined here:
;;
;;    `1on1-*Completions*-frame-flag',
;;    `1on1-*Completions*-frame-at-right-flag',
;;    `1on1-*Help*-frame-flag',
;;    `1on1-active-minibuffer-frame-background',
;;    `1on1-active-mode-line-background',
;;    `1on1-change-cursor-on-input-method-flag',
;;    `1on1-change-cursor-on-overwrite/read-only-flag',
;;    `1on1-color-minibuffer-frame-on-exit-increment',
;;    `1on1-color-minibuffer-frame-on-setup-increment',
;;    `1on1-color-mode-line-flag',
;;    `1on1-completions-frame-background',
;;    `1on1-completions-frame-mouse+cursor-color',
;;    `1on1-completions-frame-width',
;;    `1on1-completions-frame-zoom-font-difference',
;;    `1on1-default-frame-cursor-color',
;;    `1on1-default-frame-cursor-color-input-method',
;;    `1on1-default-frame-cursor-type',
;;    `1on1-default-frame-cursor-type-overwrite/read-only',
;;    `1on1-default-frame-alist', `1on1-help-frame-background',
;;    `1on1-help-frame-mouse+cursor-color',
;;    `1on1-inactive-minibuffer-frame-background',
;;    `1on1-inactive-mode-line-background',
;;    `isearch-minibuffer-frame-background',
;;    `1on1-minibuffer-frame-alist', `1on1-minibuffer-frame-flag',
;;    `1on1-minibuffer-frame-left',
;;    `1on1-minibuffer-frame-top/bottom',
;;    `1on1-minibuffer-frame-width',
;;    `1on1-minibuffer-frame-width-percent',
;;    `1on1-remap-other-frame-command-flag',
;;    `1on1-special-display-frame-alist', `1on1-task-bar-height'
;;    (Emacs < 24.4).
;;
;;  Non-interactive functions defined here:
;;
;;    `1on1-box-cursor-when-idle',
;;    `1on1-change-cursor-on-input-method',
;;    `1on1-change-cursor-on-overwrite/read-only',
;;    `1on1-color-minibuffer-frame-on-exit',
;;    `1on1-color-minibuffer-frame-on-setup',
;;    `1on1-color-isearch-minibuffer-frame',
;;    `1on1-display-*Completions*-frame', `1on1-display-*Help*-frame',
;;    `1on1-filter-no-default-minibuffer',
;;    `1on1-flash-ding-minibuffer-frame',
;;    `1on1-minibuffer-prompt-end', `1on1-reset-minibuffer-frame',
;;    `1on1-remove-if', `1on1-set-minibuffer-frame-top/bottom',
;;    `1on1-set-minibuffer-frame-width',
;;    `1on1-setup-minibuffer-frame-coloring', `1on1-setup-mode-line'.
;;
;;  Non-option variables defined here:
;;
;;    `1on1-box-cursor-when-idle-p',
;;    `1on1-box-cursor-when-idle-interval',
;;    `1on1-box-cursor-when-idle-timer',
;;    `1on1-default-frame-background', `1on1-default-frame-font',
;;    `1on1-default-frame-foreground',
;;    `1on1-default-frame-menu-bar-lines',
;;    `1on1-default-frame-mouse-color', `1on1-default-frame-size',
;;    `1on1-default-frame-upper-left-corner', `1on1-divider-width',
;;    `1on1-last-cursor-type', `1on1-minibuffer-frame',
;;    `1on1-minibuffer-frame-background',
;;    `1on1-minibuffer-frame-bottom-offset',
;;    `1on1-minibuffer-frame-cursor-color',
;;    `1on1-minibuffer-frame-font',
;;    `1on1-minibuffer-frame-foreground',
;;    `1on1-minibuffer-frame-height',
;;    `1on1-minibuffer-frame-mouse-color',
;;    `1on1-special-frame-background',
;;    `1on1-special-frame-cursor-color', `1on1-special-frame-font',
;;    `1on1-special-frame-foreground',
;;    `1on1-special-frame-menu-bar-lines',
;;    `1on1-special-frame-mouse-color', `1on1-special-frame-size',
;;    `1on1-special-frame-upper-left-corner'.
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
;;  Acknowledgements:
;;
;;  The cursor-changing on input method and read-only was inspired by
;;  Juri Linkov <juri@jurta.org>.  Joe Casadonte <joc@netaxs.com>
;;  wrote a similar hook (`joc-cursor-type-set-hook'), which he got
;;  from Steve Kemp...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; Change Log:
;;
;; 2015/08/21 dadams
;;     1on1-display-*Completions*-frame: Use icicle-mru-window-for-buffer (Emacs 24+).
;; 2015/08/20 dadams
;;     1on1-display-*Completions*-frame: Use face-remap-add-relative instead of set-face-*.
;; 2015/04/11 dadams
;;     1on1-display-*Completions*-frame: If minibuf is active, redirect to minibuf frame.
;;       And if not, do not redirect if completion-reference-buffer is *Completions*.
;; 2015/01/26 dadams
;;     1on1-display-*Completions*-frame: Use completion-reference-buffer, if displayed.
;; 2014/12/01 dadams
;;     1on1-emacs: Do not set minibuffer-auto-raise to nil.  Let user do it based on window mgr.
;;     1on1-default-frame-cursor-type(-overwrite/read-only), 1on1-emacs,
;;       1on1-change-cursor-on-overwrite/read-only:
;;         Ensure that cursor type cannot be set to nil.
;;     1on1-change-cursor-on-input-method:
;;       Do not set it if there is no setting based on null entry in default-frame-alist.
;; 2014/11/30 dadams
;;     1on1-emacs, 1on1-change-cursor-on-overwrite/read-only:
;;       Use cursor-type setting in default-frame-alist, not 1on1-default-frame-cursor-type.
;;     1on1-change-cursor-on-input-method:
;;       Use cursor-color setting in default-frame-alist, not 1on1-default-frame-cursor-color.
;; 2014/11/27 dadams
;;     Added: 1on1-task-bar-height, 1on1-minibuffer-frame-bottom-offset.
;;     1on1-emacs: Set 1on1-minibuffer-frame-bottom-offset (Emacs 24.4+ only).
;;     1on1-set-minibuffer-frame-top/bottom:
;;       Use *-minibuffer-frame-bottom-offset (or *-task-bar-height) instead of 2 * char height.
;;     1on1-set-minibuffer-frame-width: Use monitor width instead of x-display-pixel-width.
;; 2014/09/03 dadams
;;     1on1-minibuffer-frame-alist: No horizontal scroll bars.
;; 2014/08/28 dadams
;;     1on1-emacs: Removed vestigial defadvice of y-or-n-p.
;;     y-or-n-p: Fix regexp: [\n], not \n. Do not resize for Emacs 20 (it shows newlines as \n).
;; 2014/08/27 dadams
;;     y-or-n-p: Resize frame to fit PROMPT.  See Emacs bug #18340.
;;     1on1-default-frame-alist, 1on1-special-display-frame-alist: No horizontal scroll bars.
;; 2014/02/11 dadams
;;     1on1-color-minibuffer-frame-on-(exit|setup)-increment: Change type: number, not integer.
;; 2014/01/30 dadams
;;     Added: 1on1-divider-width.
;;     Renamed: 1on1-default-special-frame* to 1on1-special-frame*.
;;              This is an *incompatible change*: If you customized options with the old names
;;              then you will need recustomize using the new names.
;;     1on1-(default|special)-frame-alist: Use 1on1-divider-width.
;; 2013/12/21 dadams
;;     Removed autoload cookie: *-frame-alist.
;;     1on1-(in)active-mode-line-background: Use constant default, so can have autoload cookie.
;; 2013/08/12 dadams
;;     Added: 1on1-filter-no-default-minibuffer.
;;     1on1-minibuffer-frame-alist: Added: (cons 'desktop-dont-save t).
;;     frameset-filter-alist:  Add 1on1-filter-no-default-minibuffer and null name entries.
;; 2013/07/08 dadams
;;     1on1-color-minibuffer-frame-on-exit-increment: Set default value to 0.10.
;;     1on1-color-minibuffer-frame-on-exit:
;;       If level is 2 then set to 1on1-active-minibuffer-frame-background.
;;     1on1-setup-minibuffer-frame-coloring: Do not redefine abort-recursive-edit.
;;     y-or-n-p: Do not handle (> minibuffer-depth 0) case differently.
;; 2013/06/05 dadams
;;     *-frame-font: Changed sizes other than pixels to *, to work around an Emacs 20 bug with
;;                   x-list-fonts' on Windows 7.  See:
;;                   http://lists.gnu.org/archive/html/help-emacs-windows/2013-06/msg00009.html
;; 2013/01/18 dadams
;;     Removed: 1on1-increment-color-hue - use hexrgb-increment-hue.
;;     1on1-color-minibuffer-frame-on-(setup|exit)-increment: Divided increment by 100.
;;     1on1-color-minibuffer-frame-on-(setup|exit): use hexrgb-increment-hue.
;; 2013/01/02 dadams
;;     Added: 1on1-remap-other-frame-command-flag, 1on1-other-frame.
;; 2012/10/02 dadams
;;     Handle obsolescence of face modeline.
;; 2012/08/14 dadams
;;     1on1-fit-minibuffer-frame:
;;       Simplify by using window-frame and frame-first-window.  Thx to Martin Rudalics.
;;       Added optional arg RESETP (prefix arg), to reset to default height and position.
;; 2012/08/13 dadams
;;     Fixes to prevent losing input focus to *Completions* frame:
;;       1on1-fit-minibuffer-frame:
;;         Use save-window-excursion, not save-selected-window (2 places).
;;       1on1-set-minibuffer-frame-top/bottom: Use force-mode-line-update, not redisplay.
;; 2012/08/06 dadams
;;     Renamed: old-* to 1on1-ORIG-*:
;;       1on1-ORIG-abort-recursive-edit, 1on1-ORIG-top-level, 1on1-ORIG-y-or-n-p,
;; 2012/07/16 dadams
;;     1on1-fit-minibuffer-frame:
;;       Do nothing if last-event-frame is not the minibuffer frame.  Alternatively we could do
;;       nothing if this-command is handle-switch-frame.  Or we could just call
;;       select-frame-set-input-focus at its end to ensure minibuffer frame still has the focus.
;; 2012/03/31 dadams
;;     Wrap count-screen-lines with with-current buffer.  Fixes bug: M in Dired pops up and
;;      selects frame listing marked files - and that frame's screen lines were being used.
;; 2012/03/20 dadams
;;     1on1-fit-minibuffer-frame: Use count-screen-lines only for Emacs 23+ - see comment.
;; 2012/03/17 dadams
;;     1on1-fit-minibuffer-frame:
;;       Use count-screen-lines to get the Icomplete overlay height, to fix part of bug #11035.
;; 2012/03/02 dadams
;;     Added 1on1-remove-if: to avoid runtime load of cl.el.
;;     1on1-emacs: Use 1on1-remove-if, not remove-if.
;; 2011/10/05 dadams
;;     1on1-display-*Completions*-frame:
;;       Use same font family, not same font, as orig buff.  Only for Emacs 23+.
;; 2011/10/02 dadams
;;     1on1-display-*Completions*-frame:
;;       If using Icicles, use same font as frame that set up minibuffer.
;; 2011/08/19 dadams
;;     1on1-fit-minibuffer-frame:
;;       Removed the scroll-down because it interfered with doing stuff at eob.
;; 2011/06/15 dadams
;;     Removed soft require of files+.el (switch-to-buffer-other-frame no longer used).
;; 2011/01/04 dadams
;;     Added autoload cookies for defgroup, defcustom, and commands.
;; 2010/11/30 dadams
;;     1on1-minibuffer-frame-alist: Change fallback value from nil to 0 (Emacs bug #1077).
;; 2010/10/27 dadams
;;     1on1-default-frame-alist:
;;       Put back setting of tool-bar-lines for Emacs 24.  Emacs 24 bug was fixed.
;; 2010/07/09 dadams
;;     1on1-emacs: Soft-require fit-frame.el.
;; 2010/07/04 dadams
;;     1on1-default-frame-alist:
;;       Temp workaround: do not set tool-bar-lines for Emacs 24 (Emacs bug).
;; 2009/06/19 dadams
;;     1on1-completions-frame-zoom-font-difference: Supplied missing :type and :group.
;; 2009/05/15 dadams
;;     Added: 1on1-fit-minibuffer-frame-(flag|max-height(-percent)).
;;     1on1-emacs: Add/remove 1on1-fit-minibuffer-frame for post-command-hook (if *-flag).
;;     1on1-set-minibuffer-frame-top/bottom: Redisplay.
;;     1on1-fit-minibuffer-frame:
;;       Do nothing unless 1on1-fit-minibuffer-frame-flag.
;;       Test last-command vs 1on1-fit-minibuffer-frame, not vs this-command.
;;       Pass current frame width to fit-frame.
;;       Bind to the 1on1 minibuffer values for the call to fit-frame:
;;         fit-frame-max-height(-percent),(fit-frame|window)-min-height,fit-frame-empty-*.
;;       Don't bind: frame-width, fit-frame-(min|max)-width, window-min-width.
;;       Don't provide any extra height for Emacs 22+.
;; 2009/05/03 dadams
;;     Corrected commentary: some customizable vars were listed as non-customizable.
;; 2009/04/18 dadams
;;     1on1-emacs: Raise error if run without a graphics display.
;; 2009/04/10 dadams
;;     1on1-emacs: No menu bar or tool bar for *Completions* frame.
;; 2009/02/11 dadams
;;     1on1-display-*(Help|Completions)*-frame:
;;       Protect x-pointer-shape with boundp (Emacs 23 bug #2296 workaround).
;; 2009/01/13 dadams
;;     Added: 1on1-completions-frame-zoom-font-difference.
;;            Use in 1on1-display-*Completions*-frame.
;; 2009/01/01 dadams
;;     1on1-emacs: Removed assignment to pop-up-frame-alist.
;; 2008/09/08 dadams
;;     y-or-n-p: 1on1-color-minibuffer-frame-on-setup only if at top-level.
;; 2007/12/05 dadams
;;     1on1-minibuffer-frame-left: Added :type.
;;     1on1-color-mode-line-flag, 1on1-minibuffer-frame-flag: defvar -> defcustom.
;;     1on1-(minibuffer|(special-)default)-frame-*: Removed * doc-string prefix.
;; 2007/11/22 dadams
;;     Added: 1on1-reset-minibuffer-frame, 1on1-fit-minibuffer-frame,
;;            1on1-minibuffer-prompt-end.  Recommend C-o key binding.
;;     Use 1on1-reset-minibuffer-frame on minibuffer-exit-hook.
;; 2007/08/14 dadams
;;     1on1-emacs:
;;       Add *Completions* to special-display-buffer-names even if
;;         1on1-*Completions*-frame-flag is nil, so minibuffer gets focus.
;;       Set w(in)32-grab-focus-on-raise to nil.
;;     1on1-display-*Completions*-frame:
;;       Don't change mouse pointer unless 1on1-*Completions*-frame-flag.
;;     1on1-minibuffer-frame-background: Use std minibuffer-frame-alist bg, if defined.
;; 2007/05/28 dadams
;;     1on1-display-*Completions*-frame:
;;       Wrap zoom-frm-out in condition-case (hack for Emacs 23 problem changing size).
;; 2007/03/10 dadams
;;     Added: 1on1-completions-frame-width.  Use it in 1on1-emacs.
;; 2007/02/08 dadams
;;     Removed: ^L-appearance-vector.
;;     1on1-emacs: No longer change ^L appearance - use my library pp-c-l.el to do that.
;; 2007/02/04 dadams
;;     1on1-emacs:
;;       Initialize standard-display-table if nil (default is nil!).  Thx to FidelSalas.
;; 2006/12/27 dadams
;;     1on1-change-cursor-on-input-method: Respect 1on1-change-cursor-on-input-method-flag
;; 2006/12/12 dadams
;;     Added: 1on1-^L-appearance-vector.
;;     1on1-emacs: Use 1on1-^L-appearance-vector to set ^L appearance.
;; 2006/12/11 dadams
;;     1on1-set-minibuffer-frame-top/bottom: 2 chars up, not 1, to fit Emacs 22 better.
;; 2006/10/28 dadams
;;     1on1-(in)active-minibuffer-frame-background,
;;     1on1-isearch-minibuffer-frame-background, 1on1-(in)active-mode-line-background,
;;     1on1-(help|completions)-frame-background,
;;     1on1-(help|completions)-frame-mouse+cursor-color,
;;     1on1-default-frame-cursor-color(-input-method):
;;         Changed :type to 'color for Emacs 21+.
;; 2006/09/14 dadams
;;     Removed mode-line position enhancements - use new library modeline-posn.el.
;;       Removed: 1on1-color-mode-line-column-flag, 1on1-mode-line-column-limit.
;; 2006/09/04 dadams
;;     1on1-box-cursor-when-idle-timer: Cancel beforehand, and cancel after defining.
;;     1on1-toggle-box-cursor-when-idle:
;;       Use 1on1-box-cursor-when-idle-off on pre-command-hook.
;;       Don't read an event; just turn it on.
;;     Added: 1on1-box-cursor-when-idle-off.
;; 2006/09/02 dadams
;;      1on1-toggle-box-cursor-when-idle: Corrected.
;; 2006/08/27 dadams
;;      Added: 1on1-box-cursor-when-idle(-p|-interval|-timer), 1on1-last-cursor-type,
;;             (1on1-)toggle-box-cursor-when-idle, 1on1-set-box-cursor-when-idle-interval.
;; 2006/08/13 dadams
;;      defalias set-cursor-type to 1on1-set-cursor-type.
;; 2006/07/25 dadams
;;      Added: 1on1-minibuffer-frame-left.  Use in 1on1-minibuffer-frame-alist.
;; 2006/03/31 dadams
;;      1on1-default-frame-alist:
;;        Changed (left|right)-fringe code, to reflect Emacs 22 change.
;; 2006/03/17 dadams
;;      Renamed:
;;        1on1-color-active-minibuffer-frame to 1on1-color-minibuffer-frame-on-setup,
;;        1on1-color-inactive-minibuffer-frame to 1on1-color-minibuffer-frame-on-exit.
;;      1on1-color-minibuffer-frame-on-setup:
;;        Redefined so hue depends on minibuffer-depth.
;; 2006/03/14 dadams
;;      1on1-color-(in)active-minibuffer-frame: Change hue for each minibuffer recursion.
;;      Added: 1on1-increment-color-hue.
;;      Require hexrgb.el
;; 2006/03/13 dadams
;;      1on1-color-inactive-minibuffer-frame:
;;        Change color only when not in recursive minibuffer.
;;      abort-recursive-edit: Change minibuffer color after, not before, abort.
;; 2006/01/07 dadams
;;      Added :link
;; 2005/12/14 dadams
;;     Added: 1on1-*Completions*-frame-at-right-flag.
;;            Use in 1on1-display-*Completions*-frame.
;; 2005/11/28 dadams
;;     Added: 1on1-change-cursor-on-overwrite-flag,
;;            1on1-change-cursor-on-input-method-flag, 1on1-default-frame-cursor-type,
;;            1on1-default-frame-cursor-type-overwrite, 1on1-default-frame-cursor-color,
;;            1on1-default-frame-cursor-color-input-mode, 1on1-change-cursor-on-overwrite,
;;            1on1-change-cursor-on-insert-mode, 1on1-set-cursor-type (thanks to
;;            Juri Linkov for the last three).
;;     1on1-emacs: Use 1on1-change-cursor-* in post-command-hook.
;;     1on1-mode-line-column-limit: Corrected custom group.
;; 2005/11/22 dadams
;;     Added: 1on1-setup-mode-line, 1on1-mode-line-column-limit,
;;            1on1-color-mode-line(-column)-flag, 1on1-(in)active-mode-line-background.
;; 2005/10/28 dadams
;;     1on1-display-*Completions*-frame: Zoom to smaller font.
;; 2005/07/31 dadams
;;     1on1-emacs: Do not set initial-frame-alist to default-frame-alist (D. Reitter).
;; 2005/07/25 dadams
;;     Added :prefix to defgroup.
;; 2005/07/17 dadams
;;     Switched default colors for 1on1-(in)active-minibuffer-frame-background,
;;       so active is the brighter color.  Change inactive to LightBlue.
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(eval-when-compile (when (< emacs-major-version 21) (require 'cl))) ;; dolist, push

(require 'frame-cmds nil t) ;; (no error if not found): rename-frame
(require 'zoom-frm nil t) ;; (no error if not found):
                          ;; frame-zoom-font-difference, zoom-frm-out
(require 'hexrgb) ;; hexrgb-increment-hue


;; Ensure that this is loaded before compiling it.
(provide 'oneonone)
(require 'oneonone)


;; Quiet the byte compiler.
(defvar frameset-filter-alist)          ; In `frameset.el', Emacs 24.4+
(defvar x-pointer-box-spiral)
(defvar x-pointer-xterm)

;;;;;;;;;;;;;;;;;;;;;;;;
 
;;;###autoload
(defgroup One-On-One nil
  "Options to define initial frame configuration."
  :prefix "1on1-" :group 'frames
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=oneonone.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/DrewsElispLibraries")
  :link '(url-link :tag "Download" "http://www.emacswiki.org/oneonone.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/OneOnOneEmacs")
  :link '(emacs-commentary-link :tag "Commentary" "oneonone"))

(defvar 1on1-divider-width 2
  "Default `bottom-divider-width' and `right-divider-width'.
This is used only to define the standard value of
`1on1-default-frame-alist' and `1on1-special-display-frame-alist'.
Customize those options, not this variable.  If you change this
variable, you will need to restart Emacs for it to take effect.")
 
;;; Minibuffer frame: ********************************
;;;
(defvar 1on1-minibuffer-frame nil
  "Minibuffer-only frame used by One-on-One Emacs.
Note: This is not used if `1on1-minibuffer-frame-flag' is nil.")

;;;###autoload
(defcustom 1on1-minibuffer-frame-flag t
  "*Non-nil means use a separate, specialized frame for the minibuffer.
Note that a non-nil value for this option also causes option
`pop-up-frames' to be set to `t'.  That is, it causes `display-buffer'
to generally use a separate frame.

If you change this variable, you will need to restart Emacs for it to
take effect."
  :type 'boolean :group 'One-On-One)

(unless (fboundp 'display-monitor-attributes-list) ; Emacs < 24.4.
  (defcustom 1on1-task-bar-height 28
    "Height of area at screen bottom that is not available for Emacs.
Space reserved for the MS Windows task bar, for example.
Not used for Emacs release 24.4 or later. "
    :type 'integer :group 'One-On-One))

(defvar 1on1-minibuffer-frame-bottom-offset nil
  "Offset for bottom of minibuffer frame, from monitor bottom.
The value is negative, and measured in pixels.")

;;;###autoload
(defcustom 1on1-remap-other-frame-command-flag (> emacs-major-version 23)
  "*Non-nil means rebind keys for `other-frame' to `1on1-other-frame'.
This has no effect unless `1on1-minibuffer-frame' is a frame, which
means that `1on1-minibuffer-frame-flag' is non-nil.
A non-nil value can be useful for Emacs starting with version 24,
because an inactive minibuffer has its own keymap."
  :type 'boolean :group 'One-On-One)

(defvar 1on1-minibuffer-frame-foreground "Red"
  "Default foreground color for the minibuffer frame.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

This is used only to define the standard value of
`1on1-minibuffer-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-minibuffer-frame-background
  (or (cdr (assq 'background-color minibuffer-frame-alist)) "LightBlue")
  "Initial color of the `1on1-minibuffer-frame' background.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

This is used only to define the standard value of
`1on1-minibuffer-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

;;;###autoload
(defcustom 1on1-active-minibuffer-frame-background "PaleGoldenrod"
  "*The color of the `1on1-minibuffer-frame' when it is active.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil."
  :type (if (>= emacs-major-version 21) 'color 'string) :group 'One-On-One)

;;;###autoload
(defcustom 1on1-inactive-minibuffer-frame-background "LightBlue"
  "*The color of the `1on1-minibuffer-frame' when it is inactive.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil."
  :type (if (>= emacs-major-version 21) 'color 'string) :group 'One-On-One)

;;;###autoload
(defcustom 1on1-isearch-minibuffer-frame-background "bisque"
  "*Color of the `1on1-minibuffer-frame' when `isearch' is active.
See `1on1-color-isearch-minibuffer-frame'.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil."
  :type (if (>= emacs-major-version 21) 'color 'string) :group 'One-On-One)

;;;###autoload
(defcustom 1on1-color-mode-line-flag t
  "*Non-nil means use `1on1-(in)active-mode-line-background'.
If you change this variable, you will need to restart Emacs for it to
take effect."
  :type 'boolean :group 'One-On-One)

;;;###autoload
(defcustom 1on1-color-minibuffer-frame-on-exit-increment 0.10
  "*Increment to change minibuffer-frame hue when minibuffer is exited.
This should be opposite in sign to
`1on1-color-minibuffer-frame-on-setup-increment.'"
  :type 'number :group 'One-On-One)

;;;###autoload
(defcustom 1on1-color-minibuffer-frame-on-setup-increment -0.10
  "*Increment to change minibuffer-frame hue when minibuffer is entered.
This should be opposite in sign to
`1on1-color-minibuffer-frame-on-exit-increment.'"
  :type 'number :group 'One-On-One)

;;;###autoload
(defcustom 1on1-active-mode-line-background "PaleGoldenrod"
  "*The color of the mode-line when it is active.
Note: This is not used if `1on1-color-mode-line-flag' is nil."
  :type (if (>= emacs-major-version 21) 'color 'string) :group 'One-On-One)

;;;###autoload
(defcustom 1on1-inactive-mode-line-background "LightGray"
  "*The color of the mode-line when it is inactive.
Note: This is not used if `1on1-color-mode-line-flag' is nil."
  :type (if (>= emacs-major-version 21) 'color 'string) :group 'One-On-One)

(defvar 1on1-minibuffer-frame-font
  (if (eq system-type 'windows-nt)
      ;;; "-*-Lucida Console-normal-r-*-*-14-112-96-96-c-*-iso8859-1"
      "-*-Lucida Console-normal-r-*-*-14-*-*-*-c-*-iso8859-1"
      ;;;;;;;"-*-Lucida Console-normal-r-*-*-15-*-*-*-c-*-*-ansi-"
    ;;; "-Misc-Fixed-Medium-R-Normal--15-140-75-75-C-90-ISO8859-1")
    "-Misc-Fixed-Medium-R-Normal--15-*-*-*-C-90-ISO8859-1")
  "Default font for the minibuffer frame.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

This is used only to define the standard value of
`1on1-minibuffer-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-minibuffer-frame-mouse-color "Black"
  "Default mouse color for the minibuffer frame.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

This is used only to define the standard value of
`1on1-minibuffer-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-minibuffer-frame-cursor-color "Black"
  "Default text cursor color for the minibuffer frame.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

This is used only to define the standard value of
`1on1-minibuffer-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-minibuffer-frame-height 2
  "Height of minibuffer frame, in characters.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

This is used only to define the standard value of
`1on1-minibuffer-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

;;;###autoload
(defcustom 1on1-minibuffer-frame-left 0
  "*Position of left edge of minibuffer frame, in pixels.
An integer.  If negative, then the position is that of the frame
bottom relative to the screen right (not left) edge.

See `default-frame-alist' for an explanation of frame parameters.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  :type 'integer :group 'One-On-One)

;;;###autoload
(defcustom 1on1-minibuffer-frame-top/bottom nil
  "*Position of top (or bottom) of minibuffer frame, in pixels.
If nil, function `1on1-set-minibuffer-frame-top/bottom' will position
minibuffer at bottom of display.

An integer.  If negative, then the position is that of the frame
bottom relative to the screen bottom.

See `default-frame-alist' for an explanation of frame parameters.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  :type '(choice (const :tag "Use function `1on1-set-minibuffer-frame-top/bottom'" nil)
                 (integer :tag "Pixels from top (>= 0) or bottom (< 0)" :value 0))
  :group 'One-On-One)

;;;###autoload
(defcustom 1on1-minibuffer-frame-width nil
  "*Width, in characters, for minibuffer frame.
If nil, then function `1on1-set-minibuffer-frame-width' is used instead.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  :type '(choice (const :tag "Use function `1on1-set-minibuffer-frame-width'" nil)
                 (integer :tag "Width, in characters, for minibuffer frame" :value 0))
  :group 'One-On-One)

;;;###autoload
(defcustom 1on1-minibuffer-frame-width-percent 100
  "*Max percent of the total display width to give to minibuffer frame.
See function `1on1-set-minibuffer-frame-width'.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  :type 'integer :group 'One-On-One)

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
       (cons 'menu-bar-lines 0))
   (or (assq 'left minibuffer-frame-alist)
       (cons 'left 1on1-minibuffer-frame-left))
   (or (assq 'height minibuffer-frame-alist)
       (cons 'height 1on1-minibuffer-frame-height))
   (or (assq 'icon-type minibuffer-frame-alist)
       (cons 'icon-type (< emacs-major-version 21))) ; `t' for Emacs 21 too?
   (or (assq 'minibuffer minibuffer-frame-alist)
       (cons 'minibuffer 'only))
   (or (assq 'user-position minibuffer-frame-alist)
       (cons 'user-position t))
   (or (assq 'horizontal-scroll-bars minibuffer-frame-alist)
       (cons 'horizontal-scroll-bars nil)) ; No horizontal scroll bars by default.
   (or (assq 'vertical-scroll-bars minibuffer-frame-alist) ;  No scroll bar.
       (cons 'vertical-scroll-bars nil))
   (or (assq 'name minibuffer-frame-alist)
       (cons 'name "Emacs Minibuffer"))
   (or (assq 'desktop-dont-save minibuffer-frame-alist)
       (cons 'desktop-dont-save t)))
  "*Frame-parameter alist for the standalone minibuffer frame
`1on1-minibuffer-frame'.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  ;; If we didn't need Emacs 20 compatibility, this could be:
  ;; :type '(alist :key-type symbol :value-type sexp)
  :type '(repeat (cons :format "%v" (symbol :tag "Frame Parameter") (sexp :tag "Value")))
  :group 'One-On-One)

;;;###autoload
(defcustom 1on1-fit-minibuffer-frame-flag t
  "*Non-nil means adjust `1on1-minibuffer-frame' height to fit content.
This is done after each command or user event (e.g. each key press)
when the minibuffer is active.
This option has no effect if `1on1-minibuffer-frame-flag' is nil."
  :type 'boolean :group 'One-On-One)

;;;###autoload
(defcustom 1on1-fit-minibuffer-frame-max-height nil
  "*Maximum height, in lines, that `fit-frame' gives to `1on1-minibuffer-frame'.
If nil, then function `fit-frame-max-height' is used instead,
respecting `1on1-fit-minibuffer-frame-max-height-percent'.
This has no effect if you do not use library `fit-frame.el'."
  :type '(choice
          (const :tag "Use `1on1-fit-minibuffer-frame-max-height-percent'" nil)
          integer)
  :group 'One-On-One)

;;;###autoload
(defcustom 1on1-fit-minibuffer-frame-max-height-percent 10
  "*Max percent that `fit-frame' gives to `1on1-minibuffer-frame'.
This is a percentage of the display height.
Not used unless `1on1-fit-minibuffer-frame-max-height' is nil.
This has no effect if you do not use library `fit-frame.el'."
  :type 'integer :group 'One-On-One)

(when (boundp 'frameset-filter-alist)  ; Emacs 24.4+

  (defun 1on1-filter-no-default-minibuffer (current  _filtered  _parameters  saving)
    "Do not replace existing minibuffer frame when restoring a frameset."
    (or saving  (not (equal current '(frameset--mini t . t)))  '(frameset--mini t . nil)))

  (push '(frameset--mini . 1on1-filter-no-default-minibuffer) frameset-filter-alist)

  ;; Also, keep frame names when restoring a frameset.
  (push '(name . nil) frameset-filter-alist))


 
;;; *Help* frame: ********************************
;;;   Display of *Help* buffer in custom frame.
;;;   Background, height, cursor and pointer colors.
;;;
;;;###autoload
(defcustom 1on1-*Help*-frame-flag t
  "*Non-nil means use a special appearance for the *Help* frame.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  :type 'boolean :group 'One-On-One)

;;;###autoload
(defcustom 1on1-help-frame-background "Thistle"
  "*Default background color for the *Help* buffer's frame.

Note: This is not used if `1on1-*Help*-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  :type (if (>= emacs-major-version 21) 'color 'string) :group 'One-On-One)

;;;###autoload
(defcustom 1on1-help-frame-mouse+cursor-color "Blue Violet"
  "*Default color for cursor & pointer of *Help* frame.

Note: This is not used if `1on1-*Help*-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  :type (if (>= emacs-major-version 21) 'color 'string) :group 'One-On-One)
 
;;; *Completions* frame: ********************************
;;;   Display of *Completion* buffer in custom frame.
;;;   Background, height, cursor and pointer colors.
;;;
;;;###autoload
(defcustom 1on1-*Completions*-frame-flag t
  "*Non-nil means use a special appearance for the *Completions* frame.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  :type 'boolean :group 'One-On-One)

;;;###autoload
(defcustom 1on1-*Completions*-frame-at-right-flag nil
  "*Non-nil means place *Completions* frame at right edge of display.
This can be useful to make *Completions* more visible.
This has no effect if `1on1-*Completions*-frame-flag' is nil."
  :type 'boolean :group 'One-On-One)

;;;###autoload
(defcustom 1on1-completions-frame-background "LavenderBlush2"
  "*Default background color for the *Completions* buffer's frame.

Note: This is not used if `1on1-*Completions*-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  :type (if (>= emacs-major-version 21) 'color 'string) :group 'One-On-One)

;;;###autoload
(defcustom 1on1-completions-frame-mouse+cursor-color "VioletRed"
  "*Default color for cursor & pointer of *Completions* frame.

Note: This is not used if `1on1-*Completions*-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  :type (if (>= emacs-major-version 21) 'color 'string) :group 'One-On-One)

;;;###autoload
(defcustom 1on1-completions-frame-width 100
  "*Width, in characters, for *Completions* frame.
If this is nil, then the pertinent default frame width is used.

Note: This is not used if `1on1-*Completions*-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  :type 'integer :group 'One-On-One)

;;;###autoload
(defcustom 1on1-completions-frame-zoom-font-difference
  (or (and (require 'zoom-frm 'nil t)  (* 2 frame-zoom-font-difference))
      2)
  "*Number of points to reduce the *Completions* frame font size.
This must be less than the current default font size, since the new
font size cannot be less than 1 point.
A value of zero or nil means the *Completions* frame is not zoomed."
  :type '(restricted-sexp :match-alternatives (integerp null)) :group 'One-On-One)
 
;;; Default for normal frames: `1on1-default-frame-alist' **************************
;;;
(defvar 1on1-default-frame-foreground "Black"
  "Default foreground color for non-special frames.
This is used only to define the standard value of
`1on1-default-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-default-frame-background "LightBlue"
  "Default background color for non-special frames.
This is used only to define the standard value of
`1on1-default-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-default-frame-font
  (if (eq system-type 'windows-nt)
      ;;; "-*-Lucida Console-normal-r-*-*-14-112-96-96-c-*-iso8859-1"
      "-*-Lucida Console-normal-r-*-*-14-*-*-*-c-*-iso8859-1"
      ;;;;;;"-*-Lucida Console-normal-r-*-*-15-*-*-*-c-*-*-ansi-"
    ;;; "-Misc-Fixed-Medium-R-Normal--15-140-75-75-C-90-ISO8859-1")
    "-Misc-Fixed-Medium-R-Normal--15-*-*-*-C-90-ISO8859-1")
  "Default font for non-special frames.
This is used only to define the standard value of
`1on1-default-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-default-frame-mouse-color "Red"
  "Default mouse-pointer color for non-special frames.
This is used only to define the standard value of
`1on1-default-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

;;;###autoload
(defcustom 1on1-change-cursor-on-input-method-flag t
  "*Non-nil means to use a different cursor when using an input method.
If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  :type 'boolean :group 'One-On-One)

;;;###autoload
(defcustom 1on1-default-frame-cursor-color "Red"
  "*Default text cursor color for non-special frames.
If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.  Furthermore, if
`1on1-change-cursor-on-input-method-flag' is nil when you rerun
`1on1-emacs', you will need to toggle that variable to non-nil (and
back to nil, if that's the value you want).  Otherwise, the new value
will take effect only after you restart Emacs."
  :type (if (>= emacs-major-version 21) 'color 'string) :group 'One-On-One)

;;;###autoload
(defcustom 1on1-default-frame-cursor-color-input-method "Orange"
  "*Default cursor color for non-special frames if using an input method.
This has no effect if `1on1-change-cursor-on-input-method-flag' is nil.
If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  :type (if (>= emacs-major-version 21) 'color 'string) :group 'One-On-One)

;;;###autoload
(defcustom 1on1-change-cursor-on-overwrite/read-only-flag t
  "*Non-nil means use a different cursor when overwrite mode or read-only.
If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  :type 'boolean :group 'One-On-One)

;;;###autoload
(defcustom 1on1-default-frame-cursor-type 'bar
  "*Default text cursor type for non-special frames.
If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.  Furthermore, if
`1on1-change-cursor-on-overwrite/read-only-flag' is nil when you rerun
`1on1-emacs', you will need to toggle that variable to non-nil (and
back to nil, if that's the value you want).  Otherwise, the new value
will take effect only after you restart Emacs."
  ;; Ensure that it cannot be set to `nil'.
  :type '(restricted-sexp :match-alternatives ((lambda (x) (and x  (symbolp x)))) :value bar)
  :group 'One-On-One)

(defvar 1on1-last-cursor-type 1on1-default-frame-cursor-type "Saved last cursor type.")

;;;###autoload
(defcustom 1on1-default-frame-cursor-type-overwrite/read-only 'box
  "*Default text cursor type for overwrite mode or read-only buffer.
This applies only to non-special frames.  This has no effect if
`1on1-change-cursor-on-overwrite/read-only-flag' is nil.  If you
customize this variable, you will need to rerun `1on1-emacs' for the
new value to take effect."
  ;; Ensure that it cannot be set to `nil'.
  :type '(restricted-sexp :match-alternatives ((lambda (x) (and x  (symbolp x)))) :value box)
  :group 'One-On-One)

(defvar 1on1-box-cursor-when-idle-p t
  "Non-nil means to use a box cursor whenever Emacs is idle.
Do NOT change this yourself; instead, use `\\[toggle-box-cursor-when-idle]'.")

(defvar 1on1-box-cursor-when-idle-interval 2
  "Number of seconds to wait before changing cursor type to box.
Do NOT change this yourself to change the wait period; instead, use
`\\[1on1-set-box-cursor-when-idle-interval]'.")

(defvar 1on1-box-cursor-when-idle-timer
  (progn                                ; Cancel to prevent duplication.
    (when (boundp '1on1-box-cursor-when-idle-timer)
      (cancel-timer 1on1-box-cursor-when-idle-timer))
    (run-with-idle-timer 1on1-box-cursor-when-idle-interval t '1on1-box-cursor-when-idle))
  "Timer used to change the cursor to a box cursor when Emacs is idle.")

;; Turn it off, by default.  You must use `toggle-box-cursor-when-idle' to turn it on.
(cancel-timer 1on1-box-cursor-when-idle-timer)

(defvar 1on1-default-frame-menu-bar-lines 1
  "Number of lines used for the menu bar in non-special frames.
This is used only to define the standard value of
`1on1-default-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-default-frame-upper-left-corner '(0 . 0)
  "Position of upper left frame corner.
A cons whose car is the distance from the top in pixels
and whose cdr is the distance from the left in pixels.

This is used only to define the standard value of
`1on1-default-frame-alist'.  Customize that variable, not this one.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(defvar 1on1-default-frame-size '(80 . 35)
  "Default frame size.

A cons whose car is the frame width in characters and whose cdr is the
frame height in characters.

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
   (or (assq 'cursor-type default-frame-alist)
       (cons 'cursor-type 1on1-default-frame-cursor-type))
   (or (assq 'menu-bar-lines default-frame-alist)
       (cons 'menu-bar-lines 1on1-default-frame-menu-bar-lines))
   (or (assq 'bottom-divider-width default-frame-alist)
       (cons 'bottom-divider-width 1on1-divider-width))
   (or (assq 'right-divider-width default-frame-alist)
       (cons 'right-divider-width 1on1-divider-width))
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
   (or (assq 'horizontal-scroll-bars default-frame-alist)
       (cons 'horizontal-scroll-bars nil)) ; No horizontal scroll bars by default.
   (or (assq 'vertical-scroll-bars default-frame-alist)
       (cons 'vertical-scroll-bars 'right))
   (or (assq 'icon-type default-frame-alist)
       (cons 'icon-type (< emacs-major-version 21))) ; `t' for Emacs 21 too?
   (or (assq 'tool-bar-lines default-frame-alist)
       (cons 'tool-bar-lines 1))        ; Emacs 21+
   (if (cdr (assq 'left-fringe default-frame-alist))
       (assq 'left-fringe default-frame-alist)
     (cons 'left-fringe 0))             ; Emacs 21+
   (if (cdr (assq 'right-fringe default-frame-alist))
       (assq 'right-fringe default-frame-alist)
     (cons 'right-fringe 0))            ; Emacs 21+
   (or (assq 'fringe default-frame-alist)
       (cons 'fringe 0)))               ; Emacs 21, but not 21.3.50 - REMOVE after 22.x
  "*Properties to be used for One-on-One Emacs `default-frame-alist'.
If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  ;; If we didn't need Emacs 20 compatibility, this could be:
  ;; :type '(alist :key-type symbol :value-type sexp)
  :type '(repeat (cons :format "%v" (symbol :tag "Frame Parameter") (sexp :tag "Value")))
  :group 'One-On-One)
 
;;; Special-display frames: `1on1-special-display-frame-alist' ************************
;;;
(defvar 1on1-special-frame-foreground "Black"
  "Default foreground color for special display frames.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

(defvar 1on1-special-frame-background "LightSteelBlue"
  "Default background color for special display frames.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

(defvar 1on1-special-frame-font
  (if (eq system-type 'windows-nt)
      ;;; "-*-Lucida Console-normal-r-*-*-14-112-96-96-c-*-iso8859-1"
      "-*-Lucida Console-normal-r-*-*-14-*-*-*-c-*-iso8859-1"
      ;;;;;;;;"-*-Lucida Console-normal-r-*-*-15-*-*-*-c-*-*-ansi-"
    ;;; "-Misc-Fixed-Medium-R-Normal--15-140-75-75-C-90-ISO8859-1")
    "-Misc-Fixed-Medium-R-Normal--15-*-*-*-C-90-ISO8859-1")
  "Default font for special display frames.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

(defvar 1on1-special-frame-mouse-color "Yellow"
  "Default mouse color for special display frames.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

(defvar 1on1-special-frame-cursor-color "Yellow"
  "Default text cursor color for special display frames.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

(defvar 1on1-special-frame-menu-bar-lines 1
  "Number of lines used for the menu bar of special display frames.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

(defvar 1on1-special-frame-upper-left-corner '(0 . 0)
  "Position of upper left corner of special display frames.
A cons whose car is the distance from the top in pixels
and whose cdr is the distance from the left in pixels.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

(defvar 1on1-special-frame-size '(80 . 20)
  "Default size of special display frames.
A cons whose car is the frame width in characters and whose cdr is the
frame height in characters.

This is used only to define the standard value of
`1on1-special-display-frame-alist'.  Customize that variable, not this
one.  If you change this variable, you will need to restart Emacs for
it to take effect.")

;; Use values from the standard list, when available.  However, we have no way of
;; distinguishing values predefined in vanilla Emacs from user settings.
(defcustom 1on1-special-display-frame-alist
  (list
   (or (assq 'font special-display-frame-alist)
       (cons 'font 1on1-special-frame-font))
   (or (assq 'width special-display-frame-alist)
       (cons 'width (car 1on1-special-frame-size)))
   (or (assq 'height special-display-frame-alist)
       (cons 'height (cdr 1on1-special-frame-size)))
   (or (assq 'mouse-color special-display-frame-alist)
       (cons 'mouse-color 1on1-special-frame-mouse-color))
   (or (assq 'cursor-color special-display-frame-alist)
       (cons 'cursor-color 1on1-special-frame-cursor-color))
   (or (assq 'menu-bar-lines special-display-frame-alist)
       (cons 'menu-bar-lines 1on1-special-frame-menu-bar-lines))
   (or (assq 'bottom-divider-width special-display-frame-alist)
       (cons 'bottom-divider-width 1on1-divider-width))
   (or (assq 'right-divider-width special-display-frame-alist)
       (cons 'right-divider-width 1on1-divider-width))
   (or (assq 'foreground-color special-display-frame-alist)
       (cons 'foreground-color 1on1-special-frame-foreground))
   (or (assq 'background-color special-display-frame-alist)
       (cons 'background-color 1on1-special-frame-background))
   (or (assq 'top special-display-frame-alist)
       (cons 'top (car 1on1-special-frame-upper-left-corner)))
   (or (assq 'left special-display-frame-alist)
       (cons 'left (cdr 1on1-special-frame-upper-left-corner)))
   (or (assq 'unsplittable special-display-frame-alist)
       (cons 'unsplittable t))
   (or (assq 'user-position special-display-frame-alist)
       (cons 'user-position t))
   (or (assq 'horizontal-scroll-bars special-display-frame-alist)
       (cons 'horizontal-scroll-bars nil)) ; No horizontal scroll bars by default.
   (or (assq 'vertical-scroll-bars special-display-frame-alist)
       (cons 'vertical-scroll-bars 'right)))
  "Properties to be used for One-on-One `special-display-frame-alist'.
If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect."
  ;; If we didn't need Emacs 20 compatibility, this could be:
  ;; :type '(alist :key-type symbol :value-type sexp)
  :type '(repeat (cons :format "%v" (symbol :tag "Frame Parameter") (sexp :tag "Value")))
  :group 'One-On-One)
 
;;; Main command ***************************************
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
  (unless (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
    (error "Use `1on1-emacs' only with a graphics display, not with a text terminal"))
  (setq default-frame-alist          (append 1on1-default-frame-alist default-frame-alist)
        special-display-frame-alist  (append 1on1-special-display-frame-alist
                                             special-display-frame-alist))

  ;; *Help* frame
  (if 1on1-*Help*-frame-flag
      (add-to-list
       'special-display-buffer-names
       (list "*Help*" '1on1-display-*Help*-frame
             (list (cons 'background-color 1on1-help-frame-background)
                   (cons 'mouse-color 1on1-help-frame-mouse+cursor-color)
                   (cons 'cursor-color 1on1-help-frame-mouse+cursor-color)
                   '(height . 40))))
    (setq special-display-buffer-names  (1on1-remove-if (lambda (elt)
                                                          (equal "*Help*" (car elt)))
                                                        special-display-buffer-names)))

  ;; *Completions* frame
  ;; If `1on1-minibuffer-frame-flag' is non-nil, then *Completions* frame must be treated
  ;; specially, so that it gets focus from the minibuffer frame.  This is so, even if
  ;; `1on1-*Completions*-frame-flag' is nil.
  (if  1on1-minibuffer-frame-flag
       (if 1on1-*Completions*-frame-flag
           (add-to-list
            'special-display-buffer-names
            `("*Completions*" 1on1-display-*Completions*-frame
              ((background-color ,@1on1-completions-frame-background)
               (mouse-color      ,@1on1-completions-frame-mouse+cursor-color)
               (cursor-color     ,@1on1-completions-frame-mouse+cursor-color)
               (menu-bar-lines . 0) (tool-bar-lines . 0) ; No menu bar or tool bar.
               ,@(and 1on1-completions-frame-width
                      `((width   ,@1on1-completions-frame-width))))))
         (add-to-list 'special-display-buffer-names
                      `("*Completions*" 1on1-display-*Completions*-frame)))
    (setq special-display-buffer-names  (1on1-remove-if (lambda (elt)
                                                          (equal "*Completions*" (car elt)))
                                                        special-display-buffer-names)))

  ;; Minibuffer frame
  (when 1on1-minibuffer-frame-flag
    ;; `display-buffer' (& `*-other-window' fns) will use separate frames.
    (setq pop-up-frames  t)

    ;; Set up `1on1-minibuffer-frame'.
    (setq minibuffer-frame-alist  (append 1on1-minibuffer-frame-alist minibuffer-frame-alist))
    (if 1on1-minibuffer-frame
        (modify-frame-parameters 1on1-minibuffer-frame 1on1-minibuffer-frame-alist)
      (setq 1on1-minibuffer-frame
            (let ((after-make-frame-functions  ())) ; E.g. inhibit `fit-frame'.
              (make-frame 1on1-minibuffer-frame-alist))))

    ;; Set `1on1-minibuffer-frame-bottom-offset' (Emacs 24.4+ only).
    (when (and (not 1on1-minibuffer-frame-bottom-offset)
               (fboundp 'display-monitor-attributes-list))
      (catch '1on1-emacs
        (dolist (attr  (display-monitor-attributes-list))
          (when (memq 1on1-minibuffer-frame (cdr (assoc 'frames attr)))
            (setq 1on1-minibuffer-frame-bottom-offset
                  (- (nth 4 (assoc 'workarea attr))
                     (nth 4 (assoc 'geometry attr))
                     (or (- (frame-parameter 1on1-minibuffer-frame 'border-width))  0)))
            (throw '1on1-emacs nil)))
        ;; Fallback - should not happen.  No monitor has minibuffer frame.
        (setq 1on1-minibuffer-frame-bottom-offset
              (- (nth 4 (assoc 'workarea (car (display-monitor-attributes-list))))
                 (nth 4 (assoc 'geometry (car (display-monitor-attributes-list))))
                 (or (- (frame-parameter 1on1-minibuffer-frame 'border-width))  0)))))

    ;; Resize and reposition frame.  If variable `1on1-minibuffer-frame-width'
    ;; or `1on1-minibuffer-frame-top/bottom' is nil, calculate automatically.
    (1on1-set-minibuffer-frame-width)
    (1on1-set-minibuffer-frame-top/bottom)

    ;; Rename minibuffer frame. (`rename-frame' is defined in `frame-cmds.el'.)
    (when (fboundp 'rename-frame)
      (rename-frame 1on1-minibuffer-frame "Emacs minibuffer                         \
show/hide: hold CTRL + click in window"))
    ;; (setq minibuffer-auto-raise  t) ; $$$$$$$ Let user decide, based on window mgr behavior.
    ;; Background colors of minibuffer frame: 3 states
    (add-hook 'isearch-mode-hook '1on1-color-isearch-minibuffer-frame)
    (add-hook 'isearch-mode-end-hook '1on1-color-minibuffer-frame-on-exit)
    (add-hook 'minibuffer-setup-hook '1on1-color-minibuffer-frame-on-setup)
    (add-hook 'minibuffer-exit-hook '1on1-color-minibuffer-frame-on-exit)
    ;; Redefine built-in fns so they color minibuffer frame.
    (1on1-setup-minibuffer-frame-coloring)
    (when 1on1-remap-other-frame-command-flag
      (substitute-key-definition 'other-frame '1on1-other-frame global-map)))

  ;; Hooks.
  (if (and 1on1-fit-minibuffer-frame-flag (require 'fit-frame nil t))
      (add-hook 'post-command-hook '1on1-fit-minibuffer-frame)
    (remove-hook 'post-command-hook '1on1-fit-minibuffer-frame))
  (if 1on1-change-cursor-on-overwrite/read-only-flag
      (add-hook 'post-command-hook '1on1-change-cursor-on-overwrite/read-only)
    (let ((deflt-ctype  (cdr (assq 'cursor-type default-frame-alist))))
      (when deflt-ctype (1on1-set-cursor-type deflt-ctype)))
    (remove-hook 'post-command-hook '1on1-change-cursor-on-overwrite/read-only))
  (if 1on1-change-cursor-on-input-method-flag
      (add-hook 'post-command-hook '1on1-change-cursor-on-input-method)
    (setq current-input-method  nil)
    (1on1-change-cursor-on-input-method)
    (remove-hook 'post-command-hook '1on1-change-cursor-on-input-method))
  (add-hook 'minibuffer-exit-hook '1on1-reset-minibuffer-frame)

  (setq w32-grab-focus-on-raise    nil
        win32-grab-focus-on-raise  nil) ; older name
  (1on1-setup-mode-line))

;; Define this to avoid requiring `cl.el' at runtime.
;; Same as in `icicle-remove-if' in `icicles-fn.el'.
(defun 1on1-remove-if (pred xs)
  "A copy of list XS with no elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x  xs) (unless (funcall pred x) (push x result)))
    (nreverse result)))

;; This is inspired by code from Juri Linkov <juri@jurta.org>.
(defun 1on1-change-cursor-on-input-method ()
  "Set cursor color, depending on whether an input method is used or not."
  (when 1on1-change-cursor-on-input-method-flag
    (if current-input-method
        (set-cursor-color 1on1-default-frame-cursor-color-input-method)
      (let ((bufname  (buffer-name (current-buffer))))
        (cond ((string= "*Help*" bufname)
               (set-cursor-color 1on1-help-frame-mouse+cursor-color))
              ((string= "*Completions*" bufname)
               (set-cursor-color 1on1-completions-frame-mouse+cursor-color))
              ((eq 1on1-minibuffer-frame (selected-frame))
               (set-cursor-color 1on1-minibuffer-frame-cursor-color))
              ((special-display-p bufname)
               (set-cursor-color 1on1-special-frame-cursor-color))
              ;; Do not set it if there is no setting for it in `default-frame-alist'.
              ((cdr (assq 'cursor-color default-frame-alist))
               (set-cursor-color (cdr (assq 'cursor-color default-frame-alist)))))))))

;; This is from Juri Linkov <juri@jurta.org>, with read-only added.
(defun 1on1-change-cursor-on-overwrite/read-only ()
  "Set cursor type differently for overwrite mode and read-only buffer.
That is, use one cursor type for overwrite mode and read-only buffers,
and another cursor type otherwise."
  (let ((deflt-ctype  (cdr (assq 'cursor-type default-frame-alist))))
    (when (or buffer-read-only  overwrite-mode  deflt-ctype)
      (1on1-set-cursor-type (if (or buffer-read-only  overwrite-mode)
                                1on1-default-frame-cursor-type-overwrite/read-only
                              deflt-ctype)))))

(unless (fboundp 'set-cursor-type) (defalias 'set-cursor-type '1on1-set-cursor-type))
;; This is essentially from Juri Linkov <juri@jurta.org>.
;;;###autoload
(defun 1on1-set-cursor-type (cursor-type)
  "Set the cursor type of the selected frame to CURSOR-TYPE.
When called interactively, prompt for the type to use.
To get the frame's current cursor type, use `frame-parameters'."
  (interactive
   (list (intern (completing-read "Cursor type: "
                                  (mapcar 'list '("box" "hollow" "bar" "hbar" nil))))))
  (modify-frame-parameters (selected-frame) (list (cons 'cursor-type cursor-type))))

(defun 1on1-box-cursor-when-idle ()
  "Change the cursor to a box cursor when Emacs is idle."
  (let ((type  (cdr (assoc 'cursor-type (frame-parameters)))))
    (unless (eq type 'box)
      (setq 1on1-last-cursor-type  type)
      (1on1-set-cursor-type 'box))))

(defun 1on1-box-cursor-when-idle-off ()
  "Turn off changing the cursor to a box cursor when Emacs is idle."
  (when 1on1-last-cursor-type (1on1-set-cursor-type 1on1-last-cursor-type)))

;;;###autoload
(defalias 'toggle-box-cursor-when-idle '1on1-toggle-box-cursor-when-idle)
;;;###autoload
(defun 1on1-toggle-box-cursor-when-idle (&optional arg)
  "Turn on or off automatically changing to a box cursor when idle.
When on, the cursor is changed to a box whenever Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off."
  (interactive "P")
  (setq 1on1-box-cursor-when-idle-p  (if arg
                                         (> (prefix-numeric-value arg) 0)
                                       (not 1on1-box-cursor-when-idle-p)))
  (cond (1on1-box-cursor-when-idle-p
         (timer-activate-when-idle 1on1-box-cursor-when-idle-timer)
         (add-hook 'pre-command-hook '1on1-box-cursor-when-idle-off)
         (message "Turned ON making cursor a box when Emacs is idle."))
        (t
         (cancel-timer 1on1-box-cursor-when-idle-timer)
         (remove-hook 'pre-command-hook '1on1-box-cursor-when-idle-off)
         (message "Turned OFF making cursor a box when Emacs is idle."))))

;;;###autoload
(defun 1on1-set-box-cursor-when-idle-interval (secs)
  "Set wait until automatically change to a box cursor when Emacs is idle.
Whenever Emacs is idle for this many seconds it will change the cursor
to a box.

To turn on or off automatically changing to a box cursor when idle,
use `\\[toggle-box-cursor-when-idle]."
  (interactive
   "nSeconds to idle, before changing to a box cursor: ")
  (timer-set-idle-time 1on1-box-cursor-when-idle-timer
                       (setq 1on1-box-cursor-when-idle-interval  secs)
                       t))

;;;###autoload
(defun 1on1-other-frame (arg)
  "Same as `other-frame', except include frame `1on1-minibuffer-frame'.
If `1on1-minibuffer-frame' is non-nil then it is a standalone
minibuffer frame.  In this case, include it as well as all other
visible or iconified frames as candidates.

Select the ARGth different visible frame on current display, and raise it.
Select the frame ARG steps away in the sequence of frames.
A negative ARG moves in the opposite direction.

To make this command work properly, you must tell Emacs
how the system (or the window manager) generally handles
focus-switching between windows.  If moving the mouse onto a window
selects it (gives it focus), set `focus-follows-mouse' to t.
Otherwise, that variable should be nil."
  (interactive "p")
  (let ((frame  (selected-frame)))
    (while (> arg 0)
      (setq frame  (next-frame frame 0))
      (while (not (eq t (frame-visible-p frame)))
	(setq frame  (next-frame frame 0)))
      (setq arg  (1- arg)))
    (while (< arg 0)
      (setq frame  (previous-frame frame 0))
      (while (not (eq t (frame-visible-p frame)))
	(setq frame  (previous-frame frame 0)))
      (setq arg  (1+ arg)))
    (select-frame-set-input-focus frame)))

(defun 1on1-display-*Help*-frame (buf &optional args)
  "Display *Help* buffer in its own frame.
`special-display-function' is used to do the actual displaying.
BUF and ARGS are the arguments to `special-display-function'."
  (let ((old-ptr-shape  (and (boundp 'x-pointer-shape) x-pointer-shape))
        return-window)
    (when (boundp 'x-pointer-xterm) (setq x-pointer-shape  x-pointer-xterm))
    (setq return-window  (select-window (funcall special-display-function buf args)))
    (raise-frame)
    (setq x-pointer-shape  old-ptr-shape)
    return-window))

(defun 1on1-display-*Completions*-frame (buf &optional args)
  "Display *Completions* buffer in its own frame.
`special-display-function' is used to do the actual displaying.
Completion input events are redirected to `1on1-minibuffer-frame'.
BUF and ARGS are the arguments to `special-display-function'.

If Icicles is used, then give `*Completions*' frame the same font as
the frame that set up the minibuffer.

If `zoom-frm.el' is used, then shrink the text according to
`1on1-completions-frame-zoom-font-difference'."
  (let ((old-ptr-shape  (and (boundp 'x-pointer-shape) x-pointer-shape))
        return-window)
    (when (and 1on1-*Completions*-frame-flag (boundp 'x-pointer-box-spiral))
      (setq x-pointer-shape  x-pointer-box-spiral))
    (setq return-window  (select-window (funcall special-display-function buf args)))

    ;; In Icicles, use the font family of the original window.  This is particularly for
    ;; picking up the proper font for Unicode chars in `*Completions*'.  Emacs 23+ only.
    (when (and (boundp 'icicle-mode) icicle-mode
               icicle-pre-minibuffer-buffer
               (> emacs-major-version 22))
      ;; Prior to Emacs 24, dunno how to get last-used window showing
      ;; `icicle-pre-minibuffer-buffer'.  This only gets some window showing it - not TRT.
      ;; (This code is similar to what is in `icicle-display-candidates-in-Completions'.)
      (let* ((orig-win       (if (not (fboundp 'icicle-mru-window-for-buffer))
                                 (get-buffer-window icicle-pre-minibuffer-buffer 'visible)
                               (icicle-mru-window-for-buffer icicle-pre-minibuffer-buffer
                                                             'NOMINI 0)))
             (orig-font-fam  (and (window-live-p orig-win)
                                  (save-window-excursion (select-window orig-win)
                                                         (face-attribute 'default :family)))))
        ;; $$$$$$$$?? (when (and orig-font-fam  (not (eq orig-win return-window)))
        ;;
        ;; We do not save the cookie from this, as we do not have a function that removes it.
        (when orig-font-fam (face-remap-add-relative 'default :family orig-font-fam))))

    ;; Zoom text by `1on1-completions-frame-zoom-font-difference'.
    (when (and (fboundp 'zoom-frm-out) 1on1-completions-frame-zoom-font-difference)
      (condition-case nil
          (let ((frame-zoom-font-difference  1on1-completions-frame-zoom-font-difference))
            (zoom-frm-out))             ; In `zoom-frm.el'.
        (error nil)))

    ;; We reposition frame this way, instead of binding `special-display-frame-alist'
    ;; with this value, because `after-make-frame-functions' might resize frame.
    (when 1on1-*Completions*-frame-at-right-flag
      (modify-frame-parameters
       (selected-frame)                 ; Hard-code 7 here - what does it depend on?
       `((left . ,(- (x-display-pixel-width) (+ (frame-pixel-width) 7))))))
    (raise-frame)
    (let ((redirect  (if (active-minibuffer-window)
                         1on1-minibuffer-frame
                       (and completion-reference-buffer
                            (get-buffer-window completion-reference-buffer 'visible)
                            (not (eq (get-buffer "*Completions*") completion-reference-buffer))
                            (window-frame (get-buffer-window completion-reference-buffer t))))))
      (when redirect
        (redirect-frame-focus (selected-frame) redirect)))
    ;; $$$$$$$ (when 1on1-minibuffer-frame (redirect-frame-focus (selected-frame)
    ;;                                                           1on1-minibuffer-frame))
    (when (and 1on1-*Completions*-frame-flag (boundp 'x-pointer-box-spiral))
      (setq x-pointer-shape  old-ptr-shape))
    return-window))

(defun 1on1-color-minibuffer-frame-on-setup ()
  "Change background of minibuffer frame to reflect the minibuffer depth.
Use this when increasing the minibuffer recursion depth."
  (when 1on1-minibuffer-frame
    (save-window-excursion
      (select-frame 1on1-minibuffer-frame)
      (set-background-color 1on1-active-minibuffer-frame-background)
      (let ((count  (minibuffer-depth)))
        (while (> count 1)
          (set-background-color (hexrgb-increment-hue ; Change bg hue slightly.
                                 (frame-parameter nil 'background-color)
                                 1on1-color-minibuffer-frame-on-setup-increment))
          (setq count  (1- count)))))))

(defun 1on1-color-minibuffer-frame-on-exit ()
  "Change background of minibuffer frame to reflect the minibuffer depth.
Use this when reducing the minibuffer recursion depth."
  (when 1on1-minibuffer-frame
    (save-window-excursion
      (select-frame 1on1-minibuffer-frame)
      (cond ((= (minibuffer-depth) 2)
             (set-background-color 1on1-active-minibuffer-frame-background))
            ((< (minibuffer-depth) 2)
             (set-background-color 1on1-inactive-minibuffer-frame-background))
            (t
             (set-background-color (hexrgb-increment-hue ; Change bg hue slightly.
                                    (frame-parameter nil 'background-color)
                                    1on1-color-minibuffer-frame-on-exit-increment)))))))

(defun 1on1-color-isearch-minibuffer-frame ()
  "Use `1on1-isearch-minibuffer-frame-background' for minibuffer."
  (and 1on1-minibuffer-frame
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
  (flash-ding do-not-terminate 1on1-minibuffer-frame))

(defun 1on1-setup-minibuffer-frame-coloring ()
  "Redefine some built-in functions so they color the minibuffer frame.
Functions redefined: `y-or-n-p', `top-level'."


  (or (fboundp '1on1-ORIG-y-or-n-p)
      (fset '1on1-ORIG-y-or-n-p (symbol-function 'y-or-n-p)))

  ;; REPLACES ORIGINAL (built-in function):
  ;; Temporarily colors minibuffer frame to "active" color if at top-level.
  ;;
  (defun y-or-n-p (prompt)
    "Ask user a \"y or n\" question.  Return t if answer is \"y\".
Takes one argument, which is the string to display to ask the question.
It should end in a space; `y-or-n-p' adds `(y or n) ' to it.
No confirmation of answer is requested; a single character is enough.
Also accepts SPC to mean yes, or DEL to mean no."
    (1on1-color-minibuffer-frame-on-setup)
    ;; Resize echo area if necessary, to show `y-or-n-p' prompt.  Compensates for functions
    ;; like `find-file-literally' that pass multi-line PROMPT args to it.  See Emacs bug #18340.
    ;; (Do not do it for Emacs 20, because it shows newlines as two ordinary chars, `\n'.)
    (when (and 1on1-fit-minibuffer-frame-flag  (> emacs-major-version 20))
      (let ((nlines  (length (split-string prompt "[\n]"))))
        (set-frame-height (window-frame (minibuffer-window)) (1+ nlines))
        (1on1-set-minibuffer-frame-top/bottom)))
    (let ((result  (1on1-ORIG-y-or-n-p prompt)))
      (when (and 1on1-fit-minibuffer-frame-flag  (> emacs-major-version 20))
        (1on1-reset-minibuffer-frame))  ; Restore frame.
      (1on1-color-minibuffer-frame-on-exit)
      result))


  (or (fboundp '1on1-ORIG-top-level)
      (fset '1on1-ORIG-top-level (symbol-function 'top-level)))

  ;; REPLACES ORIGINAL (built-in function):
  ;; Resets color of minibuffer frame to "inactive" color.
  ;;
  (defun top-level ()
    "Exit all recursive editing levels."
    (interactive)
    (1on1-color-minibuffer-frame-on-exit)
    (1on1-ORIG-top-level))


;;; $$$$$ Do not do this.  `1on1-color-minibuffer-frame-on-exit' will be called anyway,
;;;       by `minibuffer-exit-hook'.

;;;   (or (fboundp '1on1-ORIG-abort-recursive-edit)
;;;       (fset '1on1-ORIG-abort-recursive-edit (symbol-function 'abort-recursive-edit)))

;;;   ;; REPLACES ORIGINAL (built-in function):
;;;   ;; Resets color of minibuffer frame to "inactive" color.
;;;   ;;
;;;   (defun abort-recursive-edit ()
;;;     "Abort command that requested this recursive edit or minibuffer input."
;;;     (interactive)
;;;     (1on1-color-minibuffer-frame-on-exit)
;;;     (1on1-ORIG-abort-recursive-edit))

  )

(defun 1on1-setup-mode-line ()
  "Set up mode-line faces."
  (when 1on1-color-mode-line-flag
    (set-face-background (if (facep 'modeline) 'modeline 'mode-line)
                         1on1-active-mode-line-background)
    (when (facep 'mode-line-inactive)   ; Emacs 22
      (set-face-background 'mode-line-inactive 1on1-inactive-mode-line-background))))

(defun 1on1-reset-minibuffer-frame ()   ; On `minibuffer-exit-hook'.
  "Reset frame `1on1-minibuffer-frame' to its normal size and position."
  (when 1on1-minibuffer-frame
    (set-frame-size 1on1-minibuffer-frame
                    (frame-width 1on1-minibuffer-frame)
                    1on1-minibuffer-frame-height)
    (1on1-set-minibuffer-frame-top/bottom)))

(defun 1on1-set-minibuffer-frame-top/bottom ()
  "Set position of minibuffer frame.
Use `1on1-minibuffer-frame-top/bottom' if non-nil.
Else, place minibuffer at bottom of display."
  (when 1on1-minibuffer-frame
    (condition-case nil
        (if nil ;; $$$$$$ (fboundp 'redisplay)
            (redisplay t)
          (force-mode-line-update t))
      (error nil))         ; Ignore errors from, e.g., killed buffers.
    (modify-frame-parameters
     1on1-minibuffer-frame
     `((top ,@ (or 1on1-minibuffer-frame-top/bottom
                   (if (not (fboundp 'display-monitor-attributes-list))
                       (- 1on1-task-bar-height)
                     1on1-minibuffer-frame-bottom-offset)))))))

(defun 1on1-set-minibuffer-frame-width ()
  "Set width of minibuffer frame, in characters.
Use `1on1-minibuffer-frame-width' if not nil.
Else, set width relative to character size of `1on1-minibuffer-frame'
and display monitor size, and depending on
`1on1-minibuffer-frame-width-percent'."
  (when 1on1-minibuffer-frame
    (set-frame-width
     1on1-minibuffer-frame
     (or 1on1-minibuffer-frame-width
         (/ (* 1on1-minibuffer-frame-width-percent
               (if (not (fboundp 'display-monitor-attributes-list))
                   (x-display-pixel-width)
                 (catch '1on1-set-minibuffer-frame-width
                   (dolist (attr  (display-monitor-attributes-list))
                     (when (memq 1on1-minibuffer-frame (cdr (assoc 'frames attr)))
                       (throw '1on1-set-minibuffer-frame-width (nth 3 (assoc 'geometry attr)))))
                   ;; Fallback - should not happen.  No monitor has minibuffer frame.
                   (nth 3 (assoc 'geometry (car (display-monitor-attributes-list)))))))
            (* 100 (frame-char-width 1on1-minibuffer-frame)))))))

;;;###autoload
(defun 1on1-fit-minibuffer-frame (&optional resetp)
  "Fit the standalone minibuffer frame height to its contents.
Repeat to increase the height by 1.
With a prefix arg, reset the frame to its default position and height.
Bind this in minibuffer keymaps to a key such as `C-o' that you can
use during minibuffer input.
This command requires library `fit-frame.el'."
  (interactive "P")
  (unless (require 'fit-frame nil t)
    (error "You need to load library `fit-frame.el' to use this command"))
  ;; We could assume the minibuffer frame is `1on1-minibuffer-frame', but we do not.
  (when (and 1on1-fit-minibuffer-frame-flag
             (active-minibuffer-window)
             ;; Do this because this command is on `post-command-hook', and an event such as
             ;; `handle-switch-frame' might have changed the selected frame.
             (eq last-event-frame (window-frame (minibuffer-window)))
             (eq (frame-first-window last-event-frame) (minibuffer-window))

             ;; $$$$$$ Previous sexp replaces this, which should do the same thing:
             ;; (one-window-p nil (window-frame (minibuffer-window)))
             ;;
             ;; And that replaces this, which, again, should do the same thing:
             ;; (save-window-excursion
             ;;  (select-window (minibuffer-window))
             ;;  ;; We should be able to use just (one-window-p),
             ;;  ;; but an Emacs bug means we need this:
             ;;  (one-window-p nil 'selected-frame))
             )
    (let* ((frame         (window-frame (minibuffer-window)))
           (frame-height  (frame-height frame)))
      (cond
        (resetp
         (set-frame-height frame 1on1-minibuffer-frame-height) ; Reset to default.
         (1on1-set-minibuffer-frame-top/bottom))
        ((eq last-command '1on1-fit-minibuffer-frame)
         (set-frame-height frame (1+ (frame-height frame)))
         (1on1-set-minibuffer-frame-top/bottom)
         ;; $$$$$$ This interfered with `C-e' and inserting text at end.
         ;; (condition-case nil (scroll-down (frame-height frame)) (error nil))
         )
        (t
         (let* ((beg                                     (1on1-minibuffer-prompt-end))
                (fit-frame-max-height                    1on1-fit-minibuffer-frame-max-height)
                (fit-frame-max-height-percent
                 1on1-fit-minibuffer-frame-max-height-percent)
                (fit-frame-min-height                    1on1-minibuffer-frame-height)
                (window-min-height                       1on1-minibuffer-frame-height)
                (fit-frame-empty-height                  1on1-minibuffer-frame-height)
                (fit-frame-empty-special-display-height  1on1-minibuffer-frame-height))
           (if (> emacs-major-version 22)
               ;; In principle, that test could have been (fboundp 'count-screen-lines), which
               ;; is defined for Emacs 21+.  But doing that makes the frame jump up and down
               ;; inappropriately in Emacs 21-22.  So do it only for Emacs 23+.

               ;; The reason for the different code for Emacs 23+:
               ;; For `icomplete.el', Emacs 23+ uses an overlay instead of inserting text into
               ;; the buffer.  `fit-frame' cannot take the height of that overlay into account.
               ;; So we use `count-screen-lines' to get the height.
               (fit-frame frame (frame-width frame)
                          ;; Need to be sure to use the right buffer.  Some commands etc. can
                          ;; pop up a frame and select it, or otherwise change the selected buf.
                          (with-current-buffer (or (window-buffer (active-minibuffer-window))
                                                   (current-buffer))
                            (1+ (count-screen-lines))))
             (fit-frame frame (frame-width frame)))
           ;; $$$$       (when (>= emacs-major-version 21)
           ;;              (set-frame-height frame (1+ (frame-height frame)))) ; A little extra.
           (1on1-set-minibuffer-frame-top/bottom)
           ;; $$$$$$ This interfered with `C-e' and inserting text at end.
           ;; (condition-case nil (scroll-down (frame-height frame)) (error nil))
           ))))))

(defun 1on1-minibuffer-prompt-end ()
  "Version of `minibuffer-prompt-end' that works for Emacs 20 and later."
  (if (fboundp 'minibuffer-prompt-end) (minibuffer-prompt-end) (point-min)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; oneonone.el ends here
