;;; aquamacs-frame-setup
;;  --- This has been adapted from Drew Adams' Emacs init file. 
;; 
;; Filename: aquamacs-frame-setup.el
;; Description: Emacs init file for use with libraries from Drew Adams
;; Author: Drew Adams
;; Maintainer: David Reitter
;; Keywords: aquamacs
 

;; Last change: $Id: aquamacs-frame-setup.el,v 1.14 2005/11/16 12:51:43 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Copyright (C) 1995-2004, Drew Adams, all rights reserved.
;; Copyright (C) 2005, David Reitter, all rights reserved.



(provide 'aquamacs-frame-setup) ;; added by dr. 12/2004
 
 (require 'fit-frame)
 (require 'aquamacs-mac-fontsets)

;(defvar 1on1-default-frame-font "fontset-mac")
;(defvar 1on1-special-display-frame-font "fontset-mac")
;(defvar 1on1-minibuffer-frame-font "fontset-mac")


;(setq 1on1-default-frame-upper-left-corner '(80 . 60))
;(setq 1on1-default-special-frame-upper-left-corner '(180 . 140))
;(setq 1on1-default-special-frame-size '(400 . 600))
 
;(set-default  'indicate-empty-lines t)
;(setq indicate-empty-lines t)
 ;; Fringes
(defun aquamacs-define-the-fringe-bitmap ()
  "Redefines a fringe bitmap (continuation) so that it looks good
even when minimal fringes are used. (Aquamacs)"
  (let ((fm fringe-mode))
    (fringe-mode 0) ;; turn off temporarily
    (define-fringe-bitmap  'continuation-line
      ;; a plus sign
      [#B0011111111
       #B0001111111
       #B0000110011
       #B0000011011
       #B0000001111
       #B0000000111
       #B0000000011
       #B0000000001])

    (fringe-mode fm) ;; turn back on
    )
  )
;;(customize-set-variable 'fringe-indicators 'empty)
(setq default-indicate-empty-lines t)

(aquamacs-define-the-fringe-bitmap)
(setq fringe-mode '(1 . 1)) ;; to reflect the default.

;; This is a hack because fringe-mode likes to round up stuff.

;; set default colors
(aquamacs-set-defaults 
 '(

;;( 1on1-help-frame-background "LightBlue") 
;( 1on1-*Help*-frame-flag nil)
;( 1on1-*Completions*-frame-flag nil)

(default-frame-alist 
((foreground-color . "Black") (background-color . "White") (font . "fontset-monaco12")  (cursor-color . "Red")   (vertical-scroll-bars . right)  (tool-bar-lines . 1) (left-fringe . 1) (right-fringe . nil) (fringe . nil)))))

 
 
; (setq autofit-frames-flag nil)
 ;(setq 1on1-minibuffer-frame-flag nil) ;; this is a defvar

(remove-hook 'same-window-regexps "\\*info\\*\\(\\|<[0-9]+>\\)")
(remove-hook 'same-window-regexps "\\`\\*Customiz.*\\*\\'")
 ;; ;; only subsequent ones should be opened in the same frame
(add-hook 'custom-mode-hook
	  (lambda ()
	    (add-to-list (make-local-variable 'same-window-regexps)
			 "\\`\\*Customiz.*\\*\\'")))
	    
;;; UNCOMMENT AND CHANGE *ONE* OF THESE, IF DEFAULT FRAME HEIGHT IS INAPPROPRIATE. 
;;; Maximum height for new frames.
;;; (defvar create-frame-max-height-percent 82) ; no more than 82% of display height.
;;; (defvar create-frame-max-height 48)         ; no more than 48 characters high.
(defvar create-frame-max-height-percent 75)

;;; UNCOMMENT AND CHANGE *ONE* OF THESE, IF DEFAULT FRAME WIDTH IS INAPPROPRIATE.
;;; Maximum width for new frames.
;;; (defvar create-frame-max-width-percent 94) ; no more than 94% of display width.
;;; (defvar create-frame-max-width 120)        ; no more than 120 characters wide. 
 
;;; COMMENT THIS OUT IF YOU DO *NOT* WANT MAXIMUM BUFFER HIGHLIGHTING.
(defconst font-lock-maximum-decoration t)


;;; COMMENT OUT IF YOU DO *NOT* WANT THE GIVEN EFFECT. 
;(put 'eval-expression 'disabled nil)    ; Enable eval of Lisp sexps.
;(put 'narrow-to-region 'disabled nil)   ; Enable region narrowing & widening.
;(put 'downcase-region 'disabled nil)    ; Enable case changes of region text.
;(put 'upcase-region 'disabled nil)      ;   "     "   
;(put 'capitalize-region 'disabled nil)  ;   "     "  
;(auto-compression-mode 1)               ; Auto decompress compressed files.

  

;;; UNCOMMENT AND CHANGE IF DEFAULT VALUES ARE INAPPROPRIATE.
;;; (NOTE: These are *not* used if
;;;        `1on1-separate-minibuffer-frame-flag' is nil.)
;;;
;;; (defvar minibuffer-frame-top/bottom nil
;;;     "Position of top (or bottom) of minibuffer frame, in pixels.
;;; If nil, function `set-minibuffer-frame-top/bottom' will position
;;; minibuffer at bottom of display.
;;;
;;; May be of form `POS', `(+ POS)' or `(- POS)', where POS is a
;;; positive integer.  Forms POS and `(+ POS)' specify the position of
;;; frame top with respect to screen top.  Forms -POS and `(- POS)'
;;; specify the position of frame bottom with respect to screen
;;; bottom.  In any case, POS itself counts toward the top.
;;;
;;; See `default-frame-alist' for an explanation of frame parameters.
;;;
;;; Note: This is not used if `1on1-separate-minibuffer-frame-flag'
;;        is nil.")
;;;
;;; (defvar minibuffer-frame-width nil
;;;     "Width, in characters, for minibuffer frame.
;;; If nil, then the function `minibuffer-frame-width' is used instead.
;;;
;;; Note: This is not used if `1on1-separate-minibuffer-frame-flag'
;;;       is nil.")
;;;
;;; (defvar minibuffer-frame-height 2 "*Height of minibuffer frame, in characters.")
 

;;; COMMENT THIS OUT IF YOU DO *NOT* WANT "SPECIAL" BUFFERS TO BE IN
;;; SEPARATE FRAMES ("Special" buffers are those, such as *grep*,
;;; whose names are within '*'s.) If `special-display-regexps' is
;;; non-nil, then special buffers are in dedicated frames (cannot
;;; dissociate the buffer and its frame).
 ;; (defconst special-display-regexps '("[ ]?[*][^*]+[*]"))
 ; (defconst special-display-regexps '("[ ]?[*][^sC][^c][^r][^*]+[*]" ))

;; We cannot just open all buffers in special frames, because
;; then we could not pop up nice *Completion* windows etc locally.

;; Help windows only start with *Help (so e.g. ESS Help gets opened separately)
(defconst special-display-regexps '( "[ ]?\\*info.*\\*[ ]?"  "[ ]?\\*[hH]elp.*" 
				     "[ ]?\\*Messages\\*[ ]?"   "[ ]?\\*Open Recent\\*[ ]?"
				     ".*SPEEDBAR.*"))
; "[ ]*Customize*"
;; If we make Backtrace dedicated,
;; the frame gets iconified all the time
;; while debugging ( "[ ]?\\*Backtrace\\*[ ]?"  )

;;(defconst special-display-regexps '("*"))
; (defconst special-display-buffer-names '())
;; we don't want dedicated frames for everything, 
;; because then switch-to-buffer and
;; the like just open new buffers (when following links e.g.) in
;; the next-best not-dedicated frame. this is no no no good.


;; AUTO-FITTING
;; we will auto-fit the frames of all *help* (and other temp) buffers
;; but everything else won't autofit

(defvar autofit-frames-flag nil) ; Inhibit automatic frame fitting.
;;; (defvar inhibit-fit-frame t) ; Inhibit *ALL* frame fitting, (even `C-x C-_').

(aquamacs-set-defaults 
 '(
   (inhibit-fit-frame-flag t)))

; never fit frames automatically, but force frame-fitting

(defun fit-frame-single-window-forced ( f)
  "Resize frame to fit selected window if it is alone in the frame.
Usable in `temp-buffer-show-hook'."

  (let ((inhibit-fit-frame-flag nil))
    (and (one-window-p t) (fit-frame f))
    )
  )

; i don't do the following because it causes the frame to dance...
; 
; (add-hook 'temp-buffer-show-hook 'fit-frame-if-one-window-regardless-of-autofit-frames-flag) 
;(add-hook 'help-mode-hook 'fit-frame-if-one-window-regardless-of-autofit-frames-flag 'append)

; we do it properly - in mode-specific themes!
 ; this is set in osx-defaults
 




;;; UNCOMMENT IF YOU DO *NOT* WANT TO USE `remove-window'.
;;; Non-nil => Use `remove-window' in place of `delete-window'.
;;; (defvar sub-remove-window nil)



;;; UNCOMMENT IF YOU DO *NOT* WANT TO USE `query-replace-w-options'.
;;; Non-nil => Use `query-replace-w-options' in place of `query-replace'.
;;; (defvar sub-query-replace-w-options nil)


;;; UNCOMMENT IF YOU DO *NOT* WANT TO USE `kill-buffer-and-its-windows'.
;;; Non-nil => Use `kill-buffer-and-its-windows' in place of `kill-buffer'.
;;; (defvar sub-kill-buffer-and-its-windows nil)


;;; UNCOMMENT IF YOU DO *NOT* WANT TO USE `exit-with-confirmation'.
;;; Non-nil => Use `exit-with-confirmation' in place of `save-buffers-kill-emacs'.
;;; (defvar sub-exit-with-confirmation nil)
 
 
 ;;;;;;;;;;;;;;;;
 ;; the following is copied from Drew Adams's start.el package:
 
(require 'simple+ nil t)                ; Corrections, extensions.
(require 'frame+ nil t)                 ; Corrections, extensions.
(require 'frame-cmds nil t)             ; Frame and window commands.
 ; (require 'autofit-frame nil t)          ; automatically fit frames to sole window. 
 

;; let's try to go without oneonone

; (require 'oneonone nil nil)                 ; Default frame configuration.

;(if (string= "mac" window-system)
    ;;(add-hook 'after-init-hook '1on1-emacs) ;; test 
;    (let ((initial-frame-alist)) ; workaround
;      (1on1-emacs) ;; need to call this here for now
;    )
;)
;;(require 'window+ nil t)                ; Corrections.
 
;;;  

;;; UNCOMMENT TO TELL CUSTOMIZE THAT THE PRESENT STATE IS THE BASE-LINE:
;;; Consider current option values as unchanged (pseudo-saved).
;;; This function is defined in `cus-edit+.el'.
;(when (fboundp 'customize-consider-all-unchanged)
;   (customize-consider-all-unchanged))

;;; COMMENT THIS OUT IF YOU DO *NOT* WANT AUTOMATIC CUSTOMIZE UPDATING,
;;; so it takes into account changes made outside Customize,
;;; considering them to "set" the preferences.
(when (fboundp 'customize-toggle-outside-change-updates)
  (customize-toggle-outside-change-updates 99))
 
 
;;; TO *INHIBIT* FITTING *NEW* FRAMES UPON CREATION, UNCOMMENT THIS
;;; AND MOVE IT AFTER (require 'start-opt), BELOW.
;(remove-hook ' after-make-frame-functions 'fit-frame)
;(add-hook 'after-make-frame-functions 'fit-frame)

;;; ******************************************************************
;;; IMPORTANT - DO THIS *LAST*, SO `rebind-minibuffer-completion-maps'
;;;             CAN PICK UP ALL CURRENT KEY DEFINITIONS.
;;;
;;; Enable minibuffer cycling of default inputs via arrow keys.
; (require 'rebind-mbuf-maps)		; Requires library 'elect-mbuf'.
;;;
;;; ******************************************************************

 