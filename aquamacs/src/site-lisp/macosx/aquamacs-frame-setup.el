;;; aquamacs-frame-setup  - Frames and Faces
;;
;; Filename: aquamacs-frame-setup.el
;; Description: Emacs init file for use with libraries from Drew Adams
;; Author: Drew Adams
;; Maintainer: David Reitter
;; Keywords: aquamacs

;; Last change: $Id: aquamacs-frame-setup.el,v 1.44 2009/03/10 21:46:02 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
;; Copyright (C) 2005, 2008, David Reitter, all rights reserved.



(provide 'aquamacs-frame-setup) ;; added by dr. 12/2004

(require 'fit-frame)

;; allow autofitting using smart-frame-positioning
(defvar smart-frame-positioning-hook nil)
(add-hook 'smart-frame-positioning-hook
          (lambda (f)
            (if (frame-parameter f 'fit-frame )
                (when (fboundp 'fit-frame-single-window-forced)
                  (progn (fit-frame-single-window-forced f))))))

(require 'aquamacs-mac-fontsets)

(defun aquamacs-define-the-fringe-bitmap ()
  "Redefines a fringe bitmap (continuation) so that it looks good
even when minimal fringes are used. (Aquamacs)"

  ;; Code by Kim F. Storm
  (define-fringe-bitmap 'right-truncation
    "\xa9\x02\x04" nil nil 'bottom)
  (define-fringe-bitmap 'left-truncation
    "\x20\x40\xaa\0\0" nil nil 'bottom)
  (define-fringe-bitmap 'right-continuation
    "\xa8\0\0" nil nil 'bottom)
  (define-fringe-bitmap 'left-continuation
    "\x2a\0\0" nil nil 'bottom)

  (let ((tr (assoc 'truncation default-fringe-indicator-alist))
        (co (assoc 'continuation default-fringe-indicator-alist)))
    (if tr (setcdr tr '(left-truncation right-truncation)))
    (if co (setcdr co '(left-continuation right-continuation)))))

;;(customize-set-variable 'fringe-indicators 'empty)
(setq default-indicate-empty-lines t)

(aquamacs-define-the-fringe-bitmap)
(setq fringe-mode nil) ;; to reflect the default.
;; This is a hack because fringe-mode likes to round up stuff.

;; set default colors
(aquamacs-set-defaults
;; because we fit *Help* frames (fit-frame), we want to specify a
 ;; minimum of 68 characters. Otherwise we'll fit, and as soon as a
 ;; new help page is displayed, text gets wrapped.
 '((create-frame-min-width 68)
;; do not set any fonts here. aquamacs.el takes care of this,
   ;; checking that the fonts actually exist.

   ;; Minimal defaults in default-frame-alist, because they
   ;; interfere with custom theme based customization
   (default-frame-alist
     (;;  These are default anyways.  Do not set them in
      ;; d-f-a to allow better customization with themes etc.
      ;; (foreground-color . "Black") (background-color . "White")
      (cursor-color . "Red")
      ;; default anyway (vertical-scroll-bars . right)
      ;; default anyway (tool-bar-lines . 1 )
      (internal-border-width . 0)
      (left-fringe . 1) (right-fringe . nil) (fringe . nil)))

   (special-display-frame-alist
    ((unsplittable . nil)
     (width . 75)
     (height . 35)
     (left . 0)
     (top . 30)))))


;;; COMMENT THIS OUT IF YOU DO *NOT* WANT MAXIMUM BUFFER HIGHLIGHTING.
(defconst font-lock-maximum-decoration t)


(defconst special-display-regexps '( "[ ]?\\*info.*\\*[ ]?"
                                     "[ ]?\\*[hH]elp.*"
                                     "[ ]?\\*Messages\\*[ ]?"
                                     "[ ]?\\*Open Recent\\*[ ]?"
                                     ".*SPEEDBAR.*"))


;; AUTO-FITTING
;; we will auto-fit the frames of all *help* (and other temp) buffers
;; but everything else won't autofit

(defvar autofit-frames-flag nil) ; Inhibit automatic frame fitting.
;;; (defvar inhibit-fit-frame t) ; Inhibit *ALL* frame fitting, (even `C-x C-_').

;; we allow frame fitting by default
(aquamacs-set-defaults
 '((inhibit-fit-frame-flag nil)))

; but never fit frames automatically, but force frame-fitting

(defun fit-frame-single-window-forced ( f)
  "Resize frame to fit selected window if it is alone in the frame.
Usable in `temp-buffer-show-hook'."
  (let ((inhibit-fit-frame-flag nil))
    (and (one-window-p t) (fit-frame f))))

 ;;;;;;;;;;;;;;;;
;; the following is copied from Drew Adams's start.el package:

(require 'simple+ nil t)                ; Corrections, extensions.
;; do not import - always fits frame for special frames
;; (require 'frame+ nil t)                 ; Corrections, extensions.
(require 'frame-cmds nil t)             ; Frame and window commands.
(when (ad-is-advised 'delete-window)
  ;; dangerous overreach?
  (ad-deactivate 'delete-window)) ;; do not advise delete-window for compatibility
