;;; def-face-const.el --- Macro for defining faces & their variables.
;; 
;; Filename: def-face-const.el
;; Description: Macro for defining faces & their variables.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1999-2004, Drew Adams, all rights reserved.
;; Created: Fri Apr  2 09:08:55 1999
;; Version: 21.0
;; Last-Updated: Sat Dec 18 17:18:15 2004
;;           By: dradams
;;     Update #: 68
;; Keywords: faces
;; Compatibility: GNU Emacs 21.x, GNU Emacs 20.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;    A macro for defining faces and their (constant) variables.
;; 
;;  Macro `define-face-const' can be used to define faces
;;  and constant variables having the faces as values,
;;  given the face foreground and/or background names (strings).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
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

(and (< emacs-major-version 20)(eval-when-compile (require 'cl))) ;; when, unless


;; Uses `make-face', defined in `faces.el'.

;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro define-face-const (foreground background)
  "Define a constant variable (via `defconst') naming a new face.
FOREGROUND is either nil or a string naming the new face's foreground color.
BACKGROUND is either nil or a string naming the new face's background color.

FOREGROUND (or BACKGROUND) nil means do not set the foreground (or the
BACKGROUND).  If both are nil, the new variable's value is nil.
The value of the new variable (new face or nil) is returned.

Only colors (strings) satisfying `x-color-defined-p' are accepted.
\"Black\" is used in place of any unacceptable foreground color name.
\"White\" is used in place of any unacceptable background color name.

The name of the new constant variable is as follows:
If both FOREGROUND and BACKGROUND are strings: FOREGROUND-on-BACKGROUND-face
If only FOREGROUND is a string:                FOREGROUND-foreground-face
If only BACKGROUND is a string:                BACKGROUND-background-face

Examples of use: 

 (define-face-const \"Blue\" \"Thistle\") => (defconst 'blue-on-thistle-face)
       where (face-foreground 'blue-on-thistle-face) = \"Blue\"
             (face-background 'blue-on-thistle-face) = \"Thistle\"

 (define-face-const \"Blue\" nil) => (defconst 'blue-foreground-face)
       where (face-foreground 'blue-foreground-face) = \"Blue\"

 (define-face-const nil \"Thistle\") => (defconst 'thistle-background-face)
       where (face-background 'thistle-background-face) = \"Thistle\"

If color ZZZZZZ is undefined:

 (define-face-const \"Blue\" \"ZZZZZZ\") => (defconst 'blue-on-white-face)
       where (face-foreground 'blue-on-white-face) = \"Blue\"
             (face-background 'blue-on-white-face) = \"White\"

 (define-face-const \"ZZZZZZ\" \"Pink\") => (defconst 'black-on-pink-face)
       where (face-foreground 'black-on-pink-face) = \"Black\"
             (face-background 'black-on-pink-face) = \"Pink\""
  (when (fboundp 'x-color-defined-p)    ; Ensure defined colors or nil.
    (when (and (stringp foreground)
               (not (x-color-defined-p foreground))
               (not (x-color-defined-p (setq foreground (downcase foreground)))))
      (setq foreground "Black"))
    (when (and (stringp background)
               (not (x-color-defined-p background))
               (not (x-color-defined-p (setq background (downcase background)))))
      (setq background "White")))
  (let ((face-name (cond ((and (stringp foreground) (stringp background))
                          (downcase
                           (concat foreground "-on-" background "-face")))
                         (foreground
                          (downcase (concat foreground "-foreground-face")))
                         (background
                          (downcase (concat background "-background-face")))
                         (t nil))))
    (if (and (fboundp 'make-face)
             (setq face-name (and face-name (intern face-name))))
        (` (progn
             (let ((new-face
                    (defconst (, face-name) (make-face '(, face-name)))))
               (when (, foreground)
                 (set-face-foreground (, face-name) (, foreground)))
               (when (, background)
                 (set-face-background (, face-name) (, background)))
               new-face)))
      (` (defconst (, face-name) nil)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'def-face-const)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; def-face-const.el ends here
