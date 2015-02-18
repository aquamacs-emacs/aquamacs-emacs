;;; aquamacs-faces.el --- Lisp faces for Aquamacs

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
;; Copyright (C) 2005, 2008, David Reitter, all rights reserved.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standard Aquamacs Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface aquamacs-variable-width
  '((((type ns))
     :height 120 :weight normal :width normal :slant normal :underline nil
     :strike-through nil :stipple nil :family "Lucida Grande"))
  "Default variable-width face"
  :group 'Aquamacs)

(defface autoface-default
  '((((type ns))
     :inherit default))
  "Default face for all buffers for `aquamacs-autoface-mode'"
  :group 'Aquamacs)

(defface text-mode-default
  '((((type ns))
     :inherit autoface-default :height 130 :weight normal :width normal
     :slant normal :underline nil :strike-through nil :stipple nil
     :family "Lucida Grande"))
  "Default face for text in `aquamacs-autoface-mode'"
  :group 'Aquamacs)

(defface latex-mode-default
  '((((type ns))
     :inherit autoface-default :height 130 :weight normal :width normal
     :slant normal :underline nil :strike-through nil :stipple nil
     :family "Lucida Grande"))
  "Default face for LaTeX in `aquamacs-autoface-mode'"
  :group 'Aquamacs)

(defface org-mode-default
  '((((type ns))
     :inherit autoface-default :height 120 :weight normal :width normal
     :slant normal :underline nil :strike-through nil :stipple nil
     :family "Monaco"))
  "Default face for Org-Mode in `aquamacs-autoface-mode'"
  :group 'Aquamacs)

;; do not inherit from autoface-default here, in case auto-face-mode is
;; switched off.
(defface echo-area
 '((((type ns))
    ;; height same as for global default font (Monaco)
    :weight normal :width normal
    :slant normal :underline nil :strike-through nil :stipple nil
    :family "Lucida Grande"))
 "Face for Echo area (minibuffer and messages)."
 :group 'basic-faces
 :group 'Aquamacs)

(defface minibuffer
 '((t :inherit echo-area))
 "Face for Echo area (minibuffer and messages)."
 :group 'basic-faces
 :group 'Aquamacs)

;; this should inherit from echo-area
; override face defined in faces.el
(set-face-attribute 'minibuffer-prompt nil 
		    :inherit 'minibuffer)
