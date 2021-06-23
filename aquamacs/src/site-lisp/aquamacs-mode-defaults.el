; Aquamacs
; Mode & Package defaults

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs

;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/


;; Aquamacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; Aquamacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Aquamacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011 David Reitter

;; Add edit-modes file hierarchy to load-path

;; aquamacs-preloaded-load-path was set during preloading
;; it is used as a cache, since searching the file hierarchy takes plenty of time.
;; here, we just turn it into a full path name
(defvar aquamacs-preloaded-load-path nil) ;; in case of no preloading

(let ((res (expand-file-name (mac-resources-path)))
      (lp2))
  (dolist (p aquamacs-preloaded-load-path)
    (push (concat res p) lp2))
  (nconc load-path lp2))

(unless aquamacs-preloaded-load-path
  (message "Error: load path cache was not computed during preloading."))

;; load auctex if present

;; AUCTEX - load at runtime (necessary?)
(condition-case nil
    (require 'auctex-config nil t)
  (error nil))

(defun smart-dnd-latex ()
   (smart-dnd-setup
    '(
      ("\\.tex\\'" . "\\input{%r}\n")
      ("\\.cls\\'" . "\\documentclass{%f}\n")
      ("\\.sty\\'" . "\\usepackage{%f}\n")
      ("\\.eps\\'" . "\\includegraphics[]{%r}\n")
      ("\\.ps\\'"  . "\\includegraphics[]{%r}\n")
      ("\\.pdf\\'" . "\\includegraphics[]{%r}\n")
      ("\\.jpg\\'" . "\\includegraphics[]{%r}\n")
      ("\\.png\\'" . "\\includegraphics[]{%r}\n")
      )))
;; non-AUCTeX mode:
(add-hook 'latex-mode-hook 'smart-dnd-latex)
;; AUCTeX:
(defvar LaTeX-mode-hook nil)
(add-hook 'LaTeX-mode-hook 'smart-dnd-latex)

(defun smart-dnd-setup-always-insert-quoted-file-name ()
  "Setup `smart-dnd-mode' so that drag&drop always inserts the file path."
  (smart-dnd-setup '((".*" . "\"%r\""))))

;; Eshell
(add-hook 'eshell-mode-hook 'smart-dnd-setup-always-insert-quoted-file-name)
;; shell mode, and friends
(add-hook 'comint-mode-hook 'smart-dnd-setup-always-insert-quoted-file-name)


;; NXHTML
;; loaded in mode-preloads.el

;; (load "autostart.el")   ;; VERY SLOW
;; (aquamacs-set-defaults '((nxhtml-menu-mode nil)))
;; (autoload 'nxhtml-mode "autostart.el" "Major mode for editing XHTML documents." 'interactive nil)
;; (autoload 'nxhtml-menu-mode "autostart.el" "Minor mode providing web project management and more." 'interactive nil)

;; JDEE
;; in mode-preloads


;; SLIME
;; in mode-preloads



(provide 'aquamacs-mode-defaults)
