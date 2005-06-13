; Aquamacs
; Mode & Package defaults
 
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-mode-defaults.el,v 1.1 2005/06/13 22:44:28 davidswelt Exp $

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
 
;; Copyright (C) 2005, David Reitter


;; load auctex if present 
(ignore-errors (require 'auctex-config nil t))




(autoload 'css-mode "css-mode" "major mode for editing CSS source." t)
(setq auto-mode-alist
      (cons '("\\.css$" . css-mode) auto-mode-alist)
      )

(autoload 'applescript-mode "applescript-mode" "major mode for editing AppleScript source." t)
(setq auto-mode-alist
      (cons '("\\.applescript$" . applescript-mode) auto-mode-alist)
      )
 
;; ---------------------------------------------------------
;; PERL EDITING and other modes

(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
;(setq cperl-hairy t)
(defalias 'perl-mode 'cperl-mode)
 (setq cperl-invalid-face nil) ;(uherbst)
 
 (setq cperl-highlight-variables-indiscriminately t)


(provide 'aquamacs-mode-defaults)