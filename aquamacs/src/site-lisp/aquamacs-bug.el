;; Aquamacs-bug.el
 
;; Maintainer: David Reitter
;; Keywords: mac bug report
 
;; Last change: $Id: aquamacs-bug.el,v 1.9 2005/12/06 12:15:42 davidswelt Exp $

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

;; Copyright (C) 1985, 1994, 1997, 1998, 2000, 2001, 2002
;; Free Software Foundation, Inc.

;; Copyright (C) 2005, David Reitter



; (require 'emacsbug)
(provide 'aquamacs-bug)

;; Request by RMS 06/2005: do not report Aquamacs bugs 
;; to the Emacs mailing lists.

(aquamacs-set-defaults 
 '(
   (report-emacs-bug-address "aquamacs-bugs@aquamacs.org")
   (report-emacs-bug-pretest-address "aquamacs-bugs@aquamacs.org")
   (report-emacs-bug-no-confirmation t)))
 
;; use standard emacs bug reporting technology.