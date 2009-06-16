;;; pstricks.el --- AUCTeX style for the `pstricks' babel option.

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2007-06-10
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; Add support for PSTricks.

;; TODO: Actually implement support.  Currently only TeX PDF mode is
;; turned off.  This is done here because adding the hook in latex.el
;; prevents other pstricks styles from being loaded.

;;; Code:

(TeX-add-style-hook
 "pstricks"
 (lambda ()
   (unless (member "pst-pdf" TeX-active-styles)
     (TeX-PDF-mode-off))))

;;; pstricks.el ends here
