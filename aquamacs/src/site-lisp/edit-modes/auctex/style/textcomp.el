;;; textcomp.el --- AUCTeX style for `textcomp.sty' (v2.0n)

;; Copyright (C) 2014, 2017, 2020 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2014-10-25
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

;; This file adds support for `textcomp.sty' (v2.0n) from 2020/02/02.
;; With this version, the package mainly no-op as the macros are moved
;; into LaTeX kernel.  `textcomp.sty' is a standard LaTeX package and
;; part of TeXLive.

;;; Code:

(defvar LaTeX-textcomp-package-options
  '("full" "almostfull" "euro" "safe" "error"
    "warn" "info" "quiet" "force")
  "Package options for the textcomp package.")

;;; textcomp.el ends here
