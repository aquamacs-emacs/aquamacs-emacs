;;;; $Id: elib-compile-all.el,v 1.1 2006/12/01 23:31:41 davidswelt Exp $
;;;; This file byte-compiles all .el files in elib.

;; Copyright (C) 1991-1995 Free Software Foundation

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;;	Inge Wallin <inge@lysator.liu.se>
;; Maintainer: elib-maintainers@lysator.liu.se
;; Created: 14 Mar 1992

;;;; This file is part of the GNU Emacs lisp library, Elib.
;;;;
;;;; GNU Elib is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; GNU Elib is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Elib; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA 02111-1307, USA
;;;;

;;; Code:

(setq elib-files '("stack-f"
		   "stack-m"
		   "queue-f"
		   "queue-m"
		   "elib-node"
		   "dll"
		   "dll-debug"
		   "bintree"
		   "avltree"
		   "cookie"
		   "string"
		   "read"))


(defun compile-file-if-necessary (file)
  "Compile the Elib file FILE if necessary.

This is done if FILE.el is newer than FILE.elc or if FILE.elc doesn't exist."
  (let ((el-name (concat file ".el"))
	(elc-name (concat file ".elc")))
    (if (or (not (file-exists-p elc-name))
	    (file-newer-than-file-p el-name elc-name))
	(progn
	  (message (format "Byte-compiling %s..." el-name))
	  (byte-compile-file el-name)))))


(defun compile-elib ()
  "Byte-compile all uncompiled files of elib."

  ;; Be sure to have . in load-path since a number of files in elib
  ;; depend on other files and we always want the newer one even if
  ;; a previous version of elib exists.

  (interactive)
  (let ((load-path (append '(".") load-path)))
    (mapcar (function compile-file-if-necessary)
	    elib-files)))

;; elib-compile-all.el ends here
