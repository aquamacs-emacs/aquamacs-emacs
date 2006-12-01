;;; JDEE configuration
;;; 

;; Maintainer: David Reitter
;; Keywords: java jde
 
;; Last change: $Id: jde-config.el,v 1.1 2006/12/01 23:06:13 davidswelt Exp $

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
 
;; Copyright (C) 2006 David Reitter


;; Compiling CEDET and JDE
;; CEDET compiled with
;; EMACS=/Applications/Aquamacs Emacs.app/Contents/MacOS/Aquamacs Emacs
;; and all calls to Emacs corrected (where needed) to "$(EMACS)"


 
(when load-file-name
  (add-to-list 'load-path (expand-file-name "cedet/common" (file-name-directory load-file-name)))
  (add-to-list 'load-path (expand-file-name "semantic" (file-name-directory load-file-name)))
  (add-to-list 'load-path (expand-file-name "jde/lisp" (file-name-directory load-file-name)))
  (add-to-list 'load-path (expand-file-name "elib" (file-name-directory load-file-name))))

(autoload 'jde-mode "jde" "JDE mode." t)
(setq auto-mode-alist
      (append
       '((".java'" . jde-mode))
       auto-mode-alist))

;; find installed JDKs

(let* ((java-path "/System/Library/Frameworks/JavaVM.framework/Versions")
      (java-versions (directory-files java-path nil "[0-9]+\..*")))
(aquamacs-set-defaults
 `((semanticdb-default-save-directory 
    ,(concat temporary-file-directory "semantic.cache"))
   (jde-jdk-registry
    ,(mapcar (lambda (version) (cons version (concat java-path "/" version))) 
	     java-versions))
   (jde-jdk ,(last java-versions)))))
 