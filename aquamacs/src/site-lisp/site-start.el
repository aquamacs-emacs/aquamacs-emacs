;; Site startup file
;; loads osx_defaults and auctex defaults 

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: site-start.el,v 1.12 2005/10/26 23:59:38 davidswelt Exp $

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


;; undefine this if you're a developer and you have changed code



(when ;; do not load this twice 
    (not (memq 'aquamacs-site-start features))

;; (if (boundp 'aq-preloaded)
;;     (mapc (lambda (p)
;; 	    (setq features (delete p features)))
;; 	  aq-preloaded))

  (provide 'aquamacs-site-start)

  (require 'load-emacs-pre-plugins)

  (require 'aquamacs)
  (aquamacs-setup)




  (require 'aquamacs-mode-defaults)
 
  (require 'load-emacs-plugins)


  )
