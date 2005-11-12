;; Aquamacs Emacs OS X defaults
;; these defaults attempt to turn Emacs into a nice application for 
;; Mac OS X that adheres to at least some of the user interface 
;; standards on the Mac
;;
;; This is the MacOSX-specific configuration file. 
;;
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: osx_defaults.el,v 1.40 2005/11/12 00:41:13 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://aquamacs.org/

;; Aquamacs Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Aquamacs Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;; Copyright (C) 2005, David Reitter

 

; remaining issues

; we need a proper font selection dialogue
; we need to go through Quartz / Carbon to print

; don't open scratch window at start
;; hide the frame
 

; this file needs cleaning up!
; move menu stuff to extra package
; move one-buffer-one-frame to extra mode



(defun aquamacs-osx-defaults-setup ()

  (require 'aquamacs-tools)

  (require 'mac-extra-functions)

  (mac-add-standard-directories)

 
  ;; load files (give full paths and load all files)
  ;; this will be called after .emacs has been loaded
  (add-hook 'after-init-hook   
	    '(lambda () 
	       (load "/Library/Preferences/Emacs/Preferences" t) 
	       (load "/Library/Preferences/Aquamacs Emacs/Preferences" t) 
	       (load "~/Library/Preferences/Emacs/Preferences" t) 
	       (load "~/Library/Preferences/Aquamacs Emacs/Preferences" t) 
	       ))

  (mac-read-environment-vars-from-shell)
  (mac-add-path-to-exec-path)
  (add-to-list 'Info-default-directory-list
	       (concat (mac-resources-path)
		       "info"))

  ;; inferior workaround, until mac.c is fixed not to set INFOPATH any longer
  (if (equal (concat (mac-resources-path)
		       "info")
	     (getenv "INFOPATH"))
      (setenv "INFOPATH"))
  ;; when INFOPATH is set from outside, it will only load INFOPATH

  (add-to-list 'Info-default-directory-list 
			     "~/Library/Application Support/Emacs/info")
  (add-to-list 'Info-default-directory-list 
			     "/Library/Application Support/Emacs/info")


  ;; emulate a three button mouse with Option / Control modifiers 
					; (setq mac-emulate-three-button-mouse t)
					; seems to prevent setting the secondary selection, so turned off for now

  ;; Mac creator

  (add-hook 'after-save-hook 'mac-set-creator-code-for-file)



  (require 'aquamacs-mule)              ; Language settings

					; Mac Drag-N-Drop

   (require 'mac-drag-N-drop)
					; this will disturb x-dnd... :-(
  (global-set-key [drag-n-drop] 'mac-drag-N-drop)

					; do this early, so we can override settings
  (require 'aquamacs-frame-setup)
					; one-on-one is called later
  (setq osxkeys-command-key 'hyper)
					; we have inhibit-fit-frame set to true... can't do this
					; (global-set-key [(control ?x) (control ?-)] 'fit-frame)
  (global-set-key `[(control ,osxkeys-command-key down)] 'enlarge-frame)
  (global-set-key `[(control ,osxkeys-command-key right)] 'enlarge-frame-horizontally)
  (global-set-key `[(control ,osxkeys-command-key up)] 'shrink-frame)
  (global-set-key `[(control ,osxkeys-command-key left)] 'shrink-frame-horizontally)
    

  (require 'aquamacs-mac-fontsets)
 
  (setq mac-allow-anti-aliasing t) 


  ;; -- KEYS AND MENUs ---------------------------

  ;; (require 'msb)
  ;; (msb-mode 1);; better buffer menu
 
					; os x like key bindings
  (require 'osxkeys)
  ;; turn on mac key mode by default

  (osx-key-mode 1) 

  (condition-case 
      nil 
      (progn (make-directory "~/Library/Preferences/Aquamacs Emacs")
	     ;; problem with this: could be started from /Volumes/.. (DMG) for first time, then moved		
	     (aquamacs-init-user-help) ;; init help system (first start)
	     )
    (error t)) 

  (aquamacs-set-defaults
   '((custom-file "~/Library/Preferences/Aquamacs Emacs/customizations.el")
     ;;   (user-init-file "~/Library/Preferences/Aquamacs Emacs/Preferences.el")
     )
   )

  (defun mac-is-mounted-volume-p (file)
    (if (string-match "/Volumes/.*" file ) t nil)
    )


  (require 'recentf)

  (aquamacs-set-defaults 
   '(
			
     (auto-save-list-file-prefix 
      "~/Library/Preferences/Aquamacs Emacs/auto-save-list/.saves-")
     ( save-place-file "~/Library/Preferences/Aquamacs Emacs/places.el")
     ( recentf-save-file "~/Library/Preferences/Aquamacs Emacs/Recent Files.el")))

  ) ;; aquamacs-osx-defaults-setup

 


(provide 'osx_defaults)
