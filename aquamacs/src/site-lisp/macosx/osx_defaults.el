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
 
;; Last change: $Id: osx_defaults.el,v 1.62 2007/11/14 23:34:55 davidswelt Exp $

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
 
;; Copyright (C) 2005, 2006 David Reitter

 

; remaining issues

; we need a proper font selection dialogue
; we need to go through Quartz / Carbon to print

; don't open scratch window at start
;; hide the frame
 

; this file needs cleaning up!
; move menu stuff to extra package
; move one-buffer-one-frame to extra mode


(defun aquamacs-create-preferences-file ()
  "Creates a Preferences.el in the right place if needed."
  (let ((pf (expand-file-name 
	     "~/Library/Preferences/Aquamacs Emacs/Preferences")))
    (unless (or (file-readable-p (concat pf ".elc"))
		(file-readable-p (concat pf ".el")))
          (condition-case nil 
      (with-temp-file (concat pf ".el")
	(insert ";; This is the Aquamacs Preferences file.
;; Add Emacs-Lisp code here that should be executed whenever
;; you start Aquamacs Emacs. If errors occur, Aquamacs will stop
;; evaluating this file and print errors in the *Messags* buffer.
;; Use this file in place of ~/.emacs (which is loaded as well.)

"))  
      ;; fail silently if file can't be written.
      (error nil)))))

(defvar aquamacs-preference-files
  '("/Library/Preferences/Emacs/Preferences" 
			  "/Library/Preferences/Aquamacs Emacs/Preferences"
			  "~/Library/Preferences/Emacs/Preferences"
			  "~/Library/Preferences/Aquamacs Emacs/Preferences")
  "List of names of source files to be loaded as Preference files.")

;; (setq init-file-debug nil)
;; (aquamacs-load-preferences)
(defun aquamacs-load-preferences ()
    "Loads the custom and preference files.

The files listed in the variables `custom-file' and
`aquamacs-preference-files' are loaded. If errors occur,
*Messages* is shown containing a helpful error message.
Aquamacs also executes compatibility code to allow transitions
from earlier versions of the distribution."
    (interactive)
    (unless (equal init-file-user nil) ;; no .emacs was read (-q option)
	(if init-file-debug
	    ;; Do this without a condition-case if the user wants to debug.
	    (mapc (lambda (file)
		    (let ((user-init-file file))
		      (load user-init-file t)))
		  aquamacs-preference-files)
	      (mapc (lambda (file)
		      	  
		      (let ((user-init-file file))
			(condition-case error 
			    (load user-init-file t)
			  (error ;; this code is from startup.el
			   (let ((message-log-max nil))
			     (save-excursion
			       (set-buffer (get-buffer-create "*Messages*"))
			       (insert "\n\n"
				       (format "An error has occurred while loading `%s.el (or .elc)':\n\n"
					       user-init-file)
				       (format "%s%s%s"
					       (get (car error) 'error-message)
					       (if (cdr error) ": " "")
					       (mapconcat (lambda (s) (prin1-to-string s t)) (cdr error) ", "))
				       "\n\n"
				       "To ensure normal operation, you should investigate and remove the\n"
				       "cause of the error in your initialization file.  Start Emacs with\n"
				       "the `--debug-init' option to view a complete error backtrace.\n\n"))
			     (message "Error in init file: %s%s%s"
				      (get (car error) 'error-message)
				      (if (cdr error) ": " "")
				      (mapconcat 'prin1-to-string (cdr error) ", "))
			     (let ((pop-up-windows nil))
			       (pop-to-buffer "*Messages*")
			       (end-of-buffer))
			     (setq init-file-had-error t)))
			  )))
		    aquamacs-preference-files))
      ;; the customization file is loaded even if the Preferences fail.
      (condition-case nil
	  (load custom-file)
	(error (message "Loading `custom-file' failed.")))
      (aquamacs-activate-features-new-in-this-version)) t)

(defun aquamacs-osx-defaults-setup ()

  (require 'aquamacs-tools)

  (require 'mac-extra-functions)

  (mac-add-standard-directories)

 
  (aquamacs-create-preferences-file)

  ;; load files (give full paths and load all files)
  ;; this will be called after .emacs has been loaded
;;   (add-hook 'after-init-hook   
;; 	    'aquamacs-load-preferences
;; 	    'append)

  ;; this was no good, since users could not change after-init-hook any more.
  ;; now solved via a patch to startup.el.


  (mac-read-environment-vars-from-shell)
  (mac-add-path-to-exec-path)
  (mac-add-local-directory-to-exec-path) ;; needed for CocoAspell

  ;; inferior workaround, until mac.c is fixed not to set INFOPATH any longer
  (if (equal (concat (mac-resources-path)
		       "info")
	     (getenv "INFOPATH"))
      (setenv "INFOPATH"))
  
;; when INFOPATH is set from outside, it will only load INFOPATH

  (let ((extra-dirs (list
		     "~/Library/Application Support/Emacs/info"
		     "/Library/Application Support/Emacs/info"
		     (concat (mac-resources-path)
			     "site-lisp/edit-modes/info")
		     (concat (mac-resources-path)
			     "info"))))
    
    (setq Info-default-directory-list (append extra-dirs
				       Info-default-directory-list
				       ))
    (when (getenv "INFOPATH")
      (setenv "INFOPATH" (apply 'concat (getenv "INFOPATH")
				(mapcar (lambda (x) (concat ":" x))
					extra-dirs)))))
 
;



  (aquamacs-set-defaults '(( frame-title-format "%b")))
 
  ;; emulate a three button mouse with Option / Control modifiers 
  ;; (setq mac-emulate-three-button-mouse t)
  ;; seems to prevent setting the secondary selection, so turned off for now

  ;; Mac creator

  (add-hook 'find-file-hook 'mac-read-file-creator-and-type)
  (add-hook 'after-save-hook 'mac-set-creator-type-codes-for-file)
					

  (unless (boundp 'unicode-emacs)
    (require 'aquamacs-mule)              ; Language settings
    )

  ;; do this early, so we can override settings
  (require 'aquamacs-frame-setup)
  ;; one-on-one is called later
  (setq osxkeys-command-key (or (if (boundp 'mac-command-modifier)
				    mac-command-modifier nil) 'alt))
  ;; we have inhibit-fit-frame set to true... can't do this
  ;; (global-set-key [(control ?x) (control ?-)] 'fit-frame)
  ;; use vector, not [...] in order to not allocate at load-time 
  ;; in pure-space
  (global-set-key (vector  `(,osxkeys-command-key control down)) 
		  'enlarge-frame)
  (global-set-key (vector  `(,osxkeys-command-key control right)) 
		  'enlarge-frame-horizontally)
  (global-set-key (vector  `(,osxkeys-command-key control up)) 
		  'shrink-frame)
  (global-set-key (vector  `(,osxkeys-command-key control left)) 
		  'shrink-frame-horizontally)
  

    

  (require 'aquamacs-mac-fontsets)
 
  (setq mac-allow-anti-aliasing t) 


  ;; -- KEYS AND MENUs ---------------------------

  ;; (require 'msb)
  ;; (msb-mode 1);; better buffer menu
 
					; os x like key bindings
  (require 'osxkeys)
  ;; turn on mac key mode by default

  (osx-key-mode 1) 


  ;; need to enforce a coding system (problems with Jap locale otherwise)
  (let ((coding-system-for-read 'iso-latin-1-unix))
    ;; not turned on by default                    
    (require 'emulate-mac-keyboard-mode) )  


  (condition-case 
      nil 
      (progn 
	(make-directory "~/Library/Preferences/Aquamacs Emacs")
	(make-directory "~/Library/Application Support/Aquamacs Emacs")
	(make-directory "~/Library/Application Support/Aquamacs Emacs/Temporary Files")
	;; problem with this: could be started from /Volumes/.. (DMG) for first time, then moved		
	(aquamacs-init-user-help) ;; init help system (first start)
	)
    (error t)) 

  (require 'cus-edit) ;; because of some autoload weirdness 
  
   (aquamacs-set-defaults
    '((custom-file "~/Library/Preferences/Aquamacs Emacs/customizations.el")
      ))

  (defun mac-is-mounted-volume-p (file)
    (if (string-match "/Volumes/.*" file ) t nil)
    )


  (require 'recentf)

  ;; create temporary directory if necessary
  

  (aquamacs-set-defaults 
   '(
			
     (auto-save-list-file-prefix 
      "~/Library/Preferences/Aquamacs Emacs/auto-save-list/.saves-")
     ( save-place-file "~/Library/Preferences/Aquamacs Emacs/places.el")
     ( recentf-save-file "~/Library/Preferences/Aquamacs Emacs/Recent Files.el")
     ( abbrev-file-name "~/Library/Preferences/Aquamacs Emacs/Abbreviations")
     ( mail-default-directory "~/Library/Application Support/Aquamacs Emacs/Temporary Files")))



  ;; de-iconify
  ;; when application gains focus, de-iconfiy


  (defun aquamacs-de-iconify-some-frame (event)
    (interactive "e")
    ;; run AFTER unhiding any hidden frames - we don't want
    ;; to de-minimize something in addition to that
    (run-with-idle-timer 0 nil 'aquamacs-de-iconify-some-frame-1))

  (defun aquamacs-de-iconify-some-frame-1 ()
     
    (unless (visible-frame-list)
	(unless (eq 'found
		    (catch 'exit 
		      (mapc (lambda (f)
			      (when (frame-iconified-p f)
				(select-frame f)
				(make-frame-visible)
				(throw 'exit 'found)))

			    ;; if current frame is iconified, use that
			    ;; if selected frame is not iconified, but hidden,
			    ;; then try to select an iconified frame
			    (append (list (selected-frame))
				    (frame-list)))))
	  ;; this should work with and without one-buffer-one-frame
	  (switch-to-buffer "*scratch*"))))

    (when (and (boundp 'mac-apple-event-map) mac-apple-event-map)
      (define-key mac-apple-event-map [core-event reopen-application ]
	'aquamacs-de-iconify-some-frame))
  ) ;; aquamacs-osx-defaults-setup

 


(provide 'osx_defaults)
