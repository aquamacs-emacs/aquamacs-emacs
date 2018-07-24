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
 
;; Last change: $Id: osx_defaults.el,v 1.84 2009/02/10 21:31:49 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://aquamacs.org/

;; Aquamacs Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; Aquamacs Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;; Copyright (C) 2005, 2006,2007,2008,2010,2014,2018
;; David Reitter

 
; remaining issues

; we need to go through Quartz / Carbon to print

; this file needs cleaning up!
; move menu stuff to extra package

(eval-when-compile
  (require 'aquamacs-macros))

(defun aquamacs-create-preferences-dirs ()
  (condition-case err
      (progn 
	(unless (file-exists-p "~/Library/Preferences/Aquamacs Emacs")
	  (make-directory "~/Library/Preferences/Aquamacs Emacs" 'parents))
	(unless (file-exists-p "~/Library/Application Support/Aquamacs Emacs/Temporary Files")	
	  (make-directory "~/Library/Application Support/Aquamacs Emacs/Temporary Files" 'parents)))
    (error (message "Error %s during preference dir creation." err))))
  
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
;; evaluating this file and print errors in the *Messages* buffer.
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
      (condition-case nil
	  (load custom-file)
	(error (message "Loading `custom-file' failed.")))
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
			     (with-current-buffer (get-buffer-create "*Messages*")
                               (setq buffer-read-only nil)
                               (insert (propertize ;; unclear why propertize does not work.
                                (concat "\n\n"
				       (format "An error has occurred while loading `%s.el (or .elc)':\n\n"
					       user-init-file)
				       (format "%s%s%s"
					       (get (car error) 'error-message)
					       (if (cdr error) ": " "")
					       (mapconcat (lambda (s) (prin1-to-string s t)) (cdr error) ", "))
				       "\n\n"
				       "To ensure normal operation, you should investigate and remove the\n"
				       "cause of the error in your initialization file.  Start Emacs with\n"
				       "the `--debug-init' option to view a complete error backtrace.\n\n \n")
                                'face 'font-lock-warning-face)))
			     (message "Error in init file: %s%s%s"
				      (get (car error) 'error-message)
				      (if (cdr error) ": " "")
				      (mapconcat 'prin1-to-string (cdr error) ", "))
			     (let ((pop-up-windows nil))
			       (pop-to-buffer "*Messages*")
			       (goto-char (point-max)))
			     (setq init-file-had-error t)))
			  )))
		    aquamacs-preference-files))
      (aquamacs-activate-features-new-in-this-version)) t)

(defun mac-is-mounted-volume-p (file)
  (if (string-match "/Volumes/.*" file ) t nil))



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
			  (when (frcmds-frame-iconified-p f)
			    (select-frame f)
			    (make-frame-visible)
			    (throw 'exit 'found)))
			;; if current frame is iconified, use that
			;; if selected frame is not iconified, but hidden,
			;; then try to select an iconified frame
			(append (list (selected-frame))
				(frame-list)))))
      (let ((one-buffer-one-frame-mode nil))
	(switch-to-buffer "*scratch*")))))

(defun aquamacs-set-file-location-defaults ()
  (aquamacs-set-defaults 
   `((mailclient-place-body-on-clipboard-flag ,(gmail-mailclient-p))
     (recentf-menu-action aquamacs-find-file-2)
     (user-emacs-directory "~/Library/Preferences/Aquamacs Emacs/Packages/")
     (ede-simple-save-directory "~/Library/Preferences/Aquamacs Emacs/EDE")
     (savehist-file "~/Library/Preferences/Aquamacs Emacs/minibuffer-history.el")
     (desktop-path ("~/Library/Preferences/Aquamacs Emacs" "." "~"))
     (trash-directory "~/.Trash")
     (save-place-file "~/Library/Preferences/Aquamacs Emacs/places.el")
     (recentf-save-file "~/Library/Preferences/Aquamacs Emacs/Recent Files.el")
     (abbrev-file-name "~/Library/Preferences/Aquamacs Emacs/Abbreviations")
     (mail-default-directory 
      "~/Library/Application Support/Aquamacs Emacs/Temporary Files")))

  (condition-case nil
      (progn
	(make-directory user-emacs-directory t)
	(make-directory mail-default-directory t))
    (error nil))
  
  (defadvice locate-user-emacs-file  (after aquamacs-move-user-package-config (&rest args) activate)
    "If file is present in the previous Aquamacs-specific location, move it."
    (let ((orig (concat "~/Library/Application Support/Aquamacs Emacs/" (car args)))
	  (dest ad-return-value))
      (and (file-exists-p orig)
	   (not (file-exists-p dest))
	   (condition-case nil
	       (rename-file orig dest)
	     (error (message "Cannot move %s to %s" orig dest))))))

  ;; Before Aquamcas 3.1, user-emacs-directory was not in "Packages"
  ;; It was moved to the new location in order to avoid having
  ;; emacs-user-directory as part of load-path, which caused (load "tramp") to
  ;; load the wrong file.  GNU Emacs bug #18512

  (condition-case nil
      (progn
	(locate-user-emacs-file "tramp")
	(locate-user-emacs-file "calc.el")
	(locate-user-emacs-file "maxima_history")
	(locate-user-emacs-file "SessionDesktop.el")
	(write-region "" nil (concat user-emacs-directory ".nosearch")))
      (error nil))
  ) ;; aquamacs-file-location-defaults


(defun aquamacs-osx-defaults-setup ()

(ats "osx defaults setup running...")

  (require 'aquamacs-tools)
(ats "tools done")

  (aquamacs-set-file-location-defaults)


  (require 'mac-extra-functions)
(ats "mac-extra done")

(mac-read-environment-vars-from-shell) ;; starts reading the env vars
(ats "read env vars done")

;; init-file-user is set at this time;
;; user-init-file is normally not set.
;; see comment in startup.el
(when (or init-file-user user-init-file)
  (mac-add-standard-directories)
  (ats "add dirs done"))

;; create preferences files (even when starting with -q)
;; because subsequent operations may throw errors if those
;; directories don't exist.
(aquamacs-create-preferences-dirs)
(aquamacs-create-preferences-file)
(ats "create prefs done")

;; load files (give full paths and load all files)
;; this will be called after .emacs has been loaded
;;   (add-hook 'after-init-hook   
;; 	    'aquamacs-load-preferences
;; 	    'append)

  ;; this was no good, since users could not change after-init-hook any more.
  ;; now solved via a patch to startup.el.



;; POST-LOAD-PATH adjustment
;; from here on, the load path has been altered to include the user's 
;; own libraries (before our own).  Users may replace libraries
;; that we load using "require" and "load".


  (aquamacs-set-defaults `((frame-title-format
			    ,(if (eq initial-window-system 'ns)
				 t
			       "%b"))))

  ;; emulate a three button mouse with Option / Control modifiers 
  ;; (setq mac-emulate-three-button-mouse t)
  ;; seems to prevent setting the secondary selection, so turned off for now

  ;; Mac creator

  (add-hook 'find-file-hook 'mac-read-file-creator-and-type)
  (add-hook 'after-save-hook 'mac-set-creator-type-codes-for-file)
					

(ats "before frame setup")


  ;; do this early, so we can override settings
  (require 'aquamacs-frame-setup)
(ats "frame setup done")
    

  (aquamacs-set-defaults '((mac-allow-anti-aliasing t)))

  (if (boundp 'mac-pointer-I-beam) ;; undefined in TTY mode
      (aquamacs-set-defaults `((x-pointer-shape ,mac-pointer-I-beam))))

  (set-mouse-color "black") ;; to make x-pointer-shape work


  ;; NS specific defaults
  (aquamacs-set-defaults '(
			 (ns-use-qd-smoothing nil)
			 (ns-use-system-highlight-color t)
			 ))

  ;; -- KEYS AND MENUs ---------------------------

  ;; (require 'msb)
  ;; (msb-mode 1);; better buffer menu
 
					; os x like key bindings
  (require 'osxkeys)
  ;; turn on mac key mode by default
(ats "osx key loaded")

  (osx-key-mode 1) 

(ats "osx key done")

  ;; need to enforce a coding system (problems with Jap locale otherwise)
  (let ((coding-system-for-read 'utf-8))
    ;; not turned on by default                    
    (require 'emulate-mac-keyboard-mode) )  

  (require 'cus-edit) ;; because of some autoload weirdness 
  
   (aquamacs-set-defaults
    '((custom-file "~/Library/Preferences/Aquamacs Emacs/customizations.el")
      ))


(ats "before recentf...")

(require 'recentf)
(ats "recentf done")

  ;; create autosave directory if necessary
(defvar aquamacs-autosave-directory "~/Library/Caches/Aquamacs Emacs/AutoSave/"
  "Autosave directory.
Setting this has no effect w.r.t. where autosave files are written. 
Only used for purging.
Set `auto-save-file-name-transforms' instead.")

(protect
 (make-directory aquamacs-autosave-directory t))
(aquamacs-set-defaults
 `((auto-save-file-name-transforms
    (("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
      ;; Don't put "\\2" inside expand-file-name, since it will be
      ;; transformed to "/2" on DOS/Windows.
      ,(concat aquamacs-autosave-directory "\\2") t)
       ;; put all files into the tmp dir (it's user specific)
     ("\\`\\([^/]*/\\)*\\([^/]*\\)\\'"
      ,(concat aquamacs-autosave-directory "\\2") t)))
   ;; Auto save file names must look "unique"
   ;; so that they can be reliably recognized as auto save files.
   ;; (auto-save-file-name-prefix "")
   ;; (auto-save-file-name-postfix "")
   ;; leave this there for historical reasons (todo: reconsider)
   (auto-save-list-file-prefix  
    "~/Library/Preferences/Aquamacs Emacs/auto-save-list/.saves-")))

;; this is run at runtime - not during preloading
(run-with-timer (* 60 60) (* 60 60 24 3) 'purge-session-and-auto-save-files)


(when (and (boundp 'mac-apple-event-map) mac-apple-event-map)
  (define-key mac-apple-event-map [core-event reopen-application]
    'aquamacs-de-iconify-some-frame))
) ;; aquamacs-osx-defaults-setup

(provide 'osx_defaults)
