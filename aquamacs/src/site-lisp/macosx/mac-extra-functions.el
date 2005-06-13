; Mac extra functions
;;
;; Functions specific to use of Emacs on Mac OS X
;;

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: mac-extra-functions.el,v 1.6 2005/06/13 22:47:16 davidswelt Exp $

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






;; File Open / Save
;; TO DO: these should be replaced with the file menu item 
;; can't do this because the internal find-file function will
;; display a file dialogue only if menu was used w/ mouse
 
(defun mac-key-open-file (filename &optional wildcards)
  "Open a file using standard file open dialog."
  (interactive
   (let ((last-nonmenu-event nil))
     (find-file-read-args "Find existing file: " t)))
  (find-file-existing filename wildcards)
  )
 

 (defun mac-save-file-as ()
   (interactive)
   (let ((file (do-applescript "try
 POSIX path of (choose file name with prompt \"Save As...\")
 end try")))
     (if (> (length file) 3)
         (setq file
               (substring file 1 (- (length file) 1))
               ))
     (if (not (equal file ""))
         (write-file file)
       (beep))
     ))
  

;; when saving a file, set its creator code

(defcustom aquamacs-set-creator-codes-after-writing-files t
  "If t, the creator code of a file is set after it is written.
This way, Emacs will open the files it writes when opened per
double-click in Finder."
:type 'boolean
:group 'Aquamacs
;; :require mac-extra-functions
;; no require, because if set in customizations, it's set to nil
;; in which case not loading this package doesn't have a negative
;; effect
)

(defun mac-set-creator-code ()
  (if aquamacs-set-creator-codes-after-writing-files
      (if buffer-file-name ;; added security
	  (do-applescript (format "try
tell application \"Finder\"
set the creator type of POSIX file \"%s\" to \"EMAx\"
end tell
end try" buffer-file-name)
			  )
	t
	)
    )
  )
(add-hook 'after-save-hook 'mac-set-creator-code)

;; copied here from osx-key-mode.el by Seiji Zenitani
;; modified to work with OS X 10.4 by David Reitter
(defun mac-key-show-in-finder ()
  "Show the open buffer in Finder"
  (interactive)
  (if (stringp (buffer-file-name))
      (do-applescript
       (format "
tell application \"Finder\"
  activate
  try
    select file \"%s\" of startup disk
  on error
    beep
  end try
end tell" 
               (if (eq selection-coding-system 'sjis-mac)
                   (replace-regexp-in-string
                    "\\\\" "\\\\\\\\"
                    (encode-coding-string
                     (posix-file-name-to-mac (buffer-file-name))
                     selection-coding-system))
                 (encode-coding-string
                  (posix-file-name-to-mac (buffer-file-name))
                  selection-coding-system))
               ))
    (message "No existing file shown in buffer!")
    ))



(defun mac-add-standard-directories ()
;; Add standard directories and automatically add their subdirectories.
; this idea blatantly copied and adapted from Martin Schwenke (meltin.net)
(let ((ddir default-directory))
(mapcar '(lambda (dir)
	   (let* ((xdir (expand-file-name dir)  )
		  (default-directory xdir)) 
	     (and xdir
		  (add-to-list 'load-path xdir)
		  ;; Now add subdirectories.
		  
		  (condition-case nil
		      (progn
			(cd xdir)
		      (normal-top-level-add-subdirs-to-load-path)
		      )		    (error nil))
		  )
	     )
	   )

	'("/Library/Application Support/Emacs"
	  ;"/Library/Application Support/Emacs/site-lisp"
	  "/Library/Application Support/Aquamacs Emacs"
	  "~/Library/Application Support/Emacs"
	  ;"~/Library/Application Support/Emacs/site-lisp"
	  "~/Library/Application Support/Aquamacs Emacs"
	  "/Library/Preferences/Emacs"	; for all Emacsen
	  "/Library/Preferences/Aquamacs Emacs"	; for Aquamacs
	  "~/Library/Preferences/Emacs"	; for all Emacsen (user-specific):
	  "~/Library/Preferences/Aquamacs Emacs" ; for Aquamacs (user-specific)
	  )
)
(setq default-directory ddir) ; restore
)
)

(defun mac-read-environment-vars-from-shell ()

; Get the environment from the default shell
; this helps to get apps to run under 10.3
; and under 10.4 if ~/.bash_profile is changed before restart
    (with-temp-buffer
      ;; execute 'set' with bash. bash is invoked from the 
      ;; user's default shell (whatever that is - probably bash as well)
      ;; so it should get all the environment variables.
      (setq default-directory "~/")	; ensure it can be executed
      (shell-command "/bin/bash -l -c printenv" t)

	   ; the following is elegant, but insecure
	   ; (query-replace-regexp "^\\([A-Za-z_0-9]+\\)=\\(.*\\)$" 
           ;   "(setenv \"\\1\" \"\\2\")")
	   ; (eval-buffer)

      (while (search-forward-regexp "^\\([A-Za-z_0-9]+\\)=\\(.*\\)$" nil t)
	;(print (format "%s=%s" (match-string 1) (match-string 2)) )
	(setenv
	 (match-string 1)
	 (if (equal (match-string 1) "PATH")
	     (concat (getenv "PATH") ":" (match-string 2))
	     (match-string 2)
	     )
	 )
	) 
      )
    
)

(defun new-frame-with-new-scratch  ()
  "Opens a new frame containing an empty buffer in ``text-mode'' and ``filladapt-mode''."
  (interactive)			
  (let ((buf (generate-new-buffer "New document")))

    ;; setting mode is done before showing the new frame
    ;; because otherwise, we get a nasty animation effect
    (save-excursion
      (set-buffer buf)
      (text-mode)
      (filladapt-mode t)
     )

  (switch-to-buffer-other-frame buf)
 
  
  (setq buffer-offer-save t)
  (set-buffer-modified-p nil)
  )
)


(provide 'mac-extra-functions)

