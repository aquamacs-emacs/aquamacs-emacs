; Mac extra functions
;;
;; Functions specific to use of Emacs on Mac OS X
;;

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: mac-extra-functions.el,v 1.18 2005/08/20 09:46:43 davidswelt Exp $

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




(defun aquamacs-find-file ()
"Open a new buffer. If `one-buffer-one-frame' is non-nil,
a new frame is opened to contain the new buffer. If find-file
leads to opening a dired buffer, newly opened files will open
right there as well."
  (interactive)

  (if (or (not one-buffer-one-frame)
	  (< (buffer-size (window-buffer (frame-first-window))) 2))
      (call-interactively 'find-file)

    ;; open new frame with empty buffer
    (let ((default-major-mode 'fundamental-mode))
      (new-frame-with-new-scratch nil) ;;  'fundamental-mode
    )
    (let ((buf (current-buffer)))
      (unwind-protect 
	  (progn 
	    ;; the following will open the file in the given
	    ;; frame, because the buffer shown is empty.
	    (call-interactively 'find-file)
	    (unless (eq (current-buffer) buf) ; get rid of old buffer
	      (kill-buffer buf)))
	    ;;(setq one-buffer-one-frame t))	
	(progn 
;	  (if (assq 'one-buffer-one-frame (frame-parameters nil) )
	      ;; this doesn't work - only works for buffers
	 ;     (kill-local-variable 'one-buffer-one-frame)
	;       )
 
	(when (eq major-mode 'dired-mode)
	    (set (make-local-variable 'one-buffer-one-frame) nil)
	    )
	(when (and (buffer-live-p buf)
		 (< (buffer-size) 2))		; for security
	    (kill-buffer buf))
	)))))

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

;; the following requires the non-standard function
;; mac-set-creator to be compiled in
(defun mac-set-creator-code-for-file ()
  (if (and aquamacs-set-creator-codes-after-writing-files
	   buffer-file-name
	   (fboundp 'mac-set-file-creator)
	   )
      (mac-set-file-creator buffer-file-name)
    )
  )

;; (do-applescript (format "try
;; tell application \"Finder\"
;; set the creator type of POSIX file \"%s\" to \"EMAx\"
;; end tell
;; end try" buffer-file-name)


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


(defvar aquamacs-mac-add-standard-directories-added-flag nil)
(defun mac-add-standard-directories ()
  ;; Add standard directories and automatically add their subdirectories.
  ;; this idea blatantly copied and adapted from Martin Schwenke (meltin.net)
  (if (not aquamacs-mac-add-standard-directories-added-flag)
      (let ((ddir default-directory))
	(setq aquamacs-mac-add-standard-directories-added-flag t)
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
				)		    (error nil)))))

		'("/Library/Application Support/Emacs"
					;"/Library/Application Support/Emacs/site-lisp"
		  "/Library/Application Support/Aquamacs Emacs"
		  "~/Library/Application Support/Emacs"
					;"~/Library/Application Support/Emacs/site-lisp"
		  "~/Library/Application Support/Aquamacs Emacs"
		  "/Library/Preferences/Emacs"	    ; for all Emacsen
		  "/Library/Preferences/Aquamacs Emacs" ; for Aquamacs
		  "~/Library/Preferences/Emacs"	; for all Emacsen (user-specific):
		  "~/Library/Preferences/Aquamacs Emacs" ; for Aquamacs (user-specific)
		  ))
	(setq default-directory ddir)	; restore
	)))

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

;; according to Apple's guidelines, we should
;; always go for "untitled", "untitled 2", ...
(defun mac-new-buffer-name (name &optional n)

  (if (not (get-buffer name))
      name
    (setq n (if n (+ n 1) 2))
    (setq new-name (concat name " " (int-to-string n)))
    (if (not (get-buffer new-name))
	new-name
      (mac-new-buffer-name name n)
      )
    )
)




;; register the help manuals
(defun aquamacs-init-user-help ()
  (if (condition-case nil 
	  (file-exists-p (car command-line-args)) 
	(error nil))
      (shell-command (concat "python -c \"from Carbon import AH; AH.AHRegisterHelpBook('" (substring (car command-line-args) 0 -30) "')\" >/dev/null 2>/dev/null") t t) 
    ; else
    (message "Aquamacs Emacs.app has been moved or renamed. Please restart Aquamacs!")
  )
)


; Call up help book
(defun aquamacs-user-help ()
  (interactive)

  (aquamacs-init-user-help) ; make sure it's registered
 
  (or (shell-command "python -c \"from Carbon import AH; AH.AHGotoPage('Aquamacs Help', None, None)\"  >/dev/null 2>/dev/null" t t)
      (message "Sorry, help function unavailable (python, OS problem?)")
  )
)
(defun aquamacs-emacs-manual ()
  (interactive)

  (aquamacs-init-user-help) ; make sure it's registered
 
  (or (shell-command "python -c \"from Carbon import AH; AH.AHGotoPage('Emacs Manual', None, None)\"  >/dev/null 2>/dev/null" t t)
      (message "Sorry, help function unavailable (python, OS problem?)")
  )
)
 
(provide 'mac-extra-functions)

