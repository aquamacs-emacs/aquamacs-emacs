; Mac extra functions
;;
;; Functions specific to use of Emacs on Mac OS X
;;

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: mac-extra-functions.el,v 1.43 2006/10/16 22:48:57 davidswelt Exp $

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

;; The following   need to be loaded at runtime. 


(defun aquamacs-mac-initialize  ()

  (defvar aquamacs-mac-application-bundle-directory
    (if (and (boundp 'load-file-name) load-file-name)
	(replace-regexp-in-string 
	 "/Contents/Resources/site-lisp" "" 
	 (directory-file-name (file-name-directory
			       (file-truename load-file-name))))
      "/Applications/Aquamacs Emacs.app")) ;; default
  )


(defun aquamacs-delete-temp-url-files ()
  (shell-command "rm -f /tmp/aquamacs-* 2>/dev/null" 'shut-up))

 


(defun browse-url-default-macosx-browser-via-redirection (url &optional new-window)
  "Opens a URL with the system's default browser.
If the URL points to a local file (file://), this will
open the file via redirection in order to ensure that the
file is actually opened with the browser, and not with the
application that happens to be handling .html files. 
As default browser, in that case, is assumed whatever application
handles files of type HTML."
  (interactive (browse-url-interactive-arg "URL: "))
  (if (not (string-match "file:/*\\(/.*\\)" url))
      (start-process (concat "open " url) nil "open" url)
    (let* ((file (match-string 1 url))
	   (newfile "/tmp/aquamacs-redirect-html.aquamacs-html"))
      ;;  (copy-file file newfile 'overwrite 'keeptime nil 'preserve)
      (let ((coding-system-for-write 'no-conversion)) 
	(write-region (format "<html><head><META HTTP-EQUIV=\"Refresh\" CONTENT=\"0; URL=%s\"></head><body></body></html>" url) nil newfile nil 'quiet ))
      (mac-set-file-type newfile "HTML")
      (start-process (concat "open " newfile) nil "open" newfile)
      (add-hook 'kill-emacs-hook 'aquamacs-delete-temp-url-files))))

(defun browse-url-safari (url &optional new-window)
   "Open URL in a new Safari window."
   (interactive (browse-url-interactive-arg "URL: "))
   (unless
       (string= ""
             (shell-command-to-string
              (concat "open -a Safari " url)))
     (message "Starting Safari...")
     (start-process (concat "open -a Safari " url) nil "open -a Safari " url)
     (message "Starting Safari... done")))



(defun mac-resources-path ()
  (substring data-directory 0 -4))

;; New documents
(defun new-frame-with-new-scratch  (&optional other-frame mode)
  "Opens a new frame containing an empty buffer."
  (interactive)			
  (let ((buf (generate-new-buffer (mac-new-buffer-name "untitled"))))
    ;; setting mode is done before showing the new frame
    ;; because otherwise, we get a nasty animation effect
    (save-excursion
      (set-buffer buf)
      (if (or mode default-major-mode)
	  (funcall  (or mode default-major-mode))))
    (if other-frame
	(switch-to-buffer-other-frame buf)
      (let ((one-buffer-one-frame-force one-buffer-one-frame-mode))
	;; force new frame
	(switch-to-buffer buf)))
    (setq buffer-offer-save t)
    (set-buffer-modified-p nil)))

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
 

(defun mac-key-open-file (filename)
  "Open a file, selecting file by dialog"
  (interactive
   (let ((last-nonmenu-event nil))
     (find-file-read-args "Open file: " t)))
  (find-file-existing filename))
(defun mac-key-open-file-other-frame (filename)
  "Open a file in new frame, selecting file by dialog"
  (interactive
   (let ((last-nonmenu-event nil))
     (find-file-read-args "Open file: " t)))
  (find-file-other-frame filename))

(defun mac-key-save-file ()
  (interactive)
  "Save buffer. If needed, select file by dialog"
   (if buffer-file-name 
       (save-buffer)
     (call-interactively (function mac-key-save-file-as))))
 
 
(defun mac-key-save-file-as (filename &optional wildcards )
  "Save buffer to a file, selecting file by dialog"
  (interactive
   (let ((last-nonmenu-event nil))
     (find-file-read-args "Save buffer to file: " nil)))
  (write-file filename))
 
  
  

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
	   (not (file-remote-p buffer-file-name)))
      (mac-set-file-creator buffer-file-name)))

;; (do-applescript (format "try
;; tell application \"Finder\"
;; set the creator type of POSIX file \"%s\" to \"EMAx\"
;; end tell
;; end try" buffer-file-name)]


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
			  (not (file-exists-p (concat xdir "/.ignore")))
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

(defvar shell-login-switch nil
"Command-line switch to be used with the shell to get a login shell.
If nil, a switch is automatically chosen depending on
`shell-file-name'.
This is relevant only for `mac-read-environment-vars-from-shell'.")

(defun aq-flat-concat (list)
  "Produces a list of all non-nil elements of list."
  (let ((c (car-safe list))
	(d (cdr-safe list)))
    (if c
	(if d
	    (cons c (aq-flat-concat d))
	  (list c))
      (if d
	  (aq-flat-concat d)
	nil))))
 
(defun mac-read-environment-vars-from-shell ()
"Import the environment from the system's default login shell
specified in `shell-file-name'."
    (with-temp-buffer
      ;; execute 'printenv' with the default login shell,
      ;; running the shell with -l (to load the environment)
      (setq default-directory "~/")	; ensure it can be executed
      ;; To Do: use call-process instead -> this here
      ;; will invoke two bashes
      
      (let ((shell-login-switch 
	     (or shell-login-switch 
		 (if (string-match ".*/\\(ba\\|z\\)sh" shell-file-name)
		     "-l"
		   (if (string-match ".*/\\tcsh" shell-file-name)
		     ""
		   (if (string-match ".*/ksh" shell-file-name)
		       "" ;; works for ksh
		     (message "Could not retrieve login shell environment with login shell: %s" shell-file-name)
		   ;; won't work for csh, because it doesn't take -l -c ...
		   ))))))
		    
	(call-process shell-file-name nil
	       t nil
	        shell-login-switch
		shell-command-switch
		"printenv"))
      (goto-char (point-min))
      (while (re-search-forward "^[A-Za-z_0-9]+=()\s*[^\x]*?
\s*}\s*$" nil t)
	(replace-match "..." nil nil))
      (goto-char (point-min))
      (while (search-forward-regexp "^\\([A-Za-z_0-9]+\\)=\\(.*\\)$" nil t)
	(setenv
	 (match-string 1)
	 (if (equal (match-string 1) "PATH")
	     (concat (getenv "PATH") ":" (match-string 2))
	     (match-string 2))))))

(defun mac-add-path-to-exec-path ()
  "Add elements from environment variable `PATH' to `exec-path'."
  (let ((l (split-string (getenv "PATH") ":")))
  (mapc
   (lambda (p)
     (unless (member p l)
       (nconc l (list p))))
   exec-path)
  (setq exec-path l)))


;; according to Apple's guidelines, we should
;; always go for "untitled", "untitled 2", ...
(defun mac-new-buffer-name (name &optional n)

  (if (not (get-buffer name))
      name
    (setq n (if n (+ n 1) 2))
    (let ((new-name (concat name " " (int-to-string n))))
      (if (not (get-buffer new-name))
	  new-name
	(mac-new-buffer-name name n)
	))
    )
  ) 
(defun aq-run-python-command (cmd)
  (let ((f (make-temp-file "emacs-command")))
    	(let ((coding-system-for-write 'no-conversion))
	  (write-region 
	   cmd nil f nil 'shut-up))
	  (let ((ret (call-process "/usr/bin/python" f)))
	    (if  (equal ret 0)  
		nil
	      (message "aq-run-python-command - Error")
	      ;; call again to produce error output
	      (call-process "/usr/bin/python" f t)
	      ret))
	  (ignore-errors (delete-file (car f)))))

;; register the help manuals
(defun aquamacs-init-user-help ()
  (if (condition-case nil 
	  (file-exists-p aquamacs-mac-application-bundle-directory) 
	(error nil))
      (aq-run-python-command
       (concat "from Carbon import AH; AH.AHRegisterHelpBook('" 
		 aquamacs-mac-application-bundle-directory "')"))
    ; else
    (message "Could not register Manual.
Aquamacs Emacs.app may have been moved or renamed. Please restart Aquamacs!")
  )
)


; Call up help book
(defun aquamacs-user-help ()
  (interactive)

  (aquamacs-init-user-help) ; make sure it's registered
  
  (and (aq-run-python-command
   "from Carbon import AH; AH.AHGotoPage('Aquamacs Help', None, None)")
      (message "Sorry, help function unavailable (python, OS problem?)")
  )
)
(defun aquamacs-emacs-manual ()
  (interactive)

  (aquamacs-init-user-help) ; make sure it's registered
 
  (and (aq-run-python-command
   "from Carbon import AH; AH.AHGotoPage('Emacs Manual', None, None)")
      (message "Sorry, help function unavailable (python, OS problem?)")
  )
)
  

;; it's imporant to make sure that the following are in the Info.plist file:
;; 	<key>CFBundleHelpBookFolder</key>
;; 	 <array>
;; 	   <string>Aquamacs Help</string>
;; 	   <string>Emacs Manual</string>
;; 	</array>
;; 	 <key>CFBundleHelpBookName</key>
;; 	 <array>
;; 	   <string>Aquamacs Help</string>
;; 	   <string>Emacs Manual</string>
;; 	</array>
;; it is vital that the folder name ("Aquamacs Help") is the same as
;; given above, and that it is also in a META tag in the help file.
;; spelling of the META tag (upper case) might be important.

; Call up help book
 
(defun aquamacs-show-change-log ()
  (interactive)
  (aquamacs-init-user-help) ; make sure it's registered
  (and (aq-run-python-command
   "from Carbon import AH; AH.AHGotoPage('Aquamacs Help', 'node4.html', None)")
      (message "Sorry, help function unavailable (python, OS problem?)"))) 

(provide 'mac-extra-functions)

