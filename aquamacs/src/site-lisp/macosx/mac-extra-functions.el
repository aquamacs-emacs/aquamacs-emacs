; Mac extra functions
;;
;; Functions specific to use of Emacs on Mac OS X
;;

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: mac-extra-functions.el,v 1.76 2008/09/20 03:05:10 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/


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
 
;; Copyright (C) 2005, 2006, 2007, David Reitter

;; The following  function needs to be loaded at runtime. 


(defun aquamacs-mac-initialize  ()
  (defvar aquamacs-mac-application-bundle-directory
    (if invocation-directory
	(replace-regexp-in-string 
	 "/Contents/MacOS" "" 
	 (directory-file-name (file-name-directory
			       (file-truename invocation-directory))))
      "/Applications/Aquamacs Emacs.app")
    "The path to the Aquamacs application bundle.")) ;; default


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
(defun new-empty-buffer-other-frame (&optional mode)
  "Opens a new frame containing an empty buffer."
  (interactive)
  (new-empty-buffer t mode))

(defun new-empty-buffer  (&optional other-frame mode)
  "Visits an empty buffer."
  (interactive)			
  (let ((buf (generate-new-buffer (mac-new-buffer-name "untitled"))))
    ;; setting mode is done before showing the new frame
    ;; because otherwise, we get a nasty animation effect
    (save-excursion
      (set-buffer buf)
      (if (or mode initial-major-mode)
	  (funcall  (or mode initial-major-mode))))
    (if other-frame
	(switch-to-buffer-other-frame buf)
      (let ((one-buffer-one-frame-force one-buffer-one-frame-mode))
	;; force new frame
	(switch-to-buffer buf)))
    (setq buffer-offer-save t)
    (set-buffer-modified-p nil)))
(defalias  'new-frame-with-new-scratch 'new-empty-buffer)

(defun aquamacs-find-file ()
"Open a new buffer. If `one-buffer-one-frame' is non-nil,
a new frame is opened to contain the new buffer. If find-file
leads to opening a dired buffer, newly opened files will open
right there as well."
  (interactive)

  (if (or (not one-buffer-one-frame)
	  tabbar-mode
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
 

(defun mac-key-open-file (filename &rest ignored)
  "Open a file, selecting file by dialog"
  (interactive
   (let ((last-nonmenu-event nil))
     (find-file-read-args "Open file: " t))) ;; may return list with two el
  (find-file-existing filename))

(defun mac-key-open-file-other-frame (filename &rest ignored)
  "Open a file in new frame, selecting file by dialog"
  (interactive
   (let ((last-nonmenu-event nil))
     (find-file-read-args "Open file: " t)))  ;; may return list with two el
  (find-file-other-frame filename))

(defun mac-key-save-file ()
  (interactive)
  "Save buffer. If needed, select file by dialog"
   (if buffer-file-name 
       (save-buffer)
     (call-interactively (function mac-key-save-file-as))))
 
 
(defun mac-key-save-file-as ()
  "Save buffer to a file, selecting file by dialog"
  (interactive)
  (let ((last-nonmenu-event nil))
    (call-interactively 'write-file)))

;; when saving a file, set its creator code

(defcustom aquamacs-set-creator-codes-after-writing-files t
  "Set creator and type when a file is written.
If t, the creator and type code of a file are set when it is
written. Visited files will retain their code, while new files
will be set to EMAx. If set to `force', the creator code is
always set to EMAx and the type code is always set to TEXT, no
matter what is was when the file was visited.  This way, Aquamacs will
open the files it writes when opened per double-click in
Finder. "
:type  '(radio (const :tag "Yes" t)
        (const :tag "No" nil)
        (other :tag "Always set to EMAx" force))
:group 'Aquamacs
;; :require mac-extra-functions
;; no require, because if set in customizations, it's set to nil
;; in which case not loading this package doesn't have a negative
;; effect
)

;; the following requires the non-Emacs function
;; mac-set-creator to be compiled in
(defvar mac-file-creator nil
  "Creator of file loaded in buffer (if any was set)")
(defvar mac-file-type nil
  "Type of file loaded in buffer (if any was set)")
 

;;  (add-hook 'find-file-hook 'mac-read-file-creator-and-type)
;;  (add-hook 'after-save-hook 'mac-set-creator-type-codes-for-file)
;; (mac-get-file-creator "~/aaa")

(defun mac-read-file-creator-and-type ()
  ;; initialize creator code for the file that was loaded.
  ;; called from `find-file-hook'
  (and buffer-file-name
     (not (file-remote-p buffer-file-name))
     (file-readable-p buffer-file-name) ;; do not set creator/type if file new
     (fboundp 'mac-get-file-creator)
     (let ((creator (mac-get-file-creator buffer-file-name))
	   (type (mac-get-file-type buffer-file-name)))
       (if (or (null creator) (equal creator "    "))
	   (set (make-local-variable 'mac-file-creator) 'none)
	 (set (make-local-variable 'mac-file-creator) creator))
       (if (or (null type) (equal type "    "))
	   (set (make-local-variable 'mac-file-type) 'none)
	 (set (make-local-variable 'mac-file-type) type)))))
 
(defun mac-set-creator-type-codes-for-file ()
  (when (and aquamacs-set-creator-codes-after-writing-files
	   buffer-file-name
	   (not (file-remote-p buffer-file-name))
	   (fboundp 'mac-set-file-creator) (fboundp 'mac-set-file-type))
    (cond
     ;; always set if configured so
     ((eq aquamacs-set-creator-codes-after-writing-files 'force)
      (mac-set-file-type buffer-file-name "TEXT"))
     ((eq mac-file-type 'none) nil) ;; do not set if not set originally
     ;; set to TEXT if a newly created file
     ;; or leave untouched otherwise
     (t (mac-set-file-type buffer-file-name (or mac-file-type "TEXT"))))

    (cond
     ((eq aquamacs-set-creator-codes-after-writing-files 'force)
      (mac-set-file-creator buffer-file-name "EMAx"))
     ((eq mac-file-creator 'none) nil)
     (t (mac-set-file-creator buffer-file-name (or mac-file-creator "EMAx"))))
  (mac-read-file-creator-and-type)))
 

;; (do-applescript (format "try
;; tell application \"Finder\"
;; set the creator type of POSIX file \"%s\" to \"EMAx\"
;; end tell
;; end try" buffer-file-name)]


;; copied here from osx-key-mode.el by Seiji Zenitani
;; modified to work with OS X 10.4 by David Reitter
(defun mac-key-show-in-finder (&optional file)
  "Show the open buffer in Finder"
  (interactive)
  (if (stringp (or file (buffer-file-name)))
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
                     (posix-file-name-to-mac (or file (buffer-file-name)))
                     selection-coding-system))
                 (encode-coding-string
                  (posix-file-name-to-mac (or file (buffer-file-name)))
                  selection-coding-system))
               ))
    (message "No existing file shown in buffer!")
    ))


(defvar aquamacs-mac-add-standard-directories-added-flag nil)
; (setq aquamacs-mac-add-standard-directories-added-flag nil)
; (setq normal-top-level-add-subdirs-inode-list nil)
; (mac-add-standard-directories)

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
				)		    
			    (error nil)))))

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
 
(defvar environment-temp-file nil)

;; (setq shell-file-name "/bin/bash")
;; (let ((debug-on-error)) (mac-read-environment-vars-from-shell))

;; Reading the environment variables is complex, primarily due to
;; bugs in OS X.  On some systems, starting the login shell and
;; printing all variables takes an hour, so we need to have a
;; timeout.  However, starting the process asynchronuously using
;; `start-process' fails as well on some other systems.  Hence the
;; need to run it with `call-process' and "&", storing the output in
;; a temporary file.
;; dr. 07/2008

(defun mac-read-environment-vars-from-shell ()
  "Import the environment from the system's default login shell
specified in `shell-file-name'."
  
  (setq environment-temp-file (make-temp-file "envvar-"))
  ;; running the shell with -l (to load the environment)
  (setq default-directory "~/")	; ensure it can be executed
  
  (message "Shell: %s" shell-file-name)

  (let* ((shell (or shell-file-name "/bin/bash"))   ;; can shell-file-name be nil?
	 (command (format "printenv >%s.tmp; mv %s.tmp %s"
			  environment-temp-file 
			  environment-temp-file 
			  environment-temp-file)))

    (if (string-match ".*/\\(ba\\|z\\)sh" shell)
	(call-process shell nil
		      0 nil
		      "-l" "-c" command)
      (if (or (string-match ".*/\\tcsh" shell)
	      (string-match ".*/ksh" shell))
	  (call-process shell nil
			0 nil
			;; we can't start tcsh as a login shell
			;; because it doesn't accept -l in combination
			;; with a command.
			;; call-process-region wouldn't work because it's
			;; not interactive.
			"-c" command)
	(message "Could not retrieve login shell environment with login shell: %s" shell)
	;; won't work for csh, because it doesn't take -l -c ...
	))))
;; we call the process asynchronuously
;; using start-process does not work for unknown reasons: 
;; sometimes it doesn't get the environment.

;; (mac-read-environment-vars-from-shell)
;; (sit-for 1)
;; (mac-read-environment-vars-from-shell-2)

(defun mac-read-environment-vars-from-shell-2 ()
  "Reads temporary file if it exists."
  (if (file-readable-p environment-temp-file)
      (prog1
	  (with-temp-buffer
	    (condition-case nil
		(progn
		  (insert-file-contents-literally environment-temp-file nil)
		  (delete-file environment-temp-file))
	      (error nil))
	    (let ((num 0))
	     (if (eq (buffer-size) 0)
		 (message "Warning: Login shell did not return environment.")
	       (goto-char (point-min))
	       (while (re-search-forward "^[A-Za-z_0-9]+=()\s*[^\x]*?
\s*}\s*$" nil t)
		 (replace-match "..." nil nil))
	       (goto-char (point-min))
	       (while (search-forward-regexp "^\\([A-Za-z_0-9]+\\)=\\(.*\\)$" nil t)
		 (setq num (1+ num))
		 (setenv
		  (match-string 1)
		  (if (equal (match-string 1) "PATH")
		      (concat (match-string 2) ":" (getenv "PATH"))
		    (match-string 2)))))
	  (message "%d environment variables imported from login shell (%s)." 
		   num shell-file-name)
	  (mac-post-environment-vars-function)
	  num)))
    nil))


(defun mac-post-environment-vars-function ()

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
					extra-dirs))))))


(defun mac-add-path-to-exec-path ()
  "Add elements from environment variable `PATH' to `exec-path'."
  (let ((l (split-string (getenv "PATH") ":")))
  (mapc
   (lambda (p)
     (unless (member p l)
       (nconc l (list p))))
   exec-path)
  (setq exec-path l)))

(defun mac-add-local-directory-to-exec-path ()
  "Add /usr/locaL/bin to `exec-path'"
  (add-to-list 'exec-path "/usr/local/bin"))

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
	(call-process "python" f (list (get-buffer "*Messages*") t))
	(condition-case nil 
	    (delete-file (car f))
	  (error nil))))

;; register the help manuals
(defun aquamacs-init-user-help ()
  (if (condition-case nil 
	  (file-exists-p aquamacs-mac-application-bundle-directory) 
	(error nil))
      (aq-run-python-command
       (concat "from Carbon import AH; AH.AHRegisterHelpBook('" 
	       aquamacs-mac-application-bundle-directory "')"))
    ;; else
    (message "Could not register Manual.
Aquamacs Emacs.app may have been moved or renamed. Please restart Aquamacs!")))

;; install start script

;; (defun aquamacs-install-shell-start-script ()

;;   (save-excursion

;;     (with-current-buffer 
;; 	(find-file-noselect 
;; 	 (concat aquamacs-mac-application-bundle-directory 
;; 		 "/Contents/Resources/etc/aquamacs-start.perl") 
;;      'nowarn 'rawfile))
    
;;     (if (search-forward "<AQUAMACS-PATH>" nil )
;; 	(replace-match (replace-regexp-in-string " " "\\\\ " aquamacs-mac-application-bundle-directory) t t))
  
;;     ;; how do we sudo?

;; ))
  
(defun aq-show-help-file (file &optional book)
  (aquamacs-init-user-help) ; make sure it's registered
  (if file
      (and
       (aq-run-python-command
	(format "from Carbon import AH; AH.AHGotoPage(None, '%s/Contents/Resources/%s/%s', None)" 
		aquamacs-mac-application-bundle-directory		      
		(or book "Aquamacs Help")
		file))
       (message "Sorry, change log unavailable (python, OS problem?)"))
    (message "Sorry, change log unavailable (not found)")))

; Call up help book
(defun aquamacs-user-help ()
  "Show the Aquamacs Help."
  (interactive)
  (aq-show-help-file "index.html"))

(defun aquamacs-emacs-manual ()
  "Show the Emacs Manual"
  (interactive)
  (aq-show-help-file "index.html" "Emacs Manual"))

(defun aquamacs-elisp-reference ()
  (interactive)
  (aq-show-help-file "index.html" "Emacs Lisp Reference"))


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
; (aquamacs-show-change-log)
(defun aquamacs-show-change-log ()
  (interactive)
  (aquamacs-init-user-help) ; make sure it's registered
  ;; check node1.html
  ;; relativ complex check because of bug in 10.5.2 that causes crashes
  ;; when we use a symlinked file.
  (let ((change-log-file
	 (with-temp-buffer
	   (insert-file-contents (format "%s/Contents/Resources/Aquamacs Help/node1.html" 
					 aquamacs-mac-application-bundle-directory))
	   (let ((case-fold-search t))
	     (re-search-forward "HREF=\"\\(node[0-9]+.html\\)\">\s*Changes")
	     (match-string 1)))))
    (aq-show-help-file change-log-file)))


(provide 'mac-extra-functions)