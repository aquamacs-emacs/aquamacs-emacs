;; check-for-updates.el
;; Checks for updates to Aquamacs - queries a remote server every 3 days.
;; If new version found, a new check will be forced next time 
;; (to show message again!)
;; Stores file .id in a folder 

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs version check
 
;; Last change: $Id: check-for-updates.el,v 1.4 2005/06/20 22:43:51 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/
;; Emacs Version: 22.0

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

;
;
; the following is user-settable (to "")
(defvar aquamacs-version-check-url "http://aquamacs.sourceforge.net/cgi-bin/currentversion.cgi" "URL to check for updates.  ")


(defun aquamacs-check-version-information ()
  (interactive) 
;(switch-to-buffer (get-buffer-create  " *Aquamacs Privacy* "))
(with-output-to-temp-buffer (help-buffer)

(print "
        Aquamacs - Version update check
 
        Aquamacs automatically checks for updates and notifies the user
	if there's something new. 
	Your privacy:  While checking for a new version, Aquamacs
	contacts an internet server to get the version number - this
	happens once every 3 days. The server will receive and 
	store the following anonymous connection data: 
	an anonymous ID, the number of program starts and the 
	version of Aquamacs that you're using. Like every web server, 
	your IP address and the time of your inquiry may be stored,
	too. This information is used to produce statistics; we delete the
	original data after a period of time. The statistics solely
	inform the Aquamacs development. 

	If you like to turn this check off, add this to your file
	~/Library/Preferences/Aquamacs Emacs/Preferences.el:

	(setq aquamacs-version-check-url nil)
"))
nil  
)




; the following is internal - shouldn't be set 
(setq aquamacs-id-file "~/Library/Preferences/Aquamacs Emacs/.id")
(setq aquamacs-user-likes-beta nil)
(setq aquamacs-version-check-buffer nil) 
(setq url-show-status nil) ;;don't annoy user
(defun aquamacs-compare-version ()

  (if aquamacs-version-check-buffer ;; just for safety
      (save-excursion 
	(set-buffer aquamacs-version-check-buffer)
 
	(if (> aquamacs-user-likes-beta 0) ; user likes beta versions?
	    (re-search-forward "<beta-version>\\(.*\\)</beta-version>" (buffer-end 1) t)
	  (re-search-forward "<version>\\(.*\\)</version>" (buffer-end 1) t)
	  )
 
	(if (equal (match-string 1) aquamacs-version)
	    (message "")	;; up-to-date; workaround for "Saw end of trailers" bug
	  (progn
	    (write-region (concat "888\n") ;; notice that a new version is available 
		  nil
		  aquamacs-id-file
		  'append 
		  'shut-up
		  nil
		  nil)
	    (aquamacs-new-version-notify (match-string 1))

	    
	  )
)

	        
	)
    )
  )
 
(defun aquamacs-new-version-notify (v)
  (message (format "Get the new Aquamacs %s from http://aquamacs.org" v))
)



(defvar aquamacs-check-update-time-period 3
  "Time to wait between online checks for update.")

(defun aquamacs-check-for-updates-if-necessary ()
  "Check (periodically) if there's an update for Aquamacs available, 
and show user a message if there is."
  (let ( (force-check)
	  
	( call-number 0)
	( session-id (random t) )
	( today (date-to-day (current-time-string)))
	( last-update-check 0) )

    (if (file-exists-p aquamacs-id-file)
	(with-temp-buffer
	  (insert-file-contents-literally aquamacs-id-file)
	    ; (set-buffer buf)
	    ; (buffer-string)
	    (goto-char (point-min)) 
	    (setq call-number (number-at-point) )
	    (goto-line 2)
	    (setq last-update-check (number-at-point))
	    (goto-line 3)
	    (setq session-id (number-at-point))
	    (goto-line 4)
	    (setq aquamacs-user-likes-beta (number-at-point))
	    (goto-line 5)
	    (setq force-check (eq 888 (number-at-point))) ;; contains 888 if new version previously found
	    ; (kill-buffer buf)
	    )
	  
      )
  
    (if (string-match "beta"  aquamacs-version)
	     (setq aquamacs-user-likes-beta 1)
      ) 
 
    (if (or force-check (>= (- today last-update-check)  aquamacs-check-update-time-period))
	(progn
	  (aquamacs-check-for-updates session-id call-number)
	  (setq last-update-check today)
	  )
      )
        (write-region (concat (number-to-string (+ 1 call-number)) "\n"
			  (number-to-string last-update-check) "\n"
			  (number-to-string session-id) "\n"
			   (number-to-string aquamacs-user-likes-beta ) "\n"
			  )
 
		  nil
		  aquamacs-id-file
		  nil 
		  'shut-up
		  nil
		  nil)

					; store file
    
		  
    )

  )


    
 

(defun aquamacs-check-for-updates (session-id calls)
 
 
    (if aquamacs-version-check-url
 
	(condition-case nil
	    (let ((url (url-generic-parse-url 
			 (concat aquamacs-version-check-url
				 "?sess=" (number-to-string session-id) 
				 "&seq=" (number-to-string calls)
				 "&beta=" (number-to-string aquamacs-user-likes-beta) 
				 "&ver=" (url-encode-string aquamacs-version)
				 ) 
			 )))
	; HTTP-GET
	(setq aquamacs-version-check-buffer   
	      (url-http url 
			'aquamacs-compare-version  nil ))

	; now make sure that the Emacs won't ask to kill this 
	; process when quitting
	(dolist ( p (process-list))
	  (if (string-match (elt url 3) (process-name p))
	      (set-process-query-on-exit-flag p nil)
	    )
	  )
	)
	(error nil))
      )
    
  nil

  )

; (aquamacs-check-for-updates-if-necessary)

(provide 'check-for-updates)

; (url-http-debug "asd")

 