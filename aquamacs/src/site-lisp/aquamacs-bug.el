
;; Author: adapted from emacsbug.el by K. Shane Hartman
;; Maintainer: David Reitter
;; Keywords: mac bug report
 
;; Last change: $Id: aquamacs-bug.el,v 1.2 2005/06/09 19:52:49 davidswelt Exp $

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

;; Copyright (C) 1985, 1994, 1997, 1998, 2000, 2001, 2002
;; Free Software Foundation, Inc.

;; Copyright (C) 2005, David Reitter


(setq report-aquamacs-bug-address "aquamacs-bugs@lists.sourceforge.net")


(require 'emacsbug)


(defun report-aquamacs-emacs-bug ( &optional recent-keys)
  "Report a bug in Aquamacs on OS X using the default mail agent. The standard report-emacs-bug function doesn't work unless sendmail/postfix is enabled."
   
  (interactive)	 
  (let ((pretest-p (string-match "\\..*\\..*\\." emacs-version))
	(from-buffer (current-buffer))
	user-point prompt-beg-point message-end-point)
      
    (setq message-end-point
	  (with-current-buffer (get-buffer-create "*Messages*")
	    (point-max-marker)))
    (setq str (with-output-to-string  
 
	 

					;   (compose-mail (if pretest-p
					;	      report-emacs-bug-pretest-address
					;    report-emacs-bug-address)
					; topic)
		;; The rest of this does not execute
		;; if the user was asked to confirm and said no.
					;(rfc822-goto-eoh)
					;(forward-line 1)

		(let ((signature (buffer-substring (point) (point-max))))
					;(delete-region (point) (point-max))
					;(princ signature)
					; (backward-char (length signature)))
					;  (setq prompt-beg-point (point))
		  (unless report-emacs-bug-no-explanations
		    ;; Insert warnings for novice users.
		    (princ "This bug report will be sent to the Free Software Foundation, not to your local site managers! Please write in English if possible, because the Emacs maintainers usually do not have translators to read other languages for them.\n\n")
		    (princ (format "Your bug report will be posted to the %s mailing list"
				   (if pretest-p
				       report-emacs-bug-pretest-address
				     report-emacs-bug-address)))
		    (if pretest-p
			(princ ".\n\n")
		      (princ ",\nand to the gnu.emacs.bug news group.\n\n")))

		  (princ "Please give this e-mail an appropriate subject line!\n\n")

		  (princ "Please describe exactly what actions triggered the bug and the precise symptoms of the bug:\n\n\n\n\n\n\n")
					; (setq report-emacs-bug-text-prompt
					;	  (buffer-substring prompt-beg-point (point)))

					;  (princ "\n\n")
					;  (setq user-point (point))
					;  (princ "\n\n\n")

		  (princ (format "In %s \n" (emacs-version)))
		  (if (fboundp 'x-server-vendor)
		      (condition-case nil
			  (princ (format "Distributor `%s' version %s .\n"
					 (x-server-vendor)
					 (x-server-version) ))
			(error t)))
		  (if (and system-configuration-options
			   (not (equal system-configuration-options "")))
		      (princ (format "configured using `configure %s' \n\n"
				     system-configuration-options))) 

		  (princ "Important settings:\n")
		  (mapcar
		   '(lambda (var)
		      (princ (format "  value of $%s: %s\n" var (getenv var))))
		   '("LC_ALL" "LC_COLLATE" "LC_CTYPE" "LC_MESSAGES"
		     "LC_MONETARY" "LC_NUMERIC" "LC_TIME" "LANG"))
		  (princ (format "  locale-coding-system: %s\n" locale-coding-system))
		  (princ (format "  default-enable-multibyte-characters: %s\n"
				 default-enable-multibyte-characters))
		  (princ "\n")
		  (princ (format "Major mode: %s\n"
				 (buffer-local-value 'mode-name from-buffer)))
		  (princ "\n")
		  (princ "Minor modes in effect:\n")
		  (dolist (mode minor-mode-list)
		    (and (boundp mode) (buffer-local-value mode from-buffer)
			 (princ (format "  %s: %s\n" mode
					(buffer-local-value mode from-buffer)))))
		  (princ "\n")
		  (princ "Recent input:\n")
		  (let ((before-keys (point)))
		    (princ (mapconcat (lambda (key)
					(if (or (integerp key)
						(symbolp key)
						(listp key))
					    (single-key-description key)
					  (prin1-to-string key nil)))
				      (or recent-keys (recent-keys))
				      " "))
		    )
		  (let ((message-buf (get-buffer "*Messages*")))
		    (if message-buf
			(let (beg-pos
			      (end-pos message-end-point))
			  (princ "\n\nRecent messages:\n")
			  (with-current-buffer message-buf
			    (goto-char end-pos)
			    (forward-line -10)
			    (setq beg-pos (point))
				   
			    (princ (buffer-substring-no-properties  beg-pos end-pos))

			    )
			  )
		      )
		    )
		  ;; This is so the user has to type something
		  ;; in order to send easily.
		  (use-local-map (nconc (make-sparse-keymap) (current-local-map)))
     
     
		  )
		)
	  )


  
    ;; open in mail program
    ;; from here on, we have no control over what's going to happen.
    ;; but at least the messages are getting through.

    (browse-url (format "mailto:%s\?cc=%s&body=%s" 
			(if pretest-p
			    report-emacs-bug-pretest-address
			  report-emacs-bug-address)
			report-aquamacs-bug-address
			(url-encode-string str) ))

    )
  )

(require 'aquamacs-tools)


(defcustom use-emacs-bugreporting nil
  "If non-nil, the original emacs bugreporting tool is used.
This expects a preconfigured e-mail sending mechanism, 
e.g. sendmail/postfix or smtp server)."
  :group 'emacsbug
  :type 'boolean)



;; we'll advice the bugreporting function for better transparency
  
(defadvice report-emacs-bug (around wrap-report-emacs-bug (&rest args) activate)
 (interactive)
  (if use-emacs-bugreporting
      ad-do-it
      (apply #'report-aquamacs-emacs-bug args)
      ) 
)

(provide 'aquamacs-bug)