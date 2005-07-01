
;; Author: adapted from emacsbug.el by K. Shane Hartman
;; Maintainer: David Reitter
;; Keywords: mac bug report
 
;; Last change: $Id: aquamacs-bug.el,v 1.7 2005/07/01 06:51:24 davidswelt Exp $

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



(require 'emacsbug)

(provide 'aquamacs-bug)

;; Request by RMS 06/2005: do not report Aquamacs bugs 
;; to the Emacs mailing lists.

(setq report-emacs-bug-address "aquamacs-bugs@aquamacs.org")
(setq report-emacs-bug-pretest-address "aquamacs-bugs@aquamacs.org")

(defvar report-emacs-bug-cc-list nil
  "List of e-mail addresses to cc when reporting bugs.")

(defun report-emacs-bug-externally-p ()
"Returns non-nil if an external mail client is to be used in 
order to report bugs. The decision is based on the browse-url 
function used, because we leave it to the underlying system or 
the HTML to bring up the appropriate mail client."
 
  (member browse-url-browser-function
	  '(browse-url-mozilla 
	    browse-url-netscape 
	    browse-url-galeon 
	    browse-url-epiphany 
	    browse-url-netscape 
	    browse-url-kde 
	    browse-url-default-windows-browser 
	    browse-url-default-macosx-browser 
	    browse-url-gnome-moz 
	    )
	  )
  )


(defun report-emacs-bug-print-preamble (address)
  (unless report-emacs-bug-no-explanations
    ;; Insert warnings for novice users.
    (insert "This bug report will be sent to the Free Software Foundation,\n")
    (let ((pos (point)))
      (insert "not to your local site managers!")
      (put-text-property pos (point) 'face 'highlight))
    (insert "\nPlease write in ")
    (let ((pos (point)))
      (insert "English")
      (put-text-property pos (point) 'face 'highlight))
    (insert " if possible, because the Emacs maintainers
usually do not have translators to read other languages for them.\n\n")
    (insert (format "Your bug report will be posted to the %s mailing list"
		    address))
    (insert " and possibly to the gnu.emacs.bug news group.\n\n")
    )

  (insert "Please describe exactly what actions triggered the bug\n"
	  "and the precise symptoms of the bug:")
  )


(defun report-emacs-bug-print-debug-log (&optional recent-keys)
  (insert "In " (emacs-version) "\n")
  (if (fboundp 'x-server-vendor)
      (condition-case nil
	  (insert "Distributor `" (x-server-vendor) "', version "
		  (mapconcat 'number-to-string (x-server-version) ".") "\n")
	(error t)))
  (if (and system-configuration-options
	   (not (equal system-configuration-options "")))
      (insert "configured using `configure "
	      system-configuration-options "'\n\n"))
  (insert "Important settings:\n")
  (mapcar
   '(lambda (var)
      (insert (format "  value of $%s: %s\n" var (getenv var))))
   '("LC_ALL" "LC_COLLATE" "LC_CTYPE" "LC_MESSAGES"
     "LC_MONETARY" "LC_NUMERIC" "LC_TIME" "LANG"))
  (insert (format "  locale-coding-system: %s\n" locale-coding-system))
  (insert (format "  default-enable-multibyte-characters: %s\n"
		  default-enable-multibyte-characters))
  (insert "\n")
  (insert (format "Major mode: %s\n"
		  (buffer-local-value 'mode-name from-buffer)))
  (insert "\n")
  (insert "Minor modes in effect:\n")
  (dolist (mode minor-mode-list)
    (and (boundp mode) (buffer-local-value mode from-buffer)
	 (insert (format "  %s: %s\n" mode
			 (buffer-local-value mode from-buffer)))))
  (insert "\n")
  (insert "Recent input:\n")
  (let ((before-keys (point)))
    (insert (mapconcat (lambda (key)
			 (if (or (integerp key)
				 (symbolp key)
				 (listp key))
			     (single-key-description key)
			   (prin1-to-string key nil)))
		       (or recent-keys (recent-keys))
		       " "))
    (save-restriction
      (narrow-to-region before-keys (point))
      (goto-char before-keys)
      (while (progn (move-to-column 50) (not (eobp)))
	(search-forward " " nil t)
	(insert "\n"))))
  (let ((message-buf (get-buffer "*Messages*")))
    (if message-buf
	(let (beg-pos
	      (end-pos message-end-point))
	  (with-current-buffer message-buf
	    (goto-char end-pos)
	    (forward-line -10)
	    (setq beg-pos (point)))
	  (insert "\n\nRecent messages:\n")
	  (insert-buffer-substring message-buf beg-pos end-pos))
      )
    )
  )

 

(defun report-emacs-bug-externally ( topic recent-keys address)
  "Report a bug using the default mail agent. "
    
  (let ( (from-buffer (current-buffer))
	user-point prompt-beg-point message-end-point)
      
    (setq message-end-point
	  (with-current-buffer (get-buffer-create "*Messages*")
	    (point-max-marker)))
    
      (with-temp-buffer  
 
		 
		 (setq prompt-beg-point (point)) 
		 (report-emacs-bug-print-preamble address)
    
		 (setq report-emacs-bug-text-prompt
		       (buffer-substring prompt-beg-point (point)))

		 (insert "\n\n\n\n\n\n\n\n\n(insert your text here)\n\n\n\n\n\n\n\n\n")

		 (report-emacs-bug-print-debug-log recent-keys)
 	       
	   
      ;; open in mail program
      ;; from here on, we have no control over what's going to happen.

      (browse-url (format "mailto:%s\?&subject=%s&body=%s%s" 
			  address
			  (if topic (url-encode-string topic) "")
			  (url-encode-string (buffer-string))
			  (apply 'concat (mapcar 
			   (lambda (a) (concat "&cc=" a))
			   report-emacs-bug-cc-list))
			  
			   )
      )
    )
  )
)


(defun report-emacs-bug-internally (topic recent-keys address)
"Report a bug using the internal mail system via ``compose-mail''"
 
  (let ((from-buffer (current-buffer))
	user-point prompt-beg-point message-end-point)
    (setq message-end-point
	  (with-current-buffer (get-buffer-create "*Messages*")
	    (point-max-marker)))
    (compose-mail address
		  topic)
    ;; The rest of this does not execute
    ;; if the user was asked to confirm and said no.
    (rfc822-goto-eoh)
    (forward-line 1)

    (let ((signature (buffer-substring (point) (point-max))))
      (delete-region (point) (point-max))
      (insert signature)
      (backward-char (length signature)))
    (setq prompt-beg-point (point))

    (report-emacs-bug-print-preamble address)
    
    (setq report-emacs-bug-text-prompt
	  (buffer-substring prompt-beg-point (point)))

    (insert "\n\n")
    (setq user-point (point))
    (insert "\n\n\n")

    (report-emacs-bug-print-debug-log recent-keys)

    ;; This is so the user has to type something
    ;; in order to send easily.
    (use-local-map (nconc (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "\C-c\C-i" 'report-emacs-bug-info)
    (unless report-emacs-bug-no-explanations
      (with-output-to-temp-buffer "*Bug Help*"
	(if (eq mail-user-agent 'sendmail-user-agent)
	    (princ (substitute-command-keys
		    "Type \\[mail-send-and-exit] to send the bug report.\n")))
	(princ (substitute-command-keys
		"Type \\[kill-buffer] RET to cancel (don't send it).\n"))
	(terpri)
	(princ (substitute-command-keys
		"Type \\[report-emacs-bug-info] to visit in Info the Emacs Manual section
about when and how to write a bug report,
and what information to supply so that the bug can be fixed.
Type SPC to scroll through this section and its subsections."))))
    ;; Make it less likely people will send empty messages.
    (make-local-variable 'mail-send-hook)
    (add-hook 'mail-send-hook 'report-emacs-bug-hook)
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (make-local-variable 'report-emacs-bug-orig-text)
      (setq report-emacs-bug-orig-text (buffer-substring (point-min) (point))))
    (goto-char user-point)))


(defun report-emacs-bug (topic &optional recent-keys)
  "Report a bug in GNU Emacs.
Prompts for bug subject. Either leaves you in a mail buffer 
or brings up an external mail client."

  ;; This strange form ensures that (recent-keys) is the value before
  ;; the bug subject string is read.
  (interactive (reverse (list (recent-keys) (read-string "Bug Subject: "))))

  (let ((address
	 ;; If there are four numbers in emacs-version, this is a pretest
	 ;; version.
	 (if (string-match "\\..*\\..*\\." emacs-version)
	     report-emacs-bug-pretest-address
	   report-emacs-bug-address)
	 )
	)
    (if (report-emacs-bug-externally-p)
	(report-emacs-bug-externally topic recent-keys address)
      (report-emacs-bug-internally topic recent-keys address)
      )
    )
  )  