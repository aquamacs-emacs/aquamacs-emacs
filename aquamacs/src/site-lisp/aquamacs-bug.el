;; Aquamacs-bug.el
 
;; Maintainer: David Reitter
;; Keywords: mac bug report
 
;; Last change: $Id: aquamacs-bug.el,v 1.16 2008/07/09 22:06:41 davidswelt Exp $

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



; (require 'emacsbug)
(provide 'aquamacs-bug)

(require 'aquamacs-tools)
;; Request by RMS 06/2005: do not report Aquamacs bugs 
;; to the Emacs mailing lists.
 
;; use standard emacs bug reporting technology.

; (report-aquamacs-bug)
(defun report-aquamacs-bug (topic &optional recent-keys)
  "Report a bug in Aquamacs Emacs.
Prompts for bug subject.  Leaves you in a mail buffer."
  ;; This strange form ensures that (recent-keys) is the value before
  ;; the bug subject string is read.
  (interactive 
   (reverse (list (recent-keys) 
		  (if (eq send-mail-function 'mailclient-send-it)
		      nil (read-string "Bug Subject: ")))))
  ;; If there are four numbers in emacs-version, this is a pretest
  ;; version.
  
  (let  ((report-emacs-bug-address "aquamacs-bugs@aquamacs.org")
	 (report-emacs-bug-pretest-address "aquamacs-bugs@aquamacs.org")
	 (report-emacs-bug-no-confirmation t))
 
    (if (not (eq send-mail-function 'mailclient-send-it))
	(report-emacs-bug topic recent-keys)
					; else
    
      (let ((one-buffer-one-frame nil)
	    (mail-interactive nil))
	(save-window-excursion
	  (let (backup-buffer)
	    (when (get-buffer "*mail*")
	      (with-current-buffer (get-buffer "*mail*")
		(setq backup-buffer (rename-buffer "mail-backup" t))))
	    (report-emacs-bug topic recent-keys)
	    (insert "Enter your bug report here.")
	    (end-of-buffer)
	    (insert (format "\nCommand line: %s\n\nPATH: %s\n\nexec-path: %s" 
			    command-line-args (getenv "PATH") exec-path))
	    (set-buffer-modified-p t)
	    (mail-send)
	    (kill-buffer "*mail*")
	    (if backup-buffer
		(with-current-buffer backup-buffer
		  (rename-buffer "*mail*" t)))
	    ))))))

(defun start-vanilla-aquamacs (&optional arg)
  "Start a vanilla Aquamacs
Starts another instance of the current session.
With prefix argument ARG, start without initializing
most Aquamacs-specific code."
  (interactive "P")
  (start-process "vanilla-aquamacs" nil (car command-line-args) (if arg "-Q" "-q")))


(defvar menu-bar-bug-help-menu (make-sparse-keymap "Diagnose and Report Bug"))

(define-key menu-bar-bug-help-menu [send-emacs-bug-report]
  `(menu-item "Send Bug Report...                 "  
	      report-aquamacs-bug
	      :help "Report a Bug in Aquamacs Emacs"
	      :keys ,(aq-binding 'report-aquamacs-bug)) )


(define-key menu-bar-bug-help-menu [start-vanilla-aquamacs]
  `(menu-item "Start Aquamacs without customizations"  
	      start-vanilla-aquamacs
	      :help "Start Aquamacs Emacs without any user-specific settings."
	      :keys ,(aq-binding 'start-vanilla-aquamacs)) )

(define-key-after menu-bar-help-menu [bug-diagnosis]
  `(menu-item "Diagnose and Report Bug " 
	      ,menu-bar-bug-help-menu
	      :help "Bug diagnosis")
  'menu-aquamacs-homepage)
(define-key menu-bar-help-menu [send-emacs-bug-report] nil)

;; code to print non-standard values
;; (mapatoms 
;;  (lambda (sym)
;;    (when (default-boundp sym)
;;      (princ (format "%s:" sym))
;;      (let ((print-level 2)
;; 	   (print-length 3))
;;        (prin1 (eval sym)))
;;      (princ "\n"))))
