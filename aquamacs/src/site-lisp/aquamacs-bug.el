;; Aquamacs-bug.el
 
;; Maintainer: David Reitter
;; Keywords: mac bug report
 
;; Last change: $Id: aquamacs-bug.el,v 1.11 2007/11/30 07:41:51 davidswelt Exp $

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
	    (set-buffer-modified-p t)
	    (mail-send)
	    (kill-buffer "*mail*")
	    (if backup-buffer
		(with-current-buffer backup-buffer
		  (rename-buffer "*mail*" t)))
	    ))))))


(define-key-after menu-bar-help-menu [send-emacs-bug-report]
  `(menu-item ,(aq-shortcut "Send Bug Report..." 
		       'report-aquamacs-bug)
	      report-aquamacs-bug
	      :help "Report a Bug in Aquamacs Emacs"
	      :keys nil) 'menu-aquamacs-homepage)
