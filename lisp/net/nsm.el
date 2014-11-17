;;; nsm.el --- Network Security Manager

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: encryption, security, network

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defvar nsm-host-settings nil)

(defgroup nsm nil
  "Network Security Manager"
  :version "25.1"
  :group 'comm)

(defcustom nsm-security-level 'medium
  "How secure the network should be."
  :version "25.1"
  :group 'nsm
  :type '(choice (const :tag "Low" 'low)
		 (const :tag "Medium" 'medium)
		 (const :tag "High" 'high)
		 (const :tag "Paranoid" 'paranoid)))

(defcustom nsm-settings-file (expand-file-name "network-security.data"
						 user-emacs-directory)
  "The file the security manager settings will be stored in."
  :version "25.1"
  :group 'nsm
  :type 'file)

(defun nsm-verify-connection (process host port &optional save-fingerprint)
  "Verify the security status of PROCESS that's connected to HOST:PORT.
If PROCESS is a gnutls connection, the certificate validity will
be examined.  If it's a non-TLS connection, it may be compared
against previous connections.  If the function determines that
there is something odd about the connection, the user will be
queried about what to do about it.

The process it returned if everything is OK, and otherwise, the
process will be deleted and nil is returned.

If SAVE-FINGERPRINT, always save the fingerprint of the
server (if the connection is a TLS connection).  This is useful
to keep track of the TLS status of STARTTLS servers."
  (let* ((status (gnutls-peer-status process))
	 (id (nsm-id host port))
	 (settings (nsm-host-settings id)))
    (cond
     ((not (process-live-p process))
      nil)
     ((not status)
      ;; This is a non-TLS connection.
      (nsm-check-plain-connection process host port settings))
     (t
      (let ((process
	     (nsm-check-tls-connection process host port status settings)))
	(when (and process save-fingerprint
		   (null (nsm-host-settings id)))
	  (nsm-save-host
	   id (list :id id
		    :fingerprint (plist-get status :fingerprint))))
	process)))))

(defun nsm-check-tls-connection (process host port status settings)
  (let ((warnings (plist-get status :warnings)))
    (cond
     ((null warnings)
      ;; The certificate is fine, but if we're paranoid, we might
      ;; want to check whether it's changed anyway.
      (if (not (equal nsm-security-level 'paranoid))
	  process
	(if (and settings
		 (not (equal (plist-get status :fingerprint)
			     (plist-get settings :fingerprint)))
		 (not (nsm-query
		       (nsm-id host port)
		       status
		       "The fingerprint for the connection to %s:%s has changed from\n%s to\n%s"
		       host port
		       (plist-get status :fingerprint)
		       (plist-get settings :fingerprint))))
	    (progn
	      (delete-process process)
	      nil)
	  ;; Save the host fingerprint so that we can check it the
	  ;; next time we connect.
	  (nsm-save-host (nsm-id host port) status)
	  process)))
     ((not (equal nsm-security-level 'low))
      ;; We have a warning, so query the user.
      (if (and (not (nsm-warnings-ok-p status settings))
	       (not (nsm-query
		     (nsm-id host port)
		     status
		     "The TLS connection to %s:%s is insecure\nfor the following reason%s:\n\n%s"
		     host port
		     (if (> (length warnings) 1)
			 "s" "")
		     (mapconcat 'cadr warnings "\n"))))
	  (progn
	    (delete-process process)
	    nil)
	process)))))

(defun nsm-check-plain-connection (process host port settings)
  ;; If this connection used to be TLS, but is now plain, then it's
  ;; possible that we're being Man-In-The-Middled by a proxy that's
  ;; stripping out STARTTLS announcements.
  (if (and (plist-get settings :fingerprint)
	   (nsm-query
	    (nsm-id host port)
	    nil
	    "The connection to %s:%s used to be an encrypted connection, but is now\nunencrypted.  This might mean that there's a man-in-the-middle tapping\nthis connection."
	    host port))
      (progn
	(delete-process process)
	nil)
    process))

(defun nsm-query (id status message &rest args)
  (let ((response
	 (condition-case nil
	     (nsm-query-user message args)
	   ;; Make sure we manage to close the process if the user hits
	   ;; `C-g'.
	   (quit 'no)
	   (error 'no))))
    (if (eq response 'no)
	nil
      (nsm-save-host id status response)
      t)))

(defun nsm-query-user (message args)
  (let ((responses '((?n . no)
		     (?s . session)
		     (?a . always)))
	(prefix "")
	response)
    (while (not response)
      (setq response
	    (cdr
	     (assq (downcase
		    (read-char (concat prefix
				       (apply 'format message args)
				       "\n\nContinue connecting? (No, Session only, Always)")))
		   responses)))
      (unless response
	(ding)
	(setq prefix "Invalid choice.\n")))
    response))

(defun nsm-save-host (id status &optional permanency)
  (nsm-remove-setting id)
  (push
   (list :id id
	 :fingerprint (if status
			  (plist-get status :fingerprint)
			;; Plain connection.
			:none)
	 :conditions
	 (cond
	  ((not status)
	   `((:unencrypted ,permanency)))
	  ((not permanency)
	   nil)
	  (t
	   (mapcar
	    (lambda (elem)
	      (list (car elem) permanency))
	    (plist-get status :warnings)))))
   nsm-host-settings)
  (nsm-write-settings))

(defun nsm-write-settings ()
  (with-temp-file nsm-settings-file
    (insert "(\n")
    (dolist (setting nsm-host-settings)
      ;; Only save those settings that are saved for paranoid tracking
      ;; purposes, or permanent settings.
      (when (or (null (plist-get setting :conditions))
		(eq (cadr (car (plist-get setting :conditions))) 'always))
	(prin1 setting (current-buffer)))
      (insert "\n"))
    (insert ")\n")))

(defun nsm-read-settings ()
  (setq nsm-host-settings
	(with-temp-buffer
	  (insert-file-contents nsm-settings-file)
	  (goto-char (point-min))
	  (ignore-errors (read (current-buffer))))))

(defun nsm-id (host port)
  (concat "sha1:" (sha1 (format "%s:%s" host port))))

(defun nsm-host-settings (id)
  (when (and (not nsm-host-settings)
	     (file-exists-p nsm-settings-file))
    (nsm-read-settings))
  (let ((result nil))
    (dolist (elem nsm-host-settings)
      (when (equal (plist-get elem :id) id)
	(setq result elem)))
    result))

(defun nsm-warnings-ok-p (status settings)
  (let ((not-ok nil)
	(conditions (plist-get settings :conditions)))
    (dolist (warning (plist-get status :warnings))
      (unless (memq (cadr (memq (car warning) conditions))
		    '(always session))
	(setq not-ok t)))
    not-ok))

(defun nsm-remove-setting (id)
  (setq nsm-host-settings (cl-delete-if
			   (lambda (elem)
			     (equal (plist-get elem :id) id))
			   nsm-host-settings)))

(provide 'nsm)

;;; nsm.el ends here
