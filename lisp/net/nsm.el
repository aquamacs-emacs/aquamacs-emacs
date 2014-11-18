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

(defvar nsm-permanent-host-settings nil)
(defvar nsm-temporary-host-settings nil)

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

(defun nsm-verify-connection (process host port &optional
				      save-fingerprint warn-unencrypted)
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
to keep track of the TLS status of STARTTLS servers.

If WARN-UNENCRYPTED, query the user if the connection is
unencrypted."
  (if (eq nsm-security-level 'low)
      process
    (let* ((status (gnutls-peer-status process))
	   (id (nsm-id host port))
	   (settings (nsm-host-settings id)))
      (cond
       ((not (process-live-p process))
	nil)
       ((not status)
	;; This is a non-TLS connection.
	(nsm-check-plain-connection process host port settings
				    warn-unencrypted))
       (t
	(let ((process
	       (nsm-check-tls-connection process host port status settings)))
	  (when (and process save-fingerprint
		     (null (nsm-host-settings id)))
	    (nsm-save-host id status 'fingerprint 'always))
	  process))))))

(defun nsm-check-tls-connection (process host port status settings)
  (let ((warnings (plist-get status :warnings)))
    (cond
     ((null warnings)
      ;; The certificate is fine, but if we're paranoid, we might
      ;; want to check whether it's changed anyway.
      (if (not (equal nsm-security-level 'paranoid))
	  process
	(if (not (nsm-fingerprint-ok-p host port status settings))
	    (progn
	      (delete-process process)
	      nil)
	  ;; Save the host fingerprint so that we can check it the
	  ;; next time we connect.
	  (nsm-save-host (nsm-id host port) status 'fingerprint 'always)
	  process)))
     ((not (equal nsm-security-level 'low))
      ;; We always want to pin the certificate of invalid connections
      ;; to track man-in-the-middle or the like.
      (if (not (nsm-fingerprint-ok-p host port status settings))
	  (progn
	    (delete-process process)
	    nil)
	;; We have a warning, so query the user.
	(if (and (not (nsm-warnings-ok-p status settings))
		 (not (nsm-query
		       (nsm-id host port) status 'conditions
		       "The TLS connection to %s:%s is insecure\nfor the following reason%s:\n\n%s"
		       host port
		       (if (> (length warnings) 1)
			   "s" "")
		       (mapconcat 'cadr warnings "\n"))))
	    (progn
	      (delete-process process)
	      nil)
	  process))))))

(defun nsm-fingerprint-ok-p (host port status settings)
  (if (and settings
	   (not (eq (plist-get settings :fingerprint) :none))
	   (not (equal (plist-get status :fingerprint)
		       (plist-get settings :fingerprint)))
	   (not (nsm-query
		 (nsm-id host port) status 'fingerprint
		 "The fingerprint for the connection to %s:%s has changed from\n%s to\n%s"
		 host port
		 (plist-get settings :fingerprint)
		 (plist-get status :fingerprint))))
      ;; Not OK.
      nil
    t))

(defun nsm-check-plain-connection (process host port settings warn-unencrypted)
  ;; If this connection used to be TLS, but is now plain, then it's
  ;; possible that we're being Man-In-The-Middled by a proxy that's
  ;; stripping out STARTTLS announcements.
  (cond
   ((and (plist-get settings :fingerprint)
	 (not (eq (plist-get settings :fingerprint) :none))
	 (not
	  (nsm-query
	   (nsm-id host port) nil 'conditions
	   "The connection to %s:%s used to be an encrypted\nconnection, but is now unencrypted.  This might mean that there's a\nman-in-the-middle tapping this connection."
	   host port)))
    (delete-process process)
    nil)
   ((and warn-unencrypted
	 (not (memq :unencrypted (plist-get settings :conditions)))
	 (not (nsm-query
	       (nsm-id host port) nil 'conditions
	       "The connection to %s:%s is unencrypted."
	       host port)))
    (delete-process process)
    nil)
   (t
    process)))

(defun nsm-query (id status what message &rest args)
  (let ((response
	 (condition-case nil
	     (nsm-query-user message args (nsm-format-certificate status))
	   ;; Make sure we manage to close the process if the user hits
	   ;; `C-g'.
	   (quit 'no)
	   (error 'no))))
    (if (eq response 'no)
	nil
      (nsm-save-host id status what response)
      t)))

(defun nsm-query-user (message args cert)
  (let ((buffer (get-buffer-create "*Network Security Manager*")))
    (with-help-window buffer
      (with-current-buffer buffer
	(erase-buffer)
	(when (> (length cert) 0)
	  (insert cert "\n"))
	(insert (apply 'format message args))))
    (let ((responses '((?n . no)
		       (?s . session)
		       (?a . always)))
	  (prefix "")
	  response)
      (while (not response)
	(setq response
	      (cdr
	       (assq (downcase
		      (read-char
		       (concat prefix
			       "Continue connecting? (No, Session only, Always)")))
		     responses)))
	(unless response
	  (ding)
	  (setq prefix "Invalid choice.  ")))
      (kill-buffer buffer)
      response)))

(defun nsm-save-host (id status what permanency)
  (let ((saved
	 (list :id id
	       :fingerprint (if status
				(plist-get status :fingerprint)
			      ;; Plain connection.
			      :none))))
    ;; We either want to save/update the fingerprint or the conditions
    ;; of the certificate/unencrypted connection.
    (when (eq what 'conditions)
      (cond
       ((not status)
	(nconc saved `(:conditions (:unencrypted))))
       ((plist-get status :warnings)
	(nconc saved
	       `(:conditions ,(mapcar 'car (plist-get status :warnings)))))))
    (if (eq permanency 'always)
	(progn
	  (nsm-remove-temporary-setting id)
	  (nsm-remove-permanent-setting id)
	  (push saved nsm-permanent-host-settings)
	  (nsm-write-settings))
      (nsm-remove-temporary-setting id)
      (push saved nsm-temporary-host-settings))))

(defun nsm-write-settings ()
  (with-temp-file nsm-settings-file
    (insert "(\n")
    (dolist (setting nsm-permanent-host-settings)
      (insert " ")
      (prin1 setting (current-buffer))
      (insert "\n"))
    (insert ")\n")))

(defun nsm-read-settings ()
  (setq nsm-permanent-host-settings
	(with-temp-buffer
	  (insert-file-contents nsm-settings-file)
	  (goto-char (point-min))
	  (ignore-errors (read (current-buffer))))))

(defun nsm-id (host port)
  (concat "sha1:" (sha1 (format "%s:%s" host port))))

(defun nsm-host-settings (id)
  (when (and (not nsm-permanent-host-settings)
	     (file-exists-p nsm-settings-file))
    (nsm-read-settings))
  (let ((result nil))
    (dolist (elem (append nsm-temporary-host-settings
			  nsm-permanent-host-settings))
      (when (and (not result)
		 (equal (plist-get elem :id) id))
	(setq result elem)))
    result))

(defun nsm-warnings-ok-p (status settings)
  (let ((not-ok nil)
	(conditions (plist-get settings :conditions)))
    (dolist (warning (plist-get status :warnings))
      (when (memq (car warning) conditions)
	(setq not-ok t)))
    not-ok))

(defun nsm-remove-permanent-setting (id)
  (setq nsm-permanent-host-settings
	(cl-delete-if
	 (lambda (elem)
	   (equal (plist-get elem :id) id))
	 nsm-permanent-host-settings)))

(defun nsm-remove-temporary-setting (id)
  (setq nsm-temporary-host-settings
	(cl-delete-if
	 (lambda (elem)
	   (equal (plist-get elem :id) id))
	 nsm-temporary-host-settings)))

(defun nsm-format-certificate (status)
  (let ((cert (plist-get status :certificate)))
    (when cert
      (format "Certificate issued by %s\nIssued to %s\nCertificate host name: %s\nPublic key: %s, signature: %s, security level: %s\nValid from: %s, valid to: %s\n"
	      (nsm-certificate-part (plist-get cert :issuer) "CN")
	      (nsm-certificate-part (plist-get cert :subject) "O")
	      (nsm-certificate-part (plist-get cert :subject) "CN")
	      (plist-get cert :public-key-algorithm)
	      (plist-get cert :signature-algorithm)
	      (propertize (plist-get cert :certificate-security-level)
			  'face 'bold)
	      (plist-get cert :valid-from)
	      (plist-get cert :valid-to)))))

(defun nsm-certificate-part (string part)
  (cadr (assoc part (nsm-parse-subject string))))

(defun nsm-parse-subject (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((start (point))
	  (result nil))
      (while (not (eobp))
	(push (replace-regexp-in-string
	       "[\\]\\(.\\)" "\\1"
	       (buffer-substring start
				 (if (re-search-forward "[^\\]," nil 'move)
				     (1- (point))
				   (point))))
	      result)
	(setq start (point)))
      (mapcar
       (lambda (elem)
	 (let ((pos (cl-position ?= elem)))
	   (if pos
	       (list (substring elem 0 pos)
		     (substring elem (1+ pos)))
	     elem)))
       (nreverse result)))))

(provide 'nsm)

;;; nsm.el ends here
