; Mac Printing functions
;; 
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: mac-print.el,v 1.17 2009/02/06 22:57:13 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://aquamacs.org/

;; This package implements export to PDF, export to HTML and
;; printing under Mac OS X.

;; It relies on the htmlize package for HTML conversion.

;; Known caveat: US Letter format only for PDFs and printing.

;; Attribution: Leave this header intact in case you redistribute this file.
;; Attribution must be given in application About dialog or similar,
;; "Contains Aquamacs Mac-Print by D Reitter" does the job.
;; Apart from that, released under the GPL:
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
 
;; Copyright (C) 2005, 2006, 2009, David Reitter

(require 'aquamacs-macros)
 
(defcustom mac-print-font-size-scaling-factor 0.5
  "The factor by which fonts are rescaled during PDF export and printing."
  :type 'float
  :group 'print)

;; Support for monochrome printing
;; Added by Norbert Zeh <nzeh@cs.dal.ca> 2007-09-23

(defcustom mac-print-monochrome-mode nil
  "If non-nil, face colors are ignored when printing.  If nil,
face colors are printed."
  :type 'boolean
  :group 'print)

(defun aquamacs-delete-temp-files ()
  (with-temp-buffer
    (shell-command "rm -f /tmp/Aquamacs\\ Printing\\ * 2>/dev/null" t nil)))

;;;###autoload
(defun aquamacs-print ()
  "Prints the current buffer or, if the mark is active, the current region.
The document is shown in Preview.app and a printing dialog is opened."
  (interactive)
  
  (message "Rendering text ...")
  
  (require 'htmlize) ;; needs to be loaded before let-bindings
  (unless (boundp 'htmlize-white-background)
    (message "Warning - incompatible htmlize package installed. 
Remove from your load-path for optimal printing / export results.")
    )
  (let ((htmlize-html-charset 'utf-8)
	(htmlize-use-rgb-txt nil)
	(htmlize-before-hook nil)
	(htmlize-after-hook nil)
	(htmlize-generate-hyperlinks nil)
	(htmlize-white-background t))
    
    (let ((html-buf (aquamacs-convert-to-html-buffer)))
      (ns-popup-print-panel nil html-buf)
      (kill-buffer html-buf)))
  
  (message nil))

;;;###autoload
(defun aquamacs-page-setup ()
  "Show the page setup dialog."
  (interactive)
  (ns-popup-page-setup-panel))

;;;###autoload
(defun export-to-html (target-file)
  "Saves the current buffer (or region, if mark is active) to a file 
in HTML format."
  (interactive 
   (list 
    (if buffer-file-name
	(read-file-name "Write HTML file: "
			(file-name-directory buffer-file-name)
			(concat
			 (file-name-sans-extension 
			  (file-name-nondirectory buffer-file-name))
			 ".html") nil nil)
      (read-file-name "Write HTML file: " default-directory
		      (expand-file-name
		       (concat
			(file-name-sans-extension 
			 (file-name-nondirectory (buffer-name)))
			".html")
		       default-directory)
		      nil nil))))
  (let ((buf (aquamacs-convert-to-html-buffer)))
    (with-current-buffer buf
      (let ((coding-system-for-write 'utf-8))
	(write-region nil nil target-file nil 'shut-up)))
    (kill-buffer buf)))

(defun aquamacs-copy-as-html (beg end)
  "Copies the region in HTML format into the clipboard."
  (interactive "r")
  (when (or (not transient-mark-mode) mark-active)
    (let ((x-select-enable-clipboard t)
	  (buf (aquamacs-convert-to-html-buffer beg end)))
      (with-current-buffer buf
	(copy-region-as-kill (point-min) (point-max))
        (ns-store-cut-buffer-internal 'PRIMARY (buffer-string) 'html))
      ;; ns-store-cut-buffer-internal with TYPE 'html doesn't seem to work
      ;; (with-current-buffer buf
      ;;   (ns-store-cut-buffer-internal 'PRIMARY (buffer-string) 'html))
      (kill-buffer buf))))

(defun aquamacs-convert-to-html-buffer (&optional beg end)
  "Creates a buffer containing an HTML rendering of the current buffer."

  (require 'htmlize)
  (if (not (protect (equal (substring htmlize-version 0 5) "1.23a")))
	 (message "Warning - possibly incompatible htmlize package installed. 
Remove from your load-path for optimal printing / export results."))
    (require 'mule) ; for coding-system-get
    (setq htmlize-ignore-colors mac-print-monochrome-mode)

    (let* ((htmlize-ignore-colors mac-print-monochrome-mode)
	   (htmlize-html-major-mode nil)
	   (htmlize-preformat nil) ;; bug in WebKit: avoid long lines
	   (htmlize-html-charset 
	    (if buffer-file-coding-system
		(coding-system-get buffer-file-coding-system
				   'mime-charset)))
	   (show-paren-mode-save show-paren-mode)
	   (html (unwind-protect
		     (progn
		       (show-paren-mode 0)
		       (if (or mark-active (not transient-mark-mode) beg)
			   (htmlize-region (or beg (region-beginning))
					   (or end (region-end)))
			 (htmlize-buffer (current-buffer))))
		   (show-paren-mode show-paren-mode-save))))
      html))

(provide 'mac-print)