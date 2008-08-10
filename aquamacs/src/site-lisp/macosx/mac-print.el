; Mac Printing functions
;; 
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: mac-print.el,v 1.15 2008/08/10 13:32:13 davidswelt Exp $

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
 
;; Copyright (C) 2005, 2006, David Reitter
 
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

(defun aquamacs-print ()
  "Prints the current buffer or, if the mark is active, the current region.
The document is shown in Preview.app and a printing dialog is opened."
  (interactive)
  
  (message "Rendering text ...")
  
  (let ((tmp-pdf-file (make-temp-file 
		       (concat "Aquamacs Printing " 
			       (if buffer-file-name
				   (file-name-nondirectory buffer-file-name)
				 "")
			       " ") 
		       nil)))
 
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
      (export-to-pdf tmp-pdf-file))

    (add-hook 'kill-emacs-hook 'aquamacs-delete-temp-files)
    (do-applescript (concat
		     "tell application \"Preview\"
        activate
	open the POSIX file \"" tmp-pdf-file  "\"
	activate
	tell application \"System Events\"
		tell process \"Preview\"
			keystroke \"p\" using command down
		end tell
	end tell
end tell"  ))
    (message "... done")))

(defun export-to-pdf (target-file)
  "Saves the current buffer (or region, if mark is active) to a file 
in PDF format.
 (Aquamacs / Mac only)"
  (interactive 
   (list 
    (if buffer-file-name
	(read-file-name "Write PDF file: "
			(file-name-directory buffer-file-name)
			(concat
			 (file-name-sans-extension 
			  (file-name-nondirectory buffer-file-name))
			 ".pdf") nil nil)
      (read-file-name "Write PDF file: " default-directory
		      (expand-file-name
		       (concat
			(file-name-sans-extension 
			 (file-name-nondirectory (buffer-name)))
			".pdf")
		       default-directory)
		      nil nil))))
  (if (file-directory-p target-file)
      (error "Must give a full file name."))
  (setq target-file (expand-file-name target-file))
   
  (let ((html-to-pdf "/System/Library/Printers/Libraries/convert")
	(html-file (make-temp-file "aquamacs-temp-" nil ".html"))
	(htmlize-font-size-scaling-factor 
	 (or 
	  mac-print-font-size-scaling-factor
	  htmlize-font-size-scaling-factor)))
    (export-to-html html-file)
;;     (with-current-buffer "*Messages*"
;;       (save-excursion
;;       (end-of-buffer)
;;       (insert
;;        (with-temp-buffer
	 (call-process html-to-pdf nil "*Messages*" nil  
		       "-f" html-file 
		       "-o" target-file
		       "-a" (concat "media=" 
				    (capitalize (symbol-name ps-paper-type)))
		       "-j" "application/pdf"
		       "-D")
;	 (buffer-string))))
)
  target-file)

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


    (require 'htmlize)
    (if (string< (substring htmlize-version 0 5) "1.23a")
      (message "Warning - outdated htmlize package installed. 
Remove from your load-path for optimal printing / export results.")
      )
    (let* (
	   (htmlize-html-major-mode nil)
	   
	   (show-paren-mode-save show-paren-mode)
	   
	   (html (unwind-protect
		     (progn
		       (show-paren-mode 0)
		       (if mark-active
			   (htmlize-region (region-beginning) 
					   (region-end)
					   mac-print-monochrome-mode)
			 (htmlize-buffer (current-buffer)
					 mac-print-monochrome-mode)))
		   (show-paren-mode show-paren-mode-save))))
 
      (with-current-buffer html
	(let ((coding-system-for-write 'utf-8))
	  (write-region nil nil target-file nil 'shut-up)))
      (kill-buffer html)))

(provide 'mac-print)