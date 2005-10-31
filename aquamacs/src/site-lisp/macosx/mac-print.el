; Mac Printing functions
;; 
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: mac-print.el,v 1.6 2005/10/31 23:14:01 davidswelt Exp $

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
 
;; Copyright (C) 2005, David Reitter


(defun aquamacs-delete-temp-files ()
  (shell-command "rm -f /tmp/Aquamacs\\ Printing\\ * 2>/dev/null" 'shut-up))

;; (aquamacs-delete-temp-files)


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
   
  (let ((html-to-pdf "/System/Library/Printers/Libraries/./convert")
	(html-file (make-temp-file "aquamacs-temp-" nil ".html")))
    (export-to-html html-file)
    (message
     (with-temp-buffer
       (shell-command (concat html-to-pdf " -f \"" html-file "\" -o \"" target-file "\" -j application/pdf ; rm -f " html-file) 'output-here)
       (buffer-string))))
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
Remove for optimal printing / export results.")
      )
    (let* (
	   (htmlize-html-major-mode nil)
	   
	   (show-paren-mode-save show-paren-mode)
	   
	   (html (unwind-protect
		     (progn
		       (show-paren-mode 0)
		       (if mark-active
			   (htmlize-region (region-beginning) (region-end))
			 (htmlize-buffer)))
		   (show-paren-mode show-paren-mode-save))))
 
      (with-current-buffer html
	(let ((coding-system-for-write 'utf-8))
	  (write-region nil nil target-file nil 'shut-up)))
      (kill-buffer html)))

(provide 'mac-print)