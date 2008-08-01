;; configuration for AUCTeX on OS X

;; Maintainer: David Reitter
;; originally authored by Kevin Walzer
;; Keywords: auctex
 
;; Last change: $Id: auctex-config.el,v 1.44 2008/08/01 07:19:00 davidswelt Exp $

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
 
;; Copyright (C) 2005, 2008, Kevin Walzer, David Reitter

  

;; set the PATH to typical locations for TeX stuff

(setenv "PATH"
	(concat (getenv "PATH")
		(let ((f (file-expand-wildcards "/usr/local/texlive/20*/bin")))
		  (if f (concat ":" (car f)) ""))
		(let ((f (file-expand-wildcards "/usr/local/teTeX/bin/*-apple-darwin-current")))
		  (if f (concat ":" (car f)) ""))))

;; don't set all of these paths. only what's necessary.
;; everything else should be initialized from PATH anyways
(setq exec-path (append exec-path
			(file-expand-wildcards "/usr/local/texlive/20*/bin")
			(file-expand-wildcards "/usr/local/teTeX/bin/*-apple-darwin-current")))

(load "edit-modes/auctex.el" nil t t)
;; this is not done by default
;; maybe add a menu option?
;;(load "preview-latex.el" nil t t)

(aquamacs-set-defaults '(
			 ( TeX-parse-self t)
			 ( TeX-master t) ;; set default
			 ( reftex-plug-into-AUCTeX t)
			 ( reftex-save-parse-info t)
			 ( reftex-use-multiple-selection-buffers t)
			 ( bib-highlight-mouse-t nil)
			 ( bib-cite-use-reftex-view-crossref t)))

(autoload 'bib-cite-minor-mode "bib-cite")
(autoload 'turn-on-bib-cite "bib-cite")
; only load if ghostscript is installed
 
(defvar aq-preview-latex-checked nil)
(defun load-preview-if-ghostscript ()
  "Loads preview-latex.el in case Ghostscript is installed.
Only checks once - subsequent calls will not result in any action."
  (unless aq-preview-latex-checked
    (with-temp-buffer
      (shell-command "which pdf2dsc " t)
      (if (string-match "^no " (buffer-string))
	   (message  
	    "No Ghostscript (pdf2dsc) found - preview-latex not activated.")
	(load "preview-latex.el" nil t nil)))
    (setq aq-preview-latex-checked t)) nil)

;; must be done here do initialize
;; can't be loaded in hook later on.
(load-preview-if-ghostscript)



(defvar LaTeX-mode-hook nil) ;; make sure it's defined

;; (add-hook 'LaTeX-mode-hook 'load-preview-if-ghostscript)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-bib-cite)
(add-hook 'LaTeX-mode-hook 'LaTeX-install-toolbar)
(add-hook 'LaTeX-mode-hook (lambda () (TeX-fold-mode t)))
(add-hook 'TeX-mode-hook 'aquamacs-latex-viewer-support 'append) ;; load reftex first

(defun file-line-number (&optional buffer-pos)
  "Returns the number of a line in the visited file.
BUFFER-POS specifies a position in the current buffer (point is assumed
if nil). The function evaluates to the corresponding number of a line in 
the file that the buffer visits (assuming the file has been saved).
This will normally be the line number at that position, unless 
`longlines-mode' is active."
  (if (and (boundp 'longlines-mode) longlines-mode)
      (let ((pos 1)  ;; use 1 for pos, not (point-min), to ignore narrowing
	    (count 1))
	(while (and (< pos (buffer-size)) 
		    (setq pos 
			  (text-property-any pos (or buffer-pos (point)) 
						 'hard t)))
	  (if (eq (char-after pos) 10)
	      (setq count (1+ count)))
	  (setq pos (1+ pos)))
	count)
    (line-number-at-pos buffer-pos)))

;; this is much slower
;; (defun fln (&optional buffer-pos)
;;   (unless buffer-pos (setq buffer-pos (point)))
;;   (save-excursion
;;     (goto-char 0)
;;     (let ((count 0))
;;       (while (search-forward "\n" buffer-pos t)
;;         (if (get-text-property (match-beginning 0) 'hard)
;; 	    (setq count (1+ count))))
;;       count)))


(defun buffer-line-number (file-line-number)
  "Returns the buffer line number given a line in the visited file."
  (if (and (boundp 'longlines-mode) longlines-mode)
      (let ((pos 1) (count 0))
	(while (and (> file-line-number 0)
		    (< pos (buffer-size))
		    (setq pos (text-property-any pos (buffer-size) 'hard t)))
	  (if (eq (char-after pos) 10)
	      (setq file-line-number (1- file-line-number)))
	  (setq pos (1+ pos)))
	(line-number-at-pos pos))
    file-line-number))

;;(defun goto-file-line (file-line-number)
;;  (goto-line (buffer-line-number file-line-number)))

(defun TeX-current-file-line ()
  "The line number in the file corresponding to the current buffer line number."
  (format "%d" (+ 1 (file-line-number))))

(defvar aquamacs-tex-pdf-viewer "Skim"
  "External viewer for `aquamacs-call-viewer' and `aquamacs-latex-crossref'.
Aquamacs defines an AUCTeX command called `Jump To PDF', 
which calls this viewer.")

(defun aquamacs-call-viewer (the-file line source)
"Display THE-FILE as PDF at LINE (as in file SOURCE).
Calls `aquamacs-tex-pdf-viewer' to display the PDF file THE-FILE."
  (let ((full-file-name
	 (expand-file-name 
	  the-file 
	  (and buffer-file-name (file-name-directory buffer-file-name))))
      (full-source-name
       (expand-file-name 
	source 
	(and buffer-file-name (file-name-directory buffer-file-name)))))
  (do-applescript
  (format 
 "
 set theSink to POSIX file \"%s\" 
 set theSource to POSIX file \"%s\" 
 tell application \"%s\" 
     activate 
     open theSink 
     tell front document to go to TeX line %d from theSource 
  end tell
" full-file-name full-source-name aquamacs-tex-pdf-viewer line))))


(defun aquamacs-latex-crossref (ev)
  "Cross-reference in LaTeX"
  (interactive "e")
  (save-excursion 
    (mouse-set-point ev)
  (condition-case nil
      (let ((aquamacs-ring-bell-on-error-flag nil))
	(reftex-view-crossref current-prefix-arg))
    (error 
	   (TeX-command  "Jump To PDF" 'TeX-master-file))
     nil )))



 
(defun aquamacs-skim-running-p ()
(ignore-errors
  (> (string-to-number 
      (with-temp-buffer
	(shell-command "ps ax | grep --count -e '[Ss]kim.app'" t)
	(buffer-string))) 
     0)))

(defvar aquamacs-skim-timer nil)

(defvar aquamacs-skim-show-info-message t)

(defun aquamacs-check-for-skim ()
"Show help message if Skim.app is running."
  (and (equal major-mode 'latex-mode) (boundp 'TeX-PDF-mode) TeX-PDF-mode 
       (file-readable-p (expand-file-name 
			 (TeX-master-file (TeX-output-extension)) 
			 (and buffer-file-name 
			      (file-name-directory buffer-file-name))))
     ;; check for running 
     (aquamacs-skim-running-p)
     (if (not (equal (cdr-safe 
		      (cdr-safe 
		       (assq-string-equal "^pdf$" TeX-output-view-style)))
		     "open %o"))
	      ;; has not been changed by us or the user
	 t
       ;; this variable should not be automatically saved by "Save Options"
       ;; (unless user customizes it explicitly)
       (setq TeX-output-view-style 
	     (cons '("^pdf$" "." "open -a Skim.app %o")
		   TeX-output-view-style)))
     (cancel-timer aquamacs-skim-timer)
     aquamacs-skim-show-info-message
     (message 
      (substitute-command-keys 
       (format 
	"Skim detected. Use \\[aquamacs-latex-crossref]%s to jump to the PDF and back."
	(if (eq 'control mac-emulate-three-button-mouse) 
	    " (Shift-Apple-Click)" ""))))))
  

(defun aquamacs-latex-viewer-support ()
  "Support for Skim as LaTeX viewer if present."
  (add-to-list 'TeX-expand-list
	       '("%(FileLine)" TeX-current-file-line) 'append)
  (add-to-list 'TeX-command-list
	     '("Jump To PDF" 
	       "(aquamacs-call-viewer \"%o\" %(FileLine) \"%b\")" 
	       TeX-run-function nil t 
	       :help "Jump here in Skim") 'append)
  
  (and (boundp 'reftex-mode-map) reftex-mode-map
       (define-key reftex-mode-map [(shift mouse-2)] nil))

  (and (boundp 'LaTeX-mode-map) LaTeX-mode-map
       (define-key LaTeX-mode-map [(shift mouse-2)] 
	 'aquamacs-latex-crossref))
  (unless aquamacs-skim-timer ;; just once per session
    (setq aquamacs-skim-timer 
	  (run-with-idle-timer 30 t 'aquamacs-check-for-skim)))
  (unless server-process
    ;; start server to make emacsclient work
    (server-start)))

(require 'server)
(defun server-goto-line-column (file-line-col)
  (goto-line (buffer-line-number (nth 1 file-line-col)))
  (let ((column-number (nth 2 file-line-col)))
    (when (> column-number 0)
      (move-to-column (1- column-number)))))


(if (boundp 'aquamacs-default-toolbarx-meaning-alist) ;; not on TTY
    (aquamacs-set-defaults 
     '((TeX-PDF-mode t)
       ;; Toolbar business
       (TeX-bar-TeX-all-button-alists
	(TeX-bar-TeX-button-alist aquamacs-default-toolbarx-meaning-alist))
       (TeX-bar-LaTeX-all-button-alists
	(TeX-bar-LaTeX-button-alist aquamacs-default-toolbarx-meaning-alist))
       (TeX-bar-TeX-buttons
	(open-file save-buffer write-file [separator nil] cut copy paste undo
		   [separator nil]   tex next-error view bibtex))
       (TeX-bar-LaTeX-buttons
	(open-file save-buffer write-file [separator nil] cut copy paste undo
		   [separator nil]   latex next-error view bibtex)))))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customize LaTeX parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; the following seem not to be AUCTeX variables ... 
;; obsolete?
;; ;; PDF previewer
;; (setq pdf-previewer-program	"open")

;; ;; PS previewer
;; (setq ps-previewer-program  "open")
			     
;; ;; DVI previewer
;; (setq dvi-previewer-program "open-x11 xdvi")
 
;; The LaTeX commands
;; this is NOT ONLY THE MENU, but also forms commands that 
; AUCTeX relies on. We can't just change the strings here and
; expect that things work - in particular, the default values
; that are set (when reading the command in the input buffer)
; are hard-coded. 

;; modify the default command list

;; This is duplicated from AUCTeX, unfortunately

(aquamacs-set-defaults
 '(
   ;; Directories containing the sites TeX macro files and style files
   (TeX-macro-global ("/usr/local/teTeX/share/texmf/tex/"
		      "/usr/local/teTeX/share/texmf.os/tex/"
		      "/usr/local/teTeX/share/texmf.local/tex/"
		      "~/Library/texmf/tex/"))
;; for TeX-command-list
;; ("XɘLaTeX" "xelatex \"%(mode)\\input{%t}\""
;;      TeX-run-TeX nil (latex-mode context-mode))

; no XDVI on the Mac
; we just use 'open'
; This TeXniscope support requires the option to be specified
; it's unclear whether this should stay that way
   (TeX-output-view-style
    (("^dvi$" "^xdvi$" "open-x11 %(o?)xdvi %dS %d")  ; %(o?) is 'o' if Omega mode
     ("^dvi$" "^TeXniscope$" "open -a TeXniscope.app %o")
     ("^pdf$" "." "open %o")
     ("^html?$" "." "open %o"))
    )))



 



;(setq TeX-command-list
   
 ;;  (list 
;;     (list "-" "" nil nil nil)
;;     (list "Compile LaTeX to PDF (pdfLaTeX)" "pdflatex '\\nonstopmode\\input{%t}'" 'TeX-run-LaTeX nil t)

;;       (list "View PDF" "open %s.pdf"  'TeX-run-command nil t)
      
;;      (list "Compile & View" "pdflatex '\\nonstopmode\\input{%t}';open %s.pdf" 'TeX-run-LaTeX nil t)
;;     (list "-" "" nil nil nil)
;;        (list "Compile with LaTeX" "%l '\\nonstopmode\\input{%t}'" 'TeX-run-LaTeX nil t)  
   
;;       (list "Convert DVI->Postscript..." "dvips %d -o %f " 'TeX-run-command t t )
;;        (list "Convert DVI->PDF..." "dvipdf %d %s.pdf" 'TeX-run-command t t) 
   
;;        (list "View DVI" "open-x11 xdvi %s.dvi..." 'TeX-run-command t t)
;;        (list "View DVI with TeXniscope" "open -a TeXniscope.app %s.dvi..." 'TeX-run-command t t)
;;        (list "View" "open %s.ps" 'TeX-run-command nil t)
;;        ; this comment cannot be called "view PS" 
;;        ; because some defaults in Tex-buf assume it's called "View"
;;      (list "-" "" nil nil nil)
;;       (list "Compile Bibliography (BibTeX)" "bibtex %s" 'TeX-run-BibTeX nil t)
;;       (list "Index" "makeindex %s" 'TeX-run-command nil t)
;;       (list "Check Syntax" "lacheck %s" 'TeX-run-compile nil t)
;;       (list "Spellcheck" "<ignored>" 'TeX-run-ispell-on-document nil t)
;;      (list "-" "" nil nil nil)
;;       (list "Convert to HTML" "htlatex %t;open %s.html" 'TeX-run-command t t) 
;;       (list "Makeinfo" "makeinfo %t" 'TeX-run-compile nil t)
;;       (list "Makeinfo HTML" "makeinfo --html %t" 'TeX-run-compile nil t)
       
;;       )
;; ) 



 

(provide 'auctex-config)
