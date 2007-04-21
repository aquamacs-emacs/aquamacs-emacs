;; configuration for AUCTeX on OS X

;; Maintainer: David Reitter
;; originally authored by Kevin Walzer
;; Keywords: auctex
 
;; Last change: $Id: auctex-config.el,v 1.25 2007/04/21 13:13:45 davidswelt Exp $

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
 
;; Copyright (C) 2005, Kevin Walzer, David Reitter

  

;; set the PATH to typical locations for TeX stuff

(setenv "PATH"
	(concat (getenv "PATH")
		":/usr/local/teTeX/bin/powerpc-apple-darwin-current"))

;; don't set all of these paths. only what's necessary.
;; everything else should be initialized from PATH anyways
(setq exec-path (append exec-path
			'(;;"/bin"
			  ;"/sbin"
			  ;"/usr/bin"
			  ;"/usr/sbin"
			  ;"/usr/local/bin" 
			  ;"/usr/local/sbin"
			  ;"/sw/bin" 
			  ;"/sw/sbin" 
 			  ;"/usr/libexec"    
 			  ;"/usr/X11R6/bin" 
			  ;"/opt/local/bin"
			  "/usr/local/teTeX/bin/powerpc-apple-darwin-current")))

(load "edit-modes/auctex.el" nil t t)
;; this is not done by default
;; maybe add a menu option?
;;(load "preview-latex.el" nil t t)

(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq bib-highlight-mouse-t t)
(setq bib-cite-use-reftex-view-crossref t)

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


(defvar aquamacs-addl-TeX-bar-TeX-button-alist




(aquamacs-set-defaults 
 '((TeX-PDF-mode t)
;; Toolbar business
   (TeX-bar-TeX-all-button-alists
    (TeX-bar-TeX-button-alist aquamacs-default-toolbarx-meaning-alist))
   (TeX-bar-LaTeX-all-button-alists
    (TeX-bar-LaTeX-button-alist aquamacs-default-toolbarx-meaning-alist))

   (TeX-bar-TeX-buttons
    (open-file save-buffer write-file cut copy paste undo
	       [separator nil]   tex next-error view bibtex))))


 
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
