;; configuration for AUCTeX on OS X

;; Maintainer: David Reitter
;; originally authored by Kevin Walzer
;; Keywords: auctex

;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/


;; Aquamacs Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; Aquamacs Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Aquamacs Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Copyright (C) 2005, Kevin Walzer
;; Copyright (C) 2008, 2009, 2015, David Reitter


;; set the PATH to typical locations for TeX stuff

(mapc (lambda (path)
	(setenv "PATH"
		(concat (getenv "PATH") ":" path))

	(add-to-list 'exec-path path 'append))
      (append
                                        ; prefer TeXLive installation
       '("/usr/texbin")
       '("/Library/TeX/texbin")
                                        ; in case /usr/texbin is missing
       (reverse (sort (file-expand-wildcards
		       "/usr/local/texlive/20*/bin")
		      'string<))
                                        ; over older teTex.
       (reverse (sort (file-expand-wildcards
		       "/usr/local/teTeX/bin/*-apple-darwin-current")
		      'string<))))

;; make sure auctex is loaded from the correct place,
;; i.e. the first file in load-path
;; load the right auctex.el (first one in load-path)
;; (locate-library "auctex.el" t)
(load "auctex" nil nil nil)
(load "preview-latex" nil nil nil)

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

(defun smart-dnd-latex ()
  (smart-dnd-setup
   '(
     ("\\.tex\\'" . "\\input{%r}\n")
     ("\\.cls\\'" . "\\documentclass{%f}\n")
     ("\\.sty\\'" . "\\usepackage{%f}\n")
     ("\\.eps\\'" . "\\includegraphics[]{%r}\n")
     ("\\.ps\\'"  . "\\includegraphics[]{%r}\n")
     ("\\.pdf\\'" . "\\includegraphics[]{%r}\n")
     ("\\.jpg\\'" . "\\includegraphics[]{%r}\n")
     ("\\.png\\'" . "\\includegraphics[]{%r}\n")
     )))

(defun smart-dnd-setup-always-insert-quoted-file-name ()
  "Setup `smart-dnd-mode' so that drag&drop always inserts the file path."
  (smart-dnd-setup '((".*" . "\"%r\""))))

;; non-AUCTeX mode:
(add-hook 'latex-mode-hook 'smart-dnd-latex)
;; AUCTeX:
(defvar LaTeX-mode-hook nil)
(add-hook 'LaTeX-mode-hook 'smart-dnd-latex)

;; only load if ghostscript is installed

;; (aquamacs-latex-find-style-file-paths)
;; (defun aquamacs-latex-find-style-file-paths ()
;;   "Find TeXLive distribution path."
;;   (let ((latex-executable
;; 	 ;; find out the selected TeXlive distribution
;; 	 ;; or another distribution that has a texmf directory
;; 	 (with-temp-buffer
;; 	   (shell-command "which latex " t)
;; 	   (if (string-match "^no " (buffer-string))
;; 	       (message
;; 		"No latex binary found.")
;; 	     (buffer-substring (point-min) (max 0 (- (point-max) (if (eq (char-before (point-max)) 10) 1 0))))))))

;;     ;; how does one print the search path from tex?
;;     (let ((path (file-name-directory (file-truename latex-executable)))
;; 	  (count 0 ))
;;       (while (and (file-exists-p path)
;; 		  (not (equal path "/"))
;; 		  (< count 5)
;; 		  (not (or (file-directory-p (concat path "/texmf" ))
;; 			   (file-directory-p (concat path "/share/texmf" )))))
;; 	(file-truename latex-executable)
;; 	(setq count (1+ count))
;; 	(setq path (file-truename (concat path "/.."))))
;;       (apply #'nconc
;; 	     (mapcar
;; 	      (lambda (file)
;; 		(if (and (> (length file) 1) (file-exists-p file))
;; 		    (list file)))
;; 	      (append (list (concat path "/texmf/tex")
;; 			    (concat path "/texmf-dist/tex")
;; 			    (concat path "/share/texmf/tex") ; used in older teTeX
;; 			    (concat path "/share/texmf.os/tex")
;; 			    (concat path "/share/texmf.local/tex")
;; 			    "/usr/local/texlive/texmf-local/tex"
;; 			    "~/Library/texmf/tex/")
;; 		      (split-string (getenv "TEXINPUTS") ":")
;; 		      ))))))

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
	(load "auctex/preview-latex" nil nil nil)))
    (setq aq-preview-latex-checked t)) nil)

;; must be done here do initialize
;; can't be loaded in hook later on.
(load-preview-if-ghostscript)

;;(autoload 'preview-mode-setup "preview")


(defvar LaTeX-mode-hook nil) ;; make sure it's defined

;; (add-hook 'LaTeX-mode-hook 'load-preview-if-ghostscript)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-bib-cite)
(add-hook 'LaTeX-mode-hook (lambda () (TeX-fold-mode t)))
(add-hook 'TeX-mode-hook 'aquamacs-latex-viewer-support 'append) ;; load reftex first

(aquamacs-set-defaults `((LaTeX-mode-hook ,LaTeX-mode-hook)
			 (TeX-mode-hook ,TeX-mode-hook)))

;; Allow open buffers in latex-mode to be processed identically to new
;; buffers processed according to text-mode-hook (specifically for
;; Flyspell)
(put 'latex-mode 'derived-mode-parent 'text-mode)

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


(defun aquamacs-latex-crossref (ev)
  "Cross-reference in LaTeX.
Jump to definition of reference clicked on, or, if
no reference is found, execute the LaTeX View command."
  (interactive "e")
  (save-excursion
    (mouse-set-point ev)
    (condition-case nil
	(let ((aquamacs-ring-bell-on-error-flag nil))
	  (reftex-view-crossref current-prefix-arg))
      (error
       (TeX-command  "View" 'TeX-master-file)))
    nil))

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
     (cancel-timer aquamacs-skim-timer)
     aquamacs-skim-show-info-message
     (message
      (substitute-command-keys
       (format
	"Skim detected. Use \\[aquamacs-latex-crossref]%s to jump to the PDF and back."
	(if (eq 'control mac-emulate-three-button-mouse)
	    " (Shift-Apple-Click)" ""))))))


;; (defun turn-on-TeX-source-correlate-mode ()
;;   "Turn on `TeX-source-correlate-mode'.
;; Note: this is a global minor mode."
;;   (interactive)
;;   (TeX-source-correlate-mode t))

;; (defun turn-off-TeX-source-correlate-mode ()
;;   "Turn off `TeX-source-correlate-mode'.
;; Note: this is a global minor mode."
;;   (interactive)
;;   (TeX-source-correlate-mode nil))

(defun aquamacs-latex-viewer-support ()
  "Support for Skim as LaTeX viewer if present."
  (add-to-list 'TeX-command-list
	     '("Jump to PDF"
	       "%V" TeX-run-discard-or-function nil t :help "Run Viewer") 'append)

  (and (boundp 'reftex-mode-map) reftex-mode-map
       (define-key reftex-mode-map [(shift mouse-2)] nil))

  (and (boundp 'LaTeX-mode-map) LaTeX-mode-map
       (define-key LaTeX-mode-map [(shift mouse-2)]
	 'aquamacs-latex-crossref))
  (unless aquamacs-skim-timer ;; just once per session
    (setq aquamacs-skim-timer
	  (run-with-idle-timer 30 t 'aquamacs-check-for-skim)))
  (TeX-source-correlate-mode 1) ;; FIXME: this is a global mode.  Should start in auctex.el?
  ;; This may start a latex process to determine whether to use synctex.
  (unless server-process
    (server-force-delete)
    ;; start server to make emacsclient work
    (server-start)))

(require 'server)

(aquamacs-set-defaults
 '((TeX-view-program-list
    (("Preview" "open -a Preview.app %o")
     ;;       ("Skim"  "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")
     ;;       ("Skim"   "%(Ad)/Contents/MacOS/bin/displayline -b %n %o %b")
     ("Skim" "(lambda () (aquamacs-call-viewer %n \"%b\"))")))
   (TeX-view-predicate-list
    ((output-pdf-skim-running
      (and (string-match "pdf" (TeX-output-extension))
	   (aquamacs-skim-running-p)))))
   (TeX-view-program-selection
    ((output-dvi "open")
     (output-pdf-skim-running "Skim") ; prefer Skim if running
     (output-pdf "open")
     (output-html "open")))

   ;; (LaTeX-command "latex --file-line-error -synctex=1")
   ;; Directories containing the sites TeX macro files and style files
   ;; AucTeX defines its own (TeX-macro-global), which serves the same function
   ;; (TeX-macro-global ,(aquamacs-latex-find-style-file-paths))

;; for TeX-command-list
;; ("XɘLaTeX" "xelatex \"%(mode)\\input{%t}\""
;;      TeX-run-TeX nil (latex-mode context-mode))

; no XDVI on the Mac
; we just use 'open'
; This TeXniscope support requires the option to be specified
; it's unclear whether this should stay that way
   ;; (TeX-output-view-style
   ;;  (("^dvi$" "^xdvi$" "open-x11 %(o?)xdvi %dS %d")  ; %(o?) is 'o' if Omega mode
   ;;   ("^dvi$" "^TeXniscope$" "open -a TeXniscope.app %o")
   ;;   ("^pdf$" "." "open %o")
   ;;   ("^html?$" "." "open %o"))
   ;;  )
   ))


; (setq aquamacs-tex-pdf-viewer "Skim")
(defvar aquamacs-tex-pdf-viewer "Skim"
  "External viewer for `aquamacs-call-viewer' and `aquamacs-latex-crossref'.")
(defvar aquamacs-skim-show-reading-bar t
  "Show Skim's `reading bar' when syncronizing LaTeX/PDF files.
This will increase visibility.
Set to t to unconditionally show it.  Set to nil to never show it.
Otherwise, leave it to Skim.")

(defun aquamacs-call-viewer (line source)
  "Display current output file as PDF at LINE (as in file SOURCE).
Calls `aquamacs-tex-pdf-viewer' to display the PDF file using the
Skim AppleScript protocol."
  (let* ((full-file-name
          (expand-file-name
           ;; as in TeX-view
           ;; C-c C-c view uses %o (from TeX-expand-list), which
           ;; is the same.
           (TeX-active-master (TeX-output-extension))
           default-directory))
         (full-source-name
          (expand-file-name
           source ;; this is relative to the master
           (file-name-directory full-file-name))))
    (do-applescript
     (format
      "
 set theSink to POSIX file \"%s\"
 set theSource to POSIX file \"%s\"
 tell application \"%s\"
     activate
     open theSink
     tell front document to go to TeX line %d from theSource%s
  end tell
"
      full-file-name full-source-name aquamacs-tex-pdf-viewer line
      ;; do not say "showing reading bar false" so users can override in future
      (cond ((eq t aquamacs-skim-show-reading-bar)
	     " showing reading bar true")
	    ((eq nil aquamacs-skim-show-reading-bar)
	     " showing reading bar false")
	    (t ""))))))

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

(setq preview-tb-icon-specs
  `((:type tiff :file ,(concat data-directory "images/" "prvtex24.tiff"))))

(provide 'auctex-config)
