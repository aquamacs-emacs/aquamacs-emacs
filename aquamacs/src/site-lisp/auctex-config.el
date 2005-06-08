;; configuration for AUCTeX on OS X
;; contributed by Kevin Walzer 04/2005


;; set the PATH to typical locations for TeX stuff

(setenv "PATH"
	(concat (getenv "PATH")

		":/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:/usr/local/teTeX/bin/powerpc-apple-darwin-current:/sw/bin:/sw/sbin:/usr/libexec:/usr/X11R6/bin:/opt/local/bin"))

(setq exec-path (append exec-path
			'("/bin"
			  "/sbin"
			  "/usr/bin"
			  "/usr/sbin"
			  "/usr/local/bin" 
			  "/usr/local/sbin"
			  "/sw/bin" 
			  "/sw/sbin" 
 			  "/usr/libexec"    
 			  "/usr/X11R6/bin" 
			  "/opt/local/bin"
			  "/usr/local/teTeX/bin/powerpc-apple-darwin-current")))

(require 'tex-site nil t) 



(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq bib-highlight-mouse-t t)
(setq bib-cite-use-reftex-view-crossref t)

(autoload 'turn-on-bib-cite "bib-cite")
(load "preview-latex.el" nil t t)

(add-hook 'LaTeX-mode-hook '(lambda ()
			      (turn-on-reftex)
			      (turn-on-bib-cite)
			      (TeX-fold-mode 1)
			      (setq ispell-parser 'tex)
			      (flyspell-mode)
			      (abbrev-mode)
			      (LaTeX-install-toolbar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customize LaTeX parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Directories containing the sites TeX macro files and style files
(setq TeX-macro-global '("/usr/local/teTeX/share/texmf/tex/"
			 "/usr/local/teTeX/share/texmf.os/tex/"
			 "/usr/local/teTeX/share/texmf.local/tex/"
			 "~/Library/texmf/tex/"))

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

; no XDVI on the Mac
; we just use 'open'
; This TeXniscope support requires the option to be specified
; it's unclear whether this should stay that way
(setq TeX-output-view-style
'(("^dvi$" "^xdvi$" "open-x11 %(o?)xdvi %dS %d")  ; %(o?) is 'o' if Omega mode
  ("^dvi$" "^TeXniscope$" "open -a TeXniscope.app %o")
  ("^pdf$" "." "open %o")
  ("^html?$" "." "open %o"))
)

; the following doesn't seem to work
; why?

(TeX-global-PDF-mode t)


(provide 'auctex-config)
