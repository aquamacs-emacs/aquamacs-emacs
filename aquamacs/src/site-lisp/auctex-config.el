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

;; PDF previewer
(setq pdf-previewer-program	"open")

;; PS previewer
(setq ps-previewer-program  "open")
			     

;; DVI previewer
(setq dvi-previewer-program "open-x11 xdvi")

;; The LaTeX command menu, completely customisable
(setq TeX-command-list
      (list 
    (list "-" "" nil nil nil)
    (list "Compile LaTeX to PDF (pdfLaTeX)" "pdflatex '\\nonstopmode\\input{%t}'" 'TeX-run-LaTeX nil t)

      (list "View PDF" "open %s.pdf"  'TeX-run-command nil t)
     (list "Compile & View" "pdflatex '\\nonstopmode\\input{%t}';open %s.pdf" 'TeX-run-LaTeX nil t)
    (list "-" "" nil nil nil)
       (list "Compile with LaTeX" "%l '\\nonstopmode\\input{%t}'" 'TeX-run-LaTeX nil t)  
   
      (list "Convert DVI->Postscript..." "dvips %d -o %f " 'TeX-run-command t t )
       (list "Convert DVI->PDF..." "dvipdf %d %s.pdf" 'TeX-run-command t t) 
   
       (list "View DVI" "open-x11 xdvi %s.dvi..." 'TeX-run-command t t)
       (list "View DVI with TeXniscope" "open -a TeXniscope.app %s.dvi..." 'TeX-run-command t t)
       (list "View PostScript" "open %s.ps" 'TeX-run-command nil t)
 
     (list "-" "" nil nil nil)
      (list "Compile Bibliography (BibTeX)" "bibtex %s" 'TeX-run-BibTeX nil t)
      (list "Index" "makeindex %s" 'TeX-run-command nil t)
      (list "Check Syntax" "lacheck %s" 'TeX-run-compile nil t)
      (list "Spellcheck" "<ignored>" 'TeX-run-ispell-on-document nil t)
     (list "-" "" nil nil nil)
      (list "Convert to HTML" "htlatex %t;open %s.html" 'TeX-run-command t t) 
      (list "Makeinfo" "makeinfo %t" 'TeX-run-compile nil t)
      (list "Makeinfo HTML" "makeinfo --html %t" 'TeX-run-compile nil t)
       
      ))


(provide 'auctex-config)
