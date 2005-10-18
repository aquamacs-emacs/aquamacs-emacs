;; AUCTeX style file with support for varioref.sty
;; Author: C. Dominik <dominik@strw.leidenuniv.nl>
;; Last change: 20 Feb 1999

(TeX-add-style-hook "varioref"
   (lambda ()
     
     (TeX-add-symbols

      ;; The macros with label arguments
      '("vref" TeX-arg-label)
      '("vpageref" [ "Same page text" ] [ "different page text" ] TeX-arg-label)
      '("fullref" TeX-arg-label)

      ;; And the other macros used for customization
      "reftextbefore" "reftextfacebefore"
      "reftextafter"  "reftextfaceafter"
      "reftextfaraway" "vreftextvario" "vrefwarning")

     ;; Install completion for labels
     (setq TeX-complete-list
	   (append
	    '(("\\\\vref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
	      ("\\\\vpageref\\(\\[[^]]*\\]\\)*{\\([^{}\n\r\\%,]*\\)" 
	       2 LaTeX-label-list "}"))
	    TeX-complete-list))))

;; varioref.el ends here
