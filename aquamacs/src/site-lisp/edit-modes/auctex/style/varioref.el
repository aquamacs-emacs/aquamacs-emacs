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

(defvar LaTeX-varioref-package-options '("draft" "final" "afrikaans" 
				       "american" "austrian" "naustrian"
				       "brazil" "breton" "catalan" "croatian"
				       "czech" "danish" "dutch" "english"
				       "esperanto" "finnish" "french"
				       "galician" "german" "ngerman" "greek"
				       "italian" "magyar" "norsk" "nynorsk"
				       "polish" "portuges" "romanian"
				       "russian" "slovak" "slovene"
				       "spanish" "swedish" "turkish"
				       "francais" "germanb")
  "Package options for the varioref package.")

;; varioref.el ends here
