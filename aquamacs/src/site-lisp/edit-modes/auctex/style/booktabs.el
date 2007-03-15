;;; booktabs.el -- AUCTeX style for booktabs.sty

;; License:  GPL, see the file COPYING in the base directory of AUCTeX
;; Author:   Ralf Angeli <angeli@iwi.uni-sb.de>
;; Created:  2003-10-21
;; Keywords: tex

;;; Commentary:

;; This file adds support for `booktabs.sty'.

;;; Code:

(defun LaTeX-booktabs-arg-paren (optional prompt)
  "Prompt for a value and use parentheses when it is inserted.
If OPTIONAL is non-nil the parameter is labeled as optional.
PROMPT is the value of the prompt to be shown."
  (let ((< "\(")
	(> "\)"))
    (TeX-parse-argument optional prompt)))

(TeX-add-style-hook
 "booktabs"
 (lambda ()

   ;; New symbols
   (TeX-add-symbols
    '("toprule" [ "Thickness" ])
    '("midrule" [ "Thickness" ])
    '("bottomrule" [ "Thickness" ])
    ;; FIXME: The qestion for the trim parameter will only be asked if
    ;; a value for the thickness parameter was given.  Is this a
    ;; feature of `TeX-parse-arguments'?
    '("cmidrule" [ "Thickness" ] [ LaTeX-booktabs-arg-paren "Trim" ]
      "Column(s)")
    '("addlinespace" [ "Height" ])
    '("morecmidrules")
    '("specialrule" "Thickness" "Space above" "Space below"))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (setq font-latex-match-function-keywords-local
	   (append font-latex-match-function-keywords-local
		   '("toprule"
		     "midrule"
		     "bottomrule"
		     "cmidrule"
		     "addlinespace"
		     "morecmidrule"
		     "specialrule")))
     (font-latex-match-function-make))))

(defvar LaTeX-booktabs-package-options nil
  "Package options for the booktabs package.")			

;;; booktabs.el ends here
