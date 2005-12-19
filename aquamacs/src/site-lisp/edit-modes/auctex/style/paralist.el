;;; paralist.el -- AUCTeX style for paralist.sty

;; License:  GPL, see the file COPYING in the base directory of AUCTeX
;; Author:   Ralf Angeli <angeli@iwi.uni-sb.de>
;; Created:  2003-10-22
;; Keywords: tex

;;; Commentary:

;; This file adds support for `paralist.sty'.

;;; Code:

;; Insert an itemize-ish environment and ask for an optional label
(defun LaTeX-paralist-env-item-opt-label (environment)
  "Insert ENVIRONMENT, an optional label and the first item."
  (LaTeX-insert-environment
   environment
   (let ((label (read-string "(Optional) Label: ")))
     (concat (unless (zerop (length label))
               (format "[%s]" label)))))
  (LaTeX-find-matching-begin)
  (end-of-line 1)
  (delete-char 1)
  (delete-horizontal-space)
  (LaTeX-insert-item))

(TeX-add-style-hook
 "paralist"
 (lambda ()

   ;; Add compactdesc to the list of environments which have an optional
   ;; argument for each item.
   (add-to-list 'LaTeX-item-list '("compactdesc" . LaTeX-item-argument))

   ;; New symbols
   (TeX-add-symbols
    '("pointedenum")
    '("pointlessenum")
    '("paradescriptionlabel")
    '("setdefaultitem" "First level" "Second level" "Third level"
      "Fourth level")
    '("setdefaultenum" "First level" "Second level" "Third level"
      "Fourth level")
    '("setdefaultleftmargin" "First level" "Second level" "Third level"
      "Fourth level" "Fifth level" "Sixth level"))

   ;; New environments
   (LaTeX-add-environments
    '("asparaenum" LaTeX-paralist-env-item-opt-label)
    '("inparaenum" LaTeX-paralist-env-item-opt-label)
    '("compactenum" LaTeX-paralist-env-item-opt-label)
    '("asparaitem" LaTeX-paralist-env-item-opt-label)
    '("inparaitem" LaTeX-paralist-env-item-opt-label)
    '("compactitem" LaTeX-paralist-env-item-opt-label)
    '("compactdesc" LaTeX-env-item)
    ;; FIXME: Should not be available if package is loaded with option
    ;; `olditem':
    '("itemize" LaTeX-paralist-env-item-opt-label)
    ;; FIXME: Should not be available if package is loaded with option
    ;; `oldenum':
    '("enumerate" LaTeX-paralist-env-item-opt-label)
    ;; FIXME: Only defined if package is loaded with option
    ;; `defblank':
    '("asparablank" LaTeX-env-item)
    '("inparablank" LaTeX-env-item))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (setq font-latex-match-variable-keywords-local
	   (append font-latex-match-variable-keywords-local
		   '("setdefaultitem"
		     "setdefaultenum"
		     "setdefaultleftmargin")))
     (font-latex-match-variable-make))))

(defvar LaTeX-paralist-package-options '("newitem" "olditem" "newenum"
					 "oldenum" "alwaysadjust"
					 "neveradjust" "neverdecrease"
					 "increaseonly" "defblank"
					 "pointedenum" "pointlessenum"
					 "cfg" "nocfg" "flushright"
					 "flushleft")
  "Package options for the paralist package.")

;;; paralist.el ends here