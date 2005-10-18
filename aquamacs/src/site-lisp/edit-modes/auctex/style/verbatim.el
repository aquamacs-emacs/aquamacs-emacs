;;; verbatim.el --- Style hook for the verbatim package.
;;;
;;; AUTHOR: Masayuki Ataka <ataka@milk.freemail.ne.jp>
;;; CREATED: 2001/05/01

;;; Commentary:
;;  M-x TeX-auto-generate verbatim.sty makes garbages.

;;; Code

(TeX-add-style-hook "verbatim"
 (function
  (lambda ()
    (LaTeX-add-environments
     "comment")
    (TeX-add-symbols
     '("verbatiminput" TeX-arg-file)))))

;;; verbatim.el ends here.
