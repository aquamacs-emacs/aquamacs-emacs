;;; jreport.el - Special code for jreport style.

;; $Id: jreport.el,v 1.5 2008/02/25 18:02:10 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jreport"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))


;;; jreport.el ends here
