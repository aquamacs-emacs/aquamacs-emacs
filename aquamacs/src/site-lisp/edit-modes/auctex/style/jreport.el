;;; jreport.el - Special code for jreport style.

;; $Id: jreport.el,v 1.2 2005/12/19 13:01:32 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jreport"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))


;;; jreport.el ends here
