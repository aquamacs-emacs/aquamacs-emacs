;;; jreport.el - Special code for jreport style.

;; $Id: jreport.el,v 1.4 2007/03/15 19:21:47 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jreport"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))


;;; jreport.el ends here
