;;; jreport.el - Special code for jreport style.

;; $Id: jreport.el,v 1.1 2005/10/18 08:34:29 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jreport"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))


;;; jreport.el ends here
