;;; report.el - Special code for report style.

;; $Id: report.el,v 1.2 2005/12/19 13:01:35 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "report"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; report.el ends here
