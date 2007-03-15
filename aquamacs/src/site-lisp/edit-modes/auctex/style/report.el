;;; report.el - Special code for report style.

;; $Id: report.el,v 1.4 2007/03/15 19:22:00 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "report"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; report.el ends here
