;;; report.el - Special code for report style.

;; $Id: report.el,v 1.5 2008/02/25 18:02:11 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "report"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; report.el ends here
