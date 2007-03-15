;;; j-report.el - Special code for j-report style.

;; $Id: j-report.el,v 1.4 2007/03/15 19:21:44 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "j-report"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))

;;; j-report.el ends here
