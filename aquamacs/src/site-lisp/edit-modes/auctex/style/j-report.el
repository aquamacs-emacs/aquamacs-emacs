;;; j-report.el - Special code for j-report style.

;; $Id: j-report.el,v 1.2 2005/12/19 13:01:32 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "j-report"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))

;;; j-report.el ends here
