;;; j-report.el - Special code for j-report style.

;; $Id: j-report.el,v 1.5 2008/02/25 18:02:10 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "j-report"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))

;;; j-report.el ends here
