;;; jbook.el - Special code for jbook style.

;; $Id: jbook.el,v 1.2 2005/12/19 13:01:32 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jbook"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))

;;; jbook.el ends here
