;;; jbook.el - Special code for jbook style.

;; $Id: jbook.el,v 1.4 2007/03/15 19:21:46 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jbook"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))

;;; jbook.el ends here
