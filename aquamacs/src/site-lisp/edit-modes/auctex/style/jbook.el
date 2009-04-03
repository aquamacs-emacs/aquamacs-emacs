;;; jbook.el - Special code for jbook style.

;; $Id: jbook.el,v 1.5 2008/02/25 18:02:10 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jbook"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))

;;; jbook.el ends here
