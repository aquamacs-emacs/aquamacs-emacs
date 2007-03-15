;;; jarticle.el - Special code for jarticle style.

;; $Id: jarticle.el,v 1.4 2007/03/15 19:21:45 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jarticle"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; jarticle.el ends here
