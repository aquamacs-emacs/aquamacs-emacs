;;; jarticle.el - Special code for jarticle style.

;; $Id: jarticle.el,v 1.2 2005/12/19 13:01:32 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jarticle"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; jarticle.el ends here
