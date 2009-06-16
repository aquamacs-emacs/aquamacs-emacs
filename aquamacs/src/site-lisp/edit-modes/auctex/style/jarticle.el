;;; jarticle.el - Special code for jarticle style.

;; $Id: jarticle.el,v 1.5 2008/02/25 18:02:10 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jarticle"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; jarticle.el ends here
