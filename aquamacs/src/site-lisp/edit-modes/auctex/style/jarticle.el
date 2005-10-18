;;; jarticle.el - Special code for jarticle style.

;; $Id: jarticle.el,v 1.1 2005/10/18 08:34:29 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jarticle"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; jarticle.el ends here
