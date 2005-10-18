;;; jsarticle.el - Special code for jsarticle style.

;; $Id: jsarticle.el,v 1.1 2005/10/18 08:34:29 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jsarticle"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; jsarticle.el ends here
