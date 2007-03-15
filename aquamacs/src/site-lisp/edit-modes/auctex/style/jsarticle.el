;;; jsarticle.el - Special code for jsarticle style.

;; $Id: jsarticle.el,v 1.4 2007/03/15 19:21:48 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jsarticle"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; jsarticle.el ends here
