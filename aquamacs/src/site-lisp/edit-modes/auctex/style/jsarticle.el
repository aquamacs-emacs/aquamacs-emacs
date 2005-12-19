;;; jsarticle.el - Special code for jsarticle style.

;; $Id: jsarticle.el,v 1.2 2005/12/19 13:01:32 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jsarticle"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; jsarticle.el ends here
