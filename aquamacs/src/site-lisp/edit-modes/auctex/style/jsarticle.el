;;; jsarticle.el - Special code for jsarticle style.

;; $Id: jsarticle.el,v 1.5 2008/02/25 18:02:10 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jsarticle"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; jsarticle.el ends here
