;;; jsbook.el - Special code for jsbook style.

;; $Id: jsbook.el,v 1.4 2007/03/15 19:21:48 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jsbook"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; jsbook.el ends here
