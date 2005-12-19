;;; jsbook.el - Special code for jsbook style.

;; $Id: jsbook.el,v 1.2 2005/12/19 13:01:32 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jsbook"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; jsbook.el ends here
