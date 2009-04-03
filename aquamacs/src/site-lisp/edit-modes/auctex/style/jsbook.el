;;; jsbook.el - Special code for jsbook style.

;; $Id: jsbook.el,v 1.5 2008/02/25 18:02:10 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jsbook"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; jsbook.el ends here
