;;; jsbook.el - Special code for jsbook style.

;; $Id: jsbook.el,v 1.1 2005/10/18 08:34:29 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "jsbook"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; jsbook.el ends here
