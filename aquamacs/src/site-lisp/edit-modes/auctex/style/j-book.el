;;; j-book.el - Special code for j-book style.

;; $Id: j-book.el,v 1.4 2007/03/15 19:21:43 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "j-book"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))

;;; j-book.el ends here
