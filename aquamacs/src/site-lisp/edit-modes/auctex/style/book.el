;;; book.el - Special code for book style.

;; $Id: book.el,v 1.4 2007/03/15 19:21:26 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "book"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; book.el ends here
