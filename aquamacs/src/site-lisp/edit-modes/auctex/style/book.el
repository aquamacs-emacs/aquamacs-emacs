;;; book.el - Special code for book style.

;; $Id: book.el,v 1.2 2005/12/19 13:01:32 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "book"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; book.el ends here
