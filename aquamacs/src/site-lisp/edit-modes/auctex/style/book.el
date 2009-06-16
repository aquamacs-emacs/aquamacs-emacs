;;; book.el - Special code for book style.

;; $Id: book.el,v 1.5 2008/02/25 18:02:08 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "book"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; book.el ends here
