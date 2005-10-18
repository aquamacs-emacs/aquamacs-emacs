;;; book.el - Special code for book style.

;; $Id: book.el,v 1.1 2005/10/18 08:34:29 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "book"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; book.el ends here
