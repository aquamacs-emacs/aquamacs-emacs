;;; j-book.el - Special code for j-book style.

;; $Id: j-book.el,v 1.1 2005/10/18 08:34:29 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "j-book"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))

;;; j-book.el ends here
