;;; j-book.el - Special code for j-book style.

;; $Id: j-book.el,v 1.2 2005/12/19 13:01:32 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "j-book"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))

;;; j-book.el ends here
