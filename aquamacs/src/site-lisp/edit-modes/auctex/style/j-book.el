;;; j-book.el - Special code for j-book style.

;; $Id: j-book.el,v 1.5 2008/02/25 18:02:10 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "j-book"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))

;;; j-book.el ends here
