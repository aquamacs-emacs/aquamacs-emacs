;;; j-article.el - Special code for j-article style.

;; $Id: j-article.el,v 1.4 2007/03/15 19:21:42 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "j-article"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; j-article.el ends here
