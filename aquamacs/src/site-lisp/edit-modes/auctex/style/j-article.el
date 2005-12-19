;;; j-article.el - Special code for j-article style.

;; $Id: j-article.el,v 1.2 2005/12/19 13:01:32 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "j-article"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; j-article.el ends here
