;;; j-article.el - Special code for j-article style.

;; $Id: j-article.el,v 1.5 2008/02/25 18:02:10 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "j-article"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; j-article.el ends here
