;;; article.el - Special code for article style.

;; $Id: article.el,v 1.4 2007/03/15 19:21:25 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "article"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; article.el ends here
