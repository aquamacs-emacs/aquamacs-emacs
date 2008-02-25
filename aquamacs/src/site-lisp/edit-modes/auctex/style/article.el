;;; article.el - Special code for article style.

;; $Id: article.el,v 1.5 2008/02/25 18:02:08 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "article"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; article.el ends here
