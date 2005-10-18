;;; article.el - Special code for article style.

;; $Id: article.el,v 1.1 2005/10/18 08:34:29 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "article"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; article.el ends here
