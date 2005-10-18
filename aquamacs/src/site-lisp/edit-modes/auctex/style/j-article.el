;;; j-article.el - Special code for j-article style.

;; $Id: j-article.el,v 1.1 2005/10/18 08:34:29 davidswelt Exp $

;;; Code:

(TeX-add-style-hook
 "j-article"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; j-article.el ends here
