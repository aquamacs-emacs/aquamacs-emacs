;;; dk.el - Setup AUC TeX for editing Danish text.

;; $Id: dk.el,v 1.4 2007/03/15 19:21:34 davidswelt Exp $

;;; Code:

(TeX-add-style-hook "dk"
 (function (lambda ()
   (run-hooks 'TeX-language-dk-hook))))

;;; dk.el ends here
