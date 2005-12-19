;;; dk.el - Setup AUC TeX for editing Danish text.

;; $Id: dk.el,v 1.2 2005/12/19 13:01:32 davidswelt Exp $

;;; Code:

(TeX-add-style-hook "dk"
 (function (lambda ()
   (run-hooks 'TeX-language-dk-hook))))

;;; dk.el ends here
