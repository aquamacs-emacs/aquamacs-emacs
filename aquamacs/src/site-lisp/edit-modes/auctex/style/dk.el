;;; dk.el - Setup AUC TeX for editing Danish text.

;; $Id: dk.el,v 1.5 2008/02/25 18:02:09 davidswelt Exp $

;;; Code:

(TeX-add-style-hook "dk"
 (function (lambda ()
   (run-hooks 'TeX-language-dk-hook))))

;;; dk.el ends here
