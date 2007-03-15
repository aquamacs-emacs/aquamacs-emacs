;;; dutch.el - Setup AUC TeX for editing Dutch text.

;; $Id: dutch.el,v 1.4 2007/03/15 19:21:35 davidswelt Exp $

;;; Code:

(TeX-add-style-hook "dutch"
 (function (lambda ()
   (run-hooks 'TeX-language-nl-hook))))

;;; dutch.el ends here
