;;; dutch.el - Setup AUC TeX for editing Dutch text.

;; $Id: dutch.el,v 1.5 2008/02/25 18:02:09 davidswelt Exp $

;;; Code:

(TeX-add-style-hook "dutch"
 (function (lambda ()
   (run-hooks 'TeX-language-nl-hook))))

;;; dutch.el ends here
