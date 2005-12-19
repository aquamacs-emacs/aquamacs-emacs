;;; dutch.el - Setup AUC TeX for editing Dutch text.

;; $Id: dutch.el,v 1.2 2005/12/19 13:01:32 davidswelt Exp $

;;; Code:

(TeX-add-style-hook "dutch"
 (function (lambda ()
   (run-hooks 'TeX-language-nl-hook))))

;;; dutch.el ends here
