;;; danish.el --- Setup AUCTeX for editing Danish text.

;;; Code:

(TeX-add-style-hook
 "danish"
 (lambda ()
   (setq TeX-quote-language `("danish" "\"`" "\"'" ,TeX-quote-after-quote))
   (setq LaTeX-babel-hyphen-language "danish")
   (run-hooks 'TeX-language-dk-hook)))

;;; danish.el ends here
