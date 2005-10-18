;;; slovak.el --- Setup AUCTeX for editing Slovak text.

(TeX-add-style-hook
 "slovak"
 (lambda ()
   (setq TeX-quote-language `("slovak" "\\uv{" "}" ,TeX-quote-after-quote))
   (run-hooks 'TeX-language-sk-hook)))
