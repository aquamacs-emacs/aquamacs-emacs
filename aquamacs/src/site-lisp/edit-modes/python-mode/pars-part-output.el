;;; pars-part-output.el --- `parse-partial-sexp' and `syntax-ppss'


;; Author: Andreas Roehler <andreas.roehler@easy-emacs.de>, unless indicated otherwise

;; Keywords: tools, lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Prints commented output 

;; (global-set-key (kbd "<M-f6>") 'parse-partial-sexp-iac)
;; (global-set-key (kbd "<M-f7>") 'syntax-ppss-iac)

(defun parse-partial-sexp-commentstop (&optional arg) 
  "Interactive form of parse-partial-sexp
output listed with documentation"
  (interactive "P")
  (save-excursion 
    (ppse-documented-base 'parse-partial-sexp (point) arg t)))

(defun parse-partial-sexp-iac (&optional arg) 
  "Interactive form of parse-partial-sexp
output listed with documentation"
  (interactive "P")
  (save-excursion 
    (ppse-documented-base 'parse-partial-sexp (point) arg nil)))

(defun syntax-ppss-iac (&optional arg) 
  "Syntax-ppss made interactive output listed with documentation"
  (interactive "P")
  (ppse-documented-base 'syntax-ppss (point) arg nil)) 

(defun ppse-documented-base (funktion end arg commentstop)
  "Parse partial symbolic expression
list results below its documentation "
  (let* ((start (point-min)) 
	 (scan (condition-case nil
                   (scan-lists (point) 1 0)
                 (error nil)))
	 (rekord
	  (if
	      (or (eq this-command 'parse-partial-sexp-iac)
                  (eq this-command 'parse-partial-sexp-commentstop))
	      (funcall funktion start end nil nil nil commentstop)
	    (funcall funktion end)))
	 (doku (documentation 'parse-partial-sexp))
	 (doku-abr (list (substring doku (1+ (string-match ":" doku))))))
    (if arg
	(what-cursor-position)
      (message "%s LEND: %s" rekord scan))
    (with-output-to-temp-buffer (concat (format "%s" funktion) "-output")
      (set-buffer standard-output)
      (insert (car doku-abr))
      (goto-char (point-min))
      (re-search-forward "^ [0-9]+\." nil t 1)
      (replace-match "-")
      (forward-line 1) 
      (split-line)
      (dolist (elt rekord)
	(insert (format "\t ====> %s <====" elt))
	(re-search-forward "^ [0-9]+\." nil t 1)
	(replace-match "-")
	(end-of-line) 
	(newline))))
  (goto-char (point-min))
  (toggle-read-only -1)
  (unless (featurep 'xemacs)
    (set-window-text-height (selected-window) 4))
  (while (not (eobp))
    (if (looking-at "^[ \t]*$")
	(delete-region (point) (progn (forward-line) (point)))
      (forward-line))))

(defun scan-lists-iac ()
  " "
  (interactive)
  (message "%s" (scan-lists (defun-beginning-position) (point)  0))) 

(provide 'pars-part-output)
;;; pars-part-output.el ends here
