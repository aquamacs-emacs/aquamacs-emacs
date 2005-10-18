;;; foils.el - Special code for FoilTeX.

;; $Id: foils.el,v 1.1 2005/10/18 08:34:29 davidswelt Exp $

;;; Code:

(TeX-add-style-hook "foils"
 (function
  (lambda ()
    (add-hook 'LaTeX-document-style-hook 'LaTeX-style-foils)
    (setq LaTeX-default-style "foils")
    (setq LaTeX-default-options '("landscape"))
    (TeX-add-symbols
     '("foilhead" [ "Rubric-body separation" ] "Foil rubric")))))

(defun LaTeX-style-foils nil
  "Prompt for and insert foiltex options."
  (require 'timezone)
  (let* ((date (timezone-parse-date (current-time-string)))
	 (year   (string-to-int (aref date 0)))
	 (month  (string-to-int (aref date 1)))
	 (day    (string-to-int (aref date 2)))
	 (title (read-input "Title: ")))
    (save-excursion
      (goto-char (point-max))
      (re-search-backward ".begin.document.")
      (insert TeX-esc "title"
	      TeX-grop title TeX-grcl "\n")
      (insert TeX-esc "author"
	      TeX-grop (user-full-name) TeX-grcl "\n")
      (insert TeX-esc "date" TeX-grop
	      (format "%d-%02d-%02d" year month day)
	      TeX-grcl "\n")
      (insert "" TeX-esc "MyLogo" TeX-grop TeX-grcl "\n")
      (insert "%" TeX-esc "Restriction" TeX-grop TeX-grcl "\n")
      (insert "%" TeX-esc "rightfooter" TeX-grop TeX-grcl "\n")
      (insert "%" TeX-esc "leftheader" TeX-grop TeX-grcl "\n")
      (insert "%" TeX-esc "rightheader" TeX-grop TeX-grcl "\n\n")
      (re-search-forward ".begin.document.")
      (end-of-line)
      (newline-and-indent)
      (insert "" TeX-esc "maketitle\n\n"))
    (forward-line -1)))

;;; foils.el ends here
