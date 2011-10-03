;;; tlc --- Major mode for editing tlc files
;;
;; Author: Eric M. Ludlam <eludlam@mathworks.com>
;; Keywords: tlc
;; X-Abstract: Major mode for editing tlc files
;; Version:

(defvar tlc-version "1.2"
  "The current version of TLC mode.")

;;
;; Copyright 1997-2005 The MathWorks, Inc.
;;
;; This program is derived from free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;;  This is a major mode for editing Target Language Compiler scripts.
;;  It automatically indents the programming constructs.
;;

;;; History:
;;
;;  10Sep1998 by Eric M.  Ludlam <eludlam@mathworks.com>
;;    Posted First revision onto the FTP site.
;;
;;  06Oct2005 Peter S galbraith <psg@debian.org>
;;    Minor changes for:
;;    - support customization.
;;    - added autoload cookies.
;;    - CVS storage elsewhere without changing the version number.
;;
;;  Add history items to the ChangeLog.

;;; Code:
(defun tlc-version ()
  "Display the current version of TLC mode."
  (interactive)
  (message tlc-version))

(defgroup tlc nil
  "Major mode for editing tlc files."
  :group 'languages)

(defvar tlc-syntax-table nil
  "Syntax table used in an TLC file.")

(unless tlc-syntax-table
  (setq tlc-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?/  ". 14" tlc-syntax-table)
  (modify-syntax-entry ?%  ". 2356" tlc-syntax-table)
  (modify-syntax-entry ?\n "> b" tlc-syntax-table)
  (modify-syntax-entry ?\" "\"" tlc-syntax-table)
  (modify-syntax-entry ?< "(>" tlc-syntax-table)
  (modify-syntax-entry ?> ")>" tlc-syntax-table)
  )

(defvar tlc-mode-map
  (let ((km  (make-sparse-keymap)))
    (define-key km "\C-m" 'tlc-return)
    (define-key km [return] 'tlc-return)
    (define-key km "\C-i" 'tlc-indent)
    km)
  "Keymap for `tlc-mode'.")

(defvar tlc-font-lock-output-code 'tlc-font-lock-output-code
  "Face for output code.")

(defface tlc-font-lock-output-code
  '((((class grayscale) (background light))
     (:foreground "DimGray" :underline t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :underline t))
    (((class color) (background light)) (:foreground "DarkGreen"))
    (((class color) (background dark))  (:foreground "chartreuse"))
    (t (:underline t)))
  "Font Lock mode face used to highlight tlc keywords."
  :group 'tlc)

(defcustom tlc-keywords
  '("CAST" "EXISTS" "FEVAL" "FILE_EXISTS" "FORMAT"
    "FIELDNAMES" "GETFIELD" "GENERATE"
    "GENERATE_FILENAME" "GENERATE_FORMATTED_VALUE"
    "GENERATE_FUNCTION_EXISTS" "GENERATE_TYPE"
    "GENERATE_TYPE_FUNCTION_EXISTS" "GET_COMMAND_SWITCH"
    "IDNUM" "IMAG"
    "INT8MAX" "INT8MIN"
    "INT16MAX" "INT16MIN"
    "INT32MAX" "INT32MIN"
    "ISEQUAL" "ISFIELD" "ISINF" "ISNAN" "ISFINITE"
    "NULL_FILE" "NUMTLCFILES"
    "OUTPUT_LINES" "SIZE" "STDOUT" "STRING" "STRINGOF"
    "SYSNAME" "TLCFILES" "TLC_TIME"
    "TLC_FALSE" "TLC_TRUE"
    "TLC_VERSION" "TYPE"
    "UINT8MAX" "UINT16MAX" "UINT32MAX"
    "UINTWHITE_SPACE" "WILL_ROLL")
  "Keywords to highlight in TLC."
  :type '(repeat (string :tag "keyword"))
  :group 'tlc)

(defvar tlc-font-lock-keywords
  (list
   ;; Some keywords
   '("^%function\\s-+\\(\\sw+\\)\\s-*(" 1 font-lock-function-name-face)
   '("^%function\\s-+\\(\\sw+\\)\\s-*("
     ("\\s-*\\(\\sw+\\)\\s-*[,)]" nil nil
      (1 font-lock-variable-name-face)))
   '("\\(%%[^\n]*\\)\n" 1 font-lock-comment-face prepend)
   '("\\(^[ \t]*\\([^ \n\t%]\\|%<\\)[^\n]*\\)$" 1 tlc-font-lock-output-code append)
   '("\\(^\\|\\s-\\)\\(%[^% \t(\n>]+\\)\\>" 2 font-lock-keyword-face)
   '("%assign\\s-+:*\\([_a-zA-Z0-9.]+\\)\\s-*\\($\\|=\\)" 1 font-lock-variable-name-face)
   '("%\\(exit\\|warning\\|error\\|trace\\) \\([^\n]+\\)$" 2 font-lock-string-face prepend)
   '("\\(%<[^%\n>]+>\\)" 1 font-lock-reference-face prepend)
   (list (concat "\\<\\(" (regexp-opt tlc-keywords) "\\)\\>")
	 1 'font-lock-type-face)
   '("[^.]\\(\\.\\.\\.\\)$" 1 'underline prepend)
   )
  "List of keywords for nicely coloring X defaults.")

;;;###autoload
(defun tlc-mode ()
  "Major mode for editing Tlc files, or files found in tlc directories."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'tlc-mode)
  (setq mode-name "TLC")
  (use-local-map tlc-mode-map)
  (set-syntax-table tlc-syntax-table)
  (make-variable-buffer-local 'comment-start-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-multi-line)
  (setq comment-start "/% "
	comment-end   " %/"
	comment-multi-line t)
  (setq comment-start-skip "%%\\|/%")
  (make-variable-buffer-local 'font-lock-comment-start-regexp)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'tlc-indent)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((tlc-font-lock-keywords
			      )
			     nil ; do not do string/comment highlighting
			     nil ; keywords are case sensitive.
			     ;; This puts _ as a word constituant,
			     ;; simplifying our keywords significantly
			     ((?_ . "w"))))
  (tlc-version)
  )

(defun tlc-return ()
  "Handle carriage return in `tlc-mode'."
  (interactive)
  (newline)
  (tlc-indent))

(defun tlc-indent ()
  "Indent the current line to the indentation of the previous line."
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (indent-to (tlc-calc-indentation))
  )

(defun tlc-calc-indentation ()
  "Calculate the indentation of this line."
  (beginning-of-line)
  (let ((i (cond
	    ((looking-at
	      "\\s-*\\(\\(\\(%end\\(roll\\|with\\|if\\|for\\|\
foreach\\|while\\|function\\)\\|%else\\|%elseif\\|%case\\|%default\\)\\>\\)\
\\|}\\)")
	     -2)
	    ((looking-at "\\s-*%/")
	     -1)
	    ((looking-at "\\s-*%endswitch")
	     -4)
	    (t 0)))
	(percent (looking-at "\\s-*%"))
	(percent-slash (looking-at "\\s-*%/"))
	(percent-percent (looking-at "\\s-*%%"))
	(indent-because-of-continuation nil))

    (if (bobp) (current-indentation)
      (save-excursion
	(forward-line -1)
	(beginning-of-line)
	(while (and (looking-at "^\\s-*$") (not (bobp))) (forward-line -1))
	(cond ((bobp) nil)
	      ((and percent (looking-at "\\s-*/%"))
	       (setq i (+ (current-indentation) 1)))
	      ((and percent-slash (tlc-in-multiline-comment)
		    (looking-at "\\s-*%"))
	       (setq i (+ (current-indentation) 0)))
	      (t
	       (let* ((nexti (tlc-calc-next-indentation)))
		 (setq i (+ (current-indentation)
			    (if (and indent-because-of-continuation
				     (or (> 0 i) percent-percent))
				i
			      (+ i nexti)))))
	       (if (< i 0) (setq i 0))))
	i))))

(defun tlc-calc-next-indentation ()
  "Calc how much more to indent the next line."
  (+
   (cond ((save-excursion
	    (and (not (tlc-assignment-continuation-p))
		 (tlc-beginning-of-statement))
	    (looking-at "\\s-*\\(\\(%\\(case\\|roll\\|with\\|if\\|for\\|\
foreach\\|while\\|else\\|elseif\\|default\\|function\\)\\>\\)\\|/%\\)"))
	  2)
	 ((looking-at "\\s-*%/")
	  -1)
	 ((looking-at "\\s-*\\(%switch\\)\\>")
	  4)
	 ;((looking-at "\\s-*%break\\>")
	 ; -2)
	 ((and (save-excursion (end-of-line)
			       (or (tlc-assignment-continuation-p)
				   (progn (forward-char -3)
					  (looking-at "\\\\$"))))
	       (save-excursion (forward-line -1)
			       (end-of-line)
			       (not
				(or (tlc-assignment-continuation-p)
				    (progn (forward-char -3)
					   (looking-at "\\\\$"))))))
	  (setq indent-because-of-continuation t)
	  2)
	 ((or (save-excursion (end-of-line)
			      (= (preceding-char) ?{))
	      )
	  2)
	 (t 0))
   (if (and (not (tlc-line-special))
	    (not (save-excursion (end-of-line)
				 (or (tlc-assignment-continuation-p)
				     (progn (forward-char -3)
					    (looking-at "\\\\$")))))
	    (save-excursion (forward-line -1)
			    (end-of-line)
			    (or (tlc-assignment-continuation-p)
				(progn (forward-char -3)
				       (looking-at "\\\\$")))))
       -2
     0)))

(defun tlc-beginning-of-statement ()
  "Goto the beginning of a statement, skipping over continuation lines."
  (beginning-of-line)
  (if (not (save-excursion (forward-line -1) (tlc-assignment-continuation-p)))
      nil
    (forward-line -1)
    (while (tlc-assignment-continuation-p)
      (forward-line -1))
    (forward-line 1)
    (beginning-of-line)))

(defun tlc-line-special ()
  "Return t if the current line is a special language line."
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (looking-at "\\s-*\\(%[^<]\\|}\\)"))))

(defun tlc-assignment-continuation-p ()
  "See if continuation lines should be indented."
  (save-excursion
    (beginning-of-line)
    (and (progn (end-of-line) (forward-char -3) (looking-at "\\.\\.\\.")))))

(defun tlc-in-multiline-comment ()
  "Return t we are in a multiline comment."
  (save-excursion
    (save-match-data
      (if (re-search-backward "/%\\|%/" nil t)
	  (if (looking-at "/%")
	      t
	    nil)
	nil))))

;;; Add to mode list
;;;###autoload(add-to-list 'auto-mode-alist '("\\.tlc$" .tlc-mode))
(add-to-list 'auto-mode-alist '("\\.tlc$" .tlc-mode))

(provide 'tlc)

;;; tlc.el ends here
