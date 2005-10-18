;;; tex-fold.el --- Fold TeX macros.

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-07-04
;; Keywords: tex, wp

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; This file provides support for hiding and unhiding TeX, LaTeX,
;; ContTeXt, Texinfo and similar macros and environments inside of
;; AUCTeX.
;;
;; Caveats:
;;
;; The display string of content which should display part of itself
;; is made by copying the text from the buffer together with its text
;; properties.  If fontification has not happened when this is done
;; (e.g. because of lazy or just-in-time font locking) the intended
;; fontification will not show up.  Maybe this could be improved by
;; using some sort of "lazy folding" or refreshing the window upon
;; scrolling.  As a workaround fontification of the whole buffer
;; currently is forced before folding it.

;;; Code:

(when (featurep 'xemacs)
  (require 'overlay))
(require 'tex)
(autoload 'LaTeX-forward-paragraph "latex")
(autoload 'LaTeX-backward-paragraph "latex")
(autoload 'LaTeX-find-matching-begin "latex")
(autoload 'LaTeX-find-matching-end "latex")
(autoload 'ConTeXt-find-matching-start "context")
(autoload 'ConTeXt-find-matching-stop "context")
(autoload 'Texinfo-find-env-start "tex-info")
(autoload 'Texinfo-find-env-end "tex-info")

(defgroup TeX-fold nil
  "Fold TeX macros."
  :group 'AUCTeX)

(defcustom TeX-fold-macro-spec-list
  '(("[f]" ("footnote"))
    ("[c]" ("cite"))
    ("[l]" ("label"))
    ("[r]" ("ref"))
    ("[i]" ("index"))
    ("*" ("item"))
    ("..." ("dots"))
    (1 ("part" "chapter" "section" "subsection" "subsubsection"
	"paragraph" "subparagraph"
	"part*" "chapter*" "section*" "subsection*" "subsubsection*"
	"paragraph*" "subparagraph*"
	"emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt"
	"textbf" "textsc" "textup")))
  "List of display strings and macros to fold."
  :type '(repeat (group (choice (string :tag "Display String")
				(integer :tag "Number of argument" :value 1))
			(repeat :tag "Macros" (string))))
  :group 'TeX-fold)

(defcustom TeX-fold-env-spec-list
  '(("[comment]" ("comment")))
  "List of display strings and environments to fold."
  :type '(repeat (group (choice (string :tag "Display String")
				(integer :tag "Number of argument" :value 1))
			(repeat :tag "Environments" (string))))
  :group 'TeX-fold)

(defcustom TeX-fold-unspec-macro-display-string "[m]"
  "Display string for unspecified macros.
This string will be displayed if a single macro is being hidden
which is not specified in `TeX-fold-macro-spec-list'."
  :type '(string)
  :group 'TeX-fold)

(defcustom TeX-fold-unspec-env-display-string "[env]"
  "Display string for unspecified environments.
This string will be displayed if a single environment is being
hidden which is not specified in `TeX-fold-env-spec-list'."
  :type '(string)
  :group 'TeX-fold)

(defcustom TeX-fold-unspec-use-name t
  "If non-nil use the name of an unspecified item as display string.
Set it to nil if you want to use the values of the variables
`TeX-fold-unspec-macro-display-string' or
`TeX-fold-unspec-env-display-string' respectively as a display
string for any unspecified macro or environment."
  :type 'boolean
  :group 'TeX-fold)

(defcustom TeX-fold-preserve-comments nil
  "If non-nil do not fold in comments."
  :type 'boolean
  :group 'TeX-fold)

(defcustom TeX-fold-unfold-around-mark t
  "Unfold text around the mark, if active."
  :type 'boolean
  :group 'TeX-fold)

(defcustom TeX-fold-help-echo-max-length 70
  "Maximum length of help echo message for folded overlays.
Set it to zero in order to disable help echos."
  :type 'integer
  :group 'TeX-fold)

(defcustom TeX-fold-force-fontify t
  "Force the buffer to be fully fontified by folding it."
  :group 'TeX-fold
  :type 'boolean)

(defface TeX-fold-folded-face
  '((((class color) (background light))
     (:foreground "SlateBlue"))
    (((class color) (background dark))
     (:foreground "SlateBlue1"))
    (((class grayscale) (background light))
     (:foreground "DimGray"))
    (((class grayscale) (background dark))
     (:foreground "LightGray"))
    (t (:slant italic)))
  "Face for the display string of folded content."
  :group 'TeX-fold)

(defvar TeX-fold-folded-face 'TeX-fold-folded-face
  "Face for the display string of folded content.")

(defface TeX-fold-unfolded-face
  '((((class color) (background light))
     (:background "#f2f0fd"))
    (((class color) (background dark))
     (:background "#38405d"))
    (((class grayscale) (background light))
     (:background "LightGray"))
    (((class grayscale) (background dark))
     (:background "DimGray"))
    (t (:inverse-video t)))
  "Face for folded content when it is temporarily opened."
  :group 'TeX-fold)

(defvar TeX-fold-unfolded-face 'TeX-fold-unfolded-face
  "Face for folded content when it is temporarily opened.")

(defvar TeX-fold-ellipsis "..."
  "String used as display string for overlays instead of a zero-length string.")

(defvar TeX-fold-open-spots nil)
(make-variable-buffer-local 'TeX-fold-open-spots)

(defvar TeX-fold-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-o\C-o" 'TeX-fold-dwim)
    (define-key map "\C-c\C-o\C-b" 'TeX-fold-buffer)
    (define-key map "\C-c\C-o\C-r" 'TeX-fold-region)
    (define-key map "\C-c\C-o\C-p" 'TeX-fold-paragraph)
    (define-key map "\C-c\C-o\C-m" 'TeX-fold-macro)
    (define-key map "\C-c\C-o\C-e" 'TeX-fold-env)
    (define-key map "\C-c\C-ob"    'TeX-fold-clearout-buffer)
    (define-key map "\C-c\C-or"    'TeX-fold-clearout-region)
    (define-key map "\C-c\C-op"    'TeX-fold-clearout-paragraph)
    (define-key map "\C-c\C-oi"    'TeX-fold-clearout-item)
    map))


;;; Folding

(defun TeX-fold-dwim ()
  "Hide or show items according to the current context.
If there is folded content, unfold it.  If there is a marked
region, fold all configured content in this region.  If there is
no folded content but a macro or environment, fold it."
  (interactive)
  (cond ((TeX-fold-clearout-item))
	((TeX-active-mark) (TeX-fold-region (mark) (point)))
	((TeX-fold-item 'macro))
	((TeX-fold-item 'env))))

(defun TeX-fold-buffer ()
  "Hide all configured macros and environments in the current buffer.
The relevant macros are specified in the variable `TeX-fold-macro-spec-list'
and environments in `TeX-fold-env-spec-list'."
  (interactive)
  (TeX-fold-clearout-region (point-min) (point-max))
  (when (and TeX-fold-force-fontify
	     (boundp 'jit-lock-mode)
	     jit-lock-mode
	     (fboundp 'jit-lock-fontify-now))
    ;; We force fontification here only because it should rarely be
    ;; needed for the other folding commands.
    (jit-lock-fontify-now))
  (TeX-fold-region (point-min) (point-max)))

(defun TeX-fold-paragraph ()
  "Hide all configured macros and environments in the current paragraph.
The relevant macros are specified in the variable `TeX-fold-macro-spec-list'
and environments in `TeX-fold-env-spec-list'."
  (interactive)
  (save-excursion
    (let ((end (progn (LaTeX-forward-paragraph) (point)))
	  (start (progn (LaTeX-backward-paragraph) (point))))
      (TeX-fold-clearout-region start end)
      (TeX-fold-region start end))))

(defun TeX-fold-region (start end &optional type)
  "Fold all items in region starting at position START and ending at END.
If optional parameter TYPE is given, fold only items of the
specified type.  TYPE can be one of the symbols 'env for
environments or 'macro for macros."
  (interactive "r")
  (if (null type)
      (progn
	(TeX-fold-region start end 'env)
	(TeX-fold-region start end 'macro))
    (when (or (and (eq type 'env)
		   (not (eq major-mode 'plain-tex-mode)))
	      (eq type 'macro))
      (save-excursion
	(let (fold-list item-list regexp)
	  (dolist (item (if (eq type 'env)
			    TeX-fold-env-spec-list
			  TeX-fold-macro-spec-list))
	    (dolist (i (cadr item))
	      (add-to-list 'fold-list (list i (car item)))
	      (add-to-list 'item-list i)))
	  (setq regexp (cond ((and (eq type 'env)
				   (eq major-mode 'context-mode))
			      (concat (regexp-quote TeX-esc)
				      "start" (regexp-opt item-list t)))
			     ((and (eq type 'env)
				   (eq major-mode 'texinfo-mode))
			      (concat (regexp-quote TeX-esc)
				      (regexp-opt item-list t)))
			     ((eq type 'env)
			      (concat (regexp-quote TeX-esc)
				      "begin[ \t]*{"
				      (regexp-opt item-list t) "}"))
			     (t
			      (concat (regexp-quote TeX-esc)
				      (regexp-opt item-list t)))))
	  (save-restriction
	    (narrow-to-region start end)
	    ;; Start from the bottom so that it is easier to prioritize
	    ;; nested macros.
	    (goto-char (point-max))
	    (let ((case-fold-search nil)
		  item-name)
	      (while (re-search-backward regexp nil t)
		(setq item-name (match-string 1))
		(unless (or (and TeX-fold-preserve-comments
				 (TeX-in-commented-line))
			    ;; Make sure no partially matched macros are
			    ;; folded.  For macros consisting of letters
			    ;; this means there should be none of the
			    ;; characters [A-Za-z@*] after the matched
			    ;; string.  Single-char non-letter macros like
			    ;; \, don't have this requirement.
			    (and (eq type 'macro)
				 (save-match-data
				   (string-match "[A-Za-z]" item-name))
				 (save-match-data
				   (string-match "[A-Za-z@*]"
						 (string (char-after
							  (match-end 0)))))))
		  (let* ((item-start (match-beginning 0))
			 (display-string-spec (cadr (assoc item-name
							   fold-list)))
			 (item-end (TeX-fold-item-end item-start type))
			 (ov (TeX-fold-make-overlay item-start item-end type
						    display-string-spec)))
		    (TeX-fold-hide-item ov)))))))))))

(defun TeX-fold-macro ()
  "Hide the macro on which point currently is located."
  (interactive)
  (unless (TeX-fold-item 'macro)
    (message "No macro found.")))

(defun TeX-fold-env ()
  "Hide the environment on which point currently is located."
  (interactive)
  (unless (TeX-fold-item 'env)
    (message "No environment found.")))

(defun TeX-fold-item (type)
  "Hide the item on which point currently is located.
TYPE specifies the type of item and can be one of the symbols
'env for environments or 'macro for macros.
Return non-nil if an item was found and folded, nil otherwise."
  (if (and (eq type 'env)
	   (eq major-mode 'plain-tex-mode))
      (message
       "Folding of environments is not supported in current mode")
    (let ((item-start (cond ((and (eq type 'env)
				  (eq major-mode 'context-mode))
			     (save-excursion
			       (ConTeXt-find-matching-start) (point)))
			    ((and (eq type 'env)
				  (eq major-mode 'texinfo-mode))
			     (save-excursion
			       (Texinfo-find-env-start) (point)))
			    ((eq type 'env)
			     (condition-case nil
				 (save-excursion
				   (LaTeX-find-matching-begin) (point))
			       (error nil)))
			    (t
			     (TeX-find-macro-start)))))
      (when item-start
	(let* ((item-name (save-excursion
			    (goto-char item-start)
			    (looking-at
			     (cond ((and (eq type 'env)
					 (eq major-mode 'context-mode))
				    (concat (regexp-quote TeX-esc)
					    "start\\([A-Za-z]+\\)"))
				   ((and (eq type 'env)
					 (eq major-mode 'texinfo-mode))
				    (concat (regexp-quote TeX-esc)
					    "\\([A-Za-z]+\\)"))
				   ((eq type 'env)
				    (concat (regexp-quote TeX-esc)
					    "begin[ \t]*{"
					    "\\([A-Za-z]+\\)}"))
				   (t
				    (concat (regexp-quote TeX-esc)
					    "\\([A-Za-z@*]+\\)"))))
			    (if (fboundp 'match-string-no-properties)
				(match-string-no-properties 1)
			      (match-string 1))))
	       (fold-list (if (eq type 'env)
			      TeX-fold-env-spec-list
			    TeX-fold-macro-spec-list))
	       fold-item
	       (display-string-spec
		(or (catch 'found
		      (while fold-list
			(setq fold-item (car fold-list))
			(setq fold-list (cdr fold-list))
			(when (member item-name (cadr fold-item))
			  (throw 'found (car fold-item)))))
		    ;; Item is not specified.
		    (if TeX-fold-unspec-use-name
			(concat "[" item-name "]")
		      (if (eq type 'env)
			  TeX-fold-unspec-env-display-string
			TeX-fold-unspec-macro-display-string))))
	       (item-end (TeX-fold-item-end item-start type))
	       (ov (TeX-fold-make-overlay item-start item-end type
					  display-string-spec)))
	  (TeX-fold-hide-item ov))))))


;;; Utilities

(defun TeX-fold-make-overlay (ov-start ov-end type display-string-spec)
  "Make a TeX-fold overlay extending from OV-START to OV-END.
TYPE is a symbol which is used to describe the content to hide
and may be 'macro for macros and 'env for environments.
DISPLAY-STRING-SPEC is the original specification of the display
string in the variables `TeX-fold-macro-spec-list' or
`TeX-fold-env-spec-list' and may be a string or an integer."
  ;; Calculate priority before the overlay is instantiated.  We don't
  ;; want `TeX-overlay-prioritize' to pick up a non-prioritized one.
  (let ((priority (TeX-overlay-prioritize ov-start ov-end))
	(ov (make-overlay ov-start ov-end (current-buffer) t nil)))
    (overlay-put ov 'category 'TeX-fold)
    (overlay-put ov 'priority priority)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'TeX-fold-type type)
    (overlay-put ov 'TeX-fold-display-string-spec display-string-spec)
    ov))

(defun TeX-fold-item-end (start type)
  "Return the end of an item of type TYPE starting at START.
TYPE can be either 'env for environments or 'macro for macros."
  (save-excursion
    (cond ((and (eq type 'env)
		(eq major-mode 'context-mode))
	   (goto-char start)
	   (ConTeXt-find-matching-stop)
	   (point))
	  ((and (eq type 'env)
		(eq major-mode 'texinfo-mode))
	   (goto-char (1+ start))
	   (Texinfo-find-env-end)
	   (point))
	  ((eq type 'env)
	   (goto-char (1+ start))
	   (LaTeX-find-matching-end)
	   (point))
	  (t
	   (goto-char start)
	   (TeX-find-macro-end)))))

(defun TeX-fold-overfull-p (ov-start ov-end display-string)
  "Return t if an overfull line will result after adding an overlay.
The overlay extends from OV-START to OV-END and will display the
string DISPLAY-STRING."
  (and (not (featurep 'xemacs)) ; Linebreaks in glyphs don't
				; work in XEmacs anyway.
       (save-excursion
	 (goto-char ov-end)
	 (search-backward "\n" ov-start t))
       (not (string-match "\n" display-string))
       (> (+ (- ov-start
		(save-excursion
		  (goto-char ov-start)
		  (line-beginning-position)))
	     (length display-string)
	     (- (save-excursion
		  (goto-char ov-end)
		  (line-end-position))
		ov-end))
	  (current-fill-column))))

(defun TeX-fold-macro-nth-arg (n macro-start &optional macro-end)
  "Return a property list of the argument number N of a macro.
The start of the macro to examine is given by MACRO-START, its
end optionally by MACRO-END.

The first item in the returned list is the string specified in
the argument, the second item may be a face if the argument
string was fontified.  In Emacs the string holds text properties
as well, so the second item is always nil.  In XEmacs the string
does not enclose any faces, so these are given in the second item
of the resulting list."
  (save-excursion
    (let ((macro-end (or macro-end
			 (save-excursion (goto-char macro-start)
					 (TeX-find-macro-end))))
	  content-start content-end)
      (goto-char macro-start)
      (if (condition-case nil
	      (progn
		(while (> n 0)
		  (skip-chars-forward "^{" macro-end)
		  (when (not (looking-at "{")) (error nil))
		  (setq content-start (progn
					(skip-chars-forward "{ \t")
					(point)))
		  (goto-char (TeX-find-closing-brace))
		  (setq content-end (save-excursion
				      (backward-char)
				      (skip-chars-backward " \t")
				      (point)))
		  (setq n (1- n)))
		t)
	    (error nil))
	  (list (TeX-fold-buffer-substring content-start content-end)
		(when (and (featurep 'xemacs)
			   (extent-at content-start))
		  ;; A glyph in XEmacs does not seem to be able to hold more
		  ;; than one face, so we just use the first one we get.
		  (car (extent-property (extent-at content-start) 'face))))
	nil))))

(defun TeX-fold-buffer-substring (start end)
  "Return the contents of buffer from START to END as a string.
Like `buffer-substring' but copy overlay display strings as well."
  ;; Swap values of `start' and `end' if necessary.
  (when (> start end) (let ((tmp start)) (setq start end end tmp)))
  (let ((overlays (overlays-in start end))
	result)
    ;; Get rid of overlays not under our control or not completely
    ;; inside the specified region.
    (dolist (ov overlays)
      (when (or (not (eq (overlay-get ov 'category) 'TeX-fold))
		(< (overlay-start ov) start)
		(> (overlay-end ov) end))
	(setq overlays (remove ov overlays))))
    (if (null overlays)
	(buffer-substring start end)
      ;; Sort list according to ascending starts.
      (setq overlays (sort (copy-sequence overlays)
			   (lambda (a b)
			     (< (overlay-start a) (overlay-start b)))))
      ;; Get the string from the start of the region up to the first overlay.
      (setq result (buffer-substring start (overlay-start (car overlays))))
      (let (ov)
	(while overlays
	  (setq ov (car overlays)
		overlays (cdr overlays))
	  ;; Add the display string of the overlay.
	  (setq result (concat result (overlay-get ov 'display)))
	  ;; Remove overlays contained in the current one.
	  (dolist (elt overlays)
	    (when (< (overlay-start elt) (overlay-end ov))
	      (setq overlays (remove elt overlays))))
	  ;; Add the string from the end of the current overlay up to
	  ;; the next overlay or the end of the specified region.
	  (setq result (concat result (buffer-substring (overlay-end ov)
							(if overlays
							    (overlay-start
							     (car overlays))
							  end))))))
      result)))

(defun TeX-fold-make-help-echo (start end)
  "Return a string to be used as the help echo of folded overlays.
The text between START and END will be used for this but cropped
to the length defined by `TeX-fold-help-echo-max-length'.  Line
breaks will be replaced by spaces."
  (let* ((spill (+ start TeX-fold-help-echo-max-length))
	 (lines (split-string (buffer-substring start (min end spill)) "\n"))
	 (result (pop lines)))
    (dolist (line lines)
      ;; Strip leading whitespace
      (when (string-match "^[ \t]+" line)
	(setq line (replace-match "" nil nil line)))
      ;; Strip trailing whitespace
      (when (string-match "[ \t]+$" line)
	(setq line (replace-match "" nil nil line)))
      (setq result (concat result " " line)))
    (when (> end spill) (setq result (concat result "...")))
    result))

(defun TeX-fold-update-at-point ()
  "Update all TeX-fold overlays at point displaying computed content."
  (let (overlays)
    ;; Get all overlays at point under our control.
    (dolist (ov (overlays-at (point)))
      (when (and (eq (overlay-get ov 'category) 'TeX-fold)
		 (numberp (overlay-get ov 'TeX-fold-display-string-spec)))
	(add-to-list 'overlays ov)))
    (when overlays
      ;; Sort list according to descending starts.
      (setq overlays (sort (copy-sequence overlays)
			   (lambda (a b)
			     (> (overlay-start a) (overlay-start b)))))
      (dolist (ov overlays)
	(TeX-fold-hide-item ov)))))


;;; Removal

(defun TeX-fold-clearout-buffer ()
  "Permanently show all macros in the buffer."
  (interactive)
  (TeX-fold-clearout-region (point-min) (point-max)))

(defun TeX-fold-clearout-paragraph ()
  "Permanently show all macros in the paragraph point is located in."
  (interactive)
  (save-excursion
    (let ((end (progn (LaTeX-forward-paragraph) (point)))
	  (start (progn (LaTeX-backward-paragraph) (point))))
      (TeX-fold-clearout-region start end))))

(defun TeX-fold-clearout-region (start end)
  "Permanently show all macros in region starting at START and ending at END."
  (interactive "r")
  (let ((overlays (overlays-in start end)))
    (TeX-fold-remove-overlays overlays)))

(defun TeX-fold-clearout-item ()
  "Permanently show the macro on which point currently is located."
  (interactive)
  (let ((overlays (overlays-at (point))))
    (TeX-fold-remove-overlays overlays)))

(defun TeX-fold-remove-overlays (overlays)
  "Remove all overlays set by TeX-fold in OVERLAYS.
Return non-nil if a removal happened, nil otherwise."
  (let (found)
    (while overlays
      (when (eq (overlay-get (car overlays) 'category) 'TeX-fold)
	(delete-overlay (car overlays))
	(setq found t))
      (setq overlays (cdr overlays)))
    found))


;;; Toggling

(defun TeX-fold-hide-item (ov)
  "Hide a single macro or environment.
That means, put respective properties onto overlay OV."
  (let* ((ov-start (overlay-start ov))
	 (ov-end (overlay-end ov))
	 (spec (overlay-get ov 'TeX-fold-display-string-spec))
	 (computed (if (stringp spec)
		       spec
		     (or (TeX-fold-macro-nth-arg spec ov-start ov-end)
			 "[Error: No content found]")))
	 (display-string (if (listp computed) (car computed) computed))
	 (face (when (listp computed) (cadr computed))))
    ;; Cater for zero-length display strings.
    (when (string= display-string "") (setq display-string TeX-fold-ellipsis))
    ;; Add a linebreak to the display string and adjust the overlay end
    ;; in case of an overfull line.
    (when (TeX-fold-overfull-p ov-start ov-end display-string)
      (setq display-string (concat display-string "\n"))
      (move-overlay ov ov-start (save-excursion
				  (goto-char ov-end)
				  (skip-chars-forward " \t")
				  (point))))
    (overlay-put ov 'mouse-face 'highlight)
    (overlay-put ov 'display display-string)
    (if (featurep 'xemacs)
	(let ((glyph (make-glyph (if (listp display-string)
				     (car display-string)
				   display-string))))
	  (overlay-put ov 'invisible t)
	  (when font-lock-mode
	    (if face
		(set-glyph-property glyph 'face face)
	      (set-glyph-property glyph 'face TeX-fold-folded-face)))
	  (set-extent-property ov 'end-glyph glyph))
      (when font-lock-mode
	(overlay-put ov 'face TeX-fold-folded-face))
      (unless (zerop TeX-fold-help-echo-max-length)
	(overlay-put ov 'help-echo (TeX-fold-make-help-echo
				    (overlay-start ov) (overlay-end ov)))))))

(defun TeX-fold-show-item (ov)
  "Show a single LaTeX macro or environment.
Remove the respective properties from the overlay OV."
  (overlay-put ov 'mouse-face nil)
  (if (featurep 'xemacs)
      (progn
	(set-extent-property ov 'end-glyph nil)
	(overlay-put ov 'invisible nil))
    (overlay-put ov 'display nil)
    (overlay-put ov 'help-echo nil)
    (when font-lock-mode
      (overlay-put ov 'face TeX-fold-unfolded-face))))

;; Copy and adaption of `reveal-post-command' from reveal.el in GNU
;; Emacs on 2004-07-04.
(defun TeX-fold-post-command ()
  ;; `with-local-quit' is not supported in XEmacs.
  (condition-case nil
      (let ((inhibit-quit nil))
	(condition-case err
	    (let* ((spots (TeX-fold-partition-list
			   (lambda (x)
			     ;; We refresh any spot in the current
			     ;; window as well as any spots associated
			     ;; with a dead window or a window which
			     ;; does not show this buffer any more.
			     (or (eq (car x) (selected-window))
				 (not (window-live-p (car x)))
				 (not (eq (window-buffer (car x))
					  (current-buffer)))))
			   TeX-fold-open-spots))
		   (old-ols (mapcar 'cdr (car spots))))
	      (setq TeX-fold-open-spots (cdr spots))
	      (when (or (and (boundp 'disable-point-adjustment)
			     disable-point-adjustment)
			(and (boundp 'global-disable-point-adjustment)
			     global-disable-point-adjustment)
			;; See preview.el on how to make this configurable.
			(memq this-command (list (key-binding [left])
						 (key-binding [right])
						 'mouse-set-point)))
		;; Open new overlays.
		(dolist (ol (nconc (when (and TeX-fold-unfold-around-mark
					      (boundp 'mark-active)
					      mark-active)
				     (overlays-at (mark)))
				   (overlays-at (point))))
		  (when (eq (overlay-get ol 'category) 'TeX-fold)
		    (push (cons (selected-window) ol) TeX-fold-open-spots)
		    (setq old-ols (delq ol old-ols))
		    (TeX-fold-show-item ol))))
	      ;; Close old overlays.
	      (dolist (ol old-ols)
		(when (and (eq (current-buffer) (overlay-buffer ol))
			   (not (rassq ol TeX-fold-open-spots))
			   (or (not (featurep 'xemacs))
			       (and (featurep 'xemacs)
				    (not (extent-detached-p ol)))))
		  (if (and (>= (point) (overlay-start ol))
			   (<= (point) (overlay-end ol)))
		      ;; Still near the overlay: keep it open.
		      (push (cons (selected-window) ol) TeX-fold-open-spots)
		    ;; Really close it.
		    (TeX-fold-hide-item ol)))))
	  (error (message "TeX-fold: %s" err))))
    (quit (setq quit-flag t))))


;;; Misc

;; Copy and adaption of `cvs-partition' from pcvs-util.el in GNU Emacs
;; on 2004-07-05 to make latex-fold.el mainly self-contained.
(defun TeX-fold-partition-list (p l)
  "Partition a list L into two lists based on predicate P.
The function returns a `cons' cell where the `car' contains
elements of L for which P is true while the `cdr' contains
the other elements.  The ordering among elements is maintained."
  (let (car cdr)
    (dolist (x l)
      (if (funcall p x) (push x car) (push x cdr)))
    (cons (nreverse car) (nreverse cdr))))


;;; The mode

;;; This autoload cookie had to be changed because of XEmacs.  This is
;;; very dissatisfactory, because we now don't have the full doc string
;;; available to tell people what to expect when using this mode
;;; before loading it.

;;;###autoload (autoload 'TeX-fold-mode "tex-fold" "Minor mode for hiding and revealing macros and environments.")
(define-minor-mode TeX-fold-mode
  "Minor mode for hiding and revealing macros and environments.

Called interactively, with no prefix argument, toggle the mode.
With universal prefix ARG (or if ARG is nil) turn mode on.
With zero or negative ARG turn mode off."
  nil nil TeX-fold-keymap
  (if TeX-fold-mode
      (progn
	(set (make-local-variable 'search-invisible) t)
	(add-hook 'post-command-hook 'TeX-fold-post-command nil t)
	(add-hook 'LaTeX-fill-newline-hook 'TeX-fold-update-at-point nil t))
    (kill-local-variable 'search-invisible)
    (remove-hook 'post-command-hook 'TeX-fold-post-command t)
    (remove-hook 'LaTeX-fill-newline-hook 'TeX-fold-update-at-point t)
    (TeX-fold-clearout-buffer))
  (TeX-set-mode-name))

;;;###autoload
(defalias 'tex-fold-mode 'TeX-fold-mode)

(provide 'tex-fold)

;;; tex-fold.el ends here
