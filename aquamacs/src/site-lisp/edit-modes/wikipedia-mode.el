;;; wikipedia-mode.el --- Mode for editing Wikipedia articles off-line
;; Copyright (C) 2003, 2004, 2006 Chong Yidong, Uwe Brauer

;; Author: Chong Yidong <cyd at stupidchicken com>
;; Maintainer: Uwe Brauer <oub at mat.ucm.es>
;; Version: 0.5(a)
;; Keywords: wiki

;; small change for Aquamacs: use word-wrap rather than longlines-mode.

;; This file is not part of GNU Emacs.
;; This file is part of Aquamacs Emacs.

;;{{{ GPL2

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;}}}

;;; Commentary:

;; This is `wikipedia-mode', a major mode for editing articles written
;; in the markup language used by Wikipedia, the free on-line
;; encyclopedia (http://www.wikipedia.org). It is intended to work
;; with GNU Emacs 21.x, and Xemacs 21.4.x. See below for details.

;; wikipedia mode can be found also at: 
;; http://en.wikipedia.org/wiki/Wikipedia:Wikipedia-mode.el

;;{{{ INSTALLING WIKIPEDIA-MODE

;; Installing wikipedia-mode
;; =========================
;;
;; Save wikipedia-mode.el in a convenient directory, preferably in
;; your `load-path'. Add the following to your `user-init-file':
;;
;;   (autoload 'wikipedia-mode
;;     "wikipedia-mode.el"
;;     "Major mode for editing documents in Wikipedia markup." t)
;;
;; If you did not save wikipedia-mode.el in your `load-path', you must
;; use the full pathname. On MS Windows, use forward slashes (/)
;; rather than back slashes (\) to indicate the directory, e.g.:
;;
;;   (autoload 'wikipedia-mode
;;     "C:/Documents and Settings/USERNAME/.emacs.d/Wikipedia-mode.el"
;;     "Major mode for editing documents in Wikipedia markup." t)
;;
;; If you want to associate filenames ending in ".wiki" with
;; wikipedia-mode, add the following to your init file:
;;
;;   (setq auto-mode-alist
;;     (cons '("\\.wiki\\'" . wikipedia-mode) auto-mode-alist))

;;}}}

;;{{{ REQUIREMENTS

;; This  is not a real requirements but I highly recommend to use 
;; outline-magic written by Carsten Dominik. If you don't want to use it  
;; you have to comment out the relevant reference to outline magic.
;; It can be found at 
;; http://www.astro.uva.nl/~dominik/Tools/outline-magic.el




;;}}}

;;{{{ RECOMMENDATIONS INSTALLING LONGLINES-MODE

;; Installing longlines-mode
;; =========================
;;
;; Wikipedia articles don't use newline characters to break paragraphs
;; into lines, so each paragraph looks like a super-long line to
;; Emacs. To let Emacs handle "soft word wrapping", you need to
;; download a third-party package, longlines-mode.
;;
;; Download longlines.el, saving into your `load-path':
;;
;;   http://www.emacswiki.org/elisp/longlines.el
;;
;; Add the following to your `user-init-file':
;;
;;   (autoload 'longlines-mode "longlines.el"
;;     "Minor mode for editing long lines." t)
;;
;;
;; WARNING: if you insert text from one file in wikipedia-mode to
;; another file in wikipedia-mode I strongly recommend, to turn
;; longlines-mode off, before the copying!

;;}}}

;;{{{ RECOMMENDATIONS INSTALLING PABBREV-MODE

;; Installing longlines-mode
;; =========================
;;
;; You may find pabbrev.el useful, which can be found at 
;; http://www.russet.org.uk/download/emacs/pabbrev.el


;;}}}

;;{{{ Xemacs or (GNU) Emacs

;; Xemacs or (GNU) Emacs
;; =====================
;; Usually that is a question of taste. However almost all wikipedia
;; articles nowadays use UTF8 coding, so the question which of the
;; Macsen to use, boils down to which degree UTF8 support is
;; implemented (no mule Xemacs is ruled out). While Xemacs has the
;; better font support, the UTF8 support still is not complete and
;; hence at the time being it is sad for the maintainer (a long time
;; Xemacs user) to recommend NOT to use Xemacs, even not 21.5.x, which
;; has a much better implemented UTF8 coding engine. That might
;; however change in the foreseeable future....
;; WARNING: at least for me in Debian testing/unstable Emacs does not
;; ship all fonts necessary for a flawless editing of UTF8  files. For
;; example you can chose Greek input, write Greek text, but then when
;; you close and open the file again, the Greek symbol are not
;; displayed but you see empty blocks. The reason seems that emacs
;; chooses for the input fonts other fonts as for the display (don't
;; ask me). However for installing the (ugly) UTF8 compatible fonts
;; from ..... solved that problem.


;;}}}

;;{{{ INSTALLING EE-HELPER or MOZEX

;; Installing the helper programs.
;; =========================
;; Helper Programs: MozEx and EE-HELPER. There are two possibilities
;; in order to use Emacs as an external editor
;;
;;     (1) EE-HELPER: This is perl script which will communicate with
;;         the  wikipedia server. However that sometimes be slow.

;;         PROS: if the editor supports UTF8, then ee-helper will
;;               pass the coding flawlessly.
;; 
;;         CONTRA: the problem with this script is that it directly
;;                 communicates with the wikipedia site and does not
;;                 warn you about simultaneous editing. Use it with
;;                 care!!! Moreover section editing is not implemented.

;;     (2) MozEx: this is a Java-script which allows to communicate
;;         Mozilla (or Firefox) directly with Emacs.

;;         PROS: After finishing editing you use the wikipedia
;;               software to submit your changes and not the script,
;;               so you are warned about possible conflicting editing.
;; 
;;         CONTRA: the official version does not support UTF8,
;;                 however there is now a new semi official version which
;;                 does support UTF8.


;; 
;; Installing ee-helper
;; ====================
;; 
;; Download   the perl script  from 
;;
;;   http://meta.wikimedia.org/wiki/Help:External_editors
;;
;; and follow the instructions. configure the .ee-ini file.  chance in
;; your personal wikipedia-mode-map account setting the editing
;; functions: activate the `external editor' option.

;; Installing MozEx
;; ================
;;
;; If your web browser is Mozilla or Firefox, take a look at the MozEx
;; extension, which allows you to call Emacs for editing text boxes:
;;
;;   http://mozex.mozdev.org/development.html
;;
;; See also
;;
;;   http://www.emacswiki.org/cgi-bin/wiki/FireFox
;;
;; If you mostly use MozEx to edit Wikipedia articles, it might be
;; worthwhile to tell Emacs to enter wikipedia-mode whenever it is
;; called by MozEx. Just add this to your `user-init-file':
;;
;;   (add-to-list 'auto-mode-alist '("mozex.\\.*" . wikipedia-mode))

;;     Recall: you have to click on edit (either edit article or edit
;;             section), then use mouse3 (or shift f10), then select
;;             mozex, then edit textarea: Edit-->mouse3-->mozex-->Edit
;;             Textarea. After editing, you have to _click_ on the
;;             text in the browser otherwise Mozilla will ignore your
;;             typing.

;;}}}

;;{{{ NEWS


;; NEWS
;; ==================================
;;     (1) Font setting has changed.
;;     (2) Some makeup formats have been added: italics, bold, strong
;;         emphasise, links.
;;     (3) outline-cycle from Carsten Dominiks outline-magic has been
;;         added.
;;     (4) "Draft", "send" and "reply" (for discussion pages)
;;         abilities `based' on ideas of John Wigleys remember.el: see
;;         the functions wikipedia-draft-*
;;         RATIONALE: This comes handy in 2 situations
;;            1. You are editing articles which various authors (this I
;;               think is the usual case), you then want not to submit
;;               your edit immediately but want to copy it somewhere and
;;               to continue later. You can use the following functions
;;               for doing that:
;;               wikipedia-draft-buffer \C-c\C-b
;;               wikipedia-draft-region \C-c\C-r
;;               then the buffer/region will be appended to the
;;               wikipedia-draft-data-file (default is
;;               "~/Wiki/discussions/draft.wiki", which you can visit via
;;               wikipedia-draft-view-draft) and it will be
;;               surrounded by the ^L marks in order to set a page.
;;               moreover on top on that a section header == will be
;;               inserted, which consists of the Word Draft, a subject
;;               you are asked for and a date stamp.
;; 
;;               Another possibility consists in using the function
;;               wikipedia-draft, bound to \C-c \C-m then a new buffer
;;               will opened already in wikipedia mode. You edit and then
;;               either can send the content of the buffer to the
;;               wikipedia-draft-data-file in the same manner as
;;               described above using the function
;;               wikipedia-draft-buffer (bound to \C-c\C-k)
;; 
;;               BACK: In order to copy/send the content of temporary
;;               buffer or of a page in the wikipedia-draft-data-file
;;               back in to your wikipedia file, use the function
;;               wikipedia-send-draft-to-mozex bound to "\C-c\C-c". You
;;               will be asked to which buffer to copy your text!
;; 
;; 
;;            2. You want to reply  in a discussion page to a specific
;;               contribution, you can use either the function
;; 
;;               \\[wikipedia-reply-at-point-simple] bound to [(meta shift r)]
;;               which inserts a newline, a hline, and the signature of
;;               the author. Or can use 
;;               \\[wikipedia-draft-reply] bound  [(meta r)]
;;               which does the same as wikipedia-reply-at-point-simple
;;               but in a temporary draft buffer.
;; 
;;               BACK: In order to copy/send the content of that buffer
;;               back in to your wikipedia file, use the function
;;               \\[wikipedia-send-draft-to-mozex] bound to "\C-c\C-c". You
;;               will be asked to which buffer to copy your text! If
;;               you want a copy to be send to your draft file, use
;;               the variable  wikipedia-draft-send-archive 
;; 

;;}}}
          
;;{{{ NEW FUNCTIONS AND VARIABLES


;; VERSION 0.4
;;==================
;; NEW FUNCTIONS
;; ------------------
;; wikipedia-insert-enumerate 
;; wikipedia-insert-itemize 
;; wikipedia-insert-strong-emphasis 
;; wikipedia-insert-bold 
;; wikipedia-insert-italics 
;; wikipedia-insert-header 
;; wikipedia-insert-link 
;; wikipedia-turn-on-outline-minor-mode 
;; wikipedia-insert-signature 
;; wikipedia-insert-hline 
;; wikipedia-unfill-paragraph-or-region 
;; wikipedia-start-paragraph 
;; wikipedia-hardlines 
;; wikipedia-outline-magic-keys 
;; wikipedia-enhance-indent 
;; wikipedia-yank-prefix 
;; wikipedia-simple-outline-promote 
;; wikipedia-simple-outline-demote 
;; wikipedia-next-long-line 
;; wikipedia-unfill-paragraph 
;; wikipedia-rename-buffer 
;; wikipedia-draft 
;; wikipedia-draft-buffer-desc 
;; wikipedia-draft-append-to-file 
;; wikipedia-draft-page 
;; wikipedia-draft-region (&optional beg end)
;; wikipedia-draft-buffer  
;; wikipedia-draft-clipboard 
;; wikipedia-draft-mode 
;; wikipedia-draft-view-draft 
;; wikipedia-mark-section 
;; wikipedia-activate-region 
;; wikipedia-copy-page-to-register 
;; wikipedia-insert-page-to-register 
;; wikipedia-send-draft-to-mozex (target-buffer)
;; wikipedia-reply-at-point-simple 
;; wikipedia-draft-reply
;; wikipedia-insert-quotation-with-signature
;; wikipedia-insert-quotation

;; NEW VARIABLES
;;---------------------
;; wikipedia-enumerate-with-terminate-paragraph 
;; wikipedia-draft-buffer "*Wikipedia-Draft*"
;; wikipedia-draft-mode-map 
;; wikipedia-draft-mode-hook 
;; wikipedia-draft-register ?R
;; wikipedia-draft-filter-functions 
;; wikipedia-draft-handler-functions '(wikipedia-draft-append-to-file)
;; wikipedia-draft-data-file "~/Wiki/discussions/draft.wiki"
;; wikipedia-draft-leader-text "== "
;; wikipedia-draft-page ?S
;; wikipedia-draft-send-archive 
;; wikipedia-reply-with-quote 


;; VERSION 0.5
;;====================================
;; NEW FUNCTIONS
;; ------------------------------------
;;  wikipedia-insert-audio 
;;  wikipedia-insert-bible-verse-template 
;;  wikipedia-insert-bible-verse-template-old 
;;  wikipedia-insert-image 
;;  wikipedia-insert-link-www 
;;  wikipedia-insert-user 
;;  wikipedia-mark-signature
;;  wikipedia-outline-cycle 
;;  wikipedia-reply-at-signature
;;  wikipedia-terminate-paragraph-and-indent 	
;;  wikipedia-yank-prefix

;; NEW VARIABLES (defvar, defcustom, defconst)
;; ----------------------
;; wikipedia-reply-with-hline 
;; wikipedia-user-simplify-signature
;; wikipedia-english-or-german 
;; wikipedia-draft-reply-register ?M
;; wikipedia-mode-version  

;;}}}

;;{{{ TODO

;; Todo
;; ----


;; * Implement TeX highlighting in <math> environment
;; * Implement (La)TeX input syntax, following the ideas of CDlatex.el  
;; * Make outline-cycle work correctly
;; * wikipedia-reply-at-point-simple should use regexp!

;;}}}



;;; Code:

(require 'derived)
(require 'font-lock)
	
(defconst wikipedia-mode-version (concat "0." (substring "$Revision: 1.2 $" 13 14))
	"$Id: wikipedia-mode.el,v 1.2 2008/11/07 13:40:39 davidswelt Exp $

Report bugs to: Uwe Brauer oub at mat.ucm.es")


;;{{{ TAGS

(defvar wikipedia-simple-tags
  '("b" "big" "blockquote" "br" "caption" "code" "center" "cite" "del"
    "dfn" "dl" "em" "i" "ins" "kbd" "math" "nowiki" "ol" "pre" "samp"
    "small" "strike" "strong" "sub" "sup" "tt" "u" "ul" "var")
  "Tags that do not accept arguments.")

(defvar wikipedia-complex-tags
  '("a" "div" "font" "table" "td" "th" "tr")
  "Tags that accept arguments.")

(defvar wikipedia-url-protocols
  '("ftp" "gopher" "http" "https" "mailto" "news")
  "Valid protocols for URLs in Wikipedia articles.")

;;}}}

;;{{{ FACES

(defvar font-wikipedia-sedate-face			'font-wikipedia-sedate-face
  "Face to use for Wikipedia minor keywords.")

(defvar font-wikipedia-italic-face			'font-wikipedia-italic-face
  "Face to use for Wikipedia italics.")
(defvar font-wikipedia-bold-face			'font-wikipedia-bold-face
  "Face to use for Wikipedia bolds.")
(defvar font-wikipedia-math-face			'font-wikipedia-math-face
  "Face to use for Wikipedia math environments.")
(defvar font-wikipedia-string-face                  'font-wikipedia-string-face
  "Face to use for strings.  This is set by Font Wikipedia.")
(defvar font-wikipedia-verbatim-face                'font-wikipedia-verbatim-face
  "Face to use for text in verbatim macros or environments.")




(defface font-wikipedia-bold-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
		    ((assq :weight custom-face-attributes) '(:weight bold))
		    (t '(:bold t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in bold."
  :group 'font-wikipedia-highlighting-faces)

(defface font-wikipedia-italic-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
		    ((assq :slant custom-face-attributes) '(:slant italic))
		    (t '(:italic t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in italic."
  :group 'font-wikipedia-highlighting-faces)

(defface font-wikipedia-math-face
  (let ((font (cond ((assq :inherit custom-face-attributes)
		     '(:inherit underline))
		    (t '(:underline t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown"))
      (((class color) (background dark))
       (:foreground "burlywood"))
      (t (,@font))))
  "Face used to highlight math."
  :group 'font-wikipedia-highlighting-faces)

(defface font-wikipedia-sedate-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark))  (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "DimGray"))
    (((class color) (background dark))  (:foreground "LightGray"))
   ;;;(t (:underline t))
    )
  "Face used to highlight sedate stuff."
  :group 'font-wikipedia-highlighting-faces)

(defface font-wikipedia-string-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
		    ((assq :slant custom-face-attributes) '(:slant italic))
		    (t '(:italic t)))))
    `((((type tty) (class color))
       (:foreground "green"))
      (((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "RosyBrown"))
      (((class color) (background dark))
       (:foreground "LightSalmon"))
      (t (,@font))))
  "Face used to highlight strings."
  :group 'font-wikipedia-highlighting-faces)

(defface font-wikipedia-warning-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
		    ((assq :weight custom-face-attributes) '(:weight bold))
		    (t '(:bold t)))))
    `((((class grayscale)(background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale)(background dark))
       (:foreground "LightGray" ,@font))
      (((class color)(background light))
       (:foreground "red" ,@font))
      (((class color)(background dark))
       (:foreground "red" ,@font))
      (t (,@font))))
  "Face for important keywords."
  :group 'font-wikipedia-highlighting-faces)

(defface font-wikipedia-verbatim-face
  (let ((font (if (and (assq :inherit custom-face-attributes)
		       (if (featurep 'xemacs)
			   (find-face 'fixed-pitch)
			 (facep 'fixed-pitch)))
		  '(:inherit fixed-pitch)
		'(:family "courier"))))
    `((((class grayscale) (background light))
	 (:foreground "DimGray" ,@font))
	(((class grayscale) (background dark))
	 (:foreground "LightGray" ,@font))
	(((class color) (background light))
	 (:foreground "SaddleBrown" ,@font))
	(((class color) (background dark))
	 (:foreground "burlywood" ,@font))
	(t (,@font))))
  "Face used to highlight TeX verbatim environments."
  :group 'font-wikipedia-highlighting-faces)


  (defvar wikipedia-font-lock-keywords
	(list

	 ;; Apostrophe-style text markup
	 (cons "''''\\([^']\\|[^']'\\)*?\\(''''\\|\n\n\\)"
           'font-lock-builtin-face)
	 (cons "'''\\([^']\\|[^']'\\)*?\\('''\\|\n\n\\)"
;           'font-lock-builtin-face)
	       'font-wikipedia-bold-face)
	 (cons "''\\([^']\\|[^']'\\)*?\\(''\\|\n\n\\)"
           'font-wikipedia-italic-face)

	 ;; Headers and dividers
	 (list "^\\(==+\\)\\(.*\\)\\(\\1\\)"
		   '(1 font-lock-builtin-face)
;		   '(2 wikipedia-header-face)
           '(2 font-wikipedia-sedate-face)
		   '(3 font-lock-builtin-face))
	 (cons "^-----*" 'font-lock-builtin-face)

	 ;; Bare URLs and ISBNs
	 (cons (concat "\\(^\\| \\)" (regexp-opt wikipedia-url-protocols t)
				   "://[-A-Za-z0-9._\/~%+&#?!=()@]+")
		   'font-lock-variable-name-face)
	 (cons "\\(^\\| \\)ISBN [-0-9A-Z]+" 'font-lock-variable-name-face)

	 ;; Colon indentation, lists, definitions, and tables
	 (cons "^\\(:+\\|[*#]+\\||[}-]?\\|{|\\)" 'font-lock-builtin-face)

	 (list "^\\(;\\)\\([^:\n]*\\)\\(:?\\)"
		   '(1 font-lock-builtin-face)
		   '(2 font-lock-keyword-face)
		   '(3 font-lock-builtin-face))



	 ;; Tags and comments

	 (list (concat "\\(</?\\)"
				   (regexp-opt wikipedia-simple-tags t) "\\(>\\)")
		   '(1 font-lock-builtin-face t t)
		   '(2 font-lock-function-name-face t t)
		   '(3 font-lock-builtin-face t t))
	 (list (concat "\\(</?\\)"
				   (regexp-opt wikipedia-complex-tags t)
				   "\\(\\(?: \\(?:[^\"'/><]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?\\)\\(>\\)")
		   '(1 font-lock-builtin-face t t)
		   '(2 font-lock-function-name-face t t)
		   '(3 font-lock-keyword-face t t)
		   '(4 font-lock-builtin-face t t))
	 (cons (concat "<!-- \\([^->]\\|>\\|-\\([^-]\\|-[^>]\\)\\)*-->")
		   '(0 font-lock-comment-face t t))



	 ;; External Links

	 (list (concat "\\(\\[\\)\\(\\(?:"
				   (regexp-opt wikipedia-url-protocols)
				   "\\)://[-A-Za-z0-9._\/~%-+&#?!=()@]+\\)\\(\\(?: [^]\n]*\\)?\\)\\(\\]\\)")
		   '(1 font-lock-builtin-face t t)
		   '(2 font-lock-variable-name-face t t)
		   '(3 font-lock-keyword-face t t)
		   '(4 font-lock-builtin-face t t))




	 ;; Wiki links
	 '("\\(\\[\\[\\)\\([^]\n|]*\\)\\(|?\\)\\([^]\n]*\\)\\(\\]\\]\\)"
	   (1 font-lock-builtin-face t t)
	   (2 font-lock-variable-name-face t t)
	   (3 font-lock-builtin-face t t)
	   (4 font-lock-keyword-face t t)
	   (5 font-lock-builtin-face t t))

	 ;; Wiki variables
	 '("\\({{\\)\\(.+?\\)\\(}}\\)"
	   (1 font-lock-builtin-face t t)
	   (2 font-lock-variable-name-face t t)
	   (3 font-lock-builtin-face t t))

	 ;; Character entity references
	 (cons "&#?[a-zA-Z0-9]+;" '(0 font-lock-type-face t t))

	 ;; Preformatted text
	 (cons "^ .*$" '(0 font-lock-constant-face t t))

	 ;; Math environment (uniform highlight only, no TeX markup)
	 (list "<math>\\(\\(\n?.\\)*\\)</math>"
		   '(1 font-lock-keyword-face t t))))

; )

;;}}}

;;{{{ Menu and header stuff

(defvar wikipedia-imenu-generic-expression
  (list '(nil "^==+ *\\(.*[^\n=]\\)==+" 1))
  "Imenu expression for `wikipedia-mode'.  See `imenu-generic-expression'.")

(defun wikipedia-next-header ()
  "Move point to the end of the next section header."
  (interactive)
  (let ((oldpoint (point)))
    (end-of-line)
    (if (re-search-forward "\\(^==+\\).*\\1" (point-max) t)
        (beginning-of-line)
      (goto-char oldpoint)
      (message "No section headers after point."))))

(defun wikipedia-prev-header ()
  "Move point to the start of the previous section header."
  (interactive)
  (unless (re-search-backward "\\(^==+\\).*\\1" (point-min) t)
    (message "No section headers before point.")))

;;}}}

;;{{{ Paragraph terminate and filling stuff (Chong)

(defun wikipedia-terminate-paragraph ()	;Version:1.58
  "In a list, start a new list item. In a paragraph, start a new
paragraph; if the current paragraph is colon indented, the new
paragraph will be indented in the same way."
  (interactive)
  (let (indent-chars)
    (save-excursion
      (beginning-of-line)
      (while (cond ((looking-at "^$") nil)
                   ((looking-at "^\\(\\(?: \\|:+\\|[#*]+\\) *\\)")
                    (setq indent-chars (match-string 1)) nil)
                   ((eq (point) (point-min)) nil)
                   ((progn (forward-line -1) t)))
        t))
    (newline) (if (not indent-chars) (newline) 
		(insert indent-chars))))

(defun wikipedia-terminate-paragraph-and-indent ()	
  "In a list, start a new list item. In a paragraph, start a new
paragraph but *,# will be ignored; if the current paragraph is colon
; indented, the new paragraph will be indented in the same way."
  (interactive)
  (let (indent-chars)
    (save-excursion
      (beginning-of-line)
      (while (cond ((looking-at "^$") nil)
                   ((looking-at "^\\(\\(?: \\|:+\\) *\\)")
                    (setq indent-chars (match-string 1)) nil)
                   ((eq (point) (point-min)) nil)
                   ((progn (forward-line -1) t)))
        t))
    (newline) (if (not indent-chars) (newline) 
		(insert indent-chars))))


(defun wikipedia-link-fill-nobreak-p ()
  "When filling, don't break the line for preformatted (fixed-width)
text or inside a Wiki link.  See `fill-nobreak-predicate'."
  (save-excursion
    (let ((pos (point)))
      (or (eq (char-after (line-beginning-position)) ? )
          (if (re-search-backward "\\[\\[" (line-beginning-position) t)
              ;; Break if the link is really really long.
              ;; You often get this with captioned images.
              (null (or (> (- pos (point)) fill-column)
                        (re-search-forward "\\]\\]" pos t))))))))

(defun wikipedia-fill-article ()
  "Fill the entire article."
  (interactive)
  (save-excursion
    (fill-region (point-min) (point-max))))

(defun wikipedia-unfill-article ()
  "Undo filling, deleting stand-alone newlines (newlines that do not
end paragraphs, list entries, etc.)"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ".\\(\n\\)\\([^# *;:|!\n]\\|----\\)" nil t)
      (replace-match " " nil nil nil 1)))
  (message "Stand-alone newlines deleted"))


(defun wikipedia-unfill-paragraph-with-newline (&optional justifyp)
  (interactive "P")
  (let ((before (point)))				;Version:1.3
	(save-excursion
	  (forward-paragraph)
	  (or (bolp) (newline 1))
	  (let ((end (point))
			(start (progn (backward-paragraph) (point))))
	    (goto-char before)
    (while (re-search-forward ".\\(\n\\)\\([^# *;:|!\n]\\|----\\)" nil t)
      (replace-match " " nil nil nil 1))))))
;  (message "Stand-alone newlines IN PARAGRAPH deleted"))

(defun wikipedia-unfill-region () 
"Undo filling, deleting stand-alone newlines (newlines that do not end
paragraphs, list entries, etc.) see also the function
\\[wikipedia-unfill-paragraph-or-region] and the even simpler function
\\[wikipedia-unfill-paragraph-simple]."
  (interactive)
  (save-excursion
	(narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (re-search-forward ".\\(\n\\)\\([^# *;:|!\n]\\|----\\)" nil t)
      (replace-match " " nil nil nil 1)))
  (message "Stand-alone newlines deleted")
  (widen))

;;}}}

;;{{{ Main function wikipedia mode (using define-derived mode)

;;;###autoload
;;{{{ Main function wikipedia-mode

(define-derived-mode wikipedia-mode text-mode "Wikipedia"
  "Major mode for editing articles written in the markup language used by
Wikipedia, the free on-line encyclopedia (http://www.wikipedia.org).

There are several ways to use wikipedia-mode. One is to copy articles
between Emacs and your web browser's text box. However for GNU emacs,
that does not work always smoothly, since copying marked regions into
other X applications is somehow buggy for GNU emacs. Another way is to
use MozEx, a Mozilla/Firefox web browser extension that allows you to
call Emacs from a text box (http://mozex.mozdev.org/). Another way is
to use the PERL script ee-helper, which allows you to up and download
wiki texts.

Wikipedia articles are usually unfilled: newline characters are not
used for breaking paragraphs into lines. Unfortunately, Emacs does not
handle word wrapping yet. As a workaround, wikipedia-mode turns on
longlines-mode automatically. In case something goes wrong, the
following commands may come in handy:

\\[wikipedia-fill-article] fills the buffer.
\\[wikipedia-unfill-article] unfills the buffer.
Be warned that function can be dead  slow, better use wikipedia-unfill-paragraph-or-region.
\\[wikipedia-unfill-paragraph-or-region] unfills the paragraph
\\[wikipedia-unfill-paragraph-simple] doehe same but simpler.



The following commands put in markup structures.

\\[wikipedia-insert-strong-emphasis] inserts italics
\\[wikipedia-insert-bold] inserts bold text
\\[wikipedia-insert-italics] italics
\\[wikipedia-insert-header] header
\\[wikipedia-insert-link] inserts a link

The following commands are also defined:
\\[wikipedia-insert-user] inserts user name
\\[wikipedia-insert-signature] inserts ~~~~
\\[wikipedia-insert-enumerate] inserts enumerate type structures
\\[wikipedia-insert-itemize] inserts itemize type structures
\\[wikipedia-insert-hline] inserts a hline

The draft functionality
\\[wikipedia-draft]
\\[wikipedia-draft-region]
\\[wikipedia-draft-view-draft]
\\[wikipedia-draft-page]
\\[wikipedia-draft-buffer]

Replying and sending functionality
\\[wikipedia-reply-at-point-simple]
\\[wikipedia-draft-reply]
\\[wikipedia-send-draft-to-mozex]


The register functionality
\\[wikipedia-copy-page-to-register]
\\[defun wikipedia-insert-page-to-register]


Some simple editing commands.
\\[wikipedia-enhance-indent]
\\[wikipedia-yank-prefix]
\\[wikipedia-unfill-paragraph-or-region]



\\[wikipedia-terminate-paragraph]     starts a new list item or paragraph in a context-aware manner.
\\[wikipedia-next-header]     moves to the next (sub)section header.
\\[wikipedia-prev-header]     moves to the previous (sub)section header."

  (set (make-local-variable 'adaptive-fill-regexp) "[ ]*")
  (set (make-local-variable 'comment-start-skip) "\\(?:<!\\)?-- *")
  (set (make-local-variable 'comment-end-skip) " *--\\([ \n]*>\\)?")
  (set (make-local-variable 'comment-start) "<!-- ")
  (set (make-local-variable 'comment-end) " -->")
  (set (make-local-variable 'paragraph-start)
       "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$")
  (set (make-local-variable 'sentence-end-double-space) nil)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'font-lock-defaults)
       '(wikipedia-font-lock-keywords t nil nil nil))
  (set (make-local-variable 'fill-nobreak-predicate)
       'wikipedia-link-fill-nobreak-p)
  (set (make-local-variable 'auto-fill-inhibit-regexp) "^[ *#:|;]")

  ;; Support for outline-minor-mode. No key conflicts, so we'll use
  ;; the normal outline-mode prefix.
  (set (make-local-variable 'outline-regexp) "==+")
;  (set (make-local-variable 'outline-regexp) "=+")
;  (set (make-local-variable 'outline-regexp) ":")
  (set (make-local-variable 'outline-minor-mode-prefix) "\C-c\C-o")

 
  ;; Turn on the Imenu automatically.
(when menu-bar-mode
    (set (make-local-variable 'imenu-generic-expression)
         wikipedia-imenu-generic-expression)
    (imenu-add-to-menubar "Contents"))

(modify-syntax-entry ?< "(>" wikipedia-mode-syntax-table)
(modify-syntax-entry ?> ")<" wikipedia-mode-syntax-table)

;;}}}

;; KEY SETTING

;;{{{ KEY SETTING

(define-key wikipedia-mode-map "\M-n" 'wikipedia-next-header)
(define-key wikipedia-mode-map "\C-c\C-n" 'wikipedia-next-long-line)
(define-key wikipedia-mode-map "\M-p" 'wikipedia-prev-header)
;  (define-key wikipedia-mode-map [(meta down)] 'wikipedia-next-header)
;  (define-key wikipedia-mode-map [(meta up)]   'wikipedia-prev-header)
(define-key wikipedia-mode-map "\C-j" 'wikipedia-terminate-paragraph)
;    'wikipedia-terminate-paragraph)

  (let ((map (make-sparse-keymap "Wikipedia")))
    (define-key wikipedia-mode-map [menu-bar wikipedia]
      (cons "Wikipedia" map))
    (define-key map [unfill-article]
      '("Unfill article" . wikipedia-unfill-article))
    (define-key map [fill-article]
      '("Fill article" . wikipedia-fill-article))
    (define-key map [separator-fill] '("--"))
    (define-key map [next-header]
      '("Next header" . wikipedia-next-header))
    (define-key map [prev-header]
      '("Previous header" . wikipedia-prev-header))
    (define-key map [separator-header] '("--"))
    (define-key map [outline]
      '("Toggle Outline Mode..." . outline-minor-mode)))


(define-key wikipedia-mode-map "\C-c\C-q"    'wikipedia-unfill-article)
(define-key wikipedia-mode-map "\C-c\M-q"    'wikipedia-fill-article) 
(define-key wikipedia-mode-map "\M-u" 'wikipedia-unfill-paragraph-or-region)
(define-key wikipedia-mode-map "\C-c\C-u" 'wikipedia-unfill-paragraph-simple)
(define-key wikipedia-mode-map "\C-c\C-f\C-s" 'wikipedia-insert-strong-emphasis)
(define-key wikipedia-mode-map "\C-c\C-f\C-b" 'wikipedia-insert-bold) ;Version:1.3
(define-key wikipedia-mode-map "\C-c\C-f\C-i" 'wikipedia-insert-italics) 
(define-key wikipedia-mode-map "\C-c\C-f\C-h" 'wikipedia-insert-header)
(define-key wikipedia-mode-map "\C-c\C-f\C-l" 'wikipedia-insert-link)
(define-key wikipedia-mode-map "\C-c\C-f\C-u" 'wikipedia-insert-user)
(define-key wikipedia-mode-map "\C-c\C-f\C-q" 'wikipedia-insert-quotation)
(define-key wikipedia-mode-map "\C-c\C-f\C-v" 'wikipedia-insert-bible-verse-template)
(define-key wikipedia-mode-map "\C-c\C-w" 'wikipedia-insert-signature)
(define-key wikipedia-mode-map "\C-c\C-h" 'wikipedia-insert-hline) ;Version:1.30
(define-key wikipedia-mode-map [(meta f7)] 'wikipedia-draft)
(define-key wikipedia-mode-map [(meta f8)] 'wikipedia-reply-at-point-simple)
(define-key wikipedia-mode-map [(meta f9)]  'wikipedia-draft-view-draft)
(define-key wikipedia-mode-map "\C-c\C-r"  'wikipedia-reply-at-point-simple)
(define-key wikipedia-mode-map "\C-cr" 'wikipedia-draft-region)
(define-key wikipedia-mode-map [(meta r)]  'wikipedia-draft-reply)
(define-key wikipedia-mode-map "\C-c\C-m" 'wikipedia-draft) ;Version:1.25 
(define-key wikipedia-mode-map "\C-c\C-b" 'wikipedia-draft-region)
(define-key wikipedia-mode-map "\C-c\C-d" 'wikipedia-draft-buffer)
(define-key wikipedia-mode-map "\C-c\C-k" 'wikipedia-draft-buffer)
(define-key wikipedia-mode-map "\C-c\C-p" 'wikipedia-draft-copy-page-to-register)	;Version:1.39
(define-key wikipedia-mode-map "\C-c\C-c" 'wikipedia-draft-send-to-mozex)
(define-key wikipedia-mode-map "\C-c\C-s" 'wikipedia-draft-yank-page-to-register)

(define-key wikipedia-mode-map [(control meta prior)] 'wikipedia-enhance-indent)
(define-key wikipedia-mode-map [(control meta next)] 'wikipedia-yank-prefix)
(define-key wikipedia-mode-map [(meta return)] 'wikipedia-insert-enumerate)
(define-key wikipedia-mode-map [(meta control return)] 'wikipedia-insert-enumerate-nonewline)
;; private setting
(define-key wikipedia-mode-map [(shift return)] 'newline-and-indent) ;Version:1.24
(define-key wikipedia-mode-map "\C-\\" 'wikipedia-insert-itemize) ;Version:1.28
(define-key wikipedia-mode-map [(control return)] 'wikipedia-insert-itemize)
(define-key wikipedia-mode-map "\C-ca" 'auto-capitalize-mode)
(define-key wikipedia-mode-map "\C-ci" 'set-input-method)
(define-key wikipedia-mode-map "\C-ct" 'toggle-input-method) ;Version:1.23

;;}}}


  (make-local-variable 'change-major-mode-hook))

; wikipedia-mode ends here
;;}}}

 ;;{{{ longlines-mode

(defun wikipedia-turn-on-longlines ()	;Version:1.58
  "Turn on longlines-mode or word-wrap if it is defined."
  (if (boundp 'word-wrap)
      (setq word-wrap t)
    (if (functionp 'longlines-mode)
	(longlines-mode 1))))
(add-hook 'wikipedia-mode-hook 'wikipedia-turn-on-longlines)
(set (make-local-variable 'auto-fill-inhibit-regexp) "^[ *#:|;]")

;;}}}

;; New formating stuff for inserting simple formating structures such

;;{{{ Insert makeup and templates

(defvar wikipedia-enumerate-with-terminate-paragraph nil
"*Before insert enumerate/itemize do \\[wikipedia-terminate-paragraph].")

(defun wikipedia-insert-enumerate ()
"Primitive Function for inserting enumerated items, check the
variable wikipedia-enumerate-with-terminate-paragraph. Note however
that the function \\[wikipedia-terminate-paragraph] does not work very
well will longlines-mode."
  (interactive)
  (when wikipedia-enumerate-with-terminate-paragraph 
	  (wikipedia-terminate-paragraph)
	  (insert "#"))
  (when (not wikipedia-enumerate-with-terminate-paragraph)
	(newline nil)
	(insert ":#"))) 





(defun wikipedia-insert-itemize ()
  "Primitive Function for inserting no enumerated items, check the
variable wikipedia-enumerate-with-terminate-paragraph. Note however
that the function \\[wikipedia-terminate-paragraph] does not work very
well will longlines-mode."
  (interactive)
  (when wikipedia-enumerate-with-terminate-paragraph
	(wikipedia-terminate-paragraph)
	(insert "*"))
  (when (not wikipedia-enumerate-with-terminate-paragraph )
	(newline nil)
	(insert ":*")))


(defun wikipedia-insert-strong-emphasis ()
"Insert strong emphasis italics via four apostrophes (e.g. ''''FOO''''.) When mark is active, surrounds region."   
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
		  (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
	  (save-excursion
		(goto-char (point))
		(insert "''''")
		(goto-char (mark))
		(insert "''''"))
	(insert "'''' ''''")
	(backward-char 5)))


(defun wikipedia-insert-bold ()
"Insert bold via three apostrophes (e.g. '''FOO'''.)
When mark is active, surrounds region."   
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
		  (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
	  (save-excursion
		(goto-char (point))
		(insert "'''")
		(goto-char (mark))
		(insert "'''"))
	(insert "''' '''")
	(backward-char 4)))


(defun wikipedia-insert-italics ()
"Insert bold via TWO apostrophes (e.g. ''FOO''.) When mark is active,
surrounds region."   
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
		  (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
	  (save-excursion
		(goto-char (point))
		(insert "''")
		(goto-char (mark))
		(insert "''"))
	(insert "'' ''")
	(backward-char 3)))

(defun wikipedia-insert-quotation-with-signature () ;Version:1.60
"Insert bold via TWO apostrophes (e.g. ''FOO''.) When mark is active,
surrounds region."   
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
		  (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
	  (save-excursion
		(goto-char (point))
		(insert "{{Quotation|}}")
		(goto-char (mark))
		(insert "{{~~~~}}"))
		(insert "{{Quotation| }}{{~~~~}}")
	(backward-sexp 1)
	(backward-char 3)))

(defun wikipedia-insert-quotation ()	;Version:1.60
"Quotation box of the form {{Quotation}}{{}}. When mark is active,
surrounds region."   
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
		  (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
	  (save-excursion
		(goto-char (point))
		(insert "{{Quotation|}}")
		(goto-char (mark))
		(insert "{{}}"))
		(insert "{{Quotation|}}{{ }}")
	(backward-char 3)))




(defun wikipedia-insert-bible-verse-template ()
  "Insert a template for the quotation of bible verses."
  (interactive)
  (insert "({{niv|")
(let ((name    (read-string "Name: ")))
     (insert (concat name "|"))
(let ((verse (read-string "Verse: ")))
  (insert (concat verse "|" name " " verse "}})")))))


(defvar wikipedia-english-or-german t
"*Variable in order to set the english (t) or german (nil) environment.")

(defun wikipedia-insert-user ()
  "Inserts, interactively a user name [[User:foo]]"
  (interactive)
  (when wikipedia-english-or-german 
	(let ((user (read-string "Name of user: " )))
	  (insert (concat "[[User:" user "|" user "]]"))))
  (when (not wikipedia-english-or-german)
	(let ((user (read-string "Name des Benutzers: " )))
	  (insert (concat "[[Benutzer:" user "|" user "]]")))))



(defun wikipedia-insert-reply-prefix () ;Version:1.60
"Quotation box of the form {{Quotation}}{{}}. When mark is active,
surrounds region."   
  (interactive)
  (beginning-of-line 1)
  (search-forward "[[")
  (backward-char 2)
  (mark-sexp 1)
  (copy-to-register wikipedia-draft-reply-register (region-beginning) (region-end) nil) 
  (end-of-line 1)
  (wikipedia-terminate-paragraph)
  (beginning-of-line 1)
  (kill-line nil)
	  (insert "----")
	  (newline 1)
	  (yank)
	  (insert ":'''Re: ")
	  (insert-register wikipedia-draft-reply-register 1)
	  (insert "''' ")
	  (end-of-line 1))

(defun wikipedia-insert-header ()
  "Insert subheader  via  == (e.g. == FOO ==.)"  
  (interactive)
  (insert "==   ==")
  (backward-char 4))

(defun wikipedia-insert-link ()
  "Insert link via [[ (e.g. [[FOO]].) When mark is active, surround region."  
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
		  (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
	  (save-excursion
		(goto-char (point))
		(insert "]]")
		(goto-char (mark))
		(insert "[["))
	(insert "[[ ]]")
	(backward-char 3)))

(defun wikipedia-insert-link-www ()
  "Insert link via [[ (e.g. [http://FOO].) When mark is active, surround region."  
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
		  (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
	  (save-excursion
		(goto-char (point))
		(insert "]")
		(goto-char (mark))
		(insert "[http://"))
	(insert "[http:// ]")
	(backward-char 2)))


(defun wikipedia-insert-image ()
  "Insert link image  [[ (e.g. [[Image:FOO]].) Check the variable 
wikipedia-english-or-german. When mark is active, surround region."  
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
		  (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
	  (save-excursion
		(goto-char (point))
		(insert "]]")
		(goto-char (mark))
		(when (not wikipedia-english-or-german)
		  (insert "[[Bild:"))
		(when wikipedia-english-or-german
		  (insert "[[Image:")))
	(when wikipedia-english-or-german
	  (insert "[[Image: ]]"))
	(when (not wikipedia-english-or-german)
	  (insert "[[Bild: ]]"))
	(backward-char 3)))

(defun wikipedia-insert-audio ()
  "Insert link image  [[ (e.g. [[Image:FOO]].) Check the variable 
wikipedia-english-or-german. When mark is active, surround region."  
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
		  (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
	  (save-excursion
		(goto-char (point))
		(insert "]]")
		(goto-char (mark))
		(when (not wikipedia-english-or-german)
		  (insert "[[Bild:"))
		(when wikipedia-english-or-german
		  (insert "[[Media:")))
	(when wikipedia-english-or-german
	  (insert "[[Media: ]]"))
	(when (not wikipedia-english-or-german)
	  (insert "[[Bild: ]]"))
	(backward-char 3)))




(defun wikipedia-turn-on-outline-minor-mode ()
  "Turn on outline minor mode."
  (interactive)
  (outline-minor-mode nil))


(defun wikipedia-insert-signature ()			;Version:1.4
  "Insert \"~~~~:\"  "
  (interactive)
  (insert "~~~~: "))





(defun wikipedia-insert-hline ()		;Version:1.29
  "Insert \"----\"  "
  (interactive)
  (insert "\n")
  (insert "----")
  (insert "\n"))

;;}}}

;;{{{ filling and longline 

(defun wikipedia-unfill-paragraph-or-region () ;Version:1.7
  "Unfill region, this function does NOT explicitly search for \"soft newlines\"
as does wikipedia-unfill-region."
  (interactive)
  (when use-hard-newlines
	;; 	(backward-paragraph 1)
	;; 	(next-line 1)
	(beginning-of-line 1)
	(set-fill-prefix)
	(set (make-local-variable 'use-hard-newlines) nil)
	(set (make-local-variable 'sentence-end-double-space) t)
	(set (make-local-variable 'paragraph-start)
		 "[ 　	\n]")
	(when  (featurep 'xemacs)	
	  (let ((fill-column (point-max)))
		(fill-paragraph-or-region nil)))
	(unless  (featurep 'xemacs)	
	  (let ((fill-column (point-max)))
		(fill-paragraph nil)))
	(set (make-local-variable 'use-hard-newlines) t)
	(set (make-local-variable 'sentence-end-double-space) nil)
	(set (make-local-variable 'paragraph-start)
		 "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$"))
  (unless use-hard-newlines
	;; 	(backward-paragraph 1)
	;; 	(next-line 1)
	(beginning-of-line 1)
	(set-fill-prefix)
	(set (make-local-variable 'sentence-end-double-space) t)
	(set (make-local-variable 'paragraph-start)
		 "[ 　	\n]")
	(when  (featurep 'xemacs)	
	  (let ((fill-column (point-max)))
		(fill-paragraph-or-region nil)))
	(unless  (featurep 'xemacs)	
	  (let ((fill-column (point-max)))
		(fill-paragraph nil)))
	(set (make-local-variable 'sentence-end-double-space) nil)
	(set (make-local-variable 'paragraph-start)
		 "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$")))

 


(defun wikipedia-start-paragraph ()
  (interactive)
	(set (make-local-variable 'paragraph-start)
       "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$"))


(defun wikipedia-hardlines ()
"Set use-hard-newlines to NIL."
  (interactive)
  (setq use-hard-newlines nil))
;; from emacs wiki 
(defun wikipedia-next-long-line ()
  "Move forward to the next long line with column-width greater
  than `fill-column'.

  TODO: When function reaches end of buffer, save-excursion to
  starting point.
  Generalise to make `previous-long-line'."
  (interactive)
  ;; global-variable: fill-column
  (if (= (forward-line) 0)
	  (let ((line-length
			 (save-excursion
			   (end-of-line)
			   (current-column))))
		(if (<= line-length fill-column)
			(wikipedia-next-long-line)
		  (message "Long line found")))
	;; Stop, end of buffer reached.
 	(error "Long line not found")))


(defun wikipedia-unfill-paragraph-simple ()
"A very simple function for unfilling a paragraph."
  (interactive)
  (if (functionp 'filladapt-mode)
  (filladapt-mode nil))
  (let ((fill-column (point-max)))
    (fill-paragraph nil)
  (if (functionp 'filladapt-mode)
    (filladapt-mode nil))))

;;}}}

;;{{{ outline and outline-magic stuff

(add-hook 'wikipedia-mode-hook 'wikipedia-turn-on-outline-minor-mode)

(defun wikipedia-outline-cycle ()
  (interactive)
  (if (functionp 'outline-cycle)
	  (outline-cycle)))


(add-hook 'outline-minor-mode-hook  'wikipedia-outline-magic-keys)

(defun wikipedia-outline-magic-keys ()
  (interactive)
  (unless  (featurep 'xemacs)
	(local-set-key [(shift iso-lefttab)] 'wikipedia-outline-cycle))
  (local-set-key [iso-left-tab] 'wikipedia-outline-cycle)
  (local-set-key [(meta left)]  'outline-promote)
  (local-set-key [(meta right)] 'outline-demote)
  (local-set-key [(shift return)] 'newline-and-indent)
  (local-set-key [(control left)]  'wikipedia-simple-outline-promote)
  (local-set-key [(control right)] 'wikipedia-simple-outline-demote)
  (local-set-key [(control up)] 'outline-move-subtree-up)
  (local-set-key [(control down)] 'outline-move-subtree-down))

(defun wikipedia-enhance-indent ()				;Version:1.26
  (interactive)
  (string-rectangle (region-beginning) (region-end) ":"))

(defun wikipedia-yank-prefix ()			;Version:1.26
  (interactive)
  (string-rectangle (region-beginning) (region-end) ":"))

;; modification for outline-magic

(defun wikipedia-simple-outline-promote ()
"Function simple deletes \"=\" and the end and the beginning of line,
does not promote the whole tree!"
  (interactive)
  (save-excursion
	(progn
	  (beginning-of-line 1)
	  (search-forward "=")
	  (delete-char 1 nil)
	  (end-of-line 1) 
	  (search-backward "=")
	  (delete-char 1 nil))))

(defun wikipedia-simple-outline-demote ()
"Function simple adds \"=\" and the end and the beginning of line,
does not promote the whole tree!"
  (interactive)
  (save-excursion
	(progn
	  (beginning-of-line 1)
	  (search-forward "=")
	  (insert "=")
	  (end-of-line 1) 
	  (search-backward "=")
	  (insert "="))))


(defun wikipedia-rename-buffer ()				;Version:1.5
  "Make sure that the option UNIQUE is used."
  (interactive)
  (rename-buffer (read-string "Name of new buffer (unique): " ) 1))

;;}}}

;;{{{ wikipedia drafts functionality: `stolen' from remember.el: 

(defgroup wikipedia-draft nil
  "A mode to wikipedia-draft information."
  :group 'data)

;;; User Variables:

(defcustom wikipedia-draft-mode-hook nil
  "*Functions run upon entering wikipedia-draft-mode."
  :type 'hook
  :group 'wikipedia-draft)

(defcustom wikipedia-draft-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'wikipedia-draft)

(defcustom wikipedia-draft-filter-functions nil
  "*Functions run to filter wikipedia-draft data.
All functions are run in the wikipedia-draft buffer."
  :type 'hook
  :group 'wikipedia-draft)

(defcustom wikipedia-draft-handler-functions '(wikipedia-draft-append-to-file)
  "*Functions run to process wikipedia-draft data.
Each function is called with the current buffer narrowed to what the
user wants wikipedia-drafted.
If any function returns non-nil, the data is assumed to have been
recorded somewhere by that function. "
  :type 'hook
  :group 'wikipedia-draft)

(defcustom wikipedia-draft-data-file "~/Wiki/discussions/draft.wiki"
  "*The file in which to store the wikipedia drafts."
  :type 'file
  :group 'wikipedia-draft)

(defcustom wikipedia-draft-reply-register ?M
  "The register in which the window configuration is stored."
  :type 'character
  :group 'wikipedia-draft)

(defcustom wikipedia-draft-page ?S		;Version:1.37
  "The register in which the a page of the wiki draft file is stored."
  :type 'character
  :group 'wikipedia-draft)


(defcustom wikipedia-draft-leader-text "== "
  "*The text used to begin each wikipedia-draft item."
  :type 'string
  :group 'wikipedia-draft)


;;; Internal Variables:

(defvar wikipedia-draft-buffer "*Wikipedia-Draft*"
  "The name of the wikipedia-draft (temporary) data entry buffer.")

;;; User Functions:

;;;###autoload
(defun wikipedia-draft ()
 "Open a temporary buffer in wikipedia mode for editing an wikipedia
 draft, which an arbitrary piece of data. After finishing the editing
 either use C-c C-k \\[wikipedia-draft-buffer] to send the data into
 the wikipedia-draft-data-file, or send  the buffer using C-c C-c
\\[wikipedia-draft-send-to-mozex]  and insert it later into a wikipedia article."
  (interactive)
  (window-configuration-to-register wikipedia-draft-register)
  (let ((buf (get-buffer-create wikipedia-draft-buffer)))
    (switch-to-buffer-other-window buf)
    (wikipedia-mode)
    (message " C-c C-k sends to draft file, C-c C-c sends to org buffer.")))





(defsubst wikipedia-draft-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defsubst wikipedia-draft-mail-date (&optional rfc822-p)
  "Return a simple date.  Nothing fancy."
  (if rfc822-p
      (format-time-string "%a, %e %b %Y %T %z" (current-time))
    (format-time-string "%c" (current-time))))

(defun wikipedia-draft-buffer-desc ()
  "Using the first line of the current buffer, create a short description."
  (buffer-substring (point-min)
		    (save-excursion
		      (goto-char (point-min))
		      (end-of-line)
		      (if (> (- (point) (point-min)) 60)
			  (goto-char (+ (point-min) 60)))
		      (point))))


;; Wikipedia-Drafting to plain files:


(defun wikipedia-draft-append-to-file ()
 "Add a header together with a subject to the text and add it to the
draft file. It might be better if longlines-mode is off."
  (let ((text (buffer-string))
		(desc (wikipedia-draft-buffer-desc)))
    (with-temp-buffer
	  (insert "\n\n")
	  (insert wikipedia-draft-leader-text)
	  (insert "Draft: ")				;Version:1.39
	  (insert (read-string "Enter Subject: "))
	  (insert " ")
	  (insert (current-time-string))
	  (insert " ")
	  (insert wikipedia-draft-leader-text)
	  (insert "\n\n")					;Version:1.27
	  (insert "")
	  (insert "\n\n")
	  (insert text)
	  (insert "\n")
	  (insert "")
	  (insert "\n")
      (if (not (bolp))
		  (insert "\n\n"))
      (if (find-buffer-visiting wikipedia-draft-data-file)
		  (let ((wikipedia-draft-text (buffer-string)))
			(set-buffer (get-file-buffer wikipedia-draft-data-file))
			(save-excursion
			  (goto-char (point-max))
			  (insert "\n")
			  (insert wikipedia-draft-text)
			  (insert "\n")
			  (save-buffer)))
		(append-to-file (point-min) (point-max) wikipedia-draft-data-file)))))


(setq wikipedia-draft-handler-functions 'wikipedia-draft-append-to-file)


(custom-add-option 'wikipedia-draft-handler-functions 'wikipedia-draft-append-to-file)

;;;###autoload
(defun wikipedia-draft-page ()			;Version:1.32
  (interactive)
  (mark-page)
  (copy-region-as-kill (region-beginning) (region-end))
  (wikipedia-draft)
  (yank nil))


(defun wikipedia-draft-region (&optional beg end)
  "Wikipedia-Draft the data from BEG to END.
If called from within the wikipedia-draft buffer, BEG and END are ignored,
and the entire buffer will be wikipedia-drafted.  If called from any other
buffer, that region, plus any context information specific to that
region, will be wikipedia-drafted."
  (interactive)
  (let ((b (or beg (min (point) (or (mark) (point-min)))))
	(e (or end (max (point) (or (mark) (point-max))))))
    (save-restriction
      (narrow-to-region b e)
      (run-hook-with-args-until-success 'wikipedia-draft-handler-functions)
    (when (equal wikipedia-draft-buffer (buffer-name))
      (kill-buffer (current-buffer))
      (jump-to-register wikipedia-draft-register)))))

;;
;;;###autoload
(defun wikipedia-draft-buffer () 
  "Wikipedia-draft-buffer sends the contents of the current (temporary)
buffer to the wikipedia-draft-buffer, see the variable
wikipedia-draft-data-file." 
  (interactive)
  (wikipedia-draft-region  (point-min) (point-max)))

(defun wikipedia-draft-clipboard ()
  "Wikipedia-Draft the contents of the current clipboard.
Most useful for wikipedia-drafting things from Netscape or other X Windows
application."
  (interactive)
  (with-temp-buffer
    (insert (x-get-clipboard))
    (run-hook-with-args-until-success 'wikipedia-draft-handler-functions)))

;;;###autoload


;;; Internal Functions:
(defvar wikipedia-draft-send-archive t	;Version:1.56
"*Archive the reply.")

(defvar wikipedia-draft-mode-map ())
(if wikipedia-draft-mode-map
    ()
  (setq wikipedia-draft-mode-map (make-sparse-keymap))
  (define-key wikipedia-draft-mode-map "\C-c\C-k" 'wikipedia-draft-buffer)
  (define-key wikipedia-draft-mode-map "\C-c\C-d" 'wikipedia-draft-buffer))

(defun wikipedia-draft-mode ()
  "Major mode for output from \\[wikipedia-draft].
\\<wikipedia-draft-mode-map> This buffer is used to collect data that
you want wikipedia-draft.  Just hit \\[wikipedia-draft-region] when
you're done entering, and it will go ahead and file the data for
latter retrieval, and possible indexing.
\\{wikipedia-draft-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (indented-text-mode)
  (use-local-map wikipedia-draft-mode-map)
  (setq major-mode 'wikipedia-draft-mode
	mode-name "Wikipedia-Draft")
  (run-mode-hooks 'wikipedia-draft-mode-hook))


(defun wikipedia-draft-view-draft ()
  (interactive)
  "Simple shortcut to visit the file, which contains the wikipedia drafts."
  (find-file wikipedia-draft-data-file))

;;}}}

;;{{{ functions for marking regions

(defun wikipedia-mark-section ()		;Version:1.36
  "Set mark at end of current logical section, and point at top."
  (interactive)
  (re-search-forward (concat  "== "  "[a-z,A-z \t]*"
							  " =="))
  (re-search-backward "^")
  (set-mark (point))
  (re-search-backward (concat "== "  "[a-z,A-z \t]*"
							  " "))
  (wikipedia-activate-region))

(defun wikipedia-mark-signature ()		;Version:1.36
  "Set mark at end of current logical section, and point at top."
  (interactive)
  (re-search-forward "]]") ;;[[ ]]
  (re-search-backward "^")
  (set-mark (point))
  (re-search-backward "[[")
  (wikipedia-activate-region))

(when (featurep 'xemacs)
  (fset 'wikipedia-activate-region (symbol-function 'zmacs-activate-region)))

(unless (featurep 'xemacs)
  (defun wikipedia-activate-region ()
    nil))

;;}}}

;;{{{ `reply' and `send' functions  

(defun wikipedia-draft-copy-page-to-register () ;Version:1.47
  "Copy a page via the wikipedia-draft-register."
  (interactive)
  (save-excursion
	  (narrow-to-page nil)
	  (copy-to-register wikipedia-draft-page (point-min) (point-max) nil) 
	  (message "draft page copied to wikipedia register wikipedia-draft-page.")
	  (widen)))

;aux function
(defun wikipedia-draft-yank-page-to-register ()	;Version:1.50
  "Insert a page   via the  wikipedia-draft-register."
  (interactive)
  (insert-register wikipedia-draft-page nil))



(defun wikipedia-draft-send-to-mozex (target-buffer) ;Version:1.56
  "Copy the current page from the wikipedia draft file  to
  TARGET-BUFFER, this buffer is named  something like  mozex.textarea.
Check the variable wikipedia-draft-send-archive.  If it is t, then
additionally the text will be archived in the draft.wiki file. Check
longlines-mode, it might be better if it is set off."
  (interactive "bTarget buffer: ")
  (let ((src-buf (current-buffer)))
	(wikipedia-draft-copy-page-to-register)
	(switch-to-buffer target-buffer)
	(end-of-line 1)
	(newline 1)
    (wikipedia-draft-yank-page-to-register)
	(message "The page has been sent (copied) to the mozex file!")
	(switch-to-buffer "*Wikipedia-Draft*")
    (when wikipedia-draft-send-archive	;Version:1.56
	  (let ((text (buffer-string))		;Version:1.59
			(desc (wikipedia-draft-buffer-desc)))
		(with-temp-buffer
		  (insert "\n\n")
		  (insert wikipedia-draft-leader-text)
		  (insert-register wikipedia-draft-reply-register 1)
		  (insert " ")
		  (insert (current-time-string))
		  (insert " ")
		  (insert wikipedia-draft-leader-text)
		  (insert "\n\n")
		  (insert "")
		  (insert "\n\n")
		  (insert text)
		  (insert "\n")
		  (insert "")
		  (insert "\n")
		  (if (not (bolp))
			  (insert "\n\n"))
		  (if (find-buffer-visiting wikipedia-draft-data-file)
			  (let ((wikipedia-draft-text (buffer-string)))
				(set-buffer (get-file-buffer wikipedia-draft-data-file))
				(save-excursion
				  (goto-char (point-max))
				  (insert "\n")
				  (insert wikipedia-draft-text)
				  (insert "\n")
				  (save-buffer)))
			(append-to-file (point-min) (point-max) wikipedia-draft-data-file)))))
    (when (equal wikipedia-draft-buffer (buffer-name))
	  (kill-buffer (current-buffer)))
	(switch-to-buffer target-buffer)))


;;Apr_22_2006
(defvar wikipedia-reply-with-hline nil
"*Whether to use a hline as a header seperator in the reply.")

(defvar wikipedia-reply-with-quote nil	;Version:1.60
"*Whether to use a quotation tempalate or not.")

(defvar wikipedia-user-simplify-signature t
  "*Simple varible in order to threat complicated signatures of users, which uses
fonts and other makeup.")
 
(defun wikipedia-reply-at-signature () ;Version:1.40
"Very simple function to add the reply prefix to the signature,
sorrounded by the boldface makeup. You have to set the point BEFORE
the signature, then the functions inserts the following
:'''Re: [[User:foo]]'''."
  (interactive)
  (beginning-of-line 1)
  (search-forward "[[")
  (mark-word 3)
  (yank)
  (end-of-line 1)
(wikipedia-terminate-paragraph)
  (insert ":'''Re: ")
  (insert "[[")
  (yank)
  (insert "]]")
  (insert "'''"))    




(defun wikipedia-draft-reply ()			;Version:1.62
  "Open a temporary buffer in wikipedia mode for editing an wikipedia
draft, with an arbitrary piece of data. After finishing the editing
|]]:either use \"C-c C-k\" \\[wikipedia-draft-buffer] to send the data into
the wikipedia-draft-data-file, or send the buffer \"C-c\C-c\",
\\[wikipedia-draft-send-to-mozex] to the current wikipedia article via
mozex. Check the varibale wikipedia-draft-send-archive."
  (interactive)
  (wikipedia-reply-at-point-simple)
(beginning-of-line 1)
(kill-line nil)
  (save-excursion
	(window-configuration-to-register wikipedia-draft-register)
	(let ((buf (get-buffer-create wikipedia-draft-buffer)))
	  (switch-to-buffer-other-window buf)
	  (wikipedia-mode)
	  (if (functionp 'pabbrev-mode)
		  (pabbrev-mode))
	  (when (not wikipedia-reply-with-quote)
		(when  wikipedia-reply-with-hline 
		  (insert "----")
		  (newline 1))
		(yank)
		(end-of-line 1))
	  (when wikipedia-reply-with-quote
		(insert "{{Quotation|")
		(yank)
		(insert "'''Re: ")
		(insert-register wikipedia-draft-reply-register 1)
		(insert "''' |~~~~}}")
		(backward-char 7))
	  (message " C-c C-k sends to draft, C-c C-c sends to org buffer."))))

(defun wikipedia-reply-at-point-simple () ;Version:1.65
  "Very simple function to reply to posts in the discussion forum. You have to set
the point around the signature, then the functions inserts the following
:'''Re: [[User:foo]]'''."
  (interactive)
  (beginning-of-line 1)
  (when wikipedia-english-or-german 
	(search-forward "(UTC)")
	(search-backward "[[User:")	  )
  (when (not wikipedia-english-or-german)
	(search-forward "(CET)")
	(search-backward "[[Benutzer:"))
  (when (not wikipedia-user-simplify-signature)
	(mark-word 3))
  (when  wikipedia-user-simplify-signature
	(mark-word 2))
  (copy-to-register wikipedia-draft-reply-register (region-beginning) (region-end) nil) 
  (end-of-line 1)
  (wikipedia-terminate-paragraph-and-indent)
  (insert ":'''Re: ")
  (insert-register wikipedia-draft-reply-register 1)
  (when  wikipedia-user-simplify-signature
	(insert "|]]''' "))
  (when  (not wikipedia-user-simplify-signature)
	(insert "]]''' ")))

;;}}}

(provide 'wikipedia-mode)
;;; wikipedia-mode.el ends here.


