;;; actr-mode.el --- ACT-R-mode/support for Emacs

;; This file is not part of Emacs
;; This file is part of Aquamacs Emacs

;; Copyright (C) 1998, 2003 Hedderik van Rijn <hvr-actrmode@van-rijn.org>
;; Copyright (C) 2008 David Reitter

;; Author:   Hedderik van Rijn <hvr-actrmode@van-rijn.org>
;; Version:  actr-mode.el v2.1, 080111
;; Keywords: ACT-R

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This code is a very much extended version of the lisp-mode that comes
;; standard with emacs. 

;; Part of the ACT-R support library for Emacs. Main file is actr-mode.el.
;; Browse the code or see the web-page
;; http://www.van-rijn.org/actr-mode for more information.

;; This code is tested with GNU Emacs 22.3 and Aquamacs Emacs 1.6.

;; How to install [on Unix]:
;;
;; Copy the files actr-mode.el in a directory that is
;; part of the load-path variable. If you don't know about such a directory,
;; create a directory (for example named "~/emacs-lisp"), copy the files in
;; that directory, and add the following line to your ".emacs":
;;
;;  (setq load-path (append '("~/emacs-lisp") load-path))
;;
;; If that's done, add the following line after the redefinition of
;; load-path:
;;
;;  (require 'actr-mode)
;;
;; After opening a file that ends in either .actr or .act, actr-mode should be 
;; enabled automatically, giving you syntax highlighting etc.

;; Bug reports, suggestions for improvements, etc: Hedderik van Rijn, 
;; hvr-actrmode@van-rijn.org

;;; (Highlights of) History: 
;;
;; 080111 - 2.1 - David Reitter <david.reitter@gmail.com>
;; - Updated for Emacs 22
;; - The major mode is now derived from lisp-mode as to inherit all the goodies
;; - We call run-mode-hooks as is convention
;; - Support Imaginal abd blending buffers (ACT-R 6)

;; 030729 - 2
;;
;; - Updated for ACT-R 5. 
;; - Updated contact information.
;; - Smart tab now works when line starts with negation 
;; - Removed outline support, wasn't using it.
;; - Removed lots of other code and info, don't have the time to update things 
;;   that I do not use.
;; - Removed all pretty print code, as it was to difficult to quickly get that 
;;   running again for ACT-R 5. Probably won't be too difficult to get it running
;;   using calls to actr-smart-tab. 
;;
;; Known bugs:
;;
;; - pressing tab when point is on the same line but after a closing parenthesis of 
;;   a prule indents the parenthesis incorrectly.
;;
;; 981118 - 1.12
;;
;; Improved syntax highlighting. "isa" and "chunk-type" are better
;; highlighted now.
;;
;; 980930 - 1.9
;;
;; Changed actr-smart-tab to indent lisp code inside an prule correctly.
;;
;; 980930 - 1.6
;;
;; Improved magic-minus behaviour. Pressing "-" only does its magic when
;; it's the first character on a line.
;;
;; 980930 - 1.5
;;
;; Added outline support. Still somewhat rough, all "("s at the column 0 are
;; seen as the start of a new outline entry. Should be able to be somewhat
;; more intelligent.
;;
;; 980929 - 1.2
;;
;; First public version
;;

;;; Code:

(provide 'actr-mode)

(defvar actr-mode-inited nil
  "nil is actr-mode was not loaded before, t if actr-mode was loaded.")

;;; ------------------------------------------------------------------------
;;
;; Definitions determining how prules are pretty printed/color coded
;;
;;; ------------------------------------------------------------------------
				
(defvar actr-prule-name-column 2	;;; (p XXX
  "*Column to which the name of prule should be indented")	   
(defvar actr-separator-column 1		;;; ==>
  "*Column to which the LHS/RHS separator should be indented")	    
(defvar actr-chunk-name-column 2	;;; =GOAL>
  "*Column to which the start of a new chunk should be indented")  
(defvar actr-slot-name-column 4		;;; ISA
  "*Column to which a slot name should be indented")		      
(defvar actr-slot-value-column 18	;;; isa CHUNK-TYPE
  "*Column to which a slot value should be indented")		  
(defvar actr-command-name-column 2	;;; !POP!
  "*Column to which a command (e.g., !POP!) should be indented")
(defvar actr-command-value-column 18	;;; !push! =SUBGOAL 
  "*Column to which a command value (e.g., a !push!ed =GOAL) should be indended") 

;; Font Lock definitions

(defvar actr-font-command font-lock-function-name-face
  "Color of the ACT-R commands like \"p\", \"chunk-type\", etc.")
(defvar actr-font-prule-name font-lock-comment-face
  "Color of the name of a prule in \"(p prule\" at the start of a prule definition")
(defvar actr-font-bang-bang font-lock-comment-face
  "Color of the text between !...!")
(defvar actr-font-nil font-lock-keyword-face
  "Color of \"nil\", anywhere in a prule")
(defvar actr-font-chunk-name font-lock-reference-face
  "Color of chunk names in a \"=xxx>\" construct")
(defvar actr-font-separator font-lock-function-name-face
  "Color of LHS/RHS separator \"==>\"")
(defvar actr-font-prule-command font-lock-keyword-face
  "Color of prule commands like \"!pop!\"")
(defvar actr-font-isa font-lock-keyword-face
  "Color of the \"ISA\" constructs")
(defvar actr-font-chunk-type font-lock-type-face
  "Color of the chunk type name both after a \"ISA\" and a \"chunk-type\"")


;;; ------------------------------------------------------------------------
;;
;; No user serviceable parts below
;;
;;; ------------------------------------------------------------------------

(defconst actr-space 32)		; ASCII representation of a space

;;; ------------------------------------------------------------------------
;;
;; Util functions. 
;;
;;; ------------------------------------------------------------------------

;; General

(defun actr-current-line ()		
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)))

(defun actr-num-non-spaces-strings (cur-point)
  (save-excursion
    (let ((num-non-spaces 0))
      (goto-char cur-point)
      (beginning-of-line)
      (while (< (point) cur-point)
	(skip-chars-forward " \t")
	(if (< (point) cur-point)
	    (progn
	      (incf num-non-spaces)
	      (skip-chars-forward "^ \t"))))
      num-non-spaces)))

(defun actr-delete-space-at-start ()
  (delete-region 
   (progn (beginning-of-line) (point)) 
   (progn (skip-chars-forward " \t") (point))))

;; Specific

(defun actr-determine-line-type (line)
  (save-excursion
    
    ;; Goto the start of the text on the to be identified line.
    (goto-char line)
    (move-to-column 0)
    (skip-chars-forward " \t")

    ;; Identify different line types
    (cond 
     ((looking-at "(p")			; Test for prule begin
      'prule-begin)
     ((looking-at "\n")			; Test for empty line
      'empty-line)
     ((looking-at ")")			; Test for end of prule
      'prule-end)
     ((looking-at ";;;")		; Test for fixed comment
      'fixed-comment)
     ((looking-at ";;")			; Test for source comment block
      'source-comment)
     ((looking-at ";")			; Test for indented comment
      'indented-comment)
     ((looking-at "==>")		; Test for ==> structure
      'lh-rh-separator)
     ((or (looking-at "=")		; Test for =goal> structure
	  (looking-at "+")
	  (looking-at "-[a-zA-Z]"))
      'start-chunk)
     ((looking-at "-")			; Test for negation/-visual etc
      'negation)
     ((looking-at "!")			; Test for !xxx! structure
      'actr-command)
     (t
      'actr-unknown))))

(defun actr-is-line-a-comment-line (type)
  (or (eq type 'fixed-comment)
      (eq type 'source-comment)
      (eq type 'indented-comment)))

(defun actr-contains-comment (work-point sexp-state)
  (save-excursion
    (goto-char work-point)
    (let ((cur-line (actr-current-line)))
      (parse-partial-sexp work-point (point-max) nil nil sexp-state t)
      (if (= cur-line (actr-current-line))
	  (1- (current-column))
	nil))))

(defun actr-in-prule (test-point)
  "Returns t if test-point is in a prule, nil otherwise."
  (interactive "d")
  (save-excursion
    (let (sexp-state bod)
      (condition-case nil
	  (forward-char 1)
	(error nil))
      (condition-case nil
	  (let ((defun-prompt-regexp "[ \t]*"))
	    (beginning-of-defun)
	    (skip-syntax-forward " "))
	(error (progn (error "Could not find start of production. Are () balanced?"))))
      (setq bod (point))
      (if (looking-at "(p")
	  (progn
	    (setq sexp-state (parse-partial-sexp bod test-point))
;;	    (message "-- %d" (nth 0 sexp-state))
	    (if (> (nth 0 sexp-state) 0)
		bod))
	nil))))

(defun actr-first-indentation-level (type)
  (cond 
   ((eq type 'lh-rh-separator)
    actr-separator-column)
   ((eq type 'start-chunk)
    actr-chunk-name-column)
   ((eq type 'actr-command)
    actr-command-name-column)
   ((eq type 'actr-unknown)
    actr-slot-name-column)
   ((eq type 'negation)
    (- actr-slot-name-column 2))
   (t nil)))

(defun actr-get-next-line-indentation (cur-point max-point)
  (save-excursion
    (goto-char cur-point)
    (forward-line)
    (if (< (point) max-point)
	(progn
	  (if (actr-first-indentation-level (actr-determine-line-type (point)))
	      (actr-first-indentation-level (actr-determine-line-type (point)))
	    (actr-get-next-line-indentation (point) max-point)))
      actr-slot-name-column)))

;;; ------------------------------------------------------------------------
;;
;; Smart tab - is sensitive of context in a prule. However, it only works
;; when the production rule is already defined as a complete defun, i.e.,
;; that its own parens are balanced, and that is has no incomplete strings and
;; parens.
;;
;;; ------------------------------------------------------------------------

(defun actr-negated-line (cur-point)
  (save-excursion
    (move-to-column 0)
    (skip-chars-forward " \t")
    (looking-at "-[ \t]")))

(defun actr-smart-tab (bprule)
  (beginning-of-line)
  (let ((startpoint (point))
	(sexp-state (parse-partial-sexp bprule (point)))
	(words (actr-num-non-spaces-strings (point)))
	(line-type (actr-determine-line-type (point)))
	(eod (save-excursion (condition-case nil
				 (end-of-defun) (error ()))
			       (point))))
    (cond
     ;; --------------------------------------------------------------------
     ((> (nth 0 sexp-state) 1)
      (lisp-indent-line))
     ;; ------------------------------------------------------------------
     ((or (eq line-type 'fixed-comment)
	  (eq line-type 'indented-comment))
      (save-excursion
	(lisp-indent-line)))
     ;; ------------------------------------------------------------------
     ((eq line-type 'source-comment)
      (save-excursion
	(actr-delete-space-at-start)
	(insert-char actr-space (actr-get-next-line-indentation (point) eod))))
     ;; ------------------------------------------------------------------
     ((and (= words 0)	
	   (actr-first-indentation-level (actr-determine-line-type (point))))
      (message "1")
      (actr-delete-space-at-start)
      (insert-char actr-space (actr-first-indentation-level (actr-determine-line-type (point)))))
     ;; ------------------------------------------------------------------
     ((= words 0)   
      (message "2")
      (actr-delete-space-at-start)
      (move-to-column 0)
      (delete-horizontal-space)
      (if (actr-negated-line (point))
	  (progn
	    (insert-char actr-space (max (- actr-slot-name-column 2) 0))
	    (forward-char 1)
	    (delete-horizontal-space)
	    (insert-char actr-space 1))
	(if (or 
	     (> (nth 0 sexp-state) 1) ; in lisp-list? (parens)
	     (nth 3 sexp-state)		; in a string?
	     (nth 4 sexp-state)		; in a comment?
	     (nth 7 sexp-state))	    ; in a "b" style comment?
	    (lisp-indent-line)
	  (unless (looking-at ")")
	    (insert-char actr-space actr-slot-name-column)))))
     ;; ------------------------------------------------------------------
     ((= words 1)
      (message "3")
      (if (actr-negated-line (point))
	  (progn
	    (move-to-column 0)
	    (delete-horizontal-space)
	    (insert-char actr-space (max (- actr-slot-name-column 2) 0))
	    (forward-char 1)
	    (delete-horizontal-space)
	    (insert-char actr-space 1))
	(cond
	 ((or (eq line-type 'lh-rh-separator)
	      (eq line-type 'start-chunk))
	  (insert-tab))
	 ((eq line-type 'actr-command)
	  (delete-horizontal-space)
	  (insert-char actr-space (max (- actr-command-value-column (current-column)) 1)))
	 ((eq line-type 'actr-unknown)
	  (delete-horizontal-space)
	  (insert-char actr-space(max (- actr-slot-value-column (current-column)) 1))))))
      ((= words 2)
       (message "4")
       (if (actr-negated-line (point))
	   (progn
	     (delete-horizontal-space)
	     (insert-char actr-space (max (- actr-slot-value-column (current-column)) 1)))
	 (insert-tab)))
      (t (insert-tab))
      )
    ;; If nothing changed after doing our tab, go to the next word.
    (if (= (point) startpoint)
	(progn (search-forward-regexp "[ \t$]" (save-excursion (end-of-line) (point)) 1)
	       (unless (eolp)
		 (backward-char 1))))) )

;;; ------------------------------------------------------------------------
;;
;; Replacement for lisp-indent-line. However, this function does more than
;; just indenting a line, as lisp-indent line does. When this function is
;; called when in a prule, it also tries to indent the second column (slot
;; values, command arguments, etc.) correctly. If both the first and the
;; optional second column are specified before point, it just inserts a
;; tab. If it just doesn't know what to do, it calls lisp-indent-line. If
;; anyone comes up with a better name for this function...
;;
;;; ------------------------------------------------------------------------

(defun actr-indent-line (&optional whole-exp)
  "Indents current line according to ACT-R style or Lisp style, based on context
If point is in a production rule, indentation is according to the
defined pretty print variables. Otherwise, the function
`lisp-indent-line' is called."
 (interactive "P") 
 (let (bprule)
   (if (condition-case nil		; Catch errors...
	   (setq bprule (actr-in-prule (point)))
	 (error nil))
       (actr-smart-tab bprule)
     (lisp-indent-line whole-exp))) )

;;; ------------------------------------------------------------------------
;;
;; Magic > - pressing this function inside a correct prule indents the first
;; column according to the pretty print variables. That is, both =goal> and
;; ==> are reindented.
;;
;;; ------------------------------------------------------------------------

(defun actr-magic-bigger-than ()
  (interactive)
  (insert-char ?\> 1)
  (save-excursion
    (let (bod sexp-state)
      (if (condition-case nil		; Catch errors...
	      (setq bod (actr-in-prule (point)))
	    (error nil))
	  (progn
	    (setq sexp-state (parse-partial-sexp bod (point)))
	    (unless 
		(or 
		 (> (nth 0 sexp-state) 1) ; in lisp-list? (parens)
		 (nth 3 sexp-state)	; in a string?
		 (nth 4 sexp-state)	; in a comment?
		 (nth 7 sexp-state)	; in a "b" style comment?
					; in first word on line?
		 (not (= (actr-num-non-spaces-strings (point)) 1)))
	      (case (actr-determine-line-type (point))
		('lh-rh-separator (progn
				    (actr-delete-space-at-start)
				    (insert-char actr-space actr-separator-column)))
		('start-chunk (progn
				(actr-delete-space-at-start)
				(insert-char actr-space actr-chunk-name-column))))))))))

(defun actr-magic-exclamation-mark ()
  (interactive)
  (insert-char ?\! 1)
  (let (bod sexp-state)
    (if (condition-case nil	    ; Catch errors...
	    (setq bod (actr-in-prule (point)))
	  (error nil))
	(progn
	  (setq sexp-state (parse-partial-sexp bod (point)))
	  (unless 
	      (or 
	       (> (nth 0 sexp-state) 1) ; in lisp-list? (parens)
	       (nth 3 sexp-state)	; in a string?
	       (nth 4 sexp-state)	; in a comment?
	       (nth 7 sexp-state)	; in a "b" style comment?
					; in first word on line?
	       (not (= (actr-num-non-spaces-strings (point)) 1)))
	    (progn
	      (save-excursion
		(actr-delete-space-at-start)
		(insert-char actr-space actr-command-name-column))))))))
	    
(defun actr-magic-minus ()
  (interactive)
  (let (bod sexp-state num-words)
    (setq num-words (actr-num-non-spaces-strings (point)))
    (insert-char ?\- 1)
    (if (condition-case nil		; Catch errors...
	    (setq bod (actr-in-prule (point)))
	  (error nil))
	(progn
	  (setq sexp-state (parse-partial-sexp bod (point)))
	  (unless 
	      (or 
	       (> (nth 0 sexp-state) 1) ; in lisp-list? (parens)
	       (nth 3 sexp-state)	; in a string?
	       (nth 4 sexp-state)	; in a comment?
	       (nth 7 sexp-state)	; in a "b" style comment?
					; in first word on line?
	       (not (= num-words 0)))
	      (progn
		(actr-delete-space-at-start)
		(insert-char actr-space (max (- actr-slot-name-column 2) 0))
		(forward-char 1)
		(delete-horizontal-space)))))))

;;; ------------------------------------------------------------------------
;;
;; Keybindings/Menu
;;
;;; ------------------------------------------------------------------------

(defvar actr-automagical-keys t
  "* Setting to nil before loading actr-mode disables automagical (tab,!,>) keys.")

(defvar actr-mode-map nil
  "Keymap for actr-mode buffers")

(if actr-mode-map
    ()					; Do not change the keymap if already set up.
  (setq actr-mode-map (make-sparse-keymap)))

(set-keymap-parent actr-mode-map lisp-mode-map)

(defun actr-add-local-menu-entry (name function)
  (let ((menu-vector (vconcat [menu-bar actr Local] (vector function))))
    (define-key actr-mode-map menu-vector (cons name function))))

(defun actr-define-keymaps ()
  (define-key actr-mode-map [menu-bar actr] (cons "ACT-R" (make-sparse-keymap "ACT-R")))

  (define-key actr-mode-map [menu-bar actr Local] (cons "Local" (make-sparse-keymap "Local")))

  ;; Order determines which keys show up in the menu bar! First is hidden.
  (when actr-automagical-keys
    (define-key actr-mode-map [tab] 'actr-indent-line)
    (define-key actr-mode-map "\t" 'actr-indent-line)
    (define-key actr-mode-map [?>] 'actr-magic-bigger-than)
    (define-key actr-mode-map [?!] 'actr-magic-exclamation-mark)
    (define-key actr-mode-map [?-] 'actr-magic-minus))
)

;;; ------------------------------------------------------------------------
;;
;; Syntax table, 
;;
;;; ------------------------------------------------------------------------

(defvar actr-mode-syntax-table nil
  "Syntax table for actr-mode buffers")

(if actr-mode-syntax-table
    ()
  (setq actr-mode-syntax-table (make-syntax-table (copy-syntax-table lisp-mode-syntax-table)))
  (modify-syntax-entry ?\; "<" actr-mode-syntax-table) ; Why these modifications?
  (modify-syntax-entry ?- "_" actr-mode-syntax-table)) ; ,,    ,,     ,,

;;


(defconst actr-mode-font-lock-keywords
  '(("^(\\(p\\)[ \t]+\\([-a-zA-Z0-9_=]+\\)" 
     (1 actr-font-command)			     ;;; (p )
     (2 actr-font-prule-name))			     ;;; Name of production
    ("^\\s-*[\-*\\s-]*\\s-\\sw+\\s-+\\(nil\\)\\b" 1 actr-font-nil) ; nil

    ("^\\s-*\\([=+-]\\(goal\\|retrieval\\|imaginal\\|visual\\|visual-location\\|visual-state\\|manual\\|manual-state\\|manual-location\\|blending\\)\\)\>" 1 actr-font-chunk-name)  ;;; goal: =xxx>
    ("^[ \t]*==\>[ \t\n]+" . actr-font-separator)    ;;; separator: ==>
    ("!\\(\\sw+\\)!" 1 actr-font-bang-bang)	     ;;; !eval!, !pop!, etc
    ("^[ \t]+\\b\\(isa\\)[ \t]*" 
     1 actr-font-isa)				     ;;; ISA
    ("\\b\\(isa\\)[ \t]*\\(\\sw+\\)" 
     (1 actr-font-isa)				     ;;; ISA
     (2 actr-font-chunk-type))			     ;;; chunk type
    ("^(\\(chunk-type\\)[ \t]+"	   
     1 actr-font-command)			     ;;; (chunk-type
    ("^(\\(chunk-type\\)[ \t]+\\([-a-zA-Z0-9_*]+\\)"	   
     (2 actr-font-chunk-type))			     ;;; chunk type
    ("^(\\(goal-focus\\|focus[-]*on[-]*goal\\|set[-]*all[-]*baselevels\\|add[-]*dm\\|set[-]*analogized[-]*parameters\\|sgp\\|set[-]*ia\\|clear[-]*all\\|pm-[-a-zA-Z]+\\)" 
     (1 actr-font-command))			     ;;; (add-dm, set-all-baselevels, etc
    )
  "Defines the regexp's for the color codings of ACT-R code" 
)

;;; ------------------------------------------------------------------------
;;
;; Identifiers Menu
;;
;;; ------------------------------------------------------------------------

(defvar actr-imenu-name "Idents"
  "Name of identifiers menu. If nil, no identifier menu is added to the menu bar")

(defvar actr-imenu-generic-expression
  '(
    (nil
     "^\\s-*([pP]\\s-+\\([-A-Za-z0-9+]+\\)" 1))
;;     ("Chunk Types"
;;      "^\\s-*(chunk-type\\s-+\\([-A-Za-z0-9+]+\\)" 1)
;;     ("Chunks"			 ; Only works if chunk is defined on one line.
;;      "^\\s-*(\\([-A-Za-z0-9+*]+\\)\\s-+[iI][sS][aA]" 1))
  "Imenu generic expression for ACT-R mode. See `imenu-generic-expression'.")

;;; ------------------------------------------------------------------------
;;
;; Definition of actr-mode
;;
;;; ------------------------------------------------------------------------

(defun actr-mode ()
  "Major mode for ACT-R code

Features:
- font lock of ACT-R code
- identifier menu
- smart tab & other automagical keys

Special commands: 

\\{actr-mode-map}

Turning on the actr-mode runs the hook `actr-mode-hook'.

If you find something wrong with actr-mode, or feel that it can be improved
in some way, feel free to email me:

Hedderik van Rijn <hvr-actrmode@van-rijn.org>

WWW-site: http://www.van-rijn.org/actr-mode
"
  (interactive)
  (kill-all-local-variables)		; Kill all old mode vars
  (delay-mode-hooks (lisp-mode)) ;; inherit from this
  
  (use-local-map actr-mode-map)
  (setq mode-name "ACT-R")
  (setq major-mode 'actr-mode)
  (set-syntax-table actr-mode-syntax-table)

  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")

  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 45)

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|$" ))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)

  (if actr-imenu-name
      (progn
	(make-local-variable 'imenu-generic-expression)
	(setq imenu-generic-expression actr-imenu-generic-expression)
	(imenu-add-to-menubar actr-imenu-name)))

  (actr-define-keymaps)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults 
	'((actr-mode-font-lock-keywords
	   actr-mode-font-lock-keywords
	   actr-mode-font-lock-keywords)
	  nil nil (("_-" . "w")) beginning-of-defun 
	  (font-lock-mark-block-function . mark-defun)
	  (font-lock-comment-start-regexp . ";")
	  (font-lock-keywords-case-fold-search . t)))

  (run-mode-hooks 'actr-mode-hook))

(put 'actr-mode 'derived-mode-parent 'lisp-mode)

;;; ------------------------------------------------------------------------
;;
;; "External" changes:
;;
;; - Link the extentions .actr and .act to this mode.
;; - Create a mouse buffer menu for ACT-R files.
;; 
;;; ------------------------------------------------------------------------

(if (not actr-mode-inited)
    (progn
      (setq auto-mode-alist (append (list (cons "\\.actr\\'" 'actr-mode))
				    (list (cons "\\.act\\'" 'actr-mode))
				    auto-mode-alist))
      ;; Test whether "msb" is defined.
      (if (condition-case nil (type-of msb-menu-cond) (error nil))
	  (setq msb-menu-cond
		(cons '((and ;(message (buffer-name))
			 (or (if buffer-file-name 
				 (string-match "act[-_]*r" buffer-file-name))
			     (string-match "act[-_]*r" (buffer-name)))
			 'multi)
			3005
			"ACT-R Files (%d)") msb-menu-cond))
	(message "Package msb not loaded... skipping initialization for msb"))))

(setq actr-mode-inited t)

;; actr-mode.el ends here
