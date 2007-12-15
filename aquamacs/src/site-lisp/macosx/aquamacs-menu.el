;; aquamacs-menu
;; redefines and modifies the menu bar

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-menu.el,v 1.90 2007/12/15 19:29:20 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/

;; Attribution: Leave this header intact in case you redistribute this file.
;; Attribution must be given in application About dialog or similar,
;; "Contains Aquamacs Menu by D Reitter" does the job.
;; Apart from that, released under the GPL:
;; Aquamacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Aquamacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;; Copyright (C) 2005, 2006, 2007 David Reitter
 

(require 'easymenu)
(require 'aquamacs-tools)



; (assq 'paste (lookup-key global-map [menu-bar edit]))
; (assq 'new-file (lookup-key global-map [menu-bar file]))


;; (defun change-menu-text-1 (list subst)
;; ;; replace the first string or relevant element with subst
;; ;; return new list
;;   (cond 
;;     ((not list) nil)
;;     ((not (listp list)) list)
;;     ((or
;;       (stringp (car list))
;;       (and (listp (car list))
;; 	   (eq 'aq-shortcut (car (car list))))
;;      )
;;     (cons subst (change-menu-text-1 (cdr list) subst)))
;;    (t  (cons (car list) (change-menu-text-1 (cdr list) subst)))
;;       ))

;; ;; this is a big hack like most other things
;; (defun change-menu-text (keymap key str)
 
;;   (define-key global-map (vconcat (append keymap (list key)))
;;     (change-menu-text-1 (cdr (assq key (lookup-key global-map keymap))) str )))
 

;; apple command character is unicode x2318  
;; 
(defun aq-describe-modifier (mod)
  ;; translate modifier
  (or
  (cond
   ((and (boundp 'mac-command-modifier) (eq mac-command-modifier mod))
    (string (decode-char 'ucs #X2318)))
   ((and (boundp 'mac-option-modifier) (eq (or mac-option-modifier 'alt)
					   mod))
    (string (decode-char 'ucs #X2325)))
   ((and (boundp 'mac-control-modifier) (eq (or mac-control-modifier 'ctrl) 
					    mod))
    (string (decode-char 'ucs #X2303)))
   ((eq mod 'shift)
    (string (decode-char 'ucs #X21E7)))
   ((and (boundp 'mac-function-modifier) (eq mac-function-modifier mod))
    "Fn ")
   )
 ;; (progn (print mod) nil)
     (signal 'search-failed nil)
   ))

(defvar apple-char (string (decode-char 'ucs #X2318)))

;; The following is a big hack. The mac port can't currently cope 
;; with putting the command key combos in the menu, for various 
;; reasons (1. they are just secondary alternatives, 2. command is defined
;; as 'alt' and only known as such)

; redefine New
; (define-key menu-bar-edit-menu [mark-whole-buffer] (cdr (assq 'mark-whole-buffer (key-binding [menu-bar edit]))))



(defun aq-resolve-remapped (list)
  (flatten  
  (mapcar
     (lambda (k)
       (cond 
	( (and 
	   (vectorp k)
	   (eq (aref k 0) 'remap)
	   )
	  (where-is-internal (aref k 1) nil nil t t))
	((and 
	   (vectorp k)
	   (eq (aref k 0) 'menu-bar)
	   )
	 nil
	 )
	(t k))
	)
     list)))

(defun aq-find-best-key (list)
  
(or (if (not list)
	"")
    (key-description (car list))
    (aq-find-best-key (cdr list)))
)
;; 
(defun aq-find-good-key (symbol)
  (aq-find-best-key
   (aq-resolve-remapped
    (or
     (where-is-internal 
      symbol
      (list  osx-key-mode-map   ) 
      )
     (where-is-internal 
      symbol
      (list    global-map ) 
      )
     (where-is-internal 
      symbol
      nil 
      nil t t)))))


;; TO DO: speed this up
;; should only update if there isn't already a string
;; or the key variables have changed

;(defun aq-shortcut (text symbol &rest more-args)
;  (apply (function format) text more-args))
 
; (aq-find-good-key 'cua-paste)
; (aq-find-good-key #'nil)

(defun aq-shortcut (text symbol &rest more-args)
  ;; symbol can be nil in some circumstances 
  ;; (e.g. in tool-bar-map from early initialization)
  (if (and symbol (if (boundp 'osx-key-mode) osx-key-mode nil))
      (condition-case err
	  (progn
	    (let* ((case-fold-search nil)
		   (s (aq-find-good-key symbol))
		   (s (replace-regexp-in-string 
		       "S-." (lambda (txt) 
			       (concat (aq-describe-modifier 'shift) "-" 
				       (downcase (substring txt 2))))
		       s)))
	      (apply (function format) 
		     (append (list (concat text "%s")) more-args 
			     (list 
			      (replace-regexp-in-string 
			       "-" ""
			       (replace-regexp-in-string 
				"-\\([a-z]\\)" 'upcase
			  
				(replace-regexp-in-string 
				 "C-" (lambda (txt) 
					(concat (aq-describe-modifier 'ctrl) 
						"-"))
				 (replace-regexp-in-string 
				  "H-" (lambda (txt) 
					 (concat (aq-describe-modifier 'hyper)
						 "-"))
				  (replace-regexp-in-string 
				   "A-" (lambda (txt) 
					  (concat (aq-describe-modifier 'alt)
						  "-"))
				   (replace-regexp-in-string 
				   "M-" (lambda (txt) 
					  (concat (aq-describe-modifier 'meta)
						  "-"))

				   (replace-regexp-in-string 
				    "-\\([A-Z]\\)" 
				    (lambda (txt) 
				      (concat 
				       (aq-describe-modifier 'shift) txt))
				    s
				    nil nil 1 ;; replace sub-exp
				    ))))))))))))
	(error nil
	       (apply (function format) text more-args)))
    ;; not osx-key-mode
    (apply (function format) text more-args)))
	  
 
;;(Defun aq-shortcut (text &rest more-args)
;;  (append (list (function format)) (append (list text 'apple-char) more-args )))
 
(defun aq-copy-list (list)
  "Return a copy of LIST, which may be a dotted list.
The elements of LIST are not copied, just the list structure itself."
  (if (consp list)
      (let ((res nil))
	(while (consp list) (push (pop list) res))
	(prog1 (nreverse res) (setcdr res list)))
    (car list)))

(defun aq-concat-symbol (sym1 sym2)
  (intern (concat (if (stringp sym1) sym1 (symbol-name sym1))
		  (if (stringp sym2) sym2 (symbol-name sym2)))))

(defvar menu-bar-new-file-menu nil)
(defun aquamacs-update-new-file-menu ()
   (setq menu-bar-new-file-menu 
	 (aquamacs-define-mode-menu (make-sparse-keymap "New Buffer")
				    "aquamacs-new-buffer-" 'new-frame-with-new-scratch
				    "Create a new buffer in `%s' mode."))
   (define-key-after menu-bar-file-menu [new-file-menu]
     (list 'menu-item "New Buffer" menu-bar-new-file-menu
	   :help "Create a new buffer with a specific major mode.")
     'new-file))

;; record recently used major modes

(defvar aquamacs-recent-major-modes ()
  "Recently used major modes.")
(defun aquamacs-record-mode-change ()
  (setq aquamacs-recent-major-modes
	(delete major-mode aquamacs-recent-major-modes))
  ;; max 10 elements
  (if (nthcdr 10 aquamacs-recent-major-modes)
      (setcdr (nthcdr 9 aquamacs-recent-major-modes) nil))
  (setq aquamacs-recent-major-modes
	(cons major-mode aquamacs-recent-major-modes))
  (if (fboundp 'aquamacs-update-new-file-menu)
      (aquamacs-update-new-file-menu))
  (if (fboundp 'aquamacs-update-change-mode-menu)
      (aquamacs-update-change-mode-menu)))

 
(add-hook 'after-change-major-mode-hook 'aquamacs-record-mode-change)
 
(defun aquamacs-change-mode (buffer mode)
  (with-current-buffer (or buffer (current-buffer))
    (if (eq major-mode mode)
	(fundamental-mode))
    (funcall mode)))

(defvar menu-bar-change-mode-menu nil)
(defun aquamacs-update-change-mode-menu ()
   (setq menu-bar-change-mode-menu 
	 (aquamacs-define-mode-menu (make-sparse-keymap "Change Mode")
				    "aquamacs-change-mode-" 'aquamacs-change-mode
				    "Change the major mode of the current buffer to `%s'."
				    ;; do not slow things down
				    ;;'(menu-bar-non-minibuffer-window-p)
				    ))
   (define-key-after menu-bar-file-menu [change-mode-menu]
     `(menu-item "Change Buffer Mode" ,menu-bar-change-mode-menu
	   :help "Change to a specific major mode."
	    :enable  (and (menu-bar-menu-frame-live-and-visible-p)
 			(menu-bar-non-minibuffer-window-p)))
     'insert-file)) 

(defun aquamacs-define-mode-menu 
  (keymap symbol-prefix function-to-call docstring &optional enable-if)
  "Defines a menu consisting of recently and commonly used major modes,
using `aquamacs-recent-major-modes' and `aquamacs-known-major-modes'."

  (unless enable-if
    (setq enable-if 't))
  (aquamacs-define-mode-menu-1 aquamacs-known-major-modes keymap 
			       symbol-prefix
			       function-to-call docstring enable-if)
  (define-key keymap [separator]  '(menu-item "--"))
  (aquamacs-define-mode-menu-1 
   ;; look up texts of mode names in case there are any (for consistency)
   (mapcar (lambda (m)
	     (or (assq m aquamacs-known-major-modes)
		 m))
	   (reverse aquamacs-recent-major-modes)) 
   keymap 
   (aq-concat-symbol symbol-prefix "recent-") 
   function-to-call docstring enable-if))

 
;; also used by osxkeys.el
(defun aquamacs-pretty-mode-name (mode)
  (capitalize 
   (replace-regexp-in-string "-mode" "" (symbol-name mode))))

(defun aquamacs-define-mode-menu-1
  (the-list keymap symbol-prefix function-to-call docstring enable-if)
  (mapc
   (lambda (modeentry)
     (let ((modename (if (consp modeentry) (car modeentry) modeentry))
	   (displayname (if (consp modeentry) (cdr modeentry) 
			  (aquamacs-pretty-mode-name modeentry))))

     (when (fboundp modename)
       (define-key ;;-after doesn't work with after- why?>? 
	 keymap 
	 (vector (aq-concat-symbol symbol-prefix modename))
	 `(menu-item  
	   ,displayname
	    ,(eval 
	     (list 'defun (aq-concat-symbol symbol-prefix modename) '() 
		   ;;(format docstring modename)
		   '(interactive)
		   (list function-to-call nil `(quote ,modename))
		   ))
	   :help ,(format docstring modename)
 	   :enable ,enable-if
	   )))))
   (reverse (sort (aq-copy-list the-list)
		  (lambda (a b) (string< 
				 (upcase (if (consp a) (cdr a) (symbol-name a))) 
				 (upcase (if (consp b) (cdr b) (symbol-name b))))))))
  keymap)

(defun set-aquamacs-known-major-modes (variable value)
  "Like `custom-set-default', but for `aquamacs-known-major-modes'."
  (custom-set-default variable value)
  (if (fboundp 'aquamacs-update-new-file-menu)
      (aquamacs-update-new-file-menu))
  (if (fboundp 'aquamacs-update-change-mode-menu)
      (aquamacs-update-change-mode-menu)))
 

(defcustom aquamacs-known-major-modes
  '(text-mode 
    change-log-mode
    (css-mode . "CSS")
    fortran-mode 
    java-mode
    (javascript-mode . "JavaScript")
    (html-mode . "HTML")
    (html-helper-mode . "HTML (Helper)")
    (latex-mode . "LaTeX")
    lisp-interaction-mode 
    emacs-lisp-mode 
    tcl-mode
    c-mode 
    (objc-mode . "Objective C")
    c++-mode 
    (cperl-mode . "Perl")
    (php-mode . "PHP")
    python-mode 
    (applescript-mode ."AppleScript")
    matlab-mode R-mode ruby-mode
    (sh-mode . "Unix Shell Script")
    (nxml-mode . "XML (nXML)")
    ) ;; can't add unix shell here
  "List of commonly used modes to include in menus.
This is used to compose the New Buffer and Change Buffer Mode menus.
Each element is either a symbol containing the name of a major mode,
or a cons cell of form (MODENAME . DISPLAYNAME), where MODENAME
is the name of a mode, and DISPLAYNAME is used for the menus.

Note that the menu will contain only defined mode name symbols.
Menus are updated automatically if this variable is changed in a
customization buffer."
  :group 'menu
  :group 'Aquamacs
  :type '(repeat (choice
		  (symbol :tag "Mode-name")
		  (cons :tag "Mode / Display name" (symbol :tag "Mode-name") (symbol :tag "Display name"))))
  :set 'set-aquamacs-known-major-modes)

;; compatibility (old symbol used in 0.9.6)
(defalias 'aquamacs-menu-new-buffer-modes 'aquamacs-known-major-modes)

(add-hook 'after-init-hook 'aquamacs-update-new-file-menu)

(defun aquamacs-menu-bar-setup ()


(define-key menu-bar-file-menu [new-file]
  `(menu-item ,(aq-shortcut  "New                                        "
			    'new-frame-with-new-scratch)  
	      new-frame-with-new-scratch
	      :key-sequence nil
	      :enable (or (and (boundp 'one-buffer-one-frame-mode)
			       one-buffer-one-frame-mode)
			  (not (window-minibuffer-p
			    (frame-selected-window menu-updating-frame))))
	      :help "Create a new buffer"))
 

(define-key menu-bar-file-menu [open-file] 
  `(menu-item
    ,(aq-shortcut  "Open File...                             " 
		  'mac-key-open-file )
    mac-key-open-file
    :key-sequence nil))

(define-key menu-bar-file-menu [insert-file]
  '(menu-item "Insert File...                         " insert-file
	      :enable (menu-bar-non-minibuffer-window-p)
	      :help "Insert another file into current buffer"))
(define-key menu-bar-file-menu [dired]
  '(menu-item "Open Directory...                 " dired
	      :enable (menu-bar-non-minibuffer-window-p)
	      :help "Read a directory, to operate on its files"))


;; redefine this
(define-key-after menu-bar-file-menu [kill-buffer]
  `(menu-item ,(aq-shortcut "Close Buffer                            " 
			   (key-binding [menu-bar file kill-buffer])) 
	      close-current-window-asktosave
	      :key-sequence nil
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Discard current buffer") 'separator-save)
 
(define-key menu-bar-edit-menu [copy]
  `(menu-item ,(aq-shortcut  "Copy                              " 
			    (key-binding [menu-bar edit copy])) 
	      clipboard-kill-ring-save
	      :key-sequence nil
	      :enable mark-active
	      :help "Copy selected text in region"
	      :keys "\\[clipboard-kill-ring-save]"))
 
(define-key menu-bar-edit-menu [paste]
  `(menu-item ,(aq-shortcut  "Paste                              " 
; (key-binding [menu-bar edit paste]) ;; doesn't work
			     'clipboard-yank) 
	      clipboard-yank
	      :key-sequence nil
	      :enable (and
		       ;; Emacs compiled --without-x doesn't have
		       ;; x-selection-exists-p.
		       (fboundp 'x-selection-exists-p)
		       (x-selection-exists-p) (not buffer-read-only)
		       (menu-bar-menu-frame-live-and-visible-p))
	      :help "Paste (yank) text most recently cut/copied"))


(require 'aquamacs-redo)
(define-key menu-bar-edit-menu [undo]
  `(menu-item ,(aq-shortcut  "Undo                              "
				 (key-binding [menu-bar edit undo]) )
	      aquamacs-undo
	      :key-sequence nil
	      :enable (and (not buffer-read-only)
			   (not (eq t buffer-undo-list))
			   (if (eq last-command 'undo)
			       pending-undo-list
			     (consp buffer-undo-list))
			   (menu-bar-menu-frame-live-and-visible-p))
	      :help "Undo last operation"))

(define-key-after menu-bar-edit-menu [redo]
  `(menu-item ,(aq-shortcut "Redo                              " 
			   (key-binding [menu-bar edit redo])) 
	      aquamacs-redo
	      :key-sequence nil
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (boundp 'last-buffer-undo-list)
			   last-buffer-undo-list)
	      :help "Redo undone operation") 'undo)

(easy-menu-add-item  nil '("Edit")
  ["-" nil nil] 'cut)

;;done
(define-key menu-bar-edit-menu [cut]
  `(menu-item ,(aq-shortcut  "Cut                                 "
			     (key-binding [menu-bar edit cut]))
	      clipboard-kill-region
	      :key-sequence nil
	      :enable (and mark-active (not buffer-read-only))
	      :help
	      "Delete text in region and copy it to the clipboard"))

(define-key menu-bar-edit-menu [mark-whole-buffer]
  `(menu-item ,(aq-shortcut  
	       "Select All                        " 
	       (key-binding [menu-bar edit mark-whole-buffer])) 
	      mark-whole-buffer
	      :key-sequence nil
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Mark the whole buffer for a subsequent cut/copy."))

(define-key menu-bar-i-search-menu [isearch-forward]
  `(menu-item ,(aq-shortcut  
		"Forward String...                         " 
		(key-binding [menu-bar edit search i-search isearch-forward]))
	      isearch-forward
	      :help "Search forward for a string as you type it"))
 
(define-key menu-bar-i-search-menu [isearch-repeat-forward]
  `(menu-item ,(aq-shortcut  
		"Repeat Forward String...             " 
		(key-binding [menu-bar edit search i-search 
				       isearch-repeat-forward])) 
	      isearch-repeat-forward
	      :help "Search forward for a string as you type it"))

(define-key menu-bar-i-search-menu [isearch-use-region]
  `(menu-item ,(aq-shortcut  
		"Use Region For Search                " 
		(key-binding [menu-bar edit search i-search 
				       isearch-use-region]))
	      aquamacs-use-selection-for-find
	      :enable region-active
	      :help "Use the selection for your next search"))

 
(define-key-after menu-bar-search-menu [grep]
  '(menu-item "Search Files (Grep)..." grep
	      :help "Search files for strings or regexps (with Grep)")
  'separator-tag-search)

(define-key-after menu-bar-search-menu [separator-grep-search]
  '(menu-item "--")
  'grep)
(define-key menu-bar-tools-menu [grep] nil)

(require 'aquamacs-editing)
(define-key menu-bar-edit-menu [fill]
`(menu-item ,(aq-shortcut "Re-Wrap Lines (fill)          " 
			   (key-binding [menu-bar edit fill]))
	    fill-paragraph-or-region
	    :key-sequence nil
	    :enable (not buffer-read-only)
	    :help
	    "Fill text in region (or paragraph) to fit between
left and right margin"))

(define-key-after menu-bar-edit-menu [unfill]
`(menu-item ,(aq-shortcut "Unwrap Lines (unfill)     " 
			   (key-binding [menu-bar edit unfill]))
	    unfill-paragraph-or-region
	    :key-sequence nil
	    :enable (not buffer-read-only)
	    :help
	    "Remove line-breaks from paragraph or region.")
'fill)

  
;; this needs an extension to show the keyboard shortcut
;; interesting extensions to menu-item: (:visible nil), (:key-sequence)

 (define-key-after menu-bar-file-menu [my-file-separator]
          '(menu-item "--") 'recover-session)
 (define-key-after menu-bar-file-menu [mac-show-in-finder]
          '(menu-item "Show In Finder" mac-key-show-in-finder

		      :enable (and  (menu-bar-menu-frame-live-and-visible-p)
				    (menu-bar-non-minibuffer-window-p)
				    buffer-file-number)
		      ) 'my-file-separator)

;; save as (redefinition for :enable)

(define-key menu-bar-file-menu [save-buffer ]
  `(menu-item ,(aq-shortcut  "Save Buffer                              "
			    'mac-key-save-file)
	      save-buffer
	      :key-sequence nil
	      :enable (and (buffer-modified-p)
			   (buffer-file-name)
			   (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Save current buffer to its file"
	      ))  

(define-key menu-bar-file-menu [write-file]
  `(menu-item ,(aq-shortcut  "Save Buffer As...                      "
			    'mac-key-save-file-as) 
	      write-file
	      :key-sequence nil
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Write current buffer to another file"))


;; Export file functions
(require 'mac-print)

(defvar menu-bar-export-file-menu (make-sparse-keymap "Export ..."))

;; (setq menu-bar-export-file-menu (make-sparse-keymap "New Buffer"))

(define-key menu-bar-export-file-menu [export-pdf]
  '(menu-item "PDF..." export-to-pdf
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Write current buffer to another file in PDF format"))

(define-key menu-bar-export-file-menu [export-html]
  '(menu-item "HTML..." export-to-html
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Write current buffer to another file in HTML format"))

(define-key-after menu-bar-file-menu [export-file-menu]
  `(menu-item (concat "Export " (if mark-active "Region" "Buffer")) 
	      ,menu-bar-export-file-menu
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Export buffer in a different format")
    'write-file)

 
;; Support for monochrome printing
;; Added by Norbert Zeh <nzeh@cs.dal.ca> 2007-09-23

(when (boundp 'mac-print-monochrome-mode)
  (defun menu-bar-toggle-mac-print-monochrome-mode ()
    (interactive)
    (customize-set-variable 'mac-print-monochrome-mode
			    (not mac-print-monochrome-mode))
    (message "Color printing %s"
	     (if mac-print-monochrome-mode
		 "disabled" "enabled")))



  (define-key-after menu-bar-file-menu [toggle-mac-print-monochrome-mode]
    '(menu-item "Color Printing"
		menu-bar-toggle-mac-print-monochrome-mode
		:help "Toggles color printing"
		:button (:toggle . (not mac-print-monochrome-mode)))))

 
(define-key-after menu-bar-file-menu [aquamacs-print]
  `(menu-item (aq-shortcut "Preview and Print %s...       " 
		       (function ,(key-binding [menu-bar file aquamacs-print]))
		      (if mark-active "Region" "Buffer")
		     ) 
	      aquamacs-print
	      :key-sequence nil
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Print current buffer or region with page headings"))



;; Printing (redefinition for :enable)
 
(easy-menu-remove-item global-map  '("menu-bar" "file") 'ps-print-region) 
(easy-menu-remove-item global-map  '("menu-bar" "file") 'ps-print-buffer) 
(easy-menu-remove-item global-map  '("menu-bar" "file") 'ps-print-region-faces) 
(easy-menu-remove-item global-map  '("menu-bar" "file") 'ps-print-buffer-faces) 
(easy-menu-remove-item global-map  '("menu-bar" "file") 'print-region) 
(easy-menu-remove-item global-map  '("menu-bar" "file") 'print-buffer) 
 
(defun menu-bar-print-region-or-buffer ()
  "Prints the current buffer or the region, if mark is active."
  (interactive)
  (if mark-active
      (print-region (region-beginning) (region-end))
    (print-buffer)))

  

(define-key-after menu-bar-file-menu [print-region-or-buffer]
  `(menu-item ,(aq-shortcut "Quick Print %s       " 
		      (key-binding [menu-bar file print-region-or-buffer])
		      (if mark-active "Region" "Buffer"))  
	      menu-bar-print-region-or-buffer
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Print buffer, or region if active"))



(require 'longlines) 
 
;; goes in simple.el
(defun turn-on-longlines ()
  "Turn on Longlines mode.
... unless buffer is read-only."
  (unless buffer-read-only
    (longlines-mode 1)))

(defun turn-off-longlines ()
  "Unconditionally turn off Longlines mode."
  (longlines-mode -1))

(custom-add-option 'text-mode-hook 'turn-on-longlines)

;; goes in textmodes/text-mode
(defun toggle-text-mode-longlines ()
  "Toggle whether to use Auto Fill in Text mode and related modes.
This command affects all buffers that use modes related to Text mode,
both existing buffers and buffers that you subsequently create."
  (interactive)
  (let ((enable-mode (not (memq 'turn-on-longlines text-mode-hook))))
    (if enable-mode
	(add-hook 'text-mode-hook 'turn-on-longlines)
      (remove-hook 'text-mode-hook 'turn-on-longlines))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(if (or (derived-mode-p 'text-mode) text-mode-variant)
	    (longlines-mode (if enable-mode 1 0)))))
    (message "Longlines %s in Text modes"
	     (if enable-mode "enabled" "disabled"))))

;; This should go into menu-bar.el
(defun menu-bar-text-mode-longlines ()
  (interactive)
  ;; First turn off auto-fill
  (if
      (if (listp text-mode-hook)
	  (member 'turn-on-auto-fill text-mode-hook)
	(eq 'turn-on-auto-fill text-mode-hook))
      (toggle-text-mode-auto-fill)
      )
  (toggle-text-mode-longlines)
  ;; This is somewhat questionable, as `text-mode-hook'
  ;; might have changed outside customize.
  ;; -- Per Abrahamsen <abraham@dina.kvl.dk> 2002-02-11.
  (customize-mark-as-set 'text-mode-hook))
 

(defun menu-bar-text-mode-auto-fill ()
  (interactive)
  ;; First turn off longlines
  (if
      (if (listp text-mode-hook)
	  (member 'turn-on-longlines text-mode-hook)
	(eq 'turn-on-longlines text-mode-hook))
      (toggle-text-mode-longlines)
      )
  (toggle-text-mode-auto-fill)
  ;; This is somewhat questionable, as `text-mode-hook'
  ;; might have changed outside customize.
  ;; -- Per Abrahamsen <abraham@dina.kvl.dk> 2002-02-11.
  (customize-mark-as-set 'text-mode-hook))




(define-key-after menu-bar-options-menu [longlines]
  '(menu-item "Soft Word Wrap in Text Modes"
	      menu-bar-text-mode-longlines
	      :help "Wrap long lines without inserting carriage returns (Longlines)"
	      :button (:toggle . (if (listp text-mode-hook)
				     (member 'turn-on-longlines text-mode-hook)
				   (eq 'turn-on-longlines text-mode-hook))))
  'auto-fill-mode
  )
 
;; in edit menu

(define-key menu-bar-search-menu [case-fold-search]
  (menu-bar-make-toggle toggle-case-fold-search case-fold-search
			"Case-Insensitive Search"
			"Case-Insensitive Search %s"
			"Ignore letter-case in search"))




(when (string= "mac" window-system)
    (require 'aquamacs-frame-setup)
    (define-key-after menu-bar-options-menu [oneonone]
      (menu-bar-make-mm-toggle 
       one-buffer-one-frame-mode
       "Display Buffers in Separate Frames"
       "Open a new Frame (window) for each new buffer."
       (:visible (boundp 'one-buffer-one-frame-mode)))
       'edit-options-separator))

(when (fboundp 'mac-font-panel-mode)

  (defun turn-on-mac-font-panel-mode ()
    (interactive)
    (mac-font-panel-mode 1))

 ;; this is a redefine

  (define-key menu-bar-showhide-menu [mac-font-panel-mode]
    nil)
  (define-key menu-bar-options-menu [mouse-set-font]
  `(menu-item ,(aq-shortcut "Show Fonts (this Frame)...                 " 
			    'mac-font-panel-mode)
	      turn-on-mac-font-panel-mode
	       :visible ,(display-multi-font-p)
	       :enable (menu-bar-menu-frame-live-and-visible-p) 
	       :help "Select a font from list of known fonts/fontsets"))

)

(define-key menu-bar-options-menu [highlight-paren-mode] nil)
(define-key menu-bar-options-menu [highlight-separator] nil)
(define-key-after menu-bar-showhide-menu [highlight-separator] '("--"))
(define-key-after menu-bar-showhide-menu [highlight-paren-mode]
  (menu-bar-make-mm-toggle show-paren-mode
			   "Paren Match Highlighting"
			   "Highlight matching/mismatched parentheses at cursor (Show Paren mode)"))

(easy-menu-add-item  nil '("Options")
  ["-" nil nil] 'mouse-set-font)

(define-key menu-bar-options-menu [blink-cursor-mode] nil)
(define-key menu-bar-options-menu [cursor-separator] nil)


(define-key-after menu-bar-options-menu [aquamacs-color-theme-select]
  `(menu-item "Show Color Themes (this Frame)..." aquamacs-color-theme-select
	       :visible (and (display-multi-font-p)
			     (fboundp 'aquamacs-color-theme-select)
			     )
	       :enable (menu-bar-menu-frame-live-and-visible-p)  
	       :help "Select a color theme from a list")
  'mouse-set-font)

;; Small Fringe

(defun aquamacs-menu-bar-showhide-fringe-menu-customize-small ()
  "Display small fringes only on the left of each window."
  (interactive)
  (require 'fringe) 

  ;; Unfortunately, fringe-mode likes to round up fringes.
  ;; Therefore, we set both to 1.
  (customize-set-variable 'fringes-outside-margins 1)
  (customize-set-variable 'left-fringe-width 1)
  (customize-set-variable 'left-margin-width 1)

  (setq default-fringes-outside-margins 1)
  (setq default-left-fringe-width 3)
  (setq default-left-margin-width 0)
  (aquamacs-define-the-fringe-bitmap) ;; redefine 
  (customize-set-variable 'fringe-mode '(1 . 1))
  ) 


 
(define-key-after menu-bar-showhide-fringe-menu [small]
  `(menu-item "Small left fringe" 
	      aquamacs-menu-bar-showhide-fringe-menu-customize-small
	      :help "Narrow fringe, left only"
	      :visible ,(display-graphic-p)
	      :button (:radio . (equal fringe-mode '(1 . 1)))) 'none)

 

;; remove this entry, because in Aquamacs, no global tool-bar-mode
;; is present
(define-key menu-bar-showhide-menu [showhide-tool-bar]
    nil)


;; move this down after "customize"

(define-key-after menu-bar-options-menu [save-custom-separator]
  '("--") 'customize)

(define-key-after menu-bar-options-menu [save]
  '(menu-item "Save Options" aquamacs-menu-bar-options-save
	      :help "Save options set from the menu above")
  'save-custom-separator)

;; Goto Line

(define-key menu-bar-goto-menu [go-to-line]
  `(menu-item ,(aq-shortcut  "Goto Line...                      "
			    'goto-line) 
	      goto-line

	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Read a line number and go to that line"))



;; Spell Checking

(defun aquamacs-ispell-install ()
  (interactive)
  (browse-url "http://aquamacs.org/spellchecking"))
 

(define-key menu-bar-tools-menu [spell] nil)
(define-key menu-bar-tools-menu [separator-spell] nil)
(define-key-after menu-bar-edit-menu [separator-spell]
  '(menu-item "--")
  'separator-bookmark)

(define-key-after menu-bar-edit-menu [spell]
  '(menu-item "Spelling" ispell-menu-map 
	      :visible (and (boundp 'ispell-program-name) ispell-program-name))
  'separator-bookmark)
(define-key-after menu-bar-edit-menu [spell-download-aspell]
	'(menu-item "Download Spell-Checking..." aquamacs-ispell-install
		    :help "Download spell-checking package"
		    :visible (not ispell-program-name))
	'spell) 

(define-key ispell-menu-map [ispell-buffer]
	'(menu-item (aq-shortcut "Spell-Check Buffer               " 
		       'ispell-buffer) ispell-buffer
		    :help "Check spelling of selected buffer"))

;; taken out - the standard Cocoa spell doesn't do it either
;; (defvar aquamacs-flyspell-buffer-checked nil)
;; (make-variable-buffer-local 'aquamacs-flyspell-buffer-checked)

;; (defun aquamacs-flyspell-maybe-check-buffer ()
;;   (unless aquamacs-flyspell-buffer-checked
    
;;     (setq aquamacs-flyspell-buffer-checked t)
    
;;     (if (< (- (point-max) (point-min)) 50000)
;; 	 (flyspell-buffer)
;;       (message "Spell-checking (flyspell) on. M-x flyspell-buffer to check buffer."))))

;; (add-hook 'flyspell-mode-hook 'aquamacs-flyspell-maybe-check-buffer)

;; HELP MENU

(define-key menu-bar-manuals-menu [emacs-lisp-reference]
  '(menu-item "Emacs Lisp Reference (Info)" menu-bar-read-lispref
	      :help "Read the Emacs Lisp Reference manual"))

(define-key menu-bar-manuals-menu [aquamacs-elisp]
  '(menu-item "Emacs Lisp Reference" aquamacs-elisp-reference
	      :help "Read the Emacs Lisp Reference manual"))
(define-key menu-bar-manuals-menu [emac-lisp-intro]
  '(menu-item "Introduction to Emacs Lisp (Info)" menu-bar-read-lispintro
	      :help "Read the Introduction to Emacs Lisp Programming"))

(define-key menu-bar-help-menu [emacs-manual]
  '(menu-item "Read the Emacs Manual (Info)" info-emacs-manual
	      :help "Full documentation of Emacs features"))

(define-key menu-bar-help-menu [menu-aquamacs-help]
  `(menu-item ,(aq-shortcut "Aquamacs Help                     " 
		       'aquamacs-user-help)
	      aquamacs-user-help
	      :key-sequence nil
	      :help "Show Aquamacs Manual in Apple Help"))
  
(define-key-after menu-bar-help-menu [menu-aquamacs-user-wiki]
  `(menu-item ,(aq-shortcut "Aquamacs Tips Wiki Online" 
		       'aquamacs-user-wiki)
	      aquamacs-user-wiki
	      :key-sequence nil
	      :help "Show Wiki (online)")
  'menu-aquamacs-help)


(define-key-after menu-bar-help-menu [menu-aquamacs-homepage]
  `(menu-item ,(aq-shortcut "Aquamacs Homepage                       " 
		       'aquamacs-homepage)
	      aquamacs-homepage
	      :key-sequence nil
	      :help "Show Aquamacs Homepage")
  'menu-aquamacs-user-wiki)

(define-key-after menu-bar-help-menu [menu-aquamacs-emacs-manual]
  `(menu-item ,(aq-shortcut "Emacs Manual                       " 
		       'aquamacs-emacs-manual)
	      aquamacs-emacs-manual
	      :key-sequence nil
	      :help "Show Emacs Manual in Apple Help")
  'menu-aquamacs-homepage)

;; remove this entry, because new versions of Aquamacs are available
;; from the Aquamacs website, not from the FSF
(define-key menu-bar-help-menu [describe-distribution]
    nil)

)

;;; ONE TIME SETUP

; 
;; we will set the following ones directly
;; customization is always possible
;; the existing menu item is badly worded and the C-c/v/x don't apply anyways
;; done
(easy-menu-remove-item global-map  '("menu-bar" "options") 'cua-emulation-mode) 
(easy-menu-remove-item global-map  '("menu-bar" "options") 'uniquify)
(easy-menu-remove-item global-map  '("menu-bar" "options") 'transient-mark-mode)
(easy-menu-remove-item global-map  '("menu-bar" "options") 'case-fold-search)


;; Quit entry shouldnt be there
(easy-menu-remove-item global-map  '("menu-bar" "file") 'separator-exit)
(easy-menu-remove-item global-map  '("menu-bar" "file") 'exit-emacs)

;; About entry is now in application menu
(easy-menu-remove-item global-map  '("menu-bar" "Help") 'about)

;; this is to set the action for the "Quit" function (Emacs menu)
(global-set-key [mac-application-quit] 'save-buffers-kill-emacs)
 

; these problems here are for X-based systems etc. and not relevant
; for Aquamacs users
(easy-menu-remove-item global-map  '("menu-bar" "Help") 'emacs-problems)
 
     
;; Battery status is displayed in menu bar 
;; additional option for this is just confusing
(easy-menu-remove-item global-map  '("menu-bar" "options" "showhide") 'showhide-battery)


;; SENDMAIL doesn't usually work on OS X
;; unless postfix is set up
(easy-menu-remove-item global-map  '("menu-bar" "tools") 'compose-mail)


 
(define-key menu-bar-file-menu [split-window]
  `(menu-item "Split Window" split-window-vertically
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Split selected window in two"))
;; will be moved to Buffers menu later on 
;; but is created here

;; --done

;; move stuff from File to the Buffers menu
 
(setq  menu-bar-buffers-menu-command-entries
       (append 
	       (list '(command-separator "--")
		     (assq 'make-frame menu-bar-file-menu)
		     (assq 'one-window menu-bar-file-menu)
		     (assq 'split-window menu-bar-file-menu))
	       menu-bar-buffers-menu-command-entries))

(assq-delete-all 'make-frame menu-bar-file-menu)
(assq-delete-all 'one-window menu-bar-file-menu)
(assq-delete-all 'split-window menu-bar-file-menu) 
(assq-delete-all 'delete-this-frame menu-bar-file-menu)
(assq-delete-all 'separator-window menu-bar-file-menu)

;; regular setup
(aquamacs-menu-bar-setup)
 
(defvar aquamacs-update-menu-old-state nil)

(defun aquamacs-update-menu (&optional force)
  "Updates the menu bar in Aquamacs if this is necessary.
Call this with FORCE non-nil if you change key-bindings
that should be represented in the Aquamacs menus."
;; We only update if modifiers have changed.
  (condition-case nil
  (let ((state 
	(list mac-control-modifier 
	      mac-command-modifier 
	      mac-option-modifier 
	      mac-function-modifier 
	      osx-key-mode)))
    (unless (and (null force) 
		 (equal aquamacs-update-menu-old-state state))
      (setq aquamacs-update-menu-old-state state)
      (aquamacs-menu-bar-setup)))
  (error nil)))

(add-hook 'menu-bar-update-hook 'aquamacs-update-menu)

 
 (defun aquamacs-user-wiki ()
  (interactive)
  (browse-url "http://aquamacs.org/wiki/"))
 
 (defun aquamacs-homepage ()
  (interactive)
  (browse-url "http://aquamacs.org/"))

(defun emacsosx-mailing-list-subscribe ()
  (interactive)
  (browse-url "mailto:macosx-emacs-on@email.esm.psu.edu?subject=subscribe%20macosx-emacs&body=Send%20off%20this%20e-mail%20to%20subscrube%20to%20the%20Emacs-on-MacOSX%20mailing%20list."))
 
(defun aquamacs-donate ()
  (interactive)
  (browse-url "http://aquamacs.org/donations.shtml"))
 
(defun emacs-user-wiki ()
  (interactive)
  (browse-url "http://www.emacswiki.org/")) 


(easy-menu-add-item  nil '("Help")
  (vector "Emacs Wiki Online" 'emacs-user-wiki) 'emacs-tutorial)
 
(easy-menu-add-item  nil '("Help")
  (vector  "Subscribe to Mailing List..."  'emacsosx-mailing-list-subscribe) 'emacs-tutorial)
(easy-menu-add-item  nil '("Help")
  (vector  "Make a Donation for Aquamacs..."  'aquamacs-donate) 'emacs-tutorial)
(easy-menu-add-item  nil '("Help")
  ["-" nil nil] 'emacs-tutorial)
  

;; workarounds for current bugs

;; done
; can't get rid of the menu bar on a Mac
(easy-menu-remove-item global-map  
		       '("menu-bar" "options" "showhide") 'menu-bar-mode)

; can't show a frame on a different display
(easy-menu-remove-item global-map 
		       '("menu-bar" "file") 'make-frame-on-display)

;; language environment
(unless (boundp 'unicode-emacs)
(when (eq system-type 'darwin) 
    (require 'aquamacs-mule)
(define-key menu-bar-options-menu [mule]
  ;; It is better not to use backquote here,
  ;; because that makes a bootstrapping problem
  ;; if you need to recompile all the Lisp files using interpreted code.
  (list 'menu-item "Aquamacs Multilingual Environment" mule-menu-keymap
;; Most of the MULE menu actually does make sense in unibyte mode,
;; e.g. language selection.
;;;	':visible 'default-enable-multibyte-characters
	':help "Default language, encodings, input method")))

;; (defvar inline-input-method-on nil)

;; for compatibility with earlier Aquamaacs version
(defalias 'mac-inline-input-method-mode 'mac-input-method-mode)


;; (define-minor-mode mac-inline-input-method-mode 
;; "Use the standard Mac input method.
;; This is usually used to allow non-roman scripts to be input.
;; Call this function for the mode to take effect."
;; :init-value t
;; :group 'Aquamacs
;; :global t

;; (when window-system
;; (if mac-inline-input-method-mode
;;     (when (fboundp 'mac-setup-inline-input-method)
;; 	(mac-setup-inline-input-method)
;; 	(add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
;; 	)
;;   (if (fboundp 'mac-exit-inline-input-method)
;;       (mac-exit-inline-input-method))
;;   (remove-hook 'minibuffer-setup-hook 'mac-change-language-to-us))))
 
(when  (fboundp 'mac-input-method-mode)
(define-key-after mule-menu-keymap [toggle-inline-input-method]
  (menu-bar-make-mm-toggle mac-input-method-mode
			   "Use System input method" 
	      "Use native Mac input method")
  'separator-mule)

;; overwrite these with text and :enable
(define-key mule-menu-keymap [toggle-input-method]
  '(menu-item "Toggle Internal Input Method" toggle-input-method
	      :enable (not mac-input-method-mode)))
(define-key mule-menu-keymap [set-input-method]
  '(menu-item "Select Internal Input Method..." set-input-method
	       :enable (not mac-input-method-mode)))

)
)
;; --done

(provide 'aquamacs-menu)
  
