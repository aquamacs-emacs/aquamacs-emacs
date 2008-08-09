;; aquamacs-menu
;; redefines and modifies the menu bar

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-menu.el,v 1.168 2008/08/09 22:12:58 davidswelt Exp $

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
 
;; Copyright (C) 2005, 2006, 2007, 2008 David Reitter
 

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
	 (aquamacs-define-mode-menu (make-sparse-keymap "New Buffer in Mode")
				    'aquamacs-menu-new-empty-buffer-in-mode
				    "Create a new buffer in `%s' mode."))
   (define-key-after menu-bar-file-menu [new-file-menu]
     (list 'menu-item "New Buffer in Mode" menu-bar-new-file-menu
	   :help "Create a new buffer with a specific major mode.")
     'make-tab))

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

 
(defun aquamacs-menu-new-empty-buffer-in-mode (&optional mode)  
  "Create a blan buffer in a given major mode"
  (interactive)
  (let ((the-mode (or last-command-event mode)))
	(new-empty-buffer nil the-mode)
	(message "New %s buffer" (aquamacs-pretty-mode-name major-mode))))

(defun aquamacs-change-major-mode (&optional mode)  
  "Change the current buffer's major mode to another one."
  (interactive)
  (let ((the-mode (or last-command-event mode))
	(buffer (if (minibufferp) (window-buffer (minibuffer-selected-window)) (current-buffer))))
	(with-current-buffer buffer
	  (if (eq major-mode the-mode)
	      (fundamental-mode))
	  (funcall the-mode)
	  (message "Changed to %s Mode" (aquamacs-pretty-mode-name major-mode))
	  )))



(defvar menu-bar-change-mode-menu nil)
; (aquamacs-update-change-mode-menu)
(defun aquamacs-update-change-mode-menu ()
   (setq menu-bar-change-mode-menu 
	 (aquamacs-define-mode-menu (make-sparse-keymap "Change Mode")
				    'aquamacs-change-major-mode
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
  (keymap function-to-call docstring &optional enable-if)
  "Defines a menu consisting of recently and commonly used major modes,
using `aquamacs-recent-major-modes' and `aquamacs-known-major-modes'."

  (unless enable-if
    (setq enable-if 't))
  (aquamacs-define-mode-menu-1 (filter-list aquamacs-known-major-modes aquamacs-recent-major-modes) keymap 
			       function-to-call docstring enable-if)
  (define-key keymap [separator]  '(menu-item "--"))
  (aquamacs-define-mode-menu-1 
   ;; look up texts of mode names in case there are any (for consistency)
   (mapcar (lambda (m)
	     (or (assq m aquamacs-known-major-modes)
		 m))
	   (reverse (filter-list aquamacs-recent-major-modes aquamacs-exclude-major-modes))) 
   keymap 
   function-to-call docstring enable-if))

 

(defun aquamacs-define-mode-menu-1
  (the-list keymap function-to-call docstring enable-if)
  (mapc
   (lambda (modeentry)
     (let ((modename (if (consp modeentry) (car modeentry) modeentry))
	   (displayname (if (consp modeentry) (cdr modeentry) 
			  (aquamacs-pretty-mode-name modeentry))))
     (when (fboundp modename)
         (define-key ;;-after doesn't work with after- why?>? 
	 keymap
	 (vector (list modename)) ;; use mode name as event type (as done in menu-bar.el for buffers)
	 `(menu-item ,displayname ,function-to-call 
		     :help ,(format docstring modename)
		     :enable ,enable-if))))) 
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

;; must come before defcustom. 
(defvar aquamacs-exclude-major-modes
  '(debugger-mode help-mode completion-list-mode)
  "Major Modes ot be excluded from menus")

;; must come after above definitions (setter function!)
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

; (aquamacs-menu-bar-setup)
(defun aquamacs-menu-bar-setup ()

(define-key menu-bar-file-menu [new-file]
  `(menu-item "New Buffer in New Frame        "
	      new-empty-buffer-other-frame
	      :keys ,(aq-binding 'new-empty-buffer-other-frame)
	      :enable (or (and (boundp 'one-buffer-one-frame-mode)
			       one-buffer-one-frame-mode)
			  (not (window-minibuffer-p
			    (frame-selected-window menu-updating-frame))))
	      :help "Create a new buffer in a New Frame"))
 (define-key-after menu-bar-file-menu [make-tab]
  `(menu-item "New Buffer in New Tab            "
	      new-tab
	      :keys ,(aq-binding 'new-tab)
	      :enable (and (fboundp 'new-tab)
			   (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Add a new tab to the window") 'new-file)

(define-key menu-bar-file-menu [open-file] 
  `(menu-item
      "Open File...                             " 
      mac-key-open-file
      :keys ,(aq-binding 'mac-key-open-file)))

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
  `(menu-item "Close Buffer                            " 
	      close-buffer
	      :keys ,(aq-binding (key-binding [menu-bar file kill-buffer]))
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Discard current buffer") 'separator-save)
 
(define-key menu-bar-file-menu [recover-session] nil)

(define-key menu-bar-edit-menu [copy]
  `(menu-item "Copy                                               " 
	      clipboard-kill-ring-save
	      :keys ,(aq-binding (key-binding [menu-bar edit copy]))
	      :enable mark-active
	      :help "Copy selected text in region"))
 
(define-key menu-bar-edit-menu [paste]
  `(menu-item "Paste                                               " 
	      clipboard-yank
	      :keys ,(aq-binding 'clipboard-yank) 
	      :enable (and
		       ;; Emacs compiled --without-x doesn't have
		       ;; x-selection-exists-p.
		       (fboundp 'x-selection-exists-p)
		       (x-selection-exists-p) (not buffer-read-only)
		       (menu-bar-menu-frame-live-and-visible-p))
	      :help "Paste (yank) text most recently cut/copied"))

(define-key menu-bar-edit-menu [paste-from-menu]
  '(menu-item "Paste Previous" yank-menu
	      :enable (and (cdr yank-menu) (not buffer-read-only))
	      :help "Choose a string from the kill ring and paste it"))

(require 'aquamacs-redo)
(define-key menu-bar-edit-menu [undo]
  `(menu-item   "Undo                                               "
	      aquamacs-undo
	      :keys ,(aq-binding (key-binding [menu-bar edit undo]))
	      :enable (and (aquamacs-can-undo-p)
			   (menu-bar-menu-frame-live-and-visible-p))
	      :help "Undo last operation"))

(define-key-after menu-bar-edit-menu [redo]
  `(menu-item "Redo                                               " 
	      aquamacs-redo
	      :keys ,(aq-binding (key-binding [menu-bar edit redo]))
	      :enable (and (aquamacs-can-redo-p) 
			   (menu-bar-menu-frame-live-and-visible-p))
	      :help "Redo undone operation") 'undo)

(easy-menu-add-item  nil '("Edit")
  ["-" nil nil] 'cut)

;;done
(define-key menu-bar-edit-menu [cut]
  `(menu-item "Cut                                                  "
	      clipboard-kill-region
	      :keys ,(aq-binding (key-binding [menu-bar edit cut]))
	      :enable (and mark-active (not buffer-read-only))
	      :help
	      "Delete text in region and copy it to the clipboard"))

(define-key menu-bar-edit-menu [mark-whole-buffer]
  `(menu-item "Select All                                         " 
	      mark-whole-buffer
	      :keys ,(aq-binding (key-binding [menu-bar edit mark-whole-buffer]))
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Mark the whole buffer for a subsequent cut/copy."))

(define-key menu-bar-i-search-menu [isearch-forward]
  `(menu-item "Forward String...                         " 
	      isearch-forward
	      :keys ,(aq-binding 'isearch-forward)
	      :help "Search forward for a string as you type it"))
 
(define-key menu-bar-i-search-menu [isearch-repeat-forward]
  `(menu-item "Repeat Forward String...             " 
	      aquamacs-repeat-isearch
	      :keys ,(aq-binding 'aquamacs-repeat-isearch)
	      :help "Search forward for a string as you type it"))

(define-key menu-bar-i-search-menu [isearch-backward]
  `(menu-item "Repeat Backward String...           "
	      aquamacs-repeat-isearch-backward
	      :keys ,(aq-binding 'aquamacs-repeat-isearch-backward)
	      :help "Search backwards for a string as you type it"))

(define-key menu-bar-i-search-menu [isearch-use-region]
  `(menu-item "Use Region For Search                " 
	      aquamacs-use-selection-for-find
	      :enable mark-active
	      :keys ,(aq-binding (key-binding [menu-bar edit search i-search 
							isearch-use-region]))
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
`(menu-item "Wrap and Re-Format (fill)                " 
	    fill-paragraph-or-region
	    :keys ,(aq-binding (key-binding [menu-bar edit fill]))
	    :enable (not buffer-read-only)
	    :help
	    "Fill text in region (or paragraph) to fit between
left and right margin"))

(define-key-after menu-bar-edit-menu [unfill]
`(menu-item "Remove Hard Line Breaks (unfill)     " 
	    unfill-paragraph-or-region
	    :keys ,(aq-binding (key-binding [menu-bar edit unfill]))
	    :enable (not buffer-read-only)
	    :help
	    "Remove line-breaks from paragraph or region.")
'fill)
(define-key-after menu-bar-edit-menu [separator-fill]
  '(menu-item "--")
  'unfill)
  
;; this needs an extension to show the keyboard shortcut
;; interesting extensions to menu-item: (:visible nil), (:keys)

 (define-key-after menu-bar-file-menu [my-file-separator]
          '(menu-item "--") 'recover-session)
 (define-key-after menu-bar-file-menu [mac-show-in-finder]
          '(menu-item "Reveal in Finder" mac-key-show-in-finder

		      :enable (and  (menu-bar-menu-frame-live-and-visible-p)
				    (menu-bar-non-minibuffer-window-p)
				    buffer-file-number)
		      ) 'my-file-separator)

;; save as (redefinition for :enable)

(define-key menu-bar-file-menu [save-buffer ]
  `(menu-item "Save Buffer                              "
	      save-buffer
	      :keys ,(aq-binding 'mac-key-save-file)
	      :enable (and (buffer-modified-p)
			   (buffer-file-name)
			   (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Save current buffer to its file"
	      ))  

(define-key menu-bar-file-menu [write-file]
  `(menu-item "Save Buffer As...                      "
	      write-file
	      :keys ,(aq-binding 'mac-key-save-file-as)
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
  `(menu-item (format "Preview and Print %s...       " 
		      (if mark-active "Region" "Buffer"))
	      aquamacs-print
	      :keys ,(aq-binding  (key-binding [menu-bar file aquamacs-print]))
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Print current buffer or region"))



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
  `(menu-item ,(format "Quick Print %s       " 
		      (if mark-active "Region" "Buffer"))  
	      menu-bar-print-region-or-buffer
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :keys ,(aq-binding (key-binding [menu-bar file print-region-or-buffer]))
	      :help "Print buffer, or region if active"))



;(require 'longlines) 
 
;;  backward compatibility (in case users have it in their customizations)
(defun turn-on-longlines ()
  "Turn on Longlines mode.
... unless buffer is read-only."
  (unless buffer-read-only
    (require 'longlines)
    (longlines-mode 1)))

(defun turn-off-longlines ()
  "Unconditionally turn off Longlines mode."
  (and (boundp 'longlines-mode)
       (longlines-mode -1)))
; (custom-add-option 'text-mode-hook 'turn-on-longlines)

(defun toggle-longlines ()
  "Toggle whether to use Longlines Mode."
  (interactive)
  (require 'longlines-mode)
  (unless longlines-mode 
    (auto-fill-mode -1))
  (longlines-mode))


(defun turn-on-word-wrap ()
  "Turn on Word Wrap mode in current buffer."
  (turn-off-longlines)
  (turn-off-auto-fill)
  (setq word-wrap t))

(defun turn-off-word-wrap ()
  "Turn off Word Wrap mode in current buffer."
  (setq word-wrap nil))

(defun toggle-word-wrap ()
  "Toggle whether to use Word Wrap."
  (interactive)
  (if word-wrap
      (turn-off-word-wrap)
    (turn-on-word-wrap)))

(defun toggle-auto-fill ()
  "Toggle whether to use Auto Fill Mode."
  (interactive)
  (unless auto-fill-function 
    (setq word-wrap nil)
    (and (boundp 'longlines-mode)
	 (longlines-mode -1))) ;; turn this off first if it is on
  (auto-fill-mode)
  (message "Hard word wrap %s"
	     (if auto-fill-function
		 "enabled" "disabled")))
  
;; redefines function from simple.el
(defun toggle-truncate-lines (&optional arg)
  "Toggle whether to fold or truncate long lines for the current buffer.
With prefix argument ARG, truncate long lines if ARG is positive,
otherwise don't truncate them.  Note that in side-by-side
windows, truncation is always enabled."
  (interactive "P")
  (setq truncate-lines
	(if (null arg)
	    (not truncate-lines)
	  (> (prefix-numeric-value arg) 0)))
  (if truncate-lines
      (setq word-wrap nil))
  (force-mode-line-update)
  (unless truncate-lines
    (let ((buffer (current-buffer)))
      (walk-windows (lambda (window)
		      (if (eq buffer (window-buffer window))
			  (set-window-hscroll window 0)))
		    nil t)))
  (message "Truncate long lines %s"
	   (if truncate-lines "enabled" "disabled")))


(require 'aquamacs-editing)
(custom-add-option 'text-mode-hook 'auto-detect-wrap)
(defun toggle-auto-text-mode-wrap ()
  "Toggle whether to automatically turn on word-wrap in Text mode and related modes.
This command affects all buffers that use modes related to Text mode,
both existing buffers and buffers that you subsequently create."
  (interactive)
  ;; remove leftover customizations from previous versions
  (remove-hook 'text-mode-hook 'turn-on-auto-fill)
  (remove-hook 'text-mode-hook 'turn-on-longlines)
  (remove-hook 'text-mode-hook 'turn-on-word-wrap)
  (let ((enable-mode (not (or (memq 'auto-detect-wrap text-mode-hook)
			      (memq 'auto-detect-longlines text-mode-hook)))))
    (if enable-mode
	(add-hook 'text-mode-hook 'auto-detect-wrap)
      (progn
	(remove-hook 'text-mode-hook 'auto-detect-wrap)
	(remove-hook 'text-mode-hook 'auto-detect-longlines))) ; longlines was used up to version 1.4
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(if (or (derived-mode-p 'text-mode) text-mode-variant)
	    (auto-detect-wrap))))
    (message "Auto Soft Wrap %s in Text modes"
	     (if enable-mode "enabled" "disabled"))))

(defun menu-bar-auto-text-mode-wrap ()
  (interactive)
  (toggle-auto-text-mode-wrap)
  (customize-mark-as-set 'text-mode-hook))
  


(define-key menu-bar-options-menu [auto-fill-mode]
  '(menu-item "Hard Word Wrap"
              toggle-auto-fill
	      :help "Automatically fill text between left and right margins (Auto Fill)"
	      :enable (menu-bar-menu-frame-live-and-visible-p)
              :button (:toggle . auto-fill-function)))

(define-key-after menu-bar-options-menu [word-wrap]
  '(menu-item "Soft Word Wrap"
	      toggle-word-wrap
	      :help "Wrap long lines without inserting carriage returns (Word Wrap)"
	      :enable (menu-bar-menu-frame-live-and-visible-p)
              :button (:toggle . word-wrap)) 'auto-fill-mode)

(define-key-after menu-bar-options-menu [truncate-lines]
  '(menu-item "Truncate Long Lines"
	      toggle-truncate-lines
	      :help "Truncate long lines on the screen"
	      :button (:toggle . truncate-lines)
	      :enable (menu-bar-menu-frame-live-and-visible-p)) 'word-wrap)


(define-key-after menu-bar-options-menu [auto-wrap]
  '(menu-item "Auto Word Wrap in Text Modes"
	      menu-bar-auto-text-mode-wrap
	      :help "Automatically use hard or soft word wrap (Auto Fill / Longlines) in text modes."
	      :button (:toggle . (if (listp text-mode-hook)
				     (or (member 'auto-detect-wrap text-mode-hook)
					 (member 'auto-detect-longlines text-mode-hook))
				   (or (eq 'auto-detect-wrap text-mode-hook)
				       (eq 'auto-detect-longlines text-mode-hook)))))
  'word-wrap)
 
;; in edit menu

(define-key menu-bar-search-menu [case-fold-search]
  (menu-bar-make-toggle toggle-case-fold-search case-fold-search
			"Case-Insensitive Search"
			"Case-Insensitive Search %s"
			"Ignore letter-case in search"))

(when window-system
    (require 'aquamacs-frame-setup)
    (define-key-after menu-bar-options-menu [tabbar]
      (menu-bar-make-mm-toggle
       tabbar-mode
       "Show Tabs"
       "Open a new tab for each new buffer.") 'edit-options-separator)
    (define-key-after menu-bar-options-menu [oneonone]
      (menu-bar-make-mm-toggle 
       one-buffer-one-frame-mode
       "Show Buffers in New Frames"
       "Make new frames to switch or pop to other buffers.") 'tabbar)
    (define-key-after menu-bar-options-menu [obof-separator]  '(menu-item "--") 'oneonone))

(when (fboundp 'mac-font-panel-mode)
  (defun turn-on-mac-font-panel-mode ()
    (interactive)
    (mac-font-panel-mode 1))

 ;; this is a redefine

  (define-key menu-bar-showhide-menu [mac-font-panel-mode]
    nil)
  (define-key menu-bar-options-menu [mouse-set-font]
  `(menu-item "Font (this Frame)...                 " 
	      turn-on-mac-font-panel-mode
	       :visible ,(display-multi-font-p)
	       :keys ,(aq-binding 'mac-font-panel-mode)
	       :enable (menu-bar-menu-frame-live-and-visible-p) 
	       :help "Select a font from list of known fonts/fontsets")))

(define-key menu-bar-options-menu [highlight-paren-mode] nil)
(define-key menu-bar-options-menu [highlight-separator] nil)
(define-key-after menu-bar-showhide-menu [highlight-separator] '("--"))
(define-key-after menu-bar-showhide-menu [highlight-paren-mode]
  (menu-bar-make-mm-toggle show-paren-mode
			   "Paren Match Highlighting"
			   "Highlight matching/mismatched parentheses at cursor (Show Paren mode)"))

;; do this here as well to make sure it follows highlight-paren-mode
(if (fboundp 'global-show-newlines-mode)
    (define-key-after menu-bar-showhide-menu [show-newlines-mode]
      (menu-bar-make-mm-toggle global-show-newlines-mode
			       "Show Newlines"
			       "Show hard newlines") 'highlight-paren-mode))


(easy-menu-add-item  nil '("Options")
  ["-" nil nil] 'mouse-set-font)

(define-key menu-bar-options-menu [blink-cursor-mode] nil)
(define-key menu-bar-options-menu [cursor-separator] nil)


(define-key-after menu-bar-options-menu [aquamacs-color-theme-select]
  `(menu-item "Color Theme (this Frame)..." aquamacs-color-theme-select
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
  (fringe-mode (cons 4 0)))
  ;; Unfortunately, fringe-mode likes to round up fringes.
  ;; Therefore, we set both to 1.
 ;;  (customize-set-variable 'fringes-outside-margins 1)
;;   (customize-set-variable 'left-fringe-width 4)
;;   (customize-set-variable 'left-margin-width 1)
;;   (customize-set-variable 'right-fringe-width 1)
;;   (customize-set-variable 'right-margin-width 1)

;;   (setq default-fringes-outside-margins 1)
;;   (setq default-left-fringe-width 1)
;;   (setq default-left-margin-width 0)
;;   (setq default-right-fringe-width 1)
;;   (setq default-right-margin-width 0)
;;  (aquamacs-define-the-fringe-bitmap) ;; redefine (NOT HERE)
;  (customize-set-variable 'fringe-mode '(4 . 0))) 
; fringe-mode
; (fringe-mode (cons 4  0))

(define-key-after menu-bar-showhide-fringe-menu [small]
  `(menu-item "Small left fringe" 
	      aquamacs-menu-bar-showhide-fringe-menu-customize-small
	      :help "Narrow fringe, left only"
	      :visible ,(display-graphic-p)
	      :button (:radio . (equal fringe-mode '(4 . 0)))) 'none)

  
(define-key-after menu-bar-options-menu [file-backups]
  (menu-bar-make-toggle toggle-make-backup-files make-backup-files  
			(format "Make Backup Files %s" (if backup-inhibited "(inhibited here)" ""))
			"Making backup files: %s"
			"Create a backup file when saving"
			) 'obof-separator)
;(define-key-after menu-bar-options-menu [backup-separator]  
;  '(menu-item "--") 'file-backups)


;; not important enough to warrant a menu entry
(easy-menu-remove-item global-map  '("menu-bar" "options") 'save-place) 

 

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
  `(menu-item "Goto Line...                      "
	      goto-line
	      :keys ,(aq-binding 'goto-line)
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

;; ispell is not loaded at startup
;; spell checking 
;; potential for trouble here:
;; if exec path isn't initialized (because PATH not read yet)
;; then locate-file might fail and we don't know if we're going to have ispell

(defun aquamacs-initialize-ispell-program-name ()
  (let ((ipn (or (if (locate-file "aspell" exec-path exec-suffixes 'file-executable-p) 
		     "aspell")
		 (if (locate-file "ispell" exec-path exec-suffixes 'file-executable-p)
		     "ispell"))))
    (if ipn ;; do not initialize if not (yet) found
	(defvar ispell-program-name ipn))))

(aquamacs-initialize-ispell-program-name)
(unless (boundp 'ispell-program-name)
  (add-hook 'after-init-hook 'aquamacs-initialize-ispell-program-name 'append))


(define-key-after menu-bar-edit-menu [spell]
  '(menu-item "Spelling" ispell-menu-map 
	      :visible (and (boundp 'ispell-program-name) ispell-program-name))
  'separator-bookmark)
(define-key-after menu-bar-edit-menu [spell-download-aspell]
	'(menu-item "Download Spell-Checking..." aquamacs-ispell-install
		    :help "Download spell-checking package"
		    :visible (and (boundp 'ispell-program-name)
				  (not ispell-program-name)))
	'spell) 

(define-key ispell-menu-map [ispell-buffer]
	`(menu-item "Spell-Check Buffer               " 
		    ispell-buffer
		    :keys ,(aq-binding 'ispell-buffer)
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


;; About entry is now in application menu
(easy-menu-remove-item global-map  '("menu-bar" "Help") 'about)

(easy-menu-add-item  nil '("Help")
  (vector  "Subscribe to Mailing List..."  'emacsosx-mailing-list-subscribe) 'emacs-tutorial)
(easy-menu-add-item  nil '("Help")
  (vector  "Make a Donation for Aquamacs..."  'aquamacs-donate) 'emacs-tutorial)
(easy-menu-add-item  nil '("Help")
  (vector "-" nil nil) 'emacs-tutorial)
(easy-menu-add-item  nil '("Help")
  (vector "Emacs Wiki Online" 'emacs-user-wiki) 'emacs-tutorial)
  


(define-key menu-bar-help-menu [menu-aquamacs-help]
  `(menu-item "Aquamacs Help                            " 
	      aquamacs-user-help
	      :keys ,(aq-binding 'aquamacs-user-help)
	      :help "Show Aquamacs Manual in Apple Help"))
  
(define-key-after menu-bar-help-menu [menu-aquamacs-user-wiki]
  `(menu-item "Aquamacs Tips Wiki Online" 
	      aquamacs-user-wiki
	      :keys ,(aq-binding 'aquamacs-user-wiki)
	      :help "Show Wiki (online)")
  'menu-aquamacs-help)


(define-key-after menu-bar-help-menu [menu-aquamacs-homepage]
  `(menu-item "Aquamacs Homepage                              " 
	      aquamacs-homepage
	      :keys ,(aq-binding 'aquamacs-homepage)
	      :help "Show Aquamacs Homepage")
  'menu-aquamacs-user-wiki)

(define-key-after menu-bar-help-menu [menu-aquamacs-emacs-manual]
  `(menu-item "Emacs Manual                              " 
	      aquamacs-emacs-manual
	      :keys ,(aq-binding 'aquamacs-emacs-manual)
	      :help "Show Emacs Manual in Apple Help")
  '-)


(define-key-after menu-bar-help-menu [sep3]
  '("--")
  'sep1)
(define-key-after menu-bar-help-menu [emac-lisp-intro]
  '(menu-item "Introduction to Emacs Lisp (Info)" menu-bar-read-lispintro
	      :help "Read the Introduction to Emacs Lisp Programming")
  'sep1)
(define-key-after menu-bar-help-menu [emacs-lisp-reference]
  '(menu-item "Emacs Lisp Reference (Info)" menu-bar-read-lispref
	      :help "Read the Emacs Lisp Reference manual")
  'sep1)
(define-key-after menu-bar-help-menu [aquamacs-elisp]
  '(menu-item "Emacs Lisp Reference" aquamacs-elisp-reference
	      :help "Read the Emacs Lisp Reference manual")
  'sep1)
(define-key menu-bar-manuals-menu [emacs-lisp-reference] nil)
(define-key menu-bar-manuals-menu [emacs-lisp-intro] nil)


(define-key menu-bar-manuals-menu [sep]
  '("--"))
(define-key menu-bar-manuals-menu [emacs-psychotherapist]
  '(menu-item "Emacs Psychotherapist" doctor
	      :help "Our doctor will help you feel better"))
(define-key menu-bar-help-menu [emacs-psychotherapist] nil)

(define-key menu-bar-manuals-menu [lookup-subject-in-all-manuals] nil)

(define-key menu-bar-search-documentation-menu [lookup-subject-in-all-manuals]
  '(menu-item "Look Up Subject in All Manuals..." info-apropos
	      :help "Find description of a subject in all installed manuals"))

(define-key menu-bar-search-documentation-menu [emacs-terminology] nil)

(define-key-after menu-bar-help-menu [emacs-terminology]
  '(menu-item "Emacs Terminology" search-emacs-glossary
	      :help "Display the Glossary section of the Emacs manual")
  'emacs-tutorial-language-specific)


;; remove this entry, because new versions of Aquamacs are available
;; from the Aquamacs website, not from the FSF
(define-key menu-bar-help-menu [describe-distribution]
    nil)
(define-key menu-bar-help-menu [getting-new-versions]
    nil)
(define-key menu-bar-help-menu [about-emacs]
    nil)
(define-key menu-bar-help-menu [about-gnu-project]
    nil) ;; this is available from the general About screen
(define-key menu-bar-help-menu [sep4]
    nil)
(define-key menu-bar-help-menu [emacs-problems]
    nil)
; these problems here are for X-based systems etc. and not relevant
; for Aquamacs users
(define-key menu-bar-help-menu [emacs-manual]
    nil)

;;defvar
(setq menu-bar-manuals-menu (make-sparse-keymap "More Manuals"))

(define-key menu-bar-manuals-menu [man]
  '(menu-item "Read Man Page..." manual-entry
	      :help "Man-page docs for external commands and libraries"))
(define-key menu-bar-manuals-menu [sep2]
  '("--"))
(define-key menu-bar-manuals-menu [order-emacs-manuals]
  '(menu-item "Ordering Manuals" view-order-manuals
	      :help "How to order manuals from the Free Software Foundation"))
(define-key menu-bar-manuals-menu [lookup-subject-in-all-manuals]
  '(menu-item "Lookup Subject in all manuals..." info-apropos
	      :help "Find description of a subject in all installed manuals"))
(define-key menu-bar-manuals-menu [other-manuals] nil)

(define-key-after menu-bar-help-menu [other-manuals]
  '(menu-item "Package Manuals" Info-directory
	      :help "Read any of the installed manuals") 'emacs-known-problems)



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

;; this is to set the action for the "Quit" function (Emacs menu)
(global-set-key [mac-application-quit] 'save-buffers-kill-emacs)
 

     
;; Battery status is displayed in menu bar 
;; additional option for this is just confusing
(easy-menu-remove-item global-map  '("menu-bar" "options" "showhide") 'showhide-battery)


;; SENDMAIL doesn't usually work on OS X
;; unless postfix is set up
(easy-menu-remove-item global-map  '("menu-bar" "tools") 'compose-mail)


(defun aquamacs-toggle-full-frame ()
  "Enlarge the selected frame to the full screen.
Unlike `mac-toggle-full-frame', this will do a better job at remembering
the previous frame size."
  (interactive)
  (if (frame-parameter nil 'fullscreen)    ;(frame-full-screen-p)
      (modify-frame-parameters 
       nil (list (cons 'fullscreen nil) 
		 (cons 'prior-left nil)  ;; needed for smart-frame-positioning (sessions persistency)
		 (cons 'prior-top nil)
		 (cons 'prior-width nil)
		 (cons 'prior-height nil)))
    ;; (frame-parameters)
    ;; save small frame position
    (modify-frame-parameters 
     nil (list (cons 'fullscreen 'fullboth) 
	       (cons 'prior-left (frame-parameter nil 'left))
	       (cons 'prior-top (frame-parameter nil 'top))
	       (cons 'prior-width (frame-parameter nil 'width))
	       (cons 'prior-height (frame-parameter nil 'height)))))
  
  (if (frame-parameter nil 'fullscreen) ; (frame-full-screen-p)
      (run-with-idle-timer 1 nil (lambda () (message  (substitute-command-keys "Press \\[aquamacs-toggle-full-frame] to exit full screen editing.")))))
  nil)

;; (set-frame-parameter nil 'fullscreen 'fullboth)
 

(define-key menu-bar-file-menu [one-window]
  `(menu-item "Remove Splits                    " 
	      aquamacs-delete-other-windows
	      :keys ,(aq-binding 'aquamacs-delete-other-windows)
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p)
			   (not (one-window-p t nil)))
	      :help "Selected window grows to fill the whole frame"))
(define-key menu-bar-file-menu [split-window]
  `(menu-item "Split Window                      " 
	      aquamacs-split-window-vertically
	      :keys ,(aq-binding 'aquamacs-split-window-vertically)
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Split selected window in two"))
(define-key menu-bar-file-menu [make-frame]
  `(menu-item "Show Buffer in New Frame " make-frame-command
	      :visible (fboundp 'make-frame-command)
	      ;; no key: use standard notation
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Open a new frame"))
(define-key menu-bar-file-menu [tile-frames]
  `(menu-item "Scatter frames" scatter-frames
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Place frames sensibly"))
(define-key menu-bar-file-menu [tile-frames-v]
  `(menu-item "Tile frames vertically" tile-frames-vertically
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Tile frames vertically"))
(define-key menu-bar-file-menu [tile-frames-h]
  `(menu-item "Tile frames horizontally" tile-frames-horizontally
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Tile frames horizontally"))
(if (boundp 'mac-autohide-menubar-on-fullscreen)
    (define-key menu-bar-file-menu [full-frame]
      `(menu-item "Full Screen Editing             " 
		  aquamacs-toggle-full-frame
		  :keys ,(aq-binding 'aquamacs-toggle-full-frame)
		  :enable (menu-bar-menu-frame-live-and-visible-p)
		  :help "Use full screen for the selected frame")))
  

;; will be moved to Buffers menu later on 
;; but is created here

;; --done
;; (tabbar-window-buffer-list)
;; move stuff from File to the Buffers menu
 
(setq  menu-bar-buffers-menu-command-entries
       (append 
	       (list 
		'(command-separator "--")
		(assq 'make-frame menu-bar-file-menu)
		(assq 'full-frame menu-bar-file-menu)
		(assq 'one-window menu-bar-file-menu)
		(assq 'split-window menu-bar-file-menu)
		'(command-separator "--")
		(assq 'place-frame menu-bar-file-menu)
		(assq 'tile-frames menu-bar-file-menu)
		(assq 'tile-frames-h menu-bar-file-menu)
		(assq 'tile-frames-v menu-bar-file-menu)
		'(command-separator "--")
		(list 'next-buffer
		      'menu-item
		      '(format "Select Next %s               "  
			       (if tabbar-mode "Tab   " "Buffer"))
		      'next-tab-or-buffer 
		      :keys (aq-binding 'next-tab-or-buffer)
		      :enable '(and (menu-bar-menu-frame-live-and-visible-p)
				    (menu-bar-non-minibuffer-window-p)
				    (or (not tabbar-mode)
					(not (tabbar-tabset-only-tab
					      (tabbar-selected-tab
					       (tabbar-current-tabset t))))))
		      :help "Switch to the \"next\" buffer in a cyclic order")
		(list 'previous-buffer
		      'menu-item
		      '(format "Select Previous %s         " 
			       (if tabbar-mode "Tab   " "Buffer"))
		      'previous-tab-or-buffer
		      :keys  (aq-binding 'next-tab-or-buffer)
		      :enable '(and (menu-bar-menu-frame-live-and-visible-p)
				    (menu-bar-non-minibuffer-window-p)
				    (or (not tabbar-mode)
					(not (tabbar-tabset-only-tab
					 (tabbar-selected-tab
					  (tabbar-current-tabset t))))))
		      :help "Switch to the \"previous\" buffer in a cyclic order")
		(list 'movetab
		      'menu-item 
		      "Move Tab to New Frame       " 
		      'tabbar-move-current-buffer-to-new-frame
		      :keys  (aq-binding 'tabbar-move-current-buffer-to-new-frame)
		      :enable '(and (menu-bar-menu-frame-live-and-visible-p)
				    (menu-bar-non-minibuffer-window-p)
				    (or (not tabbar-mode)
					(not (tabbar-tabset-only-tab
					      (tabbar-selected-tab
					       (tabbar-current-tabset t))))))
		      :help "Move the current Tab to New Frame")
		(list 'removetab
		      'menu-item 
		      '(format "Remove %s" 
			       (if tabbar-mode "Tab" "Window")) 
		      'tabbar-delete-current-tab
		      :enable '(and (menu-bar-menu-frame-live-and-visible-p)
				    (menu-bar-non-minibuffer-window-p))
		      :help "Remove the current Tab without killing the buffer.")
		(list 'mergetabs
		      'menu-item
		      "Merge All Frames       " 
		      'tabbar-window-merge-windows
		      :keys  (aq-binding 'tabbar-window-merge-windows)
		      :enable '(and (menu-bar-menu-frame-live-and-visible-p)
				   (menu-bar-non-minibuffer-window-p))
		      :help "Merge all Frames into a single one with tabs")
		)))

;(assq-delete-all 'select-named-buffer menu-bar-buffers-menu-command-entries)
;(assq-delete-all 'list-all-buffers menu-bar-buffers-menu-command-entries)

(assq-delete-all 'full-frame menu-bar-file-menu)
(assq-delete-all 'make-frame menu-bar-file-menu)
(assq-delete-all 'make-tab menu-bar-file-menu)
(assq-delete-all 'one-window menu-bar-file-menu)
(assq-delete-all 'split-window menu-bar-file-menu) 
(assq-delete-all 'delete-this-frame menu-bar-file-menu)
(assq-delete-all 'separator-window menu-bar-file-menu)
(assq-delete-all 'tile-frames menu-bar-file-menu)
(assq-delete-all 'tile-frames-h menu-bar-file-menu)
(assq-delete-all 'tile-frames-v menu-bar-file-menu)
 
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


(menu-bar-update-buffers) ;; update Buffers menu now
(aquamacs-update-menu t) ;; initial setup of the menu
 
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
  (list 'menu-item "Language" mule-menu-keymap
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
			   "Use System Input Method" 
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

(require 'recentf)
(ats "recentf loaded")
(aquamacs-set-defaults 
 '(
   (recentf-max-menu-items 25)
   (recentf-menu-before  "Open Directory...                 ")
   (recentf-keep ( mac-is-mounted-volume-p file-remote-p file-readable-p))
   (recentf-filename-handlers '(abbreviate-file-name))
   (recentf-menu-filter aquamacs-recentf-show-basenames)))  
(setq recentf-menu-items-for-commands
      (list ["Clear Menu"
	     recentf-clearlist
	     :help "Remove all excluded and non-kept files from the recent list"
	     :active t]))
(recentf-mode 1)  
(global-set-key "\C-x\ \C-r" 'recentf-open-files)  



(add-hook 'menu-bar-update-hook 'aquamacs-update-menu)

(add-hook 'after-change-major-mode-hook 'aquamacs-record-mode-change)
 
(add-hook 'after-init-hook 'aquamacs-update-new-file-menu)

(provide 'aquamacs-menu)
  
