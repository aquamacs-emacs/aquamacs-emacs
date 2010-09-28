;; aquamacs-menu
;; redefines and modifies the menu bar

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
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
 
;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010 David Reitter
 

(require 'easymenu)



(eval-when-compile (require 'aquamacs-macros))

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
  (protect
   (setq menu-bar-new-file-menu 
	 (aquamacs-define-mode-menu (make-sparse-keymap "New Buffer in Mode")
				    'aquamacs-menu-new-empty-buffer-in-mode
				    "Create a new buffer in `%s' mode."))
   (define-key-after menu-bar-file-menu [new-file-menu]
     (list 'menu-item "New Buffer in Mode" menu-bar-new-file-menu
	   :help "Create a new buffer with a specific major mode.")
     'make-tab)))

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
  (the-list keymap function-to-call docstring enable-if &optional do-not-filter)
  (mapc
   (lambda (modeentry)
     (let ((modename (if (consp modeentry) (car modeentry) modeentry))
	   (displayname (if (consp modeentry) (cdr modeentry) 
			  (aquamacs-pretty-mode-name modeentry))))
     (when (or do-not-filter (fboundp modename))
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
    org-mode
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
    org-mode
    wikipedia-mode
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


(defvar aquamacs-menu-setup-hook nil "Hook run after updating the Aquamacs menu")


; (aquamacs-menu-bar-setup)
(defun aquamacs-menu-bar-setup ()

(define-key menu-bar-file-menu [new-file]
  `(menu-item "New Buffer in New Frame"
	      new-empty-buffer-other-frame
	      :key-sequence [(,osxkeys-command-key n)]
	      :enable (or (and (boundp 'one-buffer-one-frame-mode)
			       one-buffer-one-frame-mode)
			  (not (window-minibuffer-p
			    (frame-selected-window menu-updating-frame))))
	      :help "Create a new buffer in a New Frame"))
 (define-key-after menu-bar-file-menu [make-tab]
  `(menu-item "New Buffer in New Tab"
	      new-tab
	      :key-sequence [(,osxkeys-command-key t)]
	      :enable (and (fboundp 'new-tab)
			   (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Add a new tab to the window") 'new-file)

(define-key menu-bar-file-menu [open-file] 
  `(menu-item
      "Open File..." 
      mac-key-open-file
      :key-sequence [(,osxkeys-command-key s)]
      ))


(require 'recentf)
(ats "recentf loaded")
(aquamacs-set-defaults 
 '((buffers-menu-max-size nil)
   (recentf-menu-before "Open Directory...")
   (recentf-max-menu-items 25)
   ;; must be set before turning on recentf mode
   (recentf-keep ( mac-is-mounted-volume-p file-remote-p file-readable-p))
   (recentf-filename-handlers '(abbreviate-file-name))
   (recentf-menu-filter aquamacs-recentf-show-basenames)))

(setq recentf-menu-items-for-commands
      (list ["Clear Menu"
	     recentf-clearlist
	     :help "Remove all excluded and non-kept files from the recent list"
	     :active t]))
(global-set-key "\C-x\ \C-r" 'recentf-open-files)  

(recentf-mode 1) 

;; redefine this
(define-key-after menu-bar-file-menu [kill-buffer]
  `(menu-item (format "Close Window %s"  
		      (if (or tabbar-mode one-buffer-one-frame-mode) 
			  "and Buffer" ""))
	      close-window
	      :key-sequence [(,osxkeys-command-key w)]
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Discard current buffer") 'separator-save)
 
(define-key menu-bar-file-menu [recover-session] nil)

(define-key menu-bar-edit-menu [copy]
  `(menu-item "Copy\t\t" 
	      clipboard-kill-ring-save
	      :key-sequence [(,osxkeys-command-key c)]
	      :enable mark-active
	      :help "Copy selected text in region"))
 
(define-key-after menu-bar-edit-menu [copy-html]
  `(menu-item "Copy Styled as HTML" 
	      aquamacs-copy-as-html
	      :enable mark-active
	      :help "Copy selected text in region as formatted HTML")
  'copy)

(define-key menu-bar-edit-menu [paste]
  `(menu-item "Paste\t\t" 
	      clipboard-yank
	      :key-sequence [(,osxkeys-command-key v)]
	      :enable (and
		       (cdr yank-menu)
		       ;; Emacs compiled --without-x doesn't have
		       ;; x-selection-exists-p.
		       (not buffer-read-only)
		       (menu-bar-menu-frame-live-and-visible-p))
	      :help "Paste (yank) text most recently cut/copied"))

(require 'aquamacs-redo)
(define-key menu-bar-edit-menu [undo]
  `(menu-item   "Undo\t\t"
	      aquamacs-undo
	      :key-sequence [(,osxkeys-command-key z)]
	      :enable (and (aquamacs-can-undo-p)
			   (menu-bar-menu-frame-live-and-visible-p))
	      :help "Undo last operation"))

(define-key-after menu-bar-edit-menu [redo]
  `(menu-item "Redo" 
	      aquamacs-redo
	      :key-sequence [(,osxkeys-command-key Z)]
	      :enable (and (aquamacs-can-redo-p) 
			   (menu-bar-menu-frame-live-and-visible-p))
	      :help "Redo undone operation") 'undo)

;;done
(define-key menu-bar-edit-menu [cut]
  `(menu-item "Cut"
	      clipboard-kill-region
	      :key-sequence [(,osxkeys-command-key x)]
	      :enable (and mark-active (not buffer-read-only) 
			   (menu-bar-menu-frame-live-and-visible-p))
	      :help
	      "Delete text in region and copy it to the clipboard"))

(define-key menu-bar-edit-menu [mark-whole-buffer]
  `(menu-item "Select All" 
	      mark-whole-buffer
	      :key-sequence [(,osxkeys-command-key a)]
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Mark the whole buffer for a subsequent cut/copy."))

(define-key menu-bar-i-search-menu [isearch-forward]
  `(menu-item "Forward String..." 
	      isearch-forward
	      :key-sequence [(,osxkeys-command-key f)]
	      :help "Search forward for a string as you type it"))
 
(define-key menu-bar-i-search-menu [isearch-repeat-forward]
  `(menu-item "Repeat Forward String..." 
	      aquamacs-repeat-isearch
	      :key-sequence [(,osxkeys-command-key g)]
	      :help "Search forward for a string as you type it"))

(define-key menu-bar-i-search-menu [isearch-backward]
  `(menu-item "Repeat Backward String..."
	      aquamacs-repeat-isearch-backward
	      :help "Search backwards for a string as you type it"))

(define-key menu-bar-i-search-menu [isearch-use-region]
  `(menu-item "Use Region For Search" 
	      aquamacs-use-selection-for-find
	      :enable mark-active
	      :help "Use the selection for your next search"))

 
;; Command line tool

(defun aquamacs-install-command-line-tool ()
  (interactive)
  (call-process "open" nil 0 nil "-a" "Installer" 
		(format "%s/Contents/Resources/Aquamacs Command Line Tool.mpkg" 
			aquamacs-mac-application-bundle-directory))
  (message "Call command-line-tool from a shell as `aquamacs', e.g., as \"aquamacs file.txt\""))


  
(define-key-after menu-bar-tools-menu [menu-tools-command-line-tool]
  `(menu-item "Install Command Line Tools" 
	      aquamacs-install-command-line-tool
	      :help "Install Command Line Tools...")
  'simple-calculator)

(define-key-after menu-bar-tools-menu [menu-tools-command-line-tool-sep]
  '(menu-item "--" nil)
  'simple-calculator)


(require 'aquamacs-editing)
(define-key menu-bar-edit-menu [fill]
`(menu-item "Wrap and Re-Format (fill)" 
	    fill-paragraph-or-region
	    :key-sequence [(meta q)]
	    :enable (not buffer-read-only)
	    :help
	    "Fill text in region (or paragraph) to fit between
left and right margin"))

(define-key-after menu-bar-edit-menu [unfill]
`(menu-item "Remove Hard Line Breaks (unfill)" 
	    unfill-paragraph-or-region
	    :key-sequence [(meta Q)]
	    :enable (not buffer-read-only)
	    :help
	    "Remove line-breaks from paragraph or region.")
'fill)
(define-key-after menu-bar-edit-menu [separator-fill]
  '(menu-item "--")
  'unfill)
  

 ;; leave in - NS port crashes often
 ;; (define-key-after menu-bar-file-menu [my-file-separator]
 ;;          '(menu-item "--") 'recover-session)
 (define-key-after menu-bar-file-menu [mac-show-in-finder]
          '(menu-item "Reveal in Finder" mac-key-show-in-finder

		      :enable (and  (menu-bar-menu-frame-live-and-visible-p)
				    (menu-bar-non-minibuffer-window-p)
				    buffer-file-number)
		      ) 'my-file-separator)
(define-key-after menu-bar-file-menu [reveal-in-finder-fill]
  '(menu-item "--") 'mac-show-in-finder)
  
;; save as (redefinition for :enable)

(define-key menu-bar-file-menu [save-buffer ]
  `(menu-item "Save Buffer"
	      mac-key-save-file
	      :key-sequence [(,osxkeys-command-key s)]
	      :enable (and (buffer-modified-p)
			   (buffer-file-name)
			   (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Save current buffer to its file"
	      ))  

(define-key menu-bar-file-menu [write-file]
  `(menu-item "Save Buffer As..."
	      mac-key-save-file-as
	      :key-sequence [(,osxkeys-command-key S)]
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Write current buffer to another file"))


;; ;; Export file functions
;; (require 'mac-print)

;; (defvar menu-bar-export-file-menu (make-sparse-keymap "Export ..."))

;; ;; (setq menu-bar-export-file-menu (make-sparse-keymap "New Buffer"))

;; (define-key menu-bar-export-file-menu [export-pdf]
;;   '(menu-item "PDF..." export-to-pdf
;; 	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
;; 			   (menu-bar-non-minibuffer-window-p))
;; 	      :help "Write current buffer to another file in PDF format"))

;; (define-key menu-bar-export-file-menu [export-html]
;;   '(menu-item "HTML..." export-to-html
;; 	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
;; 			   (menu-bar-non-minibuffer-window-p))
;; 	      :help "Write current buffer to another file in HTML format"))

;; (define-key-after menu-bar-file-menu [export-file-menu]
;;   `(menu-item (concat "Export " (if mark-active "Region" "Buffer")) 
;; 	      ,menu-bar-export-file-menu
;; 	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
;; 			   (menu-bar-non-minibuffer-window-p))
;; 	      :help "Export buffer in a different format")
;;     'write-file)

 
;; Support for monochrome printing
;; Added by Norbert Zeh <nzeh@cs.dal.ca> 2007-09-23

;; (when (boundp 'mac-print-monochrome-mode)
;;   (defun menu-bar-toggle-mac-print-monochrome-mode ()
;;     (interactive)
;;     (customize-set-variable 'mac-print-monochrome-mode
;; 			    (not mac-print-monochrome-mode))
;;     (message "Color printing %s"
;; 	     (if mac-print-monochrome-mode
;; 		 "disabled" "enabled")))

;;   (define-key-after menu-bar-file-menu [toggle-mac-print-monochrome-mode]
;;     '(menu-item "Color Printing"
;; 		menu-bar-toggle-mac-print-monochrome-mode
;; 		:help "Toggles color printing"
;; 		:button (:toggle . (not mac-print-monochrome-mode)))))

(autoload 'aquamacs-copy-as-html "mac-print" "Copies the region in HTML format into the clipboard." 'interactive nil)
(autoload 'aquamacs-print "mac-print" "Prints the current buffer or, if the mark is active, the current region." 'interactive nil)
(autoload 'aquamacs-page-setup "mac-print" "Show the page setup dialog." 'interactive nil)

(define-key-after menu-bar-file-menu [aquamacs-page-setup]
  `(menu-item "Page Setup..." 
	      aquamacs-page-setup
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Set the page layout."))

(define-key-after menu-bar-file-menu [aquamacs-print]
  `(menu-item (format "Print %s..." 
		      (if mark-active "Region" "Buffer"))
	      aquamacs-print
	      :key-sequence [(,osxkeys-command-key p)]
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Print current buffer or region"))



;; Printing (redefinition for :enable)

(defun menu-bar-print-region-or-buffer ()
  "Prints the current buffer or the region, if mark is active."
  (interactive)
  (if mark-active
      (print-region (region-beginning) (region-end))
    (print-buffer)))

  

;; (define-key-after menu-bar-file-menu [print-region-or-buffer]
;;   `(menu-item ,(format "Quick Print %s" 
;; 		      (if mark-active "Region" "Buffer"))  
;; 	      menu-bar-print-region-or-buffer
;; 	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
;; 			   (menu-bar-non-minibuffer-window-p))
;; 	      :help "Print buffer, or region if active"))



;(require 'longlines) 
 
(global-set-key "\C-xw" 'toggle-word-wrap)
  
(require 'aquamacs-editing)
(custom-add-option 'text-mode-hook 'auto-detect-wrap)
(defun toggle-text-mode-auto-detect-wrap ()
  "Toggle automatic word wrapping in Text and related modes.
This command affects all buffers that use modes related to Text
mode, both existing buffers and buffers that you subsequently
create.  Upon entering text-mode, the function `auto-detect-wrap'
is used to determine wrapping with either `visual-line-mode'
or with `auto-fill-mode', which see."
  (interactive)
  ;; remove leftover customizations from previous versions
  (remove-hook 'text-mode-hook 'turn-on-auto-fill)
  (remove-hook 'text-mode-hook 'turn-on-longlines)
  (remove-hook 'text-mode-hook 'turn-on-word-wrap)
  (let ((enable-mode (not (or (memq 'auto-detect-wrap text-mode-hook)
			      (memq 'auto-detect-longlines text-mode-hook)))))
    (if enable-mode
	(add-hook 'text-mode-hook 'auto-detect-wrap)
      (remove-hook 'text-mode-hook 'auto-detect-wrap)
      (remove-hook 'text-mode-hook 'auto-detect-longlines)); longlines was used up to version 1.4
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(if (or (derived-mode-p 'text-mode) text-mode-variant)
	    (auto-detect-wrap))))
    (customize-mark-as-set 'text-mode-hook)
    (message "Auto-Detect Line Wrap %s in Text modes"
	     (if enable-mode "enabled" "disabled"))))
  
(defun aquamacs-set-line-wrapping-in-text-modes ()
  "Sets line wrapping in all text modes.
Line wrapping is determined according to whether text-mode-hook
contains `turn-on-auto-fill', `turn-on-word-wrap' or `auto-detect-wrap'."
  (interactive)

  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (or (derived-mode-p 'text-mode) text-mode-variant)
					; check buffer-local values
	  (let ((function (if (memq 'turn-on-auto-fill text-mode-hook)  'turn-on-auto-fill
			    (if (memq 'turn-on-word-wrap text-mode-hook)  'turn-on-word-wrap
			      (if (memq 'auto-detect-wrap text-mode-hook)  'auto-detect-wrap
				'ignore  ;; FIXME: turn off everything
				)))))
	    (funcall function))))))


;; (defun aquamacs-menu-bar-toggle-text-mode-word-wrap ()
;;   "Toggle whether to use `visual-line-mode' in Text mode and related modes.
;; Runs `aquamacs-set-line-wrapping-in-text-modes'."
;;   (interactive)
;;   (if (memq 'auto-detect-wrap text-mode-hook)
;;       ;; we're just setting the default there.
;;       (progn (setq auto-word-wrap-default-function 'turn-on-word-wrap)
;; 	     (message "Line Wrap is auto-detected in Text modes and defaults to Soft Word Wrap (Visual Line Mode)."))
;;     (let ((enable-mode (not (memq 'turn-on-word-wrap text-mode-hook))))
;;       (if enable-mode
;; 	  (progn (add-hook 'text-mode-hook 'turn-on-word-wrap)
;; 		 (remove-hook 'text-mode-hook 'turn-on-auto-fill)
;; 		 (setq auto-word-wrap-default-function 'turn-on-word-wrap))
;; 	(remove-hook 'text-mode-hook 'turn-on-word-wrap)
;; 	(setq auto-word-wrap-default-function nil))
;;       (customize-mark-as-set 'text-mode-hook)
;;       (message "Soft Word Wrap %s in Text modes"
;; 	       (if enable-mode "enabled" "disabled")))
;;     (customize-mark-as-set 'auto-word-wrap-default-function)
;;     (aquamacs-set-line-wrapping-in-text-modes)))

;; (defun aquamacs-menu-bar-toggle-text-mode-auto-fill ()
;;   "Toggle whether to use Auto Fill in Text mode and related modes.
;; Runs `aquamacs-set-line-wrapping-in-text-modes'."
;;   (interactive)

;;   (if (memq 'auto-detect-wrap text-mode-hook)
;;       ;; we're just setting the default there.
;;       (progn (setq auto-word-wrap-default-function 'turn-on-auto-fill)
;; 	     (message "Line Wrap is auto-detected in Text modes and defaults to Hard Word Wrap (Auto Fill)."))
;;     (let ((enable-mode (not (memq 'turn-on-auto-fill text-mode-hook))))
;;       (if enable-mode
;; 	  (progn (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; 		 (remove-hook 'text-mode-hook 'turn-on-word-wrap)
;; 		 (setq auto-word-wrap-default-function 'turn-on-auto-fill))
;; 	(remove-hook 'text-mode-hook 'turn-on-auto-fill)
;; 	(setq auto-word-wrap-default-function nil))
;;       (customize-mark-as-set 'text-mode-hook)
;;       (message "Hard Word Wrap (Auto Fill) %s in Text modes"
;; 	       (if enable-mode "enabled" "disabled")))
;;     (customize-mark-as-set 'auto-word-wrap-default-function)
;;     (aquamacs-set-line-wrapping-in-text-modes)))

(define-key menu-bar-options-menu [highlight-separator] nil)

(define-key-after menu-bar-line-wrapping-menu [auto-wrap]
  '(menu-item "Detect Line Wrap in Text Files"
	      toggle-text-mode-auto-detect-wrap
	      :help "Automatically use hard or soft word wrap (Auto Fill / Longlines) in text modes."
	      :button (:toggle . (if (listp text-mode-hook)
				     (or (member 'auto-detect-wrap text-mode-hook)
					 (member 'auto-detect-longlines text-mode-hook))
				   (or (eq 'auto-detect-wrap text-mode-hook)
				       (eq 'auto-detect-longlines text-mode-hook)))))
  'wrapping-set-default)
 
;; giving these options would be too much configuration for most users
;; (define-key-after menu-bar-line-wrapping-menu [word-wrap-text-mode]
;;   `(menu-item ,(purecopy "Soft Word Wrap as Default in Text Modes")
;;               aquamacs-menu-bar-toggle-text-mode-word-wrap
;; 	      :help ,(purecopy "Automatically soft wrap text while typing (Visual Line mode)")
;;               :button (:toggle . (if (if (listp text-mode-hook)
;; 					 (member 'auto-detect-wrap text-mode-hook)
;; 				       (eq 'auto-detect-wrap text-mode-hook))
;; 				     (eq auto-word-wrap-default-function 'turn-on-word-wrap)
;; 				   (if (listp text-mode-hook)
;; 				       (member 'turn-on-word-wrap text-mode-hook)
;; 				     (eq 'turn-on-word-wrap text-mode-hook)))))
;;   'auto-wrap)

;; (define-key-after menu-bar-line-wrapping-menu [auto-fill-text-mode]
;;   `(menu-item ,(purecopy "Hard Word Wrap as Default in Text Modes")
;;               aquamacs-menu-bar-toggle-text-mode-auto-fill
;; 	      :help ,(purecopy "Automatically fill text while typing (Auto Fill mode)")
;;               :button (:toggle . (if (if (listp text-mode-hook)
;; 					 (member 'auto-detect-wrap text-mode-hook)
;; 				       (eq 'auto-detect-wrap text-mode-hook))
;; 				     (eq auto-word-wrap-default-function 'turn-on-auto-fill)
;; 				   (if (listp text-mode-hook)
;; 				       (member 'turn-on-auto-fill text-mode-hook)
;; 				     (eq 'turn-on-auto-fill text-mode-hook)))))
;;   'word-wrap-text-mode)


;; (define-key-after menu-bar-options-menu [global-smart-spacing]
;;   (menu-bar-make-mm-toggle
;;    global-smart-spacing-mode
;;    "Smart Word Spacing"
;;    "Normalize spaces between words during cut&paste"
;;    (:enable (menu-bar-menu-frame-live-and-visible-p)))
;;   'truncate-lines)

(define-key-after menu-bar-options-menu [smart-spacing]
  '(menu-item "Smart Word Spacing in Text Modes"
	      menu-bar-text-mode-smart-spacing
	      :help "Normalize spaces between words during cut&paste"
	      :button (:toggle . (if (listp text-mode-hook)
				     (member 'smart-spacing-mode text-mode-hook)
				   (eq 'smart-spacing-mode text-mode-hook))))
	      'line-wrapping)

(defun menu-bar-text-mode-smart-spacing ()
  "Toggle `smart-spacing-mode' in `text-mode-hook'"
  (interactive)
  (toggle-text-mode-smart-spacing)
  (customize-mark-as-set 'text-mode-hook)
  (message "Smart word spacing in text modes %sabled.  Use M-x global-smart-spacing-mode to toggle for all modes."
	   (if (memq 'smart-spacing-mode text-mode-hook) "en" "dis")))


;; (define-key-after menu-bar-options-menu [text-mode-smart-spacing]
;;   '(menu-item "Smart Word Spacing in Text Modes"
;; 	      toggle-text-mode-smart-spacing
;; 	      :help "Normalize spaces between words during cut&paste"
;; 	      :button (:toggle . (memq 'smart-spacing-mode text-mode-hook))
;; 	      :enable (menu-bar-menu-frame-live-and-visible-p)) 'truncate-lines)


;; in edit menu

(when (< emacs-major-version 23)
  (define-key menu-bar-search-menu [case-fold-search]
    (menu-bar-make-toggle toggle-case-fold-search case-fold-search
			  "Case-Insensitive Search"
			  "Case-Insensitive Search %s"
			  "Ignore letter-case in search")))

(when initial-window-system
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
    ;; (define-key-after menu-bar-options-menu [obof-separator] '(menu-item "--") 'oneonone)
    )

(define-key menu-bar-options-menu [showhide]
  (list 'menu-item "View" menu-bar-showhide-menu))

(define-key menu-bar-showhide-menu [mac-font-panel-mode] nil)

(when (< emacs-major-version 23)
  (define-key menu-bar-options-menu [highlight-paren-mode] nil)
  (define-key menu-bar-options-menu [highlight-separator] nil)
  (define-key-after menu-bar-showhide-menu [highlight-separator] '("--"))
  (define-key-after menu-bar-showhide-menu [highlight-paren-mode]
    (menu-bar-make-mm-toggle show-paren-mode
			     "Paren Match Highlighting"
			     "Highlight matching/mismatched parentheses at cursor (Show Paren mode)")))
(define-key-after menu-bar-showhide-menu [hl-line-mode]
  (menu-bar-make-mm-toggle global-hl-line-mode
			   "Line Highlighting"
			   "Highlight current line (hl-line-mode)"))

;; do this here as well to make sure it follows highlight-paren-mode
(if (fboundp 'global-show-newlines-mode)
    (define-key-after menu-bar-showhide-menu [show-newlines-mode]
      (menu-bar-make-mm-toggle global-show-newlines-mode
			       "Show Newlines"
			       "Show hard newlines") 'highlight-paren-mode))


(when (< emacs-major-version 23)
  (define-key menu-bar-options-menu [blink-cursor-mode] nil)
  (define-key menu-bar-options-menu [cursor-separator] nil))


;; Small Fringe

(defun aquamacs-menu-bar-showhide-fringe-menu-customize-small ()
  "Display small fringes only on the left of each window."
  (interactive)
  (require 'fringe) 
  (fringe-mode (cons 4 0)))

(defun aquamacs-menu-bar-showhide-fringe-menu-customize-tiny ()
  "Display small fringes only on the left of each window."
  (interactive)
  (require 'fringe) 
  (fringe-mode (cons 1 1)))

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
  `(menu-item "Small Left" 
	      aquamacs-menu-bar-showhide-fringe-menu-customize-small
	      :help "Narrow fringe, left only"
	      :visible ,(display-graphic-p)
	      :button (:radio . (equal fringe-mode '(4 . 0)))) 'none)

(define-key-after menu-bar-showhide-fringe-menu [tiny]
  `(menu-item "Tiny" 
	      aquamacs-menu-bar-showhide-fringe-menu-customize-tiny
	      :help "Tiny fringes, left and right"
	      :visible ,(display-graphic-p)
	      :button (:radio . (equal fringe-mode '(1 . 1)))) 'none)

(define-key menu-bar-showhide-fringe-menu [default]
  '(menu-item "Left and Right" menu-bar-showhide-fringe-menu-customize-reset
	      :help "Default width fringe on both left and right side"
	      :visible (display-graphic-p)
	      :button (:radio . (eq fringe-mode nil))))

  
(define-key-after menu-bar-options-menu [file-backups]
  (menu-bar-make-toggle toggle-make-backup-files make-backup-files  
			(format "Make Backup Files %s" (if backup-inhibited "(inhibited here)" ""))
			"Making backup files: %s"
			"Create a backup file when saving") 'mule)

;; not important enough to warrant a menu entry
(when (< emacs-major-version 23)
  (define-key menu-bar-options-menu [save-place]
    nil))
 

;; remove this entry, because in Aquamacs, no global tool-bar-mode
;; is present
(define-key menu-bar-showhide-menu [showhide-tool-bar]
    nil)


(define-key menu-bar-custom-menu [customize]
  `(menu-item "Top-level Customization Group" 
	      customize
	      :help "The master group called `Emacs'"))

(defun customize-aquamacs ()
  "Customize settings that differ between Aquamacs and Emacs."
  (interactive)
  (customize-group 'Aquamacs-is-more-than-Emacs))

(define-key-after menu-bar-custom-menu [customize-aquamacs]
  '(menu-item "Aquamacs-Specific Options" customize-aquamacs
	      :help "Settings that differ between Aquamacs and Emacs")
  'customize-changed-options)
(define-key menu-bar-options-menu [customize]
  (list 'menu-item "Customize Aquamacs" menu-bar-custom-menu))


;; move this down after "customize"

(define-key-after menu-bar-options-menu [save-custom-separator]
  '("--") 'customize)

(define-key-after menu-bar-options-menu [save]
  '(menu-item "Save Options" aquamacs-menu-bar-options-save
	      :help "Save options set from the menu above")
  'save-custom-separator)

;; Goto Line

(define-key menu-bar-goto-menu [go-to-line]
  `(menu-item "Goto Line..."
	      goto-line
	      :key-sequence [(,osxkeys-command-key l)]
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Read a line number and go to that line"))



;; Spell Checking

;; (defun aquamacs-ispell-install ()
;;   (interactive)
;;   (browse-url "http://aquamacs.org/spellchecking"))
 

(define-key menu-bar-tools-menu [spell] nil)
(define-key menu-bar-tools-menu [separator-spell] nil)
(define-key-after menu-bar-edit-menu [separator-spell]
  '(menu-item "--" nil
	      ;; :visible (and (boundp 'ispell-program-name) ispell-program-name)
	      )
  'separator-bookmark)

;; ispell is not loaded at startup
;; spell checking 
;; potential for trouble here:
;; if exec path isn't initialized (because PATH not read yet)
;; then locate-file might fail and we don't know if we're going to have ispell

;; (defun aquamacs-initialize-ispell-program-name ()
;;   "Set `ispell-program-name'."
;;   (protect
;;    (let ((ipn (or (if (locate-file "aspell" exec-path exec-suffixes 'file-executable-p) 
;; 		      "aspell")
;; 		  (if (locate-file "ispell" exec-path exec-suffixes 'file-executable-p)
;; 		      "ispell"))))
;;      (if ipn ;; do not initialize if not (yet) found
;; 	 (defvar ispell-program-name ipn)))))

;; (aquamacs-initialize-ispell-program-name)
;; (unless (boundp 'ispell-program-name)
;;   (add-hook 'after-init-hook 'aquamacs-initialize-ispell-program-name 'append))

(define-key-after menu-bar-edit-menu [spell]
  '(menu-item "Spelling" ispell-menu-map 
	      ;; :visible (and (boundp 'ispell-program-name) ispell-program-name)
	      )
  'separator-bookmark)
;; (define-key-after menu-bar-edit-menu [spell-download-aspell]
;; 	'(menu-item "Download Spell-Checking..." aquamacs-ispell-install
;; 		    :help "Download spell-checking package"
;; 		    :visible (and (boundp 'ispell-program-name)
;; 				  (not ispell-program-name)))
;; 	'spell)

;; (define-key ispell-menu-map [ispell-buffer]
	;; `(menu-item "Spell-Check Buffer" 
		    ;; ns-start-spellchecker
		    ;; :help "Check spelling of selected buffer"))
;; (define-key ispell-menu-map [ispell-complete-word]
;; 	`(menu-item "Complete Word" 
;; 		    ispell-complete-word
;; 		    :help "Complete word at cursor using dictionary"))
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

;; the text properties menu appears dysfunctional, to the naive user
;; these properties aren't commonly saved to file
;; things like "flush right" don't work properly (variable-width fonts)
;; indentation actually modifies the buffer text as it seems
;; display faces leads to face customization rather than face setting
;; as would be expected in the Edit menu
(define-key menu-bar-edit-menu [props] nil)
(define-key menu-bar-edit-menu [separator-fill] nil)

;; Check for updates

;; menu item (Aquamacs menu)
;; needs about-emacs.patch
(when (and (boundp 'mac-apple-event-map) mac-apple-event-map)
    (put 'check-for-updates 'mac-apple-event-id "chku")
    (define-key mac-apple-event-map [hi-command check-for-updates]
      'aquamacs-check-for-updates))



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
  `(menu-item "Aquamacs Help" 
	      aquamacs-user-help
	      :help "Show Aquamacs Manual in Apple Help"))
  
(define-key-after menu-bar-help-menu [menu-aquamacs-user-wiki]
  `(menu-item "Aquamacs Tips Wiki Online" 
	      aquamacs-user-wiki
	      :help "Show Wiki (online)")
  'menu-aquamacs-help)


(define-key-after menu-bar-help-menu [menu-aquamacs-homepage]
  `(menu-item "Aquamacs Homepage" 
	      aquamacs-homepage
	      :help "Show Aquamacs Homepage")
  'menu-aquamacs-user-wiki)

(define-key-after menu-bar-help-menu [menu-aquamacs-emacs-manual]
  `(menu-item "Emacs Manual" 
	      aquamacs-emacs-manual
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



(defvar menu-bar-zoom-menu (make-sparse-keymap "Zoom"))

(define-key menu-bar-zoom-menu [zoom-out]
  `(menu-item "Shrink" zoom-font-out
	      :key-sequence [(,osxkeys-command-key -)]
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Zoom font out"))

(define-key menu-bar-zoom-menu [zoom-in]
  `(menu-item "Enlarge" zoom-font
	      :key-sequence [(,osxkeys-command-key +)]
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Zoom font in"))


(defvar menu-bar-window-extras-menu (make-sparse-keymap "Extras"))

(define-key menu-bar-window-extras-menu [zoom-menu]
    `(menu-item "Zoom" ,menu-bar-zoom-menu))

(define-key menu-bar-window-extras-menu [one-window]
  `(menu-item "Join Windows"
	      aquamacs-join-windows
	      :key-sequence [(,osxkeys-command-key 1)]
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p)
			   (not (one-window-p t 'visible)))
	      :help "Selected window grows to fill the whole frame"))
(define-key menu-bar-window-extras-menu [split-window]
  `(menu-item "Split Window" 
	      aquamacs-split-window-vertically
	      :key-sequence [(,osxkeys-command-key 2)]
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :help "Split selected window in two"))
(define-key menu-bar-window-extras-menu [make-frame]
  `(menu-item "Show Buffer in New Frame " make-frame-command
	      :visible (fboundp 'make-frame-command)
	      ;; no key: use standard notation
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Open a new frame"))
(define-key menu-bar-window-extras-menu [tile-frames]
  `(menu-item "Scatter frames" scatter-frames
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Place frames sensibly"))
(define-key menu-bar-window-extras-menu [tile-frames-v]
  `(menu-item "Tile frames vertically" tile-frames-vertically
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Tile frames vertically"))
(define-key menu-bar-window-extras-menu [tile-frames-h]
  `(menu-item "Tile frames horizontally" tile-frames-horizontally
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Tile frames horizontally"))
(define-key menu-bar-window-extras-menu [full-frame]
  `(menu-item "Full Screen Editing" 
	      aquamacs-toggle-full-frame
	      :key-sequence [(,osxkeys-command-key shift 13)]
	      :button (:toggle . (eq (frame-parameter nil 'fullscreen) 'fullboth))
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Use full screen for the selected frame"))
  
(setq  menu-bar-buffers-menu-command-entries
       (append 
	       (list 
		'(command-separator2 "--")
		(assq 'make-frame menu-bar-window-extras-menu)
		(assq 'zoom-menu menu-bar-window-extras-menu)
		(assq 'full-frame menu-bar-window-extras-menu)
		(assq 'one-window menu-bar-window-extras-menu)
		(assq 'split-window menu-bar-window-extras-menu)
		'(command-separator3 "--")
		(assq 'place-frame menu-bar-window-extras-menu)
		(assq 'tile-frames menu-bar-window-extras-menu)
		(assq 'tile-frames-h menu-bar-window-extras-menu)
		(assq 'tile-frames-v menu-bar-window-extras-menu)
		'(command-separator4 "--")
		(list 'next-buffer
		      'menu-item
		      '(format "Select Next %s"  
			       (if tabbar-mode "Tab" "Buffer"))
		      'next-tab-or-buffer 
		      :key-sequence `[(,osxkeys-command-key })]
		      :enable '(and (menu-bar-menu-frame-live-and-visible-p)
				    (menu-bar-non-minibuffer-window-p)
				    (or (not tabbar-mode)
					(not (tabbar-tabset-only-tab
					      (tabbar-selected-tab
					       (tabbar-current-tabset t))))))
		      :help "Switch to the \"next\" buffer in a cyclic order")
		(list 'previous-buffer
		      'menu-item
		      '(format "Select Previous %s" 
			       (if tabbar-mode "Tab" "Buffer"))
		      'previous-tab-or-buffer
		      :key-sequence `[(,osxkeys-command-key {)]
		      :enable '(and (menu-bar-menu-frame-live-and-visible-p)
				    (menu-bar-non-minibuffer-window-p)
				    (or (not tabbar-mode)
					(not (tabbar-tabset-only-tab
					 (tabbar-selected-tab
					  (tabbar-current-tabset t))))))
		      :help "Switch to the \"previous\" buffer in a cyclic order")
		(list 'movetab
		      'menu-item 
		      "Move Tab to New Frame" 
		      'tabbar-move-current-buffer-to-new-frame
		      :enable '(and (menu-bar-menu-frame-live-and-visible-p)
				    (menu-bar-non-minibuffer-window-p)
				    (or (not tabbar-mode)
					(not (tabbar-tabset-only-tab
					      (tabbar-selected-tab
					       (tabbar-current-tabset t))))))
		      :help "Move the current Tab to New Frame")
		(list 'removetab
		      'menu-item 
		      '(format "Hide Tab" ) 
		      'tabbar-delete-current-tab
		      :enable '(and (menu-bar-menu-frame-live-and-visible-p)
				    (menu-bar-non-minibuffer-window-p)
				    (or (not tabbar-mode)
					(not (tabbar-tabset-only-tab
					      (tabbar-selected-tab
					       (tabbar-current-tabset t))))))
		      :help "Remove the current Window without killing the buffer.")
		(list 'mergetabs
		      'menu-item
		      "Merge All Frames" 
		      'tabbar-window-merge-windows
		      :enable '(and (menu-bar-menu-frame-live-and-visible-p)
				   (menu-bar-non-minibuffer-window-p))
		      :help "Merge all Frames into a single one with tabs")
		)))


;(assq-delete-all 'select-named-buffer menu-bar-buffers-menu-command-entries)
;(assq-delete-all 'list-all-buffers menu-bar-buffers-menu-command-entries)

(add-hook 'menu-bar-update-hook 'aquamacs-update-menu)
(add-hook 'after-change-major-mode-hook 'aquamacs-record-mode-change)
(add-hook 'after-init-hook 'aquamacs-update-new-file-menu)

(run-hooks 'aquamacs-menu-setup-hook)
)

;;; ONE TIME SETUP

; 
;; we will set the following ones directly
;; customization is always possible
;; the existing menu item is badly worded and the C-c/v/x don't apply anyways
;; done

;; Quit entry shouldnt be there
(easy-menu-remove-item global-map  '("menu-bar" "file") 'separator-exit)
(easy-menu-remove-item global-map  '("menu-bar" "file") 'exit-emacs)

;; this is to set the action for the "Quit" function (Emacs menu)
(global-set-key [mac-application-quit] 'save-buffers-kill-emacs)
 
    
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
       nil (list (cons 'fullscreen nil)))
    ;; save small frame position
    (smart-move-frame-inside-screen)
    (modify-frame-parameters 
     nil (list (cons 'fullscreen 'fullboth))))
  
  (if (frame-parameter nil 'fullscreen) ; (frame-full-screen-p)
      (message (substitute-command-keys 
		"Press \\[aquamacs-toggle-full-frame] to exit full screen editing.")))
  nil)


;; will be moved to Buffers menu later on 
;; but is created here

;; --done
;; (tabbar-window-buffer-list)
;; move stuff from File to the Buffers menu
 
 
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

;; to do after loading this file (at runtime)
;; (menu-bar-update-buffers) ;; update Buffers menu now
;; (aquamacs-update-menu t) ;; initial setup of the menu
 
 (defun aquamacs-user-wiki ()
  (interactive)
  (browse-url "http://aquamacs.org/wiki/"))
 
 (defun aquamacs-homepage ()
  (interactive)
  (browse-url "http://aquamacs.org/"))

(defun emacsosx-mailing-list-subscribe ()
  (interactive)
  (browse-url "http://aquamacs.org/subscribe-to-support-mailing-list"))
 
(defun aquamacs-donate ()
  (interactive)
  (browse-url "http://aquamacs.org/donations.shtml"))
 
(defun emacs-user-wiki ()
  (interactive)
  (browse-url "http://www.emacswiki.org/")) 

;; insert menu showing reference cards

(defun aquamacs--refcard-source-update ()
  "Insert code for current reference card menu."
  (interactive)
  (let ((cards))
       (with-temp-buffer
	 (cd (format "%setc/refcards" (mac-resources-path)))
	 (shell-command "grep '^\\s*\\\\title{\\(.*\\)}' *.tex" t)
	 
	 (beginning-of-buffer)
	 (while
	     (search-forward-regexp "^\\(.*?\\)\\.tex\\:\\\\title{\\(.*\\)}" nil 'noerr)
	   (if (file-readable-p (concat (match-string 1) ".pdf"))
	       (add-to-list 'cards (cons (match-string 1) (match-string 2))))
	   ))

       (mapc (lambda (c)

	       (insert (format 
"(define-key menu-bar-help-refcards-menu [%s]
             (list 'menu-item \"%s\"
                (defun show-refcard-%s () 
                  (interactive)
                  (show-refcard \"%s.pdf\"))))
"
	     (car c) (cdr c) (car c)  (car c) )))
	       (sort cards (lambda (a b) (string-lessp (cdr a) (cdr b)))))))

(defun show-refcard (pdf)
  (interactive)
  (call-process "open" nil 0 nil (format "%setc/refcards/%s" (mac-resources-path) pdf)))

(setq menu-bar-help-refcards-menu (make-sparse-keymap "Refcards"))
 
(define-key menu-bar-help-refcards-menu [fr-dired-ref]
             (list 'menu-item "Carte de r\'ef\'erence de Dired"
                (defun show-refcard-fr-dired-ref () 
                  (interactive)
                  (show-refcard "fr-dired-ref.pdf"))))
(define-key menu-bar-help-refcards-menu [fr-refcard]
             (list 'menu-item "Carte de r\'ef\'erence de GNU Emacs"
                (defun show-refcard-fr-refcard () 
                  (interactive)
                  (show-refcard "fr-refcard.pdf"))))
(define-key menu-bar-help-refcards-menu [dired-ref]
             (list 'menu-item "Dired Reference Card"
                (defun show-refcard-dired-ref () 
                  (interactive)
                  (show-refcard "dired-ref.pdf"))))
(define-key menu-bar-help-refcards-menu [calccard]
             (list 'menu-item "GNU Calc Reference Card"
                (defun show-refcard-calccard () 
                  (interactive)
                  (show-refcard "calccard.pdf"))))
(define-key menu-bar-help-refcards-menu [sk-refcard]
             (list 'menu-item "GNU Emacs -- Referen्nे karta"
                (defun show-refcard-sk-refcard () 
                  (interactive)
                  (show-refcard "sk-refcard.pdf"))))
(define-key menu-bar-help-refcards-menu [cs-refcard]
             (list 'menu-item "GNU Emacs -- Referen्n���� karta"
                (defun show-refcard-cs-refcard () 
                  (interactive)
                  (show-refcard "cs-refcard.pdf"))))
(define-key menu-bar-help-refcards-menu [refcard]
             (list 'menu-item "GNU Emacs Reference Card"
                (defun show-refcard-refcard () 
                  (interactive)
                  (show-refcard "refcard.pdf"))))
(define-key menu-bar-help-refcards-menu [pt-br-refcard]
             (list 'menu-item "GNU Emacs: Cart\~ao de Refer\^encia"
                (defun show-refcard-pt-br-refcard () 
                  (interactive)
                  (show-refcard "pt-br-refcard.pdf"))))
(define-key menu-bar-help-refcards-menu [orgcard]
             (list 'menu-item "Org-Mode Reference Card (1/2)"
                (defun show-refcard-orgcard () 
                  (interactive)
                  (show-refcard "orgcard.pdf"))))
(define-key menu-bar-help-refcards-menu [orgcard]
             (list 'menu-item "Org-Mode Reference Card (2/2)"
                (defun show-refcard-orgcard () 
                  (interactive)
                  (show-refcard "orgcard.pdf"))))
(define-key menu-bar-help-refcards-menu [pl-refcard]
             (list 'menu-item "Przegl/ad polece/n GNU Emacsa"
                (defun show-refcard-pl-refcard () 
                  (interactive)
                  (show-refcard "pl-refcard.pdf"))))
(define-key menu-bar-help-refcards-menu [de-refcard]
             (list 'menu-item "Referenzkarte zu GNU Emacs"
                (defun show-refcard-de-refcard () 
                  (interactive)
                  (show-refcard "de-refcard.pdf"))))
(define-key menu-bar-help-refcards-menu [sk-dired-ref]
             (list 'menu-item "Referen्nे karta pre Dired"
                (defun show-refcard-sk-dired-ref () 
                  (interactive)
                  (show-refcard "sk-dired-ref.pdf"))))
(define-key menu-bar-help-refcards-menu [cs-dired-ref]
             (list 'menu-item "Referen्n���� karta pro Dired"
                (defun show-refcard-cs-dired-ref () 
                  (interactive)
                  (show-refcard "cs-dired-ref.pdf"))))

(define-key-after menu-bar-help-menu [menu-refcards]
  `(menu-item "Printable Reference Cards" 
	      ,menu-bar-help-refcards-menu)
  'more-manuals)


;; workarounds for current bugs

;; done
; can't get rid of the menu bar on a Mac
(easy-menu-remove-item global-map  
		       '("menu-bar" "options" "showhide") 'menu-bar-mode)

; can't show a frame on a different display
(easy-menu-remove-item global-map 
		       '("menu-bar" "file") 'make-frame-on-display)

(provide 'aquamacs-menu)
  
