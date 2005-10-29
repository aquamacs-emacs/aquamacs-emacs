;; aquamacs-menu
;; redefines and modifies the menu bar

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-menu.el,v 1.22 2005/10/29 16:11:24 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/


;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;; Copyright (C) 2005, David Reitter
 

(require 'easymenu)

;; this is a big hack like most other things
(defun change-menu-text (keymap key str)
 

  (if (eq 'string (type-of (car (cdr (assq key (lookup-key global-map keymap))))))
    
      (setcar (cdr (assq key (lookup-key global-map keymap)))
	      str
	      )
    (if (eq 'string (type-of 
		     (car (cdr (cdr 
				(assq key 
				      (lookup-key global-map keymap)))))))
	(setcar (cdr (cdr (assq key (lookup-key global-map keymap))))
	      str)
    (setcar (cdr (assq key (lookup-key global-map keymap)))
	    str
	    )))
  (define-key global-map (vconcat (append keymap (list key)))
    (cdr (assq key (lookup-key global-map keymap)))))

(defun aquamacs-updated-is-visible-frame-p ()
  (and (frame-live-p menu-updating-frame)
       (frame-visible-p menu-updating-frame )))


 
;; apple command character is unicode x2318  
;; 
(setq apple-char (string (decode-char 'ucs #X2318)))

;; The following is a big hack. The mac port can't currently cope 
;; with putting the command key combos in the menu, for various 
;; reasons (1. they are just secondary alternatives, 2. command is defined
;; as 'hyper' and only known as such)

; redefine New
; (define-key menu-bar-edit-menu [mark-whole-buffer] (cdr (assq 'mark-whole-buffer (lookup-key global-map [menu-bar edit]))))


;; New documents
(defun new-frame-with-new-scratch  (&optional other-frame mode)
  "Opens a new frame containing an empty buffer."
  (interactive)			
  (let ((buf (generate-new-buffer (mac-new-buffer-name "untitled"))))

    ;; setting mode is done before showing the new frame
    ;; because otherwise, we get a nasty animation effect
    (save-excursion
      (set-buffer buf)
      (if default-major-mode (funcall  (or mode default-major-mode))))

    (if other-frame
	(switch-to-buffer-other-frame buf)
      (let ((one-buffer-one-frame-force one-buffer-one-frame))
	;; force new frame
	(switch-to-buffer buf)))
  
    (setq buffer-offer-save t)
    (set-buffer-modified-p nil)))


(define-key menu-bar-file-menu [new-file]
  '(menu-item (format  "New                            %sN"  apple-char)  new-frame-with-new-scratch
	      :enable (or one-buffer-one-frame
			  (not (window-minibuffer-p
			    (frame-selected-window menu-updating-frame))))
	      :help "Create a new buffer"))
 

(defcustom aquamacs-menu-new-buffer-modes
  '(text-mode html-helper-mode latex-mode lisp-interaction-mode emacs-lisp-mode c-mode perl-mode python-mode applescript-mode R-mode sh-mode tcl-mode)
  "List of modes to include in the New Buffer menu."
  :group 'menu
  :group 'Aquamacs
  :type '(repeat (symbol :tag "Mode-name"))
)
 

(defvar menu-bar-new-file-menu nil)

(defun aquamacs-update-new-file-menu ()
  (setq menu-bar-new-file-menu (make-sparse-keymap "New Buffer")) 
  (mapc
   (lambda (modename)
     (when (fboundp modename)
       (define-key ;;-after doesn't work with after- why?>? 
	 menu-bar-new-file-menu 
	 (vector (make-symbol (concat "new-buffer-" (symbol-name modename))))
	 `(menu-item  
	   ,(concat 
	     (capitalize 
	      (replace-regexp-in-string "-mode" "" (symbol-name modename)))
	     " Buffer")
	   ,(eval 
	     (list 'lambda '() '(interactive)
		   (list 'new-frame-with-new-scratch nil `(quote ,modename))
		   ))
	   :help "Create a new buffer in a specific mode."
	   ))))
      
   (reverse (sort (copy-list aquamacs-menu-new-buffer-modes)
		  (lambda (a b) (string< 
				 (upcase (symbol-name a)) 
				 (upcase (symbol-name b))))))
   )
  (define-key-after menu-bar-file-menu [new-file-menu]
    (list 'menu-item "New Buffer" menu-bar-new-file-menu
	  :help "Create a new buffer with a specific major mode.")
    'new-file)
  )

(add-hook 'after-init-hook 'aquamacs-update-new-file-menu)

(defun aquamacs-menu-make-new-buffer ()
  (interactive))
;; (apply (function defun) (make-symbol (concat "aquamacs-make-new-buffer-" (symbol-name modename))) '() '(interactive)
;; 		     (list 'new-frame-with-new-scratch nil `(quote ,modename))
;; 		     )

;(change-menu-text-2 [menu-bar application] 'quit (format  "Quit Emacs                %sQ"  apple-char))
(change-menu-text [menu-bar file] 'open-file (format  "Open File...                 %sO"  apple-char)) 

;; redefine this
(define-key menu-bar-file-menu [kill-buffer]
  '(menu-item (format "Close (current buffer)  %sW" apple-char) close-current-window-asktosave
	      :enable (aquamacs-updated-is-visible-frame-p)
	      :help "Discard current buffer"))
 
(change-menu-text [menu-bar file] 'exit-emacs (format  "Quit Emacs                %sQ"  apple-char))
;(change-menu-text [menu-bar application] 'quit (format  "Quit Emacs                %sQ"  apple-char))
(define-key menu-bar-edit-menu [copy]
  `(menu-item ,(format  "Copy                 %sC"  apple-char) 
	      clipboard-kill-ring-save
	      :enable mark-active
	      :help "Copy selected text in region"
	      :keys "\\[clipboard-kill-ring-save]"))
 
(change-menu-text [menu-bar edit] 'paste (format  "Paste                 %sV"  apple-char))
(change-menu-text [menu-bar edit] 'undo (format  "Undo                 %sZ"  apple-char))
(easy-menu-add-item  nil '("Edit")
  (vector (format "Redo                 %s-S-Z" apple-char) 'redo) 'cut)
(easy-menu-add-item  nil '("Edit")
  ["-" nil nil] 'cut)

(change-menu-text [menu-bar edit] 'cut (format  "Cut                    %sX"  apple-char))

;; still a problem with this
(change-menu-text [menu-bar edit] 'mark-whole-buffer (format  "Select All           %sA"  apple-char))
(change-menu-text [menu-bar edit search] 'search-forward (format  "Search forward              %sF"  apple-char))
(change-menu-text [menu-bar edit search] 'repeat-search-fwd (format  "Repeat forward              %s"  apple-char))

(change-menu-text [menu-bar edit] 'fill "Reformat (fill)") 

;; this needs an extension to show the keyboard shortcut 
;; interesting extensions to menu-item: (:visible nil), (:key-sequence)

 (define-key-after menu-bar-file-menu [my-file-separator]
          '(menu-item "--") 'recover-session)
 (define-key-after menu-bar-file-menu [mac-show-in-finder]
          '(menu-item "Show In Finder" mac-key-show-in-finder

		      :enable (and (aquamacs-updated-is-visible-frame-p)
				   buffer-file-number)
		      ) 'my-file-separator)
; 
;; we will set the following ones directly
;; customization is always possible
;; the existing menu item is badly worded and the C-c/v/x don't apply anyways
(easy-menu-remove-item global-map  '("menu-bar" "options") 'cua-mode) 
(easy-menu-remove-item global-map  '("menu-bar" "options") 'uniquify)
(easy-menu-remove-item global-map  '("menu-bar" "options") 'transient-mark-mode)
(easy-menu-remove-item global-map  '("menu-bar" "options") 'case-fold-search)


;; save as (redefinition for :enable)

(change-menu-text [menu-bar file] 'save-buffer (format  "Save Buffer                  %sS"  apple-char))  

(define-key menu-bar-file-menu [write-file]
  '(menu-item (format  "Save Buffer As...          %s-S-S"  apple-char) write-file

	      :enable (and (frame-live-p menu-updating-frame)
			   (frame-visible-p menu-updating-frame )
			   (not (window-minibuffer-p
				 (frame-selected-window menu-updating-frame))))
	      :help "Write current buffer to another file"))


;; Export file functions
(require 'mac-print)

(defvar menu-bar-export-file-menu (make-sparse-keymap "Export Buffer..."))

(setq menu-bar-export-file-menu (make-sparse-keymap "New Buffer"))

(define-key menu-bar-export-file-menu [export-pdf]
  '(menu-item "PDF..." export-to-pdf

	      :enable (and (frame-live-p menu-updating-frame)
			   (frame-visible-p menu-updating-frame )
			   (not (window-minibuffer-p
				 (frame-selected-window menu-updating-frame))))
	      :help "Write current buffer to another file in PDF format"))

(define-key menu-bar-export-file-menu [export-html]
  '(menu-item "HTML..." export-to-html

	      :enable (and (frame-live-p menu-updating-frame)
			   (frame-visible-p menu-updating-frame )
			   (not (window-minibuffer-p
				 (frame-selected-window menu-updating-frame))))
	      :help "Write current buffer to another file in HTML format"))

(define-key-after menu-bar-file-menu [export-file-menu]
    (list 'menu-item "Export Buffer" menu-bar-export-file-menu
	  :help "Export buffer in a different format")
    'write-file)







(define-key menu-bar-file-menu [split-window]
  '(menu-item "Split Window" split-window-vertically
	      :enable (and (frame-live-p menu-updating-frame)
			   (frame-visible-p menu-updating-frame )
			   (not (window-minibuffer-p
				 (frame-selected-window menu-updating-frame))))
	      :help "Split selected window in two"))




(define-key-after menu-bar-file-menu [aquamacs-print]
  '(menu-item "Preview and Print..." aquamacs-print
	      :enable (and (frame-live-p menu-updating-frame)
			   (frame-visible-p menu-updating-frame ))
	      :help "Print current buffer with page headings"))



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


(define-key-after menu-bar-file-menu [aquamacs-print]
  '(menu-item "Preview and Print..." aquamacs-print
	      :enable (and (frame-live-p menu-updating-frame)
			   (frame-visible-p menu-updating-frame ))
	      :help "Print current buffer with page headings"))


(define-key-after menu-bar-file-menu [print-region-or-buffer]
  '(menu-item "Quick Print Region/Buffer" menu-bar-print-region-or-buffer
	      :enable mark-active
	      :help "Print buffer, or region if active"))





(require 'longlines) 
 
;; goes in simple.el
(defun turn-on-longlines ()
  "Unconditionally turn on Longlines mode."
  (longlines-mode 1))

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




(if (string= "mac" window-system)
    (define-key-after menu-bar-options-menu [oneonone]
      (menu-bar-make-toggle 
       toggle-oneonone one-buffer-one-frame
       "Display Buffers in Separate Frames"
       "Display Buffers in Separate Frames: %s"
       "Open a new Frame (window) for each new buffer."
       (require 'aquamacs-frame-setup)
		 
       (setq one-buffer-one-frame
	     (not one-buffer-one-frame))
       ) 'edit-options-separator)
  )

(defvar mac-option-modifier-enabled-value 'meta)
(defun  toggle-mac-option-modifier (&optional interactively) 
  (interactive "p")
  (unless mac-option-modifier-enabled-value
    (setq mac-option-modifier-enabled-value 'meta))
   (setq mac-option-modifier
	 (if mac-option-modifier
	     (progn
	       (setq mac-option-modifier-enabled-value mac-option-modifier)
	       nil)
	   mac-option-modifier-enabled-value))
   (if interactively (customize-mark-as-set 'mac-option-modifier))
   (message 
    (format "Option key is %s%s" 
	    (if mac-option-modifier 
		""  "not ")
	    (upcase-initials 
	     (symbol-name (or mac-option-modifier 
			      mac-option-modifier-enabled-value))))))

(if (boundp 'mac-option-modifier) 
    (define-key-after menu-bar-options-menu [option-to-system]
      `(menu-item
	(format "Option Key for %s (not extra characters)  %s;" 
	       (upcase-initials (symbol-name 
				 (or mac-option-modifier 
				     mac-option-modifier-enabled-value)))
	       ,apple-char)
	toggle-mac-option-modifier 
	:visible (boundp 'mac-option-modifier)
	:help "Toggle whether to let Option key behave as Emacs key, 
do not let it produce special characters (passing the key to the system)."
	:button (:toggle . mac-option-modifier))
       'edit-options-separator))

 ;; this is a redefine
(define-key menu-bar-options-menu [mouse-set-font]
  '(menu-item "Set Font for this Frame..." mouse-set-font
	       :visible (display-multi-font-p)
	       :enable (aquamacs-updated-is-visible-frame-p) 
	       :help "Select a font from list of known fonts/fontsets"))



;; Quit entry shouldnt be there
(easy-menu-remove-item global-map  '("menu-bar" "file") 'separator-exit)
(easy-menu-remove-item global-map  '("menu-bar" "file") 'exit-emacs)

;; About entry is now in application menu
(easy-menu-remove-item global-map  '("menu-bar" "Help") 'about)

;; this is to set the action for the "Quit" function (Emacs menu)
(global-set-key [mac-application-quit] 'save-buffers-kill-emacs)
 


(easy-menu-add-item  nil '("Options")
  ["-" nil nil] 'mouse-set-font)
;(easy-menu-add-item  nil '("Options")
;  ["Set Color Theme..." aquamacs-color-theme-select t
;   :enable (aquamacs-updated-is-visible-frame-p) ] 'mouse-set-font)

(define-key-after menu-bar-options-menu [aquamacs-color-theme-select]
  '(menu-item "Set Color Theme for this Frame..." aquamacs-color-theme-select
	       :visible (and (display-multi-font-p)
			     (fboundp 'aquamacs-color-theme-select)
			     )
	       :enable (aquamacs-updated-is-visible-frame-p) 
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
  '(menu-item "Small left fringe" aquamacs-menu-bar-showhide-fringe-menu-customize-small
	      :help "Narrow fringe, left only"
	      :visible (display-graphic-p)
	      :button (:radio . (equal fringe-mode '(1 . 1)))) 'none)

 
;; local toolbars

(defun tool-bar-enabled-p (&optional frame)
"Evaluates to non-nil if the tool-bar is present
in frame FRAME. If FRAME is nil, the function applies
to the selected frame."
  (> (or (frame-parameter frame 'tool-bar-lines) 0) 0)) 

(defun toggle-tool-bar-here ()
(interactive)
  (modify-frame-parameters 
   nil 
   (list (cons 'tool-bar-lines 
	       (if (tool-bar-enabled-p)
		   0
		 1
		 )))))
;; (toggle-tool-bar-here)

(define-key-after menu-bar-showhide-menu [showhide-tool-bar-here]
    (list 'menu-item "Tool-bar in this frame" 'toggle-tool-bar-here
	:help ""
	:visible `(display-graphic-p)
	:button '(:toggle . (tool-bar-enabled-p)))
    'showhide-tool-bar)

;; Battery status is displayed in menu bar 
;; additional option for this is just confusing
(easy-menu-remove-item global-map  '("menu-bar" "options" "showhide") 'showhide-battery)


;; SENDMAIL doesn't usually work on OS X
;; unless postfix is set up
(easy-menu-remove-item global-map  '("menu-bar" "tools") 'compose-mail)


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

;; move this down after "customize"

(define-key-after menu-bar-options-menu [save-custom-separator]
  '("--") 'customize)

(define-key-after menu-bar-options-menu [save]
  '(menu-item "Save Options" menu-bar-options-save
	      :help "Save options set from the menu above")
  'save-custom-separator)




;; HELP MENU

; these problems here are for X-based systems etc. and not relevant
; for Aquamacs users
(easy-menu-remove-item global-map  '("menu-bar" "Help") 'emacs-problems)
 
     
 
 (defun aquamacs-user-wiki ()
  (interactive)
  (browse-url "http://aquamacs.org/wiki/")
) 
 (defun aquamacs-homepage ()
  (interactive)
  (browse-url "http://aquamacs.org/")
) 
(defun emacsosx-mailing-list-subscribe ()
  (interactive)
  (browse-url "mailto:macosx-emacs-on@email.esm.psu.edu?subject=subscribe%20macosx-emacs&body=Send%20off%20this%20e-mail%20to%20subscrube%20to%20the%20Emacs-on-MacOSX%20mailing%20list.")
) 
(defun aquamacs-donate ()
  (interactive)
  (browse-url "http://aquamacs.org/donations.shtml")
)
(easy-menu-add-item  nil '("Help")
  (vector (format "Aquamacs Help                    %s?"  apple-char) 'aquamacs-user-help) 'emacs-tutorial)

(easy-menu-add-item  nil '("Help")
  (vector  "Aquamacs Tips Wiki Online"  'aquamacs-user-wiki) 'emacs-tutorial)
 
 

(easy-menu-add-item  nil '("Help")
  (vector "Aquamacs Homepage" 'aquamacs-homepage) 'emacs-tutorial)   
 
(easy-menu-add-item  nil '("Help")
  (vector (format "Emacs Manual                   %s-S-?"  apple-char) 'aquamacs-emacs-manual) 'emacs-tutorial)
 

 (defun emacs-user-wiki ()
  (interactive)
  (browse-url "http://www.emacswiki.org/")
) 


(easy-menu-add-item  nil '("Help")
  (vector "Emacs Wiki Online" 'emacs-user-wiki) 'emacs-tutorial)
 
(easy-menu-add-item  nil '("Help")
  (vector  "Subscribe to mailing list..."  'emacsosx-mailing-list-subscribe) 'emacs-tutorial)
(easy-menu-add-item  nil '("Help")
  (vector  "Donate to Aquamacs..."  'aquamacs-donate) 'emacs-tutorial)
(easy-menu-add-item  nil '("Help")
  ["-" nil nil] 'emacs-tutorial)
  

;; workarounds for current bugs

; can't get rid of the menu bar on a Mac
(easy-menu-remove-item global-map  '("menu-bar" "options" "showhide") 'menu-bar-mode)

; can't show a frame on a different display
(easy-menu-remove-item global-map  '("menu-bar" "file") 'make-frame-on-display)

;; language environment
(when (eq system-type 'darwin) 
    (require 'aquamacs-mule)
(change-menu-text
 [menu-bar options]
 'mule
 "Aquamacs Multilingual Environment"
)
)

(provide 'aquamacs-menu)
 