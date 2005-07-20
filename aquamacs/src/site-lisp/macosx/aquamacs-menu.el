;; aquamacs-menu
;; redefines and modifies the menu bar

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-menu.el,v 1.11 2005/07/20 23:09:01 davidswelt Exp $

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
    (if (eq 'string (type-of (car (cdr (cdr (assq key (lookup-key global-map keymap)))))))
	(setcar (cdr (cdr (assq key (lookup-key global-map keymap))))
	      str
	      )

    (setcar (cdr (assq key (lookup-key global-map keymap)))
	    str
	    )

)
    )
  (define-key global-map (vconcat (append keymap (list key)))
    (cdr (assq key (lookup-key global-map keymap)))
    )

  )

(defun aquamacs-updated-is-visible-frame-p ()
  
  (and (frame-live-p menu-updating-frame)
       (frame-visible-p menu-updating-frame ))
)


 
;; apple command character is unicode x2318  
;; 
(setq apple-char (string (decode-char 'ucs #X2318)))

;; The following is a big hack. The mac port can't currently cope 
;; with putting the command key combos in the menu, for various 
;; reasons (1. they are just secondary alternatives, 2. command is defined
;; as 'hyper' and only known as such)

; redefine New
; (define-key menu-bar-edit-menu [mark-whole-buffer] (cdr (assq 'mark-whole-buffer (lookup-key global-map [menu-bar edit]))))




(define-key menu-bar-file-menu [new-file]
  '(menu-item (format  "New                            %sN"  apple-char)  new-frame-with-new-scratch
	      :enable (or one-buffer-one-frame
			  (not (window-minibuffer-p
			    (frame-selected-window menu-updating-frame))))
	      :help "Read or create a file and edit it"))
 

;(change-menu-text-2 [menu-bar application] 'quit (format  "Quit Emacs                %sQ"  apple-char))
(change-menu-text [menu-bar file] 'open-file (format  "Open File...                 %sO"  apple-char)) 

;; redefine this
(define-key menu-bar-file-menu [kill-buffer]
  '(menu-item (format "Close (current buffer)  %sW" apple-char) close-current-window-asktosave
	      :enable (aquamacs-updated-is-visible-frame-p)
	      :help "Discard current buffer"))
 
(change-menu-text [menu-bar file] 'exit-emacs (format  "Quit Emacs                %sQ"  apple-char))
;(change-menu-text [menu-bar application] 'quit (format  "Quit Emacs                %sQ"  apple-char))
(change-menu-text [menu-bar edit] 'copy (format  "Copy                 %sC"  apple-char))
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

(define-key menu-bar-file-menu [write-file]
  '(menu-item "Save Buffer As..." write-file

	      :enable (and (frame-live-p menu-updating-frame)
			   (frame-visible-p menu-updating-frame )
			   (not (window-minibuffer-p
				 (frame-selected-window menu-updating-frame))))
	      :help "Write current buffer to another file"))


(define-key menu-bar-file-menu [split-window]
  '(menu-item "Split Window" split-window-vertically
	      :enable (and (frame-live-p menu-updating-frame)
			   (frame-visible-p menu-updating-frame )
			   (not (window-minibuffer-p
				 (frame-selected-window menu-updating-frame))))
	      :help "Split selected window in two"))


;; Printing (redefinition for :enable)

(define-key menu-bar-file-menu [ps-print-region]
  '(menu-item "Postscript Print Region (B+W)" ps-print-region
	      :enable mark-active
	      :help "Pretty-print marked region in black and white to PostScript printer"))
(define-key menu-bar-file-menu [ps-print-buffer]
  '(menu-item "Postscript Print Buffer (B+W)" ps-print-buffer
	      :enable (and (frame-live-p menu-updating-frame)
			   (frame-visible-p menu-updating-frame ))
	      :help "Pretty-print current buffer in black and white to PostScript printer"))
(define-key menu-bar-file-menu [ps-print-region-faces]
  '(menu-item "Postscript Print Region" ps-print-region-with-faces
	      :enable mark-active
	      :help "Pretty-print marked region to PostScript printer"))
(define-key menu-bar-file-menu [ps-print-buffer-faces]
  '(menu-item "Postscript Print Buffer" ps-print-buffer-with-faces
	      :enable (and (frame-live-p menu-updating-frame)
			   (frame-visible-p menu-updating-frame ))
	      :help "Pretty-print current buffer to PostScript printer"))
(define-key menu-bar-file-menu [print-region]
  '(menu-item "Print Region" print-region
	      :enable mark-active
	      :help "Print region between mark and current position"))
(define-key menu-bar-file-menu [print-buffer]
  '(menu-item "Print Buffer" print-buffer
	      :enable (and (frame-live-p menu-updating-frame)
			   (frame-visible-p menu-updating-frame ))
	      :help "Print current buffer with page headings"))




;; redefinition (for :enable)
(define-key menu-bar-options-menu [truncate-lines]
  '(menu-item "Truncate Long Lines in this Buffer"
	      toggle-truncate-lines
	      :help "Truncate long lines on the screen"
	      :button (:toggle . truncate-lines)
	      :enable (and (frame-live-p menu-updating-frame)
			   (frame-visible-p menu-updating-frame ))))

(require 'longlines) 

;; must use the menu-item syntax here because longlines-mode
;; is a buffer-local variable
(define-key-after menu-bar-options-menu [longlines-on]
  '(menu-item "Soft word wrap in this Buffer"
	      longlines-mode
	      :help "Wrap long lines without inserting carriage returns"
	      :enable (aquamacs-updated-is-visible-frame-p)
	      :button (:toggle . longlines-mode))
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
  (menu-bar-make-toggle toggle-oneonone one-buffer-one-frame
			"Display Buffers in Separate Frames"
			"Display Buffers in Separate Frames: %s"
			"Open a new Frame (window) for each new buffer."
			(require 'aquamacs-frame-setup)
		 
		        (setq one-buffer-one-frame
			      (not one-buffer-one-frame))
			) 'edit-options-separator)
)

(if (boundp 'mac-pass-option-to-system) 
    (define-key-after menu-bar-options-menu [option-to-system]
      (menu-bar-make-toggle toggle-pass-option-to-system mac-pass-option-to-system
			    "Option key produces only special characters"
			    "Option key produces only special characters: %s"
			    "Let Option key produce special characters (passing the key to the system),
rather than behaving as Meta"
			    
			    (setq mac-pass-option-to-system
				       (not mac-pass-option-to-system))
			
			    ) 'edit-options-separator)

  )

 ;; this is a redefine
(define-key menu-bar-options-menu [mouse-set-font]
  '(menu-item "Set Font..." mouse-set-font
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
  '(menu-item "Set Color Theme..." aquamacs-color-theme-select
	       :visible (and (display-multi-font-p)
			     (fboundp 'aquamacs-color-theme-select)
			     )
	       :enable (aquamacs-updated-is-visible-frame-p) 
	       :help "Select a color theme from a list")
  'mouse-set-font)




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
	:button `(tool-bar-enabled-p))
    'showhide-tool-bar)


;; SENDMAIL doesn't usually work on OS X
;; unless postfix is set up
(easy-menu-remove-item global-map  '("menu-bar" "tools") 'compose-mail)


;; HELP MENU

; these problems here are for X-based systems etc. and not relevant
; for Aquamacs users
(easy-menu-remove-item global-map  '("menu-bar" "Help") 'emacs-problems)
 
 
;; register the help manuals
(defun init-user-help ()
  (if (condition-case nil 
	  (file-exists-p (car command-line-args)) 
	(error nil))
      (shell-command (concat "python -c \"from Carbon import AH; AH.AHRegisterHelpBook('" (substring (car command-line-args) 0 -21) "')\" >/dev/null 2>/dev/null") t t) 
    ; else
    (message "Emacs.app has been moved or renamed. Please restart Emacs!")
  )
)

;; it's imporant to make sure that the following are in the Info.plist file:
;; 	<key>CFBundleHelpBookFolder</key>
;; 	 <array>
;; 	   <string>Aquamacs Help</string>
;; 	   <string>Emacs Manual</string>
;; 	</array>
;; 	 <key>CFBundleHelpBookName</key>
;; 	 <array>
;; 	   <string>Aquamacs Help</string>
;; 	   <string>Emacs Manual</string>
;; 	</array>
;; it is vital that the folder name ("Aquamacs Help") is the same as
;; given above, and that it is also in a META tag in the help file.
;; spelling of the META tag (upper case) might be important.

; Call up help book
(defun aquamacs-user-help ()
  (interactive)

  (init-user-help) ; make sure it's registered
 
  (or (shell-command "python -c \"from Carbon import AH; AH.AHGotoPage('Aquamacs Help', None, None)\"  >/dev/null 2>/dev/null" t t)
      (message "Sorry, help function unavailable (python, OS problem?)")
  )
)
(defun aquamacs-emacs-manual ()
  (interactive)

  (init-user-help) ; make sure it's registered
 
  (or (shell-command "python -c \"from Carbon import AH; AH.AHGotoPage('Emacs Manual', None, None)\"  >/dev/null 2>/dev/null" t t)
      (message "Sorry, help function unavailable (python, OS problem?)")
  )
)
 
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
  ["-" nil nil] 'emacs-tutorial)
  

;; workarounds for current bugs

; can't get rid of the menu bar on a Mac
(easy-menu-remove-item global-map  '("menu-bar" "options" "showhide") 'menu-bar-mode)

; can't show a frame on a different display
(easy-menu-remove-item global-map  '("menu-bar" "file") 'make-frame-on-display)



(provide 'aquamacs-menu)
 