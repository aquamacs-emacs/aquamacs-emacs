;; aquamacs-menu
;; redefines and modifies the menu bar

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-menu.el,v 1.1 2005/06/13 22:46:56 davidswelt Exp $

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
	      :enable (not (window-minibuffer-p
			    (frame-selected-window menu-updating-frame)))
	      :help "Read or create a file and edit it"))
 

;(change-menu-text-2 [menu-bar application] 'quit (format  "Quit Emacs                %sQ"  apple-char))
(change-menu-text [menu-bar file] 'open-file (format  "Open File...                 %sO"  apple-char)) 
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


;; we will set the following ones directly
;; customization is always possible
;; the existing menu item is badly worded and the C-c/v/x don't apply anyways
(easy-menu-remove-item global-map  '("menu-bar" "options") 'cua-mode) 
(easy-menu-remove-item global-map  '("menu-bar" "options") 'uniquify)


(change-menu-text [menu-bar options] 'mouse-set-font "Set Font...")


;; Quit entry shouldnt be there
(easy-menu-remove-item global-map  '("menu-bar" "file") 'separator-exit)
(easy-menu-remove-item global-map  '("menu-bar" "file") 'exit-emacs)

;; About entry is now in application menu
(easy-menu-remove-item global-map  '("menu-bar" "Help") 'about)

;; this is to set the action for the "Quit" function (Emacs menu)
(global-set-key [mac-application-quit] 'save-buffers-kill-emacs)
 


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
  (browse-url "http://aquamacs.sourceforge.net/wiki/")
) 
 (defun aquamacs-homepage ()
  (interactive)
  (browse-url "http://aquamacs.sourceforge.net/")
) 

(easy-menu-add-item  nil '("Help")
  (vector (format "Aquamacs Help                    %s?"  apple-char) 'aquamacs-user-help) 'emacs-tutorial)

(easy-menu-add-item  nil '("Help")
  (vector (format "Aquamacs Tips Wiki Online"  apple-char) 'aquamacs-user-wiki) 'emacs-tutorial)
 

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
  ["-" nil nil] 'emacs-tutorial)
  


(provide 'aquamacs-menu)
 