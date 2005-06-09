;; Aquamacs Emacs OS X defaults
;; these defaults attempt to turn Emacs into a nice application for 
;; Mac OS X that adheres to at least some of the user interface 
;; standards on the Mac
;;
;; This is the central configuration file. Most of the code
;; here deals with the "one buffer, one frame" feature, and
;; with updating the menus to show better-conforming entries.
;;
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: osx_defaults.el,v 1.6 2005/06/09 19:52:50 davidswelt Exp $

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

 

; remaining issues

; we need a proper font selection dialogue
; we need to go through Quartz / Carbon to print

; don't open scratch window at start
;; hide the frame
 

; this file needs cleaning up!
; move menu stuff to extra package
; move one-buffer-one-frame to extra mode

(require 'aquamacs-tools)

(require 'mac-extra-functions)

(mac-add-standard-directories)

 
; load files (give full paths and load all files)
; this will be called after .emacs has been loaded
(add-hook 'after-init-hook   '(lambda () 
(load "/Library/Preferences/Emacs/Preferences" t) 
(load "/Library/Preferences/Aquamacs Emacs/Preferences" t) 
(load "~/Library/Preferences/Emacs/Preferences" t) 
(load "~/Library/Preferences/Aquamacs Emacs/Preferences" t) 
 ))

(mac-read-environment-vars-from-shell)

 (setenv "INFOPATH" (concat (getenv "INFOPATH") 
                ":~/Library/Application Support/Emacs/info"
                ":/Library/Application Support/Emacs/info"))
 





;; emulate a three button mouse with Option / Control modifiers 
; (setq mac-emulate-three-button-mouse t)
; seems to prevent setting the secondary selection, so turned off for now

;; Stop Emacs from asking for "y-e-s", when a "y" will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; No more annoying bells all the time

(aquamacs-set-defaults 
 '((ring-bell-function (lambda () (message "")))
  (visible-bell nil)
  )
)


; do this early, so we can override settings
(require 'aquamacs-frame-setup)
(setq osxkeys-command-key 'hyper)
; we have inhibit-fit-frame set to true... can't do this
; (global-set-key [(control ?x) (control ?-)] 'fit-frame)
(global-set-key `[(control ,osxkeys-command-key down)] 'enlarge-frame)
(global-set-key `[(control ,osxkeys-command-key right)] 'enlarge-frame-horizontally)
(global-set-key `[(control ,osxkeys-command-key up)] 'shrink-frame)
(global-set-key `[(control ,osxkeys-command-key left)] 'shrink-frame-horizontally)
    

(require 'mac-default-fontsets)
 
(setq mac-allow-anti-aliasing t) 

(require 'color-theme)
(setq color-theme-is-global nil)
(setq color-theme-target-frame nil)
(defadvice color-theme-install (around for-other-frame (&rest args) activate)
 
    (if color-theme-target-frame
	(select-frame color-theme-target-frame)
      )
    ad-do-it 
)

(defun aquamacs-color-theme-select ()
  (interactive)
  (setq color-theme-target-frame (selected-frame))
   
  (let ((one-buffer-one-frame t))	
; always open in new frame
; because we've redefined bury->kill-buffer-and window in color-theme
    (color-theme-select)
    )
 
  )

(easy-menu-add-item  nil '("Options")
  ["-" nil nil] 'mouse-set-font)
(easy-menu-add-item  nil '("Options")
  ["Set Color Theme..." aquamacs-color-theme-select t] 'mouse-set-font)





;; -- KEYS AND MENUs ---------------------------

;; (require 'msb)
;; (msb-mode 1);; better buffer menu
 
; os x like key bindings
(require 'osxkeys)
;; turn on mac key mode by default

(osx-key-mode 1) 

(condition-case nil (progn (make-directory "~/Library/Preferences/Aquamacs Emacs")
;; problem with this: could be started from /Volumes/.. (DMG) for first time, then moved		
	   (init-user-help) ;; init help system (first start)
			   )
  (error t)) 

(setq custom-file "~/Library/Preferences/Aquamacs Emacs/customizations.el")
(add-hook 'after-init-hook (lambda () 
		    (condition-case nil (load custom-file) (error t))
		    (aquamacs-activate-features-new-in-this-version)
		    ) 'append)

(defun font-exists-p (fontorfontset)
  (condition-case nil
      (or
       (font-info fontorfontset)
       (fontset-info fontorfontset)
       )
    (error nil)
    )
  )

(defun filter-font-from-alist (alist)
(if (and (assq 'font  alist)
	 (not (font-exists-p (cdr (assq 'font  alist)))) 
	 )
  (progn 
    (print (format "Warning: Font %s not available." (cdr (assq 'font  alist)))) 
    (assq-delete-all 'font alist) ;; return
    )
  alist)
) 

(defun filter-missing-fonts ()
  (setq default-frame-alist (filter-font-from-alist default-frame-alist))
  (setq special-display-frame-alist (filter-font-from-alist special-display-frame-alist))

  (let ((newlist))
	(dolist (th   aquamacs-mode-specific-default-themes    )
	 
	   (if (cdr th)   
	     (add-to-list 'newlist  (cons (car th)  (filter-font-from-alist (cdr th))))
	   )
	   
	   ) 
	(setq aquamacs-mode-specific-default-themes newlist)
	)
	  
  )

; filters all missing fonts from specifications, so we don't show
; stupid error messages
; especially necessary during 0.9.1 -> 0.9.2 transition, because
; scalable fonts have different names now
(add-hook 'after-init-hook 'filter-missing-fonts t) 

(require 'easymenu) 

(aquamacs-set-defaults '(
			
     (auto-save-list-file-prefix 
            "~/Library/Preferences/Aquamacs Emacs/auto-save-list/.saves-")
     ( save-place-file "~/Library/Preferences/Aquamacs Emacs/places.el")
     ( recentf-save-file "~/Library/Preferences/Aquamacs Emacs/Recent Files.el")
     ( recentf-max-menu-items 25)
     (recentf-menu-before "Insert File...")
     )
)

;; define a single command to be included in the recentf menu
(defun recentf-clearlist ()
  "Remove all files from the recent list."
  (interactive)
  (setq recentf-list ())
  )

(defvar recentf-menu-items-for-commands
 (list ["Clear Menu"
         recentf-clearlist
         :help "Remove all excluded and non-kept files from the recent list"
         :active t]
        
       )
  "List of menu items for recentf commands.")
  
(recentf-mode 1) 
(global-set-key "\C-x\ \C-r" 'recentf-open-files)  


(setq file-name-coding-system 'utf-8)

;; in the following, we use a combination of easy-menu and 
;; our own functions to modify existing menus

;; define a function to change texts in the menus
;; a long way to find out how to do this... thanks to Stefan Monnier who provided
;; some essential hints... 
 


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


;; we will set the CUA mode directly (below).
;; the existing menu item is badly worded and the C-c/v/x don't apply anyways
(easy-menu-remove-item global-map  '("menu-bar" "options") 'cua-mode)


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
  

 

(require 'aquamacs-bug) ;; successfully send bug reports on the Mac


;; ---------------------------------------------------------
;; PERL EDITING and other modes

(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
;(setq cperl-hairy t)
(defalias 'perl-mode 'cperl-mode)
 (setq cperl-invalid-face nil) ;(uherbst)

; (setq cperl-electric-keywords nil)   
; (setq cperl-font-lock t)                   ;; Turns on font lock in CPerl mode -- which colors text
; (setq cperl-electric-lbrace-space t)       ;; *Non-nil (and non-null) means { after $ in CPerl 
                                            ;; buffers should be preceded by ` '.
                                            ;; Can be overwritten by 'cperl-hairy' if nil.
; (setq cperl-electric-parens nil)             ;; *Non-nil (and non-null) means parentheses should 
                                            ;; be electric in CPerl.  Can be overwritten by 
                                            ;; 'cperl-hairy' if nil.
; (setq cperl-electric-linefeed t)
; (setq cperl-electric-keywords nil)
; (setq cperl-info-on-command-no-prompt t)
; (setq cperl-clobber-lisp-bindings t)
; (setq cperl-lazy-help-time t)
 (setq cperl-highlight-variables-indiscriminately t)



;; both prolog and perl files are often called .pl;
;; this tries to do the right thing.
(defun prolog-or-perl-mode () (interactive)
  (if
      (or (string-match "/perl\\b" (buffer-string)) ; file with perl header
          (= 1 (point-max)))            ; new file
      (progn    
        (cperl-mode)
        (message "Ambiguous suffix .pl resolved to perl mode."))
    (progn 
      (prolog-mode)
      (message "Ambiguous suffix .pl resolved to prolog mode.")))
  (sit-for 1))
 
(autoload 'applescript-mode "applescript-mode" "major mode for editing AppleScript source." t)
(setq auto-mode-alist
      (cons '("\\.applescript$" . applescript-mode) auto-mode-alist)
      )

(aquamacs-set-defaults '(

; Colorized fonts
; Turn on font-lock in all modes that support it
			 (global-font-lock-mode t)
			 (font-lock-maximum-decoration t)

; Make Text mode the default mode for new buffers
; turn on Auto Fill mode automatically in Text mode  
			 (default-major-mode text-mode)
 
; scroll just one line when hitting the bottom of the window
			 (scroll-step 1)

;; Start scrolling when 2 lines from top/bottom 
			 (scroll-margin 1)


; no flash instead of that annoying bell
			 (visible-bell nil)

; Display the column number of the point in the mode line
			 (column-number-mode t)

;;; Do not automatically add newlines on (page) down
			 (next-line-add-newlines nil)

;; Show directories in buffer names when needed
			 (buffers-menu-show-directories t)


			 )
)
 
(add-hook 'text-mode-hook 'turn-on-auto-fill)

; activate the modes now
(global-font-lock-mode t) 
(column-number-mode t)

; ------- Frames (OSX Windows) ----------

; format the title-bar to always include the buffer name
(setq frame-title-format "Emacs - %b")
 
;;   ((window-id) (buffer-list) (name) (title) (icon-name))
(add-to-list 'frame-parameters-to-exclude '(minibuffer))
(add-to-list 'frame-parameters-to-exclude '(frame-configured-for-buffer))
(defun aquamacs-set-theme-as-default () 
  "Activate current frame settings (theme) as default. Sets default-frame-alist."
  (interactive)
					; need to find out if frame
  (let ((frte frame-parameters-to-exclude)) ; make backup

    (setq frame-parameters-to-exclude 
	  (append '((user-position) (visibility)  (top) (left) (width) (height)) frame-parameters-to-exclude))

    (set-all-frame-alist-parameters-from-frame
     (if (special-display-p (buffer-name (current-buffer)))
	 'special-display-frame-alist
       'default-frame-alist)
     )
    (setq frame-parameters-to-exclude frte) ; restore old value
    )
					; (setq initial-frame-alist default-frame-alist)  
  (message (concat "Theme has been set as default for all new " (if (special-display-p (buffer-name (current-buffer)))
								    "special, internal frames"
								  "normal frames.")))
  )


(defun aquamacs-set-theme-as-mode-default () 
  (interactive)
   "Activate current theme as default for a given mode."

   ;; stolen from frame-cmds.el
   (setq theme (set-difference (frame-parameters (selected-frame))
                              (append '((user-position) (visibility) (top) (left) (width) (height)) frame-parameters-to-exclude)
                             :key 'car))


  (customize-set-variable 'aquamacs-mode-specific-default-themes

			  (cons (cons major-mode theme
				      ) 
				(assq-delete-all major-mode aquamacs-mode-specific-default-themes)
				)
 
			  )
    
  (message (format "Theme has been set as default for %s" major-mode))
  )
; 
(define-key-after menu-bar-options-menu [menu-set-theme-as-default]
  '(menu-item  "Use current theme as default"     aquamacs-set-theme-as-default
	 
	      :help "") 'mouse-set-font)

(define-key-after menu-bar-options-menu [menu-set-theme-as-mode-default]
  '(menu-item  "Use current theme for current mode"     aquamacs-set-theme-as-mode-default 
	      :help "") 'mouse-set-font)

;; one buffer per frame
 
;; define customization option
(defcustom one-buffer-one-frame t
  "When non-nil, open a new frame for each new buffer and switch to that frame
   when buffer is selected from Buffers menu. When nil, regular buffers are displayed
   in the same frame and window."
  :type '(radio 
		(const :tag "Open new frames for buffers" t)
		(const :tag "standard Emacs behavior (nil)" nil))
  :require 'aquamacs-frame-setup)
 


;; ;; add a menu item to the Options menu
;; (define-key-after menu-bar-options-menu [inhibitfitframe]
;;   (menu-bar-make-toggle toggle-oneonone inhibit-fit-frame
;; 			"Resize Frames to Fit Buffers"
;; 			"Resize Frames to Fit Buffers: %s"
;; 			"Adjusts the size of a frame depending on the buffer displayed."
;; 			(require 'aquamacs-frame-setup)
		 
;; 			(setq inhibit-fit-frame-flag
;; 			      (not inhibit-fit-frame-flag))
;; 			)
;; 			) 'edit-options-separator)

;; add a menu item to the Options menu

(if (string= "mac" window-system)
(define-key-after menu-bar-options-menu [oneonone]
  (menu-bar-make-toggle toggle-oneonone one-buffer-one-frame
			"Display Buffers in Separate Frames"
			"Display Buffers in Separate Frames: %s"
			"Open a new Frame (window) for each new buffer."
			(require 'aquamacs-frame-setup)
		 
			(not (setq one-buffer-one-frame
			      (not one-buffer-one-frame)))
			
	;; (setq pop-up-frames one-buffer-one-frame)		
			) 'edit-options-separator)
)

;(add-hook 'after-init-hook (lambda () (setq pop-up-frames one-buffer-one-frame)) t)


(require 'view)
;; redefine view-buffer
(defun view-buffer (buffer &optional exit-action)
  "View BUFFER in View mode, returning to previous buffer when done.
Emacs commands editing the buffer contents are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type H or h while viewing.

This command runs the normal hook `view-mode-hook'.

Optional argument EXIT-ACTION is either nil or a function with buffer as
argument.  This function is called when finished viewing buffer.
Use this argument instead of explicitly setting `view-exit-action'."

  (interactive "bView buffer: ")
  (let ((undo-window (list (window-buffer) (window-start) (window-point))))
    (switch-to-buffer buffer)
    (view-mode-enter (cons (selected-window) (cons (cons nil undo-window) one-buffer-one-frame))
		     exit-action)))



;; Make sure it's saved to .emacs when necessary
;; we need to redefine this here - this is copied and modified from menu-bar.el
;; there is no method to mark a customize-variable to save _and_ to 
;; set need-save so that it will be saved to .emacs. 

; this is initialized to the customization version BEFORE
; this versioning system was introduced in 0.9.2b5
; it'll be overwritten by whatever is in the customization file
(defvar aquamacs-customization-version-id 092.4)

; write the aquamacs-version to end of customizations.el
; warning: bug - this will add to the file
; so the file will grow over time
; because the last (setq is what actually counts,
; this shouldn't cause any problems.
(defadvice custom-save-all  
  (after save-aquamacs-customization-version (&rest args) activate)
 
  (write-region
   (with-output-to-string
     (print `(setq aquamacs-customization-version-id
     ,aquamacs-customization-version-id))
     )
   nil ;end
   custom-file
   'append
   'quiet
   )
)
 
(defun menu-bar-options-save ()
  "Save current values of Options menu items using Custom."
  (interactive)
  (let ((need-save nil))
    (setq aquamacs-customization-version-id aquamacs-version-id)
    ;; These are set with menu-bar-make-mm-toggle, which does not
    ;; put on a customized-value property.
    (dolist (elt '(line-number-mode column-number-mode cua-mode show-paren-mode
		   transient-mark-mode global-font-lock-mode
		    ))
      (and (customize-mark-to-save elt)
	   (setq need-save t))) 
    ;; 
    ;; These are set with `customize-set-variable'.
    (dolist (elt '(scroll-bar-mode
		   debug-on-quit debug-on-error menu-bar-mode aquamacs-tool-bar-mode
		   save-place uniquify-buffer-name-style fringe-mode
		   fringe-indicators case-fold-search
		   display-time-mode auto-compression-mode
		   current-language-environment default-input-method
		   ;; Saving `text-mode-hook' is somewhat questionable,
		   ;; as we might get more than we bargain for, if
		   ;; other code may has added hooks as well.
		   ;; Nonetheless, not saving it would like be confuse
		   ;; more often.
		   ;; -- Per Abrahamsen <abraham@dina.kvl.dk> 2002-02-11.
		   text-mode-hook

		   blink-cursor-mode
		   ;; added dr. 04/2005
		   one-buffer-one-frame 
		   default-frame-alist
		   special-display-frame-alist
		   aquamacs-mode-specific-default-themes
		   smart-frame-prior-positions
		   aquamacs-customization-version-id
		   ))
      (and (get elt 'customized-value) 
	   (customize-mark-to-save elt)
	   (setq need-save t)))
    ;; Save if we changed anything.
    (when need-save
      (custom-save-all))))

(defun fontset-exist-p (font)
(condition-case nil
    (fontset-info font)
  (error nil))
)

;; this needs to be replaced by functions defined earlier
; recursion is not so good in elisp anyways
(defun filter-fonts (list)
 "Filters the font list LIST to contain only existing fontsets.
Each element of LIST has to be of the form (symbol . fontset)."
  (if (car list)
      (if (fontset-exist-p (cdr (cdr (car list))))
	  (cons (car list)
		(filter-fonts (cdr list))
		)
					; else
	(filter-fonts (cdr list))
	) 
					; else
    nil)

  )
 
;; this is overridden by the user's customization

;; but use a different font for other modes
; this doesnt work yet, because set-frame-font is applied to the wrong
; frame at that point

;; mode-specific font settings
;;   contains a list


(defcustom aquamacs-mode-specific-default-themes
  (filter-fonts '(
   (text-mode  (font . "fontset-lucida14")) 
   (change-log-mode  (font . "fontset-lucida14"))
   (tex-mode  (font . "fontset-lucida14"))
   (paragraph-indent-text-mode  (font . "fontset-lucida14"))
   ))
  "Association list to set mode-specific themes. Each element 
is a list of elements of the form (mode-name theme), where
THEME is an association list giving frame parameters as
in default-frame-alist or (frame-parameters). The fontset is set
whenever the mode MODE-NAME is activated.")
 
 
 
; update the help-mode specification with a fit-frame
; append it, so the user's choice has priority
(defun 	make-help-mode-use-frame-fitting ()

  (unless (assq 'fit-frame 
		(assq 'help-mode aquamacs-mode-specific-default-themes)
		) ;; unless it's already set

    (assq-set 'help-mode
	      (append  
	       (cdr (assq 'help-mode aquamacs-mode-specific-default-themes))
	       '((fit-frame . t))
	       )
	      'aquamacs-mode-specific-default-themes)
    )
  )


   
(add-hook 'after-init-hook
	'make-help-mode-use-frame-fitting
	'append) ;; move to the end: after loading customizations
	
	
;;  (defun set-theme ()
;;    (interactive)
;; (debug)
;;    (set-mode-specific-theme)
;;  )
; 
; (frame-parameter (selected-frame) 'frame-configured-for-buffer)

(defun set-mode-specific-theme (&optional frame force)
  (unless frame (setq frame (selected-frame)))

  (if (frame-live-p frame)

      (condition-case err		; (otherwise, Emacs hangs)
      
	  ; frame-configured-for-buffer stores for which buffer the frame configuration
	  ; is for, so we don't have to apply the theme again. 
	  ; This is also vedry important because setting the theme in itself
	  ; will cause another menu-bar-update-hook call, so we can end up
	  ; with this function called again and again...

	  (let ((buffer (window-buffer (frame-first-window frame))))
	    (if (or 
		    (not (eq (frame-parameter frame 
					     'frame-configured-for-buffer)
			     buffer))
		     force)
	      (save-excursion
		(set-buffer buffer)

		(let ((theme (get-mode-specific-theme major-mode))
		      )
					; (print default-frame-alist)
		  (dolist (th (if (special-display-p (buffer-name)) 
				  special-display-frame-alist 
				default-frame-alist
				)
			      )
      
		    (unless (assq (car th) theme)
		      (setq theme (cons th theme))
		      )
		    )
					; make sure we don't move the whole frame -
					; it is already shown on screen, and 
					; the position is determined by "smart-frame-positioning",
					; that is per file name and according to the 'smart' heuristic
		  (setq theme
			(assq-delete-all 'user-position
					 (assq-delete-all 'menu-bar-lines 
							  (assq-delete-all 'user-position 
									   (assq-delete-all 'top (assq-delete-all
												  'left (assq-delete-all 'height (assq-delete-all 'width
																		  theme))))))))
					;(print theme)
		  (modify-frame-parameters frame (cons (cons 'frame-configured-for-buffer buffer) theme))
		  )
		)
	      )
	    )
	(error (print err))  
	)
    )
  )



(defun get-mode-specific-theme (mode) 
  (cdr (assq mode aquamacs-mode-specific-default-themes)) 
)

(defun set-mode-theme-after-change-major-mode ()       			      
; delete the configuration cache parameter
  (dolist (f (find-all-frames-internal (current-buffer)))
     
; update the theme
    (set-mode-specific-theme f t)
    )  
)
 
(add-hook 'after-change-major-mode-hook	
	  'set-mode-theme-after-change-major-mode
	  )
; (setq after-change-major-mode-hook nil) 

(defun set-mode-theme-after-make-frame (frame) 
	    ; only if we have a window and a buffer here
	    (if (and (frame-first-window) (window-buffer (frame-first-window frame)))
		; make sure we acticate the right buffer
		; and that we don't change the selected frame
		(save-excursion
		  (set-buffer (window-buffer (frame-first-window frame)))
		  (set-mode-specific-theme frame)
		  ) 
	      )
	    )
;(add-hook 'after-make-frame-functions	
;	  'set-mode-theme-after-make-frame
;	  )

;(setq last-major-mode-theme-in-this-frame nil)

(setq update-mode-theme-busy nil) ;; needed as workaround probably for some obscure bug
;; crashes otherwise
(defun update-mode-theme ()
  "Update the theme (colors, font) of the selected frame 
to be appropriate for its first buffer"
  
      (condition-case nil
					; we must catch errors here, because
					; otherwise Emacs would clear menu-bar-update-hook
					; which would be not good at all.
	  (unless
	      (minibuffer-window-active-p (selected-window))
					;(make-variable-frame-local 'last-major-mode-theme-in-this-frame)
					;(setq last-major-mode-theme-in-this-frame major-mode)
					; can't call ->crash
	    (set-mode-specific-theme)
	    )
	(error nil)
	)
  )
(add-hook 'menu-bar-update-hook 'update-mode-theme)

; menu-bar-update-hook


; (setq after-make-frame-functions nil) 

 
;; need to copy frame settings (font) into default-frame-alist (using setup-frames library)
;; avoids popping up frames and then resizing them

(add-hook 'after-init-hook   
	  '(lambda () 
	     (set-mode-specific-theme)
	     
	     ;; because we don't want the first frame to dance around
	     ;; more than necessary, we set height + width to whatever
	     ;; it is initially.
	     ;; we also ensure that the attributes for text mode
	     ;; are set, because otherwise, default-frame-alist
	     ;; is assumed by Emacs.
	     (setq initial-frame-alist 
		   (append 
		    (cdr (assq major-mode 
			       aquamacs-mode-specific-default-themes))
		    (list
		     (cons 'height (frame-parameter (selected-frame) 'height))
		     (cons 'width (frame-parameter (selected-frame) 'width))
		     )
		    )
		   )
	     ;; this is for the initial frame   
	     ;; but probably doesn't make sense at this point
	     ;; at this point, the toolbar exists - everything is fine
  
	     )
'append
	  )

;; this is to set the default font from what has been specified 
;; in the customization variables
;; the following doesn't work - even though the hook is run, set-frame-font doesnt set the font for the first frame.

;; http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries
  
;; this loads a huge package to get emacs to play ball with multiple frames
 (require 'aquamacs-frame-setup)  
(provide 'drews_init) ; migration from 0.9.1 (require in customizations)

;; set default fonts - after aquamacs-frame-setup has initialized things

(if (fontset-exist-p "fontset-monaco12") 
    (assq-set 'font "fontset-monaco12" 'default-frame-alist)
  (if (fontset-exist-p "fontset-mac_roman_12") 
      (assq-set 'font "fontset-mac_roman_12" 'default-frame-alist)

    (if (fontset-exist-p "fontset-lucida14") 
	(assq-set 'font "fontset-lucida14" 'default-frame-alist)
      )
    )
  )

(if (fontset-exist-p "fontset-mac_roman_12") 
    (assq-set 'font "fontset-mac_roman_12" 'special-display-frame-alist)

  (if (fontset-exist-p "fontset-monaco12") 
    (assq-set 'font "fontset-monaco12" 'special-display-frame-alist))
)

; automatic positioning please  
; for normal windows
; for special windows, the user can set and save things
(setq default-frame-alist (assq-delete-all 'top default-frame-alist))
(setq default-frame-alist (assq-delete-all 'left default-frame-alist))

; sensible defaults for the position of the special windows
; (in case user turns them off)
(assq-set 'top 30 'special-display-frame-alist)
(assq-set 'left '(- 0) 'special-display-frame-alist)
(assq-set 'height 30 'special-display-frame-alist)
(assq-set 'width 75 'special-display-frame-alist)
(assq-set 'user-position nil 'special-display-frame-alist)

; and turn on smart frame positioning

(require 'smart-frame-positioning)

(aquamacs-set-defaults 
 '((smart-frame-positioning-mode t)
   ( smart-frame-positioning-enforce t) ; and enforce it
   )
 )

(smart-frame-positioning-mode t) ; and turn on!

; make sure there are no old customizations around
; N.B.: if no customization file is present, 
; aquamacs-customization-version-id is 0924
(defun aquamacs-activate-features-new-in-this-version ()

  ; aquamacs-customization-version-id contains the version id
  ; of aquamacs when the customization file was written

  (if (< aquamacs-customization-version-id 092.5)

      ; make sure we fit frames
      (assq-set 'user-position nil 'default-frame-alist)

      )

  (if (< aquamacs-customization-version-id 092.8)
      ;; bring the lucida font back because
      ;; we have switched over to monaco as the default
      (dolist (th (filter-fonts '(
				  (text-mode  
				   (font . "fontset-lucida14")) 
				  (change-log-mode  
				   (font . "fontset-lucida14"))
				  (tex-mode  
				   (font . "fontset-lucida14"))
				  (paragraph-indent-text-mode  
				   (font . "fontset-lucida14"))
				  )))

	(unless (assq (car th) aquamacs-mode-specific-default-themes)
	  (assq-set (car th) 
		    (cdr th)
		    'aquamacs-mode-specific-default-themes)
	  )
	)
    )
)



(defun open-in-other-frame-p (buf)
  
  (let ( (bufname (get-bufname buf))
	 )
     
    (if one-buffer-one-frame 	 
	(if
	    (member bufname
		    '(
		      "\*Completions\*" 
		      "\*Apropos\*" 
		      " SPEEDBAR" ; speedbar package opens its own frame
		      )
		    )
	    nil
	  t 				
   
	  )
					; else --> not one-buffer-one-frame
      (special-display-p (get-bufname (car args))) ; return nil if not special-display buffer 
      )
    )
  )

(defun killable-buffer-p (buf)
  
  (let ( (bufname (get-bufname buf))
	 )
 
    (if one-buffer-one-frame
	(if (or (equal "\*Messages\*" bufname) 
	      
		(equal  "\*scratch\*" bufname) 
		(equal  "\*Help\*" bufname) 
	      
		)
	    nil
      
	  t
	  ) nil 
	    )
    )
  )


; init
(setq aquamacs-newly-opened-frames '() )

;; only for certain special buffers

 

(if (string= "mac" window-system)
(defadvice switch-to-buffer (around sw-force-other-frame (&rest args) activate)
    
  
					; is buffer shown in a frame?
  (if (and one-buffer-one-frame
	   (walk-windows
	    (lambda (w)
	      (if (eq (window-buffer w) (get-bufobj (car args)))
	     
		   (make-frame-visible (select-frame (window-frame w)
						    )
				      ) 
		  
		)
	      ) t) ;; include hidden frames
	   )
      t
  
    (if (or (not (visible-frame-list))
	    (not (frame-visible-p (selected-frame)))
	    (open-in-other-frame-p (car args))
	     
	    )
	(if (equal (car args) (buffer-name (current-buffer)))  ; is buffer already current? then make sure it's visible.
	    (raise-frame (selected-frame) )  ; bring to front
	     (progn
	        
	       (apply #'switch-to-buffer-other-frame args)
	  
	       (add-to-list 'aquamacs-newly-opened-frames (cons (selected-window) (current-buffer))) ;; store the frame/buffer information
	       )
	     )
	; else : show in same frame
      (if (window-dedicated-p (selected-window))
        (apply #'switch-to-buffer-other-window args)
					; else: show in same frame
	ad-do-it
	)
	
      )
    )
 
  (set-mode-specific-theme)
 
  )
)


;; make sure that when a minibuffer is ready to take input, 
;; the appropriate frame is raised (made visible)
(setq minibuffer-auto-raise t)

;; we'd like to open new frames for some stuff


(if (string= "mac" window-system)
    (defadvice find-file (around force-other-frame (&rest args) activate)
					; (interactive nil)
      (if one-buffer-one-frame  
   
	  (progn 
	    ;; (select-frame (make-frame))
	    ;;(apply #'find-file (find-file-read-args "Find file: " nil)) 
	    (apply #'find-file-other-frame args)
	    (add-to-list 'aquamacs-newly-opened-frames (cons (selected-window) (current-buffer)))
	    )
         
	ad-do-it 
	)
      )
  )
;;; 


  
; would like to get rid of frames -- this needs more testing.
; however, it doesn't work for various reasons
; one could make h-W just kill the buffer and then handle things here
; however, kill-buffer is called a lot for buffers that are not associated
; with a frame and we would need to make sure that only buffers for
; which a new frame was created will take their dedicated frame with
; them when they are killed!
; maybe the previous force-other-frame should keep track of
; newly opened frames!






; quit-window is usually called by some modes when the user enters 'q'
; e.g. in dired. we want to delete the window then.        
 (defadvice quit-window (around always-dedicated (&rest args) activate)
   (interactive)
   (if one-buffer-one-frame
       (let (save (window-dedicated-p (selected-window)))
	 (set-window-dedicated-p (selected-window) t)
	 ad-do-it
	 (set-window-dedicated-p (selected-window) save)
	 )
; else
     ad-do-it
     )
   )


 
;; delete window when buffer is killed
;; but only do so if aquamacs opened a new frame&window for
;; this buffer (e.g. during switch-to-buffer)

(defun delete-window-if-created-for-buffer ()

   (let (
	 (buf (current-buffer))
	 )
     
     (let ((winlist (find-all-windows-internal buf))
	   
	   )
        
       (dolist (win winlist)
	 ; force deletion if buffer is not killable
	 (delete-window-if-created-for-this-buffer win buf t)
	 ; (not (killable-buffer-p buf)))

	 
	
       )
     )
				
   ) 
)
    


(setq pop-up-frames nil)
(setq pop-up-windows t)
(setq display-buffer-reuse-frames t)

(defadvice pop-to-buffer (around always-dedicated (buf &rest args) protect activate) 

  (if one-buffer-one-frame
      (let ((puf pop-up-frames)
	    (sw (selected-window))
	    (wd (window-dedicated-p (selected-window)))
	    )
 
	(setq pop-up-frames (not 
			     (string-match "[ ]*\*(Completions|Apropos)\*" 
					   (get-bufname buf))
				 )
	      )
 
	(set-window-dedicated-p sw nil) 
	ad-do-it
	(set-window-dedicated-p sw wd)
	(setq pop-up-frames puf)

	)
    ;; else
    ad-do-it

    )
  )

; make sure that push-button does not lead to reusing of 

(defun delete-window-if-created-for-this-buffer (win buf force)
  ;; used by osxkeys, too
  ;; as of now, we're always forcing the deletion of a window if the user requests it.
  ;; 
 
  (let ((elt (car (member (cons win buf) aquamacs-newly-opened-frames))))
    (if (or force elt (window-dedicated-p win) )
	(progn
	  ;; remove entry from windows list
	  (if elt
	      (setq aquamacs-newly-opened-frames (delq elt aquamacs-newly-opened-frames))
	    )

	  ;; delete the window (or make the frame invisible)
	  (condition-case nil 
	      (if (window-live-p win)
		  (delete-window win) ;; only get rid of that current window
		)
	    (error   
	     

	     (make-frame-invisible (selected-frame) t) 
	     (if (find-all-frames-internal (get-buffer "*Messages*"))
		 (select-frame (car (find-all-frames-internal (get-buffer "*Messages*"))) 
			       )
	       )

	     ) 
	    )
)
      ;; else:
      ;; decide not to delete / make invisible
      ;; then switch buffer
	  (next-buffer)
	   
	  )
      )
    
  )
(defun delete-window-if-one-buffer-one-frame ()
  (if one-buffer-one-frame
      (delete-window-if-created-for-buffer)
    )
  )
(if (string= "mac" window-system)
    (add-hook 'kill-buffer-hook 'delete-window-if-one-buffer-one-frame t)
  )
 
;; redefine this from frame.el
;; (why advise it when we don't call it anyways?)
(require 'osxkeys)
(defun handle-delete-frame (event)
  "Handle delete-frame events from the X server."
  (interactive "e")
  (let ((frame (posn-window (event-start event)))
	(i 0)
	(delw nil)
	)
    (select-frame frame)
     

    (while 
	(and (frame-first-window frame) 
	(window-live-p (frame-first-window frame))
	(select-window (frame-first-window frame))
	(setq delw (cons (frame-first-window frame) delw))
	(close-current-window-asktosave)
	(frame-live-p frame)
	(next-window (selected-window) 'nominibuf frame)
	(not (memq  (frame-first-window frame) delw))
	)) 
    )
  )  
 



;; make sure that C-mouse-1 menu acts locally
(defadvice mouse-buffer-menu (around select-buffer-same-frame (&rest args) activate) 
 (let ((one-buffer-one-frame nil))
   ad-do-it
) 
)
  

;; as a bugfix, we're redefining this
;; in order to create a new frame if all frames are invisible
(defun fancy-splash-frame ()
  "Return the frame to use for the fancy splash screen.
Returning non-nil does not mean we should necessarily
use the fancy splash screen, but if we do use it,
we put it on this frame."
  (let (chosen-frame)
   
    (dolist (frame (append (frame-list) (list (selected-frame))))
      (if (and (frame-visible-p frame)
	       (not (window-minibuffer-p (frame-selected-window frame))))
	  (setq chosen-frame frame))) 
    (if chosen-frame
	chosen-frame
      
      (or
       ;; make visible
       (select-frame (car (frame-list))) 
       ;; or create a new one
       (make-frame)
       )
      )
    )
)
; no tool-bars, always white 
(defadvice fancy-splash-screens (around modify-frame (&rest args) activate)

  (let ( (default-frame-alist '( (tool-bar-lines . 0) (minibuffer . nil ) ) ) )
    ad-do-it
    )
  (message "") ;; workaround ("wrong argument")
)





; ----------- MISC STUFF ----------------


; Set up tool-bar
; this needs to be done after color themes are defined
 
(require 'aquamacs-tool-bar) 
(aquamacs-set-defaults '((tool-bar-mode nil)
			 (aquamacs-tool-bar-mode t)
			 ))
(tool-bar-mode -1)
(aquamacs-tool-bar-mode 1)

;; overwrite the menu option (originally defined in menu-bar.el)

(define-key menu-bar-showhide-menu [showhide-tool-bar]
  (list 'menu-item "Tool-bar" 'aquamacs-tool-bar-mode
	:help "Turn tool-bar in normal frames on/off"
	:visible `(display-graphic-p)
	:button `(:toggle . aquamacs-tool-bar-mode)))



(require 'ibuffer)

(put 'upcase-region 'disabled nil)

;; keep formatting all the time

(defun space-then-fill (fillp)
  "Insert SPACE then fill-paragraph"
  (interactive "P")
  (insert " ")
  (fill-paragraph fillp)
  (if (eolp)
  (insert " ") ) )
;; (local-set-key " " 'space-then-fill)



;; -------- MOUSE BEHAVIOR / SELECTION -------------

;; we activate CUA mode again but disable the keys 
;; thanks to Lawrence Akka for hints on the following section
;; ;; not needed


(require 'mouse-sel) ; provie functions - but don't turn on mouse-sel-mode
 

    (aquamacs-set-defaults '( 
			     (cua-use-hyper-key only) ;;this avoids shift-return
			     (cua-keep-region-after-copy t)

			     )
			   )
 
 
(setq cua-enable-cua-keys nil) ; not customizable
(cua-mode 1) ;; this goes first (so we can overwrite the settings)

  
(global-set-key [(shift down-mouse-1)] 'mouse-extend)
(global-set-key [(shift hyper down-mouse-1)] 'mouse-extend-secondary)

(global-set-key `[(,osxkeys-command-key mouse-1)] 'mouse-start-secondary)
(global-set-key `[(,osxkeys-command-key drag-mouse-1)] 'mouse-set-secondary)
(global-set-key `[(,osxkeys-command-key down-mouse-1)] 'mouse-drag-secondary)

(setq x-select-enable-clipboard nil) 
 
    (aquamacs-set-defaults '( 
			     (cua-mode t)
			     (mouse-sel-leave-point-near-mouse t)
			     )
			   )
;; mainly to ensure  that we overwrite a marked region
; (transient-mark-mode nil)

; make services menu work again
(put 'PRIMARY 'mac-scrap-name "org.gnu.Emacs.selection.PRIMARY")
(setq mac-services-selection 'PRIMARY)

;;;;;;;;;;;;
 
     
; applications on OS X don't display a splash screen 
(setq command-line-args  (append command-line-args (list "--no-splash")))
(setq inhibit-startup-message t)


(if (string= "mac" window-system)
    (defun use-fancy-splash-screens-p () t)
)
;; only the fancy splash screen is displayed more than once
;; this is a workaround    

(setq fancy-splash-image "aquamacs-splash-screen.jpg")
    
    
; scratch buffer should be empty 
; the philosophy is: don't give users any text to read to get started!    

    (aquamacs-set-defaults '(  (  initial-scratch-message nil)
			       )
			   )
 
       
    
;; INITIAL FRAME 
;; Place first frame in the location (80, 80) on screen 
(setq initial-frame-alist   default-frame-alist)
;; no effect - the first frame is already there at this point!
(text-mode) 

(require 'mac-extra-functions)

   ; while pc selection mode will be turned on, we don't
; want it to override Emacs like key bindings. 
; we need to fill the following variable with something
; that is non-nil.
(setq pc-select-default-key-bindings '(([home]      . beginning-of-buffer)
				       ([end]      . end-of-buffer)
				       ))

    (aquamacs-set-defaults '( 
			     (mouse-wheel-progessive-speed nil)
			     (mouse-wheel-scroll-amount (1 (shift . 1) (control . 0.9) ))

			     (pc-select-meta-moves-sexps t)
			     (pc-select-selection-keys-only t)
			     (pc-selection-mode t)
			     (show-paren-mode t)
			     (blink-cursor-mode t)
			     (cursor-type (bar . 2))
			     )
			   ) 
   

  
; activate the modes


(pc-selection-mode t) 
(show-paren-mode t) 
(blink-cursor-mode t)
 
(set-default 'cursor-type '(bar . 2))
 


;; Define customization group

(defgroup Aquamacs 
  '((one-buffer-one-frame custom-variable)
    (aquamacs-mode-specific-default-themes custom-variable)
    (smart-frame-positioning-enforce custom-variable)
    (smart-frame-positioning-mode custom-variable)
    (1on1-*Completions*-frame-flag custom-variable)
    (1on1-*Help*-frame-flag custom-variable)
    (mac-option-modifier  custom-variable)
    (mac-pass-option-to-system  custom-variable)
    (mac-control-modifier  custom-variable)
    (mac-pass-control-to-system  custom-variable)
    (mac-command-modifier  custom-variable)
    (mac-pass-command-to-system  custom-variable)
 )
  "Aquamacs Options"
)

;; output the list of 1on1 variables with:

; (mapatoms (lambda (symbol)
;		(when (string-match "1on1-.*" (symbol-name symbol))
;		      (print (list symbol 'custom-variable)  )
;
;		      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; temporary stuff for releases according to admin/FOR-RELEASE

(setq undo-ask-before-discard nil)

;; workarounds for current bugs

; can't get rid of the menu bar on a Mac
(easy-menu-remove-item global-map  '("menu-bar" "options" "showhide") 'menu-bar-mode)

; can't show a frame on a different display
(easy-menu-remove-item global-map  '("menu-bar" "file") 'make-frame-on-display)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is for the About dialog
(setq aquamacs-version "0.9.2 beta-8")
(setq aquamacs-version-id 092.8)
(setq emacs-build-system (concat emacs-build-system " - Aquamacs Distribution " aquamacs-version))

(require 'check-for-updates)
; via hook so it can be turned off
(add-hook 'after-init-hook 'aquamacs-check-for-updates-if-necessary 'append)
 
(provide 'osx_defaults)
