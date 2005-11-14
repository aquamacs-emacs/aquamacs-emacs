;; Aquamacs Emacs startup file
;; these defaults attempt to turn Emacs into a nice application for 
;; operating systems with a graphical user interface.
 
;; This is the central configuration file.  
;;
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs.el,v 1.24 2005/11/14 09:10:40 davidswelt Exp $ 

;; This file is part of Aquamacs Emacs
;; http://aquamacs.org/

;; Aquamacs Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Aquamacs Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;; Copyright (C) 2005, David Reitter



(defvar aquamacs-version "0.9.7beta")
(defvar aquamacs-version-id 097.0)
(defvar aquamacs-minor-version "")


(defun aquamacs-setup ()

  ;; workaround for memory corruption bug
  (garbage-collect) 
  ;; setting it to a much higher value will cause
  ;; (temporary) lockups and swapping
  (setq gc-cons-threshold 4000000)


  (aquamacs-mac-initialize) ;; call at runtime only
       
  (require 'aquamacs-tools)

  ;; Stop Emacs from asking for "y-e-s", when a "y" will do. 


  

  (defcustom aquamacs-quick-yes-or-no-prompt-flag t
    "If non-nil, the user does not have to type in yes or no at
yes-or-no prompts - y or n will do."
    :group 'Aquamacs
    :version "22.0"
    :type 'boolean
    )
  (defvaralias 'aquamacs-quick-yes-or-no-prompt 
    'aquamacs-quick-yes-or-no-prompt-flag)

; (yes-or-no-p "asda")
  ; (aquamacs-ask-for-confirmation "asd" t)
 
  (defun aquamacs-repl-yes-or-no-p (text)
    (aquamacs-ask-for-confirmation text t))
  (defun aquamacs-y-or-n-p (text)
    (aquamacs-ask-for-confirmation text nil))

  (unless (fboundp 'old-yes-or-no-p)
    (fset 'old-yes-or-no-p (symbol-function 'yes-or-no-p)))
  (unless (fboundp 'old-y-or-n-p)
    (fset 'old-y-or-n-p (symbol-function 'y-or-n-p)))
  
  (defun aquamacs-ask-for-confirmation (text long)
    (let ((f (window-frame (minibuffer-window))))
      (raise-frame f)			; make sure frame is visible
;;       (let ((y (- (display-pixel-height) (frame-total-pixel-height f) 30 ))) ; extra 30 pix for typical Dock
;; 	(print y)
;; 	(if (< y (eval (frame-parameter f 'top)))
;; 	    (modify-frame-parameters f (list (cons 'top y)))
;; 	  )
;; 	)
      (if (and
	   (or ;; ensure that the minibuffer shows up on screen
	    (not (fboundp 'mac-display-available-pixel-bounds))
	    (not (fboundp 'frame-total-pixel-height))
	    (< (+ (eval (frame-parameter f 'top)) 
		  (frame-total-pixel-height f))
	       (nth 3 (mac-display-available-pixel-bounds))))
	   (or (and last-nonmenu-event (not (consp last-nonmenu-event)))
	      (not use-dialog-box)
	      (not (fboundp 'mac-dialog-y-or-n-p))
	      (not window-system)))
	  (if (and long (not aquamacs-quick-yes-or-no-prompt))
	      (old-yes-or-no-p text)
	    (old-y-or-n-p text))
	(let ((ret (mac-dialog-y-or-n-p text "" t)))
	  (if (eq ret 'cancel)
	      (keyboard-quit))
	  ret))))         
  ;; it would be nice to offer a "cancel" option like C-g in the dialog

  (fset 'y-or-n-p 'aquamacs-y-or-n-p)
  (fset 'yes-or-no-p 'aquamacs-repl-yes-or-no-p)

  (defadvice map-y-or-n-p (around raiseframe (&rest args) activate)
    (raise-frame)
    ad-do-it)    

  ;; No more annoying bells all the time

  (aquamacs-set-defaults 
   '((ring-bell-function (lambda () (message "")))
     )
   )

  ;; this can be turned off in .emacs via
  ;; (setq ring-bell-function nil)

  (defcustom aquamacs-ring-bell-on-error-flag nil
    "If non-nil, Aquamacs gives an audio signal in cases of error, regardless of ``ring-bell-function''."
    :group 'Aquamacs
    :version "22.0"
    :type 'boolean
    )
  (defvaralias  'aquamacs-ring-bell-on-error 'aquamacs-ring-bell-on-error-flag)

  ;; but please ring the bell when there is a real error
  (defadvice error (around ring-bell (&rest args) activate protect)
 
    (if aquamacs-ring-bell-on-error-flag
	(progn
	  (let ((ring-bell-function nil))
	    (ding))
					;	(message (prin1-to-string args))
	  ad-do-it)
      ;; else
      ad-do-it)) 



  ;; Find-file may open a new frame
  (if window-system
      (global-set-key [remap find-file] 'aquamacs-find-file)  
    )
 


  (add-hook 'after-init-hook 
	    (lambda () 
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

   

  ;; Mac OS X specific stuff 

  (when (eq window-system 'mac)
    (require 'osx_defaults)
    (aquamacs-osx-defaults-setup)
    )
  (eval-when-compile
    (require 'osx_defaults)
    (aquamacs-osx-defaults-setup)
    )
  (require 'easymenu) 


  (require 'recentf)

  (defun aquamacs-recentf-show-basenames (l &optional no-dir)
    "Filter the list of menu elements L to show filenames sans directory.
When a filename is duplicated, it is appended a sequence number if
optional argument NO-DIR is non-nil, or its directory otherwise.
Separate paths from file names with --."
    (let (filtered-names filtered-list full name counters sufx)
      (dolist (elt l (nreverse filtered-list))
	(setq full (recentf-menu-element-value elt)
	      name (file-name-nondirectory full))
	(if (not (member name filtered-names))
	    (push name filtered-names)
	  (if no-dir
	      (if (setq sufx (assoc name counters))
		  (setcdr sufx (1+ (cdr sufx)))
		(setq sufx 1)
		(push (cons name sufx) counters))
	    (setq sufx (file-name-directory full)))
	  (setq name (format "%s -- %s" name sufx)))
	(push (recentf-make-menu-element name full) filtered-list))))

  (aquamacs-set-defaults 
   '(
     ( recentf-max-menu-items 25)
     (recentf-menu-before "Open Directory...                 ")
     (recentf-keep ( mac-is-mounted-volume-p file-remote-p file-readable-p))
     (recentf-filename-handler abbreviate-file-name)
     (recentf-menu-filter aquamacs-recentf-show-basenames)))  

  ;; define a single command to be included in the recentf menu
  (defun recentf-clearlist ()
    "Remove all files from the recent list."
    (interactive)
    (setq recentf-list ()))

  (setq recentf-menu-items-for-commands
	(list ["Clear Menu"
	       recentf-clearlist
	       :help "Remove all excluded and non-kept files from the recent list"
	       :active t]
        
	      ) 
	)



  (recentf-mode 1)  

  (global-set-key "\C-x\ \C-r" 'recentf-open-files)  


  (setq file-name-coding-system 'utf-8)

;; Page scrolling


  (require 'pager)
  ;; overwrites CUA stuff
 
  (global-set-key [remap scroll-up]	      'pager-page-down)
  (global-set-key [remap cua-scroll-up]	      'pager-page-down)
  (global-set-key [next] 	      'pager-page-down)
  (global-set-key [\M-up]	      'pager-page-up)
  (global-set-key [remap scroll-down]	      'pager-page-up) 
  (global-set-key [remap cua-scroll-down]	      'pager-page-up)
  (global-set-key [prior]	      'pager-page-up)
  ;; was here in 0.9.5, taken out
  ;;(global-set-key [C-up]        'pager-row-up)
  ;;(global-set-key [C-down]      'pager-row-down)
 



  (require 'aquamacs-menu)
  (require 'aquamacs-bug) ;; successfully send bug reports on the Mac



  (require 'longlines) 
  (aquamacs-set-defaults 
   `(
 
					; Colorized fonts
					; Turn on font-lock in all modes that support it
     (global-font-lock-mode t)
     (font-lock-maximum-decoration t)

					; Make Text mode the default mode for new buffers
					; turn on Auto Fill mode automatically in Text mode  
     (default-major-mode text-mode)
     (initial-major-mode text-mode)


					; scroll just one line when hitting the bottom of the window
     (scroll-step 1)
     (scroll-conservatively 10000)
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

     ;; Do not complain when a minibuffer is still open somewhere

     (enable-recursive-minibuffers t)
 
     (longlines-wrap-follows-window-size t)

     ;; do not allow user to mess with minibuffer prompt

     (minibuffer-prompt-properties
      ,(plist-put minibuffer-prompt-properties
		  'point-entered 'minibuffer-avoid-prompt))

     )
   )


  ;; set a nntp server if there's none
  (if (getenv "NNTPSERVER") ;; (gnus-getenv-nntpserver)
      nil
    (aquamacs-set-defaults '(

			     (setq gnus-select-method 
				   '(nntp "news.readfreenews.net"))
			     )
			   )
    )

					; activate the modes now
  (global-font-lock-mode 1) 
  (column-number-mode 1)

					; ------- Frames (OSX Windows) ----------

					; format the title-bar to always include the buffer name
  (setq frame-title-format "Emacs - %b")
 
 
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
    (let ((undo-window (list (window-buffer) (window-start) (window-point)))
	  (obof one-buffer-one-frame) ;;may be buffer-local!
	  ) 
      (switch-to-buffer buffer)
      (view-mode-enter (cons (selected-window) (cons (cons nil undo-window) obof))
		       exit-action)))



  ;; Make sure it's saved to .emacs when necessary
  ;; we need to redefine this here - this is copied and modified from menu-bar.el
  ;; there is no method to mark a customize-variable to save _and_ to 
  ;; set need-save so that it will be saved to .emacs. 


					; write the aquamacs-version to end of customizations.el
					; warning: bug - this will add to the file
					; so the file will grow over time
					; because the last (setq is what actually counts,
					; this shouldn't cause any problems.
  ;; (defadvice custom-save-all  
  ;;   (after save-aquamacs-customization-version (&rest args) activate)
 
  ;;   (write-region
  ;;    (with-output-to-string
  ;;      (print `(setq aquamacs-customization-version-id
  ;;      ,aquamacs-customization-version-id))
  ;;      )
  ;;    nil ;end
  ;;    custom-file
  ;;    'append
  ;;    'quiet
  ;;    )
  ;; )
 
  ;; mode-specific font settings
  (require 'aquamacs-mode-specific-themes)
  (aquamacs-mode-specific-themes-setup)

 
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
	
	
      
  (provide 'drews_init)	; migration from 0.9.1 (require in customizations)

  ;; http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries
  
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

  ;; automatic positioning please  
  ;; for normal windows
  ;; for special windows, the user can set and save things
  ;; also, we don't want the initial frame to move around
  (setq default-frame-alist (assq-delete-all 'top default-frame-alist))
  (setq default-frame-alist (assq-delete-all 'left default-frame-alist)) 
  (setq default-frame-alist (assq-delete-all 'width default-frame-alist))
  (setq default-frame-alist (assq-delete-all 'height default-frame-alist))
  ;; sensible defaults for the position of the special windows
  ;; (in case user turns them off)
  (assq-set 'top 30 'special-display-frame-alist)
  (assq-set 'left '(- 0) 'special-display-frame-alist)
  (assq-set 'height 30 'special-display-frame-alist)
  (assq-set 'width 75 'special-display-frame-alist)
  (assq-set 'user-position nil 'special-display-frame-alist)


 



					; and turn on smart frame positioning

  (require 'smart-frame-positioning)

  (aquamacs-set-defaults 
   '((smart-frame-positioning-mode t)
     ( smart-frame-positioning-enforce t) ;; and enforce it
     )
   )

  (smart-frame-positioning-mode t) ;; and turn on!

  

  ;; make sure there are no old customizations around
  ;; N.B.: if no customization file is present, 
  ;; aquamacs-customization-version-id is 0924
  (defun aquamacs-activate-features-new-in-this-version ()

    ;; aquamacs-customization-version-id contains the version id
    ;; of aquamacs when the customization file was written

    (if (< aquamacs-customization-version-id 092.5)

	;; make sure we fit frames
	(assq-set 'user-position nil 'default-frame-alist)

      )

    (if (< aquamacs-customization-version-id 092.8)
	;; bring the lucida font back because
	;; we have switched over to monaco as the default
	(mapc 
	 (lambda (th)
	   (unless (assq (car th) aquamacs-mode-specific-default-themes)
	     (assq-set (car th) 
		       (cdr th)
		       'aquamacs-mode-specific-default-themes)))
	 ;; list
	 (filter-fonts '(
			 (text-mode  
			  (font . "fontset-lucida14")) 
			 (change-log-mode  
			  (font . "fontset-lucida14"))
			 (tex-mode  
			  (font . "fontset-lucida14"))
			 (paragraph-indent-text-mode  
			  (font . "fontset-lucida14"))
			 ))))
    (if (< aquamacs-customization-version-id 094.1)
	(progn
	  ;; in the mode-spec themes, this is taken care of
	  ;; anyways
	  (setq default-frame-alist 
		(assq-delete-all 'scroll-bar-width default-frame-alist))
	  (setq special-display-frame-alist 
		(assq-delete-all 'scroll-bar-width special-display-frame-alist))
      
	  (mapc 
	   (lambda (th)
	     (unless (assq (car th) aquamacs-mode-specific-default-themes)
	       (assq-set (car th) 
			 (cdr th)
			 'aquamacs-mode-specific-default-themes)))
	   ;; list
	   (filter-fonts '(
			   (help-mode (tool-bar-lines . 0) (fit-frame . t)) 
			   (fundamental-mode (tool-bar-lines . 0))
			   (custom-mode (tool-bar-lines . 0) (fit-frame . t)))))))

;; Print warnings / compatibility options
    
    (if (boundp 'mac-reverse-ctrl-meta)
	(message "Warning: `mac-reverse-ctrl-meta' is not used any more from
Aquamacs 0.9.7 on. This variable had been deprecated for several versions.
Use `mac-{control|command|option|function}-modifier' instead."))
    (if (boundp 'mac-command-key-is-meta)
	(message "Warning: `mac-command-key-is-meta' is not used any more from
Aquamacs 0.9.7 on. This variable had been deprecated for several versions.
Use `mac-command-modifier' instead."))


    (when (boundp 'mac-pass-option-to-system)
      (when mac-pass-option-to-system
	   (setq mac-option-modifier-enabled-value mac-option-modifier)
	   (setq mac-option-modifier nil))
      (if (> aquamacs-customization-version-id 096.0)
	(message "Warning: `mac-pass-option-to-system' is deprecated from
Aquamacs 0.9.7 on. `mac-option-modifier' has been set for you."))))

  (require 'one-buffer-one-frame)
 

;; ----------- MISC STUFF ----------------


  ;; (require 'ibuffer)

  (put 'upcase-region 'disabled nil)
 

  ;; -------- MOUSE BEHAVIOR / SELECTION -------------

  ;; we activate CUA mode again but disable the keys 
  ;; thanks to Lawrence Akka for hints on the following section
  ;; ;; not needed


  (require 'mouse-sel) ; provie functions - but don't turn on mouse-sel-mode
 

  (aquamacs-set-defaults '((cua-use-hyper-key only) ;;this avoids shift-return
			   (cua-enable-cua-keys nil)))
 
  ;; enable cua-keep-region-after-copy only for the mac like commands
  (defadvice cua-copy-region (around keep-region activate)
    (if (eq this-original-command 'clipboard-kill-ring-save)
	(let ((cua-keep-region-after-copy t))
	  ad-do-it) 
      ;; respect user's setting of cua-keep-region-after-copy for M-w etc.
      ad-do-it))

  (cua-mode 1) ;; this goes first (so we can overwrite the settings)

  
  ;; do not use [ ... ] notation - pure space allocation!

  (let ((cmdkey (or mac-command-modifier 'alt)))
    (global-set-key (vector '(shift down-mouse-1)) 'mouse-extend)
    (global-set-key (vector `(shift ,cmdkey down-mouse-1)) 
		    'mouse-extend-secondary)
    (global-set-key (vector `(,cmdkey mouse-1)) 'mouse-start-secondary)
    (global-set-key (vector `(,cmdkey drag-mouse-1)) 'mouse-set-secondary)
    (global-set-key (vector `(,cmdkey down-mouse-1)) 'mouse-drag-secondary))
 
  (aquamacs-set-defaults '((x-select-enable-clipboard nil) 
			   (cua-mode t)
			   (mouse-sel-leave-point-near-mouse t)))

  ;; mainly to ensure  that we overwrite a marked region
  ;; (transient-mark-mode nil)


;  (when (string= "mac" window-system)
; don't do this -  
 ;;   (put 'PRIMARY 'mac-scrap-name "org.gnu.Emacs.selection.PRIMARY")

  ;; unnecessary
;    (setq mac-services-selection 'PRIMARY)
;    )
;;;;;;;;;;;;
 
     
					; applications on OS X don't display a splash screen 
 
(setq command-line-args  (append command-line-args (list "--no-splash")))
(setq inhibit-startup-message t)

;; redefine this
(defun startup-echo-area-message ()
  (if (eq (key-binding [(alt \?)]) 'aquamacs-user-help)
      "For an introduction to Aquamacs Emacs, type Apple-?."
    (substitute-command-keys
     "For a introduction to Aquamacs Emacs, type \
\\[aquamacs-user-help].")))

(if (string= "mac" window-system)
    (defun use-fancy-splash-screens-p () t)
  )

       
;; the following causes not-so-good things to happen.
;; (defun fancy-splash-default-action () nil)

(aquamacs-set-defaults
 '((fancy-splash-image "aquamacs-splash-screen.jpg")
   (fancy-splash-max-time 3000)))

(defadvice fancy-splash-screens (around new-frame (&rest args) activate protect)
 
  (let ((one-buffer-one-frame-force t))
    ad-do-it)
  (message " ")
  )

  ;; only the fancy splash screen is displayed more than once
  ;; this is a workaround    
 
    
  ;; scratch buffer should be empty 
  ;; the philosophy is: don't give users any text to read to get started!    

  (aquamacs-set-defaults '( 
			   (  initial-scratch-message nil)
			     )
			 )
 
    

  ;; while pc selection mode will be turned on, we don't
  ;; want it to override Emacs like key bindings. 
  ;; we need to fill the following variable with something
  ;; that is non-nil.
  (setq pc-select-default-key-bindings '(([home]      . beginning-of-buffer)
					 ([end]      . end-of-buffer)
					 ))

  (aquamacs-set-defaults 
   '( 
     (mouse-wheel-progessive-speed nil)
   ;;  (mouse-wheel-scroll-amount (1 (shift . 0.5) (control . 0.2) ))
     (mouse-wheel-scroll-amount (1 ((shift) . 0.5) ((control) . 0.2) ))
     (pc-select-meta-moves-sexps t)
     (pc-select-selection-keys-only t)
     (pc-selection-mode t)
     (show-paren-mode t)
     (blink-cursor-mode t)
     (cursor-type (bar . 2))
     ;; on modern systems, loading files doesn't take so long any more.
     (large-file-warning-threshold 20000000)
     )
   ) 
   

  
					; activate the modes


  (pc-selection-mode 1) 
  (show-paren-mode 1) 
  (blink-cursor-mode 1)

  (set-default 'cursor-type '(bar . 2))


  ;;; for initial buffer
;;; for some reason
(add-hook 'after-init-hook (lambda ()
			     (setq buffer-offer-save t)
			     ))
     
  ;; Define customization group

  (defgroup Aquamacs 
    '((one-buffer-one-frame custom-variable)
      (aquamacs-mode-specific-default-themes custom-variable)
      (smart-frame-positioning-enforce custom-variable)
      (smart-frame-positioning-mode custom-variable)
      (mac-option-modifier  custom-variable)
      (mac-pass-option-to-system  custom-variable)
      (mac-control-modifier  custom-variable)
      (mac-pass-control-to-system  custom-variable)
      (mac-command-modifier  custom-variable)
      (mac-pass-command-to-system  custom-variable)
      (special-display-regexps custom-variable)
      )
    "Options specific to Aquamacs Emacs. Some of these customizations
values exist in GNU Emacs as well, but have default values different 
from those in GNU Emacs. Customize them to achieve the GNU Emacs behavior.
Note that not all customization variables with differing defaults are
listed here."
    :group 'emacs
    )
 

;; ensure that Save Options saves all of our fancy new options
;; TO DO:
;; now that variable setting has been revised
;; we should review whether this is still necessary.

  ;; this is initialized to the current version 
  ;; it'll be overwritten by whatever is in the customization file
  (defvar aquamacs-customization-version-id 0)
  ;; the following ensures that it gets saved
  ;; as customized variable.
  (customize-set-variable 'aquamacs-customization-version-id 
			  aquamacs-customization-version-id)

  (defun menu-bar-options-save ()
    "Save current values of Options menu items using Custom."
    (interactive)
    (let ((need-save nil))
      (setq aquamacs-customization-version-id aquamacs-version-id)
      ;; These are set with menu-bar-make-mm-toggle, which does not
      ;; put on a customized-value property.
      (dolist (elt '(line-number-mode 
		     column-number-mode 
		     size-indication-mode
		     cua-mode show-paren-mode
		     transient-mark-mode 
		     global-font-lock-mode
		     display-time-mode 
		     display-battery-mode
				      ))
	(and (customize-mark-to-save elt)
	     (setq need-save t))) 
      ;; 
      ;; These are set with `customize-set-variable'.
      (dolist (elt '(scroll-bar-mode
		     debug-on-quit debug-on-error
		     tooltip-mode menu-bar-mode tool-bar-mode
		     save-place uniquify-buffer-name-style fringe-mode
		     indicate-empty-lines indicate-buffer-boundaries
		     case-fold-search
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
		     mac-pass-option-to-system
		     aquamacs-auto-frame-parameters-flag
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




  ;; workaround for people who still call this in their .emacs
  (defun mwheel-install ()
    (princ "mwheel-install ignored in Aquamacs- mouse wheel support is present by default.\n")
    t
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; temporary stuff for releases according to admin/FOR-RELEASE

  (setq undo-ask-before-discard nil)
;; http://sourceforge.net/tracker/index.php?func=detail&aid=1295333&group_id=138078&atid=740475				      


;; patch from RMS
(defun custom-save-all ()
  "Save all customizations in `custom-file'."
  (let* ((filename (custom-file))
	 (recentf-exclude (if recentf-mode
			      (cons (concat "\\`"
					    (regexp-quote (custom-file))
					    "\\'")
				    recentf-exclude)))
	 (old-buffer (find-buffer-visiting filename)))
    (with-current-buffer (or old-buffer (find-file-noselect filename))
      (let ((inhibit-read-only t))
	(custom-save-variables)
	(custom-save-faces))
      (let ((file-precious-flag t))
	(save-buffer))
      (unless old-buffer
	(kill-buffer (current-buffer))))))

  ;; workarounds for current bugs 
 
  ;; (tool-bar-setup) ;; when image.el is preloaded,
  ;; tool bar is created too early upon launch.
  ;; therefore, set it up again, this time after
  ;; image-load-path is defined properly

  '			      ; can't get rid of the menu bar on a Mac
  (easy-menu-remove-item global-map 
			 '("menu-bar" "options" "showhide") 'menu-bar-mode)

					;' can't show a frame on a different display
  (easy-menu-remove-item global-map 
			 '("menu-bar" "file") 'make-frame-on-display)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; this is for the About dialog

  (setq emacs-build-system 
	(concat emacs-build-system
		" - Aquamacs Distribution " 
		aquamacs-version aquamacs-minor-version))

  (require 'check-for-updates)
					; via hook so it can be turned off
  (add-hook 'after-init-hook 'aquamacs-check-for-updates-if-necessary 'append) 

  ) ;; aquamacs-setup


(provide 'aquamacs)

 