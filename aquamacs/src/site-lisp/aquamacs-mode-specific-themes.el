;; aquamacs-mode-specific-themes themes

;; this package realizes mode-specific themes in Aquamacs.
;; it is not complete - right now this files just
;; serves as a collection of function that interact with
;; things from osx_defaults
 
;; Filename: aquamacs-frame-setup.el
;; Description: Emacs init file for use with libraries from Drew Adams
;; Author: David Reitter
;; Maintainer: David Reitter
;; Keywords: aquamacs
 

;; Last change: $Id: aquamacs-mode-specific-themes.el,v 1.2 2005/07/08 22:27:05 davidswelt Exp $

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

;; Copyright (C) 2005, David Reitter, all rights reserved.


;; this is overridden by the user's customization

;; but use a different font for other modes
; this doesnt work yet, because set-frame-font is applied to the wrong
; frame at that point

;; mode-specific font settings
;;   contains a list


(defcustom aquamacs-mode-specific-default-themes
  (filter-fonts '(
		  (text-mode  (font . "fontset-lucida13")) 
		  (change-log-mode  (font . "fontset-lucida13"))
		  (tex-mode  (font . "fontset-lucida13"))
		  (outline-mode  (font . "fontset-lucida13"))
		  (paragraph-indent-text-mode  (font . "fontset-lucida13"))
		  (speedbar-mode (minibuffer-auto-raise . nil))
		  ))
  "Association list to set mode-specific themes. Each element 
is a list of elements of the form (mode-name theme), where
THEME is an association list giving frame parameters as
in default-frame-alist or (frame-parameters). The fontset is set
whenever the mode MODE-NAME is activated.
Set this to nil to turn mode-specific themes off.
Note that when a major mode is changed, frames are automatically
parametrized. Parameters in ``default-frame-alist'' and 
``special-display-frame-alist'' serve as defaults which are 
superceded by a setting in this list, if there is an entry
for the current major mode. To turn off this behavior, see
``aquamacs-auto-frame-parameters''.
"
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Mode-name")
		       (repeat (cons :format "%v"
				     (symbol :tag "Frame-Parameter")
				     (sexp :tag "Value")))))
  :group 'Aquamacs
  )

(defcustom aquamacs-auto-frame-parameters t
   "When non-nil, frames are automatically
parametrized when a major mode is changed. 
Parameters in ``default-frame-alist'' and 
``special-display-frame-alist'' serve as defaults which are 
superceded by a setting in ``aquamacs-mode-specific-default-themes'', 
if there is an entry for the current major mode."
   :type 'boolean
   :group 'Aquamacs
   )

  
 
 
; update the help-mode specification with a fit-frame
; append it, so the user's choice has priority
(defun 	make-help-mode-use-frame-fitting ()

  (when aquamacs-auto-frame-parameters
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
  )


   
(add-hook 'after-init-hook
	'make-help-mode-use-frame-fitting
	'append) ;; move to the end: after loading customizations
	
	
(defun aquamacs-combined-mode-specific-settings (default-alist theme)

  (if aquamacs-auto-frame-parameters
      (progn
	(dolist (th default-alist )
     
	  (unless (assq (car th) theme)
	    (setq theme (cons th theme))
	    )
	  )
	(mapc
	 (lambda (e) (setq theme (assq-delete-all e theme)))
	 '(user-position menu-bar-lines top height left width scroll-bar-width)
	 )
	;; workaround
	(assq-set 'scroll-bar-width 0 'theme)
	theme
	) 
					; else
    nil
    )
  ) 

(defun set-mode-specific-theme (&optional frame force)

  (when aquamacs-auto-frame-parameters

    (unless frame (setq frame (selected-frame)))

    (if (frame-live-p frame)  

	(condition-case err		; (otherwise, Emacs hangs)
      
					; frame-configured-for-buffer stores for which buffer
					; and which major-mode the frame configuration
					; is for, so we don't have to apply the theme again. 
					; This is also very important because setting the theme in itself
					; will cause another menu-bar-update-hook call, so we can end up
					; with this function called again and again...

	    (let ((buffer (window-buffer (frame-first-window frame))))
	    
	      (if (or 
		   (not (equal (frame-parameter frame 
						'frame-configured-for-buffer)
					;(cons 
			       buffer 
					; major-mode)
			       ))
		   force)

		  (save-excursion
		    (set-buffer buffer)
		    (let ((theme (aquamacs-combined-mode-specific-settings 
				  (if (special-display-p (buffer-name)) 
				      special-display-frame-alist 
				    default-frame-alist
				    )
				  (get-mode-specific-theme major-mode)
		      
				  )
				 )
			  )
		    
		   
		      ;; make sure we don't move the whole frame -
		      ;; it is already shown on screen, and 
		      ;; the position is determined by "smart-frame-positioning",
		      ;; that is per file name and according to the 'smart' heuristic
		    
		 
		      (modify-frame-parameters frame (cons (cons 'frame-configured-for-buffer 
				 
								 buffer 
				 
								 ) theme))
		      )
		    )
		)
	      )
	  (error (print err))  
	  )
      )
    )
  )



(defun get-mode-specific-theme (mode) 
  (if aquamacs-auto-frame-parameters
      (cdr (assq mode aquamacs-mode-specific-default-themes)) 
    nil
    )
)

(defun set-mode-theme-after-change-major-mode ()       			      
  ;; delete the configuration cache parameter
  ;; sometimes, this will be called for the buffer, but before
  ;; the target frame has been switched to the new buffer.
  ;; that's bad luck then. 
  
  (when aquamacs-auto-frame-parameters
 
    (dolist (f (find-all-frames-internal (current-buffer)))
      ;; update the theme 
      (set-mode-specific-theme f t)
      )  
    )  
  )

(add-hook 'after-change-major-mode-hook	
	  'set-mode-theme-after-change-major-mode
	  )



;; (setq after-change-major-mode-hook nil) 

(defun set-mode-theme-after-make-frame (frame) 
  ;; only if we have a window and a buffer here
  (if (and aquamacs-auto-frame-parameters
	   (frame-first-window) (window-buffer (frame-first-window frame)))
      ;; make sure we acticate the right buffer
      ;; and that we don't change the selected frame
      (save-excursion
	(set-buffer (window-buffer (frame-first-window frame)))
	(set-mode-specific-theme frame)
	) 
    )
  )
;;(add-hook 'after-make-frame-functions	
;;	  'set-mode-theme-after-make-frame
;;	  )

;;(setq last-major-mode-theme-in-this-frame nil)

(defun update-mode-theme ()
  "Update the theme (colors, font) of the selected frame 
to be appropriate for its first buffer"
  
  
  (condition-case err
      ;; we must catch errors here, because
      ;; otherwise Emacs would clear menu-bar-update-hook
      ;; which would be not good at all.
      (progn 
	(unless
	    (minibuffer-window-active-p (selected-window))
	  ;;(make-variable-frame-local 'last-major-mode-theme-in-this-frame)
	  ;;(setq last-major-mode-theme-in-this-frame major-mode)
	  ;; can't call ->crash
	  (set-mode-specific-theme)
	  )
	(define-key menu-bar-options-menu [menu-set-theme-as-mode-default]
	  '(menu-item  (format "Use current theme for %s"
			       (or major-mode "current mode"))
		       aquamacs-set-theme-as-mode-default 
		       :help ""
		       :enable  (and (frame-live-p (selected-frame))
				     (frame-visible-p (selected-frame) ))
				  
		       ))
	)

    (error nil)
    )
  t
  )
(add-hook 'menu-bar-update-hook 'update-mode-theme)

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
    
  (message (format "Theme has been set as default for %s. %s" major-mode
		   (if aquamacs-auto-frame-parameters
		       ""
		     "Note: aquamacs-auto-frame-parameters is nil - hence functionality is off!"
		     )
		   )
	   )
  )


; 
(defun aquamacs-delete-mode-specific-themes ()
  "Deletes all mode-specific themes set previously"
  (interactive)
 (customize-set-variable  'aquamacs-mode-specific-default-themes nil)
  (message "Mode-specific themes removed. Add new ones or use customize to 
revert to the default. Save Options to store setting.")
  )
 
(define-key-after menu-bar-options-menu [menu-delete-themes]
  '(menu-item  "Delete all mode-specific themes"     aquamacs-delete-mode-specific-themes 
	      :help "Deletes all mode-specific themes set previously.") 'mouse-set-font)

(define-key-after menu-bar-options-menu [menu-set-theme-as-default]
  '(menu-item  "Use current theme as default"     aquamacs-set-theme-as-default
	 
	      :help "") 'mouse-set-font)

(define-key-after menu-bar-options-menu [menu-set-theme-as-mode-default]
  '(menu-item  "Use current theme for this mode"     aquamacs-set-theme-as-mode-default 
	      :help "") 'mouse-set-font)


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


;; advise frame-notice-user-settings (from frame.el)
;; to integrate the mode-specific frame settings
;; which supersede the default-frame-alist, but not
;; the initial-frame-alist

;; for some reason, we can't byte-compile this.
(defadvice frame-notice-user-settings 
  (around aquamacs-respect-mode-defaults () activate)
  (if aquamacs-auto-frame-parameters

      (let ((default-frame-alist  
	      (aquamacs-combined-mode-specific-settings 
	       default-frame-alist
				  
	       (get-mode-specific-theme major-mode)
		      
	       )
	      )
	    )
	ad-do-it
    
	)
					; else
    ad-do-it
    )
  )


(provide 'aquamacs-mode-specific-themes)