;; aquamacs-mode-specific-themes themes

;; this package realizes mode-specific themes in Aquamacs.
;; it is not complete - right now this files just
;; serves as a collection of function that interact with
;; things from osx_defaults

;; Call aquamacs-mode-specific-themes-setup after loading to install.

;; Filename: aquamacs-frame-setup.el
;; Description: Emacs init file for use with libraries from Drew Adams
;; Author: David Reitter
;; Maintainer: David Reitter
;; Keywords: aquamacs
 

;; Last change: $Id: aquamacs-mode-specific-themes.el,v 1.18 2005/11/10 23:44:24 davidswelt Exp $

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

 


 
 
; update the help-mode specification with a fit-frame
; append it, so the user's choice has priority
(defun 	make-help-mode-use-frame-fitting ()

  (when aquamacs-auto-frame-parameters-flag
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


   
(defun aquamacs-combined-mode-specific-settings (default-alist theme)

  (if aquamacs-auto-frame-parameters-flag
      (progn
	(mapc  
	 (lambda (th)
	   (unless (assq (car th) theme)
	     (setq theme (cons th theme))
	     )
	   )
	 ;; list
	 default-alist
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

(defun set-mode-specific-theme (&optional frame force for-mode)
  "Sets the mode-specific theme (frame parameters) for FRAME
 (selected frame if nil), unless it is already set (or
FORCE is non-nil). Use theme of major mode FOR-MODE if given."
    
  (when aquamacs-auto-frame-parameters-flag

    (unless frame (setq frame (selected-frame) ))

    (if (frame-live-p frame)  

	(condition-case err ;; (otherwise, Emacs hangs)  
      
	    ;; frame-configured-for-buffer stores for which buffer  
	    ;; and which major-mode the frame configuration  
	    ;; is for, so we don't have to apply the theme again. 
	    ;; This is also very important because setting the theme in itself  
	    ;; will cause another menu-bar-update-hook call, so we can end up 
	    ;; with this function called again and again...  

	    (let ((buffer (window-buffer (frame-first-window frame))))
	    
	      (if (or 
		   force
		   (not (equal (frame-parameter frame 
						'frame-configured-for-buffer)
					;(cons  
			       buffer 
					; major-mode)  
			       ))
		   )

		  (save-excursion
		    (set-buffer buffer)
		    (let* ((theme (aquamacs-combined-mode-specific-settings 
				   (if (special-display-p (buffer-name)) 
				       special-display-frame-alist 
				     default-frame-alist
				     )
				   (if for-mode
				       (get-mode-specific-theme for-mode)
				     (append
				      (get-buffer-specific-theme (buffer-name))
				      (get-mode-specific-theme major-mode)))))
			   ;; read out color-theme		 
			   ( color-theme (cdr (assq 'color-theme theme)))
			   (theme (assq-delete-all 'color-theme theme)
				  ))  
		      ;; make sure we don't move the whole frame -  
		      ;; it is already shown on screen, and  
		      ;; the position is determined by `smart-frame-positioning',  
		      ;; that is per file name and according to the 'smart' heuristic   
		    
		 
		      (modify-frame-parameters frame (cons (cons 'frame-configured-for-buffer 
								 buffer 
								 ) theme))
		      (let ((color-theme-target-frame frame))

			(color-theme-install color-theme)
			)

		      )
		    )
		)
	      )
	  (error (print err))  
	  )
      )
    )
  )


; (get-buffer-specific-theme "*Help*")

(defun get-buffer-specific-theme (bufname) 
  (if aquamacs-auto-frame-parameters-flag
      
     
	   (cdr (assq-string-equal bufname 
				   aquamacs-buffer-specific-frame-themes))
        
    nil
    )
)


(defun get-mode-specific-theme (mode) 
  (if aquamacs-auto-frame-parameters-flag
      (or (cdr (assq mode aquamacs-mode-specific-default-themes)) 
	  ;(progn (print "resorting to default") nil)
	  (cdr (assq 'default aquamacs-mode-specific-default-themes))
	  ;(progn (print "nothing found") nil)
	  )
    nil
    )
)

(defun set-mode-theme-after-change-major-mode ()       			      
  ;; delete the configuration cache parameter
  ;; sometimes, this will be called for the buffer, but before
  ;; the target frame has been switched to the new buffer.
  ;; that's bad luck then. 
  
  (when aquamacs-auto-frame-parameters-flag
 
    (mapc 
     (lambda (f) 
       ;; update the theme 
       (set-mode-specific-theme f t)
       )  
     ;; list
     (find-all-frames-internal (current-buffer))
     )
    )  
  )



;; (setq after-change-major-mode-hook nil) 

(defun set-mode-theme-after-make-frame (frame) 
  ;; only if we have a window and a buffer here
  (if (and aquamacs-auto-frame-parameters-flag
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
	
	)

    (error nil)
    )
  t
  )
(defun update-mode-themes-everywhere ()
  "Update the themes (colors, font) of all frames
to be appropriate for its first buffer. (Aquamacs)"
   
  (mapc (lambda (frame)
  (condition-case err
      ;; we must catch errors here, because
      ;; otherwise Emacs would clear menu-bar-update-hook
      ;; which would be not good at all.
       
 
	  (set-mode-specific-theme frame 'force)
	    
    (error nil)
    )) (frame-list))
  t
  ) 
(defun aquamacs-get-theme-snapshot ()

  ;; (set-difference (frame-parameters (selected-frame))
  ;; 			      (append '((user-position) (visibility) (top) (left) (width) (height)) 
  ;; 				      frame-parameters-to-exclude)
  ;; 			      :key 'car)
  (list 
   (cons 'color-theme `(color-theme-snapshot
			;; alist of frame parameters
			,(color-theme-get-params)
			;; alist of variables
			,(color-theme-get-vars)
			;; remaining elements of snapshot: face specs
			,@(color-theme-get-face-definitions)))
   (cons 'font (frame-parameter nil 'font))
   (cons 'tool-bar-lines (frame-parameter nil 'tool-bar-lines))
   )
  )

;; (defun aquamacs-set-theme-as-default () 
;;   "Activate current frame settings (theme) as default. Sets default-frame-alist."
;;   (interactive)
;; 					; need to find out if frame
;;   (let ((frte frame-parameters-to-exclude)) ; make backup

;;     (setq frame-parameters-to-exclude 
;; 	  (append '((user-position) (visibility)  (top) (left) (width) (height)) frame-parameters-to-exclude))

;;     (set-all-frame-alist-parameters-from-frame
;;      (if (special-display-p (buffer-name (current-buffer)))
;; 	 'special-display-frame-alist
;;        'default-frame-alist)
;;      )
;;     (setq frame-parameters-to-exclude frte) ; restore old value
;;     )
;; 					; (setq initial-frame-alist default-frame-alist)  
;;   (message (concat "Theme has been set as default for all new " (if (special-display-p (buffer-name (current-buffer)))
;; 								    "special, internal frames"
;; 								  "normal frames.")))
;;   )

(defun aquamacs-set-theme-as-default () 
  "Activate current frame settings (theme) as default. 
Sets default-frame-alist. (Aquamacs)"
  (interactive)
  (aquamacs-set-theme-as-mode-default 'default))

(defun aquamacs-set-theme-as-mode-default (&optional mode) 
  (interactive)
  "Activate current theme as default for a given mode.
(Aquamacs)"
  (setq mode (or mode major-mode))
  (customize-set-variable 'aquamacs-mode-specific-default-themes
			  (cons (cons mode (aquamacs-get-theme-snapshot)) 
				(assq-delete-all mode 
						 aquamacs-mode-specific-default-themes)))
    
  ;; We need to set default-frame-alist so that frame-notice-user-settings
  ;; doesn't show anything else, and so that frames are created
  ;; (quickly) with the right parameters

  ;; (when (eq mode 'default)

;;     (let* ( 
;; 	   (theme-parms 
;; 	    (cdr (assq mode aquamacs-mode-specific-default-themes)))
;; 	   (frame-parms 
;; 	    (append 
;; 	     (car (cdr (cdr (assq 'color-theme theme-parms))))
;; 	     (assq-delete-all 'color-theme theme-parms))))
;;       ;; needs to be saved to customization file
       
;;       (customize-set-variable 'default-frame-alist frame-parms)
       
;;     )
;;     )
   

  (message (format "Theme has been set as default for %s. %s" (if (eq mode 'default) "all frames" mode)
		   (if aquamacs-auto-frame-parameters-flag
		       ""
		     "Note: aquamacs-auto-frame-parameters-flag is nil - hence functionality is off!"
		     ))))

; 

(defun set-to-custom-standard-value (symbol)
  (customize-set-variable symbol 
			  (eval (car
			   (get symbol 'standard-value))))
  )

(defun aquamacs-delete-themes ()
  "Deletes all themes (mode-specific and the default theme)"
  (interactive)
  (set-to-custom-standard-value 'aquamacs-mode-specific-default-themes)
  (set-to-custom-standard-value 'aquamacs-buffer-specific-frame-themes)
  (message "All themes reset to defaults. Add new ones or use customize to 
modify them. Save Options to store setting.")
  )

(defun aquamacs-delete-one-mode-specific-theme ()
  "Deletes mode-specific themes for current major mode"
  (interactive)
  (customize-set-variable  'aquamacs-mode-specific-default-themes 
			   (assq-delete-all major-mode
					    aquamacs-mode-specific-default-themes))
  (message "Mode-specific theme removed.")
  )

(defun aquamacs-updated-major-mode ()
"Returns the major mode of the selected window of the frame
for which the menu is being updated."
  (with-current-buffer (window-buffer
   (frame-selected-window menu-updating-frame))
    major-mode)
)




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
    (mapc (lambda (th) 
	 
	    (if (cdr th)   
		(add-to-list 'newlist  
			     (cons (car th)  
				   (filter-font-from-alist (cdr th))))))
	  aquamacs-mode-specific-default-themes) 
    (setq aquamacs-mode-specific-default-themes newlist))  
  )


(defun aquamacs-mode-specific-themes-setup ()
  "Installs Aquamacs mode specific themes."
					; filters all missing fonts from specifications, so we don't show
					; stupid error messages
					; especially necessary during 0.9.1 -> 0.9.2 transition, because
					; scalable fonts have different names now


  (defvar aquamacs-frame-theme-menu (make-sparse-keymap "Frame Appearance Themes"))

(defcustom aquamacs-buffer-specific-frame-themes
    (filter-fonts '( 
		    ("*Help*" (background-color . "lightblue")
		     (right-fringe . 1) (left-fringe . 1)
		     (toolbar-lines . 0))
		    ("*Messages*" (background-color . "light goldenrod")
		     (toolbar-lines . 0))
		    )) 
    "Association list to set buffer-specific themes. Each element 
is a list of elements of the form (buffer-name theme), where
THEME is an association list giving frame parameters as
in default-frame-alist or (frame-parameters). The frame parameters are set
whenever the buffer BUFFER-NAME is activated. BUFFER-NAME has to be a 
string. Parameters set here override parameters set in 
`aquamacs-mode-specific-default-themes'.
 
Note that when a major mode is changed, frames are automatically
parametrized. Parameters in `default-frame-alist' and 
`special-display-frame-alist' serve as defaults which are 
overruled by a setting in this list if there is an entry
for the current major mode. To turn off this behavior, see
`aquamacs-auto-frame-parameters-flag'.
"
    :type '(repeat (cons :format "%v"
			 (symbol :tag "Mode-name")
			 (repeat (cons :format "%v"
				       (symbol :tag "Frame-Parameter")
				       (sexp :tag "Value")))))
    :group 'Aquamacs
    )

(defcustom aquamacs-mode-specific-default-themes
  (filter-fonts '((help-mode (tool-bar-lines . 0) (fit-frame . t))
		  (text-mode  (font . "fontset-lucida13")) 
		  (change-log-mode  (font . "fontset-lucida13"))
		  (tex-mode  (font . "fontset-lucida13"))
		  (outline-mode  (font . "fontset-lucida13"))
		  (paragraph-indent-text-mode  (font . "fontset-lucida13"))
		  (speedbar-mode (minibuffer-auto-raise . nil))
		  (fundamental-mode (tool-bar-lines . 0))
		  (custom-mode (tool-bar-lines . 0) (fit-frame . t) 
			       (background-color . "light goldenrod"))
		  ))
  "Association list to set mode-specific themes. Each element 
is a list of elements of the form (mode-name theme), where
THEME is an association list giving frame parameters as
in default-frame-alist or (frame-parameters). The parameters are set
whenever the mode MODE-NAME is activated. 
Note that when a major mode is changed, frames are automatically
parametrized. Parameters in ``default-frame-alist'' and 
``special-display-frame-alist'' serve as defaults which are 
overruled by a setting in this list if there is an entry
for the current major mode. To turn off this behavior, see
``aquamacs-auto-frame-parameters-flag''.
"
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Mode-name")
		       (repeat (cons :format "%v"
				     (symbol :tag "Frame-Parameter")
				     (sexp :tag "Value")))))
  :group 'Aquamacs
  )

(defcustom aquamacs-auto-frame-parameters-flag t
    "When non-nil, frames are automatically
parametrized when a major mode is changed. 
Parameters in ``default-frame-alist'' and 
``special-display-frame-alist'' serve as defaults which are 
overruled by a setting in ``aquamacs-mode-specific-default-themes'' 
if there is an entry for the current major mode."
    :type 'boolean
    :group 'Aquamacs
    )

(defadvice mouse-set-font
  (after show-font-warning () activate)
  "The frame font is set for the current frame only if 
`aquamacs-auto-frame-parameters-flag' is non-nil. Otherwise,
the frame font is set as a default in `default-frame-alist'."

  (if aquamacs-auto-frame-parameters-flag
      (message "Font set for current frame only. Use functions in 
Frame Appearance Themes to make the setting stick.")
    (progn
      (modify-all-frames-parameters `((font . ,(frame-parameter nil 'font))))
      (message "Font set for this and all future frames.")
      )
    )
  )

(add-hook 'after-init-hook 'filter-missing-fonts t) 

(add-hook 'after-init-hook
	  'make-help-mode-use-frame-fitting
	  'append) ;; move to the end: after loading customizations
	
	
  (add-hook 'after-change-major-mode-hook	
	    'set-mode-theme-after-change-major-mode
	    )
  (add-hook 'menu-bar-update-hook 'update-mode-theme)
  (define-key-after aquamacs-frame-theme-menu [menu-delete-one-theme]
    '(menu-item (format "Remove Theme for %s" 
			(or 
			 (modename-to-string (aquamacs-updated-major-mode)) 
			 "current mode"))   
		aquamacs-delete-one-mode-specific-theme 
		:enable (and aquamacs-auto-frame-parameters-flag
			     (menu-bar-menu-frame-live-and-visible-p)
			     (assq (aquamacs-updated-major-mode) 
				   aquamacs-mode-specific-default-themes))
		:help "Removes a mode-specific theme."))
  (define-key-after aquamacs-frame-theme-menu [menu-delete-themes]
    '(menu-item  "Reset All Themes"     aquamacs-delete-themes 
		 :help "Resets all themes to the default."
		 :enable aquamacs-auto-frame-parameters-flag))

  (define-key aquamacs-frame-theme-menu [menu-set-theme-as-default]
    '(menu-item  "Use Current Theme as Default"     aquamacs-set-theme-as-default
		 :enable  (and aquamacs-auto-frame-parameters-flag
			       (menu-bar-menu-frame-live-and-visible-p))
		 :help ""))

  (define-key aquamacs-frame-theme-menu [menu-set-theme-as-mode-default]
    '(menu-item (format "Use Current Theme for %s" (or (modename-to-string (aquamacs-updated-major-mode)) "Current Mode"))
		aquamacs-set-theme-as-mode-default 
		:help "Set the current frame parameters as default 
for all frames with the current major-mode."
		:enable   (and aquamacs-auto-frame-parameters-flag
			       (menu-bar-menu-frame-live-and-visible-p))
		 	  
		)) 

(defun modename-to-string (modename)
  (capitalize
   (replace-regexp-in-string "-" " " (symbol-name modename))))

(defvar apptheme-mode-menu nil)

(setq apptheme-mode-menu (make-sparse-keymap "Set Mode")) 
(mapc
   (lambda (pair)
     (let ((modename (car pair)))
     (when (fboundp modename)
       (define-key ;;-after doesn't work with after- why?>? 
	 apptheme-mode-menu 
	 (vector (intern (concat "set-theme-of-" (symbol-name modename))))
	 `(menu-item  
	   ,(modename-to-string modename)
	   ,(eval 
	     (list 'lambda '() '(interactive)
		   (list 'set-mode-specific-theme nil t `(quote ,modename))
		   ))
	   :help "Apply frame theme of some major mode."
	   )))))
      
   (reverse (sort (copy-list aquamacs-mode-specific-default-themes)
		  (lambda (a b) (string< 
				 (upcase (symbol-name (car a))) 
				 (upcase (symbol-name (car b)))))))
   )
  (define-key-after aquamacs-frame-theme-menu [set-mode]
    (list 'menu-item "Apply Theme from some Mode" apptheme-mode-menu
	  :help "Apply frame theme of some major mode.")
    'menu-set-theme-as-default)



  (define-key aquamacs-frame-theme-menu [menu-auto-frame-parameters]
    (menu-bar-make-toggle 
     toggle-aquamacs-auto-frame-parameters-flag
     aquamacs-auto-frame-parameters-flag
     "Frame Appearance Themes"
     "Frame Appearance Themes %s"
     "Always set the frame parameters according to major-mode."))
      

  (define-key-after menu-bar-options-menu [aquamacs-frame-themes]

    (list 'menu-item "Frame Appearance Themes" aquamacs-frame-theme-menu 
	  :help "Set themes for frames depending on major mode in buffer")

    'aquamacs-color-theme-select)


  ;; advise frame-notice-user-settings (from frame.el)
  ;; to integrate the mode-specific frame settings
  ;; which supersede the default-frame-alist, but not
  ;; the initial-frame-alist

  ;; for some reason, we can't byte-compile this.

  (defadvice frame-notice-user-settings 
    (around aquamacs-respect-mode-defaults () activate)
    (if aquamacs-auto-frame-parameters-flag

	;;  (let ((default-frame-alist  
	;; 	      (aquamacs-combined-mode-specific-settings 
	;; 	       default-frame-alist
	;; 	       (get-mode-specific-theme major-mode))))
	(progn
	  (set-mode-specific-theme nil 'force) 
	  ;; apply initial-frame-alist and default-frame-alist
	  (let ((default-frame-alist nil))
	  ad-do-it)
	  )
      ;; else
      ad-do-it
      )
    ;; workaround for an Emacs bug
    (let ((vsb (frame-parameter nil  'vertical-scroll-bars)))
      (modify-frame-parameters nil '((vertical-scroll-bars . nil)))
      (modify-frame-parameters nil `((vertical-scroll-bars . ,vsb)))
      )
    )



  ;; color themes
  
  ;; is needed 
  (require 'color-theme)
  (setq color-theme-is-global nil)
  (setq color-theme-target-frame nil)
  (defadvice color-theme-install 
    (around for-other-frame (&rest args) activate)
 
    (if (and
	 (eq major-mode 'color-theme-mode)
	 color-theme-target-frame)
	(select-frame color-theme-target-frame)
      )
    ad-do-it 
    )


  )



(defun aquamacs-color-theme-select ()
  (interactive) 
 
  (setq color-theme-target-frame (selected-frame))
   
  (let ((one-buffer-one-frame-force t))	
    ;; always open in new frame
    ;; because we've redefined bury->kill-buffer-and window 
    ;; in color-theme
    (color-theme-select)
    )
 
  )

(provide 'aquamacs-mode-specific-themes)