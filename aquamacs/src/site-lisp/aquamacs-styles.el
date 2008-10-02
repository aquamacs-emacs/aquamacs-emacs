;; To do here:

;; use remapped default face when acquiring a new style for a mode


;; let's give up on color themes.

;; they add too much functionality that's not needed
;; and at the same time, they don't work with faces that can be set per buffer.

;; what we really need is a choice of foreground and background color.
;; this could and should be done via the font panel.  that extension is probably easy.
;; we only modify the default face.

;; then we change the face that the font panel applies to. that's
;; easy (mac-handle-font-selection).

;; then, we shift all aquamacs-styles stuff over to just storing a single face: default. 
;; we only apply very few frame parameters such as tool-bar-lines.
  
;; it appears that just changing the default face does not 
;; do enough: background of non-text can't be changed.
;; this is possibly easy to patch!

;; (setq face-remapping-alist nil)
;; (set (make-local-variable 'face-remapping-alist) '((default aquamacs-variable-width)))

;; OK, maybe use the current solution for a while and filter "font" from the color themes as frame parameters, but leave the other ones.

;; we do need to change the font seting dialog to just set the remapped face though (or create a remapped one), which should be easy.



;; - make color theme stick to buffer only by using face remapping on ALL faces from the color theme - not just the main font
;; - use same technique after color theme is selected manually
;; - make font selector change the remapped default font and not the default font for the frame.

;; - color themes should convert frame parms to face parsm as much as possible:
;;   bg / fg colors, font
;; - color-themes: do not install any other frame parameters

;; aq-styles should set font face,but then merge it w/ the face from color themes
;; maybe the face-set-spec in color-themes does that already

;; font setter: change the remapped face where appropriate
;; styles: the snapshot function may need to be adapted







;; aquamacs mode specific styles

;; this package realizes mode-specific styles in Aquamacs.
;; it is not complete - right now this files just
;; serves as a collection of function that interact with
;; things from osx_defaults

;; Call aquamacs-styles-setup after loading to install.

;; Filename: aquamacs-frame-setup.el
;; Description: Emacs init file for use with libraries from Drew Adams
;; Author: David Reitter
;; Maintainer: David Reitter
;; Keywords: aquamacs
 

;; Last change: $Id: aquamacs-styles.el,v 1.40 2008/10/02 17:58:59 davidswelt Exp $

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

;; Copyright (C) 2005, 2006, David Reitter, all rights reserved.

 
; (require 'cl)
(require 'aquamacs-cl)
 
(defmacro save-frame-size (frame &rest body)
  "Restore pixel size of selected frame after executing body."
  `(let ((old-frame-pixel-width (frame-pixel-width ,frame))
	(old-frame-pixel-height (frame-pixel-height ,frame))
	(w-offset 
	 (- (frame-pixel-width ,frame) 
	    (smart-fp--char-to-pixel-width (frame-width ,frame) 
					   ,frame)))
	(h-offset 
	 (- (frame-pixel-height ,frame) 
	    (smart-fp--char-to-pixel-height (frame-height 
					     ,frame) ,frame))))
    
    (let ((ret-val (progn ,@body)))
      (when (not (equal old-frame-pixel-height 
		      (frame-pixel-height ,frame)))
	(set-frame-height 
	 ,frame
	 (smart-fp--pixel-to-char-height
	  (- old-frame-pixel-height h-offset ) ,frame)))
      (when (not (equal old-frame-pixel-width 
			(frame-pixel-width ,frame)))
	(set-frame-width 
	 ,frame
	 (smart-fp--pixel-to-char-width
	  (- old-frame-pixel-width w-offset ) ,frame)))
      ret-val)))

 
; update the help-mode specification with a fit-frame
; append it, so the user's choice has priority
(defun 	make-help-mode-not-use-frame-fitting ()
    (if (assq 'fit-frame 
		  (assq 'help-mode aquamacs-default-styles)
		  ) ;; unless it's already set

	(assq-set 
	     'help-mode
	     (assq-delete-all
	      'fit-frame  
	      (cdr (assq 'help-mode aquamacs-default-styles)))
	     'aquamacs-default-styles)))

 
(defun aquamacs-combined-mode-specific-settings (default-alist style)
  "Return the frame parameter set resulting from two alists.
Parameters from DEFAULT-ALIST receive priority over those from STYLE.
If `aquamacs-styles-mode' is nil, returns nil."
  ;; style has priority over default-frame-alist
  (setq style (append (assq-subtract  default-alist style 'ign)
		      style))
  
  ;; delete a few things as we don't want them here
  (mapc
   (lambda (e) (setq style (assq-delete-all e style)))
   '(user-position menu-bar-lines top height left width scroll-bar-width))
  style)

;; (aquamacs-style-relevant-buffer)
(defun aquamacs-style-relevant-buffer (&optional frame)
  "For a given frame, determine the main window to be used for the style.
Return nil if no style should be applied due lack of decision-making
ability. The following rules are followed:

- Ignore minibuffers if there is only one window, use that 
- if there are several windows and if all windows show buffers
  with the same major mode, use the first 
- otherwise, return nil."

(let ((l (window-list frame 'no-minibuf (frame-first-window frame))))
  (if (cdr-safe l) ;; more than one window
      (let (mm
	    (ret (window-buffer (car l))))
	(mapcar (lambda (w) 
		(let ((m (with-current-buffer (window-buffer w) major-mode)))
		  (if mm
		      (unless (eq mm m) ;; not the same major mode?
			(setq ret nil))
		      (setq mm m))))
		l)
	ret)
    (window-buffer (car l)))))
  
 
(require 'smart-frame-positioning)
;; (frame-parameter nil 'font)
(defun aquamacs-set-style (&optional frame force for-mode)
  "Sets the mode-specific style (frame parameters) for FRAME
 (selected frame if nil), unless it is already set (or
FORCE is non-nil). Use style of major mode FOR-MODE if given."
 
  (when (or force aquamacs-styles-mode)

    (unless frame (setq frame (selected-frame) ))

    (if (frame-live-p frame)  

	(condition-case err ;; (otherwise, Emacs hangs)  
      
	    ;; frame-configured-for-buffer stores for which buffer  
	    ;; and which major-mode the frame configuration  
	    ;; is for, so we don't have to apply the style again. 
	    ;; This is also very important because setting the style in itself  
	    ;; will cause another menu-bar-update-hook call, so we can end up 
	    ;; with this function called again and again...  

	    (let ((buffer (aquamacs-style-relevant-buffer   frame)))
	    
	      (when (or 
		   (and force (or buffer for-mode))
		   (and buffer
			(not (equal (frame-parameter frame 
				     'frame-configured-for-buffer)
					;(cons  
				    buffer 
					; major-mode)  
			       ))))

		  (save-excursion 

		    (set-buffer buffer)
		    (let* ((style-face-id 
			    (intern (format "default-%s" 
					    (or for-mode 
						(if (aquamacs-get-buffer-style (buffer-name))
						    (format "%s---%s" (buffer-name) major-mode)
						  major-mode)))))
			   (style (aquamacs-combined-mode-specific-settings 
				   (if (special-display-p (buffer-name)) 
				       special-display-frame-alist 
				     default-frame-alist
				     )
				   (if for-mode
				       (aquamacs-get-style for-mode)
				     (append
				      (aquamacs-get-buffer-style (buffer-name))
				      (aquamacs-get-style major-mode)))))
			   ;; read out color-theme		 
			   ( color-theme (cdr (assq 'color-theme style)))
			   (style (assq-delete-all 'color-theme style)
				  ))  
		      ;; make sure we don't move the whole frame -  
		      ;; it is already shown on screen, and  
		      ;; the position is determined by `smart-frame-positioning',  
		      ;; that is per file name and according to the 'smart' heuristic   
		      
		      ;; ensure that setting the new frame parameters doesn't resize
		      ;; the frame significantly:
		      ;; change width / height to adapt to new font size
		      (custom-declare-face style-face-id '((t :inherit default)) "mode-specific face defined by `aquamacs-styles-mode'.
The `default' face is remapped (in the appropriate buffers) to this face.")  
		      
		      (save-frame-size 
		       frame
			
			;; do not set frame parameters if they will be overridden by the later color theme
			;; this prevents flicker 
			(let ((col-theme-parms
			       (condition-case nil 
				   (color-theme-frame-params 
				    (color-theme-canonic color-theme)) (error nil))))
			  (mapc (lambda (x)
				  (setq style (assq-delete-all (car x) style)))
				col-theme-parms))

			(when (assq 'font style)
			  (unless (facep style-face-id) ;; do not override user's existing face choices
			    (set-face-font style-face-id (cdr (assq 'font style)) nil))   ; FIXME: in all frames or just here?
			  (setq style (assq-delete-all 'font style))
			  ;; make sure we're remapping
			  (make-local-variable 'face-remapping-alist)
			  (assq-set 'default style-face-id 'face-remapping-alist))

			(modify-frame-parameters frame 
			 (cons (cons 'frame-configured-for-buffer buffer) 
			       style))
		
			(save-window-excursion
			  (select-frame frame)
			  ;; color-style-target-frame seems deprecated
			 
			  (if (and (functionp (car-safe color-theme))
				   (memq (car-safe color-theme) color-themes)
				   (not (cdr-safe color-theme)))
			      (funcall (car color-theme)) 
			    ;; just install the color style directly
			    (color-theme-install color-theme))))
			 
		      ;; do not move the frame: this can cause the frame to shrink more and more
		      ;; e.g. when switching between tabs, and it also makes the frame jump around
		      ;; especially on two-screen setups
		      ;; (if (and (fboundp 'smart-move-frame-inside-screen))
		      ;; 			     (smart-move-frame-inside-screen frame)
		      ;; 			  )
			)
		    (if window-configuration-change-hook
			(let ((selframe (selected-frame)))
			  (select-frame frame)
			  (run-hooks 'window-configuration-change-hook)
			  (select-frame selframe)))
		    
		    )))
	  (error (print err))))))


(defmacro mac-event-ae (event)
  `(nth 2 ,event))
(defmacro mac-event-spec (event)
  `(nth 1 ,event))
 


; (aquamacs-get-buffer-style "*Help*")

(defun aquamacs-get-buffer-style (bufname) 
  (if aquamacs-styles-mode
	   (cdr (assq-string-equal bufname 
				   aquamacs-buffer-default-styles))
    nil))


(defun aquamacs-get-style (mode) 
  (or (cdr (assq mode aquamacs-default-styles)) 
      ;;(progn (print "resorting to default") nil)
      (cdr (assq 'default aquamacs-default-styles))
					;(progn (print "nothing found") nil)
      ))

(defun set-mode-style-after-change-major-mode ()       			      
  ;; delete the configuration cache parameter
  ;; sometimes, this will be called for the buffer, but before
  ;; the target frame has been switched to the new buffer.
  ;; that's bad luck then. 
  
  (when aquamacs-styles-mode
    (mapc 
     (lambda (f) 
           
       ;; update the style 
       (aquamacs-set-style f t)
       )  
     ;; list
     (find-all-frames-internal (current-buffer)))))



(defun set-mode-style-after-make-frame (frame) 
  ;; only if we have a window and a buffer here
  (if (and aquamacs-styles-mode
	   (frame-first-window) (window-buffer (frame-first-window frame)))
      ;; make sure we activate the right buffer
      ;; and that we don't change the selected frame
      (save-excursion
	(set-buffer (window-buffer (frame-first-window frame)))
	(aquamacs-set-style frame))))

;; this is needed for newly created frames, because the after-mode-change
;; hook can get run before the frame is displayed.
(add-hook 'after-make-frame-functions	
	  'set-mode-style-after-make-frame
	  )

(defun aquamacs-update-mode-style ()
  "Update the style (colors, font) of the selected frame 
to be appropriate for its first buffer"
   
  (condition-case err
      ;; we must catch errors here, because
      ;; otherwise Emacs would clear menu-bar-update-hook
      ;; which would be not good at all.
      (progn 
	(unless
	    (minibuffer-window-active-p (selected-window))
	  ;;(make-variable-frame-local 'last-major-mode-style-in-this-frame)
	  ;;(setq last-major-mode-style-in-this-frame major-mode)
	  ;; can't call ->crash
	  (aquamacs-set-style)
	  )
	
	)

    (error nil)
    )
  t
  )
(defun aquamacs-update-mode-styles-everywhere ()
  "Update the styles (colors, font) of all frames
to be appropriate for its first buffer. (Aquamacs)"
   
  (when aquamacs-styles-mode
    (mapc (lambda (frame)
	    (condition-case err
		;; we must catch errors here, because
		;; otherwise Emacs would clear menu-bar-update-hook
		;; which would be not good at all.
		(aquamacs-set-style frame 'force)
	      (error nil)
	      )) (frame-list))
    t))
(defun aquamacs-get-style-snapshot ()
 

;; retrieve theme

 
  (list 
   (cons 'color-theme (let ((theme 
			     `(color-theme-snapshot
			       ;; alist of frame parameters
			       ,(color-theme-get-params)
			       ;; alist of variables
			       ,(color-theme-get-vars)
			       ;; remaining elements of snapshot: face specs
			       ,@(color-theme-get-face-definitions))))
			;; find out if this is any different from the theme that was set  


;; (let ((theme-name (frame-parameter nil 'color-theme-name)))
;;   (when (and theme-name 
;; 	     (not (eq theme-name 'color-theme-snapshot))
;; 	     (functionp theme-name))
;;     (let ((shelved-color-theme nil))
;;       ;; too bad we can't do it with let...
;;       (fset 'color-theme-backup 'color-theme-install)
;;       (unwind-protect 
;; 	  (progn
;; 	    (fset 'color-theme-install
;; 		  (lambda (theme) (setq shelved-color-theme theme)))
;; 	    (funcall theme-name)
;; 	    )
;; 	(fset 'color-theme-install 'color-theme-backup))
;;       (print shelved-color-theme))
;;     ;; compare theme
;;     (print (eq shelved-color-theme theme))
;;     theme
;;     )))

			;; - not implemented -
			theme
			))
   (cons 'font (frame-parameter nil 'font))
   (cons 'tool-bar-lines (frame-parameter nil 'tool-bar-lines))))

;; (defun aquamacs-set-style-as-default () 
;;   "Activate current frame settings (style) as default. Sets default-frame-alist."
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
;;   (message (concat "Style has been set as default for all new " (if (special-display-p (buffer-name (current-buffer)))
;; 								    "special, internal frames"
;; 								  "normal frames.")))
;;   )

(defun aquamacs-set-style-as-default () 
  "Activate current frame settings (style) as default. 
Sets default-frame-alist. (Aquamacs)"
  (interactive)

;; maybe delete mode-specific frames?
  (when 
      (let ((existing-styles  aquamacs-default-styles))
	(setq existing-styles (assq-delete-all 'default existing-styles)) 
	(and existing-styles
	     (yes-or-no-p 
	      (format "Mode-specific styles are in-place for the following modes: %s. Do you want to delete all of them so the default style is applied to frames with buffers in those modes?"
		      (apply 'concat  
			     (let ((l (mapcar (lambda (x)
						(concat
						 (symbol-name (car x)) ", ")
						)
					      existing-styles)))
			       (if (nthcdr 5 l)
				   (setcdr (nthcdr 5 l) (list "(...)")))
			       l))))))
    (aquamacs-delete-styles)) 
  (aquamacs-set-style-as-mode-default 'default))

(defun aquamacs-set-style-as-mode-default (&optional mode) 
  (interactive)
  "Activate current style as default for a given mode."
  (setq mode (or mode major-mode))
  (customize-set-variable 'aquamacs-default-styles
			  (cons (cons mode (aquamacs-get-style-snapshot)) 
				(assq-delete-all mode 
						 aquamacs-default-styles)))
    
  ;; We need to set default-frame-alist so that frame-notice-user-settings
  ;; doesn't show anything else, and so that frames are created
  ;; (quickly) with the right parameters

  ;; (when (eq mode 'default)

;;     (let* ( 
;; 	   (style-parms 
;; 	    (cdr (assq mode aquamacs-default-styles)))
;; 	   (frame-parms 
;; 	    (append 
;; 	     (car (cdr (cdr (assq 'color-theme style-parms))))
;; 	     (assq-delete-all 'color-theme style-parms))))
;;       ;; needs to be saved to customization file
       
;;       (customize-set-variable 'default-frame-alist frame-parms)
       
;;     )
;;     )
   

  (message (format "Style has been set as default for %s. %s" 
		   (if (eq mode 'default) "all frames" mode)
		   (if aquamacs-styles-mode
		       ""
		     (aquamacs-styles-mode 1)
		     "\nNote: Auto Styles have been turned on."))))


(defun set-to-custom-standard-value (symbol)
  (customize-set-variable symbol 
			  (eval (car
			   (get symbol 'standard-value)))))

(defun aquamacs-delete-styles ()
  "Deletes all styles (mode-specific and the default style)"
  (interactive)
  (customize-set-variable 'aquamacs-default-styles nil)
  (customize-set-variable 'aquamacs-buffer-default-styles nil)
  (message "All styles deleted. Use Save Options before restart to retain setting.")
  )

(defun aquamacs-reset-styles ()
  "Resets all styles (mode-specific and the default style)"
  (interactive)
  (set-to-custom-standard-value 'aquamacs-default-styles)
  (set-to-custom-standard-value 'aquamacs-buffer-default-styles)
; this doesn't work as expected...
;  (mapc
;   (lambda (frame) (aquamacs-set-style frame 'force))
;   (frame-list))
  (message "All styles reset to defaults. Add new ones or use customize to 
modify them."))

(defun aquamacs-clear-styles ()
  "Resets all styles (mode-specific and the default style)"
  (interactive)
  (setq aquamacs-default-styles nil)
  (setq aquamacs-buffer-default-styles nil)
  (message "All styles cleared. Add new ones or use customize to 
modify them."))

(defun aquamacs-delete-one-style ()
  "Deletes mode-specific styles for current major mode"
  (interactive)
  (customize-set-variable  'aquamacs-default-styles 
			   (assq-delete-all major-mode
					    aquamacs-default-styles))
  (message "Mode-specific style removed. Use Save Options before restart to retain setting.")
  )

(defun aquamacs-updated-major-mode ()
"Returns the major mode of the selected window of the frame
for which the menu is being updated."
  (with-current-buffer (window-buffer
   (frame-selected-window menu-updating-frame))
    major-mode)
)




;; (defun font-exists-p (fontorfontset)
;; "Does not work as intended when font not loaded, e.g. after startup."
;; (or
;;   (condition-case nil
;;        (font-info fontorfontset)
;;     (error nil))
;;   (condition-case nil
;;        (fontset-info fontorfontset)
       
;;    (error nil)
;;   )
;;   ))

;; (defun filter-font-from-alist (alist)
;; "Filters all missing fonts. Currently disabled, because `font-exists-p'
;; does not work properly if the fonts aren't loaded (e.g. after startup)."
;; (if (and (assq 'font  alist)
;; 	 (not (font-exists-p (cdr (assq 'font  alist)))) 
;; 	 )
;;   (progn 
;;     (print (format "Warning: Font %s not available." (cdr (assq 'font  alist))))    (assq-delete-all 'font alist) ;; return
;; alist
;;     )
;;   alist)
;; )


;; (defun filter-missing-fonts ()
;; "Filters all missing fonts. Currently disabled, because `font-exists-p'
;; does not work properly if the fonts aren't loaded (e.g. after startup)."
;;   (setq default-frame-alist (filter-font-from-alist default-frame-alist))
;;   (setq special-display-frame-alist (filter-font-from-alist special-display-frame-alist))

;;   (let ((newlist))
;;     (mapc (lambda (th) 
	 
;; 	    (if (cdr th)   
;; 		(add-to-list 'newlist  
;; 			     (cons (car th)  
;; 				   (filter-font-from-alist (cdr th))))))
;; 	  aquamacs-default-styles) 
;;     (setq aquamacs-default-styles newlist))  
;;   )



(defvar aquamacs-frame-style-menu 
  (make-sparse-keymap "Mode Styles"))


(defvar appstyle-mode-menu nil)

(defun aquamacs-apply-style-for-mode (&optional ignored modename)
  (interactive)
  (aquamacs-set-style nil t (or modename last-command-event)))
 

(defun aquamacs-update-apply-style-for-mode-menu ()
  (setq appstyle-mode-menu
	(aquamacs-define-mode-menu-1 

	 (reverse (sort (aq-copy-list 
			 (mapcar 'car aquamacs-default-styles))
			(lambda (a b) (string< 
				       (upcase (symbol-name a)) 
				       (upcase (symbol-name b))))))

	 (make-sparse-keymap "Set Mode") 
	 'aquamacs-apply-style-for-mode
	 "Apply frame style assigned to %s." 
	 '(menu-bar-non-minibuffer-window-p)
	 ))
  (define-key-after aquamacs-frame-style-menu [set-mode]
    `(menu-item "Apply Style of Some Mode" ,appstyle-mode-menu
		:help "Apply frame style of some major mode."
;; don't do this check (speed) - higher-level menu is disabled
;;		:enable (menu-bar-menu-frame-live-and-visible-p)
)
    'menu-set-style-as-default))


(defun set-aquamacs-default-styles (variable value)
  "Like `custom-set-default', but for `aquamacs-default-styles'."
  (custom-set-default variable value)
  (if (fboundp 'aquamacs-update-apply-style-for-mode-menu)
      (aquamacs-update-apply-style-for-mode-menu)))


(defcustom aquamacs-buffer-default-styles
    (filter-fonts '( 
		    ("*Help*" (background-color . "lightsteelblue")
		     (foreground-color . "black")
		     (right-fringe . 1) (left-fringe . 1)
		     (toolbar-lines . 0))
		    ("*Messages*" (background-color . "khaki") ;; "light goldenrod")
		     (foreground-color . "black")
		     (toolbar-lines . 0))
		    )) 
    "Association list to set buffer styles. Each element 
is a list of elements of the form (buffer-name style), where
STYLE is an association list giving frame parameters as
in `default-frame-alist' or `frame-parameters'. The frame parameters are set
whenever the buffer BUFFER-NAME is activated. BUFFER-NAME has to be a 
string. Parameters set here override parameters set in 
`aquamacs-default-styles'.
 
Note that when a major mode is changed, frames are automatically
parametrized. Parameters in `default-frame-alist' and
`special-display-frame-alist' serve as defaults which are
overruled by a setting in this list if there is an entry for the
current major mode. To turn off this behavior, see
`aquamacs-styles-mode'.
"
;; To Do: add a setter function maybe for menu?
    :type '(repeat (cons :format "%v"
			 (symbol :tag "Mode-name")
			 (repeat (cons :format "%v"
				       (symbol :tag "Frame-Parameter")
				       (sexp :tag "Value")))))
    :group 'Aquamacs
    )

;; create some essential fontsets
(when (fboundp 'create-aquamacs-fontset)
  (create-aquamacs-fontset "apple" "lucida grande*" "medium" "r" "normal" '(11 12 13 14) "lucida")
  (create-aquamacs-fontset "apple" "monaco*" "medium" "r" "normal" '(11 12) "monaco"))

(aquamacs-set-defaults 
 ;; because we fit *Help* frames (fit-frame), we want to specify a
 ;; minimum of 68 characters. Otherwise we'll fit, and as soon as a
 ;; new help page is displayed, text gets wrapped.
 '((create-frame-min-width 68)))


(defcustom aquamacs-default-styles
  (filter-fonts '((help-mode (tool-bar-lines . 0))
		  ;; do not fit the frame for *Help*
		  ;; because the frame will not re-adjust if the buffer
		  ;; changes. Perhaps some auto-fit-frame?
		  (text-mode  (font . "fontset-lucida13")) 
		  (change-log-mode  (font . "fontset-lucida13"))
		  (tex-mode  (font . "fontset-lucida13"))
		  (outline-mode  (font . "fontset-lucida13"))
		  (paragraph-indent-text-mode  (font . "fontset-lucida13"))
		  (speedbar-mode (minibuffer-auto-raise . nil))
		  (custom-mode (tool-bar-lines . 0) (fit-frame . t) 
			       (font . "fontset-monaco11")
			       (foreground-color . "sienna")
			       (background-color . "light goldenrod"))
		  ;; should the default be taken from default-frame-alist?
		  (default (font . "fontset-monaco12")   
			    (right-fringe . 1) (left-fringe . 1))
		  ))
  "Association list to set mode-specific styles. Each element 
is a list of elements of the form (mode-name style), where
STYLE is an association list giving frame parameters as
in default-frame-alist or (frame-parameters). The parameters are set
whenever the mode MODE-NAME is activated. 
Note that when a major mode is changed, frames are automatically
parametrized. Parameters in ``default-frame-alist'' and 
``special-display-frame-alist'' serve as defaults which are 
overruled by a setting in this list if there is an entry
for the current major mode. To turn off this behavior, see
``aquamacs-styles-mode''."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Mode-name")
		       (repeat (cons :format "%v"
				     (symbol :tag "Frame-Parameter")
				     (sexp :tag "Value")))))
  :set 'set-aquamacs-default-styles
  :group 'Aquamacs
  )



(define-minor-mode aquamacs-styles-mode
  "Automatically set frame style according to major mode
This global minor mode will cause frame settings (parameters,
faces, variables) to be set according to a style that is specific
to the major mode of the buffer currently shown in the single
window inside the frame.
`aquamacs-default-styles' specifies styles for major
modes. 

Frames are only configured in this way if there is only one
window visible.  Otherwise, the frame parameters are left as they
are. That means that additional, temporary windows (such as for
the *Completions* buffer) will not alter the style of the frame.

A special style `default' is applied when no mode-specific style
is present. Any style setting will override parameters set 
in `default-frame-alist' or `special-display-frame-alist'.
 
When this mode is turned on, parameters from
`default-frame-alist' are copied to the `default' style.
When it is turned off, parameters are copied back.

This mode is part of Aquamacs Emacs, http://aquamacs.org."
 
 
;; the condition case is because otherwise this won't
;; do it's job. don't know why.
;  (condition-case nil
 ;     (error nil))
:init-value  t
  :group 'Aquamacs
  :global t
  :require 'color-theme)
      

(defun aquamacs-styles-set-default-parameter (param value &optional mode)
  "Sets frame parameter PARAM of `default' frame style."
  (let* ((x (assq (or mode 'default) aquamacs-default-styles))
	 (x2 (cdr-safe x))
	 ;; just in case the entry is malformed (or obsolete, maybe?)
	 (y (assq param (if (listp (cdr-safe x2)) x2 (list x2)))))
    (if y
	(setcdr y value)
      (if x
	  (setcdr x (cons (cons param value) (cdr x)))
	(setq aquamacs-default-styles
	      (cons (list (or mode 'default) (cons param value)) 
		    aquamacs-default-styles))))))
 
(defun aquamacs-styles-set-all-mode-parameters (param value)

  (mapc (lambda (mode) (aquamacs-styles-set-default-parameter param value mode))
	(mapcar 'car-safe aquamacs-default-styles))
  (message "Modified the styles for all modes: %s set to %s." param value))

(defadvice modify-all-frames-parameters
  (after set-parameter-in-default-style (alist) activate)
  (mapc (lambda (x)
	  (aquamacs-styles-set-default-parameter (car-safe x)
						 (cdr-safe x)))
	alist))

(defadvice tool-bar-mode
  (after set-tool-bar-in-default-style () activate)

  (aquamacs-styles-set-default-parameter 'tool-bar-lines
					 (if tool-bar-mode 1 0)))

(defadvice set-fringe-mode
  (after set-fringe-in-default-style () activate)

  (aquamacs-styles-set-default-parameter 
   'right-fringe
   (cdr (assq 'right-fringe default-frame-alist)))
  (aquamacs-styles-set-default-parameter 
   'left-fringe
   (cdr (assq 'left-fringe default-frame-alist))))


(define-key menu-bar-options-menu [mouse-set-font]
  `(menu-item (format "Font for %s...                 "
		      (if (and (boundp 'face-remapping-alist)
			       (assq 'default face-remapping-alist))
			  (capitalize 
			   (replace-regexp-in-string "default-" "" (symbol-name (cdr (assq 'default face-remapping-alist)))))
			(if aquamacs-styles-mode
			    "this Frame's default" "this Frame")))
	      turn-on-mac-font-panel-mode
	      :visible ,(display-multi-font-p)
	      :keys ,(aq-binding 'mac-font-panel-mode)
	      :enable (menu-bar-menu-frame-live-and-visible-p) 
	      :help "Select a font from list of known fonts/fontsets"))


(defadvice mouse-set-font
  (after show-font-warning () activate)
  "The frame font is set for the current frame only if 
`aquamacs-styles-mode' is non-nil. Otherwise,
the frame font is set as a default in `default-frame-alist'."

  (if aquamacs-styles-mode
      (message "Font set for current frame only. Use functions in 
Frame Styles to make the setting stick.")
    (progn
      (modify-all-frames-parameters `((font . ,(frame-parameter nil 'font))))
      (message "Font set for this and all future frames."))))

	
	
  (add-hook 'after-change-major-mode-hook	
	    'set-mode-style-after-change-major-mode
	    )
  (add-hook 'menu-bar-update-hook 'aquamacs-update-mode-style)
  (define-key-after aquamacs-frame-style-menu [menu-delete-one-style]
    '(menu-item (format "Remove Style for %s" 
			(or 
			 (modename-to-string (aquamacs-updated-major-mode)) 
			 "current mode"))   
		aquamacs-delete-one-style 
		:enable (and (menu-bar-menu-frame-live-and-visible-p)
			     (assq (aquamacs-updated-major-mode) 
				   aquamacs-default-styles))
		:help "Removes a mode-specific style."))
  (define-key-after aquamacs-frame-style-menu [menu-reset-styles]
    '(menu-item  "Reset All Styles"     aquamacs-reset-styles 
		 :help "Resets all styles to the default."
		 :enable t))
  (define-key-after aquamacs-frame-style-menu [menu-clear-styles]
    '(menu-item  "Clear All Styles"     aquamacs-clear-styles 
		 :help "Clear all styles."
		 :enable t))


  (define-key aquamacs-frame-style-menu [menu-set-style-as-mode-default]
    '(menu-item (format "Use Current Style for %s" (or (modename-to-string (aquamacs-updated-major-mode)) "Current Mode"))
		aquamacs-set-style-as-mode-default 
		:help "Set the current frame parameters as default 
for all frames with the current major-mode."
		:enable   (menu-bar-menu-frame-live-and-visible-p)
		 	  
		))
  (define-key aquamacs-frame-style-menu [menu-set-style-as-default]
    '(menu-item  "Use Current Style as Default"     aquamacs-set-style-as-default
		 :enable (menu-bar-menu-frame-live-and-visible-p)
		 :help ""))

 

(defun modename-to-string (modename)
  (capitalize
   (replace-regexp-in-string "-" " " (symbol-name modename))))

;; (setq aquamacs-default-styles nil)
(aquamacs-set-defaults '((tool-bar-mode 0)))
;; do not turn it off globally, because that would
;; only modify the default-frame-alist etc. 
;; and needlessly change the current frame.
;; anything necessary will be done by frame notice-user-settings




;; additionally, we should ensure that default-frame-alist
;; is consistent with that. (see aquamacs-frame-setup)
 
;; (setq appstyle-mode-menu (make-sparse-keymap "Set Mode")) 
;; (mapc
;;    (lambda (pair)
;;      (let ((modename (car pair)))
;;      (when (fboundp modename)
;;        (define-key ;;-after doesn't work with after- why?>? 
;; 	 appstyle-mode-menu 
;; 	 (vector (intern (concat "set-style-of-" (symbol-name modename))))
;; 	 `(menu-item  
;; 	   ,(modename-to-string modename)
;; 	   ,(eval 
;; 	     (list 'lambda '() '(interactive)
;; 		   (list 'set-style nil t `(quote ,modename))
;; 		   ))
;; 	   :help "Apply frame style of some major mode."
;; 	   )))))
      
;;    (reverse (sort (aq-copy-list aquamacs-default-styles)
;; 		  (lambda (a b) (string< 
;; 				 (upcase (symbol-name (car a))) 
;; 				 (upcase (symbol-name (car b)))))))
;;    )

  



  (define-key aquamacs-frame-style-menu [menu-aquamacs-styles]
    (menu-bar-make-mm-toggle 
     aquamacs-styles-mode
     "Auto Style"
     "adapt the default face parameters to the major-mode"))
      

  (define-key-after menu-bar-options-menu [aquamacs-frame-styles]

    (list 'menu-item "Mode Styles" aquamacs-frame-style-menu 
	  :help "Set face styles for buffers depending on major mode in buffer")
    'aquamacs-color-theme-select)

  ;; advise frame-notice-user-settings (from frame.el)
  ;; to integrate the mode-specific frame settings
  ;; which supersede the default-frame-alist, but not
  ;; the initial-frame-alist

  ;; for some reason, we can't byte-compile this.

  (defadvice frame-notice-user-settings 
    (around aquamacs-respect-mode-defaults () activate)
    
    ;(let ((dfa-tbl (assq 'tool-bar-lines default-frame-alist)))

      (if aquamacs-styles-mode

	 
	  (progn
	    ;; apply initial-frame-alist and default-frame-alist
	    (let ((default-frame-alist nil))
	      ad-do-it)
	    ;; ensure that correct style is set. This may turn the
	    ;; tool-bar on.
	    (aquamacs-set-style nil 'force)
	    )
	;; else
	ad-do-it
	)
    
      ;; ensure that frame-notice-user-settings doesn't set tool-bar-mode in
      ;; default-frame-alist
      ;; this should not be needed since default-frame-alist is
      ;; locally bound (away) via `let'. 
      ;; (unless (eq dfa-tbl
;; 		  (assq 'tool-bar-lines default-frame-alist))
;; 	(setq default-frame-alist
;; 	      (assq-delete-all 'tool-bar-lines default-frame-alist)))
    ;  )
    ;; workaround for an Emacs bug
    (let ((vsb (frame-parameter nil  'vertical-scroll-bars)))
      (modify-frame-parameters nil '((vertical-scroll-bars . nil)))
      (modify-frame-parameters nil `((vertical-scroll-bars . ,vsb)))
      )
    )


  ;; color styles
  
  ;; is needed 
  (require 'color-theme)
  (setq color-theme-is-global nil)
  (setq color-theme-target-frame nil)
  (setq color-theme-target-buffer nil)
  (defadvice color-theme-install 
    (around for-other-frame (&rest args) activate)
 
    (let ((buffer (current-buffer)))
    (if (and
	 (eq major-mode 'color-theme-mode)
	 color-theme-target-frame)
	(progn (select-frame color-theme-target-frame)
	       (setq buffer (or color-theme-target-buffer (current-buffer)))))
    (with-current-buffer buffer
      ad-do-it 
    )))

 

(defun aquamacs-color-theme-select ()
"Select a Color Theme.
Display a buffer with a list of color themes, which, upon selection, are applied to the 
selected frame."
  (interactive) 
 
  (setq color-theme-target-frame  (selected-frame)) 
  (setq color-theme-target-buffer (current-buffer))

  (let ((one-buffer-one-frame-force t))	
    ;; always open in new frame
    ;; because we've redefined bury->kill-buffer-and window 
    ;; in color-theme
    (color-theme-select)
    )
  )

(defvar smart-frame-positioning-hook nil) ;; stub
(add-hook 'smart-frame-positioning-hook
	  (lambda (f)
	    (aquamacs-set-style f)))

;	    (modify-frame-parameters f (aquamacs-get-style major-mode))))

; (setq smart-frame-positioning-hook nil)





;; backwards compatibility
(defvaralias 'aquamacs-auto-frame-parameters-flag
  'aquamacs-styles-mode)
(defvaralias 'aquamacs-mode-specific-default-themes
  'aquamacs-default-styles)
(defvaralias 'aquamacs-buffer-specific-frame-themes
  'aquamacs-buffer-default-styles)

;; turn on if desired
(if aquamacs-styles-mode
    (aquamacs-styles-mode 1))

(provide 'aquamacs-styles)
