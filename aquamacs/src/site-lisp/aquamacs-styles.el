
;; To do here:

;; new face naming convention

;; *Help*-buffer-default -- perhaps we should get rid of this.
;; text-mode-default


;; it appears that just changing the default face does not 
;; do enough: background of non-text can't be changed.
;; this is possibly easy to patch!

;; (setq face-remapping-alist nil)
;; (set (make-local-variable 'face-remapping-alist) '((default aquamacs-variable-width)))

;; OK, maybe use the current solution for a while and filter "font" from the color themes as frame parameters, but leave the other ones.

;; we do need to change the font seting dialog to just set the remapped face though (or create a remapped one), which should be easy.
 


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
 

;; Last change: $Id: aquamacs-styles.el,v 1.47 2008/10/05 22:47:20 davidswelt Exp $

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
 
(defvar aquamacs-use-color-themes nil 
  "Show Color Themes menu item if non-nil.")

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
   

(defun aquamacs-style-default-face (mode)
  (intern (format "default-%s" mode)))

(require 'smart-frame-positioning)
;; (frame-parameter nil 'font)
;; face-remapping-alist
(defun aquamacs-set-style (&optional frame force for-mode in-buffer)
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

	    (let* ((frame-param-buffer (aquamacs-style-relevant-buffer frame))
		   (buffer (or in-buffer
			    (window-buffer (selected-window))
			    frame-param-buffer)))
	      ;(print (aquamacs-style-relevant-buffer frame))
	      (with-current-buffer buffer
		(let ((mode (or for-mode major-mode)))
		  (when (or 
			 (and force (or buffer for-mode))
			 (and buffer
			      (not (equal (frame-parameter frame
							   'frame-configured-for-buffer)
					  (cons  
					   buffer 
					   mode)  
					  ))))
		    (let* ((style-face-id (aquamacs-style-default-face (or for-mode 
						(if (aquamacs-get-buffer-style (buffer-name))
						    (format "%s---%s" (buffer-name) mode)
						  mode))))
					    
			   
			   (style (aquamacs-combined-mode-specific-settings 
				   (if (special-display-p (buffer-name)) 
				       special-display-frame-alist 
				     default-frame-alist
				     )
				   (if for-mode
				       (aquamacs-get-style for-mode)
				     (append
				      (aquamacs-get-buffer-style (buffer-name))
				      (aquamacs-get-style mode)))))
			   ;; read out color-theme		 
			   ( color-theme (if aquamacs-use-color-themes (cdr (assq 'color-theme style))))
			   (style (assq-delete-all 'color-theme style)
				  ))  
		      ;; make sure we don't move the whole frame -  
		      ;; it is already shown on screen, and  
		      ;; the position is determined by `smart-frame-positioning',  
		      ;; that is per file name and according to the 'smart' heuristic   
		      
		      ;; ensure that setting the new frame parameters doesn't resize
		      ;; the frame significantly:
		      ;; change width / height to adapt to new font size
		      
		      (custom-declare-face 
		       style-face-id '((t :inherit default)) 
		       "mode-specific face activated by `aquamacs-styles-mode'.
The `default' face is remapped (in the appropriate buffers) to this face.")  

		      ;; (get 'default-lisp-mode 'saved-face)
		      ;; (get 'font-lock-comment-face 'saved-face)
		      ;; do not set frame parameters if they will be overridden by the later color theme
		      ;; this prevents flicker 
		      (let ((col-theme-parms  (if (and aquamacs-use-color-themes color-theme)
						  (condition-case nil 
						      (color-theme-frame-params 
						       (color-theme-canonic color-theme)) (error nil)))))
			(mapc (lambda (x)
				(setq style (assq-delete-all (car x) style)))
			      col-theme-parms))

		      ;; fixme: colors don't work
		      ;; they seem to come from the color theme and not the general
		      ;; parameters
		      (unless (and (not force) (facep style-face-id)) 
			;; do not override user's existing face choices
			(when (assq 'background-color style)
			  (set-face-background style-face-id (cdr (assq 'background-color style))))
			(when (assq 'foreground-color style)
			  (set-face-foreground style-face-id (cdr (assq 'foreground-color style))))
			(when (assq 'font style)
			  (set-face-font style-face-id (cdr (assq 'font style)) nil)))

		      ;; ensure this is saved as a customization
		      (let ((value (list (list t   (custom-face-attributes-get style-face-id nil)))))
			(put style-face-id 'saved-face   value)
			(custom-push-theme 'theme-face style-face-id 'user 'set value))

		      (setq style (assq-delete-all 'background-color style))
		      (setq style (assq-delete-all 'foreground-color style))
		      (setq style (assq-delete-all 'font style))
					; (print style-face-id)
		      ;; make sure we're remapping
		     
		      (make-local-variable 'face-remapping-alist)
		      (assq-set 'default style-face-id 'face-remapping-alist)
		      (modify-frame-parameters frame 
					       `((frame-configured-for-buffer . (,buffer . ,mode))))
		      (when frame-param-buffer ;; do not apply frame params in multi-window frames
			(when style ;; any frame parameters left?
			  (modify-frame-parameters frame style))
			(let ((after-change-major-mode-hook nil) 
			      window-configuration-change-hook)
			  ;; save-window-excursion can run this hook!
			  ;; color-style-target-frame seems deprecated
			  
			  (when aquamacs-use-color-themes
			    (save-window-excursion
			      (select-frame frame)
			      (if (and (functionp (car-safe color-theme))
				       (memq (car-safe color-theme) color-themes)
				       (not (cdr-safe color-theme)))
				  (funcall (car color-theme)) 
				;; just install the color style directly
				(color-theme-install color-theme)))))))
		    
		    (if window-configuration-change-hook
			(let ((selframe (selected-frame)))
			  (select-frame frame)
			  (run-hooks 'window-configuration-change-hook)
			  (select-frame selframe)))))))
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
       (aquamacs-set-style f t nil (current-buffer))
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
	  (aquamacs-set-style)))

    (error nil))
  t)

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
  (if aquamacs-use-color-themes
      (progn
	(require 'color-theme)
	(list 
	 (cons 'color-theme (let ((theme 
				   `(color-theme-snapshot
				     ;; alist of frame parameters
				     , (color-theme-get-params)
				     ;; alist of variables
				     ,(color-theme-get-vars)
				     ;; remaining elements of snapshot: face specs
				     ,@(color-theme-get-face-definitions))))
			      ;; - not implemented -
			      theme
			      ))
	 ;; replaced by default-* face customization
	 ;; (cons 'font (frame-parameter nil 'font))
	 (cons 'tool-bar-lines (frame-parameter nil 'tool-bar-lines))))
    ;; else
    (aquamacs-get-style-frame-parameters)))


(defvar aquamacs-styles-relevant-frame-parameter-regexp "\\(color\\|mode\\|lines\\|fringe\\)"
  "Regular expression to be found in frame parameters handled by aquamacs-styles.")

(defun aquamacs-get-style-frame-parameters ()
  "Return frame parameters of selected frame to be saved in a style."
  (apply #'append (mapcar (lambda (pm)
			    (if (string-match aquamacs-styles-relevant-frame-parameter-regexp (symbol-name (car pm)))
				(list pm)))
			  (frame-parameters))))

(defun aquamacs-set-style-as-default () 
  "Activate current frame and face settings (style) as default. 
Sets `default-frame-alist' and the `default' face."
  (interactive)

;; maybe delete mode-specific frames?
  (when 
      (let ((existing-styles  (aquamacs-default-styles-list)))
	(and existing-styles
	     (yes-or-no-p 
	      (format "Mode-specific styles are in place for the following modes: %s. Do you want to delete all of them so the default style is applied to frames with buffers in those modes?"
		      (apply 
		       'concat  
		       (let ((l (mapcar 
				 (lambda (x)
				   (concat
				    (symbol-name (car x)) ", "))
				 existing-styles)))
			 (if (nthcdr 5 l)
			     (setcdr (nthcdr 5 l) (list "(...)")))
			 l))))))
    (aquamacs-clear-styles))

  ;; delete the legacy `default' style
  (aquamacs-delete-one-style 'default)
  ;; define the default face
  (copy-face (aquamacs-style-default-face mode) 'default)
  (if aquamacs-use-color-themes
      (customize-set-variable 
       'aquamacs-default-styles
       (cons (cons mode (aquamacs-get-style-snapshot)) 
	     (assq-delete-all mode 
			      aquamacs-default-styles))))
  ;; set default-frame-alist
  (customize-set-variable 
   'default-frame-alist
   (append 
    (apply #'append
	   (mapcar 
	    (lambda (pm)
	      (if (string-match 
		   aquamacs-styles-relevant-frame-parameter-regexp 
		   (symbol-name (car pm)))
		  (list pm)))
	    default-frame-alist))
    (aquamacs-get-style-frame-parameters))))

	  


  );(aquamacs-adopt-frame-parameters-for-mode-style 'default))

(defun aquamacs-default-styles-list (&optional face-names)
  "Return list of all major modes that have a default style."
  (let ((styles (unless face-names aquamacs-default-styles)))
    (unless face-names
      (setq styles (mapcar 'car (assq-delete-all 'default styles))))
    (mapatoms
     (lambda (symbol)
       (when (and (get symbol 'saved-face) ;; it's a face we've customized
		  (eq 'user (car (car-safe (get symbol 'theme-face))))
		  (eq 0 (string-match "^default-\\(.*\\)$" (symbol-name symbol))))
	 (nconc styles (list (if face-names symbol
			       (intern (match-string 1 (symbol-name symbol))))))))
     (sort styles 'string<))))
  
(defun aquamacs-adopt-frame-parameters-for-mode-style (&optional mode) 
  (interactive)
  "Activate current style as default for a given mode."
  (setq mode (or mode major-mode))
 
  (customize-set-variable 'aquamacs-default-styles
			  (cons (cons mode (aquamacs-get-style-snapshot)) 
				(assq-delete-all mode 
						 aquamacs-default-styles)))

  (message (format "Fram parameters have been adopted for %s. %s" 
		   (if (eq mode 'default) "all frames" mode)
		   (if aquamacs-styles-mode
		       ""
		     (aquamacs-styles-mode 1)
		     "\nNote: Auto Styles have been turned on."))))

(defun aquamacs-remove-frame-parameters-from-style  (&optional mode) 
  (interactive)
  "Remove frame paraemters from style for a given mode."
  (setq mode (or mode major-mode))
  (customize-set-variable 'aquamacs-default-styles
			  (assq-delete-all mode 
					   aquamacs-default-styles))
  (message (format "Frame parameters have been removed from %s." 
		   (if (eq mode 'default) "frame default" mode))))


(defun set-to-custom-standard-value (symbol)
  (customize-set-variable symbol 
			  (eval (car
			   (get symbol 'standard-value)))))

(defun aquamacs-reset-styles ()
  "Resets all styles (mode-specific and the default style)"
  (interactive)
  (aquamacs-clear-styles)
  (set-to-custom-standard-value 'aquamacs-default-styles)
  (set-to-custom-standard-value 'aquamacs-buffer-default-styles)

  (message "All styles reset to defaults. Add new ones or use customize to 
modify them."))

(defun aquamacs-clear-styles ()
  "Resets all styles (mode-specific and the default style)"
  (interactive)
  (customize-set-variable 'aquamacs-default-styles nil)
  (customize-set-variable 'aquamacs-buffer-default-styles nil)

  ;; go over all faces
  (mapc
   (lambda (face)
     (face-spec-set symbol '((t (:inherit default)))))
   (aquamacs-default-styles-list 'facenames))
  (message "All styles cleared."))

(defun aquamacs-delete-one-style (&optional mode)
  "Deletes mode-specific styles for current major mode"
  (interactive)
  (unless mode (setq mode major-mode))
  (customize-set-variable  'aquamacs-default-styles 
			   (assq-delete-all mode
					    aquamacs-default-styles))
  (unless (eq mode 'default)
    (face-spec-set (aquamacs-style-default-face mode) '((t (:inherit default)))))
  (if (interactive-p)
      (message "Mode-specific style for %s removed. Use Save Options before restart to retain setting." mode)))

(defun aquamacs-updated-major-mode ()
"Returns the major mode of the selected window of the frame
for which the menu is being updated."
  (with-current-buffer (window-buffer
   (frame-selected-window menu-updating-frame))
    major-mode))




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
(setq aquamacs-frame-style-menu (make-sparse-keymap "Mode Styles"))

(defvar appstyle-mode-menu nil)

(defun aquamacs-apply-style-for-mode (&optional ignored modename)
;; todo: "Copy over style."
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
	 '(menu-bar-non-minibuffer-window-p)))
   (define-key-after aquamacs-frame-style-menu [set-mode]
    `(menu-item "Apply Style of Some Mode" ,appstyle-mode-menu
		:help "Apply frame style of some major mode.") 
    'menu-aquamacs-styles))
;; don't do this check (speed) - higher-level menu is disabled
;;		:enable (menu-bar-menu-frame-live-and-visible-p)


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
		  ;; the default is taken from default-frame-alist
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
  :require (if aquamacs-use-color-themes color-theme))
      

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

(defvar aquamacs-faces-changed nil)
(defadvice mac-handle-font-selection
  (after mark-faced-unsaved () activate)
  (setq aquamacs-faces-changed t))

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

;; colors menu

(defvar colors-click-map (make-sparse-keymap))
(define-key colors-click-map [mouse-1]
  'aquamacs-select-color)
(define-key colors-click-map "\r"
  'aquamacs-select-color-at-position)


(defvar aquamacs-color-selected-callback nil)
(defvar aquamacs-color-selected-callback-args nil)

(defun aquamacs-show-color-selection (buffer-name callback &rest arguments)
"Pop up color selection buffer.
Upon click/selection, CALLBACK will be called with color name and
then the ARGUMENTS."
  (list-colors-display nil buffer-name) ;; will pop up new window with buffer *Colors*
  (with-current-buffer (get-buffer buffer-name)
    (beginning-of-buffer)
    (setq buffer-read-only nil)
    (insert-button "[Quit]" 'action 'aquamacs-quit-color-selection 'follow-link t)
    (insert "\n\n")
    (set (make-local-variable 'aquamacs-color-selected-callback) callback)
    (set (make-local-variable 'aquamacs-color-selected-callback-args) arguments)
    (put-text-property (point) (point-max) 
		       'keymap colors-click-map)
    (setq buffer-read-only t)))

(defun aquamacs-quit-color-selection (but)
  (View-quit))

(defun aquamacs-select-color (ev)
  "Select the color at point of click."
  (interactive "e")
  (let ((p (posn-point (event-start ev))))
    (aquamacs-select-color-at-position p)))

(defun aquamacs-select-color-at-position (&optional p)
  "Select the color at point (or P) if given.
The Background color, or, if not given, foreground color is used."
  (interactive)
  (unless p (setq p (point)))
  (apply aquamacs-color-selected-callback
	 (or (plist-get (get-text-property p 'face) :background)
	     (plist-get (get-text-property p 'face) :foreground)) 
	 aquamacs-color-selected-callback-args))

(defvar aquamacs-set-color-target-face nil)
(defvar aquamacs-set-color-target-frame nil)
(defun aquamacs-set-face-color (color-name setter-function target-face target-frame)
  (funcall setter-function  ;; set-face-fore/background
	   (or target-face 'default)
	   color-name target-frame))

(defun aquamacs-select-foreground-color ()
  "Show a list of colors that can be used to select the forground.
The foreground color of the default face used in the current
buffer is set.  If the face is remapped, the specific (remapped)
face is modified in all frames.  Otherwise, the `default' face in
the current frame is modified."
  (interactive)
  (aquamacs-show-color-selection (format "Foreground Color for %s" 
					 (aquamacs-face-or-frame-name nil))
				 'aquamacs-set-face-color
				 'set-face-foreground
				 (cdr-safe (assq 'default face-remapping-alist))
				 (if (assq 'default face-remapping-alist)
					    nil (selected-frame))))

(defun aquamacs-select-background-color ()
  "Show a list of colors that can be used to select the forground.
The foreground color of the default face used in the current
buffer is set.  If the face is remapped, the specific (remapped)
face is modified in all frames.  Otherwise, the `default' face in
the current frame is modified."
  (interactive)
  (aquamacs-show-color-selection (format "Background Color for %s" 
					 (aquamacs-face-or-frame-name nil))
				 'aquamacs-set-face-color
				 'set-face-background
				 (cdr-safe (assq 'default face-remapping-alist))
				 (if (assq 'default face-remapping-alist)
					    nil (selected-frame))))


(defun aquamacs-face-or-frame-name (generic-frame-name)
(if (assq 'default face-remapping-alist)
					     (capitalize 
					      (replace-regexp-in-string 
					       "default-" "" 
					       (symbol-name (cdr (assq 'default face-remapping-alist)))))
					   (or generic-frame-name (concat "frame " (get-frame-name)))))

(define-key-after menu-bar-options-menu [background-color]
  `(menu-item (format "Background Color for %s...                 "
		      (aquamacs-face-or-frame-name "this Frame"))
	      aquamacs-select-background-color
	      :visible ,(display-multi-font-p)
	      :keys ,(aq-binding 'aquamacs-select-background-color)
	      :enable (menu-bar-menu-frame-live-and-visible-p) 
	      :help "Select a background color") 'mouse-set-font)

(define-key-after menu-bar-options-menu [foreground-color]
  `(menu-item (format "Foreground Color for %s...                 "
		      (aquamacs-face-or-frame-name "this Frame"))
	      aquamacs-select-foreground-color
	      :visible ,(display-multi-font-p)
	      :keys ,(aq-binding 'aquamacs-select-foreground-color)
	      :enable (menu-bar-menu-frame-live-and-visible-p) 
	      :help "Select a foreground color") 'mouse-set-font)


;(setq aquamacs-set-color-target-face 'default-emacs-lisp-mode)
;(setq aquamacs-set-color-target-frame nil)

;(setq aquamacs-set-color-target-frame (selected-frame))

	
  (add-hook 'after-change-major-mode-hook	
	    'set-mode-style-after-change-major-mode
	    )
  (add-hook 'menu-bar-update-hook 'aquamacs-update-mode-style)
  
  (define-key-after aquamacs-frame-style-menu [menu-clear-sep]
    '(menu-item  "--"))
(define-key-after aquamacs-frame-style-menu [menu-reset-styles]
    '(menu-item  "Reset All Styles"     aquamacs-reset-styles 
		 :help "Resets all styles to the default."
		 :enable t))
  (define-key-after aquamacs-frame-style-menu [menu-clear-styles]
    '(menu-item  "Clear All Styles"     aquamacs-clear-styles 
		 :help "Clear all styles."
		 :enable t))
  
(define-key aquamacs-frame-style-menu [menu-remove-frame-parameters-from-style]
    '(menu-item (format "Remove Frame Parameters from %s Style" 
			(or (modename-to-string (aquamacs-updated-major-mode)) "Current Mode"))
		aquamacs-remove-frame-parameters-from-style 
		:help "Set the current frame parameters as default 
for all frames with the current major-mode."
		:enable   (menu-bar-menu-frame-live-and-visible-p)))
  (define-key aquamacs-frame-style-menu [menu-adopt-frame-parameters]
    '(menu-item (format "Adopt these Frame Parameters for %s" 
			(or (modename-to-string (aquamacs-updated-major-mode)) "Current Mode"))
		aquamacs-adopt-frame-parameters-for-mode-style 
		:help "Set the current frame parameters as default 
for all frames with the current major-mode."
		:enable   (menu-bar-menu-frame-live-and-visible-p)))
 

  
 (define-key aquamacs-frame-style-menu [menu-clear-sep-2]
    '(menu-item  "--"))
 (define-key aquamacs-frame-style-menu [menu-set-style-as-default]
    '(menu-item  "Use this Style as Default for all Modes"     aquamacs-set-style-as-default
		 :enable (menu-bar-menu-frame-live-and-visible-p)
		 :help ""))
(define-key aquamacs-frame-style-menu [menu-delete-one-style]
    '(menu-item (format "Remove Style for %s" 
			(or 
			 (modename-to-string (aquamacs-updated-major-mode)) 
			 "current mode"))   
		aquamacs-delete-one-style 
		:enable (menu-bar-menu-frame-live-and-visible-p)
		:help "Removes a mode-specific style."))

(defun modename-to-string (modename)
  (capitalize
   (replace-regexp-in-string "-" " " (symbol-name modename))))

;; (setq aquamacs-default-styles nil)
(aquamacs-set-defaults '((tool-bar-mode 0)))
;; do not turn it off globally, because that would
;; only modify the default-frame-alist etc. 
;; and needlessly change the current frame.
;; anything necessary will be done by frame notice-user-settings
 
(define-key aquamacs-frame-style-menu [menu-aquamacs-styles]
    (menu-bar-make-mm-toggle 
     aquamacs-styles-mode
     "Auto Style"
     "adapt the default face parameters to the major-mode"))

(aquamacs-update-apply-style-for-mode-menu)
(define-key-after aquamacs-frame-style-menu [menu-aquamacs-styles-sep]
  '(menu-item "--") 'set-mode)

      

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
      (modify-frame-parameters nil `((vertical-scroll-bars . ,vsb)))))
 

  ;; color styles
  
;; is needed 
(when aquamacs-use-color-themes
  (require 'color-theme))

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
    (color-theme-select)))

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

