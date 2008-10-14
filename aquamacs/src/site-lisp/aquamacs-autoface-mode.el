
;; To do here:

;; adopt as default -- we shouldn't modify the default face
;; too many other faces inherit from default

;; instead, modify some kind of unassigned-default face and inherit
;; from that face when a new mode is entered.  


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
 


;; aquamacs mode specific faces

;; this package realizes mode-specific faces in Aquamacs.

;; Filename: aquamacs-autofaces.el
;; Description: Emacs init file for use with libraries from Drew Adams
;; Author: David Reitter
;; Maintainer: David Reitter
;; Keywords: aquamacs
 

;; Last change: $Id: aquamacs-autoface-mode.el,v 1.5 2008/10/14 02:53:44 davidswelt Exp $

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

;; Copyright (C) 2008 David Reitter, all rights reserved.

 
; (require 'cl)
(require 'aquamacs-cl) 

(defvar aquamacs-faces-changed nil)

(defun aquamacs-autoface-face (mode &optional bufname)
  (intern (if bufname
	      (format "%s-%s-default" bufname mode)
	    (format "%s-default" mode))))

(defface style-default '((t :inherit default)) 
  "Default face for buffers when `aquamacs-autoface-mode' is active.")
  
(defun aquamacs-import-frame-parameters-to-auto-faces ()
  "Read `aquamacs-default-styles', convert to faces.
The faces are then to be used with `aquamacs-autoface-mode'."
  (when (boundp 'aquamacs-default-styles)
    (message "Frame styles have been converted to faces: %s."
	     (mapcar
     (lambda (elt)
       (let* ((mode (car elt))
	      (style (cdr elt))
	      (face (aquamacs-autoface-face mode))
	      (color-theme (cdr (assq 'color-theme style)))
	      (col-theme-parms (if color-theme
				   (condition-case nil 
				       (color-theme-frame-params 
					(color-theme-canonic color-theme)) 
				     (error nil)))))
	 (aquamacs-autoface-make-face face)
	 (when (assq 'background-color style)
	   (set-face-background face (cdr (assq 'background-color style))))
	 (when (assq 'foreground-color style)
	   (set-face-foreground face (cdr (assq 'foreground-color style))))
	 (when (assq 'font style)
	   (set-face-font face (cdr (assq 'font style)) nil))
       mode))
     aquamacs-default-styles)
    (setq aquamacs-faces-changed t))))

(defun aquamacs-autofaces-set-default-parameter (param value &optional mode)
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


(defun aquamacs-autoface-make-face (face )
  (when (not (facep face))
    (let ((spec '((t :inherit style-default))))
      (make-empty-face face)
      (dolist (frame (frame-list))
	(face-spec-set face spec frame))
      (if (memq window-system '(x w32 mac))
	  (make-face-x-resource-internal face))
      ;; do not set face-defface-spec - this prevents it from being
      ;; saved to custom-file properly.
      ;; purecopy needed?
      (set-face-documentation
       face 
       (purecopy "mode-specific face activated by `aquamacs-autoface-mode'.
The `default' face is remapped (in the appropriate buffers) to this face.")))))

(defun aquamacs-set-face-style (buffer &optional for-mode)
  "Set the mode-specific style for BUFFER.
Use style of major mode FOR-MODE if given." 
  (when aquamacs-autoface-mode
	(with-current-buffer buffer
	  (let ((mode (or for-mode major-mode)))
	    (when (or 
		   (or buffer for-mode)
		   (not (assq 'default face-remapping-alist))
		   (not (eq (variable-binding-locus 'face-remapping-alist) 
			    (current-buffer))))
	      (let ((style-face-id (aquamacs-autoface-face mode (buffer-name))))
		(aquamacs-autoface-make-face style-face-id)
		(make-local-variable 'face-remapping-alist)
		(assq-set 'default style-face-id 'face-remapping-alist)
		))))))


(defmacro mac-event-ae (event)
  `(nth 2 ,event))
(defmacro mac-event-spec (event)
  `(nth 1 ,event))
  

(defun set-face-after-change-major-mode ()       			      
  (when aquamacs-autoface-mode
    (let ((buf (if (minibufferp) (window-buffer (minibuffer-selected-window))
		 (current-buffer))))
      (aquamacs-set-face-style buf))))

 
(defun aquamacs-set-style-as-default () 
  "Copy style to be used as default stle.
Sets the `style-default' face."
  (interactive)
;; maybe delete mode-specific frames?
  (when 
      (let ((existing-styles  (aquamacs-default-styles-list)))
	(and existing-styles
	     (yes-or-no-p 
	      (format "Mode-specific autofaces are in place for the following modes: %s. Do you want to delete all of them so the default style is applied to frames with buffers in those modes?"
		      (apply 
		       'concat  
		       (let ((l (mapcar 
				 (lambda (x)
				   (concat
				    (symbol-name x) ", "))
				 existing-styles)))
			 (if (nthcdr 5 l)
			     (setcdr (nthcdr 5 l) (list "(...)")))
			 l))))))
    (aquamacs-clear-autofaces))
 
  (copy-face (aquamacs-autoface-face mode) 'style-default)
  (if (interactive-p)
      (message "Face for %s is now used as default." mode)))


	  

(defun aquamacs-default-autofaces-list (&optional face-names include-default)
  "Return list of all major modes that have a default style.
face-names non-nil means return face names instead of mode names.
include-default includes style-default.  face-names implies include-default."
  (let ((styles (if face-names '(0) nil)))
    (unless face-names
      (setq styles (cons 0 (mapcar 'car 
				   (if include-default styles (assq-delete-all 'style-default styles))))))
    (mapatoms
     (lambda (symbol)
       (when (and (get symbol 'saved-face) ;; it's a face we've customized
		  (eq 'user (car (car-safe (get symbol 'theme-face))))
		  (eq 0 (string-match "^\\(.*\\)-default$" (symbol-name symbol))))
	 (nconc styles (list (if face-names symbol
			       (intern (match-string 1 (symbol-name symbol)))))))
       ))
     (sort (cdr styles) 'string<)))
;;  (aquamacs-default-autofaces-list)

 
(defun set-to-custom-standard-value (symbol)
  (customize-set-variable symbol 
			  (eval (car
			   (get symbol 'standard-value)))))

(defun aquamacs-reset-autofaces ()
  "Resets all auto faces (mode-specific and the default face)"
  (interactive)
  (aquamacs-clear-autofaces)
  (set-to-custom-standard-value 'aquamacs-default-styles)
  (set-to-custom-standard-value 'aquamacs-buffer-default-styles)
  (when aquamacs-styles-mode
    (aquamacs-styles-mode 0))

  (message "All auto faces reset to defaults. Add new ones or use customize to 
modify them."))

(defun aquamacs-clear-autofaces ()
  "Resets all styles (mode-specific and the default style)"
  (interactive)
  (customize-set-variable 'aquamacs-default-styles nil)
  (customize-set-variable 'aquamacs-buffer-default-styles nil)
  (when aquamacs-styles-mode
    (aquamacs-styles-mode 0))
  ;; go over all faces
  (mapc
   (lambda (face)
     (face-spec-set face '((t (:inherit style-default)))))
   (aquamacs-default-autofaces-list 'facenames))
  (message "All styles cleared."))

(defun aquamacs-delete-one-style (&optional mode)
  "Deletes mode-specific styles for current major mode"
  (interactive)
  (unless mode (setq mode major-mode))
  
  (unless (memq mode '(default style-default))
    (let ((face (aquamacs-autoface-face mode)))
      (if (facep face)
	  (face-spec-set face '((t (:inherit style-default)))))))
  (if (interactive-p)
      (message "Mode-specific style for %s removed. Use Save Options before restart to retain setting." mode)))
 


(defvar aquamacs-autoface-menu 
  (make-sparse-keymap "Mode Styles"))
(setq aquamacs-autoface-menu (make-sparse-keymap "Mode Styles"))

(defvar appstyle-mode-menu nil)

(defun aquamacs-apply-face-for-mode (&optional ignored for-mode)
  ;; todo: "Copy over style."
  (interactive)
  (unless for-mode  (setq for-mode last-command-event))
  (let ((src-face (if for-mode (aquamacs-autoface-face for-mode) 'style-default))
	(dest-face (aquamacs-autoface-face
		    (if (aquamacs-get-buffer-style (buffer-name))
			(format "%s-%s" (buffer-name) major-mode)
		      major-mode))))

    (unless (facep src-face) (setq src-face 'style-default))
    (unless (facep src-face) (setq src-face 'default))
    (copy-face src-face dest-face))
  (if (interactive-p)
      (message "Auto face copied.")))

(defun aquamacs-update-apply-face-for-mode-menu ()
  (setq appstyle-mode-menu
	(aquamacs-define-mode-menu-1  
	 (aquamacs-default-autofaces-list nil)
	 (make-sparse-keymap "Set Mode") 
	 'aquamacs-apply-face-for-mode
	 "Apply face assigned to %s." 
	 '(menu-bar-non-minibuffer-window-p) 'dont-filter))
  (define-key-after appstyle-mode-menu [sm-sep] '(menu-item "--"))
  (define-key-after appstyle-mode-menu [style-default]
    '(menu-item "Default Style" aquamacs-apply-face-for-mode
		:help "Apply default" :enable (menu-bar-non-minibuffer-window-p)))

   (define-key-after aquamacs-autoface-menu [set-mode]
    `(menu-item "Apply Face of Some Mode" ,appstyle-mode-menu
		:help "Apply default face of some major mode.") 
    'menu-aquamacs-autofaces))
;; don't do this check (speed) - higher-level menu is disabled
;;		:enable (menu-bar-menu-frame-live-and-visible-p)
;; (aquamacs-update-apply-face-for-mode-menu)
 


(define-minor-mode aquamacs-autoface-mode
  "Automatically set default face according to major mode.
This global minor mode will cause buffers to be displayed in
a font (and face) specific to the major mode chosen.

For instance, text buffers can be edited using variable width
fonts, while buffers showing code can be displayed with
fixed-width fonts.

A special face `style-default' is applied when no mode-specific style
is present. Any face setting will override parameters set 
in `default-frame-alist' or `special-display-frame-alist'.

This mode is part of Aquamacs Emacs, http://aquamacs.org."
 
  :init-value  t
  :group 'Aquamacs
  :global t)
      

  
(defadvice mac-handle-font-selection
  (after mark-faced-unsaved () activate)
  (setq aquamacs-faces-changed t))
 
(defadvice tool-bar-mode
  (after set-tool-bar-in-default-style () activate)

  (aquamacs-autofaces-set-default-parameter 'tool-bar-lines
					 (if tool-bar-mode 1 0)))

(defadvice set-fringe-mode
  (after set-fringe-in-default-style () activate)

  (aquamacs-autofaces-set-default-parameter 
   'right-fringe
   (cdr (assq 'right-fringe default-frame-alist)))
  (aquamacs-autofaces-set-default-parameter 
   'left-fringe
   (cdr (assq 'left-fringe default-frame-alist))))


(define-key menu-bar-options-menu [mouse-set-font]
  `(menu-item (format "Font for %s...                 "
		      (if (and (boundp 'face-remapping-alist)
			       (assq 'default face-remapping-alist))
			  (capitalize 
			   (replace-regexp-in-string 
			    "-default$" ""
			    (symbol-name (cdr (assq 'default face-remapping-alist)))))
			(if aquamacs-autoface-mode
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
	"-default$" "" 
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

 
	
(add-hook 'after-change-major-mode-hook	
	  'set-face-after-change-major-mode)
 
(define-key-after aquamacs-autoface-menu [menu-reset-autofaces]
    '(menu-item  "Reset All Auto Faces"     aquamacs-reset-autofaces 
		 :help "Resets all mode-specific auto faces to the default."
		 :enable t))
  (define-key-after aquamacs-autoface-menu [menu-clear-autofaces]
    '(menu-item  "Clear All Auto Faces"     aquamacs-clear-autofaces 
		 :help "Clear all styles."
		 :enable t))
  
 (define-key aquamacs-autoface-menu [menu-clear-sep-2]
    '(menu-item  "--"))
 (define-key aquamacs-autoface-menu [menu-set-style-as-default]
    '(menu-item  "Use this Face as Default for all Modes"     aquamacs-set-style-as-default
		 :enable (menu-bar-menu-frame-live-and-visible-p)
		 :help ""))
(define-key aquamacs-autoface-menu [menu-delete-one-style]
    '(menu-item (format "Remove Face for %s" 
			(or 
			 (modename-to-string (aquamacs-updated-major-mode)) 
			 "current mode"))   
		aquamacs-delete-one-style 
		:enable (menu-bar-menu-frame-live-and-visible-p)
		:help "Removes a mode-specific face."))

(defun modename-to-string (modename)
  (capitalize
   (replace-regexp-in-string "-" " " (symbol-name modename))))

;; (setq aquamacs-default-autofaces nil)
(aquamacs-set-defaults '((tool-bar-mode 0)))
;; do not turn it off globally, because that would
;; only modify the default-frame-alist etc. 
;; and needlessly change the current frame.
;; anything necessary will be done by frame notice-user-settings
 
(define-key aquamacs-autoface-menu [menu-aquamacs-autofaces]
    (menu-bar-make-mm-toggle 
     aquamacs-autoface-mode
     "Auto Faces"
     "adapt the default face parameters to the major-mode"))

(aquamacs-update-apply-face-for-mode-menu)
(define-key-after aquamacs-autoface-menu [menu-aquamacs-autofaces-sep]
  '(menu-item "--") 'set-mode)

      

(define-key-after menu-bar-options-menu [aquamacs-frame-autofaces]
  (list 'menu-item "Auto Faces" aquamacs-autoface-menu 
	  :help "Set default face in buffers depending on major mode ")
    'background-color)
 
   
;; turn on if desired
(if aquamacs-autoface-mode
    (aquamacs-autoface-mode 1))

(provide 'aquamacs-autoface-mode)


 