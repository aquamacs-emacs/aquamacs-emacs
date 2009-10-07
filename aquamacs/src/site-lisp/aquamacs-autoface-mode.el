;; aquamacs mode specific faces

;; this package realizes mode-specific faces in Aquamacs.


;; To do here:

;; it appears that just changing the default face does not
;; do enough: background of non-text can't be changed.
;; this requires a C-level patch.  'tiling' branch seems to have this.




;; Filename: aquamacs-autofaces.el
;; Description: Emacs init file for use with libraries from Drew Adams
;; Author: David Reitter
;; Maintainer: David Reitter
;; Keywords: aquamacs


;; Last change: $Id: aquamacs-autoface-mode.el,v 1.58 2009/03/08 21:24:13 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/

;; Aquamacs Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Aquamacs Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Aquamacs Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Copyright (C) 2008 David Reitter, all rights reserved.


; (require 'cl)
(eval-when-compile (require 'aquamacs-cl))

(defvar aquamacs-faces-changed nil)

(defun aquamacs-updated-major-mode ()
"Returns the major mode of the selected window of the frame
for which the menu is being updated."
  (with-current-buffer (window-buffer
   (frame-selected-window menu-updating-frame))
    major-mode))

(defun aquamacs-autoface-inherited-face ()
  "Return face name that the autoface inherits from"
  (let ((face (cdr-safe (assq 'default face-remapping-alist))))
    (if face
	(face-attribute face :inherit)
      nil)))

(defun aquamacs-autoface-inherited-mode-name ()
  "Return mode name that the autoface inherits from"
  (let ((inh  (aquamacs-autoface-inherited-face)))
    (if (and inh (eq 0 (string-match "^\\(.*\\)-default$" (symbol-name inh))))
	(intern (match-string 1 (symbol-name inh))))))

(defun aquamacs-autoface-face (mode &optional bufname)
  (let ((face))
    (if (and
	 bufname
	 (setq face (intern (format "%s-%s-default" bufname mode)))
	 (facep face))
	face
      (intern (format "%s-default" mode)))))

(defface autoface-default '((t :inherit default))
  "Default face for buffers when `aquamacs-autoface-mode' is active."
  :group 'Aquamacs)

(defun aquamacs-import-frame-parameters-to-auto-faces ()
  "Read `aquamacs-default-styles', convert to faces.
The faces are then to be used with `aquamacs-autoface-mode'."
  (when (boundp 'aquamacs-default-styles)
    (message "Frame styles have been converted to faces: %s."
	     (mapcar
     (lambda (elt)
       (let* ((mode (car elt))
	      (style (cdr elt))
	      (face (aquamacs-autoface-make-face mode t))
	      (color-theme (cdr (assq 'color-theme style)))
	      (col-theme-parms (if color-theme
				   (condition-case nil
				       (color-theme-frame-params
					(color-theme-canonic color-theme))
				     (error nil)))))
	 (when (assq 'background-color style)
	   (set-face-background face (cdr (assq 'background-color style))))
	 (when (assq 'foreground-color style)
	   (set-face-foreground face (cdr (assq 'foreground-color style))))
	 (when (assq 'font style)
	   (set-face-font face (cdr (assq 'font style)) nil))
       mode))
     aquamacs-default-styles)
    (if (and (fboundp 'aquamacs-styles) (boundp 'aquamacs-styles) aquamacs-styles)
	;; this should ensure that styles is not kept `on' in custom-file.
	(aquamacs-styles 0)))))

(defun aquamacs-autoface-mark-face-to-save (face &optional dont-save)
  "Ensure FACE will be saved to `custom-file'."
  (when (facep face)
    (let ((value (list (list t   (custom-face-attributes-get face nil)))))
      (unless dont-save
	(put face 'saved-face value)
	(setq aquamacs-faces-changed t))
      (custom-push-theme 'theme-face face 'user 'set value))))

(defun aquamacs-autoface-make-face (mode mark-to-save)
  "Return autoface for mode
Make the face if it doesn't exist."
  (let ((face (aquamacs-autoface-face mode)))
    (when (not (facep face)) ; do not delete existing faces
      (make-empty-face face)
      ;; purecopy needed?
      (set-face-documentation
       face
       (purecopy "mode-specific face activated by `aquamacs-autoface-mode'.
The `default' face is remapped (in the appropriate buffers) to this face.")))
    ;; for this and for existing faces: ensure inheritance is correct
    (let ((derived (get mode 'derived-mode-parent)))
     (set-face-attribute face nil :inherit
			  (or (and derived
				   (symbolp derived)
				   (if (facep (aquamacs-autoface-face derived))
				       (aquamacs-autoface-face derived)
					; prevent cycles
				     (aquamacs-autoface-make-face derived nil)))
			      'autoface-default)))
    ;; do not set face-defface-spec - this prevents it from being
    ;; saved to custom-file properly.
    (aquamacs-autoface-mark-face-to-save face (not mark-to-save))
    face))

(defun aquamacs-set-autoface (buffer)
  "Set the major-mode-specific style for BUFFER."
  (when aquamacs-autoface-mode
	(with-current-buffer buffer
	  (setq aquamacs-autoface-set-for-mode major-mode)
	  (when (or
		 (not (assq 'default face-remapping-alist))
		 (not (eq (variable-binding-locus 'face-remapping-alist)
			  (current-buffer))))
	    (let ((style-face-id (aquamacs-autoface-make-face major-mode nil)))
	      (make-local-variable 'face-remapping-alist)
	      (assq-set 'default style-face-id 'face-remapping-alist))))))


(defmacro mac-event-ae (event)
  `(nth 2 ,event))
(defmacro mac-event-spec (event)
  `(nth 1 ,event))

(defun set-face-after-change-major-mode ()
  (when aquamacs-autoface-mode
    (let ((buf (if (minibufferp) (window-buffer (minibuffer-selected-window))
		 (current-buffer))))
      (aquamacs-set-autoface buf))))

;; doesn't work because copy-face can't do this.
;; (defun aquamacs-set-frame-face-as-frame-default ()
;;   "Copy default face in selected frame to default face everywhere."
;;   (interactive)
;;   (copy-face 'default 'default (selected-frame) t)
;;   (setq aquamacs-faces-changed))


 (defun aquamacs-set-face-as-default ()
  "Copy face to be used as default face.
Sets the `autoface-default' face."
  (interactive)
;; maybe delete mode-specific frames?
  (when
      (let ((existing-styles  (aquamacs-default-autofaces-list)))
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
  ;; do not just copy the (shallow) face
  ;;  (copy-face (aquamacs-autoface-face major-mode) 'autoface-default nil)
  (face-spec-set
   'autoface-default
   ;; use full specification of the face in order to set
   ;; exactly what the user sees
   `((t ,(let ((face (aquamacs-autoface-face major-mode))
	       (result))
	   (dolist (entry face-attribute-name-alist result)
	     (unless (eq entry :inherit)
	       (let* ((attribute (car entry))
		      ;; inherit from default face
		      (value (face-attribute face attribute nil 'default)))
		 (unless (eq value 'unspecified)
		   (setq result (nconc (list attribute value) result))))))
	   result)))
    ;; can be overwritten by user customization
	       (if (> emacs-major-version 22) t))
  ;; ensure that the new face does not inherit from itself:
  (set-face-attribute 'autoface-default nil
		      :inherit 'default)
  (aquamacs-autoface-mark-face-to-save 'autoface-default)
  (if (interactive-p)
      (message "Face for %s is now used as default." major-mode)))

(defvar aquamacs-relevant-frame-parameter-regexp "\\(font\\|color\\|mode\\|lines\\|fringe\\)"
  "Regular expression to be saved in frame parameters.
This is used by the function
`aquamacs-set-frame-parameters-as-default' to determine which
frame parameters to save as default. handled by
aquamacs-styles.")

(defun aquamacs-set-frame-parameters-as-special-display ()
  "Use current frame settings for special display frames.
See also `aquamacs-set-frame-parameters-as-default'."
  (interactive)
  (aquamacs-set-frame-parameters-as-default 'special-display-frame-alist))

(defun aquamacs-set-frame-parameters-as-default (&optional target)
  "Use current frame settings as default for new frames.
Sets all frame parameters in `default-frame-alist' and
`initial-frame-alist' from the selected frame, as long as they
match `aquamacs-relevant-frame-parameter-regexp'.  If TARGET is
given, set the variable named TARGET instead, e.g.,
`special-display-frame-alist'."
  (interactive)
  ;; set default face, because
  ;; this will set most relevant frame parameters as well.
  ;; that way, we're really setting the face and frame parameters.
  (let ((source-face (or (cdr-safe (assq 'default face-remapping-alist)) 'default)))
    (mapcar (lambda (att-cons)
	      (set-face-attribute
	       'default nil
	       (car att-cons)
	       (face-attribute source-face (car att-cons) nil 'default)))
	    face-attribute-name-alist))
  (and smart-frame-positioning-mode
       (dolist (frame (frame-list))
	 (smart-move-frame-inside-screen frame)))
  ;; set default-frame-alist
  (let ((new-values (append
		       (apply #'append
			      (mapcar
			       (lambda (pm)
				 (if (string-match
				      aquamacs-relevant-frame-parameter-regexp
				      (symbol-name (car pm)))
				     (list pm)))
			       (frame-parameters))))))

    (customize-set-variable
     (or target 'default-frame-alist)
     (append
      (apply #'append
	     (mapcar
	      (lambda (pm)
		(unless (assq (car pm) new-values)
		  (list pm)))
	      (symbol-value (or target 'default-frame-alist))))
      new-values)
     )
    (unless target
      ;; set initial-frame-alist
      (customize-set-variable
       'initial-frame-alist
       (append
	(apply #'append
	       (mapcar
		(lambda (pm)
		  (unless (assq (car pm) new-values)
		    (list pm)))
		initial-frame-alist))
	new-values))))
  ;; because we're setting default-frame-alist, and changes there don't get picked up
  (setq aquamacs-faces-changed t))


(defun aquamacs-default-autofaces-list (&optional face-names include-default)
  "Return list of all major modes that have a default style.
face-names non-nil means return face names instead of mode names.
include-default includes autoface-default.  face-names implies include-default."
  (let ((styles (if face-names '(0) nil)))
    (unless face-names
      (setq styles (cons 0 (mapcar 'car
				   (if include-default styles (assq-delete-all 'autoface-default styles))))))
    (mapatoms
     (lambda (symbol)
       (when (and (facep symbol)
		  (eq 0 (string-match "^\\(.*\\)-default$" (symbol-name symbol))))

	 (let ((m (intern (match-string 1 (symbol-name symbol)))))
	   (if (fboundp m)
	       (nconc styles (list (if face-names symbol m))))))))
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
  (aquamacs-init-faces)
  (when aquamacs-styles-mode
    (aquamacs-styles-mode 0))
  (message "All auto faces reset to defaults. Add new ones or use customize to
modify them."))

(defun aquamacs-clear-autofaces ()
  "Clear all autofaces."
  (interactive)
  (when (boundp 'aquamacs-default-styles)
      (setq aquamacs-default-styles nil)
      (setq aquamacs-buffer-default-styles nil))
  (when aquamacs-styles-mode
    (aquamacs-styles-mode 0))
  ;; reset the face
  (mapc (lambda (face) (face-spec-set face '((t (:inherit autoface-default)))
				      (if (> emacs-major-version 22) t)))
	(aquamacs-default-autofaces-list 'facenames))
  ;; set correct inheritance
  (mapc (lambda (mode) (aquamacs-autoface-make-face mode t))
	(aquamacs-default-autofaces-list))
  ;; and now delete the face customization from user theme
  ;; so they won't be saved to custom-file because they're
  ;; default
  (mapc (lambda (face) (custom-push-theme 'theme-face face 'user 'reset))
	(aquamacs-default-autofaces-list 'facenames))
  ;; reset the autoface-default face
  (custom-push-theme 'theme-face 'autoface-default 'user 'reset)
  ;; and re-make the autoface default face
  (face-spec-set 'autoface-default '((t (:inherit default)))
		 ;; set default (not user customization)
		 (if (> emacs-major-version 22) t))
  (setq aquamacs-faces-changed t)
  (if (interactive-p)
      (message "All styles cleared.")))

(defun aquamacs-delete-one-autoface (&optional mode)
  "Deletes mode-specific autoface for current major mode"
  (interactive)
  (unless mode (setq mode major-mode))
  (unless (memq mode '(default autoface-default))
    (let ((face (aquamacs-autoface-face mode)))
      (when (facep face)
	  (face-spec-set face '((t (:inherit autoface-default)))
			 ;; set default (not user customization)
			 (if (> emacs-major-version 22) t))
	  ;; set correct inheritance
	  (aquamacs-autoface-make-face mode t)
	  ;; and now delete the face customization from theme
	  (custom-push-theme 'theme-face face 'user 'reset)))
    (if (interactive-p)
	(message "Mode-specific face for %s removed." mode))))

(defvar aquamacs-autoface-menu
  (make-sparse-keymap "Mode Styles"))
(setq aquamacs-autoface-menu (make-sparse-keymap "Mode Styles"))

(defvar appstyle-mode-menu nil)

(defun aquamacs-apply-face-for-mode (&optional ignored for-mode)
  ;; todo: "Copy over style."
  (interactive)
  (unless for-mode  (setq for-mode last-command-event))
  (let ((src-face (if for-mode (aquamacs-autoface-face for-mode) 'autoface-default))
	(dest-face (aquamacs-autoface-face major-mode (buffer-name))))

    (unless (facep src-face) (setq src-face 'autoface-default))
    (unless (facep src-face) (setq src-face 'default))
    ;; we'll just copy the face, including its inheritance
    ;; (by reference).  Deep copy is not needed.
    (copy-face src-face dest-face)
    (aquamacs-autoface-mark-face-to-save dest-face))
  (if (interactive-p)
      (message "Auto face for %s copied." for-mode)))

(defun aquamacs-update-apply-face-for-mode-menu ()
  (setq appstyle-mode-menu
	(aquamacs-define-mode-menu-1
	 (aquamacs-default-autofaces-list nil)
	 (make-sparse-keymap "Set Mode")
	 'aquamacs-apply-face-for-mode
	 "Apply face assigned to %s."
	 '(menu-bar-non-minibuffer-window-p) 'dont-filter))
  (define-key-after appstyle-mode-menu [sm-sep] '(menu-item "--"))
  (define-key-after appstyle-mode-menu [autoface-default]
    '(menu-item "Default" aquamacs-apply-face-for-mode
		:help "Apply default" :enable (menu-bar-non-minibuffer-window-p)))

   (define-key-after aquamacs-autoface-menu [set-mode]
    `(menu-item "Apply Face of Some Mode" ,appstyle-mode-menu
		:enable (and (menu-bar-menu-frame-live-and-visible-p)
			    aquamacs-autoface-mode)
		:help "Apply default face of some major mode.")
    'menu-aquamacs-autofaces))
;; don't do this check (speed) - higher-level menu is disabled
;;		:enable (menu-bar-menu-frame-live-and-visible-p)
;; (aquamacs-update-apply-face-for-mode-menu)


(defmacro user-buffer-p (buf)
  "Evaluate to t if buffer BUF is not an internal buffer."
  `(not (string= (substring (buffer-name ,buf) 0 1) " ")))

(define-minor-mode aquamacs-autoface-mode
  "Automatically set default face according to major mode.
This global minor mode will cause buffers to be displayed in
a font (and face) specific to the major mode chosen.

For instance, text buffers can be edited using variable width
fonts, while buffers showing code can be displayed with
fixed-width fonts.

The face used in each buffer is named after the major mode, e.g.,
`text-mode' will be displayed in the `text-mode-default' face.

A special face `autoface-default' is applied when no mode-specific style
is present. Any face setting will override parameters set
in `default-frame-alist' or `special-display-frame-alist'.

This mode is part of Aquamacs Emacs, http://aquamacs.org."

  :init-value  t
  :group 'Aquamacs
  :global t

  (if aquamacs-autoface-mode
      (run-with-idle-timer 1 'repeat 'aquamacs-set-autoface-when-idle)
    (cancel-function-timers 'aquamacs-set-autoface-when-idle))

  ;; check for circularities
  (when  (and aquamacs-autoface-mode
	      (eq (face-attribute 'autoface-default :inherit) 'autoface-default))
    (set-face-attribute 'autoface-default nil
			:inherit 'default)
    (message "Autoface-default face inherited from itself!"))

  (mapc (lambda (b)
	(if (and (buffer-live-p b)
	    (user-buffer-p b))
	  (with-current-buffer b
	    (if aquamacs-autoface-mode
		(aquamacs-set-autoface b)
	      ;; else
	      (if (eq (variable-binding-locus 'face-remapping-alist) b)
		  (setq face-remapping-alist (assq-delete-all 'default
							      face-remapping-alist)))))))
	  (buffer-list)))



;; (defadvice tool-bar-mode
;;   (after set-tool-bar-in-default-style () activate)

;;   (aquamacs-autofaces-set-default-parameter 'tool-bar-lines
;; 					 (if tool-bar-mode 1 0)))

;; (defadvice set-fringe-mode
;;   (after set-fringe-in-default-style () activate)

;;   (aquamacs-autofaces-set-default-parameter
;;    'right-fringe
;;    (cdr (assq 'right-fringe default-frame-alist)))
;;   (aquamacs-autofaces-set-default-parameter
;;    'left-fringe
;;    (cdr (assq 'left-fringe default-frame-alist))))

;; the fringe should inherit from the buffer-specific default face
;; rather than from the frame default face.
;; this can be seen in a dark slime-repl-mode, when switching between different
;; buffers with previous-tab-or-buffer (fringe is not redrawn)
;; it may be some problem with redisplay, of course.
(aquamacs-set-defaults '((face-remapping-alist ((fringe . fringe)))))
;; (aquamacs-set-defaults '((face-remapping-alist nil)))

(defvar appearance-menu (make-sparse-keymap "Looks"))
(setq appearance-menu (make-sparse-keymap "Looks"))

;; colors menu
(defun aquamacs-popup-color-panel ()
  "Show the color panel.
Drag and drop colors onto text to set the accoring face.
Hold Meta while dragging to set background color for the face."
  (interactive)
  (setq aquamacs-color-panel-target-face 'none
	aquamacs-color-panel-target-frame nil
	aquamacs-color-panel-target-prop nil)
  (ns-popup-color-panel)
  (message "Drag and drop colors onto text to set the according face foreground. Hold Meta for background color."))

(defun aquamacs-select-foreground-color (&optional frame)
  "Show color panel.
The foreground color of the default face used in the current
buffer is set.  If the face is remapped, the specific (remapped)
face is modified in all frames.  Otherwise, the `default' face in
the current frame is modified.   The face valid in all frames is
modified, or in FRAME if given."
  (interactive)
  (aquamacs-show-color-panel :foreground
			     (if (assq 'default face-remapping-alist)
				 nil frame)))


(defun aquamacs-select-background-color (&optional frame)
  "Show color panel.
The foreground color of the default face used in the current
buffer is set.  If the face is remapped, the specific (remapped)
face is modified in all frames.  Otherwise, the `default' face in
the current frame is modified.   The face valid in all frames is
modified, or in FRAME if given."
  (interactive)
  (aquamacs-show-color-panel :background 
			     (if (assq 'default face-remapping-alist)
				 nil frame)))


(defun aquamacs-default-face-in-effect (&optional only-mode-face)
  (let ((face (cdr-safe (assq 'default face-remapping-alist))))
    (if face
	(if (get face 'theme-face)
	    face
	  (or (face-attribute face :inherit)
	      (if only-mode-face nil 'default)))
      'default)))

(defun aquamacs-face-or-frame-name (generic-frame-name)
  (if (assq 'default face-remapping-alist)
      (capitalize
       (replace-regexp-in-string
	"-default$" ""
	(symbol-name (aquamacs-default-face-in-effect))))
    (or generic-frame-name (concat "frame " (get-frame-name)))))

(add-hook 'after-change-major-mode-hook
	  'set-face-after-change-major-mode)

(define-key-after aquamacs-autoface-menu [menu-reset-autofaces]
  '(menu-item  "Reset All Auto Faces"     aquamacs-reset-autofaces
	       :help "Resets all mode-specific auto faces to the default."
	       :enable aquamacs-autoface-mode))

(define-key aquamacs-autoface-menu [menu-clear-sep-2]
  '(menu-item  "--"))

(define-key aquamacs-autoface-menu [menu-set-face-as-default]
  '(menu-item  "Use Current Face as Default" aquamacs-set-face-as-default
	       :enable (and (menu-bar-menu-frame-live-and-visible-p)
			    aquamacs-autoface-mode)
;;	       :visible (eq 'autoface-default (aquamacs-autoface-inherited-face))
	       :help "Copy current face to autoface-default face."))

(define-key aquamacs-autoface-menu [autoface-mode-face]
  '(menu-item (format "Default: %s"
		      (aquamacs-autoface-face-summary 'autoface-default))
	      aquamacs-autoface-customize-default-face
	      :enable nil ;; customize window management isn't good (when quitting)
;;	      :visible (eq 'autoface-default (aquamacs-autoface-inherited-face))
	      :help "This is the autoface-default face." ))

(define-key aquamacs-autoface-menu [autoface-inherited-face]
  '(menu-item (format "Based on: %s Mode, %s"
		      (aquamacs-pretty-mode-name (aquamacs-autoface-inherited-mode-name))
		      (aquamacs-autoface-face-summary (aquamacs-autoface-inherited-face)))
	      nil
	      :enable nil
	      :visible (not (eq 'autoface-default (aquamacs-autoface-inherited-face)))
	      :help "This is the face the current one is based on." ))

(define-key aquamacs-autoface-menu [menu-clear-sep-3]
  '(menu-item  "--"))

(define-key aquamacs-autoface-menu [menu-delete-one-autoface]
  '(menu-item "Remove Mode Face"
	      aquamacs-delete-one-autoface
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   aquamacs-autoface-mode)
	      :help "Removes a mode-specific face."))

(define-key aquamacs-autoface-menu [autoface-default-face]
  '(menu-item (format "%s: %s" (aquamacs-face-or-frame-name "none")
		      (aquamacs-autoface-face-summary (aquamacs-default-face-in-effect)))
	      aquamacs-autoface-customize-mode-face
	      :enable nil))


(defun modename-to-string (modename)
  (capitalize
   (replace-regexp-in-string "-" " " (symbol-name modename))))

;; (setq aquamacs-default-autofaces nil)
; (aquamacs-set-defaults '((tool-bar-mode 0)))
;; do not turn it off globally, because that would
;; only modify the default-frame-alist etc.
;; and needlessly change the current frame.
;; anything necessary will be done by frame notice-user-settings

(define-key aquamacs-autoface-menu [menu-aquamacs-autofaces-sep]
  '(menu-item "--"))
(define-key aquamacs-autoface-menu [menu-aquamacs-autofaces]
    (menu-bar-make-mm-toggle
     aquamacs-autoface-mode
     "Auto Faces"
     "adapt the default face parameters to the major-mode"))



(define-key appearance-menu [aquamacs-set-frame-defaults]
  (list 'menu-item "Adopt Face and Frame Parameters as Frame Default"
	'aquamacs-set-frame-parameters-as-default
	:visible '(not (special-display-p (buffer-name)))
	:help "Set most default frame parameters to ones of selected frame."))

(define-key appearance-menu [aquamacs-set-frame-display-display]
  (list 'menu-item "Adopt Face and Frame Parameters for Special Frames"
	'aquamacs-set-frame-parameters-as-special-display
	:visible '(special-display-p (buffer-name ))
	:help "Set most special display frame parameters to ones of selected frame."))


(define-key appearance-menu [aquamacs-frame-sep] '(menu-item "--" nil))

(define-key appearance-menu [aquamacs-frame-autofaces]
  (list 'menu-item "Auto Faces" aquamacs-autoface-menu
	:help "Set default face in buffers depending on major mode "))

(defun aquamacs-autoface-face-summary (face)
					; (setq face 'emacs-lisp-mode-default)
					; (setq face 'text-mode-default)

  (if (not (facep face))
      "none"
    (let ((str
	   (concat
	    (let ((fam (face-attribute face :family)))
	      (if (and fam (not (eq fam 'unspecified)))
		  (format "%s " (capitalize fam))
		""))
	    (let ((size (face-attribute face :height)))
	      (if (and size (not (eq size 'unspecified)))
		  (format "%0dpt " (/ size 10))
		""))
	    (let ((fg (face-attribute face :foreground)))
	      (if (and fg (not (eq fg 'unspecified)))
		  (format "%s " fg)
		""))
	    (let ((bg (face-attribute face :background)))
	      (if (and bg (not (eq bg 'unspecified)))
		  (format "on %s" bg)
		"")))))
      (if (> (length str) 0) str "none"))))

(defun aquamacs-autoface-customize-mode-face (&optional face)
  "Customize the mode-specific face for `aquamacs-autoface'"
  (interactive)
  (let ((face (or face
		  (aquamacs-default-face-in-effect 'only-mode-face)
		  (aquamacs-autoface-make-face major-mode nil))))
    (customize-face-other-window face)))

(defun aquamacs-autoface-customize-default-face ()
  "Customize the default face for `aquamacs-autoface'"
  (interactive)
  (aquamacs-autoface-customize-mode-face 'autoface-default))


(aquamacs-update-apply-face-for-mode-menu)


;; doesn't work.
;; (define-key appearance-menu [aquamacs-set-face-defaults]
;;   (list 'menu-item "Adopt Frame Face as Default"
;; 	'aquamacs-set-frame-face-as-frame-default
;; 	:visible '(and (menu-bar-menu-frame-live-and-visible-p)
;; 		      (not aquamacs-autoface-mode))
;; 	:help "Set this frame's face as default."))

;; (define-key appearance-menu [background-color]
;;   `(menu-item (if aquamacs-autoface-mode
;; 		  (format "Background Color for %s..."
;; 			  (aquamacs-face-or-frame-name "this Frame"))
;; 		"Background Color...")
;; 	      aquamacs-select-background-color
;; 	      :visible ,(display-multi-font-p)
;; 	      :enable (menu-bar-menu-frame-live-and-visible-p)
;; 	      :help "Select a background color"))

;; (define-key appearance-menu [foreground-color]
;;   `(menu-item  (if aquamacs-autoface-mode
;; 		   (format "Foreground Color for %s..."
;; 		      (aquamacs-face-or-frame-name "this Frame"))
;; 		 "Foreground Color...")
;; 	      aquamacs-select-foreground-color
;; 	      :visible ,(display-multi-font-p)
;; 	      :enable (menu-bar-menu-frame-live-and-visible-p)
;; 	      :help "Select a foreground color"))

(defun aquamacs-autoface-setup-menu ()
(define-key appearance-menu [color-panel]
  `(menu-item  "Color Panel..."
	      aquamacs-popup-color-panel
	      :help "Drag and drop colors"))


(define-key menu-bar-options-menu [mouse-set-font] nil)
(define-key appearance-menu [set-font]
  `(menu-item (format "Font%s...                 "
		      (if (and (boundp 'face-remapping-alist)
			       (assq 'default face-remapping-alist))
			  (concat " for " (capitalize
			   (replace-regexp-in-string
			    "-default$" ""
			    (symbol-name
			     (let ((face (cdr (assq 'default face-remapping-alist))))
			       (if (get face 'theme-face)
				   face
				 (or (face-attribute face :inherit) 'default)))))))
			(if aquamacs-autoface-mode
			    " (default)" "")))
	      aquamacs-popup-font-panel
	      :visible ,(display-multi-font-p)
	      :key-sequence [(,osxkeys-command-key shift t)]
	      :enable (menu-bar-menu-frame-live-and-visible-p)
	      :help "Select a font from list of known fonts/fontsets"))


(define-key-after menu-bar-options-menu [aquamacs-frame-autofaces]
  (list 'menu-item "Appearance" appearance-menu
	  :help "Appearances")
    'showhide)
)
(add-hook 'aquamacs-menu-setup-hook 'aquamacs-autoface-setup-menu)


;; A lot of major modes forget to use `run-mode-hooks'.
;; here's a workaround.

(defvar aquamacs-autoface-set-for-mode nil)
(make-variable-buffer-local 'aquamacs-autoface-set-for-mode)

(defun aquamacs-set-autoface-when-idle ()
  (when aquamacs-autoface-mode
    (let ((buf (if (minibufferp) (window-buffer (minibuffer-selected-window))
		 (current-buffer))))
      (with-current-buffer buf
	(unless (and
		 aquamacs-autoface-set-for-mode
		 (eq aquamacs-autoface-set-for-mode major-mode))

	  (with-temp-message (unless (eq major-mode default-major-mode)
			       (format "Warning: Bug in %s: it forgets to call `run-mode-hooks'" major-mode))
			     (aquamacs-set-autoface buf)))))))


;; Font Panel  (under NS)

(when (and (boundp 'initial-window-system) (eq initial-window-system 'ns))

(defvar mac-font-panel-target-face 'default)
(defvar mac-font-panel-target-frame nil)

(defvar aquamacs-color-panel-target-face 'default)
(defvar aquamacs-color-panel-target-frame nil)
(defvar aquamacs-color-panel-target-prop nil)

(defun aquamacs-show-color-panel (&optional prop frame)
  "Show color panel."
  (interactive)
  (setq aquamacs-color-panel-target-face (aquamacs-default-face-in-effect)
	aquamacs-color-panel-target-frame frame
	aquamacs-color-panel-target-prop (or prop :foreground))
  (ns-popup-color-panel frame
			(face-attribute aquamacs-color-panel-target-face aquamacs-color-panel-target-prop
					frame t))
  (message "Set %s for %s face."
	   (cond ((eq aquamacs-color-panel-target-prop :foreground)
		  "foreground color")
		 ((eq aquamacs-color-panel-target-prop :background)
		  "background color")
		 (t "color"))
	   aquamacs-color-panel-target-face))

(define-key global-map [ns-change-color] 'aqumacs-set-face)
(define-key global-map [M-ns-change-color] 'aqumacs-set-face-background)
(define-key global-map [ns-drag-color] 'ns-set-foreground-at-mouse)
(define-key global-map [M-ns-drag-color] 'ns-set-background-at-mouse)

(defun aqumacs-set-face ()
  "Respond to changeColor: event."
  (interactive)
  (aquamacs-set-face-prop (or aquamacs-color-panel-target-face 'default)
			  aquamacs-color-panel-target-frame
			  (or aquamacs-color-panel-target-prop 
			      (if ns-input-color :foreground :background))
			  (or ns-input-color ns-input-background-color)))

(defun aqumacs-set-face-background ()
  "Respond to changeColor: event."
  (interactive)
  (aquamacs-set-face-prop (or aquamacs-color-panel-target-face 'default)
			  aquamacs-color-panel-target-frame
			  :background ns-input-color))

(defun aquamacs-set-face-prop (face frame prop value)
  (unless (eq aquamacs-color-panel-target-face 'none) ;; general color picker used
    (apply 'set-face-attribute face
	   frame (list prop value))
    (aquamacs-autoface-mark-face-to-save face)
    (let ((spec
    	   (list (list t (face-attr-construct face)))))
      (put face 'customized-face spec)
      (put face 'saved-face spec)
      (custom-push-theme 'theme-face face 'user 'set spec)
      (put face 'face-modified nil))
    (message "%s set for %s face."
	     (cond ((eq prop :foreground)
		    "Foreground color")
		   ((eq prop :background)
		    "Background color")
		   (t "Color"))
	     face)))


(defvar mac-font-panel-target-face 'default)
(defvar mac-font-panel-target-frame nil)

(defun aquamacs-popup-font-panel ()
  "Show font panel."
  (interactive)
  (setq mac-font-panel-target-frame nil
	aquamacs-color-panel-target-frame nil)
  (setq mac-font-panel-target-face (aquamacs-default-face-in-effect)
	aquamacs-color-panel-target-face (aquamacs-default-face-in-effect))
  (setq ns-input-font (face-font (aquamacs-default-face-in-effect)))
  ;; cannot set correct font yet  (to do!)
  ;; Fns_popup_font_panel sets frame font here
  (ns-popup-font-panel mac-font-panel-target-frame mac-font-panel-target-face)
  (message "Choose Font for %s face." mac-font-panel-target-face))



(defun ns-respond-to-change-font ()
  "Respond to changeFont: event, expecting ns-input-font and
ns-input-fontsize of new font."
  (interactive)
  (let ((face (or mac-font-panel-target-face 'default))

	(attribute-values (list :family ns-input-font
				:height (* 10 ns-input-fontsize))))

    (apply 'set-face-attribute face
	   mac-font-panel-target-frame attribute-values)
    (aquamacs-autoface-mark-face-to-save face)
    (let ((spec
    	   (list (list t (face-attr-construct face)))))
      (put face 'customized-face spec)
      (put face 'saved-face spec)
      (custom-push-theme 'theme-face face 'user 'set spec)
      (put face 'face-modified nil))
    (message "Font set for %s face." face)))


) ;; when window-system

;; ZOOM


(defvar zoom-font-frame-local-flag t
  "* Font zoom is specific for the frame
Setting this is currently slightly error-prone.")
; (setq zoom-font-frame-local-flag t)
(defun zoom-font (&optional dir)
 "Zoom default face in current buffer.
WIth prefix argument DIR, shrink the face; otherwise enlarge.
`zoom-font-frame-local-flag' indicates whether the zoom is local
to the selected frame."
  (interactive "P")
  ;; the Zoom is per buffer and per frame.
  ;; we can't set the frame font or change the frame's or a global default face,
  ;; because this can get remapped via face-remapping-alist
  ;; thus, we must go through face-remapping-alist, which will make this setting
  ;; local to the specific buffer.
  ;; to allow
  (let ((factor-delta (if (listp last-input-event)
			  (if dir -0.1 0.1) ;; mouse wheel event
			(if dir -0.2 0.2)))
	(frame (if zoom-font-frame-local-flag (selected-frame) nil))
	(default-face 'default)
	(zoom-face nil))

    ;; set default-face to the face that default remaps to,
    ;; and find the zoom face.
    (mapc
     (lambda (entry)
       (when (and (eq (car entry) 'default)
		(symbolp (cdr entry)))
	 (if (string-match "zoom-.*" (symbol-name (cdr entry)))
	     (setq zoom-face (intern (match-string 0 (symbol-name (cdr entry)))))
	   (if (and (eq default-face 'default) ;; choose the first matching entry
		    (not (string-match "zoom-.*" (symbol-name (cdr entry)))))
	   (setq default-face (cdr entry))))))
     face-remapping-alist)

    (unless zoom-face (setq zoom-face (intern (make-temp-name "zoom-"))))
    (unless (facep zoom-face)
      (make-empty-face zoom-face)
      (set-face-documentation
       zoom-face
       (purecopy "Zoom face.")))
    (set-face-attribute zoom-face nil :inherit default-face) ;; on all frames
    ;; we can't use the global (default) value for face-remapping-alist
    ;; because global and local faces aren't merged in the same way.
    (let ((zoom-factor (face-attribute zoom-face :height frame nil))
	  (alist-entry (cons 'default zoom-face)))
      (if (or (not (member alist-entry face-remapping-alist)) (eq 'unspecified zoom-factor))
	  (setq zoom-factor 1.0))

      (setq zoom-factor (/ (round (+ zoom-factor factor-delta) 0.01) 100.0))
      ;; to do; only do this if no other frame shows it.
      (when (> zoom-factor 0.0)
	(if (= zoom-factor 1.0)
	    ;; remove zoom completely from face-remapping-alist
	    (setq face-remapping-alist (delete alist-entry face-remapping-alist))
	  (set-face-attribute zoom-face frame :height zoom-factor)
	  (unless (member alist-entry face-remapping-alist)
	    (setq face-remapping-alist (cons alist-entry face-remapping-alist))))
	(message "Zoom: %0d%%" (* 100 zoom-factor))))))

(defun zoom-font-out ()
  "Shrink face in current buffer.
`zoom-font-frame-local-flag' indicates whether the zoom
is local to the selected frame."
  (interactive)
  (zoom-font t))

(defun zoom-font-off ()
  "End zoom face in current buffer."
  (interactive)
  (let ((zoom-face nil))
    (mapc
     (lambda (entry)
       (when (and (eq (car entry) 'default)
		  (symbolp (cdr entry)))
	 (if (string-match "zoom-.*" (symbol-name (cdr entry)))
	     (setq zoom-face (intern (match-string 0 (symbol-name (cdr entry))))))))
     face-remapping-alist)
    (when zoom-face
      (setq face-remapping-alist (delete (cons 'default zoom-face) face-remapping-alist))
      (redisplay)
      (if (interactive-p)
	  (message "Zoom off.")))))

(defadvice mac-handle-font-selection
  (before zoom-off () activate)
  (if mac-font-panel-target-frame
      (if (frame-live-p mac-font-panel-target-frame)
	  (select-frame mac-font-panel-target-frame)
	(setq mac-font-panel-target-frame nil)))
  (zoom-font-off))

;; turn on if desired
(if aquamacs-autoface-mode
    (aquamacs-autoface-mode 1))

(provide 'aquamacs-autoface-mode)


;; To do: use something aquamacs-default-face-in-effect to determine the target face for the font.
