;; This is the Aquamacs Toolbar package.

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-tool-bar.el,v 1.34 2008/08/10 13:53:45 davidswelt Exp $ 

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
 
;; Copyright (C) 2005,2006, 2007: David Reitter



(require 'aquamacs-tools)

; go over tool-bar-map to find out what's in there
;; unlik plist-get/put, these will work
(defun aq-list-has-property-element (list elem &optional default)
  (let ((ret default))
    (and (listp list)
	 (or
	  (when (eq (car-safe list) elem)
	    (setq ret (car-safe (cdr-safe list)))
	    t)
	  
	  (setq ret (if (cdr-safe list) (aq-list-has-property-element (cdr-safe list) elem default) default))))
    ret))
; (aq-list-has-property-element (list :hello 1 :you "me") :yoou 'not-there)

(defun aq-list-set-property-element (list elem value)
  (and (listp list)
       (or
	(if (not (eq (car-safe list) elem))
	    (if (cdr-safe list) 
		(aq-list-set-property-element (cdr-safe list) elem value)
	      (if (consp list)
		  (setcdr list (list elem value))))
	  ;; set the cdr
	  (setcdr list (cons value (cdr-safe (cdr-safe list))))))) ;overwrite
  list)

(defvar aquamacs-menu-bar-showhide-toolbar-items-menu (make-sparse-keymap)
"Keymap with items that allow toggling items on the tool-bar.")
(defvar aquamacs-menu-bar-showhide-toolbar--hash nil)


;(require 'aquamacs-menu) ; for aquamacs-pretty-mode-name
(defun aquamacs-toolbar-update-showhide-menu ()
  "Updates the toolbar items toggle menu.
This will update the keymap `aquamacs-menu-bar-showhide-toolbar-items-menu'
if `tool-bar-map' has changed since the last call.  If not, this returns
quickly."

(when (boundp 'aquamacs-menu-bar-showhide-toolbar-items-menu)

  ;; has the tool-bar-map changed?
  (when (not (equal aquamacs-menu-bar-showhide-toolbar--hash
		    (sxhash tool-bar-map)))
    (setq aquamacs-menu-bar-showhide-toolbar-items-menu (make-sparse-keymap))
    (mapc
     (lambda (item)
       (and 
	(car item)
	(if (nth 3 item)
	    (let* ((name (let ((osx-key-mode)) 
			   (eval (car (cdr (cdr item))))))
		   (item-is-local (eq (type-of (variable-binding-locus 'tool-bar-map)) 'buffer))
		   (local-var-str (if item-is-local (symbol-name major-mode) ""))
		   (toggle-var (intern 
				(format "toolbar-menu-show-%s-%s" 
					local-var-str (car item)))))
	      (eval 
	       `(defcustom ,toggle-var t 
		  (format 
		   "If nil, the \"%s\" icon will be hidden from the toolbar." 
		   name) 
		  :group 'Aquamacs :group 'tool-bar :version "22.0" 
		  :type 'boolean))
	      (if (boundp 'aquamacs-menu-bar-customize-options-to-save)
		  (add-to-list 'aquamacs-menu-bar-customize-options-to-save
			       toggle-var 'append 'eq))
	      
	      (define-key aquamacs-menu-bar-showhide-toolbar-items-menu 
		(vector toggle-var)
		(eval
		 `(menu-bar-make-toggle 
		   ,(intern (format "toggle-toolbar-show-%s-%s" local-var-str (car item)))
		   ,toggle-var
		   ,(format "%s%s" name (if item-is-local 
					    (format " (%s)" 
						    (aquamacs-pretty-mode-name major-mode)) 
					  ""))
		   (if (eval ,(aq-list-has-property-element 
			       item 
			       :visible t))
		       ,(format "Toolbar icon for %s %%s" name)
		     (message
		      (format
		       "Item not visible due to current configuration or state:
%s"  
		       (quote ,(aq-list-has-property-element item :visible)))))
		   "Show an icon in the toolbar for this function."
		   )))
	      (let ((l (aq-list-has-property-element item :visible 'vis-missing) ));;(aq-list-has-property-element item :visible)))
		(if (not (eq l 'vis-missing))
		    ;; check if variable influences this
		    (unless (or (eq l toggle-var)
				(and (eq (car-safe l) 'and) 
				     (eq (car-safe (cdr-safe l)) toggle-var)))
		      (setq l `(and ,toggle-var ,l)))
		  (setq l toggle-var))
		(aq-list-set-property-element item :visible l)
		;; (define-key tool-bar-map (vector (car item))
;; 		  (aq-list-set-property-element item :visible l))
))
	  (define-key aquamacs-menu-bar-showhide-toolbar-items-menu 
	    (vector (intern (format "%s-sep" (car item)))) 
	    '(menu-item "--" nil)))))
     (reverse (cdr tool-bar-map))) 
    (define-key-after menu-bar-showhide-menu [small]
      `(menu-item "Toolbar Items" 
		  ,aquamacs-menu-bar-showhide-toolbar-items-menu
		  :help "Select items to show in the toolbar"
		  :visible ,(display-graphic-p))
      'showhide-tool-bar)
    (setq aquamacs-menu-bar-showhide-toolbar--hash
	  (sxhash tool-bar-map)))))


;;  (remove-hook 'menu-bar-update-hook 'aquamacs-toolbar-update-showhide-menu)
;; (aquamacs-tool-bar-setup)
 ;; (progn  (setq aquamacs-menu-bar-showhide-toolbar--hash 0) (aquamacs-toolbar-update-showhide-menu))
;; 

(defun aquamacs-toolbar-x-create-meaning-list (keymap)
  "Creates a meaning list for `toolbar-x' from a toolbar keymap."
;; FIXME: is "sep" from AUCTeX?
  (let ((meaning '((bar-separator :image "space2" :command (lambda nil (interactive) t) :enable nil :help ""))))
  (mapc
   (lambda (item)
     (and (car item)
	  (add-to-list 
	   'meaning
	   (if (nth 3 item)
		;; go over all properties of item
		(let ((img (aq-list-has-property-element item :image))
		      (vis (aq-list-has-property-element item :visible 'none))
		      (ena (aq-list-has-property-element item :enable 'none)))
		  (list (car item) 
			:title (nth 2 item)
			:command (nth 3 item)
		  :visible (if (eq vis 'none) t vis)
		  :enable (if (eq ena 'none) t ena)
		  :image (vector img (elt img 1)) ;; 1st: Emacs, 2nd: XEma
		  :help (aq-list-has-property-element item :help)))
	     (let ((img (aq-list-has-property-element item :image)))
		  (list 'separator
			:command (lambda nil (interactive) t)
			:visible t
			:enable nil
			:image (vector img img) ;; 1st: Emacs, 2nd: XEma
			:help ""))))))
   (reverse (cdr keymap))) meaning))
 
;; this to overwrite the tool-bar setup function
;  (aquamacs-tool-bar-setup)
(defun aquamacs-tool-bar-setup ()
  ;; People say it's bad to have EXIT on the tool bar, since users
  ;; might inadvertently click that button.
  ;;(tool-bar-add-item-from-menu 'save-buffers-kill-emacs "exit")
  (setq tool-bar-map (make-sparse-keymap))
 
  (face-spec-set 'tooltip '((t (:inherit variable-pitch 
				:background "lightyellow" 
				:foreground "black" 
				:height 100 
				:family "lucida sans"))) nil)

  (aquamacs-set-defaults '((auto-resize-tool-bar nil)))

  (let ((face 'tool-bar)
	;; e2e2e2 is eaeaea in imagemagick for some reason
	(spec '((t (:background "#eaeaea" :foreground "black" 
				:box (:line-width 1 :style released-button))))))
    (face-spec-set face spec nil)
    (put face 'face-defface-spec spec))
  ;; will be overwritten by color themes

  (tool-bar-add-item-from-menu 'new-empty-buffer-other-frame '("new" . "New"))
  (tool-bar-add-item-from-menu 'mac-key-open-file '("open" . "Open"))

  (tool-bar-add-item '("history" . "Recent") (lambda ()
			      (interactive)
			      (popup-menu (easy-menu-filter-return
					   (recentf-make-menu-items)
					   "Open Recent")))
		     'recent-files
		     :visible '(and (boundp 'recentf-mode) recentf-mode)
		     :help "Pop up the Recent Files menu")

  (tool-bar-add-item '("circle_stop" . "Close") 'kill-this-buffer  'kill-current-buffer
		     :visible '(or (not (boundp 'one-buffer-one-frame-mode))
				   (not one-buffer-one-frame-mode)))
 
  (tool-bar-add-item-from-menu 'revert-buffer '("update" . "Revert") nil)
  
  (tool-bar-add-item-from-menu 'save-buffer '("save" . "Save") nil
			       :visible '(and buffer-file-name
					     (not (eq 'special
						      (get major-mode
							   'mode-class)))))
  ;; Save and Save As are both called "Save" in order to keep the width the same:
  ;; these icons just get swap as soon as there is a buffer-file-name.
  (tool-bar-add-item-from-menu 'write-file '("saveas" . "Save") nil  
			       :visible '(and (not buffer-file-name)
					     (not (eq 'special
						      (get major-mode
  							   'mode-class)))))

  (tool-bar-add-item-from-menu 'aquamacs-print '("print" . "Print"))

  (tool-bar-add-item '("space2" . "--") nil 'space-1 :enable nil )

  (tool-bar-add-item-from-menu 'aquamacs-undo '("undo" . "Undo") nil
			       :visible '(not (eq 'special (get major-mode
	  							'mode-class))))

  (tool-bar-add-item-from-menu 'aquamacs-redo '("redo" . "Redo") nil
			       :visible '(not (eq 'special (get major-mode
	  							'mode-class))))
  
  (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [cut])
			       '("cut" . "Cut") nil
			       :visible '(not (eq 'special (get major-mode
								'mode-class))))
  (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [copy])
			       '("copy" . "Copy"))
  (let ((cua-mode nil))
      (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [paste])
			       '("paste" . "Paste") nil
			       :visible '(not (eq 'special (get major-mode
								'mode-class)))))
  (tool-bar-add-item-from-menu 'isearch-forward '("search" . "Search"))
;; nonincremental-search-forward
  (tool-bar-add-item-from-menu 'ispell-buffer '("spellcheck" . "Spelling"))

  ;; There's no icon appropriate for News and we need a command rather
  ;; than a lambda for Read Mail.
  ;;(tool-bar-add-item-from-menu 'compose-mail "mail/compose")



  (tool-bar-add-item '("space2" . "--") nil 'space-2 :enable nil)
  
  (tool-bar-add-item-from-menu 'make-frame-command '("new_window" . "Duplicate") nil)

  (tool-bar-add-item '("preferences" . "Preferences") 'customize 'customize
		     :help "Edit preferences (customize)")

  (tool-bar-add-item '("help" . "Help") (lambda ()
			      (interactive)
			      (popup-menu menu-bar-help-menu))
		     'help
		     :help "Pop up the Help menu")

  ;; Toolbar button, mapped to handle-toggle-tool-bar in tool-bar.el
  ;; (Toolbar button - on systems that support it!)
  (global-set-key [toggle-frame-toolbar] 'handle-toggle-tool-bar)

  (aquamacs-toolbar-update-showhide-menu) 
  (add-hook 'menu-bar-update-hook 'aquamacs-toolbar-update-showhide-menu)

  (aquamacs-set-defaults '(
			   (toolbar-menu-show--aquamacs-print nil)
			   (toolbar-menu-show--copy t)
			   (toolbar-menu-show--customize nil)	   
			   (toolbar-menu-show--cut t)
			   (toolbar-menu-show--help t)		   
			   (toolbar-menu-show--new-file t)	   
			   (toolbar-menu-show--open-file t)
			   (toolbar-menu-show--paste t)	   
			   (toolbar-menu-show--redo t)
			   (toolbar-menu-show--save-buffer t)	   
			   (toolbar-menu-show--undo t)
			   (toolbar-menu-show--write-file t)
			   (toolbar-menu-show--recent-files t)
			   (toolbar-menu-show--isearch-forward nil)))


  (defvar aquamacs-default-toolbarx-meaning-alist
    (aquamacs-toolbar-x-create-meaning-list tool-bar-map)
    "Contains Aquamacs' default toolbar buttons as a meaning list for toolbar-x.
The contents of this variable are generated from `tool-bar-map'. 
Changes to this variable will have no immediate effect.

This variable is used in the AUCTeX configuration.")
  )


(provide 'aquamacs-tool-bar)