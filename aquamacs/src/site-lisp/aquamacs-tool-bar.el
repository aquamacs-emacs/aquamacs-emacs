;; This is the Aquamacs Toolbar package.

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-tool-bar.el,v 1.9 2007/04/21 14:24:46 davidswelt Exp $ 

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





; go over tool-bar-map to find out what's in there

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


(defvar aquamacs-menu-bar-showhide-toolbar-items-menu (make-sparse-keymap)
"Keymap with items that allow toggling items on the tool-bar.")

(defun aquamacs-toolbar-create-showhide-menu ()
  "Updates the toolbar items toggle menu.
This will update the keymap `aquamacs-menu-bar-showhide-toolbar-items-menu'."
  (setq aquamacs-menu-bar-showhide-toolbar-items-menu (make-sparse-keymap))
  (mapc
   (lambda (item)
     (and (car item)
	  (if (nth 3 item)
	      (let ((name (let ((osx-key-mode)) (eval (car (cdr (cdr item))))))
		    (toggle-var (intern (format "toolbar-menu-show--%s" (car item)))))
		(eval `(defcustom ,toggle-var nil 
			 (format "If non-nil, an icon for \"%s\" is shown in the toolbar." name) 
			 :group 'Aquamacs :group 'tool-bar :version "22.0" :type 'boolean))
		(add-to-list 'aquamacs-menu-bar-customize-options-to-save
			     toggle-var 'append)
		(define-key aquamacs-menu-bar-showhide-toolbar-items-menu (vector toggle-var)
		  (eval `(menu-bar-make-toggle 
			  ,(intern (format "toggle-toolbar-show--%s" (car item)))
			  ,toggle-var
			  ,(format "%s" name)
			  (if (eval ,(aq-list-has-property-element item :visible t))
			      ,(format "Toolbar icon for %s %%s" name)
			      (message (format "Item not visible due to current configuration or state:
%s"  (quote ,(aq-list-has-property-element item :visible)))))
			  "Show an icon in the toolbar for this function."
			  )))
		(let ((l (aq-list-has-property-element item :visible)))
		  (if l
		      (setq l `(and ,l ,toggle-var))
		    (setq l toggle-var))
	 
		  (define-key tool-bar-map (vector (car item))
		    (append (cdr item)
			    `(:visible ,l)))))
	    (define-key aquamacs-menu-bar-showhide-toolbar-items-menu 
	      (vector (intern (format "%s-sep" (car item)))) 
	      '(menu-item "--" nil)))))
   (reverse (cdr tool-bar-map))) nil)


(defun aquamacs-toolbar-x-create-meaning-list (keymap)
  "Creates a meaning list for `toolbar-x' from a toolbar keymap."
;; FIXME: is "sep" from AUCTeX?
  (let ((meaning '((bar-separator :image "sep" :command t :enable nil :help ""))))
  (mapc
   (lambda (item)
     (and (car item)
	  (add-to-list 
	   'meaning
	   (if (nth 3 item)
		;; go over all properties of item
		(let ((img (aq-list-has-property-element item :image)))
		  (list (car item) 
			:command (nth 3 item)
		  :visible (aq-list-has-property-element item :visible)
		  :enable (aq-list-has-property-element item :enable)
		  :image (vector img (elt img 1)) ;; 1st: Emacs, 2nd: XEma
		  :help (aq-list-has-property-element item :help)))
	     (let ((img (aq-list-has-property-element item :image)))
		  (list 'separator
			:command t
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

  (tool-bar-add-item-from-menu 'new-frame-with-new-scratch "new")
  (tool-bar-add-item-from-menu 'mac-key-open-file "open")

  (tool-bar-add-item "history" (lambda ()
			      (interactive)
			      (popup-menu (easy-menu-filter-return
					   (recentf-make-menu-items)
					   "Open Recent")))
		     'recent-files
		     :visible '(and (boundp 'recentf-mode) recentf-mode)
		     :help "Pop up the Recent Files menu")

  (tool-bar-add-item "circle_stop" 'kill-this-buffer  'kill-current-buffer
		     :visible '(or (not (boundp 'one-buffer-one-frame-mode))
				   (not one-buffer-one-frame-mode)))
 
  (tool-bar-add-item-from-menu 'revert-buffer "update" nil)
  
  (tool-bar-add-item-from-menu 'save-buffer "save" nil
			       :visible '(and buffer-file-name
					     (not (eq 'special
						      (get major-mode
							   'mode-class))))) 
  (tool-bar-add-item-from-menu 'write-file "saveas" nil
			       :visible '(and (not buffer-file-name)
					     (not (eq 'special
						      (get major-mode
  							   'mode-class)))))
;; taken out - not enough space in toolbar
  (tool-bar-add-item-from-menu 'aquamacs-redo "redo" nil
			       :visible '(not (eq 'special (get major-mode
	  							'mode-class))))
  
  (tool-bar-add-item "space" nil 'space-1 :enable nil )
  (tool-bar-add-item-from-menu 'aquamacs-undo "undo" nil
			       :visible '(not (eq 'special (get major-mode
	  							'mode-class))))
 

 (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [cut])
			       "cut" nil
			       :visible '(not (eq 'special (get major-mode
								'mode-class))))
  (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [copy])
			       "copy")
  (let ((cua-mode nil))
      (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [paste])
			       "paste" nil
			       :visible '(not (eq 'special (get major-mode
								'mode-class)))))
  (tool-bar-add-item-from-menu 'isearch-forward "search")
;; nonincremental-search-forward
  (tool-bar-add-item-from-menu 'ispell-buffer "spell")

  ;; There's no icon appropriate for News and we need a command rather
  ;; than a lambda for Read Mail.
  ;;(tool-bar-add-item-from-menu 'compose-mail "mail/compose")


  (tool-bar-add-item-from-menu 'aquamacs-print "print")

  (tool-bar-add-item "space" nil 'space-2 :enable nil )
  
  (tool-bar-add-item-from-menu 'make-frame-command "new_window" nil)

  (tool-bar-add-item "preferences" 'customize 'customize
		     :help "Edit preferences (customize)")

  (tool-bar-add-item "help" (lambda ()
			      (interactive)
			      (popup-menu menu-bar-help-menu))
		     'help
		     :help "Pop up the Help menu")

  ;; Toolbar button, mapped to handle-toggle-tool-bar in tool-bar.el
  ;; (Toolbar button - on systems that support it!)
  (global-set-key [toggle-frame-toolbar] 'handle-toggle-tool-bar)

  (aquamacs-toolbar-create-showhide-menu)
  (define-key-after menu-bar-showhide-menu [small]
    `(menu-item "Toolbar items" 
		,aquamacs-menu-bar-showhide-toolbar-items-menu
		:help "Select items to show in the toolbar"
		:visible ,(display-graphic-p))
    'showhide-tool-bar)
  
  (aquamacs-set-defaults '(
			   (toolbar-menu-show--aquamacs-print  t)
			   (toolbar-menu-show--copy t)
			   (toolbar-menu-show--customize t)	   
			   (toolbar-menu-show--cut t)
			   (toolbar-menu-show--help t)		   
			   (toolbar-menu-show--new-file t)	   
			   (toolbar-menu-show--open-file t)
			   (toolbar-menu-show--paste t)	   
			   (toolbar-menu-show--redo nil)
			   (toolbar-menu-show--save-buffer t)	   
			   (toolbar-menu-show--undo t)
			   (toolbar-menu-show--write-file t)
			 (toolbar-menu-show--isearch-forward t)))


  (defvar aquamacs-default-toolbarx-meaning-alist
    (aquamacs-toolbar-x-create-meaning-list tool-bar-map)
    "Contains Aquamacs' default toolbar buttons as a meaning list for toolbar-x.
The contents of this variable are generated from `tool-bar-map'. 
Changes to this variable will have no immediate effect.

This variable is used in the AUCTeX configuration.")
  )


(provide 'aquamacs-tool-bar)