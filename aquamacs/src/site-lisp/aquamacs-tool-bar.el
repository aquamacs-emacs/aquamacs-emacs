;; This is the Aquamacs Toolbar package.

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
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
;; (defun aq-list-has-property-element (list elem &optional default)
;;   (let ((ret default))
;;     (and (listp list)
;; 	 (or
;; 	  (when (eq (car-safe list) elem)
;; 	    (setq ret (car-safe (cdr-safe list)))
;; 	    t)
	  
;; 	  (setq ret (if (cdr-safe list) (aq-list-has-property-element (cdr-safe list) elem default) default))))
;;     ret))

(defun aq-list-has-property-element (list elem &optional default)
  (let ((l2 list) (ret default))   
    (while (and l2 (listp l2))
      (when (eq (car l2) elem)
  	(setq ret (car-safe (cdr l2))
  	      ;; stop iteration
  	      l2 nil))
      (setq l2 (cdr-safe l2)))
    ret))
    
    
 


; (aq-list-has-property-element2 (list :hello 1 :you "me") :you 'not-there)

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

(defun aq-list-remove-property (prop list)
  "delete a property with its value."
  (let ((n 0))
    (while (and (listp list) (< n (length list)))

      (if (eq (nth n list) prop)
	  (setf (nthcdr n list) (nthcdr (+ 2 n) list)))
      (incf n)))
  list)

;; (setq ll '(2313 88 36 :v nil))
;; (aq-list-remove-property :v ll)
;; ll

;; (setq ll '(1 2 3 4))
;; (setf (nthcdr 3 ll) t)


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

  (tool-bar-add-item-from-menu 'new-empty-buffer-other-frame "new" nil :label "New")

  (tool-bar-add-item-from-menu 'mac-key-open-file "open" nil :label "Open")

  (tool-bar-add-item "history" (lambda ()
				 (interactive)
				 (popup-menu (easy-menu-filter-return
					      (recentf-make-menu-items)
					      "Open Recent")))
		     'recent-files
		     :label "Recent"
		     :visible '(and (boundp 'recentf-mode) recentf-mode)
		     :help "Pop up the Recent Files menu")

  (tool-bar-add-item "circle_stop" 'kill-this-buffer  'kill-current-buffer
		     :label "Close"
		     :visible '(or (not (boundp 'one-buffer-one-frame-mode))
				   (not one-buffer-one-frame-mode)))
 
  (tool-bar-add-item-from-menu 'revert-buffer "update" nil :label "Revert")
  
  (tool-bar-add-item "save" 'mac-key-save-file 'save-file
		     :label "Save"
		     :visible '(and buffer-file-name
				    (not (eq 'special
					     (get major-mode
						  'mode-class)))))
  (tool-bar-add-item "saveas" 'mac-key-save-file-as 'save-file-as
		     :label "Save"
		     :visible '(and (not buffer-file-name)
				    (not (eq 'special
					     (get major-mode
						  'mode-class)))))
  ;; Save and Save As are both called "Save" in order to keep the width the same:
  ;; these icons just get swap as soon as there is a buffer-file-name.

  (tool-bar-add-item-from-menu 'aquamacs-print "print" nil :label "Print")

  (tool-bar-add-item "space2" nil 'space-1 :label "--" :enable nil )

  (tool-bar-add-item-from-menu 'aquamacs-undo "undo" nil
			       :label "Undo"
			       :visible '(not (eq 'special (get major-mode
	  							'mode-class))))

  (tool-bar-add-item-from-menu 'aquamacs-redo "redo" nil
			       :label "Redo"
			       :visible '(not (eq 'special (get major-mode
	  							'mode-class))))
  
  (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [cut])
			       "cut" nil
			       :label "Cut"
			       :visible '(not (eq 'special (get major-mode
								'mode-class))))
  (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [copy])
			       "copy" nil
			       :label "Cut")
  (let ((cua-mode nil))
      (tool-bar-add-item-from-menu (lookup-key menu-bar-edit-menu [paste])
			       "paste" nil
			       :label "Paste"
			       :visible '(not (eq 'special (get major-mode
								'mode-class)))))
  (tool-bar-add-item-from-menu 'isearch-forward "search" nil :label "Search")
;; nonincremental-search-forward
  (tool-bar-add-item-from-menu 'ispell-buffer "spellcheck" nil :label "Spelling")

  ;; There's no icon appropriate for News and we need a command rather
  ;; than a lambda for Read Mail.
  ;;(tool-bar-add-item-from-menu 'compose-mail "mail/compose")

  (tool-bar-add-item "space2" nil 'space-2 :label "--" :enable nil)
  
  (tool-bar-add-item-from-menu 'make-frame-command "new_window" nil
			       :label "Duplicate")

  (tool-bar-add-item "preferences" 'customize 'customize
		     :label "Preferences"
		     :help "Edit preferences (customize)")

  (tool-bar-add-item "help" (lambda ()
			      (interactive)
			      (popup-menu menu-bar-help-menu))
		     'help
		     :label "Help"
		     :help "Pop up the Help menu")

  ;; Toolbar button, mapped to handle-toggle-tool-bar in tool-bar.el
  ;; (Toolbar button - on systems that support it!)
  (global-set-key [toggle-frame-toolbar] 'handle-toggle-tool-bar)


  (defvar aquamacs-default-toolbarx-meaning-alist
    (aquamacs-toolbar-x-create-meaning-list tool-bar-map)
    "Contains Aquamacs' default toolbar buttons as a meaning list for toolbar-x.
The contents of this variable are generated from `tool-bar-map'. 
Changes to this variable will have no immediate effect.

This variable is used in the AUCTeX configuration.")
  )

;; To Do:
;; these hashes are probably not very reliable
;; but how can we identify toolbars otherwise?
(defvar aquamacs-tool-bar-user-customization nil)
(defun store-tool-bar-configuration (config)
  (assq-set (tool-bar-hash) config
	    'aquamacs-tool-bar-user-customization)
  ;; ensure it doesn't get too big
  (if (nthcdr 12 aquamacs-tool-bar-user-customization)
      (set (nthcdr 12 aquamacs-tool-bar-user-customization) nil)))

(defun restore-tool-bar-configuration ()
  (let ((stored (assq (tool-bar-hash) 
		      aquamacs-tool-bar-user-customization)))
    (if stored
	(set-tool-bar-configuration (cdr stored)))))
 
(defvar aq-last-tool-bar-map nil)
; (defvar aq-last-tool-bar-config nil)
(defun maybe-restore-tool-bar-configuration (&optional force)
  (let ((menu-bar-update-hook))
    (mapc
     (lambda (f)
       (with-current-buffer
	   (window-buffer (frame-selected-window f))
	 (let ((tb-hash (sxhash (cons tool-bar-map aquamacs-tool-bar-user-customization))))
	   (if (or force (not (eq aq-last-tool-bar-map tb-hash)))
	       (progn
		 (set (if (local-variable-p 'tool-bar-map) (make-local-variable 'aq-last-tool-bar-map)
			'aq-last-tool-bar-map)
		      tb-hash)
		 (restore-tool-bar-configuration))
	   ;; did user change the toolbar somehow?
	   ;; this is still not quick enough 
	   ;; but it is the only chance we'll detect Command-Option drags of
	   ;; toolbar items.
	   ;; disabled for now: update-* is potentially dangerous
	   ;; and we don't want to run it all the time
	   ;; but only upon receiving the change event.
	   ;; (when (not (eq aq-last-tool-bar-config (ns-tool-bar-configuration)))
	   ;;   (set (if (local-variable-p 'tool-bar-map)
	   ;; 	      (make-local-variable  'aq-last-tool-bar-config)
	   ;; 	    'aq-last-tool-bar-config)
	   ;; 	  (ns-tool-bar-configuration))
	   ;;   (update-tool-bar-from-user-configuration))
	   ))))

     (visible-frame-list))))

(defun tool-bar-hash ()
  (sxhash (sort (apply #'append
		       (mapcar
			(lambda (m)
			  (when (and (consp m)
				     (not (equal (car-safe (cdr-safe (cdr-safe m)))
						 "--")))
			    (list (car m))))
			tool-bar-map)) 'string<)))

(defconst tool-bar-user-visible t)
(defconst tool-bar-user-invisible nil)

(defun tool-bar-maybe-set-visibility (menu-item value &optional show-messsage)
  "Maybe set visibility in menu-item
If there is a non-user-supplied visibility term, leave it.
If there is a user-supplied visibility term, set it."

  (let ((target-value (if value 'tool-bar-user-visible 'tool-bar-user-invisible))
	(current-value (aq-list-has-property-element menu-item :visible 'not-found)))
    ;; (message "%s %s %s" (car menu-item) current-value target-value)
    (prog1
	(cond 
	 ((eq current-value 'not-found)
	  ;; no :visible property present
	  (append menu-item `(:visible (and ,target-value))))
	 ((and (consp current-value) (eq 'and (car current-value)) 
	       (memq (nth 1 current-value) '(tool-bar-user-visible tool-bar-user-invisible)))
	  ;; previous user-configuration given
	  ;; override
	  (append (aq-list-remove-property :visible menu-item)
		  `(:visible (and ,target-value ,@(cddr current-value)))))
	 (t  
	  ;; conditioned externally.
	  
	  (if (eval current-value) ;; previously visible?
	      (if value 
		  ;; and still visible.  don't touch it.
		  menu-item
		;; user has removed item: inhibit it.
		(append (aq-list-remove-property :visible menu-item)
			`(:visible (and ,target-value ,current-value))))
	    ;; else: externally invisible
	    (if value 
		;; user is showing an externally invisible item.
		;; maybe warn.
		;; but do not make visible
		menu-item
	      ;; previously invisible, still invisible
	      ;; user can't have removed it.  don't touch
	      menu-item))))

;; there is still one problem:
;; if user adds items that shouldn't remain in the toolbar (because :visible evals to nil)
;; then the backend doesn't recognize that the toolbar structure needs to be updated.
;; I guess this is because it thinks that the item wasn't visible in the first place
;; (since the Cocoa palette has made it visible.)  
      ;; (and show-message
      ;; 	   (not (eq value (eval 
)))

 
(defun set-tool-bar-configuration (config &optional show-message)
  (let ((space-idx 0))
    (setq tool-bar-map
	  (append (make-sparse-keymap)
		  (apply 
		   #'append
		   (mapcar
		    (lambda (key)
		      (if key
			  (if (assq key tool-bar-map)
			      (list (tool-bar-maybe-set-visibility (assq key tool-bar-map) t show-message))
			    (and show-message
				 (message "Toolbar item \"%s\" not available here." key)
				 nil))
			`((,(intern (format "space-%s" (incf space-idx)))
			   menu-item "--" nil :enable nil))))
		    config))
		  (apply 
		   #'append
		   (mapcar
		    (lambda (item)
		      (unless (or (memq (or (car-safe item) item) ;;needed?
					config) ;; hidden?
				  (equal (car-safe (cdr-safe (cdr-safe item)))
					 "--"))
			(list 
			 (tool-bar-maybe-set-visibility item nil show-message)
			 )))
		    (cdr tool-bar-map)))))))

;; when global-set-key
;; ensure that frame parameter is correct 
(defun update-tool-bar-from-user-configuration (&optional frame)
  (interactive)
  (let ((user-config (ns-tool-bar-configuration)))

    (with-current-buffer (window-buffer 
			  (frame-selected-window (or frame (selected-frame))))
      (set-tool-bar-configuration user-config 'show-msg)
      (store-tool-bar-configuration user-config))))

 
(add-hook 'menu-bar-update-hook 'maybe-restore-tool-bar-configuration)
(define-key global-map [ns-tool-bar-customized] 'update-tool-bar-from-user-configuration)


(defun aquamacs-toolbar-update-showhide-menu ())



;; this is a complicated method
;; it probably moves around the invisible items quite a bit
;; so it's not worth the hassle
;; (defun update-tool-bar-from-user-configuration (&optional frame)

;;   (let ((user-config (ns-tool-bar-configuration))
;; 	(new-tool-bar nil)
;; 	(store nil)
;; 	(uc-index 0))
;;     (with-current-buffer (window-buffer (frame-selected-window (or frame (selected-frame))))

;;       (let ((tb (copy-tree (cdr tool-bar-map))))
;; 	;; first set visibility
;; 	;; changes tool-bar-map in place
;; 	(mapc
;; 	 (lambda (item)
;; 	   (if (consp item)

;; 	       (let* ((vise (aq-list-has-property-element item :visible t))
;; 		      (vis (eval vise))
;; 		      (cvis (memq (car item) user-config)))

;; 		 (unless (eq vis cvis)
;; 		   (aq-list-set-property-element item :visible (if cvis t nil)))
		 
;; 		 (while (and (not (eq (car item) (nth uc-index user-config)))
;; 			     cvis
;; 			     (assq (nth uc-index user-config) store)) ; retrievable?
;; 		   ;; retrieve from store
;; 		   (setq new-tool-bar (cons (cdr (assq (nth uc-index user-config) store))
;; 					    new-tool-bar))
;; 		   (assq-delete-all (nth uc-index user-config) 'store)
;; 		   (incf uc-index))
;; 		 (print store)
;; 		 (if (and (not (eq (car item) (nth uc-index user-config)))
;; 			  cvis)
;; 		     ;; put in storage
;; 		     (setq store (cons item store))
;; 		   ;; else
;; 		   (incf uc-index)
;; 		   (setq new-tool-bar (cons item new-tool-bar))))
;; 	     (setq new-tool-bar (cons item  new-tool-bar))))
;; 	 tb)
	
;; 	(setq tool-bar-map2
;; 	      (append (make-sparse-keymap)
;; 		      (reverse new-tool-bar)))))))
  


(provide 'aquamacs-tool-bar)