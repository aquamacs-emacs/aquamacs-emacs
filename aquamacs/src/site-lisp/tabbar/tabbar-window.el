;; Tabbar-window.el --- "Window Tabs" for tabbar-mode: Tab-set is
;;   specific to each window, and tabbar is hidden when only a
;;   single tab exists for that window.  Requires that tabbar.el and
;;   aquamacs-tabbar.el be loaded first.

;; This file is part of Aquamacs Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Author: Nathaniel Cunningham <nathaniel.cunningham@gmail.com>
;; Maintainer: Nathaniel Cunningham <nathaniel.cunningham@gmail.com>
;; Created: February 2008
;; (C) Copyright 2008, 2012 the Aquamacs Project

(require 'tabbar)
(require 'aquamacs-tools)

(defvar tabbar-window-alist nil)
(defvar tabbar-window-cache nil)

;; (defcustom tabbar-window-new-buffers nil
;;   "*Specify the behavior when a new buffer is opened in tabbar-mode.
;; The following options are available:

;; - `tab'
;;     Buffer is created in current window and assigned a new tab.
;; - `no-tab'
;;     Buffer is created in current window, with no tab or tab bar; window's
;; previous tabset is deleted, although buffers are not closed or killed.
;; - default
;;     Buffer is created in a new frame.  (Lone buffers show no tabs.)"
;;   :group 'tabbar
;;   :type '(choice :tag "New buffer gets created in..."
;;                  (const :tag "Current Window with New Tab" nil)
;;                  (const :tag "Current Window without a Tab" no-tab)
;;                  (const :tag "New Frame" nil)))

;; for "buffer tabs", it makes sense to have tabbar-current-tabset always
;; buffer-local.  This is not sensible for "window tabs".  Window-local variables
;; do not exist in emacs; therefore we use frame-local.
;; Probably doesn't matter much, now that we always update tabbar-current-tabset
;; when (tabbar-current-tabset) is called.
(makunbound 'tabbar-current-tabset)
(defvar tabbar-current-tabset nil
  "The tab set currently displayed on the tab bar.")
;(make-variable-frame-local 'tabbar-current-tabset)

;; redefine tabbar-current-tabset to ALWAYS update the value
;; of tabbar-current-tabset.  Required since the same buffer can have tabs
;; in multiple tabsets.  Reasonable to do, as this does not redefine all tabsets
;; when "window tabs" are on -- see tabbar-window-current-tabset below.
(defun tabbar-current-tabset (&optional update)
  "Return the tab set currently displayed on the tab bar.
If optional argument UPDATE is non-nil, call the user defined function
`tabbar-current-tabset-function' to obtain it.  Otherwise return the
current cached copy."
  (setq tabbar-current-tabset
	(if tabbar-current-tabset-function (funcall tabbar-current-tabset-function))))

(defun tabbar-window-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude internal buffers."
  (apply #'nconc 
	 (mapcar
	  (lambda (b)
	    (cond
	     ((string= (substring (buffer-name b) 0 1) " ")  ; and (null buffer-file-name)
	      nil)
	     ((buffer-live-p b) (list b))))
	  (buffer-list))))

(defun window-number (window)
  "Return window ID as a number."
  (string-to-number
   (nth 1 (save-match-data 
	    (split-string (format "%s" window)
			  "\\(<window 0x\\|<window \\| on \\)" ))) 16))

(defun window-number-list ()
  "Return IDs of all windows as list of numbers."
  (mapcar
   (lambda (w)
     (window-number w))
   (apply 'nconc
	  (mapcar (lambda (f)
		    (window-list f 'nomini (frame-first-window f)))
		  (frame-list)))))

(defun tabbar-window-alist-update (window)
  "Update the list of windows and corresponding buffers to be
shown in tabs.  Add a tabset for specified WINDOW if it does not
yet exist, and update list of tabs to include the currently
displayed buffer.  Result is an alist of alists."
  (let* ((wnumber (window-number window))
	 (buffer (window-buffer window))
	 (bufname (buffer-name buffer))
	 (bufmod (buffer-modified-p buffer))
	 ;; use buffer AND its name, so we can update it if name changes on save
	 (bufpair (list buffer bufname bufmod))
	 (window-elt (assq wnumber tabbar-window-alist))
	 (tabbar-buffers-list (and tabbar-buffer-list-function (funcall tabbar-buffer-list-function))))
    ;; only include buffers that should have tabs (ignore tooltip windows, etc.)
    (when (memq buffer tabbar-buffers-list)
      (if window-elt
	  ;;if window already included, check whether window-buffer is listed
	  ;; for this window
	  (let ((window-buffer-list (cdr window-elt)))
	    (unless (assq buffer window-buffer-list)
	      ;;add this buffer if not
	      (nconc window-buffer-list (list bufpair))
	      ))
	;; if window not included, add (window-number '(buffer . buffer-name))
	;;  to the alist
	(add-to-list 'tabbar-window-alist (cons wnumber (list bufpair)) t))))
  tabbar-window-alist)


;; avoid use of walk-windows 
;; incompatibility with ECB when called from some hook or
;; as idle timer function
(defmacro tabbar-walk-windows (fun)
  `(mapc
   ,fun
   (apply 'nconc
	  (mapcar (lambda (f)
		    (window-list f 'nomini (frame-first-window f)))
		  (frame-list)))))

(defun window-number-get-window (wnumber)
  "Return window corresponding to ID number."
  (let (win)
    (tabbar-walk-windows 
     (lambda (w)
       (if (= (window-number w) wnumber)
	   (setq win w))))
    win))
    

(defvar tabbar-display-bug-workaround nil
"Should tabbar work around a display bug?
The bug leaves horizontal lines when the window is split.
Comes with side-effects (e.g., tabbar blinking in edebug).")


(defun tabbar-window-alist-cleanup ()
  "Remove from tabbar-window-alist any elements (windows OR
buffers) that no longer exist, or buffers that don't get tabs.
Displayed buffers always get tabs."
  (let ((wnumber-list (window-number-list)))
    ;; loop through alist
    (dolist (elt tabbar-window-alist)
      (let* ((wnumber (car elt))
	     (window (window-number-get-window wnumber))
	     (buflist (cdr elt)))
	;; if the window still exists, delete any tabs as needed
	(if (memq wnumber wnumber-list)
	    ;; for extant windows, loop through buffers
	    ;; delete any that no longer exist
	    (progn
	      (dolist (thisbuffer buflist)
		(let* ((buffer (car thisbuffer)))
		  (if (buffer-live-p buffer)
		      ;; if it's a current buffer, make sure it has current buffer-name
		      (progn
			(unless (equal (buffer-name buffer) (nth 1 thisbuffer))
			  (setcar (cdr thisbuffer) (buffer-name buffer)))
			(unless (eq (buffer-modified-p buffer) (cddr thisbuffer))
			  (setcdr (cdr thisbuffer) (list (buffer-modified-p buffer)))))
		    ;; buffer is not live: remove it from buffer list for this window
		    (setq buflist (assq-delete-all buffer buflist))
		    ;; put modified list back into tabbar-window-alist for this window
		    (setcdr elt buflist))))

	      (if (and (eq (length buflist) 1)
		       ;; no other mode has installed another header line, right?
		       (eq header-line-format tabbar-header-line-format))
		  ;; if there is only 1 buffer associated with this tabset, then
		  ;;  display no tabbar (no header line).
		  ;; (add-to-list 'header-line-inhibit-window-list window)
		  ;; workaround for redisplay bug
		  (if (and tabbar-display-bug-workaround
			   (> (length (window-list (window-frame window) 'no-minibuf)) 1))
		      ;; this can cause a bit of flicker, but that's still better 
		       (run-with-idle-timer 0 nil 'add-to-list 
					    'header-line-inhibit-window-list window t)
		    (add-to-list 'header-line-inhibit-window-list window t))
		;; otherwise, ensure this window has a tabbar
		(setq header-line-inhibit-window-list
		      (delq window header-line-inhibit-window-list))))
	  ;; window doesn't exist: remove it from alist ...
	  (setq tabbar-window-alist (delq elt tabbar-window-alist))
	  ;; ... and make sure it's removed from header-line-inhibit list
	  (setq header-line-inhibit-window-list
 		(delq window header-line-inhibit-window-list))))))
  tabbar-window-alist)
   
(defun tabbar-tabset-names ()
  "Return list of strings giving names of all tabsets"
  (tabbar-map-tabsets 'symbol-name))

(defun tabbar-window-update-tabsets ()
  "Update tab sets from tabbar-window-alist.
Return the current tabset, which corresponds to (selected-window)."
  ;; run tabbar-window-alist-update for all windows
  ;; could probably change this to only windows in current frame,
  ;; since modified frame is active for 'window-configuration-change-hook
  (tabbar-walk-windows 'tabbar-window-alist-update)
  ;; (walk-windows 'tabbar-window-alist-update 'nomini t)
  ;; run tabbar-window-alist-cleanup to remove defunct entries
  (tabbar-window-alist-cleanup)
  ;; if the alist has changed, update the tab sets (compare against cache)
  (unless (equal tabbar-window-alist tabbar-window-cache)
    ;; cycle through alist.
    (dolist (elt tabbar-window-alist)
      ;; for each window group:
      (let* ((groupnum (car elt))
	     (groupname (number-to-string groupnum))
	     (buflist (cdr elt))
	     (tabset (tabbar-get-tabset groupname)))
	;; if the corresponding tabset already exists
	(if tabset
	    ;; add tabs for any buffers that arent't listed in this group in cache
	    (let ((old-buflist (cdr (assoc groupnum tabbar-window-cache))))
	      (dolist (buf buflist)
		(unless (memq buf old-buflist)
		  (tabbar-add-tab tabset (car buf) t)
		  ;;Update the tabset template when we modify the tabset
		  ;;actually don't need to here; tabbar-add-tab does that for us
		  ;;(tabbar-set-template tabset nil)
		  (tabbar-set-template tabset nil))))
	  ;; if tabset doesn't exist, create new containing first buffer
	  (tabbar-make-tabset groupname (car (car buflist)))
	  ;; get the new tabset
	  (setq tabset (tabbar-get-tabset groupname))
	  ;; then add any remaining buffers
	  (dolist (buf (cdr buflist))
	    ;; don't have to update the template, since tabset has no such prop. yet
	    (tabbar-add-tab tabset (car buf) t)))))
    ;; cycle through tabsets
    (dolist (tabset-name (tabbar-tabset-names))
      (let* ((tabset (tabbar-get-tabset tabset-name))
	     (tabset-number (string-to-number tabset-name))
	     (tabset-alist-elt (assq tabset-number tabbar-window-alist)))
	(if tabset-alist-elt
	    ;; if there is a corresponding window in tabbar-window-alist,
	    ;; cycle through tabs
	    (let ((buflist (cdr tabset-alist-elt)))
	      (dolist (tab (tabbar-tabs tabset))
		;; delete any tabs for buffers not listed with this window
		(unless (assq (tabbar-tab-value tab) buflist)
		  (tabbar-delete-tab tab))))
	  ;;if no corresponding window in tabbar-window-alist,
	  ;;delete all containted tabs and tabset
	  (dolist (tab (tabbar-tabs tabset))
	    (tabbar-delete-tab tab))
	  ;; if we are deleting the tabset, we don't have to worry about its template
	  (tabbar-delete-tabset tabset))))
    ;; duplicate tabbar-window-alist, so we can detect changes (have
    ;; to ensure that changes within tabbar-window-alist don't affect
    ;; tabbar-window cache)
    (setq tabbar-window-cache (copy-tree tabbar-window-alist)))
  ;; when triggered idle timers, Emacs does not recognize the change in the header line
  (force-window-update (window-buffer))
  ;; return current tabset
  (tabbar-get-tabset (number-to-string (window-number (selected-window)))))


(defvar tabbar-window-immediate-screen-fresh nil "See macro `fast-screen-refresh' in aquamacs-tabbar.")
(defun tabbar-window-update-tabsets-when-idle ()
  "Wait for emacs to be idle before updating tabsets.  This prevents tabs from
updating when a new window shows the current buffer, just before the window shows
new buffer."
 ; (if (eq this-command 'split-window-vertically)
  (if tabbar-window-immediate-screen-fresh ;; see macro `fast-screen-refresh' in aquamacs-tabbar
      (tabbar-window-update-tabsets)
    (run-with-idle-timer 0 nil
			 'tabbar-window-update-tabsets)))

(defadvice dnd-open-local-file (after dnd-update-tabs activate)
  (if tabbar-mode
      (tabbar-window-update-tabsets)))

(defun tabbar-update-if-changes-undone ()
  ;; have to wait until idle, or buffer's modified status isn't updated yet
  (run-with-idle-timer 0 nil (lambda ()
			       ;; update tabsets if the last undo made this unmodified
			       (unless (buffer-modified-p (current-buffer))
				 (tabbar-window-update-tabsets)))))

(defun tabbar-window-button-label (name)
  ;; Use empty string for HOME button, so it doesn't show up.
  "Return a label for button NAME.
That is a pair (ENABLED . DISABLED), where ENABLED and DISABLED are
respectively the appearance of the button when enabled and disabled.
They are propertized strings which could display images, as specified
by the variable `tabbar-button-label'."
  (if (eq name 'home)
      (cons "" "")
    (tabbar-button-label name)))

;; redefine tab labels, adding leading and trailing spaces for clarity
(defun tabbar-window-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label (format " %s " (tabbar-tab-value tab)))
	(width (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        (tabbar-expand label width tab)
      (tabbar-shorten
       label width))))

(defun tabbar-window-help-on-tab (tab)
  "Return the help string shown when mouse is onto TAB."
  (format "%s" (buffer-name (tabbar-tab-value tab))))

(defvar tab-points nil)
(defun tabbar-window-select-tab (event tab &optional prefix)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
	(one-buffer-one-frame nil)
        (buffer (tabbar-tab-value tab)))
    (if buffer
	(cond
	 ((eq mouse-button 'mouse-3)
	  (popup-menu tabbar-context-menu-map event prefix))
	 (t
	  (set-window-dedicated-p (selected-window) nil)
	  (let ((prevtab (tabbar-get-tab (window-buffer (selected-window)) 
					 (tabbar-tab-tabset tab)))
		(marker (cond ((bobp) (point-min-marker))
			      ((eobp) (point-max-marker))
			      (t (point-marker)))))
	    (set-marker-insertion-type marker t)
	    (assq-set prevtab marker
		      'tab-points))
	  (switch-to-buffer buffer)
	  (let ((new-pt (cdr (assq tab tab-points))))
	    (and new-pt 
		 (eq (marker-buffer new-pt) (window-buffer (selected-window)))
		 (let ((pos (marker-position new-pt)))
		   (unless (eq pos (point))
		     (if transient-mark-mode
			 (deactivate-mark))
		     (goto-char pos))
		   (set-marker new-pt nil) ;; delete marker
		   )))))
      ;; if there's no tab associated with clicked spot, use
      ;; special keymap for empty tab bar
      (cond ((eq mouse-button 'mouse-3)
	     ;; context menu on right-click
	     (popup-menu tabbar-empty-context-menu-map event prefix))
	    ((eq (event-click-count event) 2)
	     ;; new tab on double-click
	     (tabbar-new-tab))))))

(defun tabbar-windows-per-buffer (buffer)
  "Return a list of numbers corresponding to window tabsets to which the
specified BUFFER belongs."
  (let (buffer-window-list)
    (dolist (elt tabbar-window-alist)
      (let ((wnumber (car elt))
	    (wbuffers (cdr elt)))
	(when (assq buffer wbuffers)
	  (add-to-list 'buffer-window-list wnumber))))
    buffer-window-list))

(defun tabbar-window-other-instances (tab)
  "Return t if the buffer in this tab appears in any other tabsets or windows."
  (let* ((tabset (tabbar-tab-tabset tab))
	 (buffer (tabbar-tab-value tab))
	 (tab-tabsets (tabbar-windows-per-buffer buffer))
	 (tabset-window-number (string-to-number (symbol-name tabset)))
	 (buffer-windows (get-buffer-window-list buffer 'nomini t))
	 (tabset-window (window-number-get-window tabset-window-number)))
    (or (remq tabset-window-number tab-tabsets)
	(remq tabset-window buffer-windows))))

(defun tabbar-tabset-only-tab (tab)
  "Return t if this tab is the only member of its tabset, nil otherwise."
  (let ((buffer (tabbar-tab-value tab))
	(tabset (tabbar-tab-tabset tab)))
    (not (remq tab (tabbar-tabs tabset)))))

(defvar tabbar-retain-windows-when-tab-deleted '(not one-buffer-one-frame-mode)
  "Expression that evaluates to t when windows are to be retained 
... after their buffer is killed.")

(defun tabbar-window-delete-tab (tab)
  "Delete the named TAB.  
First check whether there are other tabs remaining in the tabset.
If so, we move to the next tab if available, otherwise previous,
before deleting."
  (let* ((tabset (tabbar-tab-tabset tab))
	 (wnumber (string-to-number (symbol-name (tabbar-tab-tabset tab))))
	 (wind (window-number-get-window wnumber))
	 (window-elt (assq wnumber tabbar-window-alist))
	 (buflist (cdr window-elt))
	 (buffer (tabbar-tab-value tab))
	 (tabbar-display-bug-workaround nil)
	 (sel    
	  (and (eq tab (tabbar-selected-tab tabset))
	       ;; we need to ensure that the selected tab
	       ;; corresponds to the currently shown buffer,
	       ;; because we possibly haven't updated 
	       ;; the tabset since the last change
	       ;; (e.g. find-alternate-file)
	       (eq (window-buffer wind) 
		   (tabbar-tab-value (tabbar-selected-tab 
				      tabset))))))
    ;; remove tab from tabbar-window-alist before deleting, so it won't be
    ;; regenerated
    (setq buflist (assq-delete-all buffer buflist))
    ;; delete window and its member in alist if no other tabs in tabset
    (if (tabbar-tabset-only-tab tab)
	(progn (unless (eval tabbar-retain-windows-when-tab-deleted)
		 (aquamacs-delete-window wind))
	       (setq tabbar-window-alist (delq window-elt tabbar-window-alist)))

      ;; otherwise, if this was selected tab, select the buffer that will be selected
      ;; by Emacs after getting killing the current buffer
      ;; if this one is not one of the tabs, we select an existing tab.
      ;; we MUST select one actively here.
      (when sel
	(with-current-buffer (current-buffer)
	  (if (assq (other-buffer buffer nil (window-frame wind)) buflist)
	      (progn
		(let ((one-buffer-one-frame))
		  (switch-to-buffer (other-buffer)))
		;; this avoids flicker
		(tabbar-display-update))
	    (if (tabbar-tab-next tabset tab)
		(tabbar-click-on-tab (tabbar-tab-next tabset tab))
	      (tabbar-click-on-tab (tabbar-tab-next tabset tab 'before))))))

      ;; put trimmed buffer list back into alist
      (setcdr window-elt buflist)
      ;; manually update tabsets now, to ensure that deleted tab is no
      ;;  longer displayed
      (tabbar-window-update-tabsets)
      (tabbar-scroll tabset -1))))

(defun tabbar-window-close-tab (tab)
  "Remove tab and kill buffer if shown exclusively."
  ;; quit current command if in minibuffer
  (when (minibuffer-window-active-p 
       (minibuffer-window (selected-frame)))
      (abort-recursive-edit))
  (let* ((buffer (tabbar-tab-value tab))
	 (killable (and
		    (killable-buffer-p buffer))) 
	 (dont-kill (tabbar-window-other-instances tab)))
    (when (and killable (not dont-kill))
      ;; ask before killing
      (with-current-buffer buffer
	(if (and
	     (or buffer-file-name buffer-offer-save)
	     (buffer-modified-p))
	    ;; a lot of buffers (e.g. dired) may be modified,
	    ;; but have no file name
	    (if (aquamacs-ask-for-confirmation
		  (format "Save buffer %s to file before closing tab? 
The buffer contains unsaved changes, which will be lost if you discard them now." (buffer-name)) 
		 nil (format "Save%s" (if buffer-file-name "" "...")) "Don't Save" t)
		(progn 
		    (if (listp last-nonmenu-event)
			(mac-key-save-file)
		      (save-buffer))
		    (if (buffer-modified-p)
			(keyboard-quit)
		      (message "File saved.")))
	      ;; mark as not modified, so it will be killed for sure
	      (set-buffer-modified-p nil))
	  (message ""))))
    (if (and killable (not dont-kill))
	;; 'kill-buffer-hook will call tabbar-window-delete-tab, so don't
	;;   do that here, unless not actually killing the buffer.
	(kill-buffer buffer)
      (tabbar-window-delete-tab tab))))

(defun tabbar-window-add-tab (tabset buffer &optional append)
  "Add to TABSET a tab with value BUFFER if there isn't one there yet.
BUFFER must be currently live. If the tab is added, it is added at the
beginning of the tab list, unless the optional argument APPEND is
non-nil, in which case it is added at the end.
Updates tabbar-window-alist in the same way."
  (let* ((wnumber (string-to-number (symbol-name tabset)))
	 (window (window-number-get-window wnumber))
	 (elt (assq wnumber tabbar-window-alist))
	 ;; find window's tabs in tabbar-window-alist
	 (buflist (cdr elt)))
    (when (and (buffer-live-p buffer) ;; only if buffer exists
	       (not (assq buffer buflist))) ;; and not already in tabbar-window-alist
      (tabbar-add-tab tabset buffer append)
      ;; to them, add current buffer in new tab.
      (let ((bufpair (list buffer (buffer-name buffer) (buffer-modified-p buffer))))
	(add-to-list 'buflist bufpair append)
	(setcdr elt buflist)
      ;; determine whether or not to show tabbar for this window:
      (if (eq (length buflist) 1)
	  ;; if there is only 1 buffer associated with this tabset, then
	  ;;  display no tabbar (no header line).
	  (add-to-list 'header-line-inhibit-window-list window t)
	;; otherwise, ensure this window has a tabbar
	(setq header-line-inhibit-window-list
	      (delq window header-line-inhibit-window-list)))))))

(defun menu-bar-select-buffer (&optional buffer)
  (interactive)
  ;; if no frame visible, code below doesn't work right (why?)
  ;; but switch-to-buffer (its one-buffer-one-frame.el advice) will
  ;; bring up a good frame. To Do: delete tabs
  (if (and display-buffer-reuse-frames (visible-frame-list))
      (let ((buffer (or buffer last-command-event)))
	(unless (bufferp buffer)
	  (error "menu-bar-select-buffer: not a buffer."))
	(if (visible-frame-list)
	    ;; find a suitable window
	    (progn
	      (let ((w (get-window-for-other-buffer 'dont-make-frame buffer)))
		(if (window-live-p w)
		    (select-window w)))
	      ;; (set-buffer (window-buffer (selected-window)))
	      ;; switch to buffer (may select a different window)
	      (if tabbar-mode
		  (switch-to-buffer-in-tab buffer)
		(switch-to-buffer buffer))
	      (select-frame-set-input-focus (window-frame (selected-window))))
	  ;; if no frame visible
	  ;; find right frame and activate that one
	  (let ((w (get-window-with-predicate
		    (lambda (w) (eq (window-buffer w) buffer)) nil t )))
	    (if w
		(progn
		  ; (raise-frame (window-frame w))
		  (make-frame-visible (window-frame w))
		  (select-frame-set-input-focus (window-frame w))
		  (select-window w)
		  (let ((tabbar-mode nil) (one-buffer-one-frame-mode nil))
		    (set-window-dedicated-p w nil)
		    (switch-to-buffer (or buffer last-command-event))))
	      ;; just create another frame for it
	      (switch-to-buffer-other-frame buffer)))))
    (let ((previously-vis (visible-frame-list)))
      (switch-to-buffer (or buffer last-command-event))
      (unless (memq (window-frame (selected-window)) previously-vis)
	;; frame was hidden before
	;; we don't want to show any leftover tabs after the switch
	;; so remove the buffer tab list for that window
	(let ((window-alist (assq (window-number (selected-window)) tabbar-window-alist)))
	  (setq tabbar-window-alist 
		(delq window-alist tabbar-window-alist)))))))

;; The following shouldn't be done, because the normal switch-to-buffer
;; is not sensitive to display-buffer-reuse-frames
;; and always switches the buffer in the selected window.
;; doing what's shown below will create incompatibilities.
;; (when window-system
;;   (defvar sw-in-tab-switching nil) 
;;   (defadvice switch-to-buffer (around sw-in-tab (&rest args) 
;; 				      activate compile protect) 
;;     (if (and display-buffer-reuse-frames tabbar-mode
;; 	     (not sw-in-tab-switching))
;; 	(let ((sw-in-tab-switching t))
;; 	  (setq ad-return-value (apply #'switch-to-buffer-in-tab args)))
;;       (setq ad-return-value ad-do-it))))

(defun switch-to-buffer-in-tab (buffer &optional norecord)
 "Switch to BUFFER, possibly switching frames.
This will display the buffer in an already-existing tab if
available.  Otherwise, give BUFFER a tab in the currently
selected window.  BUFFER may be a buffer or a string (buffer name).
Optional second arg norecord non-nil means
do not put this buffer at the front of the list of recently selected ones.
This function returns the buffer it switched to."
 ;; check existing tabsets for this buffer
 ;; priority is for tabsets where this is currently selected tab
 (let* ((buf (get-buffer buffer))
        (buffer-tab (or (assq buf (tabbar-map-tabsets 'tabbar-selected-tab))
                        (assq buf (tabbar-map-tabsets
                                   (lambda (tabset)
                                     (tabbar-get-tab buf tabset))))))
        (window (window-number-get-window
                 (string-to-number (symbol-name
                                    (tabbar-tab-tabset buffer-tab))))))
   (when buf
     (if window
         (progn
           (set-window-buffer window buf)
           (select-window window norecord)
	   (select-frame-set-input-focus (window-frame window)))
       (switch-to-buffer buf norecord)))
   buf))

(defun tabbar-window-merge-windows (&optional tabset source-tabsets)
  "Assign tabs from all tabsets to current tabset, or TABSET
if specified, then close all other tabs and windows.
Result is a single window containing all displayed buffers as tabs.
Turns on tabbar-mode if not already on."
  (interactive)
  (tabbar-mode 1)
  (let ((tabset-keep (or tabset (tabbar-current-tabset)))
	(all-tabsets 
	 (or source-tabsets 
	     (mapcar 'tabbar-get-tabset (tabbar-tabset-names)))))
    ;; cycle through tabsets, except for current one
    (dolist (this-tabset all-tabsets)
      ;; for each tabset, cycle through buffers
      (unless (eq this-tabset tabset-keep)
	(dolist (this-tab (tabbar-tabs this-tabset))
	  (let ((this-buffer (tabbar-tab-value this-tab)))
	    ;; add buffer to tabset-keep
	    (tabbar-window-add-tab tabset-keep this-buffer t))
	  ;; delete tab from prior tabset
	  (let ((tabbar-retain-windows-when-tab-deleted nil))
	    (tabbar-window-delete-tab this-tab)))))
    (unless (one-window-p 'nomini) (delete-other-windows))))

(defun tabbar-window-merge-windows-in-frame (&optional frame window)
  "Merges tabs from all window in a frame into a single one
shown in DEST-WINDOW."
  (interactive)
  (tabbar-window-merge-windows
   (tabbar-window-current-tabset window)
   (mapcar 'tabbar-window-current-tabset
	   (cdr-safe (window-list frame 'no-minibuf window))))) 
;; exclude current window

(defun tabbar-window-list-tabsets-to-save (&optional current-win)
  (if tabbar-current-tabset-function ;; has been initialized
      (let* ((tabset-names (tabbar-tabset-names))
	     (ntabsets (length tabset-names))
	     (current-tabset (tabbar-current-tabset t))
	     (current-tabset-name (symbol-name current-tabset))
	     (current-tabset-position
	      (1- (length (member current-tabset-name (reverse tabset-names)))))
	     (tabset-tabs (tabbar-map-tabsets 'tabbar-tabs))
	     (current-tabs (copy-alist (nth current-tabset-position tabset-tabs)))
	     ;; reorder list of tabs such that current tabset's tabs are 1st
	     (tabs-reordered (cons current-tabs
				   (unless current-win
				     (copy-tree (remove current-tabs tabset-tabs)))))
	     (selected-tab-buffer (car (tabbar-selected-tab current-tabset)))
	     tabset-save-list)
	;; extract nested list of buffers in tabs (i.e. remove tabset identifiers)
	(setq tabset-save-list
	      ;; loop through tabsets.  For each...
	      (mapcar
	       (function (lambda (tabset)
			   (remove nil
				   ;; ... loop through tabs.  Store buffer-name, or
				   ;;   set to nil if this buffer won't be restored by
				   ;;   desktop (i.e. not visiting a file, nor listed
				   ;;   in desktop-save-buffer)
				   (mapcar
				    (function (lambda (tab)
						(let ((buffer (tabbar-tab-value tab)))
						  (setcdr tab nil)
						  (with-current-buffer buffer
						    (if (or (buffer-file-name buffer)
							    desktop-save-buffer)
							(list 'tab (buffer-name buffer) (buffer-file-name buffer))
						      nil)))))
				    tabset))))
	       tabs-reordered))
	;; remove nils left behind for unsaved buffers
	(setq tabset-save-list (remove nil tabset-save-list)))
    ;; ELSE
    (list (list
	   (if (or (buffer-file-name)
		   desktop-save-buffer)
	       (list 'tab (buffer-name) (buffer-file-name))
	     nil)))))


(defun tabbar-find-buffer (name file)
  (or (and file 
	   (find-buffer-visiting file))
      (get-buffer name)))

(defun tabbar-window-restore-tabs-in-window (tablist)
  ;; We do not force tabs on unless we need them.
  (if (or tabbar-mode ; if tabs on, always check for restorability
	  (and tablist (> (length tablist) 1))) ; turn on tabs if tabs present
      (progn
	(unless tabbar-mode (tabbar-mode 1))
	(let ((temp-tab (car (tabbar-tabs (tabbar-current-tabset t))))
	      (prev-tab (tabbar-selected-tab (tabbar-current-tabset)))
	      (at-least-one-tab nil))
	  ;; delete tabbar here: (leads to unexplained slowdown/recursion, probably
	  ;; when tabbar is being displayed)
	  ;;   (let* ((tabset (tabbar-current-tabset t))
	  ;; 	 (wnumber (string-to-number (symbol-name tabset)))
	  ;; 	 (wind (window-number-get-window wnumber))
	  ;; 	 (window-elt (assq wnumber tabbar-window-alist)))
	  ;; 	 (setcdr window-elt nil)
	  ;; 	 (tabbar-window-update-tabsets))
	  ;; create new tabs corresponding to buffer-names in saved list
	  (dolist (spec tablist)
	    (let ((filename) (bufname))
	      (if (and (consp spec)
		       (eq 'tab (car spec)))
		  (setq bufname (second spec)
			filename (third spec))
		(setq bufname spec))
	      (let ((buffer (tabbar-find-buffer bufname filename))
		    (tabset (tabbar-current-tabset)))
		(and tabset buffer
		     (setq at-least-one-tab t)
		     (tabbar-window-add-tab tabset buffer t)))))
	  (tabbar-window-delete-tab temp-tab)  ;; this is insufficient.
	  ;; t if at least one was restored
	  at-least-one-tab))
    ;; keep window if tabs turned off
    t))

;; (defun tabbar-window-new-buffer (&optional mode)
;;   "Create a new buffer, with different behavior depending on the value of
;; tabbar-window-new-buffers: 'tab, create new buffer in current window
;; with a new tab; 'no-tab, create new buffer in current window, with
;; no tabbar (deletes all tabs in the window); default, create new buffer
;; in new frame."
;;   (cond
;;    ((eq tabbar-window-new-buffers 'tab)
;;     ;; create a new tab in current window
;;     (tabbar-new-tab mode))
;;    ((eq tabbar-window-new-buffers 'no-tab)
;;     ;; remove current window's alist from tabbar-window-alist
;;     (let ((wnumber (window-number (selected-window))))
;;       (setq tabbar-window-alist (assq-delete-all wnumber tabbar-window-alist)))
;;     ;; then create a new tab as usual -- lone tab will show no tabbar
;;     (tabbar-new-tab mode))
;;    (t
;;     ;; create a new tab in a new frame -- lone tab will show no tabbar
;;     (new-frame-with-new-scratch t))))

(defun tabbar-line ()
  "Return the header line templates that represent the tab bar.
Update the templates if tabbar-template is currently nil."
  (tabbar-current-tabset t)
  (if tabbar-current-tabset
      (or (tabbar-template tabbar-current-tabset)
	  (tabbar-line-format tabbar-current-tabset))))

(defun tabbar-window-current-tabset (&optional window)
  ;; ensure we don't count minibuffer as selected window - causes infinite loop
  (let* ((window (or window (minibuffer-selected-window) (selected-window)))
	 (tabset (tabbar-get-tabset (number-to-string (window-number window)))))
    ;; in the case where tabs have not yet been created, tabset will still be nil
    ;;  properly initialize all tabsets by running tabbar-window-update-tabsets
    (unless tabset 
      (setq tabset (tabbar-window-update-tabsets)))
    (if tabset ; update may say: display no tabs at all.
      (tabbar-select-tab-value (window-buffer window) tabset))
    tabset))

(defun tabbar-window-track-killed ()
  "Hook function run just before actually killing a buffer.
In Tabbar mode, switch to an adjacent tab if available.  Delete the
window if no other tabs exist.  Run once for each window where current
tab is displayed."
  (let* ((buffer (current-buffer))
 	 (window-numbers-list (tabbar-windows-per-buffer buffer))
	 (upd nil))
    ;; loop over all tabsets that contain a tab for this buffer
     (dolist (wnumber window-numbers-list)
      (let* ((tabset (tabbar-get-tabset (number-to-string wnumber)))
	     (tab (tabbar-get-tab buffer tabset)))
	;; ensure that tab still exists (some functions delete it
	;;     before killing buffer) ...
	(and tab
	     ;; ... and that the tab's window still exists ...
	     (window-number-get-window wnumber)
	     ;; ... and that we have created all necessary tabs here
	     ;; i.e. we don't over-zealously delete the window/frame
	     ;; when there are actually other buffer(s) to be shown
	     ;; as in find-alternate-file
	     (or upd (tabbar-window-update-tabsets) (setq upd t))
	     ;; ... and that there is currently a tabbar
	     ;; do not do this check: this function should
	     ;; also remove the window if there is an alternative header line
	     ;; (eq header-line-format tabbar-header-line-format)
	     (tabbar-window-delete-tab tab))))))


;;; Tab bar window setup
;;
(defun tabbar-window-init ()
  "Initialize tab bar data for tab grouping by window.
Run as `tabbar-init-hook'."
  (setq tabbar-window-cache nil
	;; keep previous tab data, if any
;; 	tabbar-window-alist nil
	tabbar-current-tabset-function 'tabbar-window-current-tabset
	tabbar-tab-label-function 'tabbar-window-tab-label
	tabbar-select-tab-function 'tabbar-window-select-tab
	tabbar-help-on-tab-function 'tabbar-window-help-on-tab
	tabbar-button-label-function 'tabbar-window-button-label
	tabbar-close-tab-function 'tabbar-window-close-tab
	tabbar-new-tab-function 'tabbar-window-new-buffer
	tabbar-buffer-list-function 'tabbar-window-buffer-list
	tabbar-home-function nil
	tabbar-home-help-function nil
	tabbar-home-button-value nil
	tabbar-cycle-scope 'tabs
	tabbar-inhibit-functions nil)
  (add-hook 'window-configuration-change-hook 'tabbar-window-update-tabsets-when-idle)
  (add-hook 'window-configuration-change-hook 'tabbar-reformat-all-tabsets)
  (add-hook 'first-change-hook 'tabbar-window-update-tabsets-when-idle)
  (add-hook 'after-undo-hook 'tabbar-update-if-changes-undone)
  (add-hook 'after-save-hook 'tabbar-window-update-tabsets)
  (add-hook 'kill-buffer-hook 'tabbar-window-track-killed)
  (tabbar-window-update-tabsets))

(defun tabbar-window-quit ()
  "Quit tab bar \"tabbar-window\" mode.
Run as `tabbar-quit-hook'."
  (setq tabbar-window-cache nil
	;; keep tab data, so we can regenerate current tabs
	;;  if tabbar-mode is turned back on
;; 	tabbar-window-alist nil
	tabbar-current-tabset-function nil
	tabbar-tab-label-function nil
	tabbar-select-tab-function nil
	tabbar-help-on-tab-function nil
	tabbar-button-label-function nil
	tabbar-close-tab-function nil
	tabbar-new-tab-function nil
	tabbar-buffer-list-function nil
	tabbar-home-function nil
	tabbar-home-help-function nil
	tabbar-home-button-value nil
	tabbar-cycle-scope nil
	tabbar-inhibit-functions nil
	)
  (remove-hook 'window-configuration-change-hook
	       'tabbar-window-update-tabsets-when-idle)
  (remove-hook 'window-configuration-change-hook 'tabbar-reformat-all-tabsets)
  (remove-hook 'first-change-hook 'tabbar-window-update-tabsets-when-idle)
  (remove-hook 'after-undo-hook 'tabbar-update-if-changes-undone)
  (remove-hook 'after-save-hook 'tabbar-window-update-tabsets)
  (remove-hook 'kill-buffer-hook 'tabbar-window-track-killed)
  )

;;-----------------------------------------------
(remove-hook 'tabbar-init-hook 'tabbar-buffer-init)
(remove-hook 'tabbar-quit-hook 'tabbar-buffer-quit)
(remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)

(add-hook 'tabbar-init-hook 'tabbar-window-init)
(add-hook 'tabbar-quit-hook 'tabbar-window-quit)

(provide 'tabbar-window)
(tabbar-window-list-tabsets-to-save (selected-window))
