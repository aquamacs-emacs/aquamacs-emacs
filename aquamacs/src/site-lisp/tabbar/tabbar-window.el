;; Tabbar-window.el --- "Window Tabs" for tabbar-mode: Tab-set is
;;   specific to each window, and tabbar is hidden when only a
;;   single tab exists for that window.  Requires that tabbar.el and
;;   aquamacs-tabbar.el be loaded first.

;; Author: Nathaniel Cunningham <nathaniel.cunningham@gmail.com>
;; Maintainer: Nathaniel Cunningham <nathaniel.cunningham@gmail.com>
;; Created: February 2008
;; Revision: $Id: tabbar-window.el,v 1.19 2008/04/21 22:43:09 champo Exp $

(require 'tabbar)

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
(make-variable-frame-local 'tabbar-current-tabset)

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
	(funcall tabbar-current-tabset-function)))

(defun window-number (window)
  "Return window ID as a number."
  (let ((window-string (format "%s" window)))
    (string-to-number
     (nth 1 (split-string window-string "\\(<window \\| on \\)" )))))

(defun window-number-list ()
  "Return IDs of all windows as list of numbers."
  (let (window-numbers)
    (walk-windows
     #'(lambda (window)
	 (push (window-number window) window-numbers)) 'nomini t)
    window-numbers))

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
	 (tabbar-buffers-list (funcall tabbar-buffer-list-function)))
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

(defun window-number-get-window (wnumber)
  "Return window corresponding to ID number."
  (let (window-id)
    (walk-windows
     #'(lambda (window)
	 (when (eq wnumber (window-number window))
	   (setq window-id window))) 'nomini t)
    window-id))

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
	;; if the window still exists, delete any buffers as needed
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

	      (if (eq (length buflist) 1)
		  ;; if there is only 1 buffer associated with this tabset, then
		  ;;  display no tabbar (no header line).
		  (add-to-list 'header-line-inhibit-window-list window t)
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
  (walk-windows 'tabbar-window-alist-update 'nomini t)
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
  (tabbar-get-tabset (number-to-string (window-number (selected-window))))
  )

(defun tabbar-window-update-tabsets-when-idle ()
  "Wait for emacs to be idle before updating tabsets.  This prevents tabs from
updating when a new window shows the current buffer, just before the window shows
new buffer."
  (run-with-idle-timer 0 nil
		       'tabbar-window-update-tabsets))

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

(defun tabbar-window-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label (format " %s " (tabbar-tab-value tab))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(defun tabbar-window-help-on-tab (tab)
  "Return the help string shown when mouse is onto TAB."
  (format "%s" (buffer-name (tabbar-tab-value tab))))

(defun tabbar-window-select-tab (event tab &optional prefix)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
	(one-buffer-one-frame nil)
        (buffer (tabbar-tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-3)
      (popup-menu tabbar-context-menu-map event prefix))
     (t
      (switch-to-buffer buffer)))
    ))

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

(defun tabbar-window-delete-tab (tab)
  "Delete the named TAB.  first check whether there are other
 tabs remaining in the tabset.  If so, we move to the next tab if
 available, otherwise previous, before deleting."
  (let* ((tabset (tabbar-tab-tabset tab))
	 (sel    (eq tab (tabbar-selected-tab tabset)))
	 (wnumber (string-to-number (symbol-name (tabbar-tab-tabset tab))))
	 (wind (window-number-get-window wnumber))
	 (window-elt (assq wnumber tabbar-window-alist))
	 (buflist (cdr window-elt))
	 (buffer (tabbar-tab-value tab)))
    ;; remove tab from tabbar-window-alist before deleting, so it won't be
    ;;   regenerated
    (setq buflist (assq-delete-all buffer buflist))
    ;; delete window and its member in alist if no other tabs in tabset
    (if (tabbar-tabset-only-tab tab)
	(progn (aquamacs-delete-window wind)
	       (setq tabbar-window-alist (delq window-elt tabbar-window-alist)))
      ;; otherwise, if this is selected tab, select a neighbor
      (when sel
	(if (tabbar-tab-next tabset tab)
	    (tabbar-forward-tab)
	  (tabbar-backward-tab)))
      ;; put trimmed buffer list back into alist
      (setcdr window-elt buflist)
      ;; manually update tabsets now, to ensure that deleted tab is no
      ;;  longer displayed
      (tabbar-window-update-tabsets)
      (tabbar-scroll tabset -1)
      )))

(defun tabbar-window-close-tab (tab)
  (let* ((buffer (tabbar-tab-value tab))
	 (killable (and
		    (killable-buffer-p buffer)
		    (eq   (string-match "\\*.*\\*" (buffer-name buffer)) nil)
		    (eq   (string-match " SPEEDBAR" (buffer-name buffer)) nil))) 
	 (dont-kill (tabbar-window-other-instances tab)))
    (when (and killable (not dont-kill))
      ;; ask before killing
      (with-current-buffer buffer
	(if (and
	     (or buffer-file-name buffer-offer-save)
	     (buffer-modified-p))
	    ;; a lot of buffers (e.g. dired) may be modified,
	    ;; but have no file name
	    (if (y-or-n-p (format "Save buffer %s to file before killing it? " 
				    (buffer-name)))
		(progn
		  (save-buffer)
		  (message "File saved.")
		  )
	      ;; mark as not modified, so it will be killed for sure
	      (set-buffer-modified-p nil)
	      )
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


(defun tabbar-desktop-tabsets-to-save ()
  (let* ((tabset-names (tabbar-tabset-names))
	 (ntabsets (length tabset-names))
	 (current-tabset-name (symbol-name (tabbar-current-tabset)))
	 (current-tabset-position
	  (1- (length (member current-tabset-name (reverse tabset-names)))))
	 (tabset-tabs (tabbar-map-tabsets 'tabbar-tabs))
	 (current-tabs (copy-alist (nth current-tabset-position tabset-tabs)))
	 ;; reorder list of tabs such that current tabset's tabs are 1st
	 (tabs-reordered (cons current-tabs
			       (copy-tree (remove current-tabs tabset-tabs))))
	 (selected-tab-buffer (car (tabbar-selected-tab))))
    ;; extract nested list of buffers in tabs (i.e. remove tabset identifiers)
    (setq tabbar-desktop-saved-tabsets
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
						    (buffer-name buffer)
						  nil)))))
				tabset))))
	   tabs-reordered)))
  ;; remove nils left behind for unsaved buffers
  (setq tabbar-desktop-saved-tabsets (remove nil tabbar-desktop-saved-tabsets))
  ;; store list of tab names to restore in desktop's list of global variables
  (add-to-list 'desktop-globals-to-save 'tabbar-desktop-saved-tabsets))

(defun tabbar-desktop-rebuild-saved-tabsets ()
  (or tabbar-mode (tabbar-mode 1))
;;   (tabbar-window-update-tabsets)  ;; taken care of by tabbar-current-tabset
  (when (and (boundp 'tabbar-desktop-saved-tabsets) tabbar-desktop-saved-tabsets)
  (let* ((tabsets tabbar-desktop-saved-tabsets)
	 (thiswin-tabs (car tabsets))
	 (otherwin-tabs (cdr tabsets))
	 (bname (buffer-name (current-buffer)))
	 (after-tabs (cdr (member bname thiswin-tabs)))
	 (before-tabs (cdr (member bname (reverse thiswin-tabs)))))
    ;; for the current window, where current buffer already has a tab,
    ;;  generate tabs before this one for before-tabs, and after for after-tabs
    (dolist (bufname before-tabs)
      (let ((buffer (get-buffer bufname))
	    (tabset (tabbar-current-tabset t)))
	;; add at beginning of tabset (in rev. order)
	(tabbar-window-add-tab tabset buffer)))
    (dolist (bufname after-tabs)
      (let ((buffer (get-buffer bufname))
	    (tabset (tabbar-current-tabset)))
	(tabbar-window-add-tab tabset buffer t)));;append to end of tabset

    ;; now go through any remaining tabsets; for each, create a new frame;
    ;;   add tabs in order to end of tabset; remove initial tab
    (save-selected-window
      (dolist (tablist otherwin-tabs)
	;; create new frame with blank buffer
	(new-frame-with-new-scratch t)
;; 	(tabbar-window-update-tabsets)  ;; taken care of by tabbar-current-tabset
	(let ((temp-tab (car (tabbar-tabs (tabbar-current-tabset t)))))
	  ;; create new tabs corresponding to buffer-names in saved list
	  (dolist (bufname tablist)
	    (let ((buffer (get-buffer bufname))
		  (tabset (tabbar-current-tabset)))
	      (tabbar-window-add-tab tabset buffer t)))
	  ;; delete tab from blank buffer
	  (tabbar-close-tab temp-tab)))))))

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
  (or (tabbar-template tabbar-current-tabset)
      (tabbar-line-format tabbar-current-tabset)))

(defun tabbar-window-current-tabset ()
  (let ((tabset (tabbar-get-tabset
		 (number-to-string (window-number (selected-window))))))
    ;; in the case where tabs have not yet been created, tabset will still be nil
    ;;  properly initialize all tabsets by running tabbar-window-update-tabsets
    (unless tabset 
      (setq tabset (tabbar-window-update-tabsets)))
    (tabbar-select-tab-value (current-buffer) tabset)
    tabset))

(defun tabbar-window-track-killed ()
  "Hook run just before actually killing a buffer.
In Tabbar mode, switch to an adjacent tab if available.  Delete the
window if no other tabs exist.  Run once for each window where current
tab is displayed."
  (let* ((buffer (current-buffer))
 	 (window-numbers-list (tabbar-windows-per-buffer buffer)))
    ;; loop over all tabsets that contain a tab for this buffer
     (dolist (wnumber window-numbers-list)
      (let* ((tabset (tabbar-get-tabset (number-to-string wnumber)))
	     (tab (tabbar-get-tab buffer tabset)))
	;; ensure that tab still exists (some functions delete it
	;;     before killing buffer) ...
	(and tab
	     ;; ... and that the tab's window still exists ...
	     (window-number-get-window wnumber)
	     ;; ... and that there is currently a tabbar
	     (eq header-line-format tabbar-header-line-format)
	     (tabbar-window-delete-tab tab))))))


;;; Tab bar window setup
;;
(defun tabbar-window-init ()
  "Initialize tab bar data for tab grouping by window.
Run as `tabbar-init-hook'."
  (setq tabbar-window-alist nil
	tabbar-window-cache nil
	tabbar-current-tabset-function 'tabbar-window-current-tabset
	tabbar-tab-label-function 'tabbar-window-tab-label
	tabbar-select-tab-function 'tabbar-window-select-tab
	tabbar-help-on-tab-function 'tabbar-window-help-on-tab
	tabbar-button-label-function 'tabbar-window-button-label
	tabbar-close-tab-function 'tabbar-window-close-tab
	tabbar-new-tab-function 'tabbar-window-new-buffer
	tabbar-home-function nil
	tabbar-home-help-function nil
	tabbar-home-button-value nil
	tabbar-cycle-scope 'tabs
	tabbar-inhibit-functions nil
	)
  (add-hook 'window-configuration-change-hook 'tabbar-window-update-tabsets-when-idle)
  (add-hook 'first-change-hook 'tabbar-window-update-tabsets-when-idle)
  (add-hook 'after-undo-hook 'tabbar-update-if-changes-undone)
  (add-hook 'after-save-hook 'tabbar-window-update-tabsets)
  (add-hook 'kill-buffer-hook 'tabbar-window-track-killed)
  (add-hook 'desktop-save-hook 'tabbar-desktop-tabsets-to-save)
  (add-hook 'desktop-after-read-hook 'tabbar-desktop-rebuild-saved-tabsets)
  (tabbar-window-update-tabsets)
  )

(defun tabbar-window-quit ()
  "Quit tab bar \"tabbar-window\" mode.
Run as `tabbar-quit-hook'."
  (setq tabbar-window-alist nil
	tabbar-window-cache nil
	tabbar-current-tabset-function nil
	tabbar-tab-label-function nil
	tabbar-select-tab-function nil
	tabbar-help-on-tab-function nil
	tabbar-button-label-function nil
	tabbar-close-tab-function nil
	tabbar-home-function nil
	tabbar-home-help-function nil
	tabbar-home-button-value nil
	tabbar-cycle-scope nil
	tabbar-inhibit-functions nil
	)
  (remove-hook 'window-configuration-change-hook
	       'tabbar-window-update-tabsets-when-idle)
  (remove-hook 'first-change-hook 'tabbar-window-update-tabsets-when-idle)
  (remove-hook 'after-undo-hook 'tabbar-update-if-changes-undone)
  (remove-hook 'after-save-hook 'tabbar-window-update-tabsets)
  (remove-hook 'kill-buffer-hook 'tabbar-window-track-killed)
  (remove-hook 'desktop-save-hook 'tabbar-desktop-tabsets-to-save)
  (remove-hook 'desktop-after-read-hook 'tabbar-desktop-rebuild-saved-tabsets)
  )

;;-----------------------------------------------
(remove-hook 'tabbar-init-hook 'tabbar-buffer-init)
(remove-hook 'tabbar-quit-hook 'tabbar-buffer-quit)
(remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)

(add-hook 'tabbar-init-hook 'tabbar-window-init)
(add-hook 'tabbar-quit-hook 'tabbar-window-quit)

(provide 'tabbar-window)