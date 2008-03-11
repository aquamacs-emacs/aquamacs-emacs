(require 'tabbar)

(defvar tabbar-window-alist nil)
(defvar tabbar-window-cache nil)

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
shown in tabs.  Add a tabset for specified window if it does not
yet exist, and update list of tabs to include the currently
displayed buffer."
  (let* ((wnumber (window-number window))
	 (wbuffer (window-buffer window))
	 (wbname (buffer-name wbuffer))
	 (wblist (list wbuffer wbname))
	 (window-elt (assq wnumber tabbar-window-alist))
	 (tabbar-buffers-list (funcall tabbar-buffer-list-function)))
    ;; only include buffers that should have tabs (ignore tooltip windows, etc.)
    (when (memq wbuffer tabbar-buffers-list)
      (if window-elt
	  ;;if so, check whether window-buffer is listed for this window
	  (let ((window-buffer-list (cdr window-elt)))
	    (unless (member wblist window-buffer-list)
	      ;;add this buffer if not
	      (setq tabbar-window-alist
		    (cons (cons wnumber (append window-buffer-list (list wblist)))
			  (assq-delete-all wnumber tabbar-window-alist)))))
	;; if not, add (window-number . '((buffer-window))) to the alist
	(push (cons wnumber (list wblist)) tabbar-window-alist))))
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
  (let ((tabbar-buffers-list (funcall tabbar-buffer-list-function))
	(wnumber-list (window-number-list)))
    ;; loop through alist
    (dolist (elt tabbar-window-alist)
      (let* ((wnumber (car elt))
	     (blist (cdr elt))
	     (newlist blist)
	     newelt)
	;; remove entire elt from alist
	(setq tabbar-window-alist (delq elt tabbar-window-alist))
	;; if the window still exists, delete any buffers as needed
	(if (memq wnumber wnumber-list)
	    ;; for extant windows, loop through buffers
	    ;; delete any that aren't listed by tabbar-buffer-list-function 
	    ;; later, make this UNLESS they're displayed in that window
	    (progn
	      (dolist (thisbuffer blist)
		(unless (and
			 ;; in order to keep tab,
			 ;; must be an existing buffer that should get a tab
		       (memq (car thisbuffer) tabbar-buffers-list)
		       ;; must have buffer-name current for this buffer
		       (equal (buffer-name (car thisbuffer)) (cadr thisbuffer)))
		  (setq newlist (delq thisbuffer newlist))))
	      (when newlist
		(setq newelt (cons wnumber newlist))
		(if (eq (length newlist) 1)
		    (add-to-list 'header-line-inhibit-window-list
				 (window-number-get-window wnumber))
		  (setq header-line-inhibit-window-list
			(delq (window-number-get-window wnumber)
			      header-line-inhibit-window-list)) )
		)
	      ;; replace elt with newelt -- at END to preserve alist order
	      (add-to-list 'tabbar-window-alist newelt t))
	  (setq header-line-inhibit-window-list
		(delq (window-number-get-window wnumber)
		      header-line-inhibit-window-list))
	  )
	)))
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
		(unless (assq (car tab) buflist)
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
    (setq tabbar-window-cache (copy-alist tabbar-window-alist)))
  (number-to-string (window-number (selected-window)))
  )

(defun tabbar-window-update-tabsets-when-idle ()
  "Wait for emacs to be idle before updating tabsets.  This prevents tabs from
updating when a new window shows the current buffer, just before the window shows
new buffer."
  (run-with-idle-timer 0 nil
		       'tabbar-window-update-tabsets))

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
current buffer belongs."
  (let ((blist (list buffer (buffer-name buffer)))
	buffer-window-list)
    (dolist (elt tabbar-window-alist)
      (let ((wnumber (car elt))
	    (wbuffers (cdr elt)))
	(when (member blist wbuffers)
	  (setq buffer-window-list (cons wnumber buffer-window-list)))))
    buffer-window-list))

(defun tabbar-window-other-instances (tab)
  "Return t if the buffer in this tab appears in any other tabsets or windows."
  (let* ((tabset (tabbar-tab-tabset tab))
	 (buffer (car tab))
	 (tab-tabsets (tabbar-windows-per-buffer buffer))
	 (tabset-window-number (string-to-number (symbol-name tabset)))
	 (buffer-windows (get-buffer-window-list buffer 'nomini t))
	 (tabset-window (window-number-get-window tabset-window-number)))
    (or (remq tabset-window-number tab-tabsets)
	(remq tabset-window buffer-windows))))

(defun tabbar-tabset-only-tab (tab)
  "Return t if this tab is the only member of its tabset, nil otherwise."
  (let ((buffer (car tab))
	(tabset (tabbar-tab-tabset tab)))
    (not (remq tab (tabbar-tabs tabset)))))

(defun tabbar-window-delete-tab (tab)
  (if tab ;; only if tab exists!
      (let* ((tabset (tabbar-tab-tabset tab))
	     (wnumber (string-to-number (symbol-name tabset)))
	     (window-elt (assq wnumber tabbar-window-alist))
	     (buflist (cdr window-elt))
	     (buffer (car tab))
	     (bname (buffer-name buffer))
	     newelt)
	(setq tabbar-window-alist (delq window-elt tabbar-window-alist))
	(setq buflist (remove (list buffer bname) buflist))
	(when buflist
	  (setq newelt (cons wnumber buflist))
	  (add-to-list 'tabbar-window-alist newelt t)
	  ))))

(defun tabbar-window-remove-tab (tab)
  (let* ((tabset (tabbar-tab-tabset tab))
	 (sel    (eq tab (tabbar-selected-tab tabset)))
	 (wind (window-number-get-window
		(string-to-number (symbol-name (cdr tab))))))
    ;; function to close the named tab.  first check whether there are
    ;; tabs remaining after this one.  If so, we move to the next tab if
    ;; available, otherwise previous; delete the named tab.
    (if (tabbar-tabset-only-tab tab)
	(aquamacs-delete-window wind)
      (when sel
	 (if (tabbar-tab-next tabset tab)
	     (tabbar-forward-tab)
	   (tabbar-backward-tab)))
       (tabbar-window-delete-tab tab)
       ;; manually update tabsets now, to ensure that deleted tab is no
       ;;  longer displayed
       (tabbar-window-update-tabsets)
       (tabbar-scroll tabset -1)
       )))

(defun tabbar-window-close-tab (tab)
  (let* ((buffer (car tab))
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
	    (if (progn
		  (unless (minibuffer-window)
		    (setq last-nonmenu-event nil)
		    )
		  (y-or-n-p (format "Save buffer %s to file before closing tab? " 
				    (buffer-name)))
		  )
		(progn
		  (save-buffer)
		  (message "File saved.")
		  )
	      ;; mark as not modified, so it will be killed for sure
	      (set-buffer-modified-p nil)
	      )
	  (message ""))))
    (tabbar-window-remove-tab tab)
    (when (and killable (not dont-kill))
      (kill-buffer buffer))))

;; (defun tabbar-window-close-current-tab ()
;;   (interactive)
;;   (let ((tab (tabbar-selected-tab (tabbar-current-tabset t))))
;;     (tabbar-window-close-tab tab)))

(defun tabbar-line ()
  "Return the header line templates that represent the tab bar.
Inhibit display of the tab bar in current window if any of the
`tabbar-inhibit-functions' return non-nil."
  (tabbar-current-tabset t)
  (or (tabbar-template tabbar-current-tabset)
      (tabbar-line-format tabbar-current-tabset)))

(defun tabbar-window-current-tabset ()
       (let ((tabset (tabbar-get-tabset
		      (number-to-string (window-number (selected-window))))))
	 (tabbar-select-tab-value (current-buffer) tabset)
	 tabset))


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
        tabbar-home-function nil
        tabbar-home-help-function nil
	tabbar-home-button-value nil
	tabbar-cycle-scope 'tabs
	tabbar-inhibit-functions nil
	)
  (add-hook 'window-configuration-change-hook 'tabbar-window-update-tabsets-when-idle)
  (add-hook 'after-save-hook 'tabbar-window-update-tabsets)
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
  (remove-hook 'after-save-hook 'tabbar-window-update-tabsets)
)

;;-----------------------------------------------
(remove-hook 'tabbar-init-hook 'tabbar-buffer-init)
(remove-hook 'tabbar-quit-hook 'tabbar-buffer-quit)
(remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)

(add-hook 'tabbar-init-hook 'tabbar-window-init)
(add-hook 'tabbar-quit-hook 'tabbar-window-quit)

(provide 'tabbar-window)