;; load original tabbar-mode
(require 'tabbar)

;; modify various settings:
;; eliminate gap between header-line and toolbar
(setq tool-bar-border 0)

;; change faces for better-looking tabs (and more obvious selected tab!)
(set-face-attribute 'tabbar-default nil
		    :inherit nil
		    :height 110
		    :width 'normal
		    :background "gray80"
		    :foreground "gray30"
		    :family "helvetica")

;(set-face-attribute 'tabbar-default nil
;		    :inherit 'variable-pitch
;		    :height 1.0
;		    :width 'normal
;		    :background "gray80"
;		    :foreground "gray30")

(set-face-attribute 'tabbar-selected nil
		    :background "gray95"
		    :foreground "gray20"
		    :box '(:line-width 1 :color "gray95" :style nil))
;		    :box '(:line-width 2 :color "white" :style released-button))

(set-face-attribute 'tabbar-unselected nil
		    :box '(:line-width 1 :color "gray50" :style nil))
;		    :box '(:line-width 2 :color "white" :style pressed-button))

(set-face-attribute 'tabbar-highlight nil
		    :underline nil
		    :background "gray87")

(set-face-attribute 'tabbar-button nil
		    :box nil)

;; redefine tab labels, adding leading and trailing spaces for clarity
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format " [%s] " (tabbar-tab-tabset tab))
                  (format " %s " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

;; function for closing a tab via context menu
(defun tabbar-close-clicked-tab (event)
  (interactive "@e")
  (when (tabbar-click-p event)
    (let* ((clicklocation (posn-string (event-start event)))
	   (clickedtab (get-text-property (cdr clicklocation)
						  'tabbar-tab (car clicklocation))))
      (save-current-buffer
	(tabbar-window-close-tab clickedtab)))))

;; function for closing all other tabs via context menu
(defun tabbar-close-other-tabs (event)
  "Close all tabs except the one where context menu was generated via click"
  (interactive "@e")
  (when (tabbar-click-p event)
    (let* ((clicklocation (posn-string (event-start event)))
	   (clickedtab (get-text-property (cdr clicklocation)
					  'tabbar-tab (car clicklocation)))
	   (tablist (tabbar-tabs (tabbar-tab-tabset clickedtab))))
;      (save-current-buffer
	(dolist (thistab tablist (car clickedtab))
	  (unless (equal thistab clickedtab)
	    (tabbar-window-close-tab thistab))))));)

;; function to open a new tab, suppressing new frame creation
(defun tabbar-new-tab-with-new-scratch  ()
  "Opens a new frame containing an empty buffer."
  (interactive)			
  (let ((one-buffer-one-frame-inhibit t)
	(buf (generate-new-buffer (mac-new-buffer-name "untitled"))))
    (save-excursion
      (set-buffer buf)
      (if default-major-mode
	  (funcall default-major-mode)))
    (switch-to-buffer buf)
    (setq buffer-offer-save t)
    (set-buffer-modified-p nil)))

;; keymap for tabbar context menu
(defvar tabbar-context-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [closeothers] (cons "Close Other Tabs" 'tabbar-close-other-tabs))
    (define-key map [closetab] (cons "Close Tab" 'tabbar-close-clicked-tab))
    (define-key map [newwindow] (cons "Open This Tab in New Window" 'make-frame-command))
    (define-key map [newtab] (cons "New Tab" 'tabbar-new-tab-with-new-scratch))
    map) "Keymap for the Tabbar context menu.")

;; modify hints to give only the buffer name
(defun tabbar-buffer-help-on-tab (tab)
  "Return the help string shown when mouse is onto TAB."
  (if tabbar--buffer-show-groups
      (let* ((tabset (tabbar-tab-tabset tab))
             (tab (tabbar-selected-tab tabset)))
        (format "click: switch to buffer %S in group [%s]"
                (buffer-name (tabbar-tab-value tab)) tabset))
    (format "%s"
            (buffer-name (tabbar-tab-value tab)))
    ))

;; provide new actions for middle-click/right-click on tabs
(defun tabbar-buffer-select-tab (event tab &optional prefix)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
		      (one-buffer-one-frame-inhibit t)
        (buffer (tabbar-tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-3)
      (popup-menu tabbar-context-menu-map event prefix))
     (t
      (switch-to-buffer buffer)))
    ;; Don't show groups.
    (tabbar-buffer-show-groups nil)
    ))

;; use images for tabbar buttons
(defun tabbar-button-label (name)
 ;; redefine tabbar-button-label to eliminate 1-pixel border around images
  "Return a label for button NAME.
That is a pair (ENABLED . DISABLED), where ENABLED and DISABLED are
respectively the appearance of the button when enabled and disabled.
They are propertized strings which could display images, as specified
by the variable `tabbar-NAME-button'."
  (let* ((btn (symbol-value
               (intern-soft (format "tabbar-%s-button" name))))
         (on  (tabbar-find-image (cdar btn)))
         (off (and on (tabbar-find-image (cddr btn)))))
    (when on
      (tabbar-normalize-image on)
      (if off
          (tabbar-normalize-image off)
        ;; If there is no disabled button image, derive one from the
        ;; button enabled image.
        (setq off (tabbar-disable-image on))))
    (cons
     (propertize (or (caar btn) " ") 'display on)
     (propertize (or (cadr btn) " ") 'display off))))

(setq tabbar-home-button-enabled-image
  '((:type png :file "home_sm.png")))

(setq tabbar-home-button-disabled-image nil)
;  '((:type png :file "home_sm.png")))

(setq tabbar-home-button
  (cons (cons "[o]" tabbar-home-button-enabled-image)
        (cons "[x]" tabbar-home-button-disabled-image)))

(setq tabbar-buffer-home-button
  (cons (cons "[+]" tabbar-home-button-enabled-image)
        (cons "[-]" tabbar-home-button-disabled-image)))

(setq tabbar-scroll-left-button-enabled-image
  '((:type png :file "back_sm.png")))

(setq tabbar-scroll-left-button
  (cons (cons " <" tabbar-scroll-left-button-enabled-image)
        (cons " =" nil)))

(setq tabbar-scroll-right-button-enabled-image
  '((:type png :file "forward_sm.png")))

(setq tabbar-scroll-right-button
  (cons (cons " >" tabbar-scroll-right-button-enabled-image)
        (cons " =" nil)))

;; allow fast-clicking through lists of tabs
(defsubst tabbar-click-p (event)
  "Return non-nil if EVENT is a mouse click event."
  ;;counts as a click even if it's the last of a double- or triple-click;
  ;;allows fast cycling through tabs with the mouse.
  (and (or
	(memq 'click (event-modifiers event))
	(memq 'double (event-modifiers event))
	(memq 'triple (event-modifiers event)))
       ;; don't count double- or triple-drag events
       (not (memq 'drag (event-modifiers event))))
  )

(defun tabbar-check-overflow (tabset)
  (let ((tabs (tabbar-view tabset))
	elts)
    (while tabs
      (setq elts  (cons (tabbar-line-tab (car tabs)) elts)
	    tabs  (cdr tabs)))
    (setq elts (nreverse elts))
    (with-temp-buffer
      (let ((truncate-partial-width-windows nil)
	    (inhibit-modification-hooks t)
	    deactivate-mark ;; Prevent deactivation of the mark!
	    start)
	(setq truncate-lines nil
	      buffer-undo-list t)
	(apply 'insert (tabbar-dummy-line-buttons))
	(setq start (point))
	(delete-region start (point-max))
	(goto-char (point-max))
	(apply 'insert elts)
	(goto-char (point-min))
	(> (vertical-motion 1) 0)))))

(defun tabbar-dummy-line-buttons ()
  (list
   (cdr tabbar-home-button-value)
   (cdr tabbar-scroll-left-button-value)
   (cdr tabbar-scroll-right-button-value)
   tabbar-separator-value))

(defsubst tabbar-line-buttons (tabset)
  "Return a list of propertized strings for tab bar buttons.
TABSET is the tab set used to choose the appropriate buttons."
  (list
   (if tabbar-home-function
       (car tabbar-home-button-value)
     (cdr tabbar-home-button-value))
   (if (> (tabbar-start tabset) 0)
       (car tabbar-scroll-left-button-value)
     (cdr tabbar-scroll-left-button-value))
   (if (tabbar-check-overflow tabset)
       (car tabbar-scroll-right-button-value)
     (cdr tabbar-scroll-right-button-value))
   tabbar-separator-value))
(defsubst tabbar-line-buttons (tabset)
  "Return a list of propertized strings for tab bar buttons.
TABSET is the tab set used to choose the appropriate buttons."
  (list
   (if tabbar-home-function
       (car tabbar-home-button-value)
     (cdr tabbar-home-button-value))
   (if (> (tabbar-start tabset) 0)
       (car tabbar-scroll-left-button-value))
   (if (< (tabbar-start tabset)
          (1- (length (tabbar-tabs tabset))))
       (car tabbar-scroll-right-button-value))
   tabbar-separator-value))

; turn on tabbar mode
; (tabbar-mode t)

;; changes behavior of "buffer tabs", so that tabs are associated with a
;;   window instead of a major mode.
;; This must be done after turning on tabbar-mode, as it overwrites
;;   variable values that are set when tabbar-mode is initialized.
(load "tabbar-window.el")

(provide 'aquamacs-tabbar)