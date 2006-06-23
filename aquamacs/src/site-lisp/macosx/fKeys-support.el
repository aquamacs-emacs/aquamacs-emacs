;; fKeys support for Aquamacs
;; (C) 2006 by David Reitter
;; do not copy / redistribute. All rights reserved.




; to do
; make sure mac-function-modifier is saved by save options
; change paths for fKeysd
; check dialog 
; fKeys bugs -> toggle caps lock, and system-wide mod key remapping



(defvar fKeys-program "/opt/kodachi/fKeys/system/fKeysd")
 
(defun fKeys-version ()
  "Return fKeys version, and nil if fKeys isn't running."
  (let ((x
	 (with-temp-buffer
	(call-process "kextstat" nil t nil 
		      "-k" "-l" "-b" "com.kodachi.driver.fKeys")
	(beginning-of-buffer)
	(if (search-forward-regexp "(\\([0-9]\\.[0-9]\\)" nil t)
	    (match-string 1)
	  nil))))
    (condition-case nil
	(if x
	    (string-to-number x)
	  nil)
      (error nil))))

;; needs to be run as root
;; (call-process "kextload" nil t nil "/opt/kodachi/fKeys/system/fKeys.kext") 

;; (fKeys-version)

(defun fKeys-initialize ()
  "Loads the aquamacs.plist supplied with fKeys.
Return t if successful."
  (condition-case nil
      (eq 0 (call-process "/opt/kodachi/fKeys/system/fKeysd" nil nil nil
			  "/opt/kodachi/fKeys/configs/aquamacs.plist"))
    (error nil)))

(defvar fKeys-initialized nil)
(defun fKeys-handle-focus-gained (event)
  (interactive "e")
  (when mac-capslock-modifier
    (unless fKeys-initialized
      (if (fKeys-initialize)
	  (setq fKeys-initialized t)))
    (condition-case nil
	(call-process "/opt/kodachi/fKeys/system/fKeysd" nil nil nil
		      "-s" "1" "1") 
      (error nil))))

(defun fKeys-handle-focus-lost (event)
  (interactive "e") 
  (when mac-capslock-modifier
    (unless fKeys-initialized
      (if (fKeys-initialize)
	  (setq fKeys-initialized t)))
    (condition-case nil
     (call-process "/opt/kodachi/fKeys/system/fKeysd" nil nil nil
			  "-s" "1" "0")
    (error nil))))
    
  
;; define the event class
(put 'application-event  'mac-apple-event-class "appl") ; kEventClassApplication
(put 'app-front-activated 'mac-apple-event-id "APAC") ;  
(put 'app-front-deactivated 'mac-apple-event-id "APDA") ;  
 
  
(define-key mac-apple-event-map [application-event app-front-activated ]
  'fKeys-handle-focus-gained)
(define-key mac-apple-event-map [application-event app-front-deactivated ]
  'fKeys-handle-focus-lost)


(require 'emulate-mac-keyboard-mode)
 

(defvar mac-capslock-modifier-enabled-value 'meta)
(defun  toggle-mac-capslock-modifier (&optional interactively) 
  (interactive "p")
  (unless mac-capslock-modifier-enabled-value
    (setq mac-capslock-modifier-enabled-value 'meta))
   (setq mac-capslock-modifier
	 (if mac-capslock-modifier
	     (progn
	       (setq mac-capslock-modifier-enabled-value mac-capslock-modifier)
	       nil)
	   mac-capslock-modifier-enabled-value))
   (when interactively 
     (customize-mark-as-set 'mac-capslock-modifier)
     
     (let ((v (fKeys-version)))
       (if (and v (>= 0.2 v))
	   (unless fKeys-initialized
	     (if (fKeys-initialize)
		 (setq fKeys-initialized t)))
	 (if (mac-dialog-y-or-n-p 
	      "fKeys installation is needed."
"To use Caps Lock as Meta key, you need to install Kodachi fKeys (0.2.0 or newer). Would you like to download this software now?")
	     (browse-url "http://aquamacs.org/fKeys")))))
   
   (message 
    (format "Caps Lock key is %s%s" 
	    (if mac-capslock-modifier 
		""  "not ")
	    (upcase-initials 
	     (symbol-name (or mac-capslock-modifier 
			      mac-capslock-modifier-enabled-value))))))

(define-key menu-bar-option-key-menu [capslock-modifier]
  `(menu-item
    ,(aq-shortcut  "%s Caps Lock Key for %s (not extra characters)  "
		   'toggle-mac-capslock-modifier 
		   (string (decode-char 'ucs #X21EA))
		   (upcase-initials (symbol-name 
				     (or mac-capslock-modifier 
					 mac-capslock-modifier-enabled-value))))
    toggle-mac-capslock-modifier 
    :key-sequence nil
    :visible (boundp 'mac-capslock-modifier)
    :help "Toggle whether to let Caps Lock key behave as Emacs key, 
do not let it produce shifted characters (passing the key to the system)."
    :button (:toggle . mac-capslock-modifier)))


(define-key-after menu-bar-options-menu [option-key-menu]
  `(menu-item (format "%s%s Modifier Keys" (string (decode-char 'ucs #X2325))
(string (decode-char 'ucs #X21EA)))
 ,menu-bar-option-key-menu)
  'edit-options-separator)

; (aq-describe-modifier mac-option-modifier)