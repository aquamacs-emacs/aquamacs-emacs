;; emulate-mac-*-keyboard-modes for Aquamacs
;; (C) 2005,2007 by David Reitter
;; do not copy / redistribute outside of Aquamacs. All rights reserved.

;; This defines multiple global minor modes, each of which
;; emulates common keys of a keyboard layout.
;;
;; The bindings are defined in `emulate-mac-keyboard-mode-maps'.
;;
;; By default, the following minor modes are defined:
;;
;; emulate-mac-german-keyboard-mode
;; emulate-mac-italian-keyboard-mode
;; emulate-mac-french-keyboard-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar emulate-mac-keyboard-mode-maps nil
"List of special key translation bindings for `emulate-mac-keyboard-mode'.
Each element of this list should be a cons (LANGUAGE . BINDINGS), where
LANGUAGE is a symbol named after the language associated with the keyboard
layout to be used, and BINDINGS is a list of bindings, each consisting of
a cons cell (KEY . RESULT), where KEY is a string or other keycode vector 
denoting the key, and RESULT is a string or key code giving the text to be 
inserted for the key. Example:
 ((german . ((\"\\M-l\" . \"@\")
 	     (\"\\M-/\" . \"\\\\\"))))")

(defvar emmkm--euro 342604)

(setq emulate-mac-keyboard-mode-maps
 `((german . (("\M-l" . "@")
	       ("\M-/" . "\\")
	       ("\M-5" . "[")
	       ("\M-6" . "]")
	       ("\M-7" . "|")
	       ("\M-8" . "{")
	       ("\M-9" . "}")
	       ("\M-n" . "~")
	       ("\M-e" . ,emmkm--euro) ;; euro symbol
	       ))
    (french . (([?\M-`] . "@")
	       ("\M-$" . ,emmkm--euro) 
	       ("\M-/" . "\\")
	       ([?\M-£]  . "#") ;; was: "\M-£"
	       ("\M-n" . "~")
	       ("\M-L" . "|")
	       ("\M-(" . "{")
	       ("\M-5" . "[")
	       ("\M-)" . "}")
	       ([?\M-°] . "]")))
    (italian . ( ("\M-§" . "@")  
		 
		([?\M-¤]  . "@") ; wont work either
		("\M-(" . "{")
		("\M-4" . "[")
		("\M-)" . "}")
		("\m-7" . "]")  
		("\M-\:" . "|")))
    (italian-pro . 
		(("\M-5" . "~") 
		([?\M-è] . "[") ;;  was ,(kbd "M-\217")
		([?\M-é] . "{") ;;      ,(kbd "M-\216")
		("\M-*" . "}")
		("\M-+" . "]")
		([?\M-à] . "#") ;;  was  ,(kbd "M-\210")
		 ))
    (us . (     ("\M-3" . "£")
		("\M-@" . ,emmkm--euro) ;; euro symbol
		("\M-6" . "§")))
    (british . (("\M-3" . "#")
		("\M-2" . ,emmkm--euro) ;; euro symbol
		("\M-6" . "§")))))

;; (define-emulate-mac-keyboard-modes)
;; (make-emulate-mac-keyboard-mode-map 'german)   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'aquamacs-tools) ;; aq-list-contains
  

(defun emmkm-key-binding (key)
  (or
   (if overriding-terminal-local-map
       (lookup-key overriding-terminal-local-map key)
     (key-binding key))
   ;; not all keys are bound to self-insert-command -- e.g. the pound sign.
   'self-insert-command))

;; also add it to isearch-mode-map
(defun make-emulate-mac-keyboard-mode-map (language)
  (let ((emkm-ins-count 0))
    (let ((mode-name (emkm-name language))
	  (map (make-sparse-keymap)))
      (mapcar (lambda (x)
		(setq emkm-ins-count (1+ emkm-ins-count))
		(let ((string-rep (if (stringp (cdr x))
				      (cdr x)
				    (char-to-string (cdr x)))))
		  (define-key map  (car x)
		    (eval (list 'defun  (intern (format "emkm-%s-%d" 
							language emkm-ins-count))
				'(n)
				(format "Insert %s character (%s keyboard layout).
If called with ESC prefix (rather than Meta modifier), 
call the command that would be called if 
ey`%s' was off.

This command is part of `%s'." string-rep language mode-name mode-name)
				'(interactive "p") 
				;; was called using Meta modifier?
				`(if (aq-list-contains (event-modifiers 
						     last-command-event)
						    'meta)
				     (let ((last-command-char ,(if (stringp (cdr x))
								   (string-to-char (cdr x))
								 (cdr x)))
					   (kb (emmkm-key-binding ,string-rep)))
				       (and kb
					    ;; we call the original binding to preserve 
					    ;; functionality (e.g. in isearch)
					    (call-interactively kb)))
				     
				   ;; otherwise: called using Esc prefix.
				   ;; call original binding 
				   (let* ((,mode-name nil)
					  (kb (emmkm-key-binding (this-command-keys))))
				     (and kb
					  (not (eq kb this-command))
					  (call-interactively kb)))))))))
	      (reverse (cdr 
			(assq language 
			      emulate-mac-keyboard-mode-maps))))
      map))) 

(defun emkm-name (lang &optional suf)
  (if suf
      (intern (format "emulate-mac-%s-keyboard-mode%s" lang suf))
    (intern (format "emulate-mac-%s-keyboard-mode" lang))))

(defvar aquamacs-emkm-current-keymap nil 
  "Keymap currently in use by `emulate-mac-<language>-keyboard-mode'.")

(defun turn-off-emulate-mac-keyboard-modes (&optional except-language)
  "Turn off all emulate-mac-keyboard minor modes"
  (aquamacs-emkm-uninstall-overriding-keys)
  (setq aquamacs-emkm-current-keymap nil)
  (mapcar (lambda 
	    (other-language)
	    (if (eq except-language other-language)
		t
	      (funcall (emkm-name other-language) 0)))
	  (mapcar 'car emulate-mac-keyboard-mode-maps)))

; (turn-off-emulate-mac-keyboard-modes)

(defun define-emulate-mac-keyboard-modes ()
"Read `emulate-mac-keyboard-mode-maps' and define a minor mode
for each entry in this alist. The minor mode will apply the
keymap specified there, and turn off all other keyboard emulation
minor modes."
  (mapc 
   (lambda (language)
     ;; define keymap first
     (let ((keymap-sym (emkm-name language "-map")))
       (set keymap-sym
	    (make-emulate-mac-keyboard-mode-map language))
       (eval `(define-minor-mode ,(emkm-name language) 
		"Binds a number of typically used Mac key combinations
to their keyboard-specific equivalents in order to use the 
Option key as Meta, while retaining access to commonly used  
such as [, ], @, etc. This mode is intended to be used with
`mac-option-modifier' set to `meta'.
Other mac keyboard emulation modes are turned off.
This mode has been defined from `emulate-mac-keyboard-mode-maps'
by the function `define-emulate-mac-keyboard-modes'."
		,nil ;; init-value
		,nil ;; lighter
		,keymap-sym ;; keymap
		:global t 
		:group 'Aquamacs 
		(if (or (not (eval ,(emkm-name language)))
			(eq 0 (eval ,(emkm-name language))))
		    (progn (aquamacs-emkm-uninstall-overriding-keys)
			   (setq aquamacs-emkm-current-keymap nil)
			   (remove-hook 'isearch-mode-hook 'aquamacs-emkm-install-overriding-keys))
		  ;; turning it on...
		  ;; disable competing modes
		  (turn-off-emulate-mac-keyboard-modes (quote ,language))
		  ;; Option key is Meta
		  (setq mac-option-modifier 'meta)
		  ;; not which keymap is currently active
		  (setq aquamacs-emkm-current-keymap (quote ,keymap-sym))
		  ;; install isearch hook
		  
		  (add-hook 'isearch-mode-hook 'aquamacs-emkm-install-overriding-keys)
		  (message "Emulating important Option key combinations of %s keyboard layout." (capitalize (symbol-name (quote ,language)))))
		nil
		)))
     (set (emkm-name language) nil))
   (mapcar 'car emulate-mac-keyboard-mode-maps))
  (mapc (lambda (language)
	;; define-key-after won't work - has issues.
	(define-key menu-bar-option-key-menu (vector (list language))
	  (eval `(menu-bar-make-mm-toggle 
		  ,(emkm-name language)
		  ,(format "Emulate some %s Option key combinations" 
			   (capitalize (symbol-name language)))
		  "This mode binds commonly used Option key combinations
to their equivalents used on Mac OS X."
		  (:enable (eq mac-option-modifier 'meta))
		  ))))
	(mapcar 'car emulate-mac-keyboard-mode-maps)))


(defun aquamacs-emkm-install-overriding-keys ()
  "Install keys from `aquamacs-emkm-current-keymap'  into the `overriding-terminal-local-map'."
  (and overriding-terminal-local-map
       aquamacs-emkm-current-keymap
      (map-keymap (lambda (key command)
		    (define-key overriding-terminal-local-map  `[,key] command))
		  (eval aquamacs-emkm-current-keymap))))

(defun aquamacs-emkm-uninstall-overriding-keys ()
  "Uninstall keys in `aquamacs-emkm-current-keymap' from `isearch-mode-map'."
  (and isearch-mode-map
       aquamacs-emkm-current-keymap
      (map-keymap (lambda (key command)
		    (define-key isearch-mode-map  `[,key] nil))
		  (eval aquamacs-emkm-current-keymap))))

;; Define entries for menu

(defvar menu-bar-option-key-menu (make-sparse-keymap "Option Key"))


(defvar mac-option-modifier-enabled-value 'meta)
(defun  toggle-mac-option-modifier (&optional interactively) 
  (interactive "p")
  (unless mac-option-modifier-enabled-value
    (setq mac-option-modifier-enabled-value 'meta))
   (setq mac-option-modifier
	 (if mac-option-modifier
	     (progn
	       (setq mac-option-modifier-enabled-value mac-option-modifier)
	       nil)
	   mac-option-modifier-enabled-value))
   (if interactively (customize-mark-as-set 'mac-option-modifier))
   (message 
    (format "Option key is %s%s" 
	    (if mac-option-modifier 
		""  "not ")
	    (upcase-initials 
	     (symbol-name (or mac-option-modifier 
			      mac-option-modifier-enabled-value))))))

(defvar menu-bar-option-key-menu (make-sparse-keymap "Modifier Keys"))



(define-emulate-mac-keyboard-modes)


(define-key menu-bar-option-key-menu [option-to-system-separator]
  '(menu-item "--"))

(define-key menu-bar-option-key-menu [option-to-system]
  `(menu-item
    ,(aq-shortcut  "Option Key for %s (not extra characters)  "
		   'toggle-mac-option-modifier 
		   (upcase-initials (symbol-name 
				     (or mac-option-modifier 
					 mac-option-modifier-enabled-value))))
    toggle-mac-option-modifier 
    :key-sequence nil
    :visible (boundp 'mac-option-modifier)
    :help "Toggle whether to let Option key behave as Emacs key, 
do not let it produce special characters (passing the key to the system)."
    :button (:toggle . mac-option-modifier)))
 


(define-key-after menu-bar-options-menu [option-key-menu]
  `(menu-item "Option Key" ,menu-bar-option-key-menu)
  'edit-options-separator)

(provide 'emulate-mac-keyboard-mode)
 