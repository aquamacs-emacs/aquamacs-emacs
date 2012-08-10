;; -*-mode:emacs-lisp; coding: utf-8;-*-'
;; emulate-mac-*-keyboard-modes for Aquamacs


;; Aquamacs Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; Aquamacs Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; (C) 2005, 2007, 2010, 2011, 2012 by David Reitter

;;
;;
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
inserted for the key. Prefixed keybindings are currently not supported.
Example:
 ((german . ((\"\\M-l\" . \"@\")
 	     (\"\\M-/\" . \"\\\\\"))))")

(setq emulate-mac-keyboard-mode-maps
 `((german . (("\M-l" . "@")
	       ("\M-/" . "\\")
	       ("\M-2" .  "“")
	       ("\M-\"" . "”")
	       ("\M-5" . "[")
	       ("\M-6" . "]")
	       ("\M-7" . "|")
	       ("\M-8" . "{")
	       ("\M-9" . "}")
	       ("\M-n" . "~")
	       ("\M-e" . "€") ;; euro symbol
	       ))
    (french . (([?\M-`] . "@")
	       ("\M-$" . "€") 
	       ("\M-/" . "\\")
	       ([?\M-£]  . "#") ;; was: "\M-£"
	       ("\M-n" . "~")
	       ("\M-L" . "|")
	       ("\M-(" . "{")
	       ("\M-5" . "[")
	       ("\M-)" . "}")
	       ([?\M-°] . "]")))
    (spanish . (([?\M-`] . "[")
              ("\M-+" . "]")
              ("\M-5" . "€")
              ([?\M-´]  . "{")
              ([?\M-ç]  . "}") 
              ("\M-3" . "#")
              ("\M-2" . "@")
              ("\M-1" . "|")
              ([?\M-º] . "\\")
	      ([?\M-ñ] . "~")))
    (italian . (;;("\M-§" . "@")  ;; this seems to cause problems
		([?\M-¤]  . "@") ; wont work either
		("\M-(" . "{")
		("\M-4" . "[")
		("\M-)" . "}")
		("\M-7" . "]")  
		("\M-\:" . "|")))
    (italian-pro . 
		(([?\M-ò] . "@")
		 ("\M-5" . "~") 
		 ([?\M-è] . "[") ;;  was ,(kbd "M-\217")
		 ([?\M-é] . "{") ;;      ,(kbd "M-\216")
		 ("\M-*" . "}")
		 ("\M-+" . "]")
		 ([?\M-à] . "#") ;;  was  ,(kbd "M-\210")
		 ))
    (finnish . (("\M-2" . "@")
 		("\M-4" . "$")
 		("\M-/" . "\\")
;;		(,[?\M-¨ 32] . "~") ;; won't work - prefix keybinding
		(,[?\M-¨] . "~")
 ;;		(,(quote [134219944]) . "~") ;; an alternative
 		("\M-7" . "|")
 		("\M-(" . "{")
 		("\M-8" . "[")
 		("\M-)" . "}")
 		("\M-9" . "]")))

    (swiss-german . (("\M-g" . "@")
	       ("\M-/" . "\\")
	       ("\M-3" . "#")
	       ("\M-4" . "Ç")
	       ("\M-5" . "[")
	       ("\M-6" . "]")
	       ("\M-7" . "|")
	       ("\M-8" . "{")
	       ("\M-9" . "}")
	       ("\M-n" . "~")
	       ("\M-s" . "ß")
	       ("\M-o" . "ø")
	       ("\M-O" . "Ø") 
	       ;; ("\M-a" . "å") ;; overlaps with Emacs default 
	       ;; ("\M-A" . "Å") ;; overlaps with Emacs default M-a
	       ("\M-e" . "€") ;; euro symbol
	       ))

    (swiss-french . (("\M-g" . "@")
	       ("\M-/" . "\\")
	       ("\M-3" . "#")
	       ("\M-4" . "Ç")
	       ("\M-5" . "[")
	       ("\M-6" . "]")
	       ("\M-7" . "|")
	       ("\M-8" . "{")
	       ("\M-9" . "}")
	       ("\M-n" . "~")
	       ("\M-s" . "ß")
	       ("\M-o" . "ø")
	       ("\M-O" . "Ø") 
	       ;; ("\M-a" . "å") ;; overlaps with Emacs default M-a
	       ;; ("\M-A" . "Å") ;; overlaps with Emacs default M-a
	       ("\M-e" . "€") ;; euro symbol
	       ))
    (us . (     ("\M-3" . "£")
		("\M-@" . "€") ;; euro symbol
		("\M-6" . "§")))
    (british . (("\M-3" . "#")
		("\M-2" . "€") ;; euro symbol
		("\M-6" . "§")))))

;; (progn (define-emulate-mac-keyboard-modes) (make-emulate-mac-keyboard-mode-map 'german) (emulate-mac-german-keyboard-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'aquamacs-tools) ;; aq-list-contains

;; also add it to isearch-mode-map
(defun make-emulate-mac-keyboard-mode-map (language)
  (let ((emkm-ins-count 0))
    (let ((mode-name (emkm-name language))
	  (map (make-sparse-keymap)))
      (mapcar (lambda (x)
		(setq emkm-ins-count (1+ emkm-ins-count))
		(let* ((string-rep (if (stringp (cdr x))
				      (cdr x)
				    (char-to-string (cdr x))))
		       (vec-rep (string-to-vector string-rep)))
		  (define-key map  (car x)
		    (eval (list 'defun  (intern (format "emkm-%s-%d" 
							language emkm-ins-count))
				nil
				(format "Insert %s character (%s keyboard layout).
If called with ESC prefix (rather than Meta modifier), 
call the command that would be called if key `%s' was off.

This command is part of `%s'." string-rep language mode-name mode-name)
				'(interactive) 
				;; was called using Meta modifier?
				;; caveat: because of this, prefix key bindings are not supported.
				;; To Do: check whether a command is bound to the ESC x alternative. 
				;; Or, better yet, check explicitly for ESC use. (How?)
				`(if (aq-list-contains (event-modifiers 
						     last-command-event)
						    'meta)
				     (emkm-execute-kbd-macro ,vec-rep)
				   ;; otherwise: called using Esc prefix.
				   ;; call original binding 
				   (let ((,mode-name nil))
				     (emkm-execute-kbd-macro (this-command-keys)))
				   ))))))
	      (reverse (cdr 
			(assq language 
			      emulate-mac-keyboard-mode-maps))))
      map)))

(defun emkm-execute-kbd-macro (macro)
  "Like `execute-kbd-macro'
Does not terminate when bell is rung."
  (let ((kbd-macro-termination-hook nil))
    (condition-case nil
	(execute-kbd-macro macro)
      (error nil))
    ;; special rule for isearch mode (ugly hack!)
    (if isearch-mode
	(isearch-update))))

(defun emkm-name (lang &optional suf)
  (if suf
      (intern (format "emulate-mac-%s-keyboard-mode%s" lang suf))
    (intern (format "emulate-mac-%s-keyboard-mode" lang))))

(defvar aquamacs-emkm-current-keymap nil 
  "Keymap currently in use by `emulate-mac-<lan
guage>-keyboard-mode'.")

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

(defvar aquamacs-emkm-saved-mac-option-key-modifier nil)

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
such as [, ], @, etc. This modewill set `mac-option-modifier' 
to `meta'. Other mac keyboard emulation modes are turned off.
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
			   (setq mac-option-modifier aquamacs-emkm-saved-mac-option-key-modifier)
			   (remove-hook 'isearch-mode-hook 'aquamacs-emkm-install-overriding-keys))
		  ;; turning it on...
		  ;; disable competing modes
		  (turn-off-emulate-mac-keyboard-modes (quote ,language))
		  ;; 
		  (unless aquamacs-emkm-current-keymap ;; if no emulation mode is active
		    (setq aquamacs-emkm-saved-mac-option-key-modifier mac-option-modifier))
		  ;; Option key is Meta
		  (setq mac-option-modifier 'meta)
		  ;; note which keymap is currently active
		  (setq aquamacs-emkm-current-keymap (quote ,keymap-sym))
		  ;; install isearch hook
		  (add-hook 'isearch-mode-hook 'aquamacs-emkm-install-overriding-keys)
		  (message ,(format "Emulating important Option key combinations of %s keyboard layout." (capitalize (symbol-name (quote language))))))
		nil
		)))
     (set (emkm-name language) nil))
   (mapcar 'car emulate-mac-keyboard-mode-maps))
  (mapc (lambda (language)
	;; define-key-after won't work - has issues.
	(define-key menu-bar-option-key-menu (vector (list language))
	  (eval `(menu-bar-make-mm-toggle 
		  ,(emkm-name language)
		  ,(format "...Meta & %s" 
			   (let ((str (symbol-name language))) 
			     (if (> (length str) 3)
				 (capitalize str)
			       (upcase str))))
		  "This mode binds commonly used Option key combinations
to their equivalents used on Mac OS X."
		  (:enable t)
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

(defvar menu-bar-option-key-menu (make-sparse-keymap "Option, Command, Meta keys"))


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
   (unless mac-option-modifier
     (turn-off-emulate-mac-keyboard-modes (quote ,language)))
   (message 
    (format "Option key is %s%s" 
	    (if mac-option-modifier 
		""  "not ")
	    (upcase-initials 
	     (symbol-name (or mac-option-modifier 
			      mac-option-modifier-enabled-value))))))

(defvar menu-bar-option-key-menu (make-sparse-keymap "Modifier Keys"))
 
(defun ns-modifier-setting-description (right general)
  (if (or (not right)
	  (and (eq right 'none) (eq ns-alternate-modifier 'none)))
      "is system's key modifier"
	(format "set to %s" 
		(if (eq right 'none) 
		    general right))))

(defun toggle-mac-right-option-modifier (&optional interactively)
  (interactive "p")
  (setq ns-right-alternate-modifier
	(if (eq 'meta
		(if (eq ns-right-alternate-modifier 'none)
		    ns-alternate-modifier ns-right-alternate-modifier))
	    'none
	  'meta))
  (if interactively (customize-mark-as-set 'ns-right-alternate-modifier))
  (message "Right Option %s." 
	   (ns-modifier-setting-description ns-right-alternate-modifier ns-alternate-modifier)))

(defun toggle-mac-right-command-modifier (&optional interactively)
  (interactive "p")
  (setq ns-right-command-modifier
	(if (eq 'meta
		(if (eq ns-right-command-modifier 'none)
		    ns-command-modifier ns-right-command-modifier))
	    'none  ; same function as left command modifier
	  'meta))
  (if interactively (customize-mark-as-set 'ns-right-command-modifier))
  (message "Right Command %s." (ns-modifier-setting-description ns-right-command-modifier ns-command-modifier)))


(define-key menu-bar-option-key-menu [right-command]
  `(menu-item "Right Command is Meta"
    toggle-mac-right-command-modifier 
    :visible (boundp 'mac-command-modifier)
    :help "Toggle whether to let the Right Command key behave as Meta key, 
do not let it produce special characters (passing the key to the system)."
    :button (:toggle . 
		     (eq 'meta
			 (or (if (eq ns-right-command-modifier 'none)
				 ns-command-modifier ns-right-command-modifier) 'none)))))
		    
(define-key menu-bar-option-key-menu [right-option]
  `(menu-item "Right Option is Meta"
    toggle-mac-right-option-modifier 
    :visible (boundp 'mac-option-modifier)
    :help "Toggle whether to let the Right Option key behave as Meta key, 
do not let it produce special characters (passing the key to the system)."
    :button (:toggle . 
		     (eq 'meta
			 (or (if (eq ns-right-alternate-modifier 'none)
				 ns-alternate-modifier ns-right-alternate-modifier) 'none)))))
		     
(define-key menu-bar-option-key-menu [right-sep]
  '(menu-item "--"))

(define-emulate-mac-keyboard-modes)

(defun ns-alternate-modifier-true-value ()
  (if (eq mac-option-modifier 'none)
      nil
    mac-option-modifier))


(define-key menu-bar-option-key-menu [option-is-meta]
  `(menu-item
    (format  "Option is %s"
	     (upcase-initials (symbol-name 
			       (or (ns-alternate-modifier-true-value) 
				   mac-option-modifier-enabled-value))))
    toggle-mac-option-modifier 
;; not yet known.    :key-sequence [(,osxkeys-command-key \;)]
    :visible (boundp 'mac-option-modifier)
    :help "Toggle whether to let Option key behave as Emacs key, 
do not let it produce special characters (passing the key to the system)."
    :button (:toggle . (and mac-option-modifier  (not aquamacs-emkm-current-keymap)))))
 
(define-key menu-bar-option-key-menu [option-to-system]
  `(menu-item "Option for composed characters   "
    toggle-mac-option-modifier 
;; not yet known.    :key-sequence [(,osxkeys-command-key \;)]
    :visible (boundp 'mac-option-modifier)
    :help "Toggle whether to let Option key behave as Emacs key, 
do not let it produce special characters (passing the key to the system)."
    :button (:toggle . (not mac-option-modifier))))
 

(define-key-after menu-bar-options-menu [option-key-menu]
  `(menu-item "Option, Command, Meta keys" ,menu-bar-option-key-menu)
  'mule-separator)


;; doesn't work:
;; ;;;; Keyboard layout/language change events
;; (defun aquamacs-handle-language-change (event)
;;   "Set keyboard coding system to what is specified in EVENT."
;;   (interactive "e")
;;   (setq aquamacs-current-system-keyboard-layout (car (cadr event)))
;;   (mac-handle-language-change event))

;; (define-key special-event-map [language-change] 'aquamacs-handle-language-change)


(provide 'emulate-mac-keyboard-mode)
