;; Aquamacs core files
;; to be loaded and included in dumped state at compile time
;; test
 
(defvar aq-compile-path "../aquamacs/src/site-lisp/")

(defvar aq-preloaded nil
  "List of preloaded (precomiled) features.")

(defvar aq--preloading-features-before features)

(defmacro aq-preload (f)
  `(load (concat aq-compile-path ,f)))

;(load "mwheel") ;; wants to be loaded at runtime
(load "disp-table")
;; (load "tool-bar")  ;; taken out while we're working on it!
(load "tooltip")
(load "font-lock")
(load "jit-lock")
(load "image") ;; path issue should be alright now
(load "image-file")
(load "button")
(load "view")
(load "help-mode")
(load "help-fns")

(load "emacs-lisp/debug")
(load "emacs-lisp/bytecomp")
(load "emacs-lisp/byte-opt")
(load "emacs-lisp/advice")
(load "emacs-lisp/regexp-opt")
;; (load "emacs-lisp/syntax") ; maybe not: syntax-ppss-stats is mutable
;;(load "custom") ;; loading this seems to cause problems with doc strings (why?) e.g. custom-file
;(load "emacs-lisp/cl")
;(load "emacs-lisp/cl-seq")

;; (load "international/encoded-kb")  must not be reloaded (creates mutable objects in purespace) 
(load "wid-edit.el")
; (load "emacs-lisp/easymenu") ;; needs to be loaded at runtime... causes strange behavior otherwise
; (load "recentf") 



 (let ((load-path  
	(append load-path
		(list (expand-file-name "emulation" (car load-path))))))
   (load "emulation/cua-base")
   (load "emulation/pc-select")
   )
(load "delsel")
(load "paren")
(load "calendar/time-date")
(load "timezone")
(load "calendar/parse-time")
 
;(load "emacs-lisp/cl-macs")
;(load "emacs-lisp/cl")
;(load "emacs-lisp/cl-seq")
;(load "emacs-lisp/easy-mmode.el")

;; aquamacs
;; the function aq-preload is supplied by the make-aquamacs script
(aq-preload "aquamacs-macros")
(aq-preload "aquamacs-tools")
(aq-preload "macosx/mac-extra-functions")
;(aq-preload "applescript-mode")
; the following can't be precompiled. reason unknown.
;; no text available if this is compiled in. 
;;(aq-preload "aquamacs-mode-specific-themes")
(aq-preload "aquamacs")
(aq-preload "aquamacs-tool-bar")
(aq-preload "macosx/osx_defaults")
(aq-preload "macosx/aquamacs-menu")
; these define minor modes
;(aq-preload "macosx/emulate-mac-keyboard-mode")
;(aq-preload "macosx/osxkeys")
(aq-preload "macosx/mac-extra-functions")
;; autoface must be compiled (due to require aq-cl)
;; to do: change make scripts to require compilation of all those
;; before dumping, not after. 
;(aq-preload "aquamacs-autoface-mode")
;(aq-preload "one-buffer-one-frame") [define-minor-mode]
;(aq-preload "smart-frame-positioning")
;(aq-preload "visual-line")
(aq-preload "check-for-updates")
(aq-preload "aquamacs-redo")
;(aq-preload "auctex-config")

;; (load "mail/rfc822.el")
;; (load "mail/mail-utils.el")
;; (load "international/mule-util.el")

;; (load "assoc.el")
;; (load "speedbar.el")
;; (load "mail/rmail.el")
;; (load "mail/sendmail.el")


;; (load "mail/emacsbug.el")
;; (aq-preload "aquamacs-bug.el")
;(aq-preload "aquamacs-mac-fontsets.el")

;(aq-preload "aquamacs-menu.el")
;(aq-preload "aquamacs-mode-defaults.el")
; (aq-preload "aquamacs-tool-bar.el")
; (aq-preload "auctex-config.el")

;(aq-preload "better-buffer-menu].el")
;(aq-preload "carbon-font.el")
(aq-preload "check-for-updates.el")
;(aq-preload "color-theme.el")
;; too large - will fail
;;(aq-preload "color-theme-themes.el")

;(aq-preload "css-mode.el")
;(aq-preload "def-face-const.el")

;(aq-preload "strings.el")
;(aq-preload "files+.el")
;(aq-preload "filladapt.el")
;(aq-preload "fit-frame.el")
;(aq-preload "frame+.el")
;(aq-preload "frame-cmds.el")
;(aq-preload "frame-fns.el")

;(aq-preload "icomplete.el")
;(aq-preload "icomplete+.el")
;(aq-preload "mac-drag-N-drop.el")
;(aq-preload "autofit-frame.el")
;(aq-preload "aquamacs-frame-setup.el")
;(aq-preload "oneonone.el")
;(aq-preload "smart-frame-positioning.el")
;(aq-preload "osx_defaults.el")
;(aq-preload "redo.el")
;(aq-preload "osxkeys.el")
;(aq-preload "php-mode.el")

;(aq-preload "ruby-mode.el")
; (aq-preload "site-start.el")



(mapc (lambda (e)
	    (unless (member e aq--preloading-features-before)
	      (setq aq-preloaded (cons e aq-preloaded))))
	    features)
 
;; correct paths in load-history
(setq load-history
      (mapcar
       (lambda (e)
	 (let ((case-fold-search nil))
	   (if 
	       (and (car-safe e)
		    (string-match (concat "^" aq-compile-path 
					  "\\(.*\\)$") 
				  (car e)))
	       (cons (concat "../site-lisp/"
		      (match-string 1 (car e)))
		     (cdr e))
	     e)))
       load-history))
