;; Aquamacs core files
;; to be loaded and included in dumped state at compile time
 
(defvar aq-preloaded nil
"List of preloaded (precomiled) features.")
(defun aq-preload (f)  
  (let ((features-before features))
    (load (concat aq-compile-path f))
    (mapc (lambda (e)
	    (unless (member e features-before)
	      (setq aq-preloaded (cons e aq-preloaded))))
	    features)))


;(load "mwheel") ;; wants to be loaded at runtime
(load "disp-table")
(load "tool-bar")
(load "image") ;; taken out so as to not initialize the path variable
(load "button")
(load "view")
(load "help-mode")
(load "help-fns")

(load "emacs-lisp/debug")
(load "emacs-lisp/bytecomp")
(load "emacs-lisp/byte-opt")
(load "emacs-lisp/advice")
(load "custom")
;(load "emacs-lisp/cl")
;(load "emacs-lisp/cl-seq")
(load "international/encoded-kb")
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
(aq-preload "aquamacs-tools.el")
(aq-preload "macosx/mac-extra-functions.el")
;(aq-preload "applescript-mode.el")
; the following can't be precompiled. reason unknown.
;; no text available if this is compiled in. 
;;(aq-preload "aquamacs-mode-specific-themes")
(aq-preload "aquamacs.el")
(aq-preload "macosx/osx_defaults.el")

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

;(aq-preload "longlines.el")
;(aq-preload "aquamacs-menu.el")
;(aq-preload "aquamacs-mode-defaults.el")
; (aq-preload "aquamacs-tool-bar.el")
; (aq-preload "auctex-config.el")

;(aq-preload "better-buffer-menu.el")
;(aq-preload "carbon-font.el")
(aq-preload "check-for-updates.el")
;(aq-preload "color-theme.el")
(aq-preload "color-theme-themes.el")

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
