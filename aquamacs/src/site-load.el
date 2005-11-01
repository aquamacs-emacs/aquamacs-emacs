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
;(load "aquamacs/applescript-mode.el")
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
;; (load "aquamacs/aquamacs-bug.el")
;(load "aquamacs/aquamacs-mac-fontsets.el")

;(load "longlines.el")
;(load "aquamacs/aquamacs-menu.el")
;(load "aquamacs/aquamacs-mode-defaults.el")
; (load "aquamacs/aquamacs-tool-bar.el")
; (load "aquamacs/auctex-config.el")

;(load "aquamacs/better-buffer-menu.el")
;(load "aquamacs/carbon-font.el")
(aq-preload "check-for-updates.el")
;(load "aquamacs/color-theme.el")
(aq-preload "aquamacs/color-theme-themes.el")

;(load "aquamacs/css-mode.el")
;(load "aquamacs/def-face-const.el")

;(load "aquamacs/strings.el")
;(load "aquamacs/files+.el")
;(load "aquamacs/filladapt.el")
;(load "aquamacs/fit-frame.el")
;(load "aquamacs/frame+.el")
;(load "aquamacs/frame-cmds.el")
;(load "aquamacs/frame-fns.el")

;(load "icomplete.el")
;(load "aquamacs/icomplete+.el")
;(load "aquamacs/mac-drag-N-drop.el")
;(load "aquamacs/autofit-frame.el")
;(load "aquamacs/aquamacs-frame-setup.el")
;(load "aquamacs/oneonone.el")
;(load "aquamacs/smart-frame-positioning.el")
;(load "aquamacs/osx_defaults.el")
;(load "aquamacs/redo.el")
;(load "aquamacs/osxkeys.el")
;(load "aquamacs/php-mode.el")

;(load "aquamacs/ruby-mode.el")
; (load "aquamacs/site-start.el")
