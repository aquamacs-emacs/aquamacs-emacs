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


;(aq-preload "mwheel") ;; wants to be loaded at runtime
(aq-preload "disp-table")
(aq-preload "tool-bar")
(aq-preload "image") ;; taken out so as to not initialize the path variable
(aq-preload "button")
(aq-preload "view")
(aq-preload "help-mode")
(aq-preload "help-fns")

(aq-preload "emacs-lisp/debug")
(aq-preload "emacs-lisp/bytecomp")
(aq-preload "emacs-lisp/byte-opt")
(aq-preload "emacs-lisp/advice")
(aq-preload "custom")
;(aq-preload "emacs-lisp/cl")
;(aq-preload "emacs-lisp/cl-seq")
(aq-preload "international/encoded-kb")
(aq-preload "wid-edit.el")
; (aq-preload "emacs-lisp/easymenu") ;; needs to be loaded at runtime... causes strange behavior otherwise
; (aq-preload "recentf") 
 (let ((aq-preload-path  
	(append load-path
		(list (expand-file-name "emulation" (car load-path))))))
   (aq-preload "emulation/cua-base")
   (aq-preload "emulation/pc-select")
   )
(aq-preload "delsel")
(aq-preload "paren")
(aq-preload "calendar/time-date")
(aq-preload "timezone")
(aq-preload "calendar/parse-time")
 
;(aq-preload "emacs-lisp/cl-macs")
;(aq-preload "emacs-lisp/cl")
;(aq-preload "emacs-lisp/cl-seq")
;(aq-preload "emacs-lisp/easy-mmode.el")

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

;; (aq-preload "mail/rfc822.el")
;; (aq-preload "mail/mail-utils.el")
;; (aq-preload "international/mule-util.el")

;; (aq-preload "assoc.el")
;; (aq-preload "speedbar.el")
;; (aq-preload "mail/rmail.el")
;; (aq-preload "mail/sendmail.el")


;; (aq-preload "mail/emacsbug.el")
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
