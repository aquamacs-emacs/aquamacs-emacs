;; compatibility bridge for Emacs trunk / cocoa port.

;; mostly missing functions.

; stuff dealt with elsewhere in Aquamacs:
; winmgr-display-available-pixel-bounds  (smart-frame-positioning.el)


;; Cocoa bindings
(unless (boundp 'mac-command-modifier)
  (defvaralias 'mac-command-modifier 'ns-command-modifier))
(unless (boundp 'mac-command-modifier)
  (defvaralias 'mac-control-modifier 'ns-control-modifier))
(unless (boundp 'mac-command-modifier)
  (defvaralias 'mac-option-modifier 'ns-option-modifier))

(mapc 
 (lambda (x)
   (let ((d (intern (concat "mac-" x)))
	 (s (intern (concat "ns-" x))))
     (and (not (fboundp d)) (fboundp s)
	  (defalias d s))))
 '("show-menu-bar" "hide-menu-bar" "dialog" "dialog-y-or-n-p" 
   "set-key-script" "get-current-key-script" "get-last-key-script" 
   "launch-URL-with-default-browser"))


(unless (fboundp 'winmgr-display-available-pixel-bounds)
  (if (fboundp 'mac-display-available-pixel-bounds)
      (fset 'winmgr-display-available-pixel-bounds 
	    'mac-display-available-pixel-bounds))
  (if (fboundp 'x-display-usable-bounds)
      (fset 'winmgr-display-available-pixel-bounds 
	    'x-display-usable-bounds)))
 


(unless (fboundp 'do-applescript)
  (defun do-applescript (s)
    (message "Warning: do-applescript call not implemented yet.")))

(defun turn-on-font-lock-if-enabled ())

(provide 'cocoa-compatibility)