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

(provide 'cocoa-compatibility)