;; osxkeys.el
;; Mac Style Keyboard Shortcuts

;; by David Reitter, david.reitter@gmail.com, 2005

;; provides osx-key-mode

;; This file is part of Aquamacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.



(setq mac-option-modifier 'meta) 
(setq mac-control-modifier 'ctrl)
(setq mac-command-modifier 'hyper)
(setq mac-pass-option-to-system t) ;; let system do stuff with option
(setq mac-pass-command-to-system t) ;; let system handle Apple-H and the like
;; (this is default anyways)

;; mac-command-is-meta won't work any more at this point!
;; it's deprecated

;;; MacOS X specific stuff

(setq osxkeys-command-key 'hyper)

;; Define the return key to avoid problems on MacOS X
(define-key function-key-map [return] [13])

;; Option (alt) will act like in other Mac programs
;; if it is used without a standard modifier, it is interpreted as 'Meta'
;; use Esc instead of Option (alt) if you need Meta for a reserved combination.
  
;; allow selection of secondary buffer
  
(defun aquamacs-yes-or-no-p (text)

(let ((f (window-frame (minibuffer-window))))

  (raise-frame f) ; make sure frame is visible
  (let ((y (- (display-pixel-height) (frame-total-pixel-height f) 30 ))) ; extra 30 pix for typical Dock
    (print y)
    (if (< y (frame-parameter f 'top))
	(modify-frame-parameters f (list (cons 'top y)))
    )
    )
   (yes-or-no-p text)
)
)

(defun close-current-window-asktosave ()
  "Delete currently window (and its frame), ask to save file if necessary."
  (interactive)
  (select-frame-set-input-focus (selected-frame))
 
      (let ((wind (selected-window))
	      (killable (and (killable-buffer-p (window-buffer))
			     (eq (length (find-all-frames-internal 
					  (window-buffer) 
					  'only_visible_ones)) 
				 1)
			     )
			)
	      )
	; ask before killing
	(cond ( (and (eq (current-buffer) (window-buffer)) ;; only if a document is shown
		     killable
		     (eq   (string-match "\\*.*\\*" (buffer-name)) nil)
		     (eq   (string-match " SPEEDBAR" (buffer-name)) nil) ; has no minibuffer!
		     )
		(cond ((buffer-modified-p)
		       (if (progn
			     (unless (minibuffer-window)
			       (setq last-nonmenu-event nil)
			       )
			     (aquamacs-yes-or-no-p "Save this buffer to file before closing window? ")
			     )
			   (progn
			     (save-buffer)
			     (message "File saved.")
			     )
			 ; mark as not modified, so it will be killed for sure
			 (set-buffer-modified-p nil)
			 ))
		      ((message ""))
		       
		      )      )
	      )
  

	
	  ;; only if not a *special* buffer
	  ;; if the buffer is shown in another window , just delete the current win
	  
	(if
	  (if killable 
	      (kill-buffer (window-buffer))    
	    t
	    )
	  ; always delete in this situation
	    ; unless user said "no"
	    (progn
	      (message "") ; we don't want a message in the echo area of the next window!
	      (delete-window-if-created-for-this-buffer wind (window-buffer) t)
	      )
	  )	
	)
   t 
  ) 

 
(require 'aquamacs-tools)

 

(require 'filladapt)

(require 'mac-extra-functions)

(defun switch-to-next-frame ()
  (interactive)
  (select-frame-set-input-focus (next-frame))
)
(require 'redo)
  
;; remove existing bindings that don't exist on the mac
(global-unset-key [cut])
(global-unset-key [copy])
(global-unset-key [paste])
(global-unset-key [f20])
(global-unset-key [f16])
(global-unset-key [f18])
   
  
(defun clipboard-kill-ring-save-secondary ()
  "Copy secondary selection to kill ring, and save in the X clipboard."
(interactive)
  (if mouse-secondary-overlay
  (let ((x-select-enable-clipboard t))
    (clipboard-kill-ring-save 
     (overlay-start mouse-secondary-overlay) 
     (overlay-end mouse-secondary-overlay) )
    (message "Secondary selection saved to clipboard and kill-ring.")
    )
  ; else
  (message "The secondary selection is not set.")
  )
)

(defun clipboard-kill-secondary ()
  "Kill the secondary selection, and save it in the X clipboard."
   (interactive)
   (if mouse-secondary-overlay
       (let ((x-select-enable-clipboard t))
	 (clipboard-kill-region 
	  (overlay-start mouse-secondary-overlay)
	  (overlay-end mouse-secondary-overlay))
	 (message "Secondary selection saved to clipboard and kill-ring, then killed.")    
	 )
					; else
     (message "The secondary selection is not set.")
     )
)


(defvar osx-key-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map `[(,osxkeys-command-key \?)] 'aquamacs-user-help)
    (define-key map `[(,osxkeys-command-key shift \?)] 'aquamacs-emacs-manual)

    (define-key map `[(,osxkeys-command-key n)] 'new-frame-with-new-scratch) ;open new frame empty
    (define-key map `[(,osxkeys-command-key o)] 'mac-key-open-file) ;open new frame with a file

    (define-key map `[(,osxkeys-command-key shift s)] 'mac-key-save-file-as)
    (define-key map `[(,osxkeys-command-key shift o)] 'find-file-other-frame) ;open new frame with a file
    (define-key map `[(,osxkeys-command-key a)] 'mark-whole-buffer)
    (define-key map `[(,osxkeys-command-key v)] 'clipboard-yank) 
    (define-key map `[(,osxkeys-command-key c)] 'clipboard-kill-ring-save)
    (define-key map `[(shift ,osxkeys-command-key c)] 'clipboard-kill-ring-save-secondary)
    ; this because the combination control-space usually activates Spotlight
    (define-key map `[(control ,osxkeys-command-key space)] 'set-mark)
    (define-key map `[(,osxkeys-command-key x)] 'clipboard-kill-region)
    (define-key map `[(shift ,osxkeys-command-key x)] 'clipboard-kill-secondary)
    (define-key map `[(,osxkeys-command-key s)] 'save-buffer)
    (define-key map `[(,osxkeys-command-key l)] 'goto-line)
    (define-key map `[(,osxkeys-command-key f)] 'isearch-forward)
    (define-key map `[(,osxkeys-command-key g)] 'isearch-repeat-forward)
    (define-key map `[(,osxkeys-command-key w)] 'close-current-window-asktosave)
    (define-key map `[(,osxkeys-command-key m)] 'iconify-or-deiconify-frame) 
    (define-key map `[(,osxkeys-command-key .)] 'keyboard-quit)
    (define-key map `[(,osxkeys-command-key \e)] 'keyboard-escape-quit)
    (define-key map `[(,osxkeys-command-key up)] 'beginning-of-buffer)
    (define-key map `[(,osxkeys-command-key down)] 'end-of-buffer)
    (define-key map `[(,osxkeys-command-key left)] 'beginning-of-line)
    (define-key map `[(,osxkeys-command-key right)] 'end-of-line)
    (define-key map `[(,osxkeys-command-key backspace)] 'kill-whole-line)

    (define-key map [(home)] 'beginning-of-buffer)
    (define-key map [(end)] 'end-of-buffer)

    (define-key map `[(control ,osxkeys-command-key q)] 'kill-emacs)
    (define-key map `[(,osxkeys-command-key q)] 'save-buffers-kill-emacs)
;    (define-key map `[(,osxkeys-command-key ",")] 'customize)

    (define-key map `[(,osxkeys-command-key z)] 'undo)
    (define-key map `[(,osxkeys-command-key shift z)] 'redo)
    (define-key map `[(,osxkeys-command-key \`)] 'switch-to-next-frame)
    map)
  "Keymap for `osx-key-mode'.")

(define-minor-mode osx-key-mode
  "Toggle Mac Key mode.
With arg, turn Mac Key mode on iff arg is positive.
When Mac Key mode is enabled, mac-style key bindings are provided."
  :global t
  :group 'osx-key-mode
					;  :lighter " M"
  :keymap 'osx-key-mode-map
  (if osx-key-mode
      (progn

;        (setq osx-key-mode-backup-command-key-is-meta mac-command-key-is-meta
;	      osx-key-mode-backup-pc-selection-mode pc-selection-mode)
;	(setq mac-command-key-is-meta nil)

;	(setq pc-select-selection-keys-only t) ;; disaple M-backspace = undo    	
;	(pc-selection-mode t)

        (define-key-after menu-bar-file-menu [my-file-separator]
          '("--" . nil) 'recover-session)
        (define-key-after menu-bar-file-menu [mac-show-in-finder]
          '("Show In Finder" . mac-key-show-in-finder) 'my-file-separator)
;        (define-key-after menu-bar-file-menu [mac-open-terminal]
;          '("Open Terminal" . mac-key-open-terminal) 'mac-show-in-finder)

					;    (when (file-exists-p osx-key-mode-gs-command)
					;      (defalias 'ps-mule-header-string-charsets 'ignore) ;;*
					;      (ad-enable-regexp "osx-key-mode-preview*")
					;      )
        )
    (progn

 ;     (setq mac-command-key-is-meta osx-key-mode-backup-command-key-is-meta)
  ;    (pc-selection-mode osx-key-mode-backup-pc-selection-mode)

      (define-key global-map [menu-bar file my-file-separator] nil)
      (define-key global-map [menu-bar file mac-show-in-finder] nil)
;      (define-key global-map [menu-bar file mac-open-terminal] nil)

					;      (when (file-exists-p osx-key-mode-gs-command)
					;        (ad-disable-regexp "osx-key-mode-preview*")
					;        )
      ))
  ) 

;; change encoding so you can use alt-e and alt-u accents (and others) 
(set-terminal-coding-system 'iso-8859-1) 
(set-keyboard-coding-system				  'mac-roman) ;; keyboard
(set-selection-coding-system			  'mac-roman) ;; copy'n'paste
 

(provide 'osxkeys)