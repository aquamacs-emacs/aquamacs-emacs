;; osxkeys.el
;; Mac Style Keyboard Shortcuts 
;; provides osx-key-mode


;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: osxkeys.el,v 1.20 2005/11/04 14:34:36 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/


;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

 
;; Copyright (C) 2005, David Reitter


;; Unit test  / check requirements
(require 'aquamacs-tools)
(aquamacs-require  '(boundp 'mac-command-modifier))
(aquamacs-require  '(boundp 'mac-control-modifier))
(aquamacs-require  '(boundp 'mac-option-modifier))


;; To do: this should only happen when the mode is switched on

(setq mac-option-modifier 'meta) 
(setq mac-control-modifier 'ctrl)
(setq mac-command-modifier 'hyper)
(setq mac-pass-command-to-system t) ;; let system handle Apple-H and the like
;; (this is default anyways)

;; mac-command-is-meta won't work any more at this point!
;; it's deprecated

;;; MacOS X specific stuff

(defvar osxkeys-command-key mac-command-modifier)

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
    (if (< y (eval (frame-parameter f 'top)))
	(modify-frame-parameters f (list (cons 'top y)))
    )
    )
   (yes-or-no-p text)
)
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
  

;; respects goal-column
;; does not respect (yet)  track-eol 
;; unchecked: line-move-ignore-invisible

(defun visual-line-at-point ()
(cdr (nth 6 (posn-at-point))))

(defun visual-col-at-point ()
(car (nth 6 (posn-at-point))))

(defun visual-pixel-col-at-point ()
(car (nth 2 (posn-at-point))))

(defvar visual-movement-temporary-goal-column nil)
(make-variable-buffer-local 'visual-movement-temporary-goal-column)

(defun visual-line-up ()
  (interactive)
  (let ((pixel-col (car (nth 2 (posn-at-point))))
	(visual-col (visual-col-at-point))
	(old-point (point)))

    (vertical-motion 1)	;; one down, to left

    (let ((beg-of-line (point)))

      (goto-char old-point)
      (vertical-motion 0)

;      (if (and (= (point) (point-max))  (= old-point (point-max)))
;	  (vertical-motion 0)
;	(vertical-motion -1)	;; back up, to left 
;	)
    (let* ((next-line-start 
	    (if (not (= (point) (point-max)))
		;; move right, but not further than to end of line
		(prog1 (point)
		  (vertical-motion -1))	    ;; one up again
	      (vertical-motion -1)	    ;; workaround
	      (point-max)))
	   (rel-next-line-start  (- next-line-start (point) 1))
	   )
      ;; approximate positioning
      (if (and (or goal-column visual-movement-temporary-goal-column)
	       (= old-point (- beg-of-line 1)))	;; jumping from end of line
	      
	   (forward-char (min (or goal-column visual-movement-temporary-goal-column) 
			      rel-next-line-start))
	;; else, do complete positioning
	;; save original position  
	(setq visual-movement-temporary-goal-column visual-col)
	;; approximate positioning
	(forward-char (min visual-col rel-next-line-start)) 
	;; correct position
	(let ((new-line (visual-line-at-point)))
	  (if (>= (visual-pixel-col-at-point) pixel-col)
	      (progn 
		(while (and 
			(> (visual-pixel-col-at-point) pixel-col)
			(= (visual-line-at-point) new-line))
		  (forward-char -1))
		(unless (= (visual-line-at-point) new-line)
		  ;; moved too far, beyond the line?
		  (forward-char +1)))
	    (progn 
	       
	      (while (and 
		      (< (visual-pixel-col-at-point) pixel-col)
		      (= (visual-line-at-point) new-line))
		 
		(forward-char +1))
	      (unless (= (visual-line-at-point) new-line)
		(forward-char -1))))))))))

(defun visual-line-down ()
  (interactive)
  (let ((pixel-col (car (nth 2 (posn-at-point))))
	(visual-col (visual-col-at-point))
	(old-point (point)))
    (vertical-motion +1) ;; down
    (let ( 
	  (beg-of-line (point)))
      (unless (= (point) (point-max))
	(vertical-motion +1) ;; down
	(let ((rel-next-line-start  (- (point) beg-of-line 1))) 
	  (goto-char beg-of-line) ;; jump back up
	  ;; approximate positioning
	  (if (and (or goal-column visual-movement-temporary-goal-column)
		   (= old-point (- beg-of-line 1))) ;; jumping from end of line
	      
	      (forward-char (min (or goal-column visual-movement-temporary-goal-column) 
				 rel-next-line-start))
	    ;; else, do complete positioning
	    ;; save original position  
	    (setq visual-movement-temporary-goal-column visual-col)
	    (forward-char (min visual-col rel-next-line-start))
	    ;; correct position
	    (let ((new-line (visual-line-at-point)))
	      (if (> (visual-pixel-col-at-point) pixel-col)
		  (progn 
		    (while (and 
			    (> (visual-pixel-col-at-point) pixel-col)
			    (= (visual-line-at-point) new-line))
		      (forward-char -1))
		    (unless (= (visual-line-at-point) new-line)
		      ;; moved too far, beyond the line?
		      (forward-char +1)))
		(progn 
		  (while (and 
			  (< (visual-pixel-col-at-point) pixel-col)
			  (= (visual-line-at-point) new-line))
		    (forward-char +1))
		  (unless (= (visual-line-at-point) new-line)
		    (forward-char -1)))))))))))


	    
(defun beginning-of-visual-line ()
  (interactive)
  (vertical-motion 0))

(defun end-of-visual-line ()
  (interactive)
  (vertical-motion 1)
  (unless (eq (point) (point-max))
    (backward-char 1)))


(defun clipboard-kill-ring-save-secondary ()
  "Copy secondary selection to kill ring, and save in the X clipboard."
(interactive)
  (if mouse-secondary-overlay
  (let ((x-select-enable-clipboard t)
	(cua-keep-region-after-copy t))
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

(defun make-osx-key-mode-map (&optional command-key)
"Create a mode map for OSX key mode. COMMAND-KEY specifies
which key is mapped to command. mac-command-modifier is the
default."
(garbage-collect) ;; attempted workaround
(if command-key
    (setq osxkeys-command-key command-key)
  (if mac-command-modifier
      (setq osxkeys-command-key mac-command-modifier)
    )
)
(let ((map (make-sparse-keymap)))
    (define-key map `[(,osxkeys-command-key \?)] 'aquamacs-user-help)
    (define-key map `[(,osxkeys-command-key shift \?)] 'aquamacs-emacs-manual)

    (define-key map `[(,osxkeys-command-key n)] 'new-frame-with-new-scratch) ;open new frame empty
    (define-key map `[(,osxkeys-command-key o)] 'mac-key-open-file) ;open new frame with a file

    (define-key map `[(,osxkeys-command-key shift s)] 'write-file)
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
    (define-key map `[(,osxkeys-command-key p)] 'aquamacs-print)
    (define-key map `[(,osxkeys-command-key l)] 'goto-line)
    (define-key map `[(,osxkeys-command-key f)] 'isearch-forward)
    (define-key map `[(,osxkeys-command-key g)] 'isearch-repeat-forward)
    (define-key map `[(,osxkeys-command-key w)] 'close-current-window-asktosave)
    (define-key map `[(,osxkeys-command-key m)] 'iconify-or-deiconify-frame) 
    (define-key map `[(,osxkeys-command-key .)] 'keyboard-quit)
    (define-key map `[(,osxkeys-command-key escape)] 'keyboard-escape-quit)
    (define-key map `[(,osxkeys-command-key up)] 'beginning-of-buffer)
    (define-key map `[(,osxkeys-command-key down)] 'end-of-buffer)
    (define-key map `[(,osxkeys-command-key left)] 'beginning-of-line)
    (define-key map `[(,osxkeys-command-key right)] 'end-of-line)

    (define-key map '[up] 'visual-line-up)
    (define-key map '[down] 'visual-line-down)
    (define-key map `[(,osxkeys-command-key left)] 'beginning-of-visual-line)
    (define-key map `[(,osxkeys-command-key right)] 'end-of-visual-line)

    (define-key map `[(,osxkeys-command-key backspace)] 'kill-whole-line)


    (define-key map `[(meta up)] 'cua-scroll-down)
    (define-key map `[(meta down)] 'cua-scroll-up)
    
    (define-key map `[(,osxkeys-command-key \;)] 'toggle-mac-option-modifier)

    (define-key map [(home)] 'beginning-of-buffer)
    (define-key map [(end)] 'end-of-buffer)

    (define-key map `[(control ,osxkeys-command-key q)] 'kill-emacs)
    (define-key map `[(,osxkeys-command-key q)] 'save-buffers-kill-emacs)
    (define-key map (vector (list osxkeys-command-key '\,) ) 'customize)

    (define-key map `[(,osxkeys-command-key z)] 'undo)
    (define-key map `[(,osxkeys-command-key shift z)] 'redo)
    (define-key map `[(,osxkeys-command-key \`)] 'switch-to-next-frame)
    map)
)


(defvar osx-key-mode-map
  (make-osx-key-mode-map)
  "Keymap for `osx-key-mode'.")
 
(define-minor-mode osx-key-mode
  "Toggle Mac Key mode.
With arg, turn Mac Key mode on iff arg is positive.
When Mac Key mode is enabled, mac-style key bindings are provided."
  :global t
  :group 'osx-key-mode 
  :keymap 'osx-key-mode-map  
  )
 


;; Change encoding so you can use alt-e and alt-u accents (and others) 
(set-terminal-coding-system 'iso-8859-1) 
(set-keyboard-coding-system				  'mac-roman) ;; keyboard
(set-selection-coding-system			  'mac-roman) ;; copy'n'paste
 

(provide 'osxkeys)