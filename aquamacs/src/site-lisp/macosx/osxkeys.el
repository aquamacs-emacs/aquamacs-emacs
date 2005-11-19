 ;; osxkeys.el
;; Mac Style Keyboard Shortcuts 
;; provides osx-key-mode


;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: osxkeys.el,v 1.43 2005/11/19 18:03:47 davidswelt Exp $

;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/

;; Attribution: Leave this header intact in case you redistribute this file.
;; Attribution must be given in application About dialog or similar,
;; "Contains Aquamacs osx-key-mode by D Reitter" does the job.
;; Apart from that, released under the GPL:
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
(setq mac-command-modifier 'alt)
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
 (or (cdr (nth 6 (posn-at-point))) 0))

(defun visual-col-at-point ()
  (or (car (nth 6 (posn-at-point))) 0))

(defun visual-pixel-col-at-point ()
  (or (car (nth 2 (posn-at-point))) 0))

(defvar visual-movement-temporary-goal-column nil)
(make-variable-buffer-local 'visual-movement-temporary-goal-column)

(defun visual-line-up (num-lines)
  (interactive "p")
  (if (bobp) (signal 'beginning-of-buffer nil))
  (let ((pixel-col (car (nth 2 (posn-at-point))))
	(visual-col (visual-col-at-point))
	(old-point (point))
	(beg-of-line))

    (save-excursion
      (vertical-motion 1)	;; trying going one down, to left
      (setq beg-of-line (point)))
 
      (vertical-motion 0)

    (let* ((next-line-start 
		;; move right, but not further than to end of line
	    (prog1 (point)
	      (vertical-motion (- num-lines))))	    ;; one up again
	   (rel-next-line-start  (- next-line-start (point) 1)))
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
		(forward-char -1)))))))))

(defun visual-line-down (num-lines)
 (interactive "p")
 (if (eobp) (signal 'end-of-buffer nil))
  (let ((pixel-col (car (nth 2 (posn-at-point))))
	(visual-col (visual-col-at-point))
	(old-point (point))
	(beg-of-line)
	(rel-next-line-start))
    (vertical-motion num-lines) ;; down
    (save-excursion
      (setq beg-of-line (point))
      (vertical-motion +1) ;; down
      (setq rel-next-line-start  (- (point) beg-of-line 1)))
    (unless (= beg-of-line (point-max))
	  ;; approximate positioning
	  (if (and (or goal-column visual-movement-temporary-goal-column)
		   (= old-point (- beg-of-line 1))) ;; jumping from end of line
	      
	      (forward-char (min (or goal-column 
				     visual-movement-temporary-goal-column) 
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
		    (forward-char -1)))))))))

	    
(defun beginning-of-visual-line ()
  (interactive)
  (if (bobp)
      (signal 'beginning-of-buffer nil))
  (vertical-motion 0))

(defun end-of-visual-line ()
  (interactive)
  (if (eobp)
      (signal 'end-of-buffer nil))
  (let ((end-of-line (line-end-position)))
    (vertical-motion 1)
     (unless (or (eobp)
		 (< (point) end-of-line)) ;; jumping over wrapped text
      (backward-char 1))))

;; this code based on simple.el
(defun kill-visual-line (&optional arg)
  "Kill the rest of the visual line; if no nonblanks there, kill thru newline.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.
With zero argument, kills the text before point on hthe current line.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

To kill a whole line, when point is not at the beginning, type \
\\[beginning-of-line] \\[kill-line] \\[kill-line].

If `kill-whole-line' is non-nil, then this command kills the whole line
including its terminating newline, when used at the beginning of a line
with no argument.  As a consequence, you can always kill a whole line
by typing \\[beginning-of-line] \\[kill-line].

If you want to append the killed line to the last killed text,
use \\[append-next-kill] before \\[kill-line].

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive "P")
  (kill-region (point)
	       ;; It is better to move point to the other end of the kill
	       ;; before killing.  That way, in a read-only buffer, point
	       ;; moves across the text that is copied to the kill ring.
	       ;; The choice has no effect on undo now that undo records
	       ;; the value of point from before the command was run.
	       (progn
		 (if arg
		     (vertical-motion (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (let ((end
			  (save-excursion
			    (end-of-visual-line) (point))))
		     (if (or (save-excursion
			       ;; If trailing whitespace is visible,
			       ;; don't treat it as nothing.
			       (unless show-trailing-whitespace
				 (skip-chars-forward " \t" end))
			       (= (point) end))
			     (and kill-whole-line (bolp)))
			 (visual-line-down 1)
		       (goto-char end))))
		 (point))))


(defun kill-whole-visual-line (&optional arg)
  "Kill current visual line.
With prefix arg, kill that many lines starting from the current line.
If arg is negative, kill backward.  Also kill the preceding newline.
\(This is meant to make \\[repeat] work well with negative arguments.\)
If arg is zero, kill current line but exclude the trailing newline."
  (interactive "p")
  (if (and (> arg 0) (eobp) (save-excursion (vertical-motion 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0) (bobp) (save-excursion (vertical-motion 1) (bobp)))
      (signal 'beginning-of-buffer nil))
  (unless (eq last-command 'kill-region)
    (kill-new "")
    (setq last-command 'kill-region))
  (cond ((zerop arg)
	 ;; We need to kill in two steps, because the previous command
	 ;; could have been a kill command, in which case the text
	 ;; before point needs to be prepended to the current kill
	 ;; ring entry and the text after point appended.  Also, we
	 ;; need to use save-excursion to avoid copying the same text
	 ;; twice to the kill ring in read-only buffers.
	 (save-excursion
	   ;; delete in one go
	   (kill-region (progn (vertical-motion 0) (point))
			(progn (vertical-motion 1) (point)))
	 ))
	((< arg 0)
	 (save-excursion
	   (kill-region (point) (progn (end-of-visual-line) (point))))
	 (kill-region (point)
		      (progn (vertical-motion (1+ arg))
			     (unless (bobp) (backward-char))
			     (point))))
	(t
	 (save-excursion
	   (kill-region (progn (vertical-motion 0) (point))
			(progn (vertical-motion arg) (point)))))))


;; mark functions for CUA
(dolist (cmd
	 '( beginning-of-visual-line 
	    end-of-visual-line
	    visual-line-down visual-line-up))
  (put cmd 'CUA 'move))



(defun aquamacs-clipboard-kill-ring-save-secondary ()
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

(defun aquamacs-clipboard-kill-secondary ()
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
     (message "The secondary selection is not set.")))

(defmacro allow-line-as-region-for-function (orig-function)
`(defun ,(intern (concat (symbol-name orig-function) "-or-line")) 
   ()
   ,(format "Like `%s', but acts on the current line if mark is not active." orig-function)
   (interactive)
   (if mark-active
       (call-interactively (function ,orig-function))
     (save-excursion 
       ;; define a region (temporarily) -- so any C-u prefixes etc. are preserved.
       (beginning-of-line)
       (set-mark (point))
       (end-of-line)
       (call-interactively (function ,orig-function))))))

(allow-line-as-region-for-function comment-region)
(allow-line-as-region-for-function uncomment-region)
(allow-line-as-region-for-function comment-or-uncomment-region)




; (setq garbage-collection-messages t)
(defun debug-keymap-corruption ()
  (interactive)
   (with-temp-buffer
     "*aq-temp*"
     (with-output-to-temp-buffer "*aq-temp*"
       (print "current global map:")
       (print (current-global-map))
       (print osx-key-mode-map))
       (write-region nil nil "/tmp/Aquamacs-Corrupt-Keymap.log.el")
     )
   (kill-buffer "*aq-temp*")

  (with-current-buffer "*Messages*"
	       (write-region nil nil "/tmp//Aquamacs-Messages.log.el"))	     

  ;; try to restore key map
(when nil
  (if global-map-backup
      (use-global-map global-map-backup)
    )
  (if osx-key-mode-map-backup
      (use-local-map nil)
    ))
  (mac-dialog "Internal data corruption -- Some data restored." 
"Corrupted keymaps restored from backup. 
Please save your work and restart Aquamacs.
Consider filing a bug report with Help/Send Bug Report.
Debug info left in /tmp/Aquamacs-Corrupt-Keymap.log.el."))
 
(add-hook 'after-init-hook 
	  (lambda () 
	    
	    ;; make a copy of it as a workaround attempt
	   ;;  (use-global-map (copy-keymap (current-global-map)))
	    (setq global-map-backup global-map)
	    )
	  )


;;  aquamacs context menu


(defun aquamacs-mouse-get-word ()
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if last-nonmenu-event ;; mouse used
	(save-excursion
	  (mouse-set-point last-nonmenu-event)
	  (mouse-skip-word -1)
	  (set-mark (point))
	  (mouse-set-point last-nonmenu-event)
	  (mouse-skip-word 1)
	  (buffer-substring-no-properties (region-beginning) (region-end)))
      (thing-at-point 'word))))

    

;; doubles in mailclient.el
(defun aquamacs-encode-string-as-url (string)
  "Convert STRING to a URL, using utf-8 as encoding."
  (apply (function concat)
	 (mapcar
	  (lambda (char)
	    (cond
	     ((eq char ?\x20) "%20")   ;; space
	     ((eq char ?\n) "%0D%0A")  ;; newline 
	     ((string-match "[-a-zA-Z0-9_:/.@]" (char-to-string char))
	      (char-to-string char))   ;; printable
	     (t                        ;; everything else
	      (format "%%%02x" char))))	;; escape
	  ;; Convert string to list of chars
	  (append (encode-coding-string string 'utf-8)))))

(defun aquamacs-google-lookup ()
  (interactive)
  (let ((word (aquamacs-mouse-get-word)))
    (if word
	(browse-url  
	 (concat "http://www.google.com/search?q="  
		 (aquamacs-encode-string-as-url 
		  (substring word 0 (min (length word) 128))))))))

(defun aquamacs-dictionary-lookup ()
  (interactive)
  (let ((word (aquamacs-mouse-get-word)))
    (if word
	(do-applescript (concat 
			 "tell application \"Dictionary\" to activate
tell application \"System Events\"
	tell process \"Dictionary\"
		tell text field 1 of group 1 of tool bar 1 of window \"Dictionary and Thesaurus\"
			keystroke \"" 
			 (replace-regexp-in-string 
			  "[\\\\\"]" ""
			  (substring word 0 (min (length word) 32))) "\"
			keystroke return
		end tell
	end tell
end tell")))))

(defun aquamacs-make-mouse-buffer-menu ( )
  "Return a menu keymap of buffers for selection with the mouse.
This switches buffers in the window that you clicked on,
and selects that window."
  (let ((buffers (buffer-list))  alist menu split-by-major-mode sum-of-squares)
    ;; Make an alist of elements that look like (MENU-ITEM . BUFFER).
    (let ((tail buffers))
      (while tail
	;; Divide all buffers into buckets for various major modes.
	;; Each bucket looks like (MODE NAMESTRING BUFFERS...).
	(with-current-buffer (car tail)
	  (let* ((adjusted-major-mode major-mode) elt)
	    (let ((tail mouse-buffer-menu-mode-groups))
	      (while tail
		(if (string-match (car (car tail)) mode-name)
		    (setq adjusted-major-mode (cdr (car tail))))
		(setq tail (cdr tail))))
	    (setq elt (assoc adjusted-major-mode split-by-major-mode))
	    (if (null elt)
		(setq elt (list adjusted-major-mode
				(if (stringp adjusted-major-mode)
				    adjusted-major-mode
				  mode-name))
		      split-by-major-mode (cons elt split-by-major-mode)))
	    (or (memq (car tail) (cdr (cdr elt)))
		(setcdr (cdr elt) (cons (car tail) (cdr (cdr elt)))))))
	(setq tail (cdr tail))))
    ;; Compute the sum of squares of sizes of the major-mode buckets.
    (let ((tail split-by-major-mode))
      (setq sum-of-squares 0)
      (while tail
	(setq sum-of-squares
	      (+ sum-of-squares
		 (let ((len (length (cdr (cdr (car tail)))))) (* len len))))
	(setq tail (cdr tail))))
    (if (< (* sum-of-squares mouse-buffer-menu-mode-mult)
	   (* (length buffers) (length buffers)))
	;; Subdividing by major modes really helps, so let's do it.
	(let (subdivided-menus (buffers-left (length buffers)))
	  ;; Sort the list to put the most popular major modes first.
	  (setq split-by-major-mode
		(sort split-by-major-mode
		      (function (lambda (elt1 elt2)
				  (> (length elt1) (length elt2))))))
	  ;; Make a separate submenu for each major mode
	  ;; that has more than one buffer,
	  ;; unless all the remaining buffers are less than 1/10 of them.
	  (while (and split-by-major-mode
		      (and (> (length (car split-by-major-mode)) 3)
			   (> (* buffers-left 10) (length buffers))))
	    (let ((this-mode-list (mouse-buffer-menu-alist
				   (cdr (cdr (car split-by-major-mode))))))
	      (and this-mode-list
		   (setq subdivided-menus
			 (cons (cons
				(nth 1 (car split-by-major-mode))
				this-mode-list)
			       subdivided-menus))))
	    (setq buffers-left
		  (- buffers-left (length (cdr (car split-by-major-mode)))))
	    (setq split-by-major-mode (cdr split-by-major-mode)))
	  ;; If any major modes are left over,
	  ;; make a single submenu for them.
	  (if split-by-major-mode
	      (let ((others-list
		     (mouse-buffer-menu-alist
		      ;; we don't need split-by-major-mode any more,
		      ;; so we can ditch it with nconc.
		      (apply 'nconc (mapcar 'cddr split-by-major-mode)))))
		(and others-list
		     (setq subdivided-menus
			   (cons (cons "Others" others-list)
				 subdivided-menus)))))
	  (setq menu (cons "Buffer Menu" (nreverse subdivided-menus))))
      (progn
	(setq alist (mouse-buffer-menu-alist buffers))
	(setq menu (cons "Buffer Menu"
			 (mouse-buffer-menu-split "Select Buffer" alist)))))
  
    (let ((km (make-sparse-keymap)))
      (mapc (lambda (pair)
	      (define-key km (vector (intern (car pair)))
		`(menu-item ,(car pair) 
			,(eval
			  (list 'lambda () 
				'(interactive)
				`(let ((one-buffer-one-frame nil))
				   (switch-to-buffer ,(cdr pair)))))
	      ))) alist)
      km
	 )  )
  )
 
(defvar aquamacs-context-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [paste] (cons "Paste" 'clipboard-yank))
    (define-key map [copy] (cons "Copy" 'clipboard-kill-ring-save))
    (define-key map [cut] (cons "Cut" 'clipboard-kill-region))
    (define-key map [aq-cm-sep] '(menu-item "--"))
    (define-key map [dictionary] (cons "Look Up in Dictionary" 
				   'aquamacs-dictionary-lookup))
    (define-key map [google] (cons "Search in Google" 
				   'aquamacs-google-lookup))
    (define-key map [aq-cm-sep3] '(menu-item "--"))
    (define-key map [switch-buffer] nil)
    (define-key map [aq-cm-sep4] '(menu-item "--"))
    (define-key map [yank-here] '(menu-item "Yank here" 
				     mouse-yank-at-click
				     :enable kill-ring))
    ;; (define-key map [spotlight] (cons "Search in Spotlight" 
    ;;				   'aquamacs-spotlight-lookup))

   map) "Keymap for the Aquamacs context menu.")

(defvar aquamacs-popup-context-menu-buffers-state nil)
(defun aquamacs-popup-context-menu  (event &optional  prefix)
  "Popup a context menu. 
Its content is specified in the keymap `aquamacs-context-menu-map'."
  (interactive "@e \nP")
  ;; Let the mode update its menus first.
  ;; (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (if (frame-or-buffer-changed-p 'aquamacs-popup-context-menu-buffers-state)
    (define-key aquamacs-context-menu-map [switch-buffer] 
      `(menu-item "Switch to Buffer "   
	      ,(aquamacs-make-mouse-buffer-menu)
	      :help "Show a different buffer in this frame")))
  ;; move popup menu a little so mouse pointer is over first entry
  ;; not needed
  ;; ((pos
  ;; 	 (if (not (eq (event-basic-type event) 'mouse-3))
  ;; 	     event
  ;; 	   (list (lispost (- (car (nth 2 (car (cdr event)))) 0)
  ;; 		       (- (cdr (nth 2 (car (cdr event)))) 0))
  ;; 		 (car (car (cdr event)))))))
  
    (popup-menu aquamacs-context-menu-map event prefix))





(defun make-osx-key-mode-map (&optional command-key)
  "Create a mode map for OSX key mode. COMMAND-KEY specifies
which key is mapped to command. mac-command-modifier is the
default."
  ;; (garbage-collect) ;; attempted workaround
  (if command-key
      (setq osxkeys-command-key command-key)
    (if mac-command-modifier
	(setq osxkeys-command-key mac-command-modifier)
      )
    )
  (let ((map (make-sparse-keymap)))

    ;; debug log

  
    (define-key map `[(,osxkeys-command-key shift t)] 'debug-keymap-corruption)

    (define-key map [mouse-3] 'aquamacs-popup-context-menu)


    (define-key map `[(,osxkeys-command-key \?)] 'aquamacs-user-help)
    (define-key map `[(,osxkeys-command-key shift \?)] 'aquamacs-emacs-manual)

    (define-key map `[(,osxkeys-command-key n)] 'new-frame-with-new-scratch) 
					;open new frame empty
    (define-key map `[(,osxkeys-command-key o)] 'mac-key-open-file) 
					;open new frame with a file

    (define-key map `[(,osxkeys-command-key shift s)] 'write-file)
    (define-key map `[(,osxkeys-command-key shift o)] 'find-file-other-frame) 
					;open new frame with a file
    (define-key map `[(,osxkeys-command-key a)] 'mark-whole-buffer)
    (define-key map `[(,osxkeys-command-key v)] 'clipboard-yank) 
    (define-key map `[(,osxkeys-command-key c)] 'clipboard-kill-ring-save)
    (define-key map `[(shift ,osxkeys-command-key c)] 
      'aquamacs-clipboard-kill-ring-save-secondary)
    ;; this because the combination control-space usually activates Spotlight
    (define-key map `[(control ,osxkeys-command-key space)] 'set-mark)
    (define-key map `[(,osxkeys-command-key x)] 'clipboard-kill-region)
    (define-key map `[(shift ,osxkeys-command-key x)] 'aquamacs-clipboard-kill-secondary)
    (define-key map `[(,osxkeys-command-key s)] 'save-buffer)
    (define-key map `[(,osxkeys-command-key p)] 'aquamacs-print)
    (define-key map `[(,osxkeys-command-key l)] 'goto-line)
    (define-key map `[(,osxkeys-command-key f)] 'isearch-forward)
    (define-key map `[(,osxkeys-command-key g)] 'isearch-repeat-forward)  
    (define-key map `[(,osxkeys-command-key shift g)] 'isearch-repeat-backward)
    (define-key map `[(,osxkeys-command-key w)] 'close-current-window-asktosave)
    (define-key map `[(,osxkeys-command-key m)] 'iconify-or-deiconify-frame) 
    (define-key map `[(,osxkeys-command-key .)] 'keyboard-quit)
    (define-key map `[(,osxkeys-command-key escape)] 'keyboard-escape-quit)
    (define-key map `[(,osxkeys-command-key up)] 'beginning-of-buffer)
    (define-key map `[(,osxkeys-command-key down)] 'end-of-buffer)
    (define-key map `[(,osxkeys-command-key left)] 'beginning-of-line)
    (define-key map `[(,osxkeys-command-key right)] 'end-of-line)

    (define-key global-map `[(,osxkeys-command-key {)] 'comment-region-or-line)
    (define-key global-map `[(,osxkeys-command-key })] 'uncomment-region-or-line)
    (define-key global-map `[(,osxkeys-command-key \')] 'comment-or-uncomment-region-or-line)

    (define-key map '[remap previous-line] 'visual-line-up)
    (define-key map '[remap next-line] 'visual-line-down)
    (define-key map `[(,osxkeys-command-key left)] 'beginning-of-visual-line)
    (define-key map `[(,osxkeys-command-key right)] 'end-of-visual-line)

    (define-key map `[(,osxkeys-command-key delete)] 'kill-visual-line)
    (define-key map `[(,osxkeys-command-key backspace)] 'kill-whole-visual-line)

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
;; (setq  osx-key-mode-map (make-osx-key-mode-map))

(define-minor-mode osx-key-mode
  "Toggle Mac Key mode.
With arg, turn Mac Key mode on iff arg is positive.
When Mac Key mode is enabled, mac-style key bindings are provided."
  :global t
  :group 'osx-key-mode 
  :keymap 'osx-key-mode-map  


;; Change encoding so you can use alt-e and alt-u accents (and others) 
  (set-terminal-coding-system 'iso-8859-1) 
  (set-keyboard-coding-system 'mac-roman) ;; keyboard
  (set-selection-coding-system 'mac-roman) ;; copy'n'paste
 
  (setq mac-emulate-three-button-mouse (if osx-key-mode
					   'ctrl
					 nil)))
  



(provide 'osxkeys)