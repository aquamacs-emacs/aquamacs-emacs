;; osxkeys.el
;; Mac Style Keyboard Shortcuts 
;; provides osx-key-mode


;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/

;; Attribution: Leave this header intact in case you redistribute this file or
;; any of the code contained.
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

 
;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010 David Reitter


;; Unit test  / check requirements
(require 'aquamacs-tools)
(aquamacs-require  '(boundp 'ns-command-modifier))
(aquamacs-require  '(boundp 'ns-control-modifier))
(aquamacs-require  '(boundp 'ns-alternate-modifier))

(require 'emulate-mac-keyboard-mode)

;; To do: this should only happen when the mode is switched on

(aquamacs-set-defaults '((ns-alternate-modifier meta) 
;;(setq mac-control-modifier nil) ;; use default
			 (ns-command-modifier alt)
			 (ns-alternate-meta-special-codes 
(
 ?\xff51 ?\xff52 ?\xff53 ?\xff54 ;; arrow keys
	 ?\xff08 ; backspace
	 ?\xff9f ; delete
	 ?\xff09 ; tab
	 ?\xff0d ; return
	 ?\xff50 ; home
	 ?\xff57 ; end
	 ?\xff55 ; page-up
	 ?\xff56 ; page-down
	 ))
;; let system handle Apple-H and the like
;; (this is default anyways)
))

;; mac-command-is-meta won't work any more at this point!
;; it's deprecated

;;; MacOS X specific stuff

(defvar osxkeys-command-key ns-command-modifier
"Command key used for `osx-key-mode'.
Defaults to the value of `ns-command-modifier'.
You will need to run

\(setq  osx-key-mode-map (make-osx-key-mode-map))

after updating this variable.")

;; Define the return key to avoid problems on MacOS X
(define-key function-key-map [return] [13])

;; Option (alt) will act like in other Mac programs
;; if it is used without a standard modifier, it is interpreted as 'Meta'
;; use Esc instead of Option (alt) if you need Meta for a reserved combination.
  
;; allow selection of secondary buffer
   
 

(require 'filladapt)

(require 'mac-extra-functions)
 
(require 'aquamacs-redo)
  
;; remove existing bindings that don't exist on the mac
(global-unset-key [cut])
(global-unset-key [copy])
(global-unset-key [paste])
(global-unset-key [f20])
(global-unset-key [f16])
(global-unset-key [f18])
  

(defvar cua--explicit-region-start) ;; in case CUA isn't loaded

(aquamacs-set-defaults '((x-select-enable-clipboard t)))

;; support copy&paste at the right level
(setq interprogram-paste-function 'aquamacs-cut-buffer-or-selection-value)

(defun aquamacs-cut-buffer-or-selection-value ()
  (when x-select-enable-clipboard
    (let (text)
      ;; Consult the selection, then the cut buffer.  Treat empty strings
      ;; as if they were unset.
      (or text (setq text (ns-get-pasteboard)))
      (if (string= text "") (setq text nil))
      (cond
       ((not text) nil)
       ((eq text ns-last-selected-text) nil)
       ((string= text ns-last-selected-text)
	;; Record the newer string, so subsequent calls can use the `eq' test.
	(setq ns-last-selected-text text)
	nil)
       (t (setq ns-last-selected-text text))))))

;; overwrite x-select-text, to honor x-select-enable-clipboard
(defun x-select-text (text &optional push)
  "Put TEXT, a string, on the pasteboard.
Ignored if text was selected by mouse. PUSH is ignored."
  (when x-select-enable-clipboard
    (ns-set-pasteboard text))
  (setq ns-last-selected-text text))

(defun aquamacs-backward-char ()
  "Move point to the left or the beginning of the region.
 Like `backward-char', but moves point to the beginning of the region
provided `cua-mode' and the mark are active."
  (interactive)
  (let ((left (min (point) (or (mark t) 0))))

    (if (and cua-mode transient-mark-mode 
	     mark-active
	     (not cua--explicit-region-start)
	     (not this-command-keys-shift-translated))
	(goto-char left)
      (let ((this-command 'backward-car)) ;; maintain compatibility
	(call-interactively 'backward-char)))))


(defun aquamacs-forward-char (&rest args)
  "Move point to the right or the end of the region.
 Like `forward-char', but moves point to the end of the region
provided `cua-mode' and the mark are active."
  (interactive)
  (let ((right (max (point) (or (mark t) 0))))

    (if (and cua-mode transient-mark-mode 
	     mark-active
	     (not cua--explicit-region-start)
	     (not this-command-keys-shift-translated))
	(goto-char right)
       (let ((this-command 'forward-car)) ;; maintain compatibility
	 (call-interactively 'forward-char)))))

(dolist (cmd
	 '(aquamacs-backward-char 
	   aquamacs-forward-char
	   aquamacs-previous-line
	   aquamacs-previous-line))
  (put cmd 'CUA 'move))

(defun aquamacs-previous-line (&optional arg try-vscroll)
  "Move cursor vertically up ARG buffer lines.
Like `previous-line', but move by logical buffer lines
if `visual-line-mode' is off and `line-move-visual' is set to `arrow-keys-only'."
  (interactive "^p\np")
  (setq this-command 'previous-line)  ; ensure last-command will be set
  ;; visual-line-mode sets line-move-visual to t (unconditionally)
  (let ((line-move-visual (and line-move-visual
			       (not (eq line-move-visual 'arrow-keys-only)))))
    (previous-line arg try-vscroll)))

(defun aquamacs-next-line (&optional arg try-vscroll)
  "Move cursor vertically down ARG buffer lines.
Like `next-line', but move by logical buffer lines
if `visual-line-mode' is off and `line-move-visual' is set to `arrow-keys-only'."
  (interactive "^p\np")
  (setq this-command 'next-line)  ; ensure last-command will be set
  ;; visual-line-mode sets line-move-visual to t (unconditionally)
  (let ((line-move-visual (and line-move-visual
			       (not (eq line-move-visual 'arrow-keys-only)))))
    (next-line arg try-vscroll)))


(defun beginning-of-visual-line (&optional n)
  "Move point to the beginning of the current line.
If `word-wrap' is nil, we move to the beginning of the buffer
line (as in `beginning-of-line'); otherwise, point is moved to
the beginning of the visual line."
  (interactive)
  (if word-wrap
      (progn 
	(if (and n (/= n 1))
	    (vertical-motion (1- n))
;; the following would need Emacs 23
;; 	    (let ((line-move-visual t))
;; 	      (line-move (1- n) t)))
	  (vertical-motion 0))
	(skip-read-only-prompt))
    (beginning-of-line n)))

(defun end-of-visual-line (&optional n)
  "Move point to the end of the current line.
If `word-wrap' is nil, we move to the end of the line (as in
`beginning-of-line'); otherwise, point is moved to the end of the
visual line."
  (interactive)
  (if word-wrap
      (unless (eobp)
	(progn
	  (if (and n (/= n 1))
	      (vertical-motion (1- n))
	    (vertical-motion 1))
	  (skip-chars-backward " \r\n" (- (point) 1))))
    (end-of-line n)))


(defun aquamacs-move-beginning-of-line (arg)
 "Move point to beginning of current buffer line.
As `move-beginning-of-line', but move by logical buffer lines
if `visual-line-mode' is off and `line-move-visual' is set to `arrow-keys-only'."
  (interactive "^p")
  (setq this-command 'move-beginning-of-line)  ; ensure last-command will be set
  ;; visual-line-mode sets line-move-visual to t (unconditionally)
  (let ((line-move-visual (and line-move-visual
			       (not (eq line-move-visual 'arrow-keys-only)))))
    (if line-move-visual
	(progn 
	  (if (and arg (/= arg 1))
	      (vertical-motion (1- arg))
	    (vertical-motion 0))
	  (skip-read-only-prompt))
      (move-beginning-of-line arg))))

(defun aquamacs-move-end-of-line (arg)
 "Move point to end of current buffer line.
As `move-end-of-line', but move by logical buffer lines
if `visual-line-mode' is off and `line-move-visual' is set to `arrow-keys-only'."
  (interactive "^p")
  (setq this-command 'move-end-of-line)  ; ensure last-command will be set
  ;; visual-line-mode sets line-move-visual to t (unconditionally)
  (let ((line-move-visual (and line-move-visual
			       (not (eq line-move-visual 'arrow-keys-only)))))
    (if line-move-visual
	(unless (eobp)
	  (progn
	    (if (and arg (/= arg 1))
		(vertical-motion (1- arg))
	      (vertical-motion 1))
	    (skip-chars-backward " \r\n" (- (point) 1))))
      (move-end-of-line arg))))
  
;; mark functions for CUA
(dolist (cmd
	 '( beginning-of-visual-line
	    end-of-visual-line
	    aquamacs-move-beginning-of-line
	    aquamacs-move-end-of-line))
 (put cmd 'CUA 'move))

(defun aquamacs-kill-word (&optional arg)
  "Kill characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (and transient-mark-mode mark-active)
      (kill-region (region-beginning) (region-end))
    (kill-region (point) 
		 (let ((at-wb
			(or 
			 (not smart-spacing-mode)
			 (eolp)
			 (string-match 
			  "\\w" 
			  (buffer-substring-no-properties 
			   (point) (min (point-max) (1+ (point))))))))
		   (forward-word arg) 
		   (if (not at-wb)
		       (if (> arg 0)
			   (skip-chars-forward " " 1)
			 (skip-chars-backward " " 1)))
		   (point)))))

(defun aquamacs-backward-kill-word (&optional arg)
  "Kill characters backward until encountering the beginning of a word.
With argument, do this that many times."
  (interactive "p")
  (aquamacs-kill-word (- (or arg 1))))


;; respects goal-column
;; does not respect (yet)  track-eol 
;; unchecked: line-move-ignore-invisible
(defun buffer-line-at-point ()
 (or (cdr (nth 2 (posn-at-point))) 0))

;; the following functions are necessary because we want
;; to disable cut/copy if mark is not active
;; doing so directly in `clipboard-kill-ring-save' etc
;; would hurt their functionality and cause bugs (e.g., mailclient)

; just binding those to A-c and A-x won't work: 
; cua binds them to cua-copy-region
(defun clipboard-kill-ring-save-active-region (beg end)
  "Like `clipboard-kill-ring-save', but only if mark is active.
\(Or if `transient-mark-mode' is off.)"
  (interactive "r")
  (when (or mark-active (not transient-mark-mode))
    (setq this-command 'clipboard-kill-ring-save)
    (clipboard-kill-ring-save beg end)))

(defun clipboard-kill-active-region (beg end)
  "Like `clipboard-kill-region', but only if mark is active.
\(Or if `transient-mark-mode' is off.)"
  (interactive "r")
  (when (or mark-active (not transient-mark-mode))
    (setq this-command 'clipboard-kill-region)
    (clipboard-kill-region beg end)))

(defun aquamacs-clipboard-kill-ring-save-secondary ()
  "Copy secondary selection to kill ring, and save in the X clipboard."
  (interactive)
  (if (and mouse-secondary-overlay
	   (overlay-start mouse-secondary-overlay)
	   (overlay-end mouse-secondary-overlay))
      (let ((x-select-enable-clipboard t)
	    (mark-was-active mark-active))
	(kill-ring-save 
	 (overlay-start mouse-secondary-overlay) 
	 (overlay-end mouse-secondary-overlay) )
	(message "Secondary selection saved to clipboard and kill-ring.")
	(setq mark-active mark-was-active
	      deactivate-mark nil)
	)
					; else
    (message "The secondary selection is not set.")
    ))

(defun aquamacs-clipboard-kill-secondary ()
  "Kill the secondary selection, and save it in the X clipboard."
  (interactive)
  (if mouse-secondary-overlay
      (let ((x-select-enable-clipboard t)
	    (mark-was-active mark-active))
	(kill-region 
	 (overlay-start mouse-secondary-overlay)
	 (overlay-end mouse-secondary-overlay))
	(message "Secondary selection saved to clipboard and kill-ring, then killed.")
	(setq mark-active mark-was-active
	      deactivate-mark nil)
	)
    (message "The secondary selection is not set.")))


(defcustom set-region-to-isearch-match t
  "Whether to set the region after searching.
If non-nil, the mark will be set after searching with
`aquamacs-repeat-isearch', `aquamacs-repeat-isearch-backward'
such that the region matches the search match, provided
`transient-mark-mode' is on.
If it is `always', it will also be set 
whenever isearch-mode is exited, even if it was invoked with
`isearch-foward' and friends."
  :group 'Aquamacs
  :type '(choice (const nil) (const t) (const always)))

(defun aquamacs-set-region-to-search-match ()
  ;; match beginning / end aren't guaranteed to be defined here (e.g., in flyspell-mode)
  (when (and set-region-to-isearch-match
	     (or aquamacs-isearching
		 (eq set-region-to-isearch-match 'always))
	     transient-mark-mode (not mark-active)) ; mark could have been set explicitly: don't change it
      (set-mark isearch-other-end))
  (setq aquamacs-isearching))

(defvar aquamacs-isearching nil)

(defun aquamacs-isearch-forward ()
  (interactive)
  (setq aquamacs-isearching t)
  (call-interactively 'isearch-forward))

(defun aquamacs-isearch-backward ()
  (interactive)
  (setq aquamacs-isearching t)
  (call-interactively 'isearch-backward)) 

(defun aquamacs-repeat-isearch ()
  "Repeats the last string isearch.
Set region to match if `set-region-to-isearch-match'.
Wraps around after throwing and error once."
  (interactive)
  (setq aquamacs-isearching t)
  (if set-region-to-isearch-match
    (progn
      (if (or (and (eq last-command 'aquamacs-repeat-isearch)
		   (not mark-active))) ;; failed error has been shown once (and mark deactivated)
	  (condition-case nil
	      (search-forward isearch-string)
	    (error 
	     (let (new-point)
	       (save-excursion
		 (beginning-of-buffer)
		 (condition-case x
		     (progn (search-forward isearch-string)
			    (setq new-point (point)))
		   (error
		    (signal (car x) (cdr x)))))
	       (and new-point (goto-char new-point)))))
      (deactivate-mark)
      (search-forward isearch-string))
      (set-mark (match-beginning 0)))
    (isearch-repeat 'forward)))

(defun aquamacs-repeat-isearch-backward ()
  "Repeats the last string isearch backwards.
Set region to match. 
Wraps around after throwing and error once."
  (interactive)
  (setq aquamacs-isearching t)
  (if set-region-to-isearch-match
      (progn
	(if (and (eq last-command 'aquamacs-repeat-isearch-backward)
		 (not mark-active)) ;; failed error has been shown once (and mark deactivated)
	    (condition-case nil
		(search-backward isearch-string)
	      (error
	       (let (new-point)
		 (save-excursion
		   (end-of-buffer)
		   (condition-case x
		       (progn (search-backward isearch-string)
			      (setq new-point (point)))
		     (error
		      (signal (car x) (cdr x)))))
		 (and new-point (goto-char new-point)))))
	  (deactivate-mark)
	  (if (and (mark) (< (mark) (point)))
	      (goto-char (mark)))
	  (search-backward isearch-string))
	(set-mark (match-end 0)))
    (isearch-repeat 'backward)))

(defun aquamacs-isearch-yank-kill ()
  (interactive)			
  (if (and isearch-string (> (length isearch-string) 0))
      (call-interactively 'clipboard-yank)
    (let ((x-select-enable-clipboard t))
      (call-interactively 'isearch-yank-kill))))


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


(defun aquamacs-use-selection-for-find (beg end)
  (interactive "r")  
  (when mark-active
      (setq isearch-string (buffer-substring-no-properties beg end))
      (setq search-ring (cons (buffer-substring-no-properties beg end) 
			      search-ring))))
 

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
(if (fboundp 'mac-spotlight-search)
    (defun aquamacs-spotlight-lookup ()
      "Search marked word in Spotlight
OS X 10.4 and up only."
      (interactive)
      (let ((word (aquamacs-mouse-get-word)))
	(if word
	    (mac-spotlight-search word)))))

;; (aquamacs-make-mouse-buffer-menu)
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
	  (aquamacs--keymap-from-alist subdivided-menus))
       (aquamacs--keymap-from-alist (mouse-buffer-menu-alist buffers)))))

(defun aquamacs--keymap-from-alist (alist)
  (let ((km (make-sparse-keymap)))
    (mapc (lambda (pair)
	    (define-key km (vector (intern (car pair)))
	      `(menu-item ,(car pair) 
			  ,(if (consp (cdr pair))
			       (aquamacs--keymap-from-alist (cdr pair))
			     (eval
			      (list 'lambda () 
				    '(interactive)
				`(let ((one-buffer-one-frame nil))
				   (switch-to-buffer ,(cdr pair))))))
			  ))) 
	  (sort alist (lambda (a b) (string< (car b)
					     (car a)))))
    km))


;; (aquamacs-update-context-menus t)

(defun aquamacs-get-mouse-major-mode-menu ()
  "Pop up a mode-specific menu of mouse commands.
Defaults to nil if the major mode doesn't define a menu."
  ;; Switch to the window clicked on, because otherwise
  ;; the mode's commands may not make sense.
  (interactive "@e\nP")
  ;; Let the mode update its menus first.
  (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (let* (;; This is where mouse-major-mode-menu-prefix
	 ;; returns the prefix we should use (after menu-bar).
	 ;; It is either nil or (SOME-SYMBOL).
	 (mouse-major-mode-menu-prefix nil)
	 ;; Keymap from which to inherit; may be null.
	 (ancestor (if (> emacs-major-version 22)
		       (mouse-menu-major-mode-map)
		     (mouse-major-mode-menu-1
		      (and (current-local-map)
			   (local-key-binding [menu-bar])))))
	 ;; Make a keymap in which our last command leads to a menu or
	 ;; default to the edit menu.
	 (newmap (if ancestor
		     (make-sparse-keymap (concat mode-name " Mode"))
		   nil)))
    (if ancestor
	;; Make our menu inherit from the desired keymap which we want
	;; to display as the menu now.
	(set-keymap-parent newmap ancestor))
    newmap))


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
    (if (fboundp 'aquamacs-spotlight-lookup)
	(define-key map [spotlight] (cons "Search on this Mac" 
				       'aquamacs-spotlight-lookup)))
    (define-key map [aq-cm-sep3] '(menu-item "--"))
    (define-key map [switch-buffer] nil)
    (define-key map [change-mode] nil)
    (define-key map [mode-menu] nil)
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
  (aquamacs-update-context-menus)
  
  ;; move popup menu a little so mouse pointer is over first entry
  ;; not needed
  ;; ((pos
  ;; 	 (if (not (eq (event-basic-type event) 'mouse-3))
  ;; 	     event
  ;; 	   (list (lispost (- (car (nth 2 (car (cdr event)))) 0)
  ;; 		       (- (cdr (nth 2 (car (cdr event)))) 0))
  ;; 		 (car (car (cdr event)))))))
  
    (popup-menu aquamacs-context-menu-map event prefix))

;; (aquamacs-update-context-menus t)
(defun aquamacs-update-context-menus (&optional force)
  "Update the buffer- and mode-specific items in
`aquamacs-context-menu-map' if frame or buffer has changed.
Update unconditionally if optional argument FORCE is non-nil."
  ;; (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (when (or force
	    (frame-or-buffer-changed-p 'aquamacs-popup-context-menu-buffers-state))
    (let ((mode-menu (aquamacs-get-mouse-major-mode-menu)))
      (if mode-menu
	  ;; TO DO major mode might not work unless we switch buffer
	  (define-key aquamacs-context-menu-map [mode-menu] 
	    `(menu-item ,(aquamacs-pretty-mode-name major-mode) ,mode-menu :visible t))
	(define-key aquamacs-context-menu-map [mode-menu] mouse-buffer-menu
	  '(menu-item nil :visible nil))))

    (define-key aquamacs-context-menu-map [switch-buffer] 
      `(menu-item "Switch to Buffer "   
		  ,(aquamacs-make-mouse-buffer-menu)
		  :help "Show a different buffer in this frame"))
    (define-key aquamacs-context-menu-map [change-mode] 
      `(menu-item "Change Major Mode "   
		  ,menu-bar-change-mode-menu
		  :help "Show a different buffer in this frame"))))

  
(defcustom osx-key-mode-mouse-3-behavior #'aquamacs-popup-context-menu
  "Determine behavior of (down-)mouse-3 in osx-key-mode.
When set to `aquamacs-popup-context-menu' or nil,  down-mouse-3
\(usually: clicking the right mouse button) will bring up a
context menu.  When set to `mouse-save-then-kill', mouse-3 will
extend the region with `mouse-save-then-kill' (traditional Emacs
behavior)."
  :group 'Aquamacs
  :type '(radio 
	  (function-item :tag "Save or kill text between point and mouse"  
			 mouse-save-then-kill) 
	  (function-item :tag "Show context menu" 
			 aquamacs-popup-context-menu)))


(defun osx-key-mode-mouse-3 (event &optional prefix)
  "Run command specified in `osx-key-mode-mouse-3-behavior'."
  (interactive "@e \nP")
  (unless (or (null osx-key-mode-mouse-3-behavior)
	      (eq osx-key-mode-mouse-3-behavior 'aquamacs-popup-context-menu))
    (setq this-command osx-key-mode-mouse-3-behavior) ;; this will set last-command
    ;; mouse-save-then-kill requires last-command:
    (apply osx-key-mode-mouse-3-behavior
	   event prefix))
  )

(defun osx-key-mode-down-mouse-3 (event &optional prefix)
  "Activate context menu, when `osx-key-mode-mouse-3-behavior' is
set to `aquamacs-popup-context-menu' or nil"
  (interactive "@e \nP")
  (if (or (eq osx-key-mode-mouse-3-behavior #'aquamacs-popup-context-menu)
	  (not osx-key-mode-mouse-3-behavior))
      (aquamacs-popup-context-menu event prefix)
    ;; else: pretend this command never happened
    ;; (for the benefit of mouse-save-then-kill)
    (setq this-command last-command
	  last-command nil)))

(defun make-osx-key-low-priority-map (&optional command-key)

  (if command-key
      (setq osxkeys-command-key command-key)
    (if ns-command-modifier
	(setq osxkeys-command-key ns-command-modifier)))
  (let ((map (make-sparse-keymap)))

    (define-key map `[(,osxkeys-command-key meta 49)] 'aquamacs-join-windows) ; 49='1'
    (define-key map `[(,osxkeys-command-key meta 50)] 'aquamacs-split-window-vertically) ; 50='2'

    (define-key map `[(meta q)] 'fill-paragraph-or-region)
    (define-key map `[(meta shift q)] 'unfill-paragraph-or-region)

    (define-key map '[(control meta left)] 'backward-sexp-nomark)
    (define-key map '[(control meta right)] 'forward-sexp-nomark)
    (define-key map '[(control shift meta left)] 'backward-sexp-mark)
    (define-key map '[(control shift meta right)] 'forward-sexp-mark)

    (define-key map `[(,osxkeys-command-key delete)] 'kill-visual-line)
    (define-key map `[(,osxkeys-command-key kp-delete)] 'kill-visual-line)
    (define-key map `[(,osxkeys-command-key backspace)] 'kill-whole-visual-line)
    (define-key map `[(,osxkeys-command-key shift backspace)] 'kill-whole-line)

    (define-key map `[(control a)] 'aquamacs-move-beginning-of-line)
    (define-key map `[(control e)] 'aquamacs-move-end-of-line)
    (define-key map `[(control p)] 'aquamacs-previous-line)
    (define-key map `[(control n)] 'aquamacs-next-line)
    (define-key map `[(meta up)] 'cua-scroll-down)
    (define-key map `[(meta down)] 'cua-scroll-up)
    ;; left / right (for transient-mark-mode)
    ;; could be moved into transient-mark-mode-map?
    (define-key map '[(left)] 'aquamacs-backward-char)
    (define-key map '[(right)] 'aquamacs-forward-char)
    (define-key map `[(,osxkeys-command-key up)] 'beginning-of-buffer)
    (define-key map `[(,osxkeys-command-key down)] 'end-of-buffer)
    (define-key map `[(,osxkeys-command-key prior)] 'beginning-of-buffer)  ; PageUp
    (define-key map `[(,osxkeys-command-key next)] 'end-of-buffer)  ; PageDown
    (define-key map `[(,osxkeys-command-key left)] 'beginning-of-visual-line)
    (define-key map `[(,osxkeys-command-key right)] 'end-of-visual-line)
    (define-key map `[(control left)] 'beginning-of-visual-line)
    (define-key map `[(control right)] 'end-of-visual-line)

    (define-key map `[(control z)] 'ignore) ;; hit by mistake often enough

    map))

(defvar osx-key-low-priority-key-map
  (make-osx-key-low-priority-map)
  "Low-priority keymap for `osx-key-mode'.
These bindings will be added to the global key map when the mode is
turned on. Toggle mode in order to update the global map.")
;; (setq  osx-key-low-priority-key-map (make-osx-key-low-priority-map))


(defun make-osx-key-mode-map (&optional command-key)
  "Create a mode map for OSX key mode. COMMAND-KEY specifies
which key is mapped to command. The value of 
`ns-command-modifier' is the default."
  (if command-key
      (setq osxkeys-command-key command-key)
    (if ns-command-modifier
	(setq osxkeys-command-key ns-command-modifier)))
  (let ((map (make-sparse-keymap)))

    ;; debug log

    (define-key map [mouse-3] 'osx-key-mode-mouse-3) 
    (define-key map [down-mouse-3] 'osx-key-mode-down-mouse-3)
    (define-key map `[(,osxkeys-command-key \?)] 'aquamacs-user-help)
    (define-key map `[(,osxkeys-command-key shift \?)] 'aquamacs-emacs-manual)

    (define-key map `[(,osxkeys-command-key n)] 'new-empty-buffer-other-frame) 
    (define-key map `[(,osxkeys-command-key o)] 'mac-key-open-file) 
    (define-key map `[(,osxkeys-command-key s)] 'mac-key-save-file)
    (define-key map `[(,osxkeys-command-key shift s)] 'mac-key-save-file-as)
    (define-key map `[(,osxkeys-command-key shift o)] 'mac-key-open-file-other-frame) 

    (define-key map `[(,osxkeys-command-key a)] 'mark-whole-buffer)
    (define-key map `[(,osxkeys-command-key v)] 'clipboard-yank) 
    (define-key map `[(,osxkeys-command-key c)] 'clipboard-kill-ring-save)
    (define-key map `[(meta ,osxkeys-command-key c)] 
      'aquamacs-clipboard-kill-ring-save-secondary)
    ;; this because the combination control-space usually activates Spotlight
    (define-key map `[(control ,osxkeys-command-key space)] 'set-mark)
    (define-key map `[(,osxkeys-command-key x)] 'clipboard-kill-region)
    (define-key map `[(meta ,osxkeys-command-key x)] 
      'aquamacs-clipboard-kill-secondary)
    (define-key map `[(,osxkeys-command-key p)] 'aquamacs-print)
    (define-key map `[(,osxkeys-command-key l)] 'goto-line)
    (define-key map `[(,osxkeys-command-key f)] 'aquamacs-isearch-forward)
    (define-key map `[(,osxkeys-command-key g)] 'aquamacs-repeat-isearch)  
    (define-key map `[(,osxkeys-command-key shift g)] 'aquamacs-repeat-isearch-backward)
    (if (fboundp 'ns-do-hide-emacs)
	(define-key map `[(,osxkeys-command-key h)] 'ns-do-hide-emacs))
    (define-key map `[(,osxkeys-command-key e)] 'aquamacs-use-selection-for-find)
    (define-key map `[(,osxkeys-command-key w)] 'close-window)
    (define-key map `[(,osxkeys-command-key m)] 'iconify-or-deiconify-frame) 
    (define-key map `[(,osxkeys-command-key .)] 'keyboard-quit)
    ;; workaround for bug in menu key description
    (define-key map `[(,osxkeys-command-key shift 13)] 'aquamacs-toggle-full-frame)
    (define-key map `[(,osxkeys-command-key shift return)] 'aquamacs-toggle-full-frame)
    (define-key map `[(,osxkeys-command-key escape)] 'keyboard-escape-quit) 
    (define-key map `[(,osxkeys-command-key :)] 'spellchecker-panel-or-ispell) 
    (define-key map `[(,osxkeys-command-key \;)] 'spellcheck-now)
    (define-key map `[(meta ,osxkeys-command-key \;)] 'flyspell-buffer)

    ;; Zoom Zoom!
    (define-key map `[(meta wheel-up)] 'zoom-font)
    (define-key map `[(meta wheel-down)] 'zoom-font-out)
    (define-key map `[(,osxkeys-command-key =)] 'zoom-font) ; comapt. Safari/Firefox
    (define-key map `[(,osxkeys-command-key +)] 'zoom-font)
    (define-key map `[(,osxkeys-command-key -)] 'zoom-font-out)

;; these go away in 1.4
;;    (define-key global-map `[(,osxkeys-command-key {)] 'comment-region-or-line)
;;    (define-key global-map `[(,osxkeys-command-key })] 'uncomment-region-or-line)
   (define-key global-map `[(,osxkeys-command-key meta \')]
     'uncomment-region-or-line)
   
   (define-key global-map `[(,osxkeys-command-key \')]
     'comment-or-uncomment-region-or-line)

    ;; tabbar stuff

    (when (fboundp 'previous-tab-or-buffer)
      (define-key map `[(,osxkeys-command-key meta left)] 'previous-tab-or-buffer)
      (define-key map `[(,osxkeys-command-key {)] 'previous-tab-or-buffer)
      (define-key map `[(,osxkeys-command-key meta 2294)] 'previous-tab-or-buffer) ; 366
      (define-key map `[(,osxkeys-command-key meta right)] 'next-tab-or-buffer)
      (define-key map `[(,osxkeys-command-key })] 'next-tab-or-buffer))
      (define-key map `[(,osxkeys-command-key meta 2276)] 'next-tab-or-buffer) ; 344


    ;; only those keys - C-n and C-p stay Emacs-like
    
    (define-key map '[(control \;)] 'toggle-mac-option-modifier)

    (define-key map `[(control ,osxkeys-command-key q)] 'kill-emacs)
    (define-key map `[(,osxkeys-command-key q)] 'save-buffers-kill-emacs)
    (define-key map (vector (list osxkeys-command-key '\,) ) 'customize)

    (define-key map `[(,osxkeys-command-key z)] 'aquamacs-undo)
    (define-key map `[(,osxkeys-command-key shift z)] 'aquamacs-redo)
    (define-key map `[(,osxkeys-command-key \`)] 'raise-next-frame)
    (define-key map `[(,osxkeys-command-key \~)] 'raise-previous-frame)
    (define-key map `[(,osxkeys-command-key \<)] 'raise-next-frame)
    (define-key map `[(,osxkeys-command-key \>)] 'raise-previous-frame)
    (define-key map `[(,osxkeys-command-key \])] 'other-window)
    (define-key map `[(,osxkeys-command-key \[)] 'other-previous-window)

    (define-key map `[(,osxkeys-command-key t)] 'new-tab)
 
    ;; delete key (needed on 23)
    (define-key map '[(delete)] 'delete-char) 
    (define-key map '[(kp-delete)] 'delete-char) 

    ;; handle transient-mark-mode better
    (define-key map '[(meta delete)] 'kill-word)
    (define-key map '[(meta kp-delete)] 'kill-word)
    (define-key map '[(control delete)] 'kill-word)
    (define-key map '[(control kp-delete)] 'kill-word)
    (define-key map '[(meta backspace)] 'backward-kill-word)
    (define-key map '[(control backspace)] 'backward-kill-word)
    (define-key map '[remap kill-word] 'aquamacs-kill-word)
    (define-key map '[remap backward-kill-word] 'aquamacs-backward-kill-word)

    ;; some modes attempt to override them
    ;; so we'll define these here.
    (define-key map [(home)] 'beginning-of-buffer)
    (define-key map [(end)] 'end-of-buffer)

    (if (fboundp 'mac-font-panel-mode)
	(define-key map `[(,osxkeys-command-key shift t)] 'mac-font-panel-mode))

    ;; we have inhibit-fit-frame set to true... can't do this
    ;; (global-set-key [(control ?x) (control ?-)] 'fit-frame)
    ;; use vector, not [...] in order to not allocate at load-time 
    ;; in pure-space
    (define-key map `[(,osxkeys-command-key control down)] 'enlarge-frame)
    (define-key map `[(,osxkeys-command-key control right)] 'enlarge-frame-horizontally)
    (define-key map `[(,osxkeys-command-key control up)] 'shrink-frame)
    (define-key map `[(,osxkeys-command-key control left)] 'shrink-frame-horizontally)
 
    map))
 

(defvar osx-key-mode-map
  (make-osx-key-mode-map)
  "Keymap for `osx-key-mode'.")
;;  (setq  osx-key-mode-map (make-osx-key-mode-map))


(defun osx-key-mode-command-key-warning ()
  (and osx-key-mode
       (not (eq ns-command-modifier osxkeys-command-key))
       (message (format
"Warning: You have set `ns-command-modifier' to %s in your 
customizations or init file. The Mac-like keyboard shortcuts
provided by `osx-key-mode' won't work with this setting.
The mode uses `osxkeys-command-key' als Command, which is
currently set to %s. You should change one of those two
variables or turn off `osx-key-mode'.
See the description of `osxkeys-command-key'." 
ns-command-modifier osxkeys-command-key))))

(defun aquamacs-install-low-priority-global-key-map (keymap &optional target)
  "Install keys from keymap keymap into the target (or global) map."
  (let ((target (or target (current-global-map)))
	(overwritten (make-sparse-keymap)))
    (map-keymap
   (lambda (key command)
     (let ((old (lookup-key  target `[,key])))
       (if (keymapp command)  ; key is a prefix key
	   (if (keymapp old)
	       ;; recurse
	       (setq old (aquamacs-install-low-priority-global-key-map
			  command old)))
	 (define-key target `[,key] command))
	 ;; also save "nil" entries for unassigned keys
       (define-key overwritten `[,key] old))) 
   keymap)
    overwritten))

;(aquamacs-install-low-priority-global-key-map osx-key-low-priority-key-map)



;; ensure that we remap the right backward-kill-word 
(define-key minibuffer-local-filename-completion-map 
  [remap aquamacs-backward-kill-word] 'backward-kill-filename)
(define-key minibuffer-local-filename-must-match-map 
  [remap aquamacs-backward-kill-word] 'backward-kill-filename)
 

(defvar osx-key--saved-low-priority-map (make-sparse-keymap)
  "Bindings in the global map overwritten when `osx-key-mode' was turned on.")
(defvar osx-key--saved-x-select-enable-clipboard 'unset
  "Value of `x-select-enable-clipboard' when `osx-key-mode' was turned on.")

(define-minor-mode osx-key-mode
  "Toggle Mac Key mode.
With arg, turn Mac Key mode on iff arg is positive.
When Mac Key mode is enabled, Mac-style key bindings are provided.
Setting the `osx-key-mode' variable has limited effect - call
the `osx-key-mode' function to switch mode on or off.
`osx-key-mode-map' and `osx-key-low-priority-key-map' contain the
keymaps used by this mode. They may be modified where necessary."
  :global t
  :group 'osx-key-mode 
  :keymap osx-key-mode-map  

  (setq mac-emulate-three-button-mouse (if osx-key-mode 'control
					   nil))

  ;; use right mouse click as mouse-3
  (setq mac-wheel-button-is-mouse-2 osx-key-mode)

  (let ((nv osx-key--saved-x-select-enable-clipboard)) 
    (setq osx-key--saved-x-select-enable-clipboard
	  (cons (not osx-key-mode)
		x-select-enable-clipboard))
    (setq x-select-enable-clipboard
	  (if (and (consp nv) (eq (car nv) osx-key-mode))
	      (cdr nv)
	    ;; use default (in case of various errors)
	    (if osx-key-mode nil t))))

  (if osx-key-mode
      ;; install low priority map
      (progn
	(setq osx-key--saved-low-priority-map 
	      (aquamacs-install-low-priority-global-key-map
	       osx-key-low-priority-key-map))
	(define-key isearch-mode-map `[(,osxkeys-command-key v)] 'aquamacs-isearch-yank-kill)
	(add-hook 'isearch-mode-end-hook 'aquamacs-set-region-to-search-match))
    ;; restore old map
    (when osx-key--saved-low-priority-map
      (aquamacs-install-low-priority-global-key-map
       osx-key--saved-low-priority-map)
      (setq osx-key--saved-low-priority-map (make-sparse-keymap)))
    (define-key isearch-mode-map `[(,osxkeys-command-key v)] nil)
    (remove-hook 'isearch-mode-end-hook 'aquamacs-set-region-to-search-match))

  (osx-key-mode-command-key-warning))


;; (osx-key-mode 1)
(provide 'osxkeys)