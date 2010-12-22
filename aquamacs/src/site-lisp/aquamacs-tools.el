;; Aquamacs tools
;; some helper functions for Aquamacs
 
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/

;; Aquamacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Aquamacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;; Copyright (C) 2005, 2007, 2009 David Reitter


; remove an element from an associative list (alist) 
;; (defun remove-alist-name (name alist)
;;   "Removes element whose car is NAME from ALIST."
;;   (cond ((equal name (car (car alist)))	  ; found name
;;          (cdr alist))
;;         ((null alist)		; end of list (termination cond)
;;          nil)
;;         (t
;;          (cons (car alist)	; first of alist plus rest w/ recursion
;;                (remove-alist-name name (cdr alist))))))

;; this is assq
;; (defun get-alist-value-for-name (name alist)
;;   "Returns value of element whose car is NAME from ALIST. nil if not found"
;;   (cond ((equal name (car (car alist)))	  ; found name
;;          (cdr (car alist)))
;;         ((null alist)		; end of list (termination cond)
;;          nil)
;;         (t
;;           	; first of alist plus rest w/ recursion
;;           (get-alist-value-for-name name (cdr alist)))))


(defun running-on-a-mac-p ()
  (memq initial-window-system '(mac ns)))


(defun aquamacs-ask-for-confirmation (text long &optional yes-button no-button sheet no-cancel)
    (let ((f (window-frame (minibuffer-window))))
      (make-frame-visible f)
      (raise-frame f)			; make sure frame is visible
      (if (or  
	   (and last-nonmenu-event 
		(not (consp last-nonmenu-event))) 
	   ;;(not (eq (car-safe last-nonmenu-event)  
	   ;;	  'mac-apple-event)))
	   (not use-dialog-box)
	   (not window-system))
	  (progn
	    ;; make sure the frame's minibuffer is actually visible
	    ;; because minibuffer-setup-hook is not executed.
	    (and (fboundp 'smart-move-minibuffer-inside-screen)
		 smart-frame-positioning-mode
		 (smart-move-minibuffer-inside-screen f))
	    (let ((text (if (string-match "\\(.*\\)\n" text)
			    (match-string 1 text)
			  text)))
	      (if (and long (not aquamacs-quick-yes-or-no-prompt))
		  (old-yes-or-no-p text)
		(old-y-or-n-p text))))
	(let ((ret (x-popup-dialog (or sheet (if (mouse-event-p last-command-event) last-command-event)
				        `(mouse-1      (,(selected-window) 100 (0 . 50) -1)))
				    (list text
					  `((,(or yes-button "Yes") . ?\r) . t) ; use \r instead of y until we have multi-keyEquivs
					  (if no-cancel 'no-cancel 'cancel)
					  `((,(or no-button "No") . ?n) . nil)))))
	  (if (eq ret 'cancel)
	      (keyboard-quit))
	  ret))))


(defun filter-list (lst elements)
"Returns LST sans ELEMENTS.
Creates a new list where all elements in ELEMENTS from LST
are removed. Comparison is done with `eq'."

(if (null lst) 
    nil
  (if (member (car lst) elements)
      (filter-list (cdr lst) elements)
    (cons (car lst) (filter-list (cdr lst) elements)))))

(defun assq-set-all (source dest-sym)
  "Writes all values from alist SOURCE into alist DEST-SYM,
overwriting any previous associations in DEST"
  (mapc (lambda (x)
	  (set dest-sym (assq-delete-all (car x) (eval dest-sym))))
	source)
  (set dest-sym (append source (eval dest-sym))))

; (setq test '((a . 1) (b . 2)))
; (assq-set-all '((b . 5) (c . 6)) 'test)
      


; (assq-subtract '((asd . 3) (wqe . 5)) '((wqq . 3) (wqe . 5)))
; (assq-subtract '((asd . 3) (wqe . 5)) '((wqq . 3) (wqe . 2)))
; (assq-subtract '((asd . 3) (wqe . 5)) '((wqq . 3) (wqe . 2)) t)
(defun assq-subtract (a b &optional ignore-values)
  "Subtracts alist B from A. Order of elements is NOT preserved.
If IGNORE-VALUES is non-nil, alist elements with differing cdrs (values)
are still subtracted."
  
  (let ((ret))
    (mapc (lambda (x)
	    (let ((p (assq (car x) b)))
	      (unless (and p (or ignore-values (eq (cdr p) (cdr x))))
		(setq ret (cons x ret)))))
	  a)
    ret))

(defun assq-set (key val alist)
  "Sets value associated with KEY to VAL in ALIST.
ALIST must be a symbol giving the variable name.
Comparison of keys is done with `eq'.
New key-value pair will be in car of ALIST."
  (set alist (cons (cons key val)
		   (assq-delete-all key (eval alist)))))

(defun assq-set-equal (key val alist)
  "Sets value associated with the string KEY to VAL in ALIST.
Comparison of keys is done with `equal'.
ALIST must be a symbol giving the variable name.
New key-value pair will be in car of ALIST."
  (set alist (cons (cons key val)
		   (assq-delete-all-equal key (eval alist)))))

(defun assq-string-equal (key alist)
  
  (loop for element in alist 
        if (string-equal (car element) key)
	return element))


;; (setq asd (list 1 2 3 4 5))
;; (aq-replace-in-list asd 1 'a)
;; asd
(defun aq-replace-in-list (list from to)
  (if (eq (car-safe list) from)
      (setcar list to))
  (if (cdr-safe list)
      (aq-replace-in-list (cdr-safe list) from to)))

 
(defun assq-delete-all-equal (key alist)
  "Delete from ALIST all elements whose car is `equal' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist))
	      (equal (car (car alist)) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
	       (equal (car (car tail-cdr)) key))
	  (setcdr tail (cdr tail-cdr))
	(setq tail tail-cdr))))
  alist)


(defun aq-list-contains (list element)
  "Return non-nil if the LIST contains ELEMENT. Aquamacs only.
Comparison is done with `eq'."
  (let (first result)
    (while list
      (if (not (eq (car-safe list) element))
	  (setq list (cdr-safe list))
	(setq list nil)
	(setq result t))
      )
    result))
;; (aq-list-contains (list 1 2 3 4 5 'a 'b nil 'x) 1)

(defun aq-list-contains-equal (list element)
  "Return non-nil if the LIST contains ELEMENT. Aquamacs only.
Comparison is done with `equal'."
  (let (first result)
    (while list
      (if (not (equal (car-safe list) element))
	  (setq list (cdr-safe list))
	(setq list nil)
	(setq result t))
      )
    result))


(defun aq-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun fontset-exist-p (font)
(condition-case nil
    (fontset-info font)
  (error nil))
)

;; this needs to be replaced by functions defined earlier
; recursion is not so good in elisp anyways
(defun filter-fonts (list)
 "Filters the font list LIST to contain only existing fontsets.
Each element of LIST has to be of the form (symbol . fontset)."
 (mapcar
  (lambda (p)
    (mapcar
     (lambda (e)
       (if (and (consp e)
		(eq (car e) 'font)
		(not (fontset-exist-p (cdr e)))
		)
	   '(font . "fontset-standard")
	 e)) 
     p))
  list))

  

 

(defun get-bufname (buf)
   (if (eq (type-of buf) 'string)
		    buf
		  (buffer-name buf))
	
)
 
(defun get-bufobj (buf)
   (if (eq (type-of buf) 'string)
		   (get-buffer buf)
		  buf)
	
)

(defun find-all-windows-internal (buffer &optional onlyvis)
  "Find all windows that display a buffer." 
  (let ((windows nil))
    (walk-windows (lambda (wind)
                     
		     (if (eq (window-buffer wind) buffer) 
			 (push wind windows))) t (if onlyvis 'visible t))
    windows 
    )
)
; (find-all-frames-internal (current-buffer))
(defun find-all-frames-internal (buffer &optional onlyvis)
  (let ((frames nil)) 
    (walk-windows (lambda (wind)
		  
                     (if (eq (window-buffer wind) buffer)
			 (let ((frm (window-frame wind)))
			    
			   (unless (memq frm frames)
			     (push frm frames)))))
                  nil (if onlyvis 'visible t))
    frames))






(defgroup Aquamacs-is-more-than-Emacs nil
  "All defaults in Aquamacs that are different from GNU Emacs.
This customization group contains every default for customization
variables that is changed in Aquamacs compared to GNU Emacs 22 or
an additionally included package. 
Note that non-customization variables as well as code may be 
changed or advised in Aquamacs (compared to GNU Emacs), so reverting
all of these defaults to their GNU Emacs value will not give you
a GNU Emacs. To achieve that, use a self-compiled binary of 
Carbon Emacs instead of Aquamacs."
:group 'Aquamacs)

(setq  messages-buffer-max-lines 500)

(defun aquamacs-set-defaults (list)
  "Set a new default for a customization option in Aquamacs.
Add the value to the customization group `Aquamacs-is-more-than-Emacs'."

  (mapc (lambda (elt)
	  (custom-load-symbol (car elt))
	  (let* ((symbol (car elt))
		 ;; we're accessing the doc property here so
		 ;; if the symbol is an autoload symbol,
		 ;; it'll get loaded now before setting its defaults
		 ;; (e.g. standard-value), which would otherwise be
		 ;; overwritten.
		 (old-doc 
		  (condition-case nil
		      (documentation-property 
		       symbol 
		       'variable-documentation)
		    (error "")))
		(value (car (cdr elt)))
		(s-value (get symbol 'standard-value)))
	    (set symbol value)
	    (set-default symbol value) ;; new in post-0.9.5
 
	    ;; make sure that user customizations get 
	    ;; saved to customizations.el (.emacs)
	    ;; and that this appears as the new default.

	    (put symbol 'standard-value `((quote  ,(copy-tree (eval symbol)))))
	    ;; since the standard-value changed, put it in the
	    ;; group

	    (unless (or (eq s-value (get symbol 'standard-value))
			(get symbol 'aquamacs-original-default))
	      (put symbol 'aquamacs-original-default
		   s-value)
	      (if old-doc ;; in some cases the documentation
		  ;; might not be loaded. Can we load it somehow?
		  ;; either way, the "if" is a workaround.
		  (put symbol 'variable-documentation
		       (concat
			old-doc
			(format "

The original default (in GNU Emacs or in the package) was:
%s" 
				s-value))))
	      (custom-add-to-group 'Aquamacs-is-more-than-Emacs 
				   symbol 'custom-variable))))
	list))

; (aquamacs-setup)

(defun url-encode-string (string &optional coding)
  "Encode STRING by url-encoding.
Optional CODING is used for encoding coding-system."
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((eq ch ?\n)		; newline
	      "%0D%0A")
	     ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch))
	      (char-to-string ch))	; printable
	     ((char-equal ch ?\x20)	; space
	      "%20")
	     (t
	      (format "%%%02x" ch))))	; escape
	  ;; Coerce a string to a list of chars.
	  (append (encode-coding-string (or string "")
					(or coding
					    file-name-coding-system))
		  nil))))



(defun load-post-sitestart-files ()
  "Load the Aquamacs plugins from site-start directories."
  (let (loaded)
    (mapcar 
     (lambda (p) (unless (file-exists-p (concat p "/.ignore"))
		   (let ((infod (concat p "/info"))
			 (file (expand-file-name (concat p "/site-start") "~/")))
		     
		     (unless (member file loaded)
		       (if (file-directory-p infod)
			   (add-to-list 'Info-default-directory-list infod))
		       (load file 'noerror)
		       (setq loaded (cons file loaded))))))
     load-path)
    t)) 
 ; (load-post-sitestart-files)

(defun load-pre-sitestart-files ()
  "Load the pre-start Aquamacs plugins from site-prestart directories."
  (let (loaded)
    (mapcar 
     (lambda (p) (unless (file-exists-p (concat p "/.ignore"))
		   (let ((infod (concat p "/info"))
			 (file (expand-file-name (concat p "/site-prestart") "~/")))
		     (unless (member file loaded)
		       (if (file-directory-p infod)
			   (add-to-list 'Info-default-directory-list infod))
		       (load file 'noerror)
		       (setq loaded (cons file loaded))))))
     load-path)
    t))
; (load-pre-sitestart-files)


(defun aq-current-milliseconds ()
  (let ((ti (cdr (current-time)))
	
	)
    (+ (* 1000 (- (car ti) (car (cdr aq-timer)))) 
       (/ (- (car  (cdr ti))
	  (car (cdr (cdr aq-timer)))
	  ) 1000))))

(defun aq-start-timer ()
  (setq aq-timer (current-time))
)
;(aq-start-timer)
(defun aq-print-timer ()
  (message  (format "%d" (aq-current-milliseconds))  ))


(defun aquamacs-pretty-mode-name (mode)
  (capitalize 
   (replace-regexp-in-string "-mode" "" (symbol-name mode))))

;; apple command character is unicode x2318  
;;  (aq-describe-modifier 'hyper)
(defun aq-describe-modifier (mod)
  ;; translate modifier
  (if (eq mod 'ctrl)
      (setq mod 'control))
  (or
  (cond
   ((and (boundp 'mac-command-modifier) (eq mac-command-modifier mod))
    (string (decode-char 'ucs #X2318)))
   ((and (boundp 'mac-option-modifier) (eq (or mac-option-modifier 'alt)
					   mod))
    (string (decode-char 'ucs #X2325)))
   ((and (boundp 'mac-control-modifier) (eq (or mac-control-modifier 'control) 
					    mod))
    (string (decode-char 'ucs #X2303)))
   ((eq mod 'shift)
    (string (decode-char 'ucs #X21E7)))
   ((and (boundp 'mac-function-modifier) (eq mac-function-modifier mod))
    "Fn ")
   )
 ;; (progn (print mod) nil)
     (signal 'search-failed nil)
   ))

(defvar apple-char (string (decode-char 'ucs #X2318)))

;; The following is a big hack. The mac port can't currently cope 
;; with putting the command key combos in the menu, for various 
;; reasons (1. they are just secondary alternatives, 2. command is defined
;; as 'alt' and only known as such)

; redefine New
; (define-key menu-bar-edit-menu [mark-whole-buffer] (cdr (assq 'mark-whole-buffer (key-binding [menu-bar edit]))))


(defun get-window-for-other-buffer (&optional dont-make-frame buffer)
  "Find a suitable window for other buffers.
Preferably the selected one.
If a frame is created for the other buffer,
show BUFFER in that frame."
  (let ((sel-win (selected-window))) ; search all visible&iconified frames
    (unless
	(and sel-win
	     (window-live-p sel-win)
	     (eq t (frame-visible-p (window-frame sel-win)))
	     (not (special-display-p
		   (or (buffer-name (window-buffer sel-win)) ""))))
      ;; search visible frames (but not dedicated ones)
      (setq sel-win (get-largest-window 'visible nil)))
    (unless
	(and sel-win
	     (window-live-p sel-win)
	     (eq t (frame-visible-p (window-frame sel-win)))
	     (not (special-display-p
		   (or (buffer-name (window-buffer sel-win)) ""))))
      (unless dont-make-frame
	  (setq sel-win (frame-first-window
			 (with-current-buffer (or buffer (current-buffer))
			   ;; make sure we're not creating some "special" frame
			   (make-frame))))))
    (if sel-win
	(unless (eq t (frame-visible-p (window-frame sel-win)))
	  (make-frame-visible (window-frame sel-win))))
    sel-win))

;; New documents
(defun new-empty-buffer-other-frame (&optional mode)
  "Opens a new frame containing an empty buffer."
  (interactive)
  (new-empty-buffer t mode))

(defcustom aquamacs-default-major-mode 'text-mode
  "Major mode in effect when new empty buffers are created.
Specifies the major mode to be used for `new-empty-buffer' 
and `new-empty-buffer-other-frame'."
  :group 'Aquamacs)

(defun new-empty-buffer  (&optional other-frame mode)
  "Visits an empty buffer.
The major mode is set to MODE, or, if that is nil,
the value of `aquamacs-default-major-mode'."
  (interactive)			
  (let ((buf (generate-new-buffer (mac-new-buffer-name "untitled"))))
    ;; setting mode is done before showing the new frame
    ;; because otherwise, we get a nasty animation effect
    (save-excursion
      (set-buffer buf)
      (funcall (or mode aquamacs-default-major-mode (default-value 'major-mode) 'ignore)))
    (if other-frame
	(switch-to-buffer-other-frame buf)
      (let ((one-buffer-one-frame-force one-buffer-one-frame-mode))
	;; change window in case its unsuitable (dedicated or special display)
	(select-window (get-window-for-other-buffer))
	;; force new frame
	(switch-to-buffer buf)
	(select-frame-set-input-focus (window-frame (selected-window)))))
    (setq buffer-offer-save t)
    (put 'buffer-offer-save 'permanent-local t)
    (set-buffer-modified-p nil)))

(defalias  'new-frame-with-new-scratch 'new-empty-buffer)

;; auto save purging

(defun purge-session-and-auto-save-files (&optional days)
  "Deletes old auto-save files and session files.
If given, DAYS indicates the number of days to keep such files.
Otherwise, a sensible default is assumed.
Files may be moved to the trash or deleted.

Aquamacs only.
"
  (interactive)

  (let* ((days (or days 31))
	 (count1
	  (aquamacs-purge-directory (file-name-directory auto-save-list-file-prefix)
			   (concat "\\`" (regexp-quote
					  (file-name-nondirectory
					   auto-save-list-file-prefix)))
			   days))
	 (count2
	  (aquamacs-purge-directory (file-name-directory aquamacs-autosave-directory)
			   ".*"
			   days)))
    (if (called-interactively-p) 
	(message "%s Session and %s Auto save files older than %s days purged." count1 count2 days))))

(defun aquamacs-purge-directory (directory regexp days)
  "Delete old files from directory"
  (condition-case nil
      (let* ((count 0)
	     (cutoff-time (- (car (current-time)) (/ (* days 24) 18)))) ; that's about a week
	(mapc
	 (lambda (file)
	   (when (and (< (car (nth 5 (file-attributes file)))
			 cutoff-time)
		      (not (file-directory-p file)))
	     (move-file-to-trash file)
	     (setq count (1+ count))))
	 (directory-files (expand-file-name directory) t
			  regexp t))
	count)
    (error 0)))
   

 

(provide 'aquamacs-tools)

