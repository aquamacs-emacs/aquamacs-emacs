;; Aquamacs tools
;; some helper functions for Aquamacs
 
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-tools.el,v 1.35 2008/08/13 09:39:07 davidswelt Exp $

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
 
;; Copyright (C) 2005, 2007 David Reitter


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
  (memq window-system '(mac ns)))


(defun aquamacs-ask-for-confirmation (text long)
    (let ((f (window-frame (minibuffer-window))))
      (raise-frame f)			; make sure frame is visible
      (if (or  
	   (and last-nonmenu-event 
		(not (consp last-nonmenu-event))) 
	   ;;(not (eq (car-safe last-nonmenu-event)  
	   ;;	  'mac-apple-event)))
	   (not use-dialog-box)
	   (not (fboundp 'mac-dialog-y-or-n-p))
	   (not window-system))
	  (progn
	    ;; make sure the frame's minibuffer is actually visible
	    ;; this should be done automatically by smart-frame-positioning-mode
	    ;; (if (fboundp 'smart-move-minibuffer-inside-screen)
	    ;;	(smart-move-minibuffer-inside-screen f))
	    (if (and long (not aquamacs-quick-yes-or-no-prompt))
		(old-yes-or-no-p text)
	      (old-y-or-n-p text)))
	(let ((ret (mac-dialog-y-or-n-p text "" t)))
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
Comparison of keys is done with `eq'."
  (set alist (assq-delete-all key (eval alist)))
  (add-to-list alist (cons key  val))) 

(defun assq-set-equal (key val alist)
"Sets value associated with the string KEY to VAL in ALIST.
Comparison of keys is done with `equal'."

  (set alist (assq-delete-all-equal key (eval alist)))
  (add-to-list alist (cons key  val))) 

(defun assq-string-equal (key alist)
  
  (loop for element in alist 
        if (string-equal (car element) key)
	return element
	) 
  )


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
		 (old-doc (documentation-property symbol 
						  'variable-documentation))
		(value (car (cdr elt)))
		(s-value (get symbol 'standard-value)))
	    (set symbol value)
	    (set-default symbol value) ;; new in post-0.9.5
 
	    ;; make sure that user customizations get 
	    ;; saved to customizations.el (.emacs)
	    ;; and that this appears as the new default.

	    (put symbol 'standard-value `((quote  ,(eval symbol))))
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
				   symbol 'custom-variable))




	    ))
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

;; Aquamacs Unit Tests

(defvar aquamacs-require-list nil)
(defun aquamacs-require (arg)
  (setq aquamacs-require-list (cons arg aquamacs-require-list))
)
(defun aquamacs-run-unit-tests ()
  (message "Aquamacs Unit Tests...")
  (mapc
   (lambda (expr)
     (if (eval expr)
	 (message (format "%s passed." expr))
       (message (format "%s failed." expr))))
   aquamacs-require-list)
  (message "... done")
)
 


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
(aq-start-timer)
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



(defun aq-resolve-remapped (list)
  (flatten  
  (mapcar
     (lambda (k)
       (cond 
	( (and 
	   (vectorp k)
	   (eq (aref k 0) 'remap)
	   )
	  (where-is-internal (aref k 1) nil nil t t))
	((and 
	   (vectorp k)
	   (eq (aref k 0) 'menu-bar)
	   )
	 nil
	 )
	(t k))
	)
     list)))


; (aq-find-good-key 'aquamacs-toggle-full-frame)

(defun aq-find-best-key (list)
  (or (if (not list)
	"")
    (key-description (car list))
    (aq-find-best-key (cdr list)))
)
;; 
(defun aq-find-good-key (symbol)
  (aq-find-best-key
   (aq-resolve-remapped
    (or
     (where-is-internal 
      symbol
      (list  osx-key-mode-map   ) 
      )
     (where-is-internal 
      symbol
      (list    global-map ) 
      )
     (where-is-internal 
      symbol
      nil 
      nil t t)))))

(defun aq-shortcut (text symbol &rest more-args)
  ;; symbol can be nil in some circumstances 
  ;; (e.g. in tool-bar-map from early initialization)
  (if (and symbol (if (boundp 'osx-key-mode) osx-key-mode nil))
      (apply (function format) 
	     (append (list (concat text "%s")) more-args 
		     (list (aq-binding symbol))))
    ;; not osx-key-mode
    (apply (function format) text more-args)))
	  
; (aq-binding 'make-frame-command)
(defun aq-binding (symbol)
   (condition-case err
  (let* ((case-fold-search nil)
	 (s (aq-find-good-key symbol))
	 (s (replace-regexp-in-string 
	     "S-." (lambda (txt) 
		     (concat (aq-describe-modifier 'shift) "-" 
			     (downcase (substring txt 2))))
	     s)))

   (replace-regexp-in-string 
    "[-<>]" ""
    (replace-regexp-in-string 
     "-\\([a-z]\\)" 'upcase
			  
     (replace-regexp-in-string 
      "C-" (lambda (txt) 
	     (concat (aq-describe-modifier 'ctrl) 
		     "-"))
      (replace-regexp-in-string 
       "H-" (lambda (txt) 
	      (concat (aq-describe-modifier 'hyper)
		      "-"))
       (replace-regexp-in-string 
	"A-" (lambda (txt) 
	       (concat (aq-describe-modifier 'alt)
		       "-"))
	(replace-regexp-in-string 
	 "M-" (lambda (txt) 
		(concat (aq-describe-modifier 'meta)
			"-"))

	 (replace-regexp-in-string 
	  "-\\([A-Z]\\)" 
	  (lambda (txt) 
	    (concat 
	     (aq-describe-modifier 'shift) txt))
	  s
	  nil nil 1 ;; replace sub-exp
	  ))))))))
  (error "")))

(provide 'aquamacs-tools)

