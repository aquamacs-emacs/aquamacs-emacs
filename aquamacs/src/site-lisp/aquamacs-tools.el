;; Aquamacs tools
;; some helper functions for Aquamacs
 
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-tools.el,v 1.11 2005/09/19 19:03:17 davidswelt Exp $

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

(defun assq-set (key val alist)
  (set alist (assq-delete-all key (eval alist)))
  (add-to-list alist (cons key  val))
) 

(defun assq-set-equal (key val alist)
  (set alist (assq-delete-all-equal key (eval alist)))
  (add-to-list alist (cons key  val))
) 

(defun assq-string-equal (key alist)
  
  (loop for element in alist 
        if (string-equal (car element) key)
	return element
	) 
  )




 
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
	   '(font . "fontset-mac")
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




(defun aquamacs-set-defaults (list)
  "Set a new default for a customization option in Aquamacs."

  (mapc (lambda (elt)
	  (let ((symbol (car elt))
		(value (car (cdr elt))))
	    (set symbol value)
	    (set-default symbol value) ;; new in post-0.9.5
 
	    ;; make sure that user customizations get 
	    ;; saved to customizations.el (.emacs)
	    ;; and that this appears as the new default.

	    (put symbol 'standard-value `((quote  ,(eval symbol))))
	    )
	 

	  )
	list
	)
  )


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
  (mapcar 
    (lambda (p) (load (concat p "/site-start") 'noerror))
    load-path
    )
  t
)
 

(defun load-pre-sitestart-files ()
  (mapcar 
    (lambda (p) (load (concat p "/site-prestart") 'noerror))
    load-path
    )
  t
)

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


(provide 'aquamacs-tools)

