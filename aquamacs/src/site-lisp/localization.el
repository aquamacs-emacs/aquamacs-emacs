;; Aquamacs Localization

(setq mbuh menu-bar-update-hook)
(setq  menu-bar-update-hook mbuh)
; (menu-bar-update-buffers)

(defvar localization-log-strings t)
(defvar localization-target-language nil);; "fr")

(defvar localization-directory 
  (make-hash-table :weakness nil :test 'equal :size 2000))
;
;(setq localization-directory  (make-hash-table :weakness nil :test 'equal :size 2000))

(defun localization-extract-strings (struct)
(let ((localization--string-acc)
      (localization--struc-nesting 0))
  
  (localization-extract-strings-1 struct)

localization--string-acc))
 
(defun localization-extract-strings-1 (struct)
  "Extends `localization--string-acc' with strings found in STRUCT."
  (let ((localization--struc-nesting (1+ localization--struc-nesting)))
    
    (if (stringp struct)
	(add-to-list 'localization--string-acc struct)
      ;else
      (if (and 
       (< localization--struc-nesting 38)
       struct (listp struct))
  (mapc (lambda (elem)
	  (or (and (listp elem)
		   (localization-extract-strings-1 elem))
	      (and (stringp elem)
		   (add-to-list 'localization--string-acc
				elem))))
	(if (consp struct) (list (car struct) (cdr struct))
	  struct))))))

;; (localization--translate-strings '(("hello" . "world") ("dave")))
(defun localization--translate-strings (structure)
  "Translate the strings in the structure STRUCTURE.
Returns the translated structure."
    (mapc (lambda (str)
	      (let ((val (gethash str localization-directory)))
       (if val
	   ;; present in hash
	   (progn
	     ;; translate?
	     (if localization-target-language
		 (let ((tl
			(cdr (assq localization-target-language
			    val))))
		   (and tl
			;; replace the stuff
			(setcdr structure 
				(cons
				 (cdr tl)
				 (cdr-safe (cdr-safe structure))))))))
	 ;; not present 
	 (if localization-log-strings
	     (puthash str nil localization-directory)))))
	    (localization-extract-strings structure))
    structure)

(defadvice define-key  (before localize-definition  activate)
; (keymap key def)
  (localization--translate-strings def))
(defadvice format  (before localize-string  activate)
 (ad-set-arg 0 (localization--translate-strings string)))


;(format "Change the major mode of the current buffer to `%s'." "python-mode")
; (localization-extract-strings "Change the major mode of the current buffer to `%s'.")

(defun localization-strings ()
  (let ((l))
  (maphash (lambda (elem val) (add-to-list 'l elem)) localization-directory)
  (sort l 'string<)))
; (print (localization-strings))

; (puthash "david" "x" 
; (setq lang "fr")
; (localization-output-source-file "~/Temp/loc.el" "fr") 
(defun localization-output-source-file (file lang)
  (interactive "FTo file: 
SLanguage Code: ")
 (let ((stringlist nil)
       (localization-log-strings nil))
    (maphash (lambda (elem val) 
	       (add-to-list 'stringlist (copy-sequence (cons elem
				     (assq lang val)))))
	     localization-directory)
    
    ;; sort only a copy
    
    ;; (sort stringlist (lambda (a b) (string< (car a) (car b))))
    (message "%d" (length stringlist))


    (with-temp-file file
      (delete-region (point-min) (point-max))
      (insert (format ";; %s\n" (time-stamp-string)))
      (insert (format ";; Aquamacs Emacs localization file
;;Created for Aquamacs version %s\n\n\n;; Translator:\n\n\n\n\n"
		      aquamacs-version))
      (insert "(localize-define-language 'fr \"French\" \n")
      (mapc
       (lambda (x) (insert (format "( \t%S . \t\t%S )\n" (car x) (cdr x))))
       stringlist)
      (insert ")\n")
      )
    (message "Wrote %s" file)))

(require 'time-stamp)