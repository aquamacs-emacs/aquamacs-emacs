;; Mac-default-fontsets

;; specify some default fontsets that should work on the Mac
;; some fonts might not be found - a warning is issues

;; this should eventually be replaced by a complete font selection
;; dialog


;; Author: David Reitter, http://www.reitter-it-media.de/
;;                        http://www.davids-world.com/
;;
;; This file is part of Aquamacs Emacs.
;; Released under the GNU General Public License.


;;; FONT DEFAULTS 

(setq ignore-font-errors nil)
; (setq scalable-fonts-allowed t) ; this is default anyway
 
(condition-case e
    (create-fontset-from-fontset-spec
     "-apple-monaco*-medium-r-normal--9-*-*-*-*-*-fontset-monaco9,
	ascii:-apple-monaco*-medium-r-normal--9-90-75-75-m-90-mac-*,
	latin-iso8859-1:-apple-monaco*-medium-r-normal--9-90-75-75-m-90-mac-*,
	latin-iso8859-9:-apple-monaco*-medium-r-normal--9-90-75-75-m-90-mac-*" nil t)
  (error (print (list "Warning: " e))))


(condition-case e
    (create-fontset-from-fontset-spec
     "-apple-monaco*-medium-r-normal--10-*-*-*-*-*-fontset-monaco10,
	ascii:-apple-monaco*-medium-r-normal--10-100-75-75-m-100-mac-*,
	latin-iso8859-1:-apple-monaco*-medium-r-normal--10-100-75-75-m-100-mac-*,
	latin-iso8859-9:-apple-monaco*-medium-r-normal--10-100-75-75-m-100-mac-*" nil t)
  (error (print (list "Warning: " e))))



(condition-case e
    (create-fontset-from-fontset-spec
     "-apple-monaco*-medium-r-normal--11-*-*-*-*-*-fontset-monaco11,
	ascii:-apple-monaco*-medium-r-normal--11-110-75-75-m-120-mac-*,
	latin-iso8859-1:-apple-monaco*-medium-r-normal--11-110-75-75-m-120-mac-*,
	latin-iso8859-9:-apple-monaco*-medium-r-normal--11-110-75-75-m-120-mac-*,
	utf-8:-apple-monaco*-medium-r-normal--11-110-75-75-m-120-mac-*,
	" nil t)
  (error (print (list "Warning: " e))))
	

(condition-case e
    (create-fontset-from-fontset-spec
     "-apple-monaco*-medium-r-normal--12-*-*-*-*-*-fontset-monaco12,
	ascii:-apple-monaco*-medium-r-normal--12-120-75-75-m-120-mac-*,
	latin-iso8859-1:-apple-monaco*-medium-r-normal--12-120-75-75-m-120-mac-*,
	latin-iso8859-9:-apple-monaco*-medium-r-normal--12-120-75-75-m-120-mac-*,
	utf-8:-apple-monaco*-medium-r-normal--12-120-75-75-m-120-mac-*,
	" nil t)
  (error (print (list "Warning: " e))))
	

(condition-case e
    (create-fontset-from-fontset-spec
     "-apple-monaco*-medium-r-normal--14-*-*-*-*-*-fontset-monaco14,
	ascii:-apple-monaco*-medium-r-normal--14-140-75-75-m-140-mac-*,
	latin-iso8859-1:-apple-monaco*-medium-r-normal--14-140-75-75-m-140-mac-*,
	latin-iso8859-9:-apple-monaco*-medium-r-normal--14-140-75-75-m-140-mac-*" nil t)
  (error (print (list "Warning: " e))))
	

(condition-case e 
    (create-fontset-from-fontset-spec
     "-*-lucida grande*-medium-r-normal--14-*-*-*-*-*-fontset-lucida14,
	latin-iso8859-1:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	latin-iso8859-2:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	latin-iso8859-3:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	latin-iso8859-4:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	latin-iso8859-5:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	latin-iso8859-6:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	latin-iso8859-7:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	latin-iso8859-8:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	latin-iso8859-9:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	latin-iso8859-12:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	latin-iso8859-13:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	latin-iso8859-14:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	latin-iso8859-15:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	latin-iso8859-16:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	mac-roman-lower:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	mac-roman-upper:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	mule-unicode-0100-24ff:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	mule-unicode-2500-33ff:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	mule-unicode-e000-ffff:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	iso10646-1:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*,
	ascii:-*-lucida grande*-medium-r-normal--0-0-0-0-m-0-mac-*" t ignore-font-errors)

  (error (print (list "Warning: " e))))

;; only the following function is able to scale scalable fonts.
;; it only seems to find ASCII fonts, though.
(condition-case e 
(progn
 (create-fontset-from-mac-roman-font "-*-lucida grande*-medium-r-*-*-9-*-*-*-*-*-mac-roman" nil "Lucida Grande Roman 9")
 (create-fontset-from-mac-roman-font "-*-lucida grande*-medium-r-*-*-10-*-*-*-*-*-mac-roman" nil "Lucida Grande Roman 10")
 (create-fontset-from-mac-roman-font "-*-lucida grande*-medium-r-*-*-11-*-*-*-*-*-mac-roman" nil "Lucida Grande Roman 11")
 (create-fontset-from-mac-roman-font "-*-lucida grande*-medium-r-*-*-12-*-*-*-*-*-mac-roman" nil "Lucida Grande Roman 12")
 (create-fontset-from-mac-roman-font "-*-lucida grande*-medium-r-*-*-13-*-*-*-*-*-mac-roman" nil "Lucida Grande Roman 13")
 (create-fontset-from-mac-roman-font "-*-lucida grande*-medium-r-*-*-14-*-*-*-*-*-mac-roman" nil "Lucida Grande Roman 14")
 (create-fontset-from-mac-roman-font "-*-lucida grande*-medium-r-*-*-16-*-*-*-*-*-mac-roman" nil "Lucida Grande Roman 16")
 (create-fontset-from-mac-roman-font "-*-lucida grande*-medium-r-*-*-18-*-*-*-*-*-mac-roman" nil "Lucida Grande Roman 18")
)
	  (error (print (list "Warning: " e))))

	
;; some other default fonts


(condition-case e 
    (create-fontset-from-mac-roman-font "-apple-lucida sans typewrite*-medium-r-normal--10-*-*-*-*-*-mac-*" 
					nil "Lucida Sans Typewrite Roman 10")
    (create-fontset-from-mac-roman-font "-apple-lucida sans typewrite*-medium-r-normal--12-*-*-*-*-*-mac-*" 
					nil "Lucida Sans Typewrite Roman 12")
    (create-fontset-from-mac-roman-font "-apple-lucida sans typewrite*-medium-r-normal--14-*-*-*-*-*-mac-*" 
					nil "Lucida Sans Typewrite Roman 14")
    (create-fontset-from-mac-roman-font "-apple-lucida sans typewrite*-medium-r-normal--9-*-*-*-*-*-mac-*" 
					nil "Lucida Sans Typewrite Roman 9")
  (error (print (list "Warning: " e))))
	  

;; leaving this one in for now
(condition-case e 
    (create-fontset-from-fontset-spec
     "-*-lucida sans type*-medium-r-*-*-11-*-*-*-*-*-fontset-11pt_lucida_sans_typewriter,
	latin-iso8859-1:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	latin-iso8859-2:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	latin-iso8859-3:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	latin-iso8859-4:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	latin-iso8859-5:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	latin-iso8859-6:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	latin-iso8859-7:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	latin-iso8859-8:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	latin-iso8859-9:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	latin-iso8859-12:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	latin-iso8859-13:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	latin-iso8859-14:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	latin-iso8859-15:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	latin-iso8859-16:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	mac-roman-lower:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	mac-roman-upper:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	mule-unicode-0100-24ff:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	mule-unicode-2500-33ff:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	mule-unicode-e000-ffff:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	iso10646-1:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*,
	ascii:-*-lucida sans type*-medium-r-normal--11-110-75-75-m-110-mac-*" t ignore-font-errors)

  (error (print (list "Warning: " e))))


(condition-case e 
    (create-fontset-from-fontset-spec
     "-*-lucida console*-medium-r-*-*-11-*-*-*-*-*-fontset-11pt_lucida_console,
	latin-iso8859-1:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-2:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-3:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-4:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-5:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-6:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-7:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-8:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-9:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-12:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-13:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-14:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-15:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-16:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	mac-roman-lower:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	mac-roman-upper:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	mule-unicode-0100-24ff:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	mule-unicode-2500-33ff:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	mule-unicode-e000-ffff:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	iso10646-1:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	ascii:-*-lucida console*-medium-r-normal--11-110-75-75-m-110-mac-roman" t ignore-font-errors)

  (error (print (list "Warning: " e))))

(condition-case e 
    (create-fontset-from-fontset-spec
     "-*-lucida sans type*-medium-r-*-*-11-*-*-*-*-*-fontset-11pt_bitstream_courier,
	latin-iso8859-1:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-2:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-3:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-4:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-5:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-6:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-7:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-8:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-9:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-12:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-13:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-14:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-15:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	latin-iso8859-16:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	mac-roman-lower:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	mac-roman-upper:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	mule-unicode-0100-24ff:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	mule-unicode-2500-33ff:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	mule-unicode-e000-ffff:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	iso10646-1:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman,
	ascii:-*-courier*-medium-r-normal--11-110-75-75-m-110-mac-roman" t ignore-font-errors)
  (error (print (list "Warning: " e))))

(condition-case e 
(create-fontset-from-fontset-spec
        "-*-bitstream vera sans mono-medium-r-*-*-0-*-*-*-*-*-fontset-12pt_vera_sans_mono,
             latin-iso8859-1:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
             latin-iso8859-2:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
             latin-iso8859-3:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
             latin-iso8859-4:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
          cyrillic-iso8859-5:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
            arabic-iso8859-6:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
             greek-iso8859-7:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
            hebrew-iso8859-8:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
             latin-iso8859-9:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
            latin-iso8859-11:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
            latin-iso8859-13:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
            latin-iso8859-14:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
            latin-iso8859-15:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
            latin-iso8859-16:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
             mac-roman-lower:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
             mac-roman-upper:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
      mule-unicode-0100-24ff:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
      mule-unicode-2500-33ff:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
      mule-unicode-e000-ffff:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
                  iso10646-1:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman,
                       ascii:-*-bitstream vera sans mono-medium-r-normal--*-*-75-75-m-120-mac-roman" t ignore-font-errors )
 (error (print (list "Warning: " e))))


; if you'd like to increase the font size, you need to check if the fonts
; exist. copy and paste this into the scratch buffer, then execute
; by going to its end, then C-x C-e


; (print-elements-of-list (x-list-fonts "*lucida grande*")) 

 


;; interesting thread about fonts:
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2004-01/msg00398.html
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2003-03/msg00436.html



(defun print-elements-of-list (list)
       "Print each element of LIST on a line of its own."
       (while list
         (insert (car list)) (insert "\n")
         (setq list (cdr list))))
         
         

;; delete all the other fonts from the menu
;; - because they're not present on the Mac

;; default gets put in autom.
(setq x-fixed-font-alist
      '("--- Font menu" ("Misc" () ))) 


(provide 'mac-default-fontsets)