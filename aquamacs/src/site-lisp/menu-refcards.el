;; insert menu showing reference cards

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
 
;; Copyright (C) 2005, 2006, 2007, 2008, 2009 David Reitter



(defun aquamacs--refcard-source-update ()
  "Insert code for current reference card menu."
  (interactive)
  (let ((cards))
       (with-temp-buffer
	 (cd (format "%setc/refcards" (mac-resources-path)))
	 (shell-command "grep '^\\s*\\\\title{\\(.*\\)}' *.tex" t)
	 
	 (beginning-of-buffer)
	 (while
	     (search-forward-regexp "^\\(.*?\\)\\.tex\\:\\\\title{\\(.*\\)}" nil 'noerr)
	   (if (file-readable-p (concat (match-string 1) ".pdf"))
	       (add-to-list 'cards (cons (match-string 1) (match-string 2))))
	   ))

       (mapc (lambda (c)

	       (insert (format 
"(define-key menu-bar-help-refcards-menu [%s]
             (list 'menu-item \"%s\"
                (defun show-refcard-%s () 
                  (interactive)
                  (show-refcard \"%s.pdf\"))))
"
	     (car c) (cdr c) (car c)  (car c) )))
	       (sort cards (lambda (a b) (string-lessp (cdr a) (cdr b)))))))

(defun show-refcard (pdf)
  (interactive)
  (call-process "open" nil 0 nil (format "%setc/refcards/%s" (mac-resources-path) pdf)))

(setq menu-bar-help-refcards-menu (make-sparse-keymap "Refcards"))
 
(define-key menu-bar-help-refcards-menu [fr-dired-ref]
             (list 'menu-item "Carte de r\'ef\'erence de Dired"
                (defun show-refcard-fr-dired-ref () 
                  (interactive)
                  (show-refcard "fr-dired-ref.pdf"))))
(define-key menu-bar-help-refcards-menu [fr-refcard]
             (list 'menu-item "Carte de r\'ef\'erence de GNU Emacs"
                (defun show-refcard-fr-refcard () 
                  (interactive)
                  (show-refcard "fr-refcard.pdf"))))
(define-key menu-bar-help-refcards-menu [dired-ref]
             (list 'menu-item "Dired Reference Card"
                (defun show-refcard-dired-ref () 
                  (interactive)
                  (show-refcard "dired-ref.pdf"))))
(define-key menu-bar-help-refcards-menu [calccard]
             (list 'menu-item "GNU Calc Reference Card"
                (defun show-refcard-calccard () 
                  (interactive)
                  (show-refcard "calccard.pdf"))))
(define-key menu-bar-help-refcards-menu [sk-refcard]
             (list 'menu-item "GNU Emacs -- Referenèná karta"
                (defun show-refcard-sk-refcard () 
                  (interactive)
                  (show-refcard "sk-refcard.pdf"))))
(define-key menu-bar-help-refcards-menu [cs-refcard]
             (list 'menu-item "GNU Emacs -- Referenèní karta"
                (defun show-refcard-cs-refcard () 
                  (interactive)
                  (show-refcard "cs-refcard.pdf"))))
(define-key menu-bar-help-refcards-menu [refcard]
             (list 'menu-item "GNU Emacs Reference Card"
                (defun show-refcard-refcard () 
                  (interactive)
                  (show-refcard "refcard.pdf"))))
(define-key menu-bar-help-refcards-menu [pt-br-refcard]
             (list 'menu-item "GNU Emacs: Cart\~ao de Refer\^encia"
                (defun show-refcard-pt-br-refcard () 
                  (interactive)
                  (show-refcard "pt-br-refcard.pdf"))))
(define-key menu-bar-help-refcards-menu [orgcard]
             (list 'menu-item "Org-Mode Reference Card (1/2)"
                (defun show-refcard-orgcard () 
                  (interactive)
                  (show-refcard "orgcard.pdf"))))
(define-key menu-bar-help-refcards-menu [orgcard]
             (list 'menu-item "Org-Mode Reference Card (2/2)"
                (defun show-refcard-orgcard () 
                  (interactive)
                  (show-refcard "orgcard.pdf"))))
(define-key menu-bar-help-refcards-menu [pl-refcard]
             (list 'menu-item "Przegl/ad polece/n GNU Emacsa"
                (defun show-refcard-pl-refcard () 
                  (interactive)
                  (show-refcard "pl-refcard.pdf"))))
(define-key menu-bar-help-refcards-menu [de-refcard]
             (list 'menu-item "Referenzkarte zu GNU Emacs"
                (defun show-refcard-de-refcard () 
                  (interactive)
                  (show-refcard "de-refcard.pdf"))))
(define-key menu-bar-help-refcards-menu [sk-dired-ref]
             (list 'menu-item "Referenèná karta pre Dired"
                (defun show-refcard-sk-dired-ref () 
                  (interactive)
                  (show-refcard "sk-dired-ref.pdf"))))
(define-key menu-bar-help-refcards-menu [cs-dired-ref]
             (list 'menu-item "Referenèní karta pro Dired"
                (defun show-refcard-cs-dired-ref () 
                  (interactive)
                  (show-refcard "cs-dired-ref.pdf"))))

(define-key-after menu-bar-help-menu [menu-refcards]
  `(menu-item "Printable Reference Cards" 
	      ,menu-bar-help-refcards-menu)
  'more-manuals)

(provide 'menu-refcards)
