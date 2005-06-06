; aquamacs-tool-bar-mode

;; This minor mode displays the tool bar only in default frames, 
;; but not in special display frames  
;;
;; Author: David Reitter, david.reitter@gmail.com, 
;; http://www.reitter-it-media.de/

;; This file is part of Aquamacs Emacs.
;; http://aquamacs.sourceforge.net

;; Licensed under the GNU General Public License (GPL).
;; Copy, change, redistribute. Please acknowledge the original author.

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


;; 

(define-minor-mode aquamacs-tool-bar-mode
 "Turn on toolbars in default frames, but not in special display frames.
That means that the toolbar is not shown in in frames with help, info
or similar buffers.
See `tool-bar-add-item' and `tool-bar-add-item-from-menu' for
conveniently adding tool bar items."
  :init-value t
  :global t
  :group 'mouse
  :group 'frames
  (and (display-images-p)
       (eq tool-bar-mode nil) ;; only if tool-bar-mode is off
	 ;; Alter existing frames...
	 (mapc (lambda (f)
		 (let ( (buf (window-buffer (frame-first-window f))))
       
	   (modify-frame-parameters f (list (cons 'tool-bar-lines   (if aquamacs-tool-bar-mode (if (special-display-p (buffer-name buf)) 0 1) 0)))
   
				    )
	   ))
	       (frame-list))
	 ;; ...and future ones.
	 
	 (if aquamacs-tool-bar-mode
	     (progn
	       (assq-set  'tool-bar-lines 1 'default-frame-alist )
	       (assq-set  'tool-bar-lines 0 'special-display-frame-alist )
	       )
					; else
	   (progn
	     (setq default-frame-alist (assq-delete-all 'tool-bar-lines default-frame-alist))
	     (setq special-display-frame-alist (assq-delete-all 'tool-bar-lines special-display-frame-alist))
	     )
	   )

       (if (and aquamacs-tool-bar-mode
		(display-graphic-p)
		(= 1 (length (default-value 'tool-bar-map)))) ; not yet set up
	   (tool-bar-setup)))
)

(defun turn-on-toolbar-here ()
  (modify-frame-parameters (selected-frame) (list (cons 'tool-bar-lines 1))
					 )
)

(add-hook 'Info-mode-hook
	  (lambda ()
	    (if aquamacs-tool-bar-mode (turn-on-toolbar-here))	    )
)


; defaults to true
(put 'aquamacs-tool-bar-mode 'standard-value '(t))

;; "save options" needs to be augmented externally (aquamacs does that)
 

;; some
;(tool-bar-mode -1) ;; turn it off now

; we want tool-bars only in normal frames, if at all
; (assq-set  'tool-bar-lines 1 'default-frame-alist )
; (assq-set  'tool-bar-lines 0 'special-display-frame-alist )

(provide 'aquamacs-tool-bar)
 