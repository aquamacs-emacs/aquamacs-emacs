;;; matlab-publish.el --- Utilities for editing MATLAB files for publishing

;; Copyright (C) 2009 Uwe Brauer

;; Author: Uwe Brauer oub@mat.ucm.es
;; Maintainer: Uwe Brauer oub@mat.ucm.es
;; Created: 25 Feb 2009
;; Version: 1.0
;; Keywords:

 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to oub@mat.ucm.es) or from
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;; 02139, USA.

;; LCD Archive Entry:
;; my-pub|Uwe Brauer|oub@mat.ucm.es
;; |
;; |$Date: 2009/07/06 19:59:10 $|$Revision: 1.1 $|~/packages/my-pub.el

;;; Commentary:

;;{{{Change log:

;; $Id: matlab-publish.el,v 1.1 2009/07/06 19:59:10 zappo Exp $
;; $Log: matlab-publish.el,v $
;; Revision 1.1  2009/07/06 19:59:10  zappo
;; Utilities for editing MATLAB files for publishing
;;
;; Revision 1.9  2009/03/08 09:38:31  oub
;; 	* matlab-publish.el (matlab-select-environment): change function
;; 	slightly
;; 	(matlab-insert-section): new function
;;
;; Revision 1.8  2009/03/07 13:54:57  oub
;; 	  (matlab-write-region-novisit): New function
;; 	  (matlab-publish-region-old): new function
;; 	  (matlab-publish-region): New function
;; 	  (matlab-publish-region-latex): new function
;; 	  (matlab-publish-region-html): new function
;;
;; Revision 1.7  2009/03/07 13:26:55  oub
;; 	(matlab-show-matlab-shell): New variable
;; 	(matlab-publish-file-latex): add
;; 	  (if matlab-show-matlab-shell
;; 	  (matlab-show-matlab-shell-buffer)))
;;
;; Revision 1.6  2009/03/07 11:00:04  oub
;; 	* matlab-publish.el (matlab-boldify): Modify the function.
;;
;; Revision 1.5  2009/03/06 14:24:47  oub
;; 	* matlab-publish.el (matlab-temp-region-file): new variable
;; 	(matlab-write-region): New function
;;
;; Revision 1.4  2009/02/26 16:18:23  oub
;; modifiy the insert functions in order that the jump to the text where
;; new stuff has to be inserted.
;;
;; Revision 1.3  2009/02/26 16:06:08  oub
;; (defun matlab-select-environment (ch)
;;   (interactive "c1: title, 2: descr, 3: bold:, 4:mono, 5:pre, 6:equation, 7:list ")
;;   (setq ch (downcase ch))
;;   (call-interactively (cond ((eql ch ?1) #'matlab-insert-title)
;;                             ((eql ch ?2) #'matlab-insert-description-text)
;;                             ((eql ch ?3) #'matlab-insert-bold-text)
;;                             ((eql ch ?4) #'matlab-insert-monospaces-text)
;;                             ((eql ch ?5) #'matlab-insert-preformated-text)
;;                             ((eql ch ?6) #'matlab-insert-equation)
;;                             ((eql ch ?7) #'matlab-insert-bullet-list)
;;                             (t (error 'args-out-of-range '(1 2 3 5 6 7 ch))))))
;;
;; Revision 1.2  2009/02/26 11:24:17  oub
;; New functions
;; (defun matlab-insert-title ()
;;
;; (defun matlab-insert-description-text ()
;;
;;
;; (defun matlab-insert-bold-text ()
;;
;;
;; (defun matlab-insert-monospaces-text ()
;;
;;
;;
;; (defun matlab-insert-preformated-text ()
;;
;;
;; (defun matlab-insert-equation ()
;;
;;
;; (defun matlab-insert-bullet-list ()
;;
;; Revision 1.1  2009/02/26 11:15:02  oub
;; Initial revision
;;

;;}}}

;;; Code:


;; (defun my-publish-matlab ()
;;   (interactive)
;;   (let ((pub (file-name-nondirectory (buffer-file-name))))
;; 	(let ((arg (concat "publish('" pub "','latex')")))
;; 	  (matlab-shell-run-command arg))))

(require 'matlab)

;; Variables
(defvar matlab-temp-region-file "region.m"  ;Version-1.5
  "*Variable for the file which saves the region for publishing.")

(defvar matlab-show-matlab-shell t
"* t means the matlab buffer shell is shown when running matlab.")

;; Functions
(defun matlab-select-publish-form (ch)
"This function allows to publish the m file, either as in LaTeX or in
HTML format."
  (interactive "c1: LaTeX, 2: HTML ")
  (setq ch (downcase ch))
  (call-interactively (cond ((eql ch ?1) #'matlab-publish-file-latex)
                            ((eql ch ?2) #'matlab-publish-file-html)
                            (t (error 'args-out-of-range '(1 2 ch))))))

(defun matlab-publish-file-latex ()		;Version-1.7
"Publish a matlab file in the LaTeX format."
  (interactive)
  (let ((pub (file-name-nondirectory (buffer-file-name)))) 
	(matlab-shell-run-command (format "publish('%s','latex')" pub)))
  (if matlab-show-matlab-shell
	  (matlab-show-matlab-shell-buffer)))



(defun matlab-publish-file-html ()		;Version-1.7
  (interactive)
"Publish a matlab file in the html format."
  (let ((pub (file-name-nondirectory (buffer-file-name))))
 	(matlab-shell-run-command (format "publish('%s','html')" pub)))
  (if matlab-show-matlab-shell
	  (matlab-show-matlab-shell-buffer)))



(defun matlab-select-environment (ch) 	;Version-1.9
"This functions inserts structured text, which results for example 
in LaTeX mode in title, sections, description, boldified text,  unnumbered equations and bullet list."
  (interactive "c1: title, 2: section, 3:descrip, 4:boldify, 5:equation, 6:list ")
  (setq ch (downcase ch))
  (call-interactively (cond ((eql ch ?1) #'matlab-insert-title)
                            ((eql ch ?2) #'matlab-insert-section)
                            ((eql ch ?3) #'matlab-insert-description-text)
                            ((eql ch ?4) #'matlab-boldify)
                            ((eql ch ?5) #'matlab-insert-equation)
                            ((eql ch ?6) #'matlab-insert-bullet-list)
                            (t (error 'args-out-of-range '(1 2 3 5 6 ch))))))


(defun matlab-insert-title ()
  (interactive)
  (beginning-of-buffer nil)
  (insert "%% TITLE\n")
  (previous-line 1)
  (forward-char 3))

(defun matlab-insert-section ()		;Version-1.9
  (interactive)
  (insert "%% Section\n")
  (previous-line 1)
  (forward-char 3))


(defun matlab-insert-description-text ()
  (interactive)
  (insert "%%\n")
  (insert "% DESCRIPTIVE TEXT\n")
  (previous-line 1)
  (forward-char 2))



(defun matlab-boldify ()				;Version-1.6
"Insert either \%\%\\n \% \*BOLD TEXT\*\\n or, when mark is active,
surrounds region by * *."
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
		  (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
	  (save-excursion
		(goto-char (point))
		(insert "*")
		(goto-char (mark))
		(insert "*"))
  (insert "\n%%\n")
  (insert "% *BOLD TEXT*\n")
	(backward-char 2)))


(defun matlab-insert-bold-text ()
  (interactive)
  (insert "%%\n")
  (insert "% *BOLD TEXT*\n")
  (previous-line 1)
  (forward-char 2))

(defun matlab-insert-monospaces-text ()
  (interactive)
  (insert "%%\n")
  (insert "% |MONOSPACED TEXT|\n")
  (previous-line 1)
  (forward-char 2))


(defun matlab-insert-preformated-text ()
  (interactive)
  (insert "%%\n")
  (insert "%\n") 
  (insert "%  PREFORMATTED\n")
  (insert "%  TEXT\n")
  (insert "% \n")
  (previous-line 3)
  (forward-char 3))

(defun matlab-insert-equation ()
  (interactive)
  (insert "%%\n")
  (insert "% \n")
  (insert "% $$e^{\pi i} + 1 = 0$$\n")
  (insert "% \n")
  (previous-line 2)
  (forward-char 4))


(defun matlab-insert-bullet-list ()
  (interactive)
  (insert "%%\n")
  (insert "% \n")
  (insert "% * ITEM1\n")
  (insert "% * ITEM2\n")
  (insert "% \n")
  (previous-line 3)
  (forward-char 4))




(defun matlab-write-region (start end)	;Version-1.5
  (interactive "r")
  (write-region start end (expand-file-name matlab-temp-region-file) nil nil nil nil)
  (find-file matlab-temp-region-file))


(defun matlab-write-region-novisit (start end)
  (interactive "r")
  (write-region start end (expand-file-name matlab-temp-region-file) nil nil nil nil))




(defun matlab-publish-region-old (ch)	;Version-1.8
  (interactive "c1: LaTeX, 2: HTML ")
  (setq ch (downcase ch))
  (matlab-write-region-novisit (region-beginning) (region-end))
  (call-interactively (cond ((eql ch ?1) #'matlab-publish-region-latex)
                            ((eql ch ?2) #'matlab-publish-region-html)
                            (t (error 'args-out-of-range '(1 2 ch))))))


(defun matlab-publish-region (start end ch)	 ;Version-1.8
  (interactive "r\nc1: LaTeX, 2: HTML ")
  (setq ch (downcase ch))
  (write-region start end (expand-file-name matlab-temp-region-file) nil nil nil nil)
  (call-interactively (cond ((eql ch ?1) #'matlab-publish-region-latex)
                            ((eql ch ?2) #'matlab-publish-region-html)
                            (t (error 'args-out-of-range '(1 2 ch))))))



(defun matlab-publish-region-latex ()	;Version-1.8
  (interactive)
	(matlab-shell-run-command (format "publish('%s','latex')" matlab-temp-region-file))
  (if matlab-show-matlab-shell
	  (matlab-show-matlab-shell-buffer)))



(defun matlab-publish-region-html ()	;Version-1.8
  (interactive)
	(matlab-shell-run-command (format "publish('%s','html')" matlab-temp-region-file))
  (if matlab-show-matlab-shell
	  (matlab-show-matlab-shell-buffer)))




(provide 'matlab-publish)

;;; MY-PUB.EL ends here

;; (defvar isp "hallo"
;; "*check")
;;
;; (when (string-match isp "hallo"))
;; (message "it works"))