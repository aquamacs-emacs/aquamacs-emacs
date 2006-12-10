; Aquamacs
; Mode & Package defaults
 
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-mode-defaults.el,v 1.17 2006/12/10 10:37:27 davidswelt Exp $

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

(require 'smart-dnd) ;; Smart Drag&Drop

;; load auctex if present 

(ignore-errors (require 'auctex-config nil t))
(defun smart-dnd-latex ()
   (smart-dnd-setup
    '(
      ("\\.tex\\'" . "\\input{%r}\n")
      ("\\.cls\\'" . "\\documentclass{%f}\n")
      ("\\.sty\\'" . "\\usepackage{%f}\n")
      ("\\.eps\\'" . "\\includegraphics[]{%r}\n")
      ("\\.ps\\'"  . "\\includegraphics[]{%r}\n")
      ("\\.pdf\\'" . "\\includegraphics[]{%r}\n")
      ("\\.jpg\\'" . "\\includegraphics[]{%r}\n")
      ("\\.png\\'" . "\\includegraphics[]{%r}\n")
      )))
(add-hook 'latex-mode-hook 'smart-dnd-latex)
(defvar LaTeX-mode-hook nil)
(add-hook 'LaTeX-mode-hook 'smart-dnd-latex)

;; JDEE

(ignore-errors (require 'jde-config nil t))



;; NXML

(unless (boundp 'nxml-version)
  (load "edit-modes/nxml/rng-auto")) 
 
(assq-set-equal "\\.\\(xml\\|xsl\\|rng\\|xhtml\\)" 
		'nxml-mode 'auto-mode-alist)
 
(assq-set-equal "<\\?xml " 'nxml-mode 'magic-mode-alist)


;; SLIME


;; update SLIME
;; cd ~/Temp
;; mkdir slime
;; cd slime
;; curl -O http://common-lisp.net/project/slime/slime-2.0.zip

;; unzip slime-*.zip
;; mv slime-* ~/src/edit-modes/slime
;; cd ~/src/edit-modes
;; mv slime/doc/slime.info info/
;; rm -r slime/doc slime/ChangeLog 
;; gzip slime/*.el


 
;;(aquamacs-set-defaults '((inferior-lisp-program "/opt/sbcl/bin/sbcl") ))

(defun load-and-setup-slime ()
  "Load and setup SLIME"
  (require 'slime)
  (slime-setup) 
  ;; run hook directly, because it wouldn't
  ;; be picked up otherwise
  (slime-lisp-mode-hook)
  ;; remove this from lisp-mode-hook
  (remove-hook 'lisp-mode-hook 'load-and-setup-slime))

(add-hook 'lisp-mode-hook 'load-and-setup-slime)
(autoload 'slime "slime.el" "Start an inferior/superior Lisp and connect to its Swank server." 'interactive nil)




;; ESS
;; Need this to prevent the tramp problem.
(setq ess-r-versions nil)

(autoload 'ess-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'S-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'Rnw-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'omegahat-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'XLS-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'STA-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'SAS-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'S-transcript-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R-transcript-mode "ess-site" "Emacs Speaks Statistics" t)



(setq auto-mode-alist
	(append
	 '(("\\.sp\\'"		. S-mode) ;; re: Don MacQueen <macq@llnl.gov>
	   ("\\.[qsS]\\'"	. S-mode) ;; q,s,S [see ess-restore-asm-extns above!]
	   ("\\.ssc\\'"		. S-mode) ;; Splus 4.x script files.
	   ("\\.[rR]\\'"	. R-mode)
	   ("\\.[rR]nw\\'"	. Rnw-mode)
	   ("\\.[rR]profile\\'" . R-mode)
	   ("NAMESPACE\\'"	. R-mode)
	   ("\\.omg\\'"         . omegahat-mode)
	   ("\\.hat\\'"         . omegahat-mode) ;; Duncan's pref'd...
	   ("\\.lsp\\'"		. XLS-mode)
	   ("\\.do\\'"		. STA-mode)
	   ("\\.ado\\'"		. STA-mode)
	   ("\\.[Ss][Aa][Ss]\\'"	. SAS-mode)
	   ;; Many .log/.lst files, not just SAS
	   ;;("\\.log\\'"	. SAS-log-mode)
	   ;;("\\.lst\\'"	. SAS-listing-mode)
	   ("\\.[Ss]t\\'"	. S-transcript-mode)
	   ("\\.[Ss]out"	. S-transcript-mode)
	   ("\\.[Rr]t\\'"	. R-transcript-mode)
	   ("\\.[Rr]out"	. R-transcript-mode) 
          )
	 auto-mode-alist))


(defvar ess-etc-directory (concat  (mac-resources-path)
				  "/site-lisp/edit-modes/ess-mode/etc"
				  ))

(aquamacs-set-defaults 
 '((html-helper-mode-uses-JDE nil)))
(autoload 'html-helper-mode "html-helper-mode" 
  "major mode for editing HTML source." t)
(assq-set-equal "\\.html$" 'html-helper-mode 'auto-mode-alist)
(assq-set-equal "\\.shtml$" 'html-helper-mode 'auto-mode-alist)
(assq-set-equal "\\(?:<\\?xml\\s +[^>]*>\\)?\\s *<\\(?:!--\\(?:[^-]\\|-[^-]\\)*-->\\s *<\\)*\\(?:!DOCTYPE\\s +[^>]*>\\s *<\\s *\\(?:!--\\(?:[^-]\\|-[^-]\\)*-->\\s *<\\)*\\)?[Hh][Tt][Mm][Ll]" 'html-helper-mode 'magic-mode-alist)
(defun smart-dnd-html ()
   (smart-dnd-setup
    '(
      ("\\.gif\\'" . "<img src=\"%r\">\n")
      ("\\.jpg\\'" . "<img src=\"%r\">\n")
      ("\\.png\\'" . "<img src=\"%r\">\n")
      ("\\.css\\'" . "<link rel=\"stylesheet\" type=\"text/css\" href=\"%r\">\n" )
      ("\\.js\\'"  . "<script type=\"text/javascript\" src=\"%r\"></script>\n" )
      (".*" . "<a href=\"%r\">%f</a>\n")
      )))
(add-hook 'html-mode-hook 'smart-dnd-html)
 
(autoload 'javascript-mode "javascript-mode" "JavaScript mode" t)
(assq-set-equal "\\.js$" 'javascript-mode 'auto-mode-alist)


(autoload 'css-mode "css-mode" "major mode for editing CSS source." t)
(assq-set-equal "\\.css$" 'css-mode 'auto-mode-alist)

(autoload 'applescript-mode "applescript-mode" 
  "major mode for editing AppleScript source." t)
(assq-set-equal "\\.applescript$" 'applescript-mode 'auto-mode-alist)

(autoload 'php-mode "php-mode" "major mode for editing PHP source." t)
(assq-set-equal "\\.php$" 'php-mode 'auto-mode-alist)


(autoload 'ruby-mode "ruby-mode" "major mode for editing Ruby source." t)
(assq-set-equal "\\.rb$" 'ruby-mode 'auto-mode-alist) 
;; watch out - .rb is also used for realbasic
;; do we need to distinguish?
;; we don't have a REALBasic mode yet

(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
(assq-set-equal "\\.m$" 'matlab-mode 'auto-mode-alist) 


;; ---------------------------------------------------------
;; PERL EDITING  

(autoload 'cperl-mode "cperl-mode" "major mode for editing Perl source." t)
(assq-set-equal "\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'"  
		'cperl-mode 'auto-mode-alist)
(assq-set-equal "perl" 'cperl-mode 'interpreter-mode-alist)
(assq-set-equal "perl5" 'cperl-mode 'interpreter-mode-alist)
(assq-set-equal "miniperl" 'cperl-mode 'interpreter-mode-alist)

(aquamacs-set-defaults 
 '((cperl-invalid-face nil)
   (cperl-highlight-variables-indiscriminately t)))

;; C-Mode
(defun smart-dnd-c () (smart-dnd-setup '(("\\.h\\'" . "#include <%f>"))))
(add-hook 'c-mode-common-hook 'smart-dnd-c)
(setq auto-mode-alist
	(append
	 '(("\\.cp\\'"		. c++-mode)    ;; old Mac c++ code
          )
	 auto-mode-alist))

(provide 'aquamacs-mode-defaults)