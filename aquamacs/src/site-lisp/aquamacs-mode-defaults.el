; Aquamacs
; Mode & Package defaults
 
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-mode-defaults.el,v 1.34 2009/03/21 02:33:24 davidswelt Exp $

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

(condition-case nil 
    (require 'auctex-config nil t)
  (error nil))

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


;; NXML

(unless (or (boundp 'nxml-version) (>= emacs-major-version 23))
  (load "rng-auto")) 
 
(assq-set-equal "\\.\\(xml\\|xsl\\|rng\\|xhtml\\)" 
		'nxml-mode 'auto-mode-alist)
 
(assq-set-equal "<\\?xml " 'nxml-mode 'magic-mode-alist)

;; JDEE

(defun aquamacs-announce-jdee ()
  "Notify users of the JDEE plugin."
  (unless (boundp 'jde-jdk)
    (message "For the best Java editing environment, get the JDEE plugin at http://aquamacs.org."))
  (remove-hook 'jde-mode-hook 'aquamacs-announce-jdee))

(add-hook 'jde-mode-hook 'aquamacs-announce-jdee)


;; SLIME

(defun aquamacs-announce-slime ()
  "Notify users of the SLIME plugin."
  (unless (fboundp 'slime-setup)
    (message "For superior Lisp interaction, get the SLIME plugin at http://aquamacs.org."))
  (remove-hook 'lisp-mode-hook 'aquamacs-announce-slime))

(add-hook 'lisp-mode-hook 'aquamacs-announce-slime)

;; we don't want header lines (tab uses them)
(aquamacs-set-defaults 
 '((Info-use-header-line nil)
   (slime-header-line-p nil)
   (erc-mode-line-format "%s %a. %n on %t (%m,%l) %o")
   (erc-header-line-format nil)))

(assq-set-equal "\\.wiki$" 'wikipedia-mode 'auto-mode-alist) 
(autoload 'wikipedia-mode "wikipedia-mode.el" "Major mode for editing Wikipedia articles." 'interactive nil)

;; ESS
;; Need this to prevent the tramp problem.
(setq ess-r-versions nil)

(autoload 'ess-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R "ess-site" "Emacs Speaks Statistics" t)
(autoload 'S-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'Rnw-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'omegahat-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'XLS-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'STA-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'SAS-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'S-transcript-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R-transcript-mode "ess-site" "Emacs Speaks Statistics" t)


;; 
(autoload 'actr-mode "actr-mode" "ACT-R mode" 'interactive nil)
(add-to-list (quote auto-mode-alist) (quote ("\\.actr\\'" . actr-mode)))

(load "haskell-site-file") ;; autoloads

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

(autoload 'applescript-mode "applescript-mode" 
  "major mode for editing AppleScript source." t)
(assq-set-equal "\\.applescript$" 'applescript-mode 'auto-mode-alist)

(autoload 'php-mode "php-mode" "major mode for editing PHP source." t)
(assq-set-equal "\\.php$" 'php-mode 'auto-mode-alist)

;; do we need to distinguish?
(autoload 'rails-minor-mode "rails.el" "Enter Ruby on Rails mode" 'interactive nil)

;; Matlab
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
(assq-set-equal "\\.m$" 'matlab-mode 'auto-mode-alist) 

;; Objective C

(defun objc-mode-buffer-check ()
  (if (string-match "\\.m$" buffer-file-name)
      (save-restriction
	(narrow-to-region (point-min)
			  (min (point-max)
			       (+ (point-min) magic-mode-regexp-match-limit)))
	(looking-at "\\(.\\|\n\\)*#\\(include\\|import\\|define\\)"))))

(setq magic-mode-alist
     (append '(("\\(.\\|\n\\)*\n@\\(implementation\\|interface\\|protocol\\)" . objc-mode)
	       (objc-mode-buffer-check . objc-mode))
	   magic-mode-alist))

(assq-set-equal "\\.org\\'" 'org-mode 'auto-mode-alist) 
(aquamacs-set-defaults 
 '((org-support-shift-select t)))

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

(add-to-list (quote interpreter-mode-alist) (quote ("jython" . jython-mode)))

(add-to-list (quote interpreter-mode-alist) (quote ("python" . python-mode)))

(add-to-list (quote auto-mode-alist) (quote ("\\.py\\'" . python-mode)))


(autoload (quote py-shell) "python-mode" 
  "Start an interactive Python interpreter in another window.")
(defalias 'python-shell 'py-shell)
; what about run-python - we'll leave it for now

(autoload (quote python-mode) "python-mode" 
  "Major mode for editing Python files." t nil)

(autoload (quote jython-mode) "python-mode" 
  "Major mode for editing Jython/Jython files." t nil)


(provide 'aquamacs-mode-defaults)