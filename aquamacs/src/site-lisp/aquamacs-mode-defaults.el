; Aquamacs
; Mode & Package defaults
 
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-mode-defaults.el,v 1.6 2005/11/09 12:49:43 davidswelt Exp $

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

;; load auctex if present 
(ignore-errors (require 'auctex-config nil t))

;; NXML

(unless (boundp 'nxml-version)
  (load "edit-modes/nxml/rng-auto")) 
 
(assq-set-equal "\\.\\(xml\\|xsl\\|rng\\|xhtml\\)" 
		'nxml-mode 'auto-mode-alist)
 
(assq-set-equal "<\\?xml " 'nxml-mode 'magic-mode-alist)




;; ESS
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
(setenv "INFOPATH" 
	(concat (getenv "INFOPATH") 
		(concat ":" (mac-resources-path)
			"/site-lisp/edit-modes/info"
			)
		))

(autoload 'html-helper-mode "html-helper-mode" 
  "major mode for editing HTML source." t)
(assq-set-equal "\\.html$" 'html-helper-mode 'auto-mode-alist)
(assq-set-equal "\\.shtml$" 'html-helper-mode 'auto-mode-alist)
(assq-set-equal "\\(?:<\\?xml\\s +[^>]*>\\)?\\s *<\\(?:!--\\(?:[^-]\\|-[^-]\\)*-->\\s *<\\)*\\(?:!DOCTYPE\\s +[^>]*>\\s *<\\s *\\(?:!--\\(?:[^-]\\|-[^-]\\)*-->\\s *<\\)*\\)?[Hh][Tt][Mm][Ll]" 'html-helper-mode 'magic-mode-alist)


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

 
;; ---------------------------------------------------------
;; PERL EDITING and other modes

(autoload 'perl-mode "cperl-mode" 
  "alternate mode for editing Perl programs" t)
(defalias 'perl-mode 'cperl-mode)
(setq cperl-invalid-face nil) ;(uherbst)
 
(setq cperl-highlight-variables-indiscriminately t)


(provide 'aquamacs-mode-defaults)