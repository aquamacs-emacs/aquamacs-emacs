; Aquamacs
; Mode & Package defaults
 
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; Last change: $Id: aquamacs-mode-defaults.el,v 1.23 2008/08/08 13:46:15 davidswelt Exp $

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

;; JDEE

(condition-case nil 
    (require 'jde-config nil t)
  (error nil))


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

;; we don't want header lines (tab uses them)
(aquamacs-set-defaults 
 '((slime-header-line-p nil)
   (erc-mode-line-format "%s %a. %n on %t (%m,%l) %o")
   (erc-header-line-format nil)))

(defun load-and-setup-slime ()
  "Load and setup SLIME"
  (require 'slime)
  (slime-setup) 

  ;; slime 2.0 bug workarounds
  (define-key sldb-mode-map [follow-link] 'mouse-face)
  (put 'slime-space 'delete-selection t)


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


;; 
(autoload 'actr-mode "actr-mode" "ACT-R mode" 'interactive nil)
(autoload 'haskell-mode "haskell-mode" "Haskell mode" 'interactive nil)

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
(autoload 'rails-minor-mode "rails.el" "Enter Ruby on Rails mode" 'interactive nil)

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

;;;***

;;;### (autoloads (jython-mode python-mode run-python) "python" "progmodes/python.el"
;;;;;;  (17640 42650))
;;; Generated autoloads from progmodes/python.el

(add-to-list (quote interpreter-mode-alist) (quote ("jython" . jython-mode)))

(add-to-list (quote interpreter-mode-alist) (quote ("python" . python-mode)))

(add-to-list (quote auto-mode-alist) (quote ("\\.py\\'" . python-mode)))

(autoload (quote run-python) "python" "\
Run an inferior Python process, input and output via buffer *Python*.
CMD is the Python command to run.  NOSHOW non-nil means don't show the
buffer automatically.

Normally, if there is a process already running in `python-buffer',
switch to that buffer.  Interactively, a prefix arg allows you to edit
the initial command line (default is `python-command'); `-i' etc. args
will be added to this as appropriate.  A new process is started if:
one isn't running attached to `python-buffer', or interactively the
default `python-command', or argument NEW is non-nil.  See also the
documentation for `python-buffer'.

Runs the hook `inferior-python-mode-hook' (after the
`comint-mode-hook' is run).  (Type \\[describe-mode] in the process
buffer for a list of commands.)

\(fn &optional CMD NOSHOW NEW)" t nil)

(autoload (quote python-mode) "python" "\
Major mode for editing Python files.
Font Lock mode is currently required for correct parsing of the source.
See also `jython-mode', which is actually invoked if the buffer appears to
contain Jython code.  See also `run-python' and associated Python mode
commands for running Python under Emacs.

The Emacs commands which work with `defun's, e.g. \\[beginning-of-defun], deal
with nested `def' and `class' blocks.  They take the innermost one as
current without distinguishing method and class definitions.  Used multiple
times, they move over others at the same indentation level until they reach
the end of definitions at that level, when they move up a level.
\\<python-mode-map>
Colon is electric: it outdents the line if appropriate, e.g. for
an else statement.  \\[python-backspace] at the beginning of an indented statement
deletes a level of indentation to close the current block; otherwise it
deletes a character backward.  TAB indents the current line relative to
the preceding code.  Successive TABs, with no intervening command, cycle
through the possibilities for indentation on the basis of enclosing blocks.

\\[fill-paragraph] fills comments and multi-line strings appropriately, but has no
effect outside them.

Supports Eldoc mode (only for functions, using a Python process),
Info-Look and Imenu.  In Outline minor mode, `class' and `def'
lines count as headers.  Symbol completion is available in the
same way as in the Python shell using the `rlcompleter' module
and this is added to the Hippie Expand functions locally if
Hippie Expand mode is turned on.  Completion of symbols of the
form x.y only works if the components are literal
module/attribute names, not variables.  An abbrev table is set up
with skeleton expansions for compound statement templates.

\\{python-mode-map}

\(fn)" t nil)

(autoload (quote jython-mode) "python" "\
Major mode for editing Jython files.
Like `python-mode', but sets up parameters for Jython subprocesses.
Runs `jython-mode-hook' after `python-mode-hook'.

\(fn)" t nil)


(provide 'aquamacs-mode-defaults)