;;; html-helper-mode.el --- Major mode for composing html files.
;;; Revision: 3.10aquamacs2
;; based on beta version dated 22-Mar-2004 http://savannah.inetbridge.net/baol-hth/
;; plus: 3.0.4Kilo's CSS mode
;; plus: changes to set major-mode while running mode hooks  (Aquamacs compatibility)
;;       - David Reitter, 2008

;; Mantainer : Gian Uberto "Saint" Lauri <saint@eng.it>
;;                                       <saint@dei.unipd.it>*
;; Original Author: Nelson Minar <nelson@santafe.edu>
;; Original version Maintainer: Nelson Minar <nelson@santafe.edu>
;; Changes by by: Gian Uberto Lauri <lauri@eng.it>, <saint@dei.unipd.it>*
;;                                * works only from DEI, Padova.
;; Credits : Larry Smith and Tony Graham for the ASP/PHP matching regexp
;;           prototype.
;;           Stan Lanning for the defadvice code that prevents indenting
;;           of <PRE></PRE>, for the defadvice code that leaves the cursor
;;           where it is during narrowing to script code, enhancments to
;;           the scripting narrowing
;;           Charles Curley for the commentary of tempo.el behaviour
;;           Samir Barjoud for giving me the luser cap when I didn't notice
;;           that *ALL* I needed to write  html-helper-match-asp-php was in
;;           font-lock.el.
;;           Theodore A. Jump for fixing fold tags in this source (after I
;;           broke them
;;           David J. Biesack for suggesting a good version checking.

;;  Project  URL: https://savannah.nongnu.org/projects/baol-hth/
;; Homepage URL: http://www.nongnu.org/baol-hth/index.html

;; Created: 01 Feb 1994
;; Keywords: HTML major-mode

;; LCD Archive Entry:
;; html-helper-mode|Gian Uberto Lauri|saint@eng.it|
;; Major mode for editing HTML.|
;;

;; Copyright (C) 1994 Nelson Minar
;; Copyright (C) 1995 Nelson Minar and Ulrik Dickow
;; Copyright (C) 1999 Nelson Minar, Ulrik Dickow and Gian Uberto Lauri

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

;;{{{ Commentary

;; Installation:
;;   Add this line in your .emacs:
;;     (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;;   To invoke html-helper-mode automatically on .html files, do this:
;;     (setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
;;   To invoke html-helper-mode automatically on .asp files, do this:
;;     (setq auto-mode-alist (cons '("\\.asp$" . html-helper-mode) auto-mode-alist))
;;   To invoke html-helper-mode automatically on .phtml files, do this:
;;     (setq auto-mode-alist (cons '("\\.phtml$" . html-helper-mode) auto-mode-alist))

;;
;;   This mode requires another lisp file, tempo.el. This can be
;;     retrieved from ftp://ftp.lysator.liu.se/pub/emacs/tempo.el
;;   Xemacs users need to have auc-menu installed.
;;   Emacs 18 users need to have auc-menu and add-hook installed.
;;   If your OS has broken 14 character filenames
;;      this mode will also work with the name "html-mode.el".

;; Configuration:
;;   see the "user variables" section, or the documentation on configuration
;;   in http://www.santafe.edu/~nelson/tools/. There are variables you want to
;;   configure, particularly html-helper-address-string and
;;   html-helper-use-expert-menu

;; See also: http://www.gest.unipd.it/~saint/hth.html for further details
;; regarding server code support.

;; Description:
;;   html-helper-mode makes it easier to write HTML documents. This mode
;;   handles inserting HTML codes in a variety of ways (keybindings, menus,
;;   completion in the buffer). It also supports indentation, timestamps,
;;   skeletons for new documents, hilit19 patterns, and a variety of other
;;   things. For the full skinny, see the HTML documentation that comes
;;   with the package or is at http://www.santafe.edu/~nelson/tools/

;; Thank yous:
;;   David Kågedal <davidk@lysator.liu.se> for the tempo code which
;;     forms the core of the HTML insertion, as well as the HTML+ tag.
;;   Marc Hedlund <march@europa.com> for general encouragement and
;;     many helpful suggestions, especially with HTML/2.0 compliance
;;     and form design.
;;   Ulrik Dickow <dickow@nbi.dk> for the font-lock code
;;   Denis Howe <dbh@doc.ic.ac.uk> for writing browse-url.
;;   Magnus Homann <d0asta@dtek.chalmers.se> and Jamshid Afshar
;;     <jamshid@ses.com> for timestamp suggestions.
;;   Everyone who sent me a version of menus (16 in all!)
;;   Marc Andreessen <marca@mcom.com> for writing the original html-mode

;; The newest version of html-helper-mode should always be available from
;;   http://www.gest.unipd.it/~saint/hth.html

;; Changes moved to hhm-changelog

;; This code was writting using folding.el, a wonderful folding editor
;; minor mode for emacs. That's what the strange {{{ comments are for.

;;}}}
;;{{{ Macro
(eval-when-compile
  ;;
  ;; We don't do this at the top-level as we only use non-autoloaded macros.
  (require 'cl)
  ;;
  ;; Borrowed from font-lock that borrowed it from lazy-lock.el.
  ;; We use this to preserve or protect things when modifying text properties.
  (defmacro save-buffer-state (varlist &rest body)
    "Bind variables according to VARLIST and eval BODY restoring buffer state."
    `(let* ,(append varlist
		    '((modified (buffer-modified-p)) (buffer-undo-list t)
		      (inhibit-read-only t) (inhibit-point-motion-hooks t)
		      (inhibit-modification-hooks t)
		      deactivate-mark buffer-file-name buffer-file-truename))
       ,@body
       (when (and (not modified) (buffer-modified-p))
	 (set-buffer-modified-p nil)))))
;;}}}
;;{{{ Code:

(defconst html-helper-mode-version "3.10")

;;{{{ user variables

;;{{{ defcustoms
(defgroup html-helper nil
  "Customizing html-helper-mode"
  :group 'languages
  :group 'hypermedia
  :group 'local)

(defgroup html-helper-faces nil
  "Customizing html-helper-mode custom faces"
  :group 'html-helper
  :group 'faces)

;; Default distribution doesn't include visual-basic-mode
(defcustom html-helper-mode-uses-visual-basic nil
  "Non nil to require visual-basic-mode"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)

;; Default distribution doesn't include jde
;;
;; Suggestion by :
;; Jari Aalto <jari.aalto@poboxes.com>
;;
;;    I think that people that have installed JDE, use it, so
;;    it would be logical to preset this automatically using
;;    `locate-library'
(defcustom html-helper-mode-uses-JDE (locate-library "jde")
  "No nil to use jde instead of java-mode"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)

(defcustom html-helper-mode-uses-bold-italic nil
  "Non nil to use the bold-italic font (if your font supports it)"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)

(defcustom html-helper-mode-uses-KG-style nil
  "Non nil to make Emacs consider PHP/ASP code blocks beginning in
the first column"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)

(defcustom html-helper-mode-global-JSP-not-ASP-flag t
  "Non nil to make Emacs consider <% %> blocks as JSP (global default behaviour)"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)

(defcustom html-helper-mode-insert-attributes-always nil
  "non nil to make Emacs insert empty tag attributes when tempo-interactive is nil"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)

(defcustom html-helper-mode-using-iso-8859-15-characters t
  "non nil to make Emacs map some iso-8859-15 characters (i.e. àìùòèé
 to the appropriate HTML entities"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)
;;}}}

;;{{{ defvars...
(defvar html-helper-mode-local-JSP-not-ASP-flag
  html-helper-mode-global-JSP-not-ASP-flag
  "Non nil to make Emacs consider <% %> blocks as JSP (buffer local behaviour)")

(defvar html-helper-mode-run-the-mode t "When t, the following variables

 comment-start, comment-end, comment-column, comment-start-skip,
 indent-line-function, html-helper-count,
 html-helper-mode-local-JSP-not-ASP-flag

are created as local. When nil the creation is skipped.")
;; Visual basic mode is not in the standard distribution, so I let the user
;; override html-helper-mode-uses-visual-basic with a nil value.
(cond (html-helper-mode-uses-visual-basic (require 'visual-basic-mode)))
(cond (html-helper-mode-uses-JDE (require 'jde)))
(require 'cc-mode)
'(require 'cl)

;; Set this to be whatever signature you want on the bottom of your pages.
(defvar html-helper-address-string ""
  "*The default author string of each file.")

;; Features; these are all good to have on. (see also tempo.el)

(defvar html-helper-use-expert-menu t
  "*If not nil, then use the full HTML menu.")

(defvar html-helper-do-write-contents-functions t
  "*If not nil, then modify `write-contents-functions' to do timestamps.")
(defvaralias 'html-helper-do-write-file-hooks
  'html-helper-do-write-contents-functions)

(defvar html-helper-build-new-buffer-flag t
  "*If not nil, then insert `html-helper-new-buffer-strings' for new buffers.")

;; variables to configure (these defaults are reasonable.)

(defvar html-helper-htmldtd-version "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n"
  "*Version of HTML DTD you're using.")

(defvar html-helper-user-menu nil
  "*Extra items to put in the HTML expert menu.
The value of this symbol is appended to the beginning of the expert
menu that is handed off to easymenu for definition. It should be a
list of vectors or lists which themselves are vectors (for submenus).")

(defvar html-helper-basic-offset 2
  "*Basic indentation size used for list indentation")

;; Wed Jan 10 09:35:53 2001 Saint
;;
;; fixing indentation
;;(defvar html-helper-item-continue-indent 2
;;  "*Indentation of lines that follow a <li> item.
;;Default is 2, the length of things like \"<li>\" and \"<dd>\".")

(defvar html-helper-never-indent nil
  "*If not nil, the indentation code for html-helper is turned off.")

(defvar html-helper-font-lock-keywords nil
  "Data for font lock (used to set font-lock-keywords)")

(defvar font-lock-no-comments nil
  "Free variable used by XEmacs, here for sake of clean compilation")

(defvar html-helper-bold-face nil
  "Mode created bold face (used for <b></b> HTML tags)")

(defvar html-helper-underline-face nil
  "Mode created underline face (used for <u></u> HTML tags)")

(defvar html-tag-face nil
  "Mode created face for HTML tags")

(defvar font-lock-preprocessor-face nil
  "Emacs considers a free variable this XEmacs face... here for sake of clean compilation")

(defvar html-helper-count2 0
  "Counter for the ticks showed during fontification")

(defvar html-helper-script-toggle-key [f4]
  "Key to activate the smart switch to the appropriate scripting mode (either server or client)")

(defvar html-helper-mode-function-pointer nil
  "Reference to the mode function to use in the buffer")

;; hooks (see also tempo.el)

;; On prompts... Charles Curley (http://w3.trib.com/~ccurley/): It took
;; some time to figure this out... The (p "prompt: ") and (r "prompt:
;; ") entries indicate where the prompting mode should prompt for a
;; field in the tag. (p ) indicates a parameter, such as the color of a
;; <font> tag. (r ) indicates a region, where the text to be surrounded
;; by the tag should go, such as the text to be turned that color. The
;; difference is this: when prompting mode is turned off and the user
;; is surrounding a region with the tag, the (r ) (region) parameter
;; indicates where the surrounded region will go. The first (p )
;; (parameter) is where the cursor will go, ready to input the first
;; parameter to the tag.

;; So when you have prompting on, and use the font with color and size
;; tag, put the cursor where you want the modified text to go. Start
;; inserting the tag. You will be prompted for the color, the size, and
;; then the text to display that way. When you have prompting turned
;; off, and don't have a region blocked, insert the font tag, and the
;; cursor will be at the the first parameter. Then tab over to the
;; space between the two parts of the tag, and type in your text. If
;; you have region blocked, C-u followed by the tag will surround the
;; region with the tag. The blocked region goes into the (r )
;; parameter. Then the cursor is placed at the first (p ) location,
;; ready for you to type in a parameter, such as the color of the text.

(defvar html-helper-mode-hook nil
  "*Hook run when html-helper-mode is started.")

(defvar html-helper-load-hook nil
  "*Hook run when html-helper-mode is loaded.")

(defvar html-helper-timestamp-hook 'html-helper-default-insert-timestamp
  "*Hook called for timestamp insertion.
Override this for your own timestamp styles.")

;; strings you might want to change

(defvar html-helper-new-buffer-template
  '(html-helper-htmldtd-version
    "<html> <head>\n"
    "<title>" p "</title>\n</head>\n\n"
    "<body>\n"
    "<h1>" p "</h1>\n\n"
    p
    "\n\n<hr>\n"
    "<address>" html-helper-address-string "</address>\n"
    html-helper-timestamp-start
    html-helper-timestamp-end
    "\n</body> </html>\n")
  "*Template for new buffers.
Inserted by `html-helper-insert-new-buffer-strings' if
`html-helper-build-new-buffer-flag' is set to t")

(defvar html-helper-new-ASP-buffer-template
  '("<%@ LANGUAGE=\"" p "\" %>\n"
    "<html> <head>\n"
    "<%\n\n%>\n"
    "</head><body>\n"
    "<% '<!-- " html-helper-timestamp-start "  " html-helper-timestamp-end
    " --> %>\n"
    "\n</body></html>\n")
"*Template for new ASP buffers.
Inserted by `html-helper-insert-new-ASP-buffer-strings' if
`html-helper-build-new-buffer-flag' is set to t and
html-helper-mode-local-JSP-not-ASP-flag is nil")

(defvar html-helper-new-JSP-buffer-template
  '("<%@ page include=\"" p "\" session=\"\"%>\n"
    "<html> <head>\n"
    "<%\n\n%>\n"
    "</head><body>\n"
    "<%-- " html-helper-timestamp-start "  " html-helper-timestamp-end
    " --%>\n"
    "\n</body></html>\n")
"*Template for new JSP buffers.
Inserted by `html-helper-insert-new-ASP-buffer-strings' if
`html-helper-build-new-buffer-flag' is set to t and
html-helper-mode-local-JSP-not-ASP-flag is nil")

(defvar html-helper-new-PHP-buffer-template
  '("<html> <head>\n"
    "<? PHP\n\n?>"
    "</head><body>\n"
    "<? /* " html-helper-timestamp-start "\n\n"
    html-helper-timestamp-end
    " */ ?>\n"
    "\n</body></html>\n"
    )
"*Template for new PHP buffers.
Inserted by `html-helper-insert-new-PHP-buffer-strings' if
`html-helper-build-new-buffer-flag' is set to t")

(defvar html-helper-timestamp-start "<!-- hhmts start -->"
  "*Start delimiter for timestamps.
Everything between `html-helper-timestamp-start' and
`html-helper-timestamp-end' will be deleted and replaced with the output
of the functions `html-helper-timestamp-hook' if
`html-helper-do-write-file-hooks' is t")

(defvar html-helper-timestamp-end "<!-- hhmts end -->"
  "*End delimiter for timestamps.
Everything between `html-helper-timestamp-start' and
`html-helper-timestamp-end' will be deleted and replaced with the output
of the function `html-helper-insert-timestamp' if
`html-helper-do-write-file-hooks' is t")

;; control over what types of tags to load. By default, we load all the
;; ones we know of.

(defvar html-helper-types-to-install
  '(anchor list header logical phys textel entity image table head form script)
  "*List of tag types to install when html-helper-mode is first loaded.
If you want to not install some type of tag, override this variable.
Order is significant: menus go in this order.")

;;}}} end of defvars

;;}}} end of user variables

;;{{{ Prologue

(require 'tempo)			;essential part of html-helper-mode
(condition-case nil			;menu support, standard in emacs19
    (require 'auc-menu)			;add-on for XEmacs. *why* does this
  (error (require 'easymenu)))		;package have to have two names?

;; Wed Mar 28 09:55:29 2001 Saint
;;
;; Placing here (require 'psgml-html ) after a suggestion from Graham
;; Gough who explored the faces problem with XEmacs (many thanks)
(if (string-match "XEmacs\\|Lucid" (emacs-version))
      ;; This will initialize html-helper-bold-face
      (require 'psgml-html))

(require 'font-lock)
;;}}}

;;{{{ Italian keyboard bindings

;; Fri Sep 26 14:11:14 2003 Saint
;;
;; Removed the C-c(wovel) keybindings that violates rule. Mapped
;; wovels with accents to the corresponding character entities
(defun html-hleper-mode-add-accents-support ()
"Maps some iso-8859-15 characters to their HTML charachter entity.
 At this time threre's just the support for italia wovels"
  (cond (html-helper-mode-using-iso-8859-15-characters
	 (local-set-key [?\340] "&agrave;")
	 (local-set-key [?\362] "&ograve;")
	 (local-set-key [?\350] "&egrave;")
	 (local-set-key [?\351] "&eacute;")
	 (local-set-key [?\354] "&igrave;")
	 (local-set-key [?\371] "&ugrave;"))))
;;}}}

;;{{{ html-helper-mode-insert-agravesyntax-table and html-helper-mode-abbrev-table

;; emacs doesn't seem to be able to really handle SGML like syntax. In
;; particular, comments are a loss.
;; We do try this, though: give < and > matching semantics

(defvar html-helper-mode-syntax-table nil
  "Syntax table for html-helper.")

(if html-helper-mode-syntax-table
    ()
  (setq html-helper-mode-syntax-table (make-syntax-table text-mode-syntax-table))
  (modify-syntax-entry ?<  "(>  " html-helper-mode-syntax-table)
  (modify-syntax-entry ?>  ")<  " html-helper-mode-syntax-table)
  (modify-syntax-entry ?\" "\"   " html-helper-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " html-helper-mode-syntax-table)
  (modify-syntax-entry ?'  ".   " html-helper-mode-syntax-table))

(defvar html-helper-mode-abbrev-table nil
  "Abbrev table used while in html-helper-mode.")
(define-abbrev-table 'html-helper-mode-abbrev-table ())

;;}}}

;;{{{ type based keymap and menu variable and function setup

;; Our basic keymap.
(defvar html-helper-mode-map (make-sparse-keymap)
  "Keymap for html-helper")
(defvar html-helper-mode-menu nil
  "Menu for html-helper. Clobbered and rebuilt by `html-helper-install-menu'")


;; html-helper-mode has a concept of "type" of tags. Each type is a
;; list of tags that all go together in one keymap and one menu.
;; Types can be added to the system after html-helper has been loaded,
;; briefly by doing html-helper-add-type-to-alist, then
;; html-helper-install-type, then html-helper-add-tag (for each tag)
;; then html-helper-rebuild-menu. See the mode documentation for more detail.

(defconst html-helper-type-alist nil
  "Alist: type of tag -> keymap, keybinding, menu, menu string.
Add to this with `html-helper-add-type-to-alist'.")

(defun html-helper-keymap-for (type)
  "Accessor function for alist: for type, return keymap or nil"
  (nth 0 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-key-for (type)
  "Accessor function for alist: for type, return keybinding or nil"
  (nth 1 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-menu-for (type)
  "Accessor function for alist: for type, return menu or nil"
  (nth 2 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-menu-string-for (type)
  "Accessor function for alist: for type, return menustring or nil"
  (nth 3 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-normalized-menu-for (type)
  "Helper function for building menus from submenus: add on string to menu."
  (cons (html-helper-menu-string-for type)
	(eval (html-helper-menu-for type))))

(defun html-helper-add-type-to-alist (type)
  "Add a type specification to the alist.
The spec goes (type . (keymap-symbol keyprefix menu-symbol menu-string)).
See code for an example."
  (setq html-helper-type-alist (cons type html-helper-type-alist)))

;; Here are the types provided by html-helper-mode.
(mapcar 'html-helper-add-type-to-alist
  '((entity  . (nil nil html-helper-entity-menu "Insert Character Entities"))
    (textel  . (nil nil html-helper-textel-menu "Insert Text Elements"))
    (head    . (html-helper-head-map "\C-c\C-h"
				     html-helper-head-menu
				     "Insert Structural Elements"))
    (header  . (html-helper-header-map "\C-c\M-h"
				       html-helper-header-menu
				       "Insert Headers"))
    (anchor  . (html-helper-anchor-map "\C-c\C-a"
				       html-helper-anchor-menu
				       "Insert Hyperlinks"))
    (logical . (html-helper-logical-map "\C-c\M-l"
					html-helper-logical-menu
					"Insert Logical Styles"))
    (phys    . (html-helper-phys-map "\C-c\C-p"
				     html-helper-phys-menu
				     "Insert Physical Styles"))
    (list    . (html-helper-list-map "\C-c\C-l"
				     html-helper-list-menu
				     "Insert List Elements"))
    (form    . (html-helper-form-map "\C-c\C-f"
				     html-helper-form-menu
				     "Insert Form Elements"))
    (image   . (html-helper-image-map "\C-c\C-i"
				      html-helper-image-menu
				      "Insert Inlined Images"))
    (table   . (html-helper-table-map "\C-c\C-t"
				      html-helper-table-menu
				      "Insert Tables"))
    (script  . (html-helper-script-map "\C-c\C-s"
				       html-helper-script-menu
				       "Insert Scripts"))
    ))


;; Once html-helper-mde is aware of a type, it can then install the
;; type: arrange for keybindings, menus, etc.

(defconst html-helper-installed-types nil
  "The types that have been installed (used when building menus).
There is no support for removing a type once it has been installed.")

;; For easy ASP/JSP switch
(defun html-helper-use-JSP-this-buffer ()
  (interactive)
  (setq html-helper-mode-local-JSP-not-ASP-flag t)
  (setq mode-name "HTML/JSP helper")
  (setq major-mode 'jsp-html-helper-mode))

(defun html-helper-use-ASP-this-buffer ()
  (interactive)
  (cond (html-helper-mode-uses-visual-basic
	 (setq html-helper-mode-local-JSP-not-ASP-flag nil)
	 (setq mode-name "HTML/ASP helper")
	 (setq major-mode 'asp-html-helper-mode))
	(t (error "Visual basic mode required for ASP"))))

(defun html-helper-install-type (type)
  "Install a new tag type: add it to the keymap, menu structures, etc.
For this to work, the type must first have been added to the list of types
with html-helper-add-type-to-alist."
  (setq html-helper-installed-types (cons type html-helper-installed-types))
  (let ((keymap (html-helper-keymap-for type))
	(key (html-helper-key-for type))
	(menu (html-helper-menu-for type))
	(menu-string (html-helper-menu-string-for type)))
    (and key
	 (progn
	   (set keymap nil)
	   (define-prefix-command keymap)))
    (and menu
	 (progn
	   (set menu nil)))))

;; install the default types.
(mapcar 'html-helper-install-type html-helper-types-to-install)

;; special mode keys
(mapcar
 (function (lambda (l) (define-key html-helper-mode-map (car l) (nth 1 l))))
 '(("\M-\C-f" tempo-forward-mark)
   ("\M-\C-b" tempo-backward-mark)
   ("\M-\t"   tempo-complete-tag)))

;; Extra commands that HTML helper supports that aren't insertions
(defvar html-helper-mode-functions-map nil
  "Keymap for extra HTML mode functions")

(define-prefix-command 'html-helper-mode-functions-map)
(define-key html-helper-mode-map "\C-c\C-z"
  'html-helper-mode-functions-map)
(define-key html-helper-mode-functions-map "t"
  'html-helper-insert-timestamp-delimiter-at-point)
(define-key html-helper-mode-functions-map "a"
  'html-script-narrow-to-asp)
(define-key html-helper-mode-functions-map "p"
  'html-script-narrow-to-php)
(define-key html-helper-mode-functions-map "b"
  'html-script-narrow-to-vbscript)
(define-key html-helper-mode-functions-map "j"
  'html-script-narrow-to-javascript)
(define-key html-helper-mode-functions-map "c"
  'html-script-narrow-to-css)

;; indentation keys - only rebind these if the user wants indentation
(if html-helper-never-indent
    ()
  (define-key html-helper-mode-map "\t" 'html-helper-indent-command)
  (define-key html-helper-mode-map "\C-m" 'newline-and-indent))

;; browse url stuff
(if (fboundp 'browse-url-of-file)
    (define-key html-helper-mode-functions-map "v" 'browse-url-of-file))

;; Fri Jan 12 18:30:32 2001 Saint
;;
;; Jack Vinson supplies this code to handle the case when
;; browse-url-browser-function is a list and not a function (it can be
;; a list, says its documentation)
;;
;; Replacing :
;; (if (and (boundp 'browse-url-browser-function)
;; 	 (fboundp browse-url-browser-function))
;;     (define-key html-helper-mode-functions-map "u"
;;       browse-url-browser-function))

(if (boundp 'browse-url-browser-function)
    (let ((bf browse-url-browser-function)
	  re)
      (while (consp bf)
	(setq re (car (car bf))
	      bf (if (string-match re "http")
		     (cdr (car bf))	; The function
		   (cdr bf))))		; More pairs
      (or bf (error "No browser in browse-url-browser-function for =
general URL's"))
      (fboundp bf)
      (define-key html-helper-mode-functions-map "u" bf)
      ))

;;}}}

;;{{{ html-helper-add-tag function for building basic tags

(defvar html-helper-tempo-tags nil
  "List of tags used in completion.")

;; this while loop is awfully Cish
;; isn't there an emacs lisp function to do this?
(defun html-helper-string-to-symbol (input-string)
  "Given a string, downcase it and replace spaces with -.
We use this to turn menu entries into good symbols for functions.
It's not entirely successful, but fortunately emacs lisp is forgiving."
  (let* ((s (copy-sequence input-string))
	 (l (1- (length s))))
    (while (> l 0)
      (if (char-equal (aref s l) ?\ )
	  (aset s l ?\-))
      (setq l (1- l)))
    (concat "html-" (downcase s))))


(defun html-helper-add-tag (l)
  "Add a new tag to html-helper-mode.
Builds a tempo-template for the tag and puts it into the
appropriate keymap if a key is requested. Format:
`(html-helper-add-tag '(type keybinding completion-tag menu-name template doc)'"
  (let* ((type (car l))
	 (keymap (html-helper-keymap-for type))
	 (menu (html-helper-menu-for type))
	 (key (nth 1 l))
	 (completer (nth 2 l))
	 (name (nth 3 l))
	 (tag (nth 4 l))
	 (doc (nth 5 l))
	 (command (if (string-equal completer "function")
		      (nth 4 l)
		      (tempo-define-template (html-helper-string-to-symbol name)
					 tag completer doc
					 'html-helper-tempo-tags))))

    (if (null (memq type html-helper-installed-types))    ;type loaded?
	t                                                 ;no, do nothing.
      (if (stringp key)			                  ;bind key somewhere?
	  (if keymap			                  ;special keymap?
	      (define-key (eval keymap) key command)      ;t:   bind to prefix
	    (define-key html-helper-mode-map key command));nil: bind to global
	t)
      (if menu				                  ;is there a menu?
	  (set menu			                  ;good, cons it in
	       (cons (vector name command t) (eval menu))))
      )))

;; for backwards compatability
(fset 'html-helper-add-cookie 'html-helper-add-tag)

;;}}}

;;{{{ most of the HTML tags


(defun html-helper-insert-or-wipe (string)
  "Propmts for the value of an optional attribute named STRING and
inserts it in the current buffer. Inserts nothing if the users replies
with a null string."
  (cond ((and (stringp string)
	      tempo-interactive)
	 (let ((val (read-from-minibuffer (concat string " :"))))
	   (cond ((> (string-width val) 0)
		  (insert-string (concat " " string "=\"" val "\"" )))
	  )))
	;; just to tell that there's something weird in the calling
	;; code... But behaves like a no op if tempo-interactive is
	;; nil
	(tempo-interactive
	 (error (concat "Wrong type argument: stringp, " string)))
	;; Wed Mar 28 10:06:24 2001 Saint
	;;
 	;; To insert empty attributes if
	;; html-helper-mode-insert-attributes-always is non nil
	((and (stringp string)
	      html-helper-mode-insert-attributes-always)
	 (insert-string (concat " " string "=\"\"" )))
	      ))

;; These tags are an attempt to be HTML/2.0 compliant, with the exception
;; of container <p>, <li>, <dd>, <dt> (we adopt 3.0 behaviour).
;; For reference see <URL:http://www.w3.org/hypertext/WWW/MarkUp/MarkUp.html>

;; order here is significant: within a tag type, menus and mode help
;; go in the reverse order of what you see here. Sorry about that, it's
;; not easy to fix.

(mapcar
 'html-helper-add-tag
 '(
   ;;entities
   (entity  "\C-c#"   "&#"              "Ascii Code"      ("&#" (r "Ascii: ") ";"))
   (entity  "\C-c\""  "&quot;"          "Quotation mark"  ("&quot;"))
   (entity  "\C-c$"   "&reg;"           "Registered"      ("&reg;"))
   (entity  "\C-c@"   "&copy;"          "Copyright"       ("&copy;"))
   (entity  "\C-c-"   "&shy;"           "Soft Hyphen"     ("&shy;"))
   (entity  "\C-c "   "&nbsp;"		"Nonbreaking Space"  ("&nbsp;"))
   (entity  "\C-c&"   "&amp;"		"Ampersand"	  ("&amp;"))
   (entity  "\C-c>"   "&gt;"	  	"Greater Than"       ("&gt;"))
   (entity  "\C-c<"   "&lt;"		"Less Than"	  ("&lt;"))

   ;; logical styles
   (logical "b"       "<blockquote>"	"Blockquote"
	    ("<blockquote>" (r "Quote: ") "</blockquote>"))
   (logical "c"       "<code>"		"Code"
	    ("<code>" (r "Code: ") "</code>"))
   (logical "x"       "<samp>"		"Sample"
	    ("<samp>" (r "Sample code") "</samp>"))
   (logical "r"       "<cite>"		"Citation"
	    ("<cite>" (r "Citation: ") "</cite>"))
   (logical "k"       "<kbd>"		"Keyboard Input"
	    ("<kbd>" (r "Keyboard: ") "</kbd>"))
   (logical "v"       "<var>"		"Variable"
	    ("<var>" (r "Variable: ") "</var>"))
   (logical "d"       "<dfn>"		"Definition"
	    ("<dfn>" (r "Definition: ") "</dfn>"))
   (logical "a"	      "<address>"	"Address"
	    ("<address>" r "</address>"))
   (logical "e"       "<em>"		"Emphasized"
	    ("<em>" (r "Text: ") "</em>"))
   (logical "s"       "<strong>"	"Strong"
	    ("<strong>" (r "Text: ") "</strong>"))
   (logical "p"       "<pre>"		"Preformatted"
	    ("<pre>" (r "Text: ") "</pre>"))

   ;;physical styles
   (phys    "s"       "<strike>"	"Strikethru"
	    ("<strike>" (r "Text: ") "</strike>"))
   (phys    "u"       "<u>"		"Underline"
	    ("<u>" (r "Text: ") "</u>"))
   (phys    "i"       "<i>"		"Italic"
	    ("<i>" (r "Text: ") "</i>"))
   (phys    "b"	      "<b>"    		"Bold"
	    ("<b>" (r "Text: ") "</b>"))
   (phys    "f"       "<tt>"		"Fixed"
	    ("<tt>" (r "Text: ") "</tt>"))
   (phys    "c"       "<center>"        "Center"
	    ("<center>" (r "Text: ") "</center>"))

;; html4.0 stuff, omitted

;    (phys    "5" "<span style=" "Spanning style"
; 	     ("<span style=\"" (p "style: ") "\">" 'r "</span>"))
;    (phys    "l" "<span class=" "Spanning class"
; 	     ("<span class=\"" (p "class: ") "\">" 'r "</span>"))


   ;;headers
   (header  "6"       "<h6>"		"Header 6"
	    ("<h6>" (r "Header: ") "</h6>"))
   (header  "5"       "<h5>"		"Header 5"
	    ("<h5>" (r "Header: ") "</h5>"))
   (header  "4"       "<h4>"		"Header 4"
	    ("<h4>" (r "Header: ") "</h4>"))
   (header  "3"       "<h3>"		"Header 3"
	    ("<h3>" (r "Header: ") "</h3>"))
   (header  "2"       "<h2>"		"Header 2"
	    ("<h2>" (r "Header: ") "</h2>"))
   (header  "1"	      "<h1>"     	"Header 1"
	    ("<h1>" (r "Header: ") "</h1>"))

   ;; forms
   (form    "o"       "<option>"        "Option"
	    (& "<option>" > ))
   (form    "v"       "<option value"   "Option with Value"
	    (& "<option value=\"" (r "Value: ") "\">" >))
   (form    "s"       "<select"		"Selections"
	    ("<select"
	     (html-helper-insert-or-wipe "name") ">\n<option>" > "\n</select>")"<select")
   (form    "z"	      "<input"		"Reset Form"
	    ("<input type=\"RESET\""
	     (html-helper-insert-or-wipe "value") ">"))
   (form    "m"	      "<input"		"Submit Form"
	    ("<input type=\"SUBMIT\""
	     (html-helper-insert-or-wipe "value") ">"))
   (form    "b"       "<input"          "Button"
	    ("<input type=\"BUTTON\""
	     (html-helper-insert-or-wipe "value")
	     (html-helper-insert-or-wipe "name") ">"))
   (form    "i"	      "<input"		"Image Field"
	    ("<input type=\"IMAGE\""
	     (html-helper-insert-or-wipe "Name")
	     (html-helper-insert-or-wipe "src") ">"))
   (form    "h"       "<input"          "Hidden Field"
	    ("<input type=\"HIDDEN\""
	     (html-helper-insert-or-wipe "name")
	     (html-helper-insert-or-wipe "value") ">"))
   (form    "p"	      "<textarea"	"Text Area"
	    ("<textarea"
	     (html-helper-insert-or-wipe "name")
	     (html-helper-insert-or-wipe "rows")
	     (html-helper-insert-or-wipe "cols") ">" r "</textarea>"))
   (form    "c"	      "<input"		"Checkbox"
	    ("<input type=\"CHECKBOX\""
	     (html-helper-insert-or-wipe "name") ">"))
   (form    "r"	      "<input"		"Radiobutton"
	    ("<input type=\"RADIO\""
	     (html-helper-insert-or-wipe "Name") ">"))
   (form    "t"	      "<input"		"Text Field"
	    ("<input type=\"TEXT\""
	     (html-helper-insert-or-wipe "name")
	     (html-helper-insert-or-wipe "size") ">"))
   (form    "f"	      "<form"           "Form"
	    ("<form"
	     (html-helper-insert-or-wipe "action")
	     (html-helper-insert-or-wipe "method") ">\n</form>\n"))

   ;;lists
   (list    "t"       "<dt>"            "Definition Item"
	    (& "<dt>" > (r "Term: ") "\n<dd>" >
	       (r "Definition: ")))
   (list    "l"       "<li>"            "List Item"
	    (& "<li>" > (r "Item: ") > "</li>"))
   (list    "r"	      "<dir>"		"DirectoryList"
	    (& "<dir>" > "\n<li>" > (r "Item: ") "\n</li>" > "\n</dir>" >))
   (list    "m"	      "<menu>"		"Menu List"
	    (& "<menu>" > "\n<li>" > (r "Item: ") "\n</li>" > "\n</menu>" >))
   (list    "o"	      "<ol>"		"Ordered List"
	    (& "<ol>" > "\n<li>" > (r "Item: ") "\n</li>" > "\n</ol>" >))
   (list    "d"	      "<dl>"		"Definition List"
	    (& "<dl>" > "\n<dt>" >
	       (p "Term: ") "\n<dd>" >
	       (r "Definition: ") "\n</dl>" >))
   (list    "u"	      "<ul>"		"Unordered List"
	    (& "<ul>" > "\n<li>" > (r "Item: ") "\n</li>\n</ul>" >))

   ;;anchors
   (anchor  "n"	      "<a name="	"Link Target"
	    ("<a " (html-helper-insert-or-wipe "name") ">"
	     (r "Anchor text: ") "</a>"))
   (anchor  "l"	      "<a href="        "Hyperlink"
	    ("<a href=\"" (p "href: ") "\" >"
	     (r "Anchor text: ") "</a>"))

   ;;graphics
;    (image   "a"       nil               "Aligned Image"
; 	    ("<img align=\""
; 	     (r "Alignment: ") "\" src=\"" (r "Image URL: ") "\">"))
;    (image   "i"       "<img src="	"Image"
; 	    ("<img src=\""
; 	     (r "Image URL: ") "\">"))
;    (image   "e"       "<img align="     "Aligned Image With Alt. Text"
; 	    ("<img align=\""
; 	     (r "Alignment: ") "\" src=\""
; 	     (r "Image URL: ") "\" alt=\""
; 	     (r "Text URL: ") "\">"))
;    (image   "t"       "<img alt="	"Image With Alternate Text"
; 	    ("<img alt=\""
; 	     (r "Text URL: ") "\" src=\""
; 	     (r "Image URL: ") "\">"))
;; New, (almost) all including, single menu item entry
;; src has to be there!
   (image	"a"	nil	"Image"
		("<img src=\""
		 (p "src" ) "\" "
		 (html-helper-insert-or-wipe  "alt" )
		 (html-helper-insert-or-wipe  "height" )
		 (html-helper-insert-or-wipe  "width" )
		 (html-helper-insert-or-wipe  "align" )
		 ">"))
   ;; table
   (table   "t"       "<table>"         "Table"
	    ("<table"
	     (html-helper-insert-or-wipe  "border" )
	     (html-helper-insert-or-wipe "width" )">\n</table>"))
   (table   "r"       "<tr>"         "Table Row"
	    ("<TR>\n</TR>"))
   (table   "h"       "<th>"         "Table Header"
	    ("<TH"
	     (html-helper-insert-or-wipe "rowspan" )
	     (html-helper-insert-or-wipe "colspan")">\n</TH>"))
   (table   "d"       "<td>"         "Table Data"
	    ("<TD"
	     (html-helper-insert-or-wipe "align" )
	     (html-helper-insert-or-wipe "colspan")"></TD>"))
   (table   "p"	"<caption>"	"html table caption"
	    ("<caption>" (r . "Table: ") "</caption>"))
   ;;text elements
   (textel  "\C-c="    nil		"Horizontal Line"
	    (& "<hr>\n"))
   (textel  "\C-c\C-m" nil		"Line Break"
	    ("<br>\n"))
   (textel  "\e\C-m"  nil		"Paragraph"
	    ("<p>"
	     (r "Text: ") "</p>"))

   ;;head elements
   (head    "H"       "<head>"          "Head"
	    ("<head>\n" "</head>\n"))
   (head    "B"       "<body>"          "Body"
	    ("<body>\n" "</body>\n"))
   (head    "i"	      "<isindex>"	"Isindex"
	    ("<isindex>\n"))
   (head    "n"	      "<nextid>"	"Nextid"
	    ("<nextid>\n"))
   (head    "h"       "<meta http-equiv=" "HTTP Equivalent"
	    ("<meta"
	     (html-helper-insert-or-wipe "http-equiv") " content=\""
	     (r "Content: ") "\">\n"))
   (head    "m"       "<meta name="     "Meta Name"
	    ("<meta"
	     (html-helper-insert-or-wipe "name") " content=\""
	     (r "Content: ") "\">\n"))
   (head    "l"	      "<link"		"Link"
	    ("<link href=\"" p "\">"))
   (head    "b"       "<base"		"Base"
	    ("<base href=\"" r "\">"))
   (head    "t"	      "<title>"		"Title"
	    ("<title>" (r "Document title: ") "</title>"))
   ;; scripting elements
   (script  "j"    "<SCRIPT>"       "JavaScript"
	    ("<SCRIPT TYPE=\"text/javascript\">\n"
	     (r "Script") "</SCRIPT>"))
   (script  "v"    "<SCRIPT>"       "VBScript"
	    ("<SCRIPT TYPE=\"text/vbscript\">\n"
	     (r "Script") "</SCRIPT>"))
   (script  "%"    "<%="            "ASP output"
	    ("<%="(p " Variabile: ")"%>"))
   (script  "a"    "<%xx%>"         "JSP/ASP code"
	    ("<%\n"(r "Code: " )"\n%>"))
   (script  "<"    "<%xx%>"         "JSP/ASP break"
	    ("%>\n"(r "Code: " )"\n<%"))
   (script  "="    "<?="            "PHP output"
	    ("<?="(p " Variabile: ")"?>"))
   (script  "p"    "<?xx?>"         "PHP code"
	    ("<? PHP\n"(r "Code: " )"\n?>"))
   (script  "?"    "<?xx?>"         "PHP break"
	    ("?>\n"(r " Code: " )"\n<? PHP"))
   ))

;;}}}

;;{{{ html-helper-smart-insert-item

;; there are two different kinds of items in HTML - those in regular
;; lists <li> and those in dictionaries <dt>..<dd>
;; This command will insert the appropriate one depending on context.

(defun html-helper-smart-insert-item (&optional arg)
  "Insert a new item, either in a regular list or a dictionary."
  (interactive "*P")
  (let ((case-fold-search t))
    (if
        (save-excursion
          (re-search-backward "<li>\\|<dt>\\|<ul>\\|<ol>\\|<dd>\\|<menu>\\|<dir>\\|<dl>" nil t)
          (looking-at "<dt>\\|<dl>\\|<dd>"))
        (tempo-template-html-definition-item arg)
      (tempo-template-html-list-item arg))))

;; special keybindings in the prefix maps (not in the list of tags)
(and (boundp 'html-helper-list-map)
     (define-key html-helper-list-map "i" 'html-helper-smart-insert-item))

;; and, special menu bindings
(and (boundp 'html-helper-list-menu)
     (setq html-helper-list-menu
	   (cons '["List Item" html-helper-smart-insert-item t] html-helper-list-menu)))

;;}}}

;;{{{ menu support

;; menus are built for easymenu. html-helper-add-tag builds
;; submenus based on tag type, the expert menu code lumps them
;; together into one list and calls easy-menu-define

(defvar html-helper-novice-menu
  '("HTML"
    ["Insert Paragraph" tempo-template-html-paragraph t]
    ["Insert Hyperlink" tempo-template-html-hyperlink t]
    ["Insert Big Header" tempo-template-html-header-2 t]
    ["Insert Unordered List" tempo-template-html-unordered-list t]
    ["Insert List Item" html-helper-smart-insert-item t]
    ["Insert Inlined Image" tempo-template-html-image-with-alternate-text t]
    ["Turn on Expert Menu" html-helper-toggle-expert-menu t])
  "Menu for novices, only installed if `html-helper-use-expert-menu is nil'")

(defun html-helper-menu nil
  "Return the proper menu. Looks at `html-helper-use-expert-menu'"
  (if html-helper-use-expert-menu
      (html-helper-expert-menu)
    html-helper-novice-menu))

(defun html-helper-rebuild-menu nil
  "Rebuild and install the HTML menu (using `easy-menu-define').
If `html-helper-use-expert-menu' is nil, then just use a novice menu."
  (let ((menu (html-helper-menu)))
    (easy-menu-remove menu)
    (easy-menu-define html-helper-mode-menu-symbol
		      html-helper-mode-map "HTML menus" menu)
    (easy-menu-add menu html-helper-mode-map)))

(defun html-helper-toggle-expert-menu (&optional arg)
  "Toggle full HTML menus. Optional arg acts like minor-mode args."
  (interactive "P")
  (setq html-helper-use-expert-menu
	(if (null arg) (not html-helper-use-expert-menu)
	  (> (prefix-numeric-value arg) 0)))
  (html-helper-rebuild-menu))

;; If browse-url loaded, add this in the novice menu.
(if (fboundp 'browse-url-of-file)
    (setq html-helper-novice-menu
	  (append html-helper-novice-menu
		  (list ["Load This Buffer in Browser" browse-url-of-file t]))))

;; Narrrowing to scripts, this don't use tempo because has to call functions
;; and not insert templates


;; Expert menus: consed up out of html-helper-installed-types
(defun html-helper-expert-menu ()
  "This menu is based on the current value of `html-helper-installed-types'.
This function can be called again, it redoes the entire menu."
  ;; first, reset this so we can call this again and again.
  (setq html-helper-mode-menu nil)

  ;; Cons in the toggle of the menu
  (setq html-helper-mode-menu
	(cons '["Turn on Novice Menu"
		html-helper-toggle-expert-menu t]
	      html-helper-mode-menu))

  ;; Now add in user-provided menu stuff
  (setq html-helper-mode-menu
	(append html-helper-user-menu html-helper-mode-menu))

  ;; Now cons in the browse-url functions
  (if (fboundp 'browse-url-of-file)
    (setq html-helper-mode-menu
	  (cons '["Load this Buffer in Browser" browse-url-of-file t]
		html-helper-mode-menu)))

  ;; Mon Jan 15 07:49:39 2001 Saint
  ;;
  ;; Jack Vinson supplied this code to handle the case where
  ;; browse-url-browser-function is a list, too
  (if (boundp 'browse-url-browser-function)
      (let ((bf browse-url-browser-function)
	    re)
	(while (consp bf)
	  (setq re (car (car bf))
		bf (if (string-match re "http")
		       (cdr (car bf))	; The function
		     (cdr bf))))		; More pairs
	(or bf
	    (error
	     "No browser in browse-url-browser-function for general URL's"))
	(fboundp bf)
	(setq html-helper-mode-menu
	      (cons (vector "Browse URL at point" bf t)
		    html-helper-mode-menu))))

  ;; cons in the timestamp delimiters
  (setq html-helper-mode-menu
	(cons '["Insert Timestamp Delimiter"
		html-helper-insert-timestamp-delimiter-at-point t]
	      html-helper-mode-menu))

  ;; cons script narrowing
  (setq html-helper-mode-menu
	(append html-helper-mode-menu
		(list ["Narrow to ASP" html-script-narrow-to-asp t])))
  (setq html-helper-mode-menu
	(append html-helper-mode-menu
		(list ["Narrow to PHP" html-script-narrow-to-php t])))
  (setq html-helper-mode-menu
	(append html-helper-mode-menu
		(list ["Narrow to VBScript" html-script-narrow-to-vbscript t])))
   (setq html-helper-mode-menu
	 (append html-helper-mode-menu
		 (list ["Narrow to JavaScript" html-script-narrow-to-javascript t])))
   (setq html-helper-mode-menu
	 (append html-helper-mode-menu
		 (list ["Narrow to CSS" html-script-narrow-to-css t])))
   (setq html-helper-mode-menu
	 (append html-helper-mode-menu
		 (list ["Use ASP" html-helper-use-ASP-this-buffer  t])))

   (setq html-helper-mode-menu
	 (append html-helper-mode-menu
		 (list ["Use JSP" html-helper-use-JSP-this-buffer t])))

  ;; now cons up the main menu out of the submenus
  (mapcar
   (function (lambda (type)
	       (setq html-helper-mode-menu
		     (cons (html-helper-normalized-menu-for type)
			   html-helper-mode-menu))))
	  html-helper-installed-types)

  ;; now tack on our name
  (setq html-helper-mode-menu (cons "HTML" html-helper-mode-menu))
  html-helper-mode-menu)

(html-helper-rebuild-menu)

;;}}}

;;{{{ context guessing

;; guess where we are in indented lists based on the last list token.
;; it would be much better to try to match </ul> to <ul>, and </ol> to <ol>
;; etc, but that is pretty unwieldy and slow.
;; Note, we make select/option look like a list structure too, so indentation
;; works. This is a bit weird, but it's ok.

;; Wed Jan 10 09:01:06 2001 Saint
;;
;; Changed regexps to handle tags with attributes
(defvar html-helper-any-list-item-start
  ;;  "<li>\\|<dt>\\|<dd>\\|<option\\|<th>\\|<td>")
  "<li\W\\|<dt\\|<dd\\|<option\\|<th\\|<td\\|<tbody\\|<div\\|<tfoot")
(defvar html-helper-any-list-item-end "</li>\\|</dt>\\|</dd>\\|</th>\\|</td\\|</tbody>\\|</div>\\|</tfoot")
(defvar html-helper-any-list-start
  ;;  "<dl>\\|<ul>\\|<ol>\\|<menu>\\|<dir>\\|<select\\|<table\\|<tr>")
  "<dl\\|<ul\\|<ol\\|<menu\\|<dir\\|<select\\|<table\\|<tr")
(defvar html-helper-any-list-end "</dl>\\|</ul>\\|</ol>\\|</menu>\\|</dir>\\|</select>\\|</table>\\|</tr>")
(defvar html-helper-any-list
  (format "\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)"
	  html-helper-any-list-start
	  html-helper-any-list-end
	  html-helper-any-list-item-start
	  html-helper-any-list-item-end))
;; Wed Jan 10 09:50:53 2001 Saint
;;
;; New indentation. As for other modes leave a single indentation
;; sensible tag on each line.
(defvar html-helper-indentation-list html-helper-any-list)
  ;;  (format "\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)"
  ;;	  html-helper-any-list-start
  ;;	  html-helper-any-list-end
  ;;	  html-helper-any-list-item-start))
(defvar html-helper-search-limit 2000 "limit on how far back we search")

(defun html-helper-context-symbol ()
  "Return the symbol the last match (against `html-helper-any-list') found."
  (cond ((match-beginning 1) 'list-start)
	((match-beginning 2) 'list-end)
	((match-beginning 3) 'item-start)
	((match-beginning 4) 'item-end)
	(t 'error)))

; Wed Jan 10 09:53:46 2001 Saint
;
; Doesn't ignore item-end any more.
(defun html-helper-guess-prev-context ()
  "Figure out the last list-type tag before point relevant to indentation.
Returns 'item-start if the last list tag is a list item start
        'list-start if the last list tag is the start of a list
        'item-end   if the last list tag is the end of a list item
        'list-end   if the last list tag is the end of a list."
  (save-excursion
    (let* ((lim (max (point-min) (- (point) html-helper-search-limit)))
	   (context (if (re-search-backward html-helper-indentation-list lim t)
			(html-helper-context-symbol)
		      nil)))
      (cons context (current-indentation)))))

(defun html-helper-print-prev-context ()
  (interactive)
  (message "%s" (html-helper-guess-prev-context)))

;;}}}

;;{{{ indentation
(defvar html-helper-buffers nil "buffers using html-helper-mode alist")

(defvar html-helper-print-indent-info nil
  "If t, indent will print out information as a message.")

(defun html-helper-indent-command ()
  "Command for indenting HTML to the appropriate column.
Calls `html-helper-indent' which tries to examine how many levels down
in nested lists we are and does the appropriate indentation.'
See also `html-helper-basic-offset' and `html-helper-never-indent'."
  (interactive)
  (html-helper-indent))

;; some ideas borrowed from cc-mode.el.
;; Basic logic:
;;   if this line is some sort of list token, indent according to prev context:
;;     if previous context was a list-end or item-start, use its indentation
;;     if previous context was a list start, indent forward basic-offset
;;     ignore previous list-ends, their indentation is unreliable.
;;     then if this is some sort of list-item, do special case fixups:
;;       if this is a item start or end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and prev *not* a list end, go back basic-offset
;;   else if this line is not a list item, and previous line is a item-start
;;     indent continue-indent, because this is part of the item

;; code to never indent <PRE></PRE> sections. Many thanks to
;; Stan Lanning <lanning@pobox.com>
(defun html-helper-indent-leave-alone-p ()
  (let* ((pre (save-excursion (search-backward "<pre>" nil t)))
	 (endpre (save-excursion (search-backward "</pre>" pre t))))
    (and pre (null endpre))))

(defadvice html-helper-indent (around leave-pre-alone activate)
  (cond ((not (html-helper-indent-leave-alone-p))
	 ad-do-it)
	(html-helper-print-indent-info
	 (message "In <pre> -- skipping indentation"))
	(t nil)))

(defun html-helper-indent ()
  "Indentation workhorse function."
  (if html-helper-never-indent
      ()
    (let ((m (point-marker))
	  (bol (progn (beginning-of-line) (point))))

      ;; unindent the line
      (delete-region (point) (progn (back-to-indentation) (point)))

      (let* ((where (html-helper-guess-prev-context))
	     (prev-context (car where))
	     (this-context nil)
	     (previ (cdr where))
	     (newi (cond
		    ((eq prev-context 'list-end) previ)
		    ((eq prev-context 'item-start) previ)
		    ((eq prev-context 'list-start) (+ previ html-helper-basic-offset))
		    (t previ))))

	;; newi is set to the basic indentation, now adjust indentation
	;; based on what the current line is.
	(if (looking-at html-helper-any-list)
	    (progn
	      (setq this-context (html-helper-context-symbol))
	      (cond
	       ;; item start or end and last line was a list-end: go backwards
	       ((and
		 (or (eq this-context 'item-start) (eq this-context 'item-end))
		 (eq prev-context 'list-end))
		(setq newi (- newi html-helper-basic-offset)))

	       ;; end of list and last line was an end: go backwards twice
	       ((and (eq this-context 'list-end) (eq prev-context 'list-end))
		;; Wed Jan 10 09:35:53 2001 Saint
		;;
		;; fixing indentation
		;;		(setq newi (- newi html-helper-basic-offset html-helper-item-continue-indent)))
		(setq newi (- newi html-helper-basic-offset)))

	       ;; Any other end of list? Indent negative
	       ((and (eq this-context 'list-end))
		(setq newi (- newi html-helper-basic-offset)))

	       ;; start of list and last line beginning of item, go forwards
	       ((and (eq this-context 'list-start)
		     (eq prev-context 'item-start))
		(setq newi (+ newi html-helper-basic-offset)))))

	  ;; default: no special case, indent forward for text
	  (cond
	   ;; last line an item? Beginning of continued item - go forward
	   ((eq prev-context 'item-start)
	    (setq newi (+ newi html-helper-basic-offset)))))

	(if html-helper-print-indent-info
	    (message
	     "Last Context: %s, This Context: %s, Previous: %s New: %s"
	     prev-context this-context previ newi))

	;; just in case
	(if (< newi 0)
	    (setq newi 0))
	(indent-to newi newi)

	;; adjust point to where it was before, or at start of indentation
	(goto-char (marker-position m))
	(if (< (current-column) (current-indentation))
	    (back-to-indentation))))))

;;}}}

;;{{{ completion finder for tempo

(defvar html-helper-completion-finder
  "\\(\\(<\\|&\\).*\\)\\="
  "Passed to tempo-use-tag-list, used to find tags to complete.")

;; The regexp finds everything between the last < or & and point,
;; which is good enough to match the tags HTML might complete.
;; emacs18 doesn't have the \= for regexps, though, so we do something
;; more hackish.

(defun html-helper-emacs18-completion-finder ()
  "Unfortunately emacs18 doesn't support \\= in regexps, so we do this hack.
If you have problems with it, maybe you should upgrade to emacs19 :-)"
  (let* ((where nil)
         (s (buffer-substring
             (point)
             (setq where (save-excursion
                           (re-search-backward "<\\|&" (min (point-min) 100) t)
                           (point))))))
    (cons s where)))

;;}}}

;;{{{ timestamps

(defun html-helper-update-timestamp ()
  "Basic function for updating timestamps.
It finds the timestamp in the buffer by looking for
`html-helper-timestamp-start', deletes all text up to
`html-helper-timestamp-end', and runs `html-helper-timestamp-hook' which
will should insert an appropriate timestamp in the buffer."
  (save-excursion
    (goto-char (point-max))
    (if (not (search-backward html-helper-timestamp-start nil t))
	(message "timestamp delimiter start was not found")
      (let ((ts-start (+ (point) (length html-helper-timestamp-start)))
	    (ts-end (if (search-forward html-helper-timestamp-end nil t)
			(- (point) (length html-helper-timestamp-end))
		      nil)))
	(if (not ts-end)
	    (message "timestamp delimiter end was not found. Type C-c C-t to insert one.")
	  (delete-region ts-start ts-end)
	  (goto-char ts-start)
	  (run-hooks 'html-helper-timestamp-hook)))))
  nil)

(defun html-helper-default-insert-timestamp ()
  "Default timestamp insertion function."
  (let ((time (current-time-string)))
    (insert "Last modified: "
	    (substring time 0 20)
	    (nth 1 (current-time-zone))
	    " "
	    (substring time -4)
	    " ")))

(defun html-helper-insert-timestamp-delimiter-at-point ()
  "Simple function that inserts timestamp delimiters at point.
Useful for adding timestamps to existing buffers."
  (interactive)
  (insert html-helper-timestamp-start)
  (insert html-helper-timestamp-end))

;;}}}

;;{{{ html-helper-insert-new-buffer-strings

(tempo-define-template "html-skeleton" html-helper-new-buffer-template
		       nil
		       "Insert a skeleton for a HTML document")

(tempo-define-template "ASP-skeleton" html-helper-new-ASP-buffer-template
		       nil
		       "Insert a skeleton for a ASP document")

(tempo-define-template "PHP-skeleton" html-helper-new-PHP-buffer-template
		       nil
		       "Insert a skeleton for a PHP document")

(tempo-define-template "JSP-skeleton" html-helper-new-JSP-buffer-template
		       nil
		       "Insert a skeleton for a JSP document")

(defun html-helper-insert-new-buffer-strings ()
  "Insert `html-helper-new-buffer-strings'."
  (tempo-template-html-skeleton))

(defun html-helper-insert-new-ASP-buffer-strings ()
  "Insert `html-helper-new-ASP-buffer-strings' or `html-helper-new-JSP-buffer-string'."
  (cond (html-helper-mode-local-JSP-not-ASP-flag
	 (tempo-template-JSP-skeleton))
	(t
	 (tempo-template-ASP-skeleton))))

(defun html-helper-insert-new-PHP-buffer-strings ()
  "Insert `html-helper-new-PHP-buffer-strings'."
  (tempo-template-PHP-skeleton))

;;}}}

;;{{{ new font lock stuff

(defvar html-helper-font-lock-syntactic-keywords
  `(("<\\([%?]\\|[a-zA-Z][^>]\\|[!/][a-zA-Z]\\|!--\\)" 1 "\<")))
;;    ("\\([%?a-zA-Z]\\-\\)>" 2 "\>")))


(defun html-helper-font-lock-unfontify-region (beg end)
  (font-lock-default-unfontify-region beg end)
  (while (< beg end)
    (let ((next (next-single-property-change beg 'display nil end))
	  (prop (get-text-property beg 'display)))
      (if (and (eq (car-safe prop) 'raise)
	       (member (car-safe (cdr prop)) '(-0.3 +0.3))
	       (null (cddr prop)))
	  (put-text-property beg next 'display nil))
      (setq beg next))))

(defun html-helper-font-lock-last-char-helper ()
  (when (eq (char-syntax (preceding-char)) ?/)
    (put-text-property (1- (point)) (point) 'syntax-table '(1)))
  (unless (eobp)
    (put-text-property (point) (1+ (point)) 'syntax-table '(12))))

(defun html-helper-skip-to-regexp (regexp)
  "Goes past the regexp or to point-max (used by
   html-helper-font-lock-syntactic-face-function"
  (if (re-search-forward regexp nil t)
      (backward-char 2)
    (goto-char (point-max))))

(defun html-helper-font-lock-syntactic-face-function (state)
  (let ((char (nth 3 state)))
    (cond
     ;; char è nil
     (char font-lock-string-face)
     (t
      (set 'char (char-before (point)))
      (cond ((string-match "[a-zA-Z/]" (char-to-string char))
	     ;; This is an HTML tag
;	     (put-text-property (- (point) 2) 'syntax-table '(11))
	     (save-excursion
	       (cond ((char-equal ?> (char-after (point)))
		      (put-text-property (point) (1+ (point)) 'syntax-table '(12)))
		     (t
		      (html-helper-skip-to-regexp "[^?%]>")
		      (html-helper-font-lock-last-char-helper))))
	     html-helper-tag-face)
	    ((string-match "[%?]"  (char-to-string char))
	     ;; This is a server script block
;	     (put-text-property (- (point) 2) 'syntax-table '(11))
	     (save-excursion
	       (html-helper-skip-to-regexp "[%?]>")
	       (html-helper-font-lock-last-char-helper))
	     html-helper-server-script-face)
	    ((char-equal ?! char)
	     ;; This is an HTML tag
	     (if (char-equal ?- (following-char))
;		 (put-text-property (- (point) 2) 'syntax-table '(11))
		 (progn
		   (save-excursion
		     (html-helper-skip-to-regexp "-->")
		     (html-helper-font-lock-last-char-helper))
		   font-lock-comment-face)
		 (progn
		   (save-excursion
		     (html-helper-skip-to-regexp "[^%?]>")
		     (html-helper-font-lock-last-char-helper))
		   html-helper-tag-face)))
	    (t
	     ;; This is a comment...
	     nil))))))

(defun html-helper-mark-sexp ()
  (interactive)
  (let ((here (point))
	(point-open (1+ (point)))
	(point-close (1- (point))))
    (if (not (= 0 (skip-chars-backward "^<")))
	(set 'point-open (1- (point))))
    (if (not (= 0 (skip-chars-forward "^>")))
	(set 'point-close (1+ (point))))
	(goto-char point-open)
    (if (and (<= point-open here)
	     (<= here point-close))
	(mark-defun)
      (push-mark here)
      (goto-char here))))

(defun html-helper-tag-beginning-position (&optional inizio)
  "finds the begin of a tag"
  (save-excursion
    (if (not (= 0 (+ (skip-chars-backward "^<")
		     (skip-chars-backward "<"))))
	(point)
      (line-beginning-position inizio))))

(defun html-helper-fontify-region (beg end &optional loudly)
  "Fontify a region, moving at the beginning of mark if necessary
This is a replica of the font-lock original with a change for the
line-beginning-position that's replaced with a custom tag oriented
function"
  (save-buffer-state
      ((parse-sexp-lookup-properties font-lock-syntactic-keywords)
       (old-syntax-table (syntax-table)))
    (unwind-protect
	(save-restriction
	  (widen)
	  ;; Use the fontification syntax table, if any.
	  (when font-lock-syntax-table
	    (set-syntax-table font-lock-syntax-table))
;; 	  ;; check to see if we should expand the beg/end area for
;; 	  ;; proper multiline matches
;; 	  (when (and font-lock-multiline
;; 		     (> beg (point-min))
;; 		     (get-text-property (1- beg) 'font-lock-multiline))
;; 	    ;; We are just after or in a multiline match.
;; 	    (setq beg (or (previous-single-property-change
;; 			   beg 'font-lock-multiline)
;; 			  (point-min)))
;; 	    (goto-char beg)
;; 	    (setq beg (html-helper-tag-beginning-position)))
;; 	  (when font-lock-multiline
;; 	    (setq end (or (text-property-any end (point-max)
;; 					     'font-lock-multiline nil)
;; 			  (point-max))))
	  (set 'font-lock-keywords html-helper-font-lock-keywords)
	  (setq beg (html-helper-tag-beginning-position))
	  (goto-char end)
	  (setq end (html-helper-tag-beginning-position 2))
	  ;; Now do the fontification.
	  (font-lock-unfontify-region beg end)
	  (set 'font-lock-keywords html-helper-font-lock-keywords)
	  (when font-lock-syntactic-keywords
	    (font-lock-fontify-syntactic-keywords-region beg end))
	  (unless font-lock-keywords-only
	    (font-lock-fontify-syntactically-region beg end loudly))
	  (font-lock-fontify-keywords-region beg end loudly)
      ;; Clean up.
      (set-syntax-table old-syntax-table)))))

;;}}}

;;{{{ html-helper-mode

;; New highlighting
(defvar html-helper-font-lock-keywords nil
  "Additional expressions to highlight in HTML helper mode.")

(unless html-helper-font-lock-keywords
  (set 'html-helper-font-lock-keywords  (list
   ;; Avoid use of `keep', since XEmacs will treat it the same as `t'.
   ;; First fontify the text of a HREF anchor.  It may be overridden later.
   ;; Anchors in headings will be made bold, for instance
   '("<a\\s-+href[^>]*>\\([^>]+\\)</a>"
     1 font-lock-warning-face t)
     ;; Underline is rarely used. Only handle it when no tags inside.
   '("<u>\\([^<]*\\)</u>" 1 html-helper-underline-face t)
   '("<b>\\([^<]*\\)</b>" 1 'html-helper-bold-face t)
     ;; Italic
   '("<i>\\([^<]*\\)</i>" 1 'html-helper-italic-face t)
     ;; w3 org says that a tag is <element-name> not < element-name>
     ;; I don't know of any non alphabetic HTML entity, if you know
     ;; about one, please drop me a mail
     ;;						Saint
   '("\\(</?[A-Za-z]+[^>]*>\\)" 1 html-helper-tag-face t)
     ;; SGML things like <!DOCTYPE ...> with possible <!ENTITY...> inside.
   '("<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>"
     0 font-lock-keyword-face t)
     ;; Paint [PA][HS]P skripts in font-lock-builtin-face,
     '("<[?%]=\\([^%?]\\|[?%][^>]\\)*[%?]>" 0 html-helper-builtin-face t t)
     ;; string stuff is pretty weird with asp. You can have strings
     ;; containing asp code containing strings and empty
     ;; strings. Replaced original [^\"] with this one...
   '("[=(&]?[ \t\n]*\\(\"[^\"\n]*<%[^\"\n]*\\(\"[^\"\n]*\"\\)[^\"\n]*%>[^\"\n]*\\)" 1 font-lock-string-face t)
   '("[=(&]?[ \t\n]*\\(\"[^\"\n]*\"\\)"  1 font-lock-string-face t)
     ;; after painting strings, you have to restore asp stuff inside strings
   '("\\(<%=\\w\\)" 1 html-helper-server-script-face t)
   '("\\(\")[^\"\n]*%>\\)" 1 html-helper-server-script-face t)
   '("\\(<%=[^%]*%>\\)" 1 html-helper-server-script-face t)
   '("\\(<\\?=\\w\\)" 1 html-helper-server-script-face t)
   '("\\(\")[^\"\n]*\\?>\\)" 1 html-helper-server-script-face t)
   '("\\(<\\?=[^%]*\\?>\\)" 1 html-helper-server-script-face t)
     ;; Comment declarations according to the HTML 2.0 spec at
     ;; <URL:http://www.w3.org/pub/WWW/MarkUp/html-spec/html-spec_3.html>.
     ;; Usually ` <!-- ... -->', but also e.g the single, complete declaration
     ;; ` <!--c1--  -- c2 -- -->c3 (still comment) ----c4- c4--   >'.
     ;; Note that e.g. Netscape 3.01 Gold doesn't fully live up to the spec.

     ;; That's krazy, strings higlight matches ) too, so i paint
     ;; parantheses...
   '("\\(<\\|\\s(\\)" 1 font-lock-function-name-face t)
   '("\\(\\s)\\|>\\)" 1 font-lock-function-name-face t)
   '("\\([\"]\\)" 1 font-lock-string-face t)
     ;; HTML special characters
   '("&[a-zA-Z0-9#]+;" 0 font-lock-warning-face t))))

(defun base-html-helper-mode (template-maker)
  "(base-html-helper-mode TEMPLATE-FUNCTION
basic mode engine, called by the exported/exposed modes.

TEMPLATE-FUNCTION is the function to use to insert the template of
a new document and can be nil. It's called onlu when
html-helper-build-new-buffer-flag is set to t"
  (kill-all-local-variables)

  (use-local-map html-helper-mode-map)
  (setq local-abbrev-table html-helper-mode-abbrev-table)
  (set-syntax-table html-helper-mode-syntax-table)
  ( html-hleper-mode-add-accents-support)
  (cond (html-helper-mode-run-the-mode
	 (make-local-variable 'comment-start)
	 (make-local-variable 'comment-end)
	 (make-local-variable 'comment-column)
	 (make-local-variable 'comment-start-skip)
	 (make-local-variable 'indent-line-function)
	 (make-local-variable 'html-helper-count)
	 (make-local-variable 'html-helper-mode-local-JSP-not-ASP-flag)
	 (make-variable-buffer-local 'html-helper-mode-run-the-mode)
	 (make-variable-buffer-local 'html-helper-mode-function-pointer)
	 (set 'html-helper-mode-run-the-mode nil)))
	 (set (make-local-variable 'font-lock-multiline) nil)


  ;; font-lock setup for various emacsen: XEmacs, Emacs 19.29+, Emacs <19.29.
  ;; By Ulrik Dickow <dickow@nbi.dk>.  (Last update: 05-Sep-1995).
  (cond	((string-match "XEmacs\\|Lucid" (emacs-version)) ; XEmacs/Lucid
	 (put major-mode 'font-lock-keywords-case-fold-search t)
	 )
	(t
	 (set (make-local-variable 'font-lock-defaults)
	      '((html-helper-font-lock-keywords)
		nil t nil nil
		;; Who ever uses that anyway ???
		(font-lock-mark-block-function . html-helper-mark-sexp)
		(font-lock-syntactic-face-function
		 . html-helper-font-lock-syntactic-face-function)
		(font-lock-unfontify-region-function
		 . html-helper-font-lock-unfontify-region)
		(font-lock-syntactic-keywords
		 . html-helper-font-lock-syntactic-keywords)
		(font-lock-fontify-region-function
		 . html-helper-fontify-region)
		(parse-sexp-lookup-properties . ((7) . ?>))))
	 (set (make-local-variable 'font-lock-keywords) html-helper-font-lock-keywords )))

;; 	 (make-local-variable 'font-lock-keywords-case-fold-search)
;; 	 (make-local-variable 'font-lock-keywords)
;; 	 (make-local-variable 'font-lock-no-comments)
;; 	 (setq font-lock-keywords-case-fold-search t)
;; 	 (setq font-lock-keywords html-helper-font-lock-keywords)
;; 	 (setq font-lock-no-comments t)))

  (setq comment-start "<!-- "
	comment-end " -->"
	comment-start-skip "<!--[ \t]*"
	comment-column 0
	indent-line-function 'html-helper-indent)

  (tempo-use-tag-list 'html-helper-tempo-tags html-helper-completion-finder)

  (if html-helper-do-write-file-hooks
      (add-hook 'write-contents-functions 'html-helper-update-timestamp))

  (if (and html-helper-build-new-buffer-flag (zerop (buffer-size)))
      (if template-maker
	  (funcall template-maker)))

  (easy-menu-add (html-helper-menu) html-helper-mode-map)

  (let ((major-mode 'html-helper-mode))
    (run-mode-hooks 'text-mode-hook)
    (run-mode-hooks 'html-mode-hook)
    ;; put keybindings here
    (run-mode-hooks 'html-helper-mode-hook)))

(cond (html-helper-mode-uses-visual-basic
       (defun asp-html-helper-mode ()
  "Mode for editing HTML documents with ASP server scripts.

The main function html-helper-mode provides a menu and keybindings
for the HTML tags one inserts when writing HTML documents. Selecting
the menu item or typing the key sequence for a command inserts the
corresponding tag and places point in the right place. If a prefix
argument is supplied, the tags is instead wrapped around the region.
Alternately, one can type in part of the tag and complete it with M-TAB.

There is also code for indentation, timestamps, skeletons for new
documents, and lots of other neat features.

Uses :`visual-basic-mode'  for ASP e VBSCript
      `easymenu' for menu creation
      `cc-mode'  for javascript support
      `tempo'    for templates
Supports server (actually ASP & PHP, JSP) and client
\(JavaScript, VBScript) scripting

Customizable flags you would like to alter

`html-helper-mode-uses-visual-basic' : non nil requires visual-basic-mode and activates VBScript support functions in ASP and client script
`html-helper-mode-uses-bold-italic' : non nil creates a bold italic face (could fail if there's not such face available)
`html-helper-mode-uses-KG-style' : nil to make Emacs consider PHP/JSP/ASP code blocks beginning in the first colum
`html-helper-mode-global-JSP-not-ASP-flag' : non nil to make Emacs consider <% %> sequence as JSP blocks by default in html-helper-mode. The local copy of this flag is set to nil in asp-html-helper-mode, it is set to t in jsp-html-helper-mode.
Alter the behaviour locally by changing html-helper-mode-local-JSP-not-ASP-flag value
Special command (not in menu - default bound to [f4]): attempts a smart narrowing to the current scripting block. Fails in client script containing server script sections.
\\{html-helper-mode-map}
Written by nelson@santafe.edu, http://www.santafe.edu/~nelson/
Mantained by lauri@eng.it, http:/www.gest.unipd.it/~saint/"
  (interactive)

  ;; Register this buffer as being handled by asp-html-helper-mode
  ;; this has to be done only once, therefore...

  (html-helper-add-buffer (current-buffer) 'asp-html-helper-mode-run)

  ;; the rest of the code is put in a separate function that can be
  ;; called several times.

  (asp-html-helper-mode-run)))
)

;;
(defun asp-html-helper-mode-run ()
  "Basic behaviour of the mode, to be called when returning from visual-basic-mode"
  (setq html-helper-mode-local-JSP-not-ASP-flag nil)
  (setq major-mode 'asp-html-helper-mode)
  (base-html-helper-mode 'html-helper-insert-new-buffer-strings)
  (setq mode-name "HTML/ASP helper")
  ;; Mon Jun 25 16:14:44 2001 Saint
  ;;
  ;; Supporto imenu (da cui discende il supporto Speedbar)
  (setq imenu-generic-expression
	'(( "VBA Variables" "\\([Dd][Ii][Mm][ \t]+\\)\\([a-z0-9]+\\)" 2)
	  ( "ASP Output" "^.*\\(<%=[ \t]*\\)\\([a-z0-9]+\\)" 2)
	  ( "ASP Block" "^.*\\(<%[^=][\t\n ]+\\)\\([a-z0-9]+\\)" 2)
	  ( "VBA Sub" "^.*\\([sS]ub[\t\n ]+\\)\\([a-z0-9]+\\)" 2)
	  ( "VBA Function" "^.*\\([fF]unciton[\t\n ]+\\)\\([a-z0-9]+\\)" 2)))
  (speedbar-add-supported-extension ".asp")
  ;;/Saint
  (setq major-mode 'asp-html-helper-mode))

(defun jsp-html-helper-mode ()
  "Mode for editing HTML documents with ASP server scripts.

The main function html-helper-mode provides a menu and keybindings
for the HTML tags one inserts when writing HTML documents. Selecting
the menu item or typing the key sequence for a command inserts the
corresponding tag and places point in the right place. If a prefix
argument is supplied, the tags is instead wrapped around the region.
Alternately, one can type in part of the tag and complete it with M-TAB.

There is also code for indentation, timestamps, skeletons for new
documents, and lots of other neat features.

Uses :`visual-basic-mode' for VBSCript
      `easymenu' for menu creation
      `cc-mode'  for javascript support
      `java-mode' or `JDE' for Java blocks
      `tempo'    for templates
Supports server (actually ASP & PHP, JSP) and client
\(JavaScript, VBScript) scripting

Customizable flags you would like to alter

`html-helper-mode-uses-visual-basic' : non nil requires visual-basic-mode and activates ASP and VBScript support functions
`html-helper-mode-uses-bold-italic' : non nil creates a bold italic face (could fail if there's not such face available)
`html-helper-mode-uses-KG-style' : nil to make Emacs consider PHP/JSP/ASP code blocks beginning in the first colum
`html-helper-mode-global-JSP-not-ASP-flag' : non nil to make Emacs consider <% %> sequence as JSP blocks by default in html-helper-mode. The local copy of this flag is set to nil in asp-html-helper-mode, it is set to t in jsp-html-helper-mode.
Alter the behaviour locally by changing html-helper-mode-local-JSP-not-ASP-flag value


Special command (not in menu - default bound to [f4]): attempts a smart narrowing to the current scripting block. Fails in client script containing server script sections.
\\{html-helper-mode-map}
Written by nelson@santafe.edu, http://www.santafe.edu/~nelson/
Mantained by lauri@eng.it, http:/www.gest.unipd.it/~saint/"
  (interactive)

  ;; Register this buffer as being handled by jsp-html-helper-mode
  ;; this has to be done only once, therefore...

  (html-helper-add-buffer (current-buffer) 'jsp-html-helper-mode-run)

  ;; the rest of the code is put in a separate function that can be
  ;; called several times.

  (jsp-html-helper-mode-run)
)

(defun jsp-html-helper-mode-run ()
  "Basic behaviour of the mode, to be called when returning from java-mode or jde"
  (interactive)
  (setq html-helper-mode-local-JSP-not-ASP-flag t)
  (setq major-mode 'jsp-html-helper-mode)
  (base-html-helper-mode 'html-helper-insert-new-ASP-buffer-strings)
  (setq mode-name "HTML/JSP helper")  
  (setq major-mode 'jsp-html-helper-mode))

(defun html-helper-mode ()
  "Mode for editing HTML documents.

The main function html-helper-mode provides is a menu and keybindings
for the HTML tags one inserts when writing HTML documents. Selecting
the menu item or typing the key sequence for a command inserts the
corresponding tag and places point in the right place. If a prefix
argument is supplied, the tags is instead wrapped around the region.
Alternately, one can type in part of the tag and complete it with M-TAB.

There is also code for indentation, timestamps, skeletons for new
documents, and lots of other neat features.

Uses :`visual-basic-mode' (optional - see below ) for ASP and VBScript
      `easymenu' for menu creation
      `cc-mode'  for javascript support
      `tempo'    for templates

Supports server (actually ASP & PHP, JSP) and client
(JavaScript, VBScript) scripting

Customizable flags you would like to alter

`html-helper-mode-uses-visual-basic' : non nil requires visual-basic-mode and activates ASP and VBScript support functions
`html-helper-mode-uses-bold-italic' : non nil creates a bold italic face (could fail if there's not such face available)
\\{html-helper-mode-map}
Written by nelson@santafe.edu, http://www.santafe.edu/~nelson/
Mantained by lauri@eng.it, http:/www.gest.unipd.it/~saint/
"
  (interactive)

  ;; Register this buffer as being handled by asp-html-helper-mode
  ;; this has to be done only once, therefore...

  (html-helper-add-buffer (current-buffer) 'html-helper-mode-run)

  ;; the rest of the code is put in a separate function that can be
  ;; called several times.

  (html-helper-mode-run))

(defun html-helper-mode-run ()
  "Basic behaviour of the mode, to be called when returning from
an alternate mode"
  (interactive)
  (setq html-helper-mode-local-JSP-not-ASP-flag
	html-helper-mode-global-JSP-not-ASP-flag)
  (setq major-mode 'html-helper-mode)
  (base-html-helper-mode 'html-helper-insert-new-buffer-strings)
  (setq mode-name "HTML helper")
  (setq html-helper-mode-local-JSP-not-ASP-flag html-helper-mode-global-JSP-not-ASP-flag)
  (setq major-mode 'html-helper-mode))

(defun php-html-helper-mode ()
  "Mode for editing HTML documents with PHP server scripts.

The main function html-helper-mode provides is a menu and keybindings
for the HTML tags one inserts when writing HTML documents. Selecting
the menu item or typing the key sequence for a command inserts the
corresponding tag and places point in the right place. If a prefix
argument is supplied, the tags is instead wrapped around the region.
Alternately, one can type in part of the tag and complete it with M-TAB.

There is also code for indentation, timestamps, skeletons for new
documents, and lots of other neat features.

Uses :`visual-basic-mode' for VBSCript
      `easymenu' for menu creation
      `cc-mode'  for javascript support and PHP blocks
      `tempo'    for templates

Supports server (actually ASP & PHP, JSP) and client
\(JavaScript, VBScript) scripting

Customizable flags you would like to alter

`html-helper-mode-uses-visual-basic' : non nil requires visual-basic-mode and activates ASP and VBScript support functions
`html-helper-mode-uses-bold-italic' : non nil creates a bold italic face (could fail if there's not such face available)
`html-helper-mode-uses-KG-style' : nil to make Emacs consider PHP/JSP/ASP code blocks beginning in the first colum
`html-helper-mode-global-JSP-not-ASP-flag' : non nil to make Emacs consider <% %> sequence as JSP blocks by default in html-helper-mode. Anyway the local copy of this flag is set to nil in asp-html-helper-mode, set to t in jsp-html-helper-mode.
Alter the behaviour locally by changing html-helper-mode-local-JSP-not-ASP-flag value

Special command (not in menu - default bound to [f4]): attempts a smart narrowing to the current scripting block. Fails in client script containing server script sections.
\\{html-helper-mode-map}
Written by nelson@santafe.edu, http://www.santafe.edu/~nelson/
Mantained by lauri@eng.it, http:/www.gest.unipd.it/~saint/"
  (interactive)

  ;; Register this buffer as being handled by jsp-html-helper-mode
  ;; this has to be done only once, therefore...

  (html-helper-add-buffer (current-buffer) 'php-html-helper-mode-run )

  ;; the rest of the code is put in a separate function that can be
  ;; called several times.

  (php-html-helper-mode-run))

(defun php-html-helper-mode-run ()
  "Basic behaviour of the mode, to be called when returning from cc-mode"
  (interactive)
  (setq html-helper-mode-local-JSP-not-ASP-flag
	html-helper-mode-global-JSP-not-ASP-flag)
  (setq major-mode 'php-html-helper-mode)
  (base-html-helper-mode 'html-helper-insert-new-PHP-buffer-strings )
  (setq mode-name "HTML/PHP helper")
  (setq major-mode 'php-html-helper-mode))

;;}}}

;;{{{ text faces

;; By Ulrik Dickow <dickow@nbi.dk>.
;;
;; Originally aimed at Emacs 19.29.  Later on disabled syntactic fontification
;; and reordered regexps completely, to be compatible with XEmacs (it doesn't
;; understand OVERRIDE=`keep').
;;
;; We make an effort on handling nested tags intelligently.

(make-face 'info-menu-6)

;; font-lock compatibility with XEmacs/Lucid and older Emacsen (<19.29).
;;
(if (string-match "XEmacs\\|Lucid" (emacs-version))
    ;; XEmacs/Lucid
    ;; Make needed faces if the user hasn't already done so.
    ;; Respect X resources (`make-face' uses them when they exist).
    (let ((change-it
 	   (function (lambda (face)
 		       (or (if (fboundp 'facep)
 			       (facep face)
 			     (memq face (face-list)))
 			   (make-face face))
 		       (not (face-differs-from-default-p face))))))
      (if (funcall change-it 'html-helper-bold-face)
 	  (progn (make-face 'html-helper-bold-face)
 		 (make-face-bold 'html-helper-bold-face)
		 (set-face-foreground html-helper-bold-face "peru")))
      (if (funcall change-it 'html-helper-italic-face)
 	  (progn (make-face 'html-helper-italic-face)
 		 (make-face-italic 'html-helper-italic-face)
		 (set-face-foreground 'html-helper-italic-face "medium sea green")))
      (if (funcall change-it 'html-helper-underline-face)
 	  (set-face-underline-p 'html-helper-underline-face t))
      (if (funcall change-it 'font-lock-variable-name-face)
 	  (set-face-foreground 'font-lock-variable-name-face "salmon"))
      (if (funcall change-it 'font-lock-reference-face)
 	  (set-face-foreground 'font-lock-reference-face "violet"))
      ; experimental code
      (if (funcall change-it 'html-helper-bold-italic-face)
 	  (progn (cond (html-helper-mode-uses-bold-italic
			(make-face 'html-helper-bold-italic-face)
			(make-face-bold-italic 'html-helper-bold-italic-face)
			(set-face-foreground 'html-helper-bold-italic-face "orange")))))
      (if (funcall change-it 'html-helper-underline-face)
	  (progn (make-face 'html-helper-underline-face)
		 (set-face-underline-p 'html-helper-underline-face t)
		 (set-face-foreground html-helper-underline-face "goldenrod")))
      (if (funcall change-it 'html-helper-tag-face)
	  (progn (make-face 'html-helper-tag-face)
		 (make-face-bold 'html-helper-tag-face)
		 (set-face-foreground html-tag-face "dodger blue")))
      (if (funcall change-it 'html-helper-server-script-face )
	  (progn (make-face 'html-helper-server-script-face)
		 (make-face-bold 'html-helper-server-script-face)
		 (set-face-foreground html-helper-server-script-face "orange")))
	;; PETER Neergaard <turtle@bu.edu> says
	;;
	;; "Another issue I just noticed is that font-lock-builtin-face
	;; is not a face defined Xemacs; instead they use
	;; font-lock-preprocessor-face (I too fail to see any good
	;; reasons that they have made this design choice).  I did not
	;; notice this at first because I hack define
	;; font-lock-builtin-face in my .emacs as I had some packages
	;; using font-lock-builtin-face when I started using Xemacs,
	;; but this has been changed now."  Then suggests this change :
      (make-face 'html-helper-builtin-face)
      (copy-face 'font-lock-preprocessor-face
		 'html-helper-builtin-face))
  ;; Emacs
  ;;
  ;; Note that Emacs evaluates the face entries in `font-lock-keywords',
  ;; while XEmacs doesn't.  So XEmacs doesn't use the following *variables*,
  ;; but instead the faces with the same names as the variables.


  ;; Use customization. I don't recall if earier version support it...
  (progn
    (defvar html-helper-server-script-face
      (defface html-helper-server-script-face
	'((((class color)
	    (background dark))
	   (:foreground "orange" :bold t))
	  (((class color)
	    (background light))
	   (:foreground "firebrick" :bold t))
	  (t
	   (:foreground "orange" :bold t)))
	"Face to use for HTML tags."
	  :group 'html-helper-faces))
    (defvar html-helper-tag-face
      (defface html-helper-tag-face
	'((((class color)
	    (background dark))
	   (:foreground "deep sky blue" :bold t))
	  (((class color)
	    (background light))
	   (:foreground "dodger blue" :bold t))
	  (t
	   (:foreground "dodger blue" :bold t)))
	"Face to use for HTML tags."
	  :group 'html-helper-faces))
    (defvar html-helper-bold-face
      (defface html-helper-bold-face
	'((((class color)
	    (background dark))
	   (:foreground "wheat" :bold t))
	  (((class color)
	    (background light))
	   (:foreground "peru" :bold t))
	  (t
	   (:foreground "peru" :bold t)))
	"Custom bold face."
	:group 'html-helper-faces))
    (defvar html-helper-italic-face
      (defface html-helper-italic-face
	'((((class color)
	    (background dark))
	   (:foreground "spring green" :italic t))
	  (((class color)
	    (background light))
	   (:foreground "medium sea green" :italic t))
	  (t
	   (:foreground "medium sea green" :italic t)))
	"Custom italic face."
	:group 'html-helper-faces))
    (cond (html-helper-mode-uses-bold-italic
	   (defvar html-helper-bold-italic-face
	     (defface html-helper-bold-italic-face
	       '((((class color)
		   (background dark))
		  (:foreground "peachpuff" :bold t:italic t))
		 (((class color)
		   (background light))
		  (:foreground "orange" :bold t :italic t))
		 (t
		  (:foreground "orange" :bold t :italic t)))
	       "Custom bold italic face."
	       :group 'html-helper-faces))))
    (defvar html-helper-underline-face
      (defface html-helper-underline-face
	'((((class color)
	    (background dark))
	   (:foreground "cornsilk" :underline t))
	  (((class color)
	    (background light))
	   (:foreground "goldenrod" :underline t))
	  (t
	   (:foreground "goldenrod" :underline t)))
	"Custom underline face."
	:group 'html-helper-faces))
    (defvar html-helper-builtin-face
      (defface html-helper-builtin-face
	'((((class color)
	    (background dark))
	   (:foreground "light goldenrod" :underline nil))
	  (((class color)
	    (background light))
	   (:foreground "dark goldenrod" :underline nil))
	  (t
	   (:foreground "light goldenrod" :underline nil)))
	"Custom Server Script face."
	:group 'html-helper-faces))))

  ;;

(copy-face
 (cond ((string-match "XEmacs\\|Lucid" emacs-version)
	font-lock-preprocessor-face)
       ;; Emacs
       (t font-lock-builtin-face)) html-helper-builtin-face)

;; internal variables

(defvar html-helper-count 0 "Counter during server script matching")

(defvar html-helper-verbose t
  "Non nil to show a counter during server script matching")

(defun html-helper-ticker ()
  "Returns the next prop image"
  (set 'html-helper-count (mod (incf html-helper-count) 8))
  (make-string html-helper-count 46))

;; match html tag attributes

;; (defun html-helper-fontify-region-old (beg end verbose)
;;   (setq html-helper-count 0)
;;   (setq html-helper-count2 0)
;;   (let ((loudly (and verbose
;; 		     (> (- end beg) (/ (buffer-size) 2)))))
;;     (setq html-helper-verbose loudly)
;;     (font-lock-default-fontify-region beg end loudly)))

;; (set (make-local-variable font-lock-fontify-region-function)
;;      'html-helper-fontify-region)

;; (defun html-helper-fontify-buffer ()
;;   (setq html-helper-count 0)
;;   (setq html-helper-count2 0)
;;   (setq html-helper-verbose (if (numberp font-lock-verbose)
;; 				(> (buffer-size) font-lock-verbose)
;; 			      font-lock-verbose))
;;   (font-lock-default-fontify-buffer))

;; (set (make-local-variable font-lock-fontify-buffer-function)
;;      'html-helper-fontify-buffer)

;;}}} faces

;;{{{ patterns for hilit19

;; Define some useful highlighting patterns for the hilit19 package.
;; These will activate only if hilit19 has already been loaded.
;; Thanks to <dickow@nbi.dk> for some pattern suggestions

(if (featurep 'hilit19)
    (hilit-set-mode-patterns
     'html-helper-mode
     '(("<!--" "-->" comment)
       ("<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>" nil comment) ;<!DOCTYPE ...>
       ("<title>" "</title>" defun)
       ("<h[1-6]>" "</h[1-6]>" bold) ;only colour inside tag
       ("<a\\b" ">" define)
       ("</a>" nil define)
       ("<img\\b" ">" include)
       ("<option\\|</?select\\|<input\\|</?form\\|</?textarea" ">" include)
       ;; First <i> highlighting just handles unnested tags, then do nesting
       ("<i>[^<]*</i>" nil italic)
       ("<b>" "</b>" bold)
       ("<i>" "</i>" italic)
       ("<u>" "</u>" underline)
       ("&[^;\n]*;" nil string)
       ;; w3 org says that a tag is <element-name> not < element-name>
       ("<[^ \t]" ">" keyword))
     nil 'case-insensitive)
  nil)
;;}}}

;;{{{ indentation

(defvar html-helper-print-indent-info nil
  "If t, indent will print out information as a message.")

;; some ideas borrowed from cc-mode.el.
;; Basic logic:
;;   if this line is some sort of list token, indent according to prev context:
;;     if previous context was a list-end or item-start, use its indentation
;;     if previous context was a list start, indent forward basic-offset
;;     ignore previous list-ends, their indentation is unreliable.
;;     then if this is some sort of list-item, do special case fixups:
;;       if this is a item start or end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and prev *not* a list end, go back basic-offset
;;   else if this line is not a list item, and previous line is a item-start
;;     indent continue-indent, because this is part of the item

;;}}}

;;{{{ Script Narrowing and mode switch
;; These are for supporting html-script. With small changes can be useful for
;; javascript

;; Stan Lanning <lanning@pobox.com> wrote these defadvice to preserve
;; cursor position. Thank you very much Stan!
(defadvice html-script-narrow-to-asp (around save-excursion activate)
  (save-excursion
    ad-do-it))
(defadvice html-script-narrow-to-php (around save-excursion activate)
  (save-excursion
    ad-do-it))
(defadvice html-script-narrow-to-vbscript (around save-excursion activate)
  (save-excursion
    ad-do-it))
(defadvice html-script-narrow-to-javascript (around save-excursion activate)
  (save-excursion
    ad-do-it))
(defadvice html-script-narrow-to-css (around save-excursion activate)
  (save-excursion
    ad-do-it))

(defadvice html-script-release-region (around save-excursion activate)
  (save-excursion
    ad-do-it))

(defun html-script-narrow-to-asp ()
  "Narrows to an JSP/ASP script and switches to either java-mode/JDE or visual-basic-mode.
Does nothing if both html-helper-mode-uses-visual-basic and
html-helper-mode-local-JSP-not-ASP-flag are nil"
  (interactive)
  (cond ((html-script-choose-mode)
	 (html-script-search-start-tag)
	 (let ((beg (point)))
	   (html-script-search-end-tag)
	   (narrow-to-region beg (point)))
	 (html-script-choose-server-mode)
	 (goto-char 0))))


(defun html-script-search-start-tag ()
 (cond ( html-helper-mode-uses-KG-style
		 (search-backward-regexp "^<%") )
	       (t (search-backward "<%" ))))

(defun html-script-search-end-tag ()
  (cond ( html-helper-mode-uses-KG-style
	  (search-forward-regexp "^%>" ))
	( t (search-forward "%>" nil t))))

(defun html-script-choose-mode ()
  (or html-helper-mode-uses-visual-basic
	     html-helper-mode-local-JSP-not-ASP-flag))

(defun html-script-choose-server-mode ()
  (cond (html-helper-mode-local-JSP-not-ASP-flag
	 (cond (html-helper-mode-uses-JDE (html-helper-enters-jde-mode))
	       (t (java-mode))))
	(t
	 (visual-basic-mode))))

(defun html-script-narrow-to-php ()
  "Narrows to an ASP script and setups c-mode"
  (interactive)
  (search-backward "<?")
  (let ((beg (point)))
    (search-forward "?>" nil t)
    (narrow-to-region beg (point)))
  (c-mode)
  (goto-char 0))

(defun html-script-narrow-to-vbscript ()
  "Narrows to a VB Script script and setups visual basic mode. Does nothing if html-helper-mode-uses-visual-basic is nil"
  (interactive)
  (cond (html-helper-mode-uses-visual-basic
	 (search-backward-regexp "<SCRIPT[ \t]+\\(LANGUAGE=\"VBScript\"\\|TYPE=\"text/vbscript\"\\)[ \t]*>")
	 (let ((beg (point)))
	   (search-forward "</SCRIPT>" nil t)
	   (narrow-to-region beg (point)))
	 (visual-basic-mode)
	 (goto-char 0))))

(defun html-script-narrow-to-javascript ()
  "Narrows to a JavaScript script and setups java mode"
  (interactive)
  (search-backward-regexp "<SCRIPT[ \t]+\\(LANGUAGE=\"JavaScript\"\\|TYPE=\"text/javascript\"\\)[ \t]*>")
  (let ((beg (point)))
    (search-forward "</SCRIPT>" nil t)
    (narrow-to-region beg (point)))
  (cond (html-helper-mode-uses-JDE (jde-mode))
	(t java-mode))
  (goto-char 0))

(defun html-script-narrow-to-css ()
  "Narrows to a style area  and setups css mode"
  (interactive)
  (search-backward-regexp "<STYLE>")
  (let ((beg (point)))
    (search-forward "</Style>" nil t)
    (narrow-to-region beg (point)))
  (css-mode)
  (goto-char 0))

(defun html-helper-add-buffer (buffer tag)
  (cond ((and html-helper-buffers
	      (html-helper-buffer-listed))
	 (set 'html-helper-buffers
	      (cons (cons buffer tag) html-helper-buffers )))
	(t (set 'html-helper-buffers (list (cons buffer tag))))))

;; Fri Aug 03 18:12:14 2001 Saint
;;
;; This function checks if the current buffer is not in
;; html-herlper-buffer list
(defun html-helper-buffer-listed ()
  (let ((retval t))
    (mapcar (lambda (a)
	    (cond ((eq (current-buffer) (car a))
		   (set 'retval nil))))
	    html-helper-buffers)
    retval))
;;/Saint

(defun html-helper-remove-buffer ()
  (let ((nl nil))
    (while html-helper-buffers
      (cond ((not (eq (current-buffer)
	      (car (car html-helper-buffers))))
	     (set 'nl (cons (car html-helper-buffers) nl))
	     (set 'html-helper-buffers (cdr html-helper-buffers)))
	    (t (set 'html-helper-buffers (cdr html-helper-buffers)))))
    (set 'html-helper-buffers nl)))

(defun html-script-release-region ()
   "widens the window to the complete buffer and runs html-helper-mode. MUST be interactive."
  (interactive)
  (mapcar (lambda (a)
	    (cond ((eq (current-buffer) (car a))
		   (html-helper-seletct-appropriate-mode (cdr a)))))
	  html-helper-buffers))

(defun html-helper-seletct-appropriate-mode( html-helper-used-mode)
  "Does the releasing of the narrowed region and calls the saved mode"
   (interactive)
   (goto-char 0)
   (widen)
   (cond (html-helper-used-mode
	 (funcall html-helper-used-mode))))

(defun html-script-release-setup()
  (interactive)
  (local-set-key html-helper-script-toggle-key 'html-script-release-region))

(cond (html-helper-mode-uses-visual-basic
      (cond
       (visual-basic-mode-hook
	(add-hook 'visual-basic-mode-hook 'html-script-release-setup))
       (t (setq visual-basic-mode-hook 'html-script-release-setup)))))

(cond
 (c-mode-hook
  (add-hook 'c-mode-hook 'html-script-release-setup))
 (t (setq c-mode-hook 'html-script-release-setup)))

(condition-case nil
    (cond
     ((or (boundp css-mode-hook) css-mode-hook)
      (add-hook 'css-mode-hook 'html-script-release-setup))
     (t (setq css-mode-hook 'html-script-release-setup)))
  (error (setq css-mode-hook 'html-script-release-setup)))

;; Very Very ALPHA!!!
;;
;; Adding html-script-release-setup to jde-entering-java-buffer-hooks
;;
(cond (html-helper-mode-uses-JDE
       (if (and html-helper-mode-uses-JDE (fboundp 'jde-mode))
	      (add-hook 'jde-mode-hook 'html-script-release-setup)))
       (t
	(cond
	 (java-mode-hook
	  (add-hook 'java-mode-hook
		    'html-script-release-setup))
	 (t (setq java-mode-hook
		  'html-script-release-setup))))
       )

(defun html-helper-enters-jde-mode()
  (interactive)
  (and html-helper-mode-uses-JDE (fboundp 'jde-mode))
   (add-hook 'jde-mode-hook 'html-script-release-setup)
  (jde-mode))

;; Still from Stan Lanning here it comes the code for a "smart switch" to
;; the appropriate scripting mode.

(defvar html-script-narrow-alist
  `((,(regexp-quote "<%") . html-script-narrow-to-asp)
    (,(regexp-quote "<?") . html-script-narrow-to-php)
    ("<SCRIPT[ \t]+LANGUAGE=\"VBScript\"[ \t]*>" . html-script-narrow-to-vbscript)
    ("<SCRIPT[ \t]+TYPE=\"text/vbscript\"[ \t]*>" . html-script-narrow-to-vbscript)
    ("<SCRIPT[ \t]+LANGUAGE=\"JavaScript\"[ \t]*>" . html-script-narrow-to-javascript)
    ("<SCRIPT[ \t]+TYPE=\"text/javascript\"[ \t]*>" . html-script-narrow-to-javascript)
    ("<STYLE>" . html-helper-narrow-to-css)))

(defvar html-script-start-regexp
  (concat "\\(" (mapconcat (lambda (x) (car x)) html-script-narrow-alist "\\|") "\\)"))

(defun html-script-toggle-narrow ()
  (interactive)
  (let ((handler-fn (save-excursion
		      (if (re-search-backward html-script-start-regexp nil t)
			  (catch 'handler-found
			    (mapcar (lambda (p)
				      (if (looking-at (car p))
					  (throw 'handler-found (cdr p))))
				    html-script-narrow-alist)
			    nil)))))
    (if handler-fn
	(apply handler-fn nil)
      (error "No script tag found"))))

(defun html-script-install-toggle-key ()
  (local-set-key html-helper-script-toggle-key 'html-script-toggle-narrow))

(add-hook 'html-helper-mode-hook 'html-script-install-toggle-key)

(defadvice html-script-release-setup (after key-binding activate)
  (local-set-key html-helper-script-toggle-key 'html-script-release-region))
;;}}}

;; folding tags: End of code tree
;;}}}

;;{{{ Epilogue

(provide 'html-helper-mode)
(provide 'php-html-helper-mode)
(provide 'asp-html-helper-mode)
(provide 'jsp-html-helper-mode)
(provide 'html-mode)			;for 14 character filename
(cond ((boundp 'kill-buffer-hook)
       (add-hook 'kill-buffer-hook 'html-helper-remove-buffer))
      (t (set 'kill-buffer-hook 'html-helper-remove-buffer)))
(run-hooks 'html-load-hook)
(run-hooks 'html-helper-load-hook)

;;}}}

;;; html-helper-mode.el ends here