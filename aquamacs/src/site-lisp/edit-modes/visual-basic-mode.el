;;; visual-basic-mode.el --- A mode for editing Visual Basic programs.

;; This is free software.
;; Modified version of Fred White's visual-basic-mode.el


;; Copyright (C) 1996 Fred White <fwhite@alum.mit.edu>
;; Copyright (C) 1998 Free Software Foundation, Inc.
;;   (additions by Dave Love)
;; Copyright (C) 2008-2009 Free Software Foundation, Inc.
;;   (additions by Randolph Fritz and Vincent Belaiche (VB1) )

;; Author: Fred White <fwhite@alum.mit.edu>
;; Adapted-by: Dave Love <d.love@dl.ac.uk>
;;           : Kevin Whitefoot <kevin.whitefoot@nopow.abb.no>
;;           : Randolph Fritz <rfritz@u.washington.edu>
;;           : Vincent Belaiche (VB1) <vincentb1@users.sourceforge.net>
;; Version: 1.4.12 (2010-10-18)
;; Serial Version: %Id: 32%
;; Keywords: languages, basic, Evil
;; X-URL:  http://www.emacswiki.org/cgi-bin/wiki/visual-basic-mode.el


;; (Old) LCD Archive Entry:
;; basic-mode|Fred White|fwhite@alum.mit.edu|
;; A mode for editing Visual Basic programs.|
;; 18-Apr-96|1.0|~/modes/basic-mode.el.Z|

;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs  is free software;  you can redistribute it and/or modify
;; it under the terms of  the GNU General  Public License as published
;; by  the Free Software  Foundation;  either version  2, or (at  your
;; option) any later version.
;;
;; GNU  Emacs is distributed  in the hope that  it will be useful, but
;; WITHOUT    ANY  WARRANTY;  without even the     implied warranty of
;; MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See the  GNU
;; General Public License for more details.
;;
;; You should have received  a copy of  the GNU General Public License
;; along with GNU Emacs; see  the file COPYING.  If  not, write to the
;; Free Software Foundation, 675  Mass Ave, Cambridge, MA 02139,  USA.
;; This  program  is free  software;  you  can  redistribute it and/or
;; modify it  under  the terms of the  GNU  General Public License  as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;;; Commentary:

;; Purpose of this package:
;;  This is a mode for editing programs written in The World's Most
;;  Successful Programming Language.  It features automatic
;;  indentation, font locking, keyword capitalization, and some minor
;;  convenience functions.

;; Installation instructions
;;  Put visual-basic-mode.el somewhere in your path, compile it, and add
;;  the following to your init file:

;;  (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
;;  (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
;;                                  visual-basic-mode)) auto-mode-alist))
;;
;;  If you are doing Rhino scripts, add:
;;  (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|rvb\\)$" .
;;                                  visual-basic-mode)) auto-mode-alist))

;;  If you had visual-basic-mode already installed, you may need to call
;;  visual-basic-upgrade-keyword-abbrev-table the first time that
;;  visual-basic-mode is loaded.

;; Of course, under Windows 3.1, you'll have to name this file
;; something shorter than visual-basic-mode.el

;; Revisions:
;; 1.0 18-Apr-96  Initial version
;; 1.1 Accomodate Emacs 19.29+ font-lock-defaults
;;     Simon Marshall <Simon.Marshall@esrin.esa.it>
;; 1.2 Rename to visual-basic-mode
;; 1.3 Fix some indentation bugs.
;; 1.3+ Changes by Dave Love: [No attempt at compatibility with
;;      anything other than Emacs 20, sorry, but little attempt to
;;      sanitize for Emacs 20 specifically.]
;;      Change `_' syntax only for font-lock and imenu, not generally;
;;      provide levels of font-locking in the current fashion;
;;      font-lock case-insensitively; use regexp-opt with the font-lok
;;      keywords; imenu support; `visual-basic-split-line', bound to
;;      C-M-j; account for single-statement `if' in indentation; add
;;      keyword "Global"; use local-write-file-hooks, not
;;      write-file-hooks.
;; 1.4 September 1998
;; 1.4 KJW Add begin..end, add extra keywords
;;     Add customisation for single line if.  Disallow by default.
;;     Fix if regexp to require whitespace after if and require then.
;;     Add more VB keywords.  Make begin..end work as if..endif so
;;     that forms are formatted correctly.
;; 1.4.1 KJW Merged Dave Love and KJW versions.
;;     Added keywords suggested by Mickey Ferguson
;;     <MFerguson@peinc.com>
;;     Fixed imenu variable to find private variables and enums

;;     Changed syntax class of =, <, > to punctuation to allow dynamic
;;     abbreviations to pick up only the word at point rather than the
;;     whole expression.

;;     Fixed bug introduced by KJW adding suport for begin...end in
;;     forms whereby a single end outdented.

;;     Partially fixed failure to recognise if statements with
;;     continuations (still fails on 'single line' if with
;;     continuation, ugh).
;; 1.4.2 RF added "class" and "null" keywords, "Rhino" script note.
;; 1.4.3 VB1 added
;;     1) function visual-basic-if-not-on-single-line to recognize single line
;;      if statements, even when line is broken.  variable
;;      visual-basic-allow-single-line-if default set to t again.
;;     2) use of 'words in calling regexp-opt rather than concat \\< ...\\>
;;     3) new keywords Preserve and Explicit
;; 1.4.4 VB1 added function visual-basic-close-block
;; 1.4.5 VB1, (expand-abbrev) within (save-excusion...)
;; 1.4.6 VB1 correct visual-basic-close-block (single line If case)
;; 1.4.7 VB1 correct visual-basic-close-block (For/Next)
;; 1.4.8 VB1 correct visual-basic-close-block (Property, + add With /End With)
;;           add command visual-basic-insert-item
;; 1.4.8 2010-05-15 Lennart Borgman:
;;        - Minor corrections
;; 1.4.9 VB1 - make customizable variable accessible through defcustom
;;           - add support for `Select Case' in visual-basic-insert-item
;;           - reword of the `Dim' case in  visual-basic-insert-item
;; 1.4.9b Lennart Borgman+VB1: correct abbreviation and support `_' as a valid
;;        symbol character
;; 1.4.10 VB1 - Add punctuation syntax for operators
;;            - create visual-basic-check-style
;;            - improve idiom detection
;; 1.4.10b,c VB1 -improve visual-basic-check-style
;; 1.4.10d   VB1 -correct font lock keywords for case
;;               -improve visual-basic-check-style + add highlight overlay 
;; 1.4.11 Wang Yao - correct the regular expression for imenu
;;                 - remove the string-to-char for imenu-syntax-alist, for xemacs error
;;                 - change the condition of visual-basic-enable-font-lock which prevents emacs from running in command-line mode when the emacs-version is 19.29
;;                 - correct the implement of droping tailing comment in visual-basic-if-not-on-single-line
;; 1.4.12 VB1 - add visual-basic-propertize-attribute

;;
;; Notes:
;; Dave Love
;; BTW, here's a script for making tags tables that I (Dave Love) have
;; used with reasonable success.  It assumes a hacked version of etags
;; with support for case-folded regexps.  I think this is now in the
;; development version at <URL:ftp://fly.cnuce.cnr.it/pub/> and should
;; make it into Emacs after 20.4.

;; #! /bin/sh

;; # etags-vb: (so-called) Visual (so-called) Basic TAGS generation.
;; # Dave Love <d.love@dl.ac.uk>.  Public domain.
;; # 1997-11-21

;; if [ $# -lt 1 ]; then
;;     echo "Usage: `basename $0` [etags options] VBfile ... [etags options] " 1>&2
;;     exit 1
;; fi

;; if [ $1 = "--help" ] || [ $1 = "-h" ]; then
;;     echo "Usage: `basename $0` [etags options] VBfile ... [etags options]

;; "
;;     etags --help
;; fi

;; exec etags --lang=none -c '/\(global\|public\)[ \t]+\(\(const\|type\)[ \t]+\)*\([a-z_0-9]+\)/\4/' \
;;     -c '/public[ \t]+\(sub\|function\|class\)[ \t]+\([a-z_0-9]+\)/\2/' \
;;   "$@"

;; End Notes Dave Love


;; Known bugs:
;;  Doesn't know about ":" separated stmts



;; todo:
;;  fwd/back-compound-statement
;;  completion over OCX methods and properties.
;;  IDE integration
;;  Change behaviour of ESC-q to recognise words used as paragraph
;;  titles and prevent them being dragged into the previous
;;  paragraph.
;;  etc.



;;; History:
;;

;;; Code:

(eval-when-compile (require 'cl))

(defvar visual-basic-xemacs-p (string-match "XEmacs\\|Lucid" (emacs-version)))
(defvar visual-basic-winemacs-p (string-match "Win-Emacs" (emacs-version)))
(defvar visual-basic-win32-p (eq window-system 'w32))

;; Variables you may want to customize.
(defgroup visual-basic nil
  "Customization of the Visual Basic mode."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages   )

(defcustom visual-basic-mode-indent 8
  "*Default indentation per nesting level."
  :type 'integer
  :group 'visual-basic)

(defcustom visual-basic-fontify-p t
  "*Whether to fontify Basic buffers."
  :type 'boolean
  :group 'visual-basic)

(defcustom visual-basic-capitalize-keywords-p t
  "*Whether to capitalize BASIC keywords."
  :type 'boolean
  :group 'visual-basic)

(defcustom visual-basic-wild-files "*.frm *.bas *.cls"
  "*Wildcard pattern for BASIC source files."
  :type 'string
  :group 'visual-basic)

(defcustom visual-basic-ide-pathname nil
  "*The full pathname of your Visual Basic exe file, if any."
  :type '(choice
	  (const nil :tag "no IDE available" )
	  (file :must-match  t :tag "IDE exe path" ))
  :group 'visual-basic)

(defcustom visual-basic-allow-single-line-if t
  "*Whether to allow single line if."
  :type 'boolean
  :group 'visual-basic)


(defcustom visual-basic-auto-check-style-level -1
  "Tune what style error are automatically corrected by function
`visual-basic-check-style'. The higher this number, the more
types of errors are automatically corrected.

* -1 : all errors correction need confirmation by user

*  0 : punctuation errors are automatically corrected"
  :type 'integer
  :group 'visual-basic)

(defcustom visual-basic-variable-scope-prefix-re
  "[gm]?"
  "Variable naming convention, scope prefix regexp. Please refer
to
http://en.wikibooks.org/wiki/Visual_Basic/Coding_Standards. This
is used by function `visual-basic-propertize-attribute'. 

Note: shall not contain any \\( \\) (use \\(?: if need be)."
  :type 'regexp
  :group 'visual-basic
  )

(defcustom visual-basic-variable-type-prefix-re
  (regexp-opt '("i" ; integer
		"l" ; long
		"flt"; single or double
		"obj" "o"; object
		"v" ; variant
		"dbl" "sng"; double single
		"s"; string
		) t)
  "Variable naming convention, type prefix regexp. Please refer
to
http://en.wikibooks.org/wiki/Visual_Basic/Coding_Standards. This
is used by function `visual-basic-propertize-attribute'.

Note: shall not contain any \\( \\) (use \\(?: if need be)."
  :type 'regexp
  :group 'visual-basic
  )

(defvar visual-basic-defn-templates
  (list "Public Sub ()\nEnd Sub\n\n"
        "Public Function () As Variant\nEnd Function\n\n"
        "Public Property Get ()\nEnd Property\n\n")
  "*List of function templates though which `visual-basic-new-sub' cycles.")

(defvar visual-basic-imenu-generic-expression
  '((nil "^\\s-*\\(public\\|private\\)*\\s-*\\(declare\\s-+\\)*\\(sub\\|function\\)\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\>\\)"
         4)
    ("Constants"
     "^\\s-*\\(private\\|public\\|global\\)*\\s-*\\(const\\s-+\\)\\(\\(?:\\sw\\|\\s_\\)+\\>\\s-*=\\s-*.+\\)\\($\\|'\\)"
     3)
    ("Variables"
     "^\\(private\\|public\\|global\\|dim\\)+\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\>\\s-+as\\s-+\\(?:\\sw\\|\\s_\\)+\\>\\)"
     2)
    ("Types" "^\\(public\\s-+\\)*type\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 2)))



(defvar visual-basic-mode-syntax-table nil)
(if visual-basic-mode-syntax-table
    ()
  (setq visual-basic-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\<" visual-basic-mode-syntax-table) ; Comment starter
  (modify-syntax-entry ?\n ">" visual-basic-mode-syntax-table)
  (modify-syntax-entry ?\\ "w" visual-basic-mode-syntax-table)
  (modify-syntax-entry ?_ "_" visual-basic-mode-syntax-table)
  ; Make operators puncutation so that regexp search \_< and \_> works properly
  (modify-syntax-entry ?+ "." visual-basic-mode-syntax-table)
  (modify-syntax-entry ?- "." visual-basic-mode-syntax-table)
  (modify-syntax-entry ?* "." visual-basic-mode-syntax-table)
  (modify-syntax-entry ?/ "." visual-basic-mode-syntax-table)
  (modify-syntax-entry ?\\ "." visual-basic-mode-syntax-table)
  ; Make =, etc., punctuation so that dynamic abbreviations work properly
  (modify-syntax-entry ?\= "." visual-basic-mode-syntax-table)
  (modify-syntax-entry ?\< "." visual-basic-mode-syntax-table)
  (modify-syntax-entry ?\> "." visual-basic-mode-syntax-table))


(defvar visual-basic-mode-map nil)
(if visual-basic-mode-map
    ()
  (setq visual-basic-mode-map (make-sparse-keymap))
  (define-key visual-basic-mode-map "\t" 'visual-basic-indent-line)
  (define-key visual-basic-mode-map "\r" 'visual-basic-newline-and-indent)
  (define-key visual-basic-mode-map "\M-\r" 'visual-basic-insert-item)
  (define-key visual-basic-mode-map "\C-c\C-j" 'visual-basic-insert-item)
  (define-key visual-basic-mode-map "\M-\C-a" 'visual-basic-beginning-of-defun)
  (define-key visual-basic-mode-map "\M-\C-e" 'visual-basic-end-of-defun)
  (define-key visual-basic-mode-map "\M-\C-h" 'visual-basic-mark-defun)
  (define-key visual-basic-mode-map "\M-\C-\\" 'visual-basic-indent-region)
  (define-key visual-basic-mode-map "\M-q" 'visual-basic-fill-or-indent)
  (define-key visual-basic-mode-map "\M-\C-j" 'visual-basic-split-line)
  (define-key visual-basic-mode-map "\C-c]" 'visual-basic-close-block)
  (cond (visual-basic-winemacs-p
         (define-key visual-basic-mode-map '(control C) 'visual-basic-start-ide))
        (visual-basic-win32-p
         (define-key visual-basic-mode-map (read "[?\\S-\\C-c]") 'visual-basic-start-ide)))
  (if visual-basic-xemacs-p
      (progn
        (define-key visual-basic-mode-map "\M-G" 'visual-basic-grep)
        (define-key visual-basic-mode-map '(meta backspace) 'backward-kill-word)
        (define-key visual-basic-mode-map '(control meta /) 'visual-basic-new-sub))))


;; These abbrevs are valid only in a code context.
(defvar visual-basic-mode-abbrev-table nil)

(defvar visual-basic-mode-hook ())


;; Is there a way to case-fold all regexp matches?
;; Change KJW Add enum, , change matching from 0 or more to zero or one for public etc.
(eval-and-compile
  (defconst visual-basic-defun-start-regexp
    (concat
     "^[ \t]*\\([Pp]ublic \\|[Pp]rivate \\|[Ss]tatic\\|[Ff]riend \\)?"
     "\\([Ss]ub\\|[Ff]unction\\|[Pp]roperty +[GgSsLl]et\\|[Tt]ype\\|[Ee]num\\|[Cc]lass\\)"
     "[ \t]+\\(\\w+\\)[ \t]*(?")))


(defconst visual-basic-defun-end-regexp
  "^[ \t]*[Ee]nd \\([Ss]ub\\|[Ff]unction\\|[Pp]roperty\\|[Tt]ype\\|[Ee]num\\|[Cc]lass\\)")

(defconst visual-basic-dim-regexp
  "^[ \t]*\\([Cc]onst\\|[Dd]im\\|[Pp]rivate\\|[Pp]ublic\\)\\_>"  )

(defconst visual-basic-lettable-type-regexp 
  (concat "\\`" 
	  (regexp-opt '("Integer" "Long" "Variant" "Double" "Single" "Boolean") t)
	  "\\'"))

;; Includes the compile-time #if variation.
;; KJW fixed if to require a whitespace so as to avoid matching, for
;; instance, iFileName and to require then.

;; Two versions; one recognizes single line if just as though it were
;; a multi-line and the other does not.  Modified again to remove the
;; requirement for then so as to allow it to match if statements that
;; have continuations -- VB1 further elaborated on this for single line
;; if statement to be recognized on broken lines.
;;(defconst visual-basic-if-regexp
;;   "^[ \t]*#?[Ii]f[ \t]+.*[ \t]+[Tt]hen[ \t]*.*\\('\\|$\\)")
(defconst visual-basic-if-regexp
  "^[ \t]*#?[Ii]f[ \t]+.*[ \t_]+")

(defconst visual-basic-ifthen-regexp "^[ \t]*#?[Ii]f.+\\<[Tt]hen\\>\\s-\\S-+")

(defconst visual-basic-else-regexp "^[ \t]*#?[Ee]lse\\([Ii]f\\)?")
(defconst visual-basic-endif-regexp "[ \t]*#?[Ee]nd[ \t]*[Ii]f")

(defconst visual-basic-looked-at-continuation-regexp   "_[ \t]*$")

(defconst visual-basic-continuation-regexp
  (concat "^.*" visual-basic-looked-at-continuation-regexp))

(eval-and-compile
  (defconst visual-basic-label-regexp "^[ \t]*[a-zA-Z0-9_]+:$"))

(defconst visual-basic-select-regexp "^[ \t]*[Ss]elect[ \t]+[Cc]ase\\_>")
(defconst visual-basic-case-regexp "^\\([ \t]*\\)[Cc]ase\\_>")
(defconst visual-basic-case-else-regexp "^\\([ \t]*\\)[Cc]ase\\(\\s-+[Ee]lse\\)\\_>")
(defconst visual-basic-select-end-regexp "^\\([ \t]*\\)[Ee]nd[ \t]+[Ss]elect\\_>")


(defconst visual-basic-for-regexp "^[ \t]*[Ff]or\\b")
(defconst visual-basic-next-regexp "^[ \t]*[Nn]ext\\b")

(defconst visual-basic-do-regexp "^[ \t]*[Dd]o\\b")
(defconst visual-basic-loop-regexp "^[ \t]*[Ll]oop\\b")

(defconst visual-basic-while-regexp "^[ \t]*[Ww]hile\\b")
(defconst visual-basic-wend-regexp "^[ \t]*[Ww]end\\b")

;; Added KJW Begin..end for forms
(defconst visual-basic-begin-regexp "^[ \t]*[Bb]egin)?")
;; This has created a bug.  End on its own in code should not outdent.
;; How can we fix this?  They are used in separate Lisp expressions so
;; add another one.
(defconst visual-basic-end-begin-regexp "^[ \t]*[Ee]nd")

(defconst visual-basic-with-regexp "^[ \t]*[Ww]ith\\b")
(defconst visual-basic-end-with-regexp "^[ \t]*[Ee]nd[ \t]+[Ww]ith\\b")

(defconst visual-basic-blank-regexp "^[ \t]*$")
(defconst visual-basic-comment-regexp "^[ \t]*\\s<.*$")


;; This is some approximation of the set of reserved words in Visual Basic.
(eval-and-compile
  (defvar visual-basic-all-keywords
    '("Add" "Aggregate" "And" "App" "AppActivate" "Application" "Array" "As"
      "Asc" "AscB" "Atn" "Attribute"
      "Beep" "Begin" "BeginTrans" "Boolean" "ByVal" "ByRef"
      "CBool" "CByte" "CCur"
      "CDate" "CDbl" "CInt" "CLng" "CSng" "CStr" "CVErr" "CVar" "Call"
      "Case" "ChDir" "ChDrive" "Character" "Choose" "Chr" "ChrB" "Class"
      "ClassModule" "Clipboard" "Close" "Collection" "Column" "Columns"
      "Command" "CommitTrans" "CompactDatabase" "Component" "Components"
      "Const" "Container" "Containers" "Cos" "CreateDatabase" "CreateObject"
      "CurDir" "Currency"
      "DBEngine" "DDB" "Data" "Database" "Databases"
      "Date" "DateAdd" "DateDiff" "DatePart" "DateSerial" "DateValue" "Day"
      "Debug" "Declare" "Deftype" "DeleteSetting" "Dim" "Dir" "Do"
      "DoEvents" "Domain"
      "Double" "Dynaset" "EOF" "Each" "Else" "Empty" "End" "EndProperty"
      "Enum" "Environ" "Erase" "Err" "Error" "Exit" "Exp" "Explicit" "FV" "False" "Field"
      "Fields" "FileAttr" "FileCopy" "FileDateTime" "FileLen" "Fix" "Font" "For"
      "Form" "FormTemplate" "Format" "FormatCurrency" "FormatDateTime" "FormatNumber"
      "FormatPercent" "Forms" "FreeFile" "FreeLocks" "Friend" "Function"
      "Get" "GetAllSettings" "GetAttr" "GetObject" "GetSetting" "Global" "GoSub"
      "GoTo" "Group" "Groups" "Hex" "Hour" "IIf" "IMEStatus" "IPmt" "IRR"
      "If" "Implements" "InStr" "Input" "Int" "Integer" "Is" "IsArray" "IsDate"
      "IsEmpty" "IsError" "IsMissing" "IsNull" "IsNumeric" "IsObject" "Kill"
      "LBound" "LCase" "LOF" "LSet" "LTrim" "Left" "Len" "Let" "Like" "Line"
      "Load" "LoadPicture" "LoadResData" "LoadResPicture" "LoadResString" "Loc"
      "Lock" "Log" "Long" "Loop" "MDIForm" "MIRR" "Me" "MenuItems"
      "MenuLine" "Mid" "Minute" "MkDir" "Month" "MsgBox" "NPV" "NPer" "Name"
      "New" "Next" "Not" "Now" "Nothing" "Null" "Object" "Oct" "On" "Open"
      "OpenDatabase"
      "Operator" "Option" "Optional"
      "Or" "PPmt" "PV" "Parameter" "Parameters" "Partition"
      "Picture" "Pmt" "Preserve" "Print" "Printer" "Printers" "Private"
      "ProjectTemplate" "Property"
      "Properties" "Public" "Put" "QBColor" "QueryDef" "QueryDefs"
      "RSet" "RTrim" "Randomize" "Rate" "ReDim" "Recordset" "Recordsets"
      "RegisterDatabase" "Relation" "Relations" "Rem" "RepairDatabase"
      "Reset" "Resume" "Return" "Right" "RmDir" "Rnd" "Rollback" "RowBuffer"
      "SLN" "SYD" "SavePicture" "SaveSetting" "Screen" "Second" "Seek"
      "SelBookmarks" "Select" "SelectedComponents" "SendKeys" "Set"
      "SetAttr" "SetDataAccessOption" "SetDefaultWorkspace" "Sgn" "Shell"
      "Sin" "Single" "Snapshot" "Space" "Spc" "Sqr" "Static" "Step" "Stop" "Str"
      "StrComp" "StrConv" "String" "Sub" "SubMenu" "Switch" "Tab" "Table"
      "TableDef" "TableDefs" "Tan" "Then" "Time" "TimeSerial" "TimeValue"
      "Timer" "To" "Trim" "True" "Type" "TypeName" "UBound" "UCase" "Unload"
      "Unlock" "Val" "Variant" "VarType" "Verb" "Weekday" "Wend"
      "While" "Width" "With" "Workspace" "Workspaces" "Write" "Year")))

(defvar visual-basic-font-lock-keywords-1
  (eval-when-compile
    (list
     ;; Names of functions.
     (list visual-basic-defun-start-regexp
           '(1 font-lock-keyword-face nil t)
           '(2 font-lock-keyword-face nil t)
           '(3 font-lock-function-name-face))

     ;; Statement labels
     (cons visual-basic-label-regexp 'font-lock-keyword-face)

     ;; Case values
     ;; String-valued cases get font-lock-string-face regardless.
     (list "^[ \t]*case[ \t]+\\([^:'\n]+\\)" 1 'font-lock-keyword-face t)

     ;; Any keywords you like.
     (list (regexp-opt
            '("Dim" "If" "Then" "Else" "ElseIf" "End If") 'words)
           1 'font-lock-keyword-face))))

(defvar visual-basic-font-lock-keywords-2
  (append visual-basic-font-lock-keywords-1
          (eval-when-compile
            `((, (regexp-opt visual-basic-all-keywords 'words)
                 1 font-lock-keyword-face)))))

(defvar visual-basic-font-lock-keywords visual-basic-font-lock-keywords-1)


(put 'visual-basic-mode 'font-lock-keywords 'visual-basic-font-lock-keywords)

;;;###autoload
(defun visual-basic-mode ()
  "A mode for editing Microsoft Visual Basic programs.
Features automatic indentation, font locking, keyword capitalization,
and some minor convenience functions.
Commands:
\\{visual-basic-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map visual-basic-mode-map)
  (setq major-mode 'visual-basic-mode)
  (setq mode-name "Visual Basic")
  (set-syntax-table visual-basic-mode-syntax-table)

  ;; This should be the users choice
  ;;(add-hook 'local-write-file-hooks 'visual-basic-untabify)

  (setq local-abbrev-table visual-basic-mode-abbrev-table)
  (if visual-basic-capitalize-keywords-p
      (progn
        ;;(make-local-variable 'pre-abbrev-expand-hook)
        ;;(add-hook 'pre-abbrev-expand-hook 'visual-basic-pre-abbrev-expand-hook)
        (add-hook 'abbrev-expand-functions 'visual-basic-abbrev-expand-function nil t)
        (abbrev-mode 1)))

  (make-local-variable 'comment-start)
  (setq comment-start "' ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "'+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'visual-basic-indent-line)

  (if visual-basic-fontify-p
      (visual-basic-enable-font-lock))

  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression visual-basic-imenu-generic-expression)

  (set (make-local-variable 'imenu-syntax-alist) `(("_" . "w")))
  (set (make-local-variable 'imenu-case-fold-search) t)

  ;;(make-local-variable 'visual-basic-associated-files)
  ;; doing this here means we need not check to see if it is bound later.
  (add-hook 'find-file-hooks 'visual-basic-load-associated-files)

  (run-hooks 'visual-basic-mode-hook))


(defun visual-basic-enable-font-lock ()
  "Enable font locking."
  ;; Emacs 19.29 requires a window-system else font-lock-mode errs out.
  (cond ((or visual-basic-xemacs-p window-system (not (string-equal (emacs-version) "19.29")))

         ;; In win-emacs this sets font-lock-keywords back to nil!
         (if visual-basic-winemacs-p
             (font-lock-mode 1))

         ;; Accomodate emacs 19.29+
         ;; From: Simon Marshall <Simon.Marshall@esrin.esa.it>
         (cond ((boundp 'font-lock-defaults)
                (make-local-variable 'font-lock-defaults)
                (setq font-lock-defaults
                      `((visual-basic-font-lock-keywords
                         visual-basic-font-lock-keywords-1
                         visual-basic-font-lock-keywords-2)
                        nil t ((,(string-to-char "_") . "w")))))
               (t
                (make-local-variable 'font-lock-keywords)
                (setq font-lock-keywords visual-basic-font-lock-keywords)))

         (if visual-basic-winemacs-p
             (font-lock-fontify-buffer)
           (font-lock-mode 1)))))

;; KJW should add some odds and bobs here to cover "end if" one way
;; could be to create the abbreviations by removing whitespace then we
;; could put "end if", "end with" and so on in the keyword table
;; Another idea would be to make it intelligent enough to substitute
;; the correct end for the construct (with, select, if)
;; Is this what the abbrev table hook entry is for?
(defun visual-basic-construct-keyword-abbrev-table ()
  "Construction abbreviation table from list of keywords."
(if visual-basic-mode-abbrev-table
      nil
    (let ((words visual-basic-all-keywords)
          (word nil)
          (list nil))
      (while words
        (setq word (car words)
              words (cdr words))
        (setq list (cons (list (downcase word) word) list)))

      (define-abbrev-table 'visual-basic-mode-abbrev-table list))))

;; Would like to do this at compile-time.
(visual-basic-construct-keyword-abbrev-table)


(defun visual-basic-upgrade-keyword-abbrev-table ()
  "Use this in case of upgrading visual-basic-mode.el."
  (interactive)

  (let ((words visual-basic-all-keywords)
        (word nil)
        (list nil))
    (while words
      (setq word (car words)
            words (cdr words))
      (setq list (cons (list (downcase word) word) list)))
    (define-abbrev-table 'visual-basic-mode-abbrev-table list)))


(defun visual-basic-in-code-context-p ()
  "Predicate true when pointer is in code context."
  (save-match-data
  (if (fboundp 'buffer-syntactic-context) ; XEmacs function.
      (null (buffer-syntactic-context))
    ;; Attempt to simulate buffer-syntactic-context
    ;; I don't know how reliable this is.
    (let* ((beg (save-excursion
                  (beginning-of-line)
                  (point)))
           (list
            (parse-partial-sexp beg (point))))
      (and (null (nth 3 list))          ; inside string.
	     (null (nth 4 list)))))))      ; inside comment

(defun visual-basic-abbrev-expand-function (expand-fun)
  "Expansion of abbreviations.  EXPAND-FUN is called at the end of this function."
  ;; Allow our abbrevs only in a code context.
  (setq local-abbrev-table
        (if (visual-basic-in-code-context-p)
            visual-basic-mode-abbrev-table))
  (funcall expand-fun))


(defun visual-basic-newline-and-indent (&optional count)
  "Insert a newline, updating indentation.  Argument COUNT is ignored."
  (interactive)
  (save-excursion
    (expand-abbrev)
    (visual-basic-indent-line))
  (call-interactively 'newline-and-indent))

(defun visual-basic-beginning-of-defun ()
  "Set the pointer at the beginning of the Sub/Function/Property within which the pointer is located."
  (interactive)
  (re-search-backward visual-basic-defun-start-regexp))

(defun visual-basic-end-of-defun ()
  "Set the pointer at the beginning of the Sub/Function/Property within which the pointer is located."
  (interactive)
  (re-search-forward visual-basic-defun-end-regexp))

(defun visual-basic-mark-defun ()
  "Set the region pointer around Sub/Function/Property within which the pointer is located."
  (interactive)
  (beginning-of-line)
  (visual-basic-end-of-defun)
  (set-mark (point))
  (visual-basic-beginning-of-defun)
  (if visual-basic-xemacs-p
      (zmacs-activate-region)))

(defun visual-basic-indent-defun ()
  "Indent the function within which the pointer is located.  This has a border on mark."
  ;; VB1 to Lennart: is border effect on mark an issue ?
  (interactive)
  (save-excursion
    (visual-basic-mark-defun)
    (call-interactively 'visual-basic-indent-region)))


(defun visual-basic-fill-long-comment ()
  "Fills block of comment lines around point."
  ;; Derived from code in ilisp-ext.el.
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((comment-re "^[ \t]*\\s<+[ \t]*"))
      (if (looking-at comment-re)
          (let ((fill-prefix
                 (buffer-substring
                  (progn (beginning-of-line) (point))
                  (match-end 0))))

            (while (and (not (bobp))
                        (looking-at visual-basic-comment-regexp))
              (forward-line -1))
            (if (not (bobp)) (forward-line 1))

            (let ((start (point)))

              ;; Make all the line prefixes the same.
              (while (and (not (eobp))
                          (looking-at comment-re))
                (replace-match fill-prefix)
                (forward-line 1))

              (if (not (eobp))
                  (beginning-of-line))

              ;; Fill using fill-prefix
              (fill-region-as-paragraph start (point))))))))


(defun visual-basic-fill-or-indent ()
  "Fill long comment around point, if any, else indent current definition."
  (interactive)
  (cond ((save-excursion
           (beginning-of-line)
           (looking-at visual-basic-comment-regexp))
         (visual-basic-fill-long-comment))
        (t
         (visual-basic-indent-defun))))


(defun visual-basic-new-sub ()
  "Insert template for a new subroutine.  Repeat to cycle through alternatives."
  (interactive)
  (beginning-of-line)
  (let ((templates (cons visual-basic-blank-regexp
                         visual-basic-defn-templates))
        (tem nil)
        (bound (point)))
    (while templates
      (setq tem (car templates)
            templates (cdr templates))
      (cond ((looking-at tem)
             (replace-match (or (car templates)
                                ""))
             (setq templates nil))))

    (search-backward "()" bound t)))


(defun visual-basic-untabify ()
  "Do not allow any tabs into the file."
  (if (eq major-mode 'visual-basic-mode)
      (untabify (point-min) (point-max)))
  nil)

(defun visual-basic-default-tag ()
  "Return default TAG at point to search by grep."
  ;; VB1 to Lennart: is border effect on match-data an issue
  (if (and (not (bobp))
           (save-excursion
             (backward-sexp)
             (looking-at "\\w")))
      (backward-word 1))
  (let ((s (point))
        (e (save-excursion
             (forward-sexp)
             (point))))
    (buffer-substring s e)))

(defun visual-basic-grep (tag)
  "Search BASIC source files in current directory for TAG."
  (interactive
   (list (let* ((def (visual-basic-default-tag))
                (tag (read-string
                      (format "Grep for [%s]: " def))))
           (if (string= tag "") def tag))))
  (grep (format "grep -n %s %s" tag visual-basic-wild-files)))


;;; IDE Connection.

(defun visual-basic-buffer-project-file ()
  "Return a guess as to the project file associated with the current buffer."
  (car (directory-files (file-name-directory (buffer-file-name)) t "\\.vbp")))

(defun visual-basic-start-ide ()
  "Start Visual Basic (or your favorite IDE, (after Emacs, of course))
on the first project file in the current directory.
Note: it's not a good idea to leave Visual Basic running while you
are editing in Emacs, since Visual Basic has no provision for reloading
changed files."
  (interactive)
  (let (file)
    (cond ((null visual-basic-ide-pathname)
           (error "No pathname set for Visual Basic.  See visual-basic-ide-pathname"))
          ((null (setq file (visual-basic-buffer-project-file)))
           (error "No project file found"))
          ((fboundp 'win-exec)
           (suspend-frame)
           (win-exec visual-basic-ide-pathname 'win-show-normal file))
          ((fboundp 'start-process)
           (iconify-frame (selected-frame))
           (start-process "*VisualBasic*" nil visual-basic-ide-pathname file))
          (t
           (error "No way to spawn process!")))))



;;; Indentation-related stuff.

(defun visual-basic-indent-region (start end)
  "Perform `visual-basic-indent-line' on each line in region delimited by START and END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (while (and (not (eobp))
                (< (point) end))
      (if (not (looking-at visual-basic-blank-regexp))
          (visual-basic-indent-line))
      (forward-line 1)))

  (cond ((fboundp 'zmacs-deactivate-region)
         (zmacs-deactivate-region))
        ((fboundp 'deactivate-mark)
         (deactivate-mark))))



(defun visual-basic-previous-line-of-code ()
  "Set point on previous line of code, skipping any blank or comment lines."
  (if (not (bobp))
      (forward-line -1))        ; previous-line depends on goal column
  (while (and (not (bobp))
              (or (looking-at visual-basic-blank-regexp)
                  (looking-at visual-basic-comment-regexp)))
    (forward-line -1)))

(defun visual-basic-next-line-of-code ()
  "Set point on next line of code, skipping any blank or comment lines."
  (if (null (eobp))
      (forward-line 1))        ; next-line depends on goal column
  (while (and (null (eobp))
              (looking-at visual-basic-comment-regexp))
    (forward-line 1)))


(defun visual-basic-find-original-statement ()
  "If the current line is a continuation, move back to the original stmt."
  (let ((here (point)))
    (visual-basic-previous-line-of-code)
    (while (and (not (bobp))
                (looking-at visual-basic-continuation-regexp))
      (setq here (point))
      (visual-basic-previous-line-of-code))
    (goto-char here)))

(defun visual-basic-find-predicate-matching-stmt (open-p close-p)
  "Find opening statement statisfying OPEN-P predicate for which
matching closing statement statisfies CLOSE-P predicate.

Point is set on line statifying OPEN-P predicate, with ignoring
any line satifying OPEN-P but for which a matching line
statifying CLOSE-P was visited before during this search."
  ;; Searching backwards
  (let ((level 0))
    (while (and (>= level 0) (not (bobp)))
      (visual-basic-previous-line-of-code)
      (visual-basic-find-original-statement)
      (cond ((funcall close-p)
             (setq level (+ level 1)))
            ((funcall open-p)
             (setq level (- level 1)))))))

(defun visual-basic-find-matching-stmt (open-regexp close-regexp)
  "Same as function `visual-basic-find-predicate-matching-stmt' except that regexps OPEN-REGEXP CLOSE-REGEXP are supplied instead of predicate, equivalent predicate being to be looking at those regexps."
  (visual-basic-find-predicate-matching-stmt
   (lambda () (looking-at open-regexp))
   (lambda () (looking-at close-regexp))))

(defun visual-basic-get-complete-tail-of-line ()
  "Return the tail of the current statement line, starting at
point and going up to end of statement line. If you want the
complete statement line, you have to call functions
`visual-basic-find-original-statement' and then
`beginning-of-line' before"
  (let* ((start-point (point))
	 complete-line
	 (line-beg start-point)
	 line-end)
    (while (null line-end)
      (end-of-line)
      (setq line-end (point))
      (if (search-backward "_" line-beg t)
	  (if (looking-at  visual-basic-looked-at-continuation-regexp)
	      ;; folded line
	      (progn
		(setq line-end (1- (point))
		      complete-line (cons
				     (buffer-substring-no-properties
				      line-beg line-end)
				     complete-line)
		      line-end nil)
		(beginning-of-line 2)
		(setq line-beg (point)))
	    ;; _ found, but not a folded line (this is a syntax error)
	    (setq complete-line
		  (cons (buffer-substring-no-properties line-beg line-end) complete-line)))
	;; not a folded line
	(setq complete-line
	      (cons (buffer-substring-no-properties line-beg line-end)
		    complete-line))))
    (mapconcat 'identity (nreverse complete-line) " ")))

(defun visual-basic-if-not-on-single-line ()
  "Return non-`nil' when the If statement is not on a single statement
line, i.e. requires a matching End if. Note that a statement line may
be folded over several code lines."
  (if (looking-at visual-basic-if-regexp)
      (save-excursion
	(beginning-of-line)
	(let (p1
	      p2
	      ;; 1st reconstruct complete line
	      (complete-line (visual-basic-get-complete-tail-of-line)) )

	  ;; now complete line has been reconstructed, drop confusing elements

	  ;; remove any VB string from complete line, as strings may disrupt : and ' detection
	  (while (and (setq p1 (string-match "\"" complete-line))
		      (setq p2 (string-match "\"" complete-line (1+ p1))))
	    (setq complete-line (concat (substring complete-line 0 p1)
					(substring complete-line (1+ p2)))))
	  ;; now drop tailing comment if any
	  (when (setq p1 (string-match "'" complete-line))
	    (setq complete-line (substring complete-line 0 (1- p1))))
	  ;; now drop 1st concatenated instruction if any
	  (when (setq p1 (string-match ":" complete-line))
	    (setq complete-line (substring complete-line p1)))
	  ;;
	  (string-match "Then\\s-*$" complete-line))); end (save-excursion ...)
    ;; else, not a basic if
    nil))

(defun visual-basic-find-matching-if ()
  "Set pointer on the line with If stating the If ... Then ... [Else/Elseif ...] ... End If block containing pointer."
  (visual-basic-find-predicate-matching-stmt 'visual-basic-if-not-on-single-line
                                             (lambda () (looking-at visual-basic-endif-regexp))))

(defun visual-basic-find-matching-select ()
  "Set pointer on the line with Select Case stating the Select Case ... End Select block containing pointer."
  (visual-basic-find-matching-stmt visual-basic-select-regexp
                                   visual-basic-select-end-regexp))

(defun visual-basic-find-matching-for ()
  "Set pointer on the line with For stating the `For ... Next' block containing pointer."
  (visual-basic-find-matching-stmt visual-basic-for-regexp
                                   visual-basic-next-regexp))

(defun visual-basic-find-matching-do ()
  "Set pointer on the line with Do stating the `Do ... Loop' block containing pointer."
  (visual-basic-find-matching-stmt visual-basic-do-regexp
                                   visual-basic-loop-regexp))

(defun visual-basic-find-matching-while ()
  "Set pointer on the line with While stating the `While ... Wend' block containing pointer."
  (visual-basic-find-matching-stmt visual-basic-while-regexp
                                   visual-basic-wend-regexp))

(defun visual-basic-find-matching-with ()
  "Set pointer on the line with With stating the `With ... End with' block containing pointer."
  (visual-basic-find-matching-stmt visual-basic-with-regexp
                                   visual-basic-end-with-regexp))

;;; If this fails it must return the indent of the line preceding the
;;; end not the first line because end without matching begin is a
;;; normal simple statement
(defun visual-basic-find-matching-begin ()
  "Set pointer on the line with Begin stating the `Begin ... End' block containing pointer."
  (let ((original-point (point)))
    (visual-basic-find-matching-stmt visual-basic-begin-regexp
                                     visual-basic-end-begin-regexp)
    (if (bobp) ;failed to find a matching begin so assume that it is
                                        ;an end statement instead and use the indent of the
                                        ;preceding line.
        (progn (goto-char original-point)
               (visual-basic-previous-line-of-code)))))


(defun visual-basic-calculate-indent ()
  "Return indent count for the line of code containing pointer."
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)
      ;; Some cases depend only on where we are now.
      (cond ((or (looking-at visual-basic-defun-start-regexp)
                 (looking-at visual-basic-label-regexp)
                 (looking-at visual-basic-defun-end-regexp))
             0)

            ;; The outdenting stmts, which simply match their original.
            ((or (looking-at visual-basic-else-regexp)
                 (looking-at visual-basic-endif-regexp))
             (visual-basic-find-matching-if)
             (current-indentation))

            ;; All the other matching pairs act alike.
            ((looking-at visual-basic-next-regexp) ; for/next
             (visual-basic-find-matching-for)
             (current-indentation))

            ((looking-at visual-basic-loop-regexp) ; do/loop
             (visual-basic-find-matching-do)
             (current-indentation))

            ((looking-at visual-basic-wend-regexp) ; while/wend
             (visual-basic-find-matching-while)
             (current-indentation))

            ((looking-at visual-basic-end-with-regexp) ; with/end with
             (visual-basic-find-matching-with)
             (current-indentation))

            ((looking-at visual-basic-select-end-regexp) ; select case/end select
             (visual-basic-find-matching-select)
             (current-indentation))

            ;; A case of a select is somewhat special.
            ((looking-at visual-basic-case-regexp)
             (visual-basic-find-matching-select)
             (+ (current-indentation) visual-basic-mode-indent))

            ;; Added KJW: Make sure that this comes after the cases
            ;; for if..endif, end select because end-regexp will also
            ;; match "end select" etc.
            ((looking-at visual-basic-end-begin-regexp) ; begin/end
             (visual-basic-find-matching-begin)
             (current-indentation))

            (t
             ;; Other cases which depend on the previous line.
             (visual-basic-previous-line-of-code)

             ;; Skip over label lines, which always have 0 indent.
             (while (looking-at visual-basic-label-regexp)
               (visual-basic-previous-line-of-code))

             (cond
              ((looking-at visual-basic-continuation-regexp)
               (visual-basic-find-original-statement)
               ;; Indent continuation line under matching open paren,
               ;; or else one word in.
               (let* ((orig-stmt (point))
                      (matching-open-paren
                       (condition-case ()
                           (save-excursion
                             (goto-char original-point)
                             (beginning-of-line)
                             (backward-up-list 1)
                             ;; Only if point is now w/in cont. block.
                             (if (<= orig-stmt (point))
                                 (current-column)))
                         (error nil))))
                 (cond (matching-open-paren
                        (1+ matching-open-paren))
                       (t
                        ;; Else, after first word on original line.
                        (back-to-indentation)
                        (forward-word 1)
                        (while (looking-at "[ \t]")
                          (forward-char 1))
                        (current-column)))))
              (t
               (visual-basic-find-original-statement)

               (let ((indent (current-indentation)))
                 ;; All the various +indent regexps.
                 (cond ((looking-at visual-basic-defun-start-regexp)
                        (+ indent visual-basic-mode-indent))

                       ((or (visual-basic-if-not-on-single-line)
                            (and (looking-at visual-basic-else-regexp)
                                 (not (and visual-basic-allow-single-line-if
                                           (looking-at visual-basic-ifthen-regexp)))))
                        (+ indent visual-basic-mode-indent))

                       ((or (looking-at visual-basic-select-regexp)
                            (looking-at visual-basic-case-regexp))
                        (+ indent visual-basic-mode-indent))

                       ((or (looking-at visual-basic-do-regexp)
                            (looking-at visual-basic-for-regexp)
                            (looking-at visual-basic-while-regexp)
                            (looking-at visual-basic-with-regexp)
                            (looking-at visual-basic-begin-regexp))
                        (+ indent visual-basic-mode-indent))

                       (t
                        ;; By default, just copy indent from prev line.
                        indent))))))))))

(defun visual-basic-indent-to-column (col)
  "Indent line of code containing pointer up to column COL."
  (let* ((bol (save-excursion
                (beginning-of-line)
                (point)))
         (point-in-whitespace
          (<= (point) (+ bol (current-indentation))))
         (blank-line-p
          (save-excursion
            (beginning-of-line)
            (looking-at visual-basic-blank-regexp))))

    (cond ((/= col (current-indentation))
           (save-excursion
             (beginning-of-line)
             (back-to-indentation)
             (delete-region bol (point))
             (indent-to col))))

    ;; If point was in the whitespace, move back-to-indentation.
    (cond (blank-line-p
           (end-of-line))
          (point-in-whitespace
           (back-to-indentation)))))


(defun visual-basic-indent-line ()
  "Indent current line for BASIC."
  (interactive)
  (visual-basic-indent-to-column (visual-basic-calculate-indent)))


(defun visual-basic-split-line ()
  "Split line at point, adding continuation character or continuing a comment.
In Abbrev mode, any abbrev before point will be expanded."
  (interactive)
  (let ((pps-list (parse-partial-sexp (save-excursion
                                        (beginning-of-line)
                                        (point))
                                      (point))))
    ;; Dispatch on syntax at this position.
    (cond ((equal t (nth 4 pps-list))  ; in comment
           (indent-new-comment-line))
          ((equal t (nth 4 pps-list))   ; in string
           (error "Can't break line inside a string"))
          (t (just-one-space)           ; leading space on next line
                                        ; doesn't count, sigh
             (insert "_")
             (visual-basic-newline-and-indent)))))

(defun visual-basic-detect-idom ()
  "Detects whether this is a VBA or VBS script. Returns symbol
`vba' if it is VBA, `nil' otherwise."
  (let (ret
        (case-fold-search t))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(cond
	 ((looking-at "^\\s-*Attribute\\s-+VB_Name\\s-+= ")
	  (setq ret 'vba))
	 ((looking-at "^\\s-*Version\\s-+[^ \t\n\r]+Class\\s-*$")
	  (setq ret 'vba)))
        ))
    ret))

(defun visual-basic-close-block ()
  "Insert `End If' is current block is a `If Then ...', `End
With' if the block is a `With ...', etc..."
  (interactive)
  (let (end-statement end-indent)
    (save-excursion
      (save-match-data
	(while
	    (unless  (bobp)
	      (visual-basic-previous-line-of-code)
	      (visual-basic-find-original-statement)
	      (cond
	       ;; Cases where the current statement is a start-of-smthing statement
	       ((looking-at visual-basic-defun-start-regexp)
		(let ((smt (match-string 2)))
		  (when (string-match "\\`Prop" smt)
		    (setq smt "Property"))
		  (setq end-statement (concat "End " smt)
			end-indent 0))
		nil)
	       ((looking-at visual-basic-select-regexp)
		(setq  end-statement "End Select"
		       end-indent (current-indentation))
		nil)
	       ((looking-at visual-basic-with-regexp)
		(setq  end-statement "End With"
		       end-indent (current-indentation))
		nil)
	       ((looking-at visual-basic-case-regexp)
		(setq  end-statement  "End Select"
		       end-indent (max 0 (- (current-indentation) visual-basic-mode-indent)))
		nil)
	       ((looking-at visual-basic-begin-regexp)
		(setq  end-statement "End"
		       end-indent (current-indentation))
		nil)
	       ((or (visual-basic-if-not-on-single-line)
		    (looking-at visual-basic-else-regexp))
		(setq  end-statement "End If"
		       end-indent (current-indentation))
		nil)

	       ((looking-at visual-basic-do-regexp)
		(setq  end-statement "Loop"
		       end-indent (current-indentation))
		nil)

	       ((looking-at visual-basic-while-regexp)
		(setq  end-statement "Wend"
		       end-indent (current-indentation))
		nil)

	       ((looking-at visual-basic-for-regexp)
		(goto-char (match-end 0))
		(setq  end-statement "Next"
		       end-indent (current-indentation))
		(let ((vb-idom (visual-basic-detect-idom)))
		  (cond
		   ;; for VBA add the variable name after Next.
		   ((eq vb-idom 'vba)
		    (when (looking-at "\\s-+\\(Each\\s-+\\|\\)\\([^ \t\n\r]+\\)")
		      (setq end-statement (concat end-statement " " (match-string 2)))))))
		nil)
	       ;; Cases where the current statement is an end-of-smthing statement
	       ((or (looking-at visual-basic-else-regexp)
		    (looking-at visual-basic-endif-regexp))
		(visual-basic-find-matching-if)
		t)
	       ((looking-at visual-basic-next-regexp) ; for/next
		(visual-basic-find-matching-for)
		t)
	       ((looking-at visual-basic-loop-regexp) ; do/loop
		(visual-basic-find-matching-do)
		t)
	       ((looking-at visual-basic-wend-regexp) ; while/wend
		(visual-basic-find-matching-while)
		t)
	       ((looking-at visual-basic-end-with-regexp) ; with/end with
		(visual-basic-find-matching-with)
		t)
	       ((looking-at visual-basic-select-end-regexp) ; select case/end select
		(visual-basic-find-matching-select)
		t)


	       ;; default is to loop again back to previous line of code.
	       (t t))))))
    (when end-statement
      (insert end-statement)
      (visual-basic-indent-to-column end-indent))))

(defun visual-basic-insert-item ()
  "Insert a new item in a block.

This function is under developement, and for the time being only
Dim and Case items are handled.

Interting an item means:

* Add a `Case' or `Case Else' into a `Select ... End Select'
  block. **under construction** Pressing again toggles between
  `Case' and `Case Else'. `Case Else' is possible only if there
  is not already a `Case Else'.

* Split a Dim declaration over several lines. Split policy is
  that:

  - the split always occur just before or just after the
    declaration of the variable V such that the pointer is
    located over this declaration. For instance if the
    declaration is `V(2) as T' then pointer position maybe any
    `^' as follows:

       Dim X, V(2) As T, Y
              ^^^^^^^^^^^

  - the split being after or before `V(2) as T' decalration and
    the position of pointer after split depends on where the
    pointer was before the split:

    - if the pointer is over variable name (but with array size
      inclusive) like this:

       Dim X, V(2) As T, Y
              ^^^^

      then the split is as follows (split is before declaration
      and pointer goes to next line):

       Dim X
       Dim V(2) As T, Y
           ^

    - if the pointer is not over variable name like this:


       Dim X, V(2) As T, Y
                  ^^^^^^^

      then the split is as follows (split is after declaration
      and pointer remains on same line):

       Dim X, V(2) As T
                       ^
       Dim Y

* **under construction** Add an `Else' or `ElseIf ... Then' into
  an `If ... Then ... End If' block.  Pressing again toggles
  between `Else' and `ElseIf ... Then'.  `Else' is possible only
  if therei s not already an `Else'."
  (interactive)
  ;; possible cases are

  ;; dim-split-before => pointer remains before `Dim' inserted by split
  ;; dim-split-after => pointer goes after `Dim' inserted by split
  ;; if-with-else
  ;; if-without-else
  ;; select-with-else
  ;; select-without-else
  ;; not-itemizable
  (let (item-case
	item-ident
	split-point
	org-split-point
	prefix
	is-const
	tentative-split-point
	block-stack (cur-point (point)) previous-line-of-code)
    (save-excursion
      (save-match-data
	(beginning-of-line)
	(while
	    (progn
	      (visual-basic-find-original-statement)
	      (cond
	       ;; dim case
	       ;;--------------------------------------------------------------
	       ((and (null previous-line-of-code)
		     (looking-at visual-basic-dim-regexp)
		     (null (save-match-data (looking-at visual-basic-defun-start-regexp))))
		(setq prefix (buffer-substring-no-properties
			      (point)
			      (goto-char (setq split-point (match-end 0)
					       org-split-point split-point)))
		      is-const (string-match "\\_<Const\\_>" prefix)
		      item-case ':dim-split-after)
		;; determine split-point, which is the point at which a new
		;; Dim item is to be inserted. To that purpose the line is gone through
		;; from beginning until cur-point is past
		(while
                    (if
			(looking-at "\\(\\s-*\\)\\(?:\\sw\\|\\s_\\)+\\s-*"); some symbol
			(if (>  (setq tentative-split-point (match-end 0)) cur-point)
                            (progn
			      (setq item-case (if (>= cur-point (match-end 1))
						  ':dim-split-after
                                                ':dim-split-before))
			      nil;; stop loop
			      )
			  (goto-char tentative-split-point)
			  (setq item-case ':dim-split-before)
			  (let ((loop-again t))
			    (while
				(or
				 ;; array variable
				 (when (looking-at "\\(([^)\n]+)\\)\\s-*")
                                   (if (< cur-point (match-end 1))
                                       (setq item-case ':dim-split-after
                                             loop-again nil)
                                     t))
				 ;; continuation
				 (and loop-again
				      (looking-at visual-basic-looked-at-continuation-regexp) ))
                              (goto-char (setq tentative-split-point (match-end 0))))
			    (when loop-again
			      (when (looking-at "As\\s-+\\(?:\\sw\\|\\s_\\)+\\s-*")
				(setq item-case ':dim-split-after)
				(goto-char (setq tentative-split-point (match-end 0))))
			      (when (looking-at visual-basic-looked-at-continuation-regexp)
				(beginning-of-line 2))
			      (if (looking-at ",")
				  (goto-char (setq split-point (match-end 0)))
				(setq split-point (point))
				nil))))
		      nil))
		;; now make the split. This means that some comma may need to be deleted.
		(goto-char split-point)
		(looking-at "\\s-*")
		(delete-region split-point (match-end 0))
		(cond
		 ((looking-back ",")
		  (while
		      (progn
			(delete-region split-point (setq split-point (1- split-point)))
			(looking-back "\\s-"))))
		 ((= split-point org-split-point)
		  (insert " ")
		  (setq split-point (point))))
		(insert "\n" prefix " ")
		(setq cur-point (point))
		nil)

	       ;;  case of Case (in Select ... End Select)
	       ;;----------------------------------------------------------------------
	       ((looking-at visual-basic-case-regexp)
		(if (looking-at visual-basic-case-else-regexp)
		    ;; if within a Case Else statement, then insert
		    ;; a Case just before with same indentation
		    (let ((indent (current-indentation)))
		      (beginning-of-line)
		      (insert "Case ")
		      (visual-basic-indent-to-column indent)
		      (setq item-case ':select-with-else
			    split-point (point))
		      (insert ?\n))
		  (setq item-case ':select-without-else))
		nil; break loop
		)

	       ;; next
	       ((looking-at visual-basic-next-regexp)
		(push (list 'next) block-stack))
	       ;; default
	       ;;--------------------------------------------------------------
	       (t (if (bobp)
		      (setq item-case 'not-itemizable)))
	       )
	      (when (null item-case)
		(visual-basic-previous-line-of-code)
		(setq previous-line-of-code t))
	      (null item-case)))))
    (case item-case
      ((:dim-split-after)   (message "split after") (goto-char cur-point))
      ((:dim-split-before)  (message "split before") (goto-char split-point))
      ((:select-with-else)  (goto-char split-point))
      ((:select-without-else)
       ;; go forward until the End Select or next case is met in order to
       ;; to insert the new case at this position
       (let ((select-case-depth 0))
	 (while
	     (progn
	       (visual-basic-next-line-of-code)
               (cond
		;; case was found, insert case and exit loop
		((and (= 0 select-case-depth)
		      (looking-at visual-basic-case-regexp))
		 (let ((indent (current-indentation)))
		   (beginning-of-line)
		   (insert "Case ")
		   (visual-basic-indent-to-column indent)
		   (save-excursion (insert ?\n))
		   nil))
		((looking-at visual-basic-select-regexp)
		 (setq select-case-depth (1+ select-case-depth))
		 (if
		     (re-search-forward (concat visual-basic-select-regexp
						"\\|"
						visual-basic-select-end-regexp)
					nil nil)
		     (progn
		       (beginning-of-line)
		       t ; loop again
		       )
		   (let ((l (line-number-at-pos)))
		     (goto-char cur-point)
		     (error "Select Case without matching end at line %d" l))))
		((looking-at visual-basic-select-end-regexp)
		 (setq select-case-depth (1- select-case-depth))
		 (if (= select-case-depth -1)
		     (let ((indent (current-indentation)))
		       (insert  "Case ")
		       (save-excursion (insert ?\n ))
		       (visual-basic-indent-to-column
		        (+ indent visual-basic-mode-indent))
		       nil;; break loop
                       )
		   t; loop again
                   ))
		((eobp)
		 (goto-char cur-point)
		 (error "Case without ending"))
		;; otherwise loop again
		(t t)))))) ; end of select-case-without-else
      )))

(defun visual-basic-propertize-attribute ()
  "Insert Let/Set and Get property functions suitable to
manipulate some private attribute, the cursor is assumed to be on
the concerned attribute declartion"
  (interactive)

  (save-excursion
    (save-match-data
      (beginning-of-line)
      (let (variable property type lettable pos type-prefix)
	(if (looking-at "^\\s-*Private\\s-+\\(\\sw\\(?:\\sw\\|\\s_\\)*\\)\\s-+As\\s-+\\(\\sw\\(?:\\sw\\|\\s_\\)*\\)")
	    (progn
	      (setq variable (match-string 1)
		    type (match-string 2)
		    lettable (string-match visual-basic-lettable-type-regexp type))
	      (if (string-match (concat "\\`"
					visual-basic-variable-scope-prefix-re
					"\\(" visual-basic-variable-type-prefix-re "\\)")
				variable)
		  (setq 
		   type-prefix (match-string 1 variable)
		   property (substring variable (match-end 0)))
		(setq type-prefix ""
		      property variable))
	      (beginning-of-line 2)
	      (insert
	       "Property " (if lettable "Let" "Set") " " property "(" 
	       (if lettable "ByVal " "")
	       type-prefix "NewValue_IN As " type ")\n"
	       "\t"  (if lettable "Let" "Set") " " variable " = " type-prefix "NewValue_IN\nEnd Property\n"
	       "Property Get " property "() As " type "\n"
	       "\t"  (if lettable "Let" "Set") " " property " = " variable "\nEnd Property\n"))
	  (error "Not on a propertizable variable declaration."))))))


;;; Some experimental functions

;;; Load associated files listed in the file local variables block
(defun visual-basic-load-associated-files ()
  "Load files that are useful to have around when editing the
source of the file that has just been loaded.  The file must have
a local variable that lists the files to be loaded.  If the file
name is relative it is relative to the directory containing the
current buffer.  If the file is already loaded nothing happens,
this prevents circular references causing trouble.  After an
associated file is loaded its associated files list will be
processed."
  (if (boundp 'visual-basic-associated-files)
      (let ((files visual-basic-associated-files)
            (file nil))
        (while files
          (setq file (car files)
                files (cdr files))
          (message "Load associated file: %s" file)
          (visual-basic-load-file-ifnotloaded file default-directory)))))



(defun visual-basic-load-file-ifnotloaded (file default-directory)
  "Load file if not already loaded.
If FILE is relative then DEFAULT-DIRECTORY provides the path."
  (let((file-absolute (expand-file-name file default-directory)))
    (if (get-file-buffer file-absolute); don't do anything if the buffer is already loaded
        ()
      (find-file-noselect file-absolute ))))

(defun visual-basic-check-style ()
  "Check coding style of currently open buffer, and make
corrections under the control of user.

This function is under construction"
  (interactive)
  (flet
      ((insert-space-at-point
	()
	(insert " "))
       ;; avoid to insert space inside a floating point number
       (check-plus-or-minus-not-preceded-by-space-p
	()
	(save-match-data
	  (and
	   (visual-basic-in-code-context-p)
	   (null (looking-back "\\([0-9]\\.\\|[0-9]\\)[eE]")))))
       (check-plus-or-minus-not-followed-by-space-p
	()
	(save-match-data
	  (and
	   (visual-basic-in-code-context-p)
	   (null  (looking-at "\\(\\sw\\|\\s_\\|\\s\(\\|[.0-9]\\)"))
	   (null (looking-back "\\([0-9]\\.\\|[0-9]\\)[eE]\\|,\\s-*\\(\\|_\\s-*\\)\\|:=\\s-*")))));
       (check-comparison-sign-not-followed-by-space-p
	()
	(save-match-data
	  (and
	   (visual-basic-in-code-context-p)
	   (let ((next-char (match-string 2))
		 (str--1 (or (= (match-beginning 1) (point-min))
			     (buffer-substring-no-properties (1- (match-beginning 1))
							     (1+ (match-beginning 1))))))
	     (null (or
		    (and (stringp str--1)
			 (string= str--1 ":="))
		    (string-match "[<=>]" next-char ))) ))));
       (replace-by-&
	()
	(goto-char (1- (point)))
	(let* ((p1 (point))
	       (p2 (1+ p1)))
	  (while (looking-back "\\s-")
	    (goto-char (setq p1 (1- p2))))
	  (goto-char p2)
	  (when (looking-at "\\s-+")
	    (setq p2 (match-end 0)))
	  (delete-region p1 p2)
	  (insert " & ")));
       (check-string-concatenation-by-+
	()
	(save-match-data
	  (and
	   (visual-basic-in-code-context-p)
	   (or
	    (looking-at "\\s-*\\(\\|_\n\\s-*\\)\"")
	    (looking-back "\"\\(\\|\\s-*_\\s-*\n\\)\\s-*\\+")))));
       )
    (let (vb-other-buffers-list
	  ;; list of found error styles
	  ;; each element is a list (POSITION PROMPT ERROR-SOLVE-HANDLER)
	  next-se-list
	  next-se
	  case-fold-search
	  (hl-style-error (make-overlay 1 1)); to be moved
	  (style-errors
	   '(
	     ;; each element is a vector
	     ;;   0	 1	2	3	  4		      5		    6
	     ;; [ REGEXP PROMPT GET-POS RE-EXP-NB ERROR-SOLVE-HANDLER ERROR-CONFIRM LEVEL]
	     [ "\\(\\s\)\\|\\sw\\|\\s_\\)[-+]"
	       "Plus or minus not preceded by space"
	       match-end 1
	       insert-space-at-point
	       check-plus-or-minus-not-preceded-by-space-p
	       0 ]
	     [ "\\(\\s\)\\|\\sw\\|\\s_\\)[/\\*&]"
	       "Operator not preceded by space"
	       match-end 1
	       insert-space-at-point
	       visual-basic-in-code-context-p
	       0 ]
	     [ "[/\\*&]\\(\\s\(\\|\\sw\\|\\s_\\|\\s.\\)"
	       "Operator not followed by space"
	       match-beginning 1
	       insert-space-at-point
	       visual-basic-in-code-context-p
	       0 ]
	     [ "[-+]\\(\\s\(\\|\\sw\\|\\s_\\|\\s.\\)"
	       "Plus or minus not followed by space"
	       match-beginning 1
	       insert-space-at-point
	       check-plus-or-minus-not-followed-by-space-p
	       0 ]
	     [ "\\(\\s\)\\|\\sw\\|\\s_\\)\\(=\\|<\\|>\\)"
	       "Comparison sign not preceded by space"
	       match-end 1
	       insert-space-at-point
	       visual-basic-in-code-context-p
	       0 ]
	     [ "\\(=\\|<\\|>\\)\\(\\s\(\\|\\sw\\|\\s_\\|\\s.\\)"
	       "Comparison sign not followed by space"
	       match-end 1
	       insert-space-at-point
	       check-comparison-sign-not-followed-by-space-p
	       0 ]
	     [ ",\\(\\sw\\|\\s_\\)"
	       "Comma not followed by space"
	       match-beginning 1
	       insert-space-at-point
	       visual-basic-in-code-context-p
	       0 ]
	     [ "\\+"
	       "String should be concatenated with & rather than with +"
	       match-end 0
	       replace-by-&
	       check-string-concatenation-by-+
	       0 ]
	     )); end of style error types
	  )
      (condition-case nil 
	  (progn
	    (overlay-put hl-style-error 'face hl-line-face)
	    (overlay-put hl-style-error 'window (selected-window))
	    (dolist (x (buffer-list))
	      (if (and (save-excursion
			 (set-buffer x)
			 (derived-mode-p 'visual-basic-mode))
		       (null (eq x (current-buffer))))
		  (push x vb-other-buffers-list)))
	    (save-excursion
	      (save-restriction
		(widen)
		(goto-char (point-min))
		(while
		    (progn
		      (setq next-se-list nil)
		      (dolist (se style-errors)
			(save-excursion
			  (when
			      (and
			       (re-search-forward (aref se 0) nil t)
			       (progn
				 (goto-char  (funcall (aref se 2)
						      (aref se 3)))
				 (or (null (aref se 5))
				     (funcall  (aref se 5))
				     (let (found)
				       (while (and
					       (setq found (re-search-forward (aref se 0) nil t))
					       (null (progn
						       (goto-char  (funcall (aref se 2)
									    (aref se 3)))
						       (funcall  (aref se 5))))))
				       found))))
			    (push (list (point)
					(match-beginning 0) 
					(match-end 0)
					(aref se 1)
					(and (> (aref se 6) visual-basic-auto-check-style-level)
					     (aref se 4)))
				  next-se-list))))
		      (when next-se-list
			(setq next-se-list
			      (sort next-se-list (lambda (x y) (< (car x) (car y))))
			      next-se (pop next-se-list))
			(goto-char (pop next-se))
			(move-overlay hl-style-error (pop next-se) (pop next-se))
			(when (y-or-n-p (concat (pop next-se)
						", solve it ? "))
			  (funcall (pop next-se)))
			t; loop again
			))))) )
	;; error handlers
	(delete-overlay hl-style-error))
      (delete-overlay hl-style-error)))
  (message "Done Visual Basic style check"))

(provide 'visual-basic-mode)



;;; visual-basic-mode.el ends here


;External Links
;* [http://visualbasic.freetutes.com/ Visual Basic tutorials]
;* [http://en.wikibooks.org/wiki/Visual_Basic/Coding_Standards]
