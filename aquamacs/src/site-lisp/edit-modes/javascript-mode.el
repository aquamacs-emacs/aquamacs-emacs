;;; javascript-mode.el --- major mode for editing JavaScript code

;; Copyright (C) 1997-2001 Steven Champeon
;;               2002-2004 Ville Skyttä

;; Author:     1997 Steven Champeon <schampeo@hesketh.com>
;; Maintainer: Ville Skyttä <scop@xemacs.org>
;; Keywords:   languages javascript

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Synched up with: not in GNU Emacs.
;;                  ... but in Aquamacs

;;; Commentary:

;; javascript-mode was originally derived from java-cust.el
;; (by Jonathan Payne) by Steven Champeon. It has been modified
;; a lot afterwards by Ville Skyttä.

;; Contributors:
;;   Sreng Truong      (bug fix for 21.1)
;;   Sebastian Delmont (fix for prototype function indentation problems)
;;   Stefan Schlee     (GNU Emacs compatibility fixes)
;;   Igor Rayak        (ditto)

;; TODO:
;; - Multiple font-lock/highlight levels.
;; - Investigate if Semantic Bovinator should be used.
;; - Check syntax-table stuff.

;;; Code:

(require 'cc-mode)
(require 'comint)

(eval-when-compile
  (require 'regexp-opt)
  (require 'font-lock)
  (require 'speedbar)
  )

;; ------------------------------------------------------------------------ ;;

(defconst javascript-mode-version "1.9" "Version of `javascript-mode'.")

;; ------------------------------------------------------------------------ ;;

(defgroup javascript nil
  "Major mode for editing JavaScript code."
  :tag "JavaScript"
  :group 'languages
  :prefix "javascript-")

(defcustom javascript-mode-hook nil
  "Hook for customizing `javascript-mode'."
  :group 'javascript
  :type 'hook)

(defgroup javascript-shell nil
  "JavaScript shell options."
  :group 'javascript
  :prefix "javascript-shell-")

(defcustom javascript-shell-command "jsshell"
  "*Command for starting `javascript-shell'.
Set arguments for this command in `javascript-shell-command-args'."
  :type 'string
  :group 'javascript-shell)

(defcustom javascript-shell-command-args '()
  "*Command line arguments for `javascript-shell-command'."
  :type '(repeat (string :tag "Argument"))
  :group 'javascript-shell)

(defcustom javascript-shell-prompt-pattern "^js> *"
  "*JavaScript shell prompt pattern."
  :type 'regexp
  :group 'javascript-shell)

(defcustom javascript-shell-mode-hook nil
  "Hook for customizing `javascript-shell-mode'."
  :type 'hook
  :group 'javascript-shell)

;; ------------------------------------------------------------------------ ;;

(defvar javascript-mode-abbrev-table nil
  "Abbrev table in use in `javascript-mode' buffers.")
(define-abbrev-table 'javascript-mode-abbrev-table ())

;; ------------------------------------------------------------------------ ;;

(defvar javascript-mode-map (c-make-inherited-keymap)
  "Keymap used in `javascript-mode' buffers.")

(defvar javascript-menu nil)
(easy-menu-define javascript-menu javascript-mode-map
                  "JavaScript Mode Commands" (c-mode-menu "JavaScript"))

;; ------------------------------------------------------------------------ ;;

;; Reserved words in JavaScript.
(defconst javascript-reserved-words
  (eval-when-compile
    (regexp-opt
     '(
       "abstract"
       "boolean"
       "break"
       "byte"
       "case"
       "catch"
       "char"
       "class"
       "const"
       "continue"
       "debugger"
       "default"
       "delete"
       "do"
       "double"
       "else"
       "enum"
       "export"
       "extends"
       "false"
       "final"
       "finally"
       "float"
       "for"
       "function"
       "goto"
       "if"
       "implements"
       "import"
       "in"
       "instanceof"
       "int"
       "interface"
       "long"
       "native"
       "new"
       "null"
       "package"
       "private"
       "protected"
       "public"
       "return"
       "short"
       "static"
       "super"
       "switch"
       "synchronized"
       "this"
       "throw"
       "throws"
       "transient"
       "true"
       "try"
       "typeof"
       "var"
       "void"
       "volatile"
       "while"
       "with"
       ) t))
  "Expression for matching reserved words in `javascript-mode' buffers.

From Core JavaScript Reference 1.5, Appendix A (Reserved Words):
<http://developer.netscape.com/docs/manuals/js/core/jsref15/keywords.html>")


;; JavaScript identifiers
;; This one is intentionally not too strict...
(defconst javascript-identifier
  "[a-zA-Z_\\$][a-zA-Z0-9_\\$]*"
  "Expression for matching identifiers in `javascript-mode' buffers.

From Core JavaScript Guide 1.5, Chapter 2 (Values, Variables and Literals):
<http://developer.netscape.com/docs/manuals/js/core/jsguide15/ident.html>")

;; ------------------------------------------------------------------------ ;;

;; Font lock keywords

(defconst javascript-function-re
  (concat "\\(^\\|[ \t;{]\\)function[ \t]+\\("
          javascript-identifier
          "\\)"))

(defconst javascript-variable-re
  (concat "\\(^\\|[ \t;{(]\\)\\(const\\|var\\)[ \t]+\\("
          javascript-identifier
          "\\)"))

(defconst javascript-font-lock-keywords
  (list

   ;; Reserved words.
   (cons (concat
          "\\(^\\|[ \t;{(]\\)\\("
          javascript-reserved-words
          "\\)[ \t\n(){};,]")
         '(2 'font-lock-keyword-face))

   ;; Function declarations.
   (cons javascript-function-re '(2 'font-lock-function-name-face))
   ;; This would catch both declarations and calls.
   ;(cons (concat
   ;       "\\(^\\|[ \t.;{(]\\)\\("
   ;       javascript-identifier
   ;       "\\)[ \t]*(")
   ;      '(2 'font-lock-function-name-face))

   ;; Variables and constants.
   (cons javascript-variable-re '(3 'font-lock-variable-name-face))
   ;; This would catch more of them and properties as well.
   ;(cons (concat
   ;       "\\(^\\|[ \t(\\[\\.{;]\\)\\("
   ;       javascript-identifier
   ;       "\\)[ \t]*[^(]")
   ;      '(2 'font-lock-variable-name-face))

   )
  "Highlighting rules for `javascript-mode' buffers.")

;; ------------------------------------------------------------------------ ;;

(defvar javascript-imenu-generic-expression
  `((nil ,javascript-function-re 2)
    ;; ("Variables" ,javascript-variable-re 3)
    )
  "Imenu generic expression for JavaScript mode.
See `imenu-generic-expression'.")

;; ------------------------------------------------------------------------ ;;

;;;###autoload
(defun javascript-mode ()
  "Major mode for editing JavaScript code.

See the documentation for `c++-mode': JavaScript mode is an extension of it.
Use the hook `javascript-mode-hook' to execute custom code when entering
JavaScript mode.

\\{javascript-mode-map}"
  (interactive)

  (let ((current-c++-mode-hook (and (boundp 'c++-mode-hook) c++-mode-hook)))

    ;; Temporarily disable the c++-mode hook; don't wanna run
    ;; it when loading up c++-mode.
    (setq c++-mode-hook nil)
    (c++-mode)

    ;; Do our stuff.
    (setq major-mode 'javascript-mode mode-name "JavaScript")
    (use-local-map javascript-mode-map)
    (setq local-abbrev-table javascript-mode-abbrev-table)
    (c-set-offset 'inher-cont '+)

    ;; Change menu name.  Kudos to Geert Ribbers and Igor Rayak.
    (easy-menu-remove '("C++"))
    (easy-menu-add javascript-menu)

    ;; GNU Emacs reportedly needs this for font locking to work properly.
    (unless (featurep 'xemacs)
      (set (make-local-variable 'font-lock-defaults)
           '(javascript-font-lock-keywords nil nil)))

    ;; cc-mode does not handle JavaScript prototype function declarations well.
    ;; Thanks to Sebastian Delmont.
    (set (make-local-variable 'c-lambda-key) "function")
    (c-set-offset 'inlambda 0)

    ;; imenu support.
    (set (make-local-variable 'imenu-generic-expression)
         javascript-imenu-generic-expression)

    ;; Restore the original c++-mode-hook.
    (setq c++-mode-hook current-c++-mode-hook)

    (run-hooks 'javascript-mode-hook)))

;; ------------------------------------------------------------------------ ;;

;;;###autoload
(defun javascript-shell ()
  "Run a JavaScript shell as an inferior process.

Use the `javascript-shell-command' variable to set the command and
`javascript-shell-command-args' for its arguments to specify the
command line that invokes your preferred JavaScript shell.

Free JavaScript shell implementations are available for example from
<http://www.mozilla.org/js/>.

Usage examples:        command    arguments
 Mozilla SpiderMonkey  jsshell
 Mozilla Rhino         java       -jar /path/to/js.jar"

  (interactive)

  (unless (comint-check-proc "*JavaScript*")
    (set-buffer
     (apply 'make-comint "JavaScript"
            javascript-shell-command nil javascript-shell-command-args))
    (javascript-shell-mode)
    )

  (pop-to-buffer "*JavaScript*")
  )


(defun javascript-shell-mode ()
  "Major mode for interacting with a JavaScript shell."
  (comint-mode)
  (setq comint-prompt-regexp javascript-shell-prompt-pattern)
  (setq mode-name 'javascript-shell-mode)
  (setq mode-name "JavaScript Shell")
  (setq mode-line-process '(":%s"))
  (run-hooks 'javascript-shell-mode-hook)
  )

;; ------------------------------------------------------------------------ ;;

;;;###autoload(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.pac$" . javascript-mode))

;; Speedbar handling
(if (fboundp 'speedbar-add-supported-extension)
    (speedbar-add-supported-extension '(".js" ".pac")))

;; ------------------------------------------------------------------------ ;;

(provide 'javascript-mode)

;;; javascript-mode.el ends here
