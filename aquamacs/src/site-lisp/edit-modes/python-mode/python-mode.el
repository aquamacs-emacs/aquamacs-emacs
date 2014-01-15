;; python-mode.el --- Towards an Python-IDE in Emacs

;; Maintainer: Andreas Röhler <andreas.roehler@online.de>
;; Keywords: languages, processes, python, oop

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 2003-2013 https://launchpad.net/python-mode
;;         1995-2002 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Maintainer: python-mode@python.org
;; Created:    Feb 1992
;; Keywords:   python languages oop

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; commands-python-mode.org in directory doc reports
;; available commands, also a menu is provided

;; as for `py-add-abbrev':
;; Similar to `add-mode-abbrev', but uses
;; `py-partial-expression' before point for expansion to
;; store, not `word'. Also provides a proposal for new
;; abbrevs.

;; Proposal for an abbrev is composed from the downcased
;; initials of expansion - provided they are of char-class
;; [:alpha:]
;;
;; For example code below would be recognised as a
;; `py-expression' composed by three
;; py-partial-expressions.
;;
;; OrderedDict.popitem(last=True)
;;
;; Putting the curser at the EOL, M-3 M-x py-add-abbrev
;;
;; would prompt "op" for an abbrev to store, as first
;; `py-partial-expression' beginns with a "(", which is
;; not taken as proposal.

;;; Code

(require 'comint)
(require 'custom)
(eval-when-compile (require 'cl))
(require 'compile)
(require 'ansi-color)
(require 'cc-cmds)
(require 'shell)
(require 'rx)
(ignore-errors (require 'flymake))
(require 'imenu)
(require 'thingatpt)

(defgroup python-mode nil
  "Support for the Python programming language, <http://www.python.org/>"
  :group 'languages
  :prefix "py-")

(defconst py-version "6.1.2")

;;; Customization
(defcustom python-mode-modeline-display "Py"
  "String to display in Emacs modeline "

  :type 'string
  :group 'python-mode)

(defcustom py-indent-offset 4
  "Amount of offset per level of indentation.
`\\[py-guess-indent-offset]' can usually guess a good value when
you're editing someone else's Python code."
  :type 'integer
  :group 'python-mode)
(make-variable-buffer-local 'py-indent-offset)

(defcustom py-backslashed-lines-indent-offset 5
  "Amount of offset per level of indentation of backslashed.
No semantic indent,  which diff to `py-indent-offset' indicates "
  :type 'integer
  :group 'python-mode)
(make-variable-buffer-local 'py-backslashed-lines-indent-offset)

(defcustom pdb-path '/usr/lib/python2.7/pdb.py
  "Where to find pdb.py. Edit this according to your system.

If you ignore the location `M-x py-guess-pdb-path' might display it"
  :type 'variable
  :group 'python-mode)

(defcustom py-verbose-p nil
  "If indenting functions should report reached indent level.

Default is nil. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-store-result-p nil
  "When non-nil, put resulting string of `py-execute-...' into kill-ring, so it might be yanked.

Default is nil"

  :type 'boolean
  :group 'python-mode)

(defcustom py-load-skeletons-p nil
  "If skeleton definitions should be loaded, default is nil. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-load-pymacs-p nil
  "If Pymacs related stuff should be loaded.

Default is nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca"

  :type 'boolean
  :group 'python-mode)

(defun py-smart-operator-check ()
  "Check, if smart-operator-mode is loaded resp. available.

Give some hints, if not."
  (interactive)
  (if (featurep 'smart-operator)
      't
    (progn
      (and (boundp 'py-smart-operator-mode-p) py-smart-operator-mode-p (message "%s" "Don't see smart-operator.el. Make sure, it's installed. See in menu Options, Manage Emacs Packages. Or get it from source: URL: http://xwl.appspot.com/ref/smart-operator.el")
           nil))))

(defun py-autopair-check ()
  "Check, if autopair-mode is available.

Give some hints, if not."
  (interactive)
  (if (featurep 'autopair)
      't
    (progn
      (message "%s" "Don't see autopair.el. Make sure, it's installed. If not, maybe see source: URL: http://autopair.googlecode.com")
      nil)))

(defcustom py-smart-operator-mode-p nil
  "If python-mode calls (smart-operator-mode-on)

Default is non-nil. "

  :type 'boolean
  :group 'python-mode
  :set (lambda (symbol value)
         (and (py-smart-operator-check)
              (set-default symbol value)
              (smart-operator-mode (if value 1 0)))))
(make-variable-buffer-local 'py-smart-operator-mode-p)

(defcustom py-sexp-function nil
  "When set, it's value is called instead of `forward-sexp', `backward-sexp'

Default is nil. "

  :type '(choice
          (const :tag "default" nil)
          (const :tag "py-end-of-partial-expression" py-end-of-partial-expression)
          (const :tag "py-end-of-expression" py-end-of-expression))
  :group 'python-mode)
(make-variable-buffer-local 'py-sexp-function)

(defvar py-autopair-mode nil)
(defvar highlight-indent-active nil)
(defvar smart-operator-mode nil)

(defvar py-fill-column-orig fill-column)
(defvar py-autofill-timer nil)

(defcustom py-autopair-mode nil
  "If python-mode calls (autopair-mode-on)

Default is nil
Load `autopair-mode' written by Joao Tavora <joaotavora [at] gmail.com>
URL: http://autopair.googlecode.com "
  :type 'boolean
  :group 'python-mode
  :set (lambda (symbol value)
         (and
          ;; (py-autopair-check)
          (set-default symbol value)
          (autopair-mode (if value 1 0)))))

(defcustom py-no-completion-calls-dabbrev-expand-p t
  "If completion function should call dabbrev-expand when no completion found. Default is `t'

See also `py-indent-no-completion-p'"
  :type 'boolean
  :group 'python-mode)

(defcustom py-indent-no-completion-p t
  "If completion function should insert a TAB when no completion found. Default is `t'

See also `py-no-completion-calls-dabbrev-expand-p'"
  :type 'boolean
  :group 'python-mode)

(defcustom py-set-fill-column-p nil
  "If python-mode should set fill-column

according values in `py-comment-fill-column' and `py-docstring-fill-column'.
Default is  nil"

  :type 'boolean
  :group 'python-mode)

(defcustom py-autofill-timer-delay 1
  "Delay when idle before functions ajusting  `py-docstring-fill-column' resp. `py-comment-fill-column' are called. "
  :type 'integer

  :group 'python-mode)

(defcustom py-docstring-fill-column 72
  "Value of `fill-column' to use when filling a docstring.
Any non-integer value means do not use a different value of
`fill-column' when filling docstrings."
  :type '(choice (integer)
                 (const :tag "Use the current `fill-column'" t))
  :group 'python-mode)

(defcustom py-comment-fill-column 79
  "Value of `fill-column' to use when filling a comment.
Any non-integer value means do not use a different value of
`fill-column' when filling docstrings."
  :type '(choice (integer)
                 (const :tag "Use the current `fill-column'" t))
  :group 'python-mode)

(defcustom py-fontify-shell-buffer-p nil
  "If code in Python shell should be highlighted as in script buffer.

Default is nil.

If `t', related vars like `comment-start' will be set too.
Seems convenient when playing with stuff in IPython shell
Might not be TRT when a lot of output arrives "

  :type 'boolean
  :group 'python-mode)

(defcustom py-modeline-display-full-path-p nil
  "If the full PATH/TO/PYTHON should be displayed in shell modeline.

Default is nil. Note: when `py-shell-name' is specified with path, it's shown as an acronym in buffer-name already. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-modeline-acronym-display-home-p nil
  "If the modeline acronym should contain chars indicating the home-directory.

Default is nil "
  :type 'boolean
  :group 'python-mode)

(defcustom py-install-directory ""
  "Directory where python-mode.el and it's subdirectories should be installed. Needed for completion and other environment stuff only. "
  :type 'string
  :group 'python-mode)

(defcustom py-guess-py-install-directory-p t
  "If in cases, `py-install-directory' isn't set,  `py-set-load-path'should guess it from `buffer-file-name'. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-extensions "py-extensions.el"
  "File where extensions to python-mode.el should be installed. Used by virtualenv support. "

  :type 'string
  :group 'python-mode)

(defcustom py-pylint-offer-current-p t
  "If current buffers file should be offered for check.

Default is non-nil. If nil, `py-pylint-run' offers filename from history "

  :type 'boolean
  :group 'python-mode)

(defcustom py-hide-show-minor-mode-p nil
  "If hide-show minor-mode should be on, default is nil. "

  :type 'boolean
  :group 'python-mode)

(defcustom empty-comment-line-separates-paragraph-p t
  "Consider paragraph start/end lines with nothing inside but comment sign.

Default is  non-nil"
  :type 'boolean
  :group 'python-mode)

(defcustom py-if-name-main-permission-p t
  "Allow execution of code inside blocks started
by \"if __name__== '__main__':\".

Default is non-nil"

  :type 'boolean
  :group 'python-mode)

(defcustom py-use-font-lock-doc-face-p nil
  "If documention string inside of def or class get `font-lock-doc-face'.

`font-lock-doc-face' inherits `font-lock-string-face'.
Call M-x `customize-face' in order to have a visible effect. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-tab-shifts-region-p nil
  "If `t', TAB will indent/cycle the region, not just the current line.

Default is  nil"

  :type 'boolean
  :group 'python-mode)

(defcustom py-tab-indents-region-p nil
  "When `t' and first TAB doesn't shift, indent-region is called.

Default is  nil"

  :type 'boolean
  :group 'python-mode)

(defcustom py-block-comment-prefix-p t
  "If py-comment inserts py-block-comment-prefix.

Default is t"

  :type 'boolean
  :group 'python-mode)

(defcustom py-org-cycle-p nil
  "When non-nil, command `org-cycle' is available at shift-TAB, <backtab>

Default is nil. "

  :type 'boolean
  :group 'python-mode)

(defcustom ipython-complete-use-separate-shell-p nil

  "If `ipython-complete' should use a separate shell. Thus prompt-counter is not incremented by completion. "
  :type 'boolean :group 'python-mode)

(defcustom py-outline-minor-mode-p t
  "If outline minor-mode should be on, default is `t'. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-outline-mode-keywords
  '("class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with")
  "Keywords composing visible heads. "
  :type '(repeat string)
  :group 'python-mode)

(defcustom py-start-run-py-shell nil
  "If `python-mode' should start a python-shell, `py-shell'.

Default is `nil'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-start-run-ipython-shell nil
  "If `python-mode' should start an ipython-shell.

Default is `nil'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-close-provides-newline t
  "If a newline is inserted, when line after block isn't empty. Default is non-nil. "
  :type 'boolean
  :group 'python-mode)
(make-variable-buffer-local 'py-close-provides-newline)

(defcustom py-dedent-keep-relative-column t
  "If point should follow dedent or kind of electric move to end of line. Default is t - keep relative position. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-indent-honors-multiline-listing nil
  "If `t', indents to 1+ column of opening delimiter. If `nil', indent adds one level to the beginning of statement. Default is `nil'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-indent-paren-spanned-multilines-p nil
  "If non-nil, indents elements of list a value of `py-indent-offset' to first element:

def foo():
    if (foo &&
            baz):
        bar()

Default lines up with first element:

def foo():
    if (foo &&
        baz):
        bar()
"
  :type 'boolean
  :group 'python-mode)

(defcustom py-indent-honors-inline-comment nil
  "If non-nil, indents to column of inlined comment start.
Default is nil. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-closing-list-dedents-bos nil
  "If non-nil, closing parentesis dedents onto column of statement, otherwise keeps additional `py-indent-offset', default is nil "
  :type 'boolean
  :group 'python-mode)

(defcustom py-closing-list-space 1
  "Number of chars, closing parentesis outdent from opening, default is 1 "
  :type 'number
  :group 'python-mode)

(defcustom py-closing-list-keeps-space nil
  "If non-nil, closing parentesis dedents onto column of opening plus `py-closing-list-space', default is nil "
  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-kill-backward-p nil
  "Affects `py-electric-backspace'. Default is nil.

If behind a delimited form of braces, brackets or parentheses,
backspace will kill it's contents

With when cursor after
my_string[0:1]
--------------^

==>

my_string[]
----------^

In result cursor is insided emptied delimited form."

  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-colon-active-p nil
  "`py-electric-colon' feature.  Default is `nil'. See lp:837065 for discussions.

See also `py-electric-colon-bobl-only' "
  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-colon-bobl-only t

  "When inserting a colon, do not indent lines unless at beginning of block

See lp:1207405 resp. `py-electric-colon-active-p' "

  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-colon-greedy-p nil
  "If py-electric-colon should indent to the outmost reasonable level.

If nil, default, it will not move from at any reasonable level. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-colon-newline-and-indent-p nil
  "If non-nil, `py-electric-colon' will call `newline-and-indent'.  Default is `nil'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-comment-p nil
  "If \"#\" should call `py-electric-comment'. Default is `nil'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-comment-add-space-p nil
  "If py-electric-comment should add a space.  Default is `nil'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-mark-decorators nil
  "If py-mark-def-or-class functions should mark decorators too. Default is `nil'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-tab-indent t
  "Non-nil means TAB in Python mode calls `py-indent-line'."
  :type 'boolean
  :group 'python-mode)

(defcustom py-return-key 'py-newline-and-indent
  "Which command <return> should call. "
  :type '(choice
          (const :tag "default" py-newline-and-indent)
          (const :tag "newline" newline)
          (const :tag "py-newline-and-indent" py-newline-and-indent)
          (const :tag "py-newline-and-dedent" py-newline-and-dedent)
          )
  :group 'python-mode)

(defcustom py-complete-function 'py-shell-complete
  "When set, enforces function todo completion, default is nil.

Normally python-mode, resp. inferior-python-mode know best which function to use. "
  :type '(choice
          (const :tag "default" nil)
          (const :tag "py-completion-at-point" py-completion-at-point)
          (const :tag "Pymacs based py-complete-completion-at-point" py-complete-completion-at-point)
          (const :tag "py-shell-complete" py-shell-complete)
          (const :tag "IPython's ipython-complete" ipython-complete)
          )
  :group 'python-mode)

(defcustom ipython-complete-function 'ipython-complete
  "Function used for completion in IPython shell buffers. "
  :type '(choice (const :tag "py-completion-at-point" py-completion-at-point)
                 (const :tag "py-shell-complete" py-shell-complete)
                 (const :tag "Pymacs based py-complete" py-complete)
                 (const :tag "IPython's ipython-complete" ipython-complete))
  :group 'python-mode)
(make-variable-buffer-local 'ipython-complete-function)

(defcustom py-encoding-string " # -*- coding: utf-8 -*-"
  "Default string specifying encoding of a Python file. "
  :type 'string
  :group 'python-mode)

(defcustom py-shebang-startstring "#! /bin/env"
  "Detecting the shell in head of file. "
  :type 'string
  :group 'python-mode)

(defcustom py-python-command-args '("-i")
  "List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python-mode)
(make-variable-buffer-local 'py-python-command-args)

(defcustom py-jython-command-args '("-i")
  "List of string arguments to be used when starting a Jython shell."
  :type '(repeat string)
  :group 'python-mode
  :tag "Jython Command Args")

(defcustom py-cleanup-temporary t
  "If temporary buffers and files used by functions executing region should be deleted afterwards. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-execute-no-temp-p nil
  "Seems Emacs-24.3 provided a way executing stuff without temporary files. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-lhs-inbound-indent 1
  "When line starts a multiline-assignment: How many colums indent should be more than opening bracket, brace or parenthesis. "
  :type 'integer
  :group 'python-mode)
(make-variable-buffer-local 'py-lhs-inbound-indent)

(defcustom py-continuation-offset 2
  "Additional amount of offset to give for some continuation lines.
Continuation lines are those that immediately follow a backslash
terminated line. "
  :type 'integer
  :group 'python-mode)

(defcustom py-indent-tabs-mode nil
  "Python-mode starts `indent-tabs-mode' with the value specified here, default is nil. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-smart-indentation t
  "Should `python-mode' try to automagically set some indentation variables?
When this variable is non-nil, two things happen when a buffer is set
to `python-mode':

    1. `py-indent-offset' is guessed from existing code in the buffer.
       Only guessed values between 2 and 8 are considered.  If a valid
       guess can't be made (perhaps because you are visiting a new
       file), then the value in `py-indent-offset' is used.

    2. `indent-tabs-mode' is turned off if `py-indent-offset' does not
       equal `tab-width' (`indent-tabs-mode' is never turned on by
       Python mode).  This means that for newly written code, tabs are
       only inserted in indentation if one tab is one indentation
       level, otherwise only spaces are used.

Note that both these settings occur *after* `python-mode-hook' is run,
so if you want to defeat the automagic configuration, you must also
set `py-smart-indentation' to nil in your `python-mode-hook'."
  :type 'boolean
  :group 'python-mode)
(make-variable-buffer-local 'py-smart-indentation)

(defcustom py-block-comment-prefix "##"
  "String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
`...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'python-mode)

(defcustom py-indent-comments t
  "When t, comment lines are indented. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-uncomment-indents-p nil
  "When non-nil, after uncomment indent lines. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-separator-char 47
  "The character, which separates the system file-path components.

Precedes guessing when not empty, returned by function `py-separator-char'. "
  :type 'character
  :group 'python-mode)
;; used as a string finally
;; kept a character not to break existing customizations
(and (characterp py-separator-char)(setq py-separator-char (char-to-string py-separator-char)))

(defcustom py-custom-temp-directory ""
  "If set, will take precedence over guessed values from `py-temp-directory'. Default is the empty string.

When set, make sure the directory exists. "
  :type 'string
  :group 'python-mode)

(defcustom py-beep-if-tab-change t
  "Ring the bell if `tab-width' is changed.
If a comment of the form

  \t# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) `tab-width' does not
equal <number>, `tab-width' is set to <number>, a message saying so is
displayed in the echo area, and if `py-beep-if-tab-change' is non-nil
the Emacs bell is also rung as a warning."
  :type 'boolean
  :group 'python-mode)

(defcustom py-jump-on-exception t
  "Jump to innermost exception frame in *Python Output* buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame."
  :type 'boolean
  :group 'python-mode)

(defcustom py-ask-about-save t
  "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking."
  :type 'boolean
  :group 'python-mode)

(defcustom py-backspace-function 'backward-delete-char-untabify
  "Function called by `py-electric-backspace' when deleting backwards."
  :type 'function
  :group 'python-mode)

(defcustom py-delete-function 'delete-char
  "Function called by `py-electric-delete' when deleting forwards."
  :type 'function
  :group 'python-mode)

(defcustom py-pdbtrack-do-tracking-p t
  "Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the *Python* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb."
  :type 'boolean
  :group 'python-mode)
(make-variable-buffer-local 'py-pdbtrack-do-tracking-p)

(defcustom py-pdbtrack-filename-mapping nil
  "Supports mapping file paths when opening file buffers in pdbtrack.
When non-nil this is an alist mapping paths in the Python interpreter
to paths in Emacs."
  :type 'alist
  :group 'python-mode)

(defcustom py-pdbtrack-minor-mode-string " PDB"
  "String to use in the minor mode list when pdbtrack is enabled."
  :type 'string
  :group 'python-mode)

(defcustom py-import-check-point-max
  20000
  "Maximum number of characters to search for a Java-ish import statement.
When `python-mode' tries to calculate the shell to use (either a
CPython or a Jython shell), it looks at the so-called `shebang' line
-- i.e. #! line.  If that's not available, it looks at some of the
file heading imports to see if they look Java-like."
  :type 'integer
  :group 'python-mode)

(defcustom py-jython-packages
  '("java" "javax")
  "Imported packages that imply `jython-mode'."
  :type '(repeat string)
  :group 'python-mode)
(make-obsolete-variable 'py-jpython-packages 'py-jython-packages nil)

(defcustom py-current-defun-show  t
  "If `py-current-defun' should jump to the definition, highlight it while waiting PY-WHICH-FUNC-DELAY seconds, before returning to previous position.

Default is `t'."

  :type 'boolean
  :group 'python-mode)

(defcustom py-current-defun-delay  2
  "When called interactively, `py-current-defun' should wait PY-WHICH-FUNC-DELAY seconds at the definition name found, before returning to previous position. "

  :type 'number
  :group 'python-mode)

(defcustom py-send-receive-delay  5
  "Seconds to wait for output, used by `py-send-receive'. "

  :type 'number
  :group 'python-mode)

(defcustom py-honor-IPYTHONDIR-p nil
  "When non-nil ipython-history file is constructed by $IPYTHONDIR
followed by \"/history\". Default is nil.

Otherwise value of py-ipython-history is used. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-ipython-history "~/.ipython/history"
  "ipython-history default file. Used when py-honor-IPYTHONDIR-p is nil (default) "

  :type 'string
  :group 'python-mode)

(defcustom py-honor-PYTHONHISTORY-p nil
  "When non-nil python-history file is set by $PYTHONHISTORY
Default is nil.

Otherwise value of py-python-history is used. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-python-history "~/.python_history"
  "python-history default file. Used when py-honor-PYTHONHISTORY-p is nil (default) "

  :type 'string
  :group 'python-mode)

(defcustom py-master-file nil
  "If non-nil, \\[py-execute-buffer] executes the named
master file instead of the buffer's file.  If the file name has a
relative path, the value of variable `default-directory' for the
buffer is prepended to come up with a file name.

Beside you may set this variable in the file's local
variable section, e.g.:

    # Local Variables:
    # py-master-file: \"master.py\"
    # End:

"
  :type 'string
  :group 'python-mode)
(make-variable-buffer-local 'py-master-file)

(defcustom py-pychecker-command "pychecker"
  "Shell command used to run Pychecker."
  :type 'string
  :group 'python-mode
  :tag "Pychecker Command")

(defcustom py-pychecker-command-args '("--stdlib")
  "List of string arguments to be passed to pychecker."
  :type '(repeat string)
  :group 'python-mode
  :tag "Pychecker Command Args")

(defcustom py-pep8-command "pep8"
  "Shell command used to run pep8."
  :type 'string
  :group 'python-mode
  :tag "PEP 8 Command")

(defcustom py-pep8-command-args '("")
  "List of string arguments to be passed to pep8.

Default is \"\" "
  :type '(repeat string)
  :group 'python-mode
  :tag "PEP 8 Command Args")

(defcustom py-pyflakespep8-command (concat py-install-directory "/pyflakespep8.py")
  "Shell command used to run `pyflakespep8'."
  :type 'string
  :group 'python-mode
  :tag "Pyflakespep8 Command")

(defcustom py-pep8-command "pep8"
  "Shell command used to run pep8."
  :type 'string
  :group 'python-mode
  :tag "PEP 8 Command")

(defcustom py-pep8-command-args '("")
  "List of string arguments to be passed to pep8.

Default is \"\" "
  :type '(repeat string)
  :group 'python-mode
  :tag "PEP 8 Command Args")

(defcustom py-pyflakespep8-command-args '("")
  "List of string arguments to be passed to pyflakespep8.

Default is \"\" "
  :type '(repeat string)
  :group 'python-mode
  :tag "Pyflakes-pep8 Command Args")

(defcustom py-pyflakes-command "pyflakes"
  "Shell command used to run Pyflakes."
  :type 'string
  :group 'python-mode
  :tag "Pyflakes Command")

(defcustom py-pyflakes-command-args '("")
  "List of string arguments to be passed to pyflakes.

Default is \"\" "
  :type '(repeat string)
  :group 'python-mode
  :tag "Pyflakes Command Args")

(defcustom py-pep8-command-args '("")
  "List of string arguments to be passed to pylint.

Default is \"\" "
  :type '(repeat string)
  :group 'python-mode
  :tag "PEP 8 Command Args")

(defcustom py-pylint-command "pylint"
  "Shell command used to run Pylint."
  :type 'string
  :group 'python-mode
  :tag "Pylint Command")

(defcustom py-pylint-command-args '("--errors-only")
  "List of string arguments to be passed to pylint.

Default is \"--errors-only\" "
  :type '(repeat string)
  :group 'python-mode
  :tag "Pylint Command Args")

(defcustom py-shell-input-prompt-1-regexp "^>>> "
  "A regular expression to match the input prompt of the shell."
  :type 'string
  :group 'python-mode)

(defcustom py-shell-input-prompt-2-regexp "^[.][.][.] "
  "A regular expression to match the input prompt of the shell after the
  first line of input."
  :type 'string
  :group 'python-mode)

(defcustom py-shell-prompt-read-only t
  "If non-nil, the python prompt is read only.  Setting this
variable will only effect new shells."
  :type 'boolean
  :group 'python-mode)

(defcustom py-fileless-buffer-use-default-directory-p t
  "When `py-use-current-dir-when-execute-p' is non-nil and no buffer-file exists, value of `default-directory' sets current working directory of Python output shell"
  :type 'boolean
  :group 'python-mode)

(defcustom py-keep-shell-dir-when-execute-p nil
  "Don't change Python shell's current working directory when sending code.

See also `py-execute-directory'"
  :type 'boolean
  :group 'python-mode)

(defcustom py-switch-buffers-on-execute-p nil
  "When non-nil switch to the Python output buffer. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-split-windows-on-execute-p t
  "When non-nil split windows. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-max-split-windows 2
  "When split windows is enabled the maximum windows to allow
  before reusing other windows."
  :type 'number
  :group 'python-mode)

(defcustom py-split-windows-on-execute-function 'split-window-vertically
  "How window should get splitted to display results of py-execute-... functions. "
  :type '(choice (const :tag "split-window-vertically" split-window-vertically)
		 (const :tag "split-window-horizontally" split-window-horizontally)
                 )
  :group 'python-mode)
(make-variable-buffer-local 'py-split-windows-on-execute-function)

(defcustom py-hide-show-keywords
  '("class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with")
  "Keywords composing visible heads.
Also used by (minor-)outline-mode "
  :type '(repeat string)
  :group 'python-mode)

(defcustom py-hide-show-hide-docstrings t
  "Controls if doc strings can be hidden by hide-show"
  :type 'boolean
  :group 'python-mode)

(defcustom py-paragraph-fill-docstring-p nil
  "If `py-fill-paragraph', when inside a docstring, should fill the complete string.

Default is nil.

Convenient use of `M-q' inside docstrings
See also `py-docstring-style'
"

  :type 'boolean
  :group 'python-mode)
(make-variable-buffer-local 'py-paragraph-fill-docstring-p)

(defcustom python-mode-hook nil
  "Hook run when entering Python mode."
  :group 'python-mode
  :type 'hook)

(defcustom py-imenu-create-index-p nil
  "Non-nil means Python mode creates and displays an index menu of functions and global variables. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-imenu-create-index-function 'py-imenu-create-index-new
  "Switch between `py-imenu-create-index-new', which also lists modules variables,  and series 5. index-machine"
  :type '(choice (const :tag "'py-imenu-create-index-new, also lists modules variables " py-imenu-create-index-new)
                 (const :tag "py-imenu-create-index, series 5. index-machine" py-imenu-create-index-function))
  :group 'python-mode)

(defcustom py-shell-name "python"
  "A PATH/TO/EXECUTABLE or default value `py-shell' may look for, if no shell is specified by command. "
  :type 'string
  :group 'python-mode)
(make-variable-buffer-local 'py-shell-name)
(defvaralias 'py-python-command 'py-shell-name)

(defcustom py-shell-toggle-1 py-shell-name
  "A PATH/TO/EXECUTABLE or default value used by `py-toggle-shell'. "
  :type 'string
  :group 'python-mode)
(make-variable-buffer-local 'py-shell-toggle-1)

(defcustom py-shell-toggle-2 "python3"
  "A PATH/TO/EXECUTABLE or default value used by `py-toggle-shell'. "
  :type 'string
  :group 'python-mode)
(make-variable-buffer-local 'py-shell-toggle-2)

(defcustom py-match-paren-mode nil
  "Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets `py-match-paren-key' in python-mode-map.
Customize `py-match-paren-key' which key to use. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-match-paren-key "%"
  "String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
`...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'python-mode)

(defcustom py-kill-empty-line t
  "If t, py-indent-forward-line kills empty lines. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-remove-cwd-from-path t
  "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
an inferior Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion)."
  :type 'boolean
  :group 'python-mode)

(defcustom py-imenu-show-method-args-p nil
  "Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed."
  :type 'boolean
  :group 'python-mode)

(defcustom py-history-filter-regexp "\\`\\s-*\\S-?\\S-?\\s-*\\'"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters."
  :type 'regexp
  :group 'python-mode)

(defcustom inferior-python-filter-regexp "\\`\\s-*\\S-?\\S-?\\s-*\\'"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters."
  :type 'regexp
  :group 'python-mode)

(defcustom py-set-complete-keymap-p  nil
  "If `py-complete-initialize', which sets up enviroment for Pymacs based py-complete, should load it's keys into `python-mode-map'

Default is nil.
See also resp. edit `py-complete-set-keymap' "

  :type 'boolean
  :group 'python-mode)

(defcustom py-use-local-default nil
  "If `t', py-shell will use `py-shell-local-path' instead
  of default Python.

Making switch between several virtualenv's easier,
 `python-mode' should deliver an installer, so named-shells pointing to virtualenv's will be available. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-highlight-error-source-p nil
  "When py-execute-... commands raise an error, respective code in source-buffer will be highlighted. Default is nil.

M-x `py-remove-overlays-at-point' removes that highlighting.
 "
  :type 'boolean
  :group 'python-mode)

(defcustom py-set-pager-cat-p nil
  "If the shell environment variable $PAGER should set to `cat'.

If `t', use `C-c C-r' to jump to beginning of output. Then scroll normally.

Avoids lp:783828, \"Terminal not fully functional\", for help('COMMAND') in python-shell

When non-nil, imports module `os' "

  :type 'boolean
  :group 'python-mode)

(defcustom py-prompt-on-changed-p t
  "When called interactively, ask for save before a changed buffer is sent to interpreter.

Default is `t'"

  :type 'boolean
  :group 'python-mode)

(defcustom py-dedicated-process-p nil
  "If commands executing code use a dedicated shell.

Default is nil")

(defcustom py-shell-local-path ""
  "If `py-use-local-default' is non-nil, `py-shell' will use EXECUTABLE indicated here incl. path. "

  :type 'string
  :group 'python-mode)

(defcustom py-edit-only-p nil
  "When `t' `python-mode' will not take resort nor check for installed Python executables. Default is nil.

See bug report at launchpad, lp:944093. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-force-py-shell-name-p nil
  "When `t', execution with kind of Python specified in `py-shell-name' is enforced, possibly shebang doesn't take precedence. "

  :type 'boolean
  :group 'python-mode)

(defcustom python-mode-v5-behavior-p nil
  "Execute region through `shell-command-on-region' as
v5 did it - lp:990079. This might fail with certain chars - see UnicodeEncodeError lp:550661"

  :type 'boolean
  :group 'python-mode)

(defcustom py-trailing-whitespace-smart-delete-p nil
  "Default is nil. When t, python-mode calls
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)

Also commands may delete trailing whitespace by the way.
When editing other peoples code, this may produce a larger diff than expected "
  :type 'boolean
  :group 'python-mode)

(defcustom py-newline-delete-trailing-whitespace-p t
  "Delete trailing whitespace maybe left by `py-newline-and-indent'.

Default is `t'. See lp:1100892 "
  :type 'boolean
  :group 'python-mode)

(defcustom py-warn-tmp-files-left-p nil
  "Messages a warning, when `py-temp-directory' contains files susceptible being left by previous Python-mode sessions. See also lp:987534 "
  :type 'boolean
  :group 'python-mode)

(defcustom py-ipython-execute-delay 0.3
  "Delay needed by execute functions when no IPython shell is running. "
  :type 'float
  :group 'python-mode)

(defcustom python-shell-buffer-name "Python"
  "Default buffer name for Python interpreter."
  :type 'string
  :group 'python-mode)

(defcustom python-shell-interpreter "python"
  "Default Python interpreter for shell."
  :type 'string
  :group 'python-mode)

(defcustom python-shell-prompt-regexp ">>> "
  "Regular Expression matching top\-level input prompt of python shell.
It should not contain a caret (^) at the beginning."
  :type 'string
  :group 'python-mode)

(defvar py-ffap-p nil)
(defvar py-ffap nil)
(defvar python-ffap nil)
(defvar ffap-alist nil)

(defun py-set-ffap-form ()
  (cond ((and py-ffap-p py-ffap)
         (eval-after-load "ffap"
           '(push '(python-mode . py-module-path) ffap-alist))
         (setq ffap-alist (remove '(python-mode . py-ffap-module-path) ffap-alist))
         (setq ffap-alist (remove '(inferior-python-mode . py-ffap-module-path)
                                  ffap-alist)))
        (t (setq ffap-alist (remove '(python-mode . py-ffap-module-path) ffap-alist))
           (setq ffap-alist (remove '(inferior-python-mode . py-ffap-module-path)
                                    ffap-alist))
           (setq ffap-alist (remove '(python-mode . py-module-path) ffap-alist)))))

(defcustom py-ffap-p nil

  "Select python-modes way to find file at point.

Default is nil "

  :type '(choice
          (const :tag "default" nil)
          (const :tag "use py-ffap" py-ffap))
  :group 'python-mode
  :set (lambda (symbol value)
         (set-default symbol value)
         (py-set-ffap-form)))

(defcustom python-ffap-setup-code
  "def __FFAP_get_module_path(module):
    try:
        import os
        path = __import__(module).__file__
        if path[-4:] == '.pyc' and os.path.exists(path[0:-1]):
            path = path[:-1]
        return path
    except:
        return ''"
  "Python code to get a module path."
  :type 'string
  :group 'python-mode)

(defcustom py-ffap-string-code
  "__FFAP_get_module_path('''%s''')\n"
  "Python code used to get a string with the path of a module."
  :type 'string
  :group 'python-mode)

(defcustom py-eldoc-setup-code
  "def __PYDOC_get_help(obj):
    try:
        import inspect
        if hasattr(obj, 'startswith'):
            obj = eval(obj, globals())
        doc = inspect.getdoc(obj)
        if not doc and callable(obj):
            target = None
            if inspect.isclass(obj) and hasattr(obj, '__init__'):
                target = obj.__init__
                objtype = 'class'
            else:
                target = obj
                objtype = 'def'
            if target:
                args = inspect.formatargspec(
                    *inspect.getargspec(target)
                )
                name = obj.__name__
                doc = '{objtype} {name}{args}'.format(
                    objtype=objtype, name=name, args=args
                )
        else:
            doc = doc.splitlines()[0]
    except:
        doc = ''
    try:
        exec('print doc')
    except SyntaxError:
        print(doc)"
  "Python code to setup documentation retrieval."
  :type 'string
  :group 'python-mode)

(defcustom py-setup-codes '(python-shell-completion-setup-code
                            python-ffap-setup-code
                            py-eldoc-setup-code)
  "List of code run by `py-shell-send-setup-codes'."
  :type '(repeat symbol)
  :group 'python-mode)

(defcustom py-shell-prompt-regexp ">>> "
  "Regular Expression matching top\-level input prompt of python shell.
It should not contain a caret (^) at the beginning."
  :type 'string
  :group 'python-mode)
(defvar py-shell-prompt-regexp ">>> ")

(defcustom python-shell-completion-setup-code
  "try:
    import readline
except ImportError:
    def __COMPLETER_all_completions(text): []
else:
    import rlcompleter
    readline.set_completer(rlcompleter.Completer().complete)
    def __COMPLETER_all_completions(text):
        import sys
        completions = []
        try:
            i = 0
            while True:
                res = readline.get_completer()(text, i)
                if not res: break
                i += 1
                completions.append(res)
        except NameError:
            pass
        return completions"
  "Code used to setup completion in inferior Python processes."
  :type 'string
  :group 'python-mode)

(defcustom python-shell-module-completion-string-code ""
  "Python code used to get completions separated by semicolons for imports.

For IPython v0.11, add the following line to
`python-shell-completion-setup-code':

from IPython.core.completerlib import module_completion

and use the following as the value of this variable:

';'.join(module_completion('''%s'''))"
  :type 'string
  :group 'python-mode)

(defcustom strip-chars-before  "\\`[ \t\r\n]*"
  "Regexp indicating which chars shall be stripped before STRING - which is defined by `string-chars-preserve'."

  :type 'string
  :group 'convenience)

(defcustom strip-chars-after  "[ \t\r\n]*\\'"
  "Regexp indicating which chars shall be stripped after STRING - which is defined by `string-chars-preserve'."

  :type 'string
  :group 'convenience)

(defcustom py-docstring-style 'pep-257-nn
  "Implemented styles are DJANGO, ONETWO, PEP-257, PEP-257-NN,
SYMMETRIC, and NIL.

A value of NIL won't care about quotes
position and will treat docstrings a normal string, any other
value may result in one of the following docstring styles:

DJANGO:

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

ONETWO:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

PEP-257:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

PEP-257-NN:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

SYMMETRIC:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\""
  :type '(choice
          (const :tag "Don't format docstrings" nil)
          (const :tag "Django's coding standards style." django)
          (const :tag "One newline and start and Two at end style." onetwo)
          (const :tag "PEP-257 with 2 newlines at end of string." pep-257)
          (const :tag "PEP-257 with 1 newline at end of string." pep-257-nn)
          (const :tag "Symmetric style." symmetric))
  :group 'python-mode)

;; Faces
(defface py-number-face
  '((t (:inherit default)))
  ;; '((t (:inherit 'font-lock-variable-name-face)))
  "Highlight numbers. "
  :group 'python-mode)
(defvar py-number-face 'py-number-face)

(defface py-XXX-tag-face
  '((t (:inherit font-lock-string-face)))
  "XXX\\|TODO\\|FIXME "
  :group 'python-mode)
(defvar py-XXX-tag-face 'py-XXX-tag-face)

;; Face for None, True, False, self, and Ellipsis
(defface py-pseudo-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for pseudo keywords in Python mode, like self, True, False, Ellipsis."
  :group 'python-mode)
(defvar py-pseudo-keyword-face 'py-pseudo-keyword-face)

(defface py-variable-name-face
  '((t (:inherit default)))
  ;; '((t (:inherit 'font-lock-variable-name-face)))
  "Face method decorators."
  :group 'python-mode)
(defvar py-variable-name-face 'py-variable-name-face)

;; PEP 318 decorators
(defface py-decorators-face
  '((t (:inherit font-lock-keyword-face)))
  "Face method decorators."
  :group 'python-mode)
(defvar py-decorators-face 'py-decorators-face)

;; Face for builtins
(defface py-builtins-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for builtins like TypeError, object, open, and exec."
  :group 'python-mode)
(defvar py-builtins-face 'py-builtins-face)

(defface py-class-name-face
  '((t (:inherit font-lock-type-face)))
  "Face for classes."
  :group 'python-mode)
(defvar py-class-name-face 'py-class-name-face)

;; XXX, TODO, and FIXME comments and such
(defface py-exception-name-face
  '((t (:inherit font-lock-builtin-face)))
  "."
  :group 'python-mode)
(defvar py-exception-name-face 'py-exception-name-face)

(defvar virtualenv-old-path)

(defvar virtualenv-old-exec-path)

(defvar py-underscore-word-syntax-p t
  "This is set later by defcustom, only initial value here.

If underscore chars should be of syntax-class `word', not of `symbol'.
Underscores in word-class makes `forward-word' etc. travel the indentifiers. Default is `t'.
See also command `toggle-py-underscore-word-syntax-p' ")

(defvar python-mode-message-string "python-mode.el"
  "Reports the python-mode branch in use.")

(defvar py-local-command nil
  "Returns locally used executable-name. ")
(make-variable-buffer-local 'py-local-command)

(defvar py-local-versioned-command nil
  "Returns locally used executable-name including its version. ")
(make-variable-buffer-local 'py-local-versioned-command)

(defvar py-preoutput-leftover nil)
(defvar py-preoutput-skip-next-prompt nil)

(defvar py-shell-complete-debug nil
  "For interal use when debugging." )

(defvar py-encoding-string-re "^[ \t]*#[ \t]*-\\*-[ \t]*coding:.+-\\*-"
  "Matches encoding string of a Python file. ")

(defvar symbol-definition-start-re)
(setq symbol-definition-start-re "^[ \t]*(\\(defun\\|defvar\\|defcustom\\)")

(defvar py-shebang-regexp "#![ \t]?\\([^ \t\n]+\\)[ \t]*\\([biptj]+ython[^ \t\n]*\\)"
  "Detecting the shell in head of file. ")

(make-variable-buffer-local 'py-python-command-args)

(set-default 'py-python-command-args  '("-i"))

(make-obsolete-variable 'py-jpython-command-args 'py-jython-command-args nil)

(defvar py-separator-char 47
  "Values set by defcustom only will not be seen in batch-mode. ")

(defvar py-temp-directory
  (let ((ok '(lambda (x)
               (and x
                    (setq x (expand-file-name x)) ; always true
                    (file-directory-p x)
                    (file-writable-p x)
                    x)))
        erg)
    (or
     (and (not (string= "" py-custom-temp-directory))
          (if (funcall ok py-custom-temp-directory)
              (setq erg (expand-file-name py-custom-temp-directory))
            (if (file-directory-p (expand-file-name py-custom-temp-directory))
                (error "py-custom-temp-directory set but not writable")
              (error "py-custom-temp-directory not an existing directory"))))
     (and (funcall ok (getenv "TMPDIR"))
          (setq erg (getenv "TMPDIR")))
     (and (funcall ok (getenv "TEMP/TMP"))
          (setq erg (getenv "TEMP/TMP")))
     (and (funcall ok "/usr/tmp")
          (setq erg "/usr/tmp"))
     (and (funcall ok "/tmp")
          (setq erg "/tmp"))
     (and (funcall ok "/var/tmp")
          (setq erg "/var/tmp"))
     (and (eq system-type 'darwin)
          (funcall ok "/var/folders")
          (setq erg "/var/folders"))
     (and (or (eq system-type 'ms-dos)(eq system-type 'windows-nt))
          (funcall ok (concat "c:" py-separator-char "Users"))
          (setq erg (concat "c:" py-separator-char "Users")))
     ;; (funcall ok ".")
     (error
      "Couldn't find a usable temp directory -- set `py-temp-directory'"))
    (when erg (setq py-temp-directory erg)))
  "Directory used for temporary files created by a *Python* process.
By default, guesses the first directory from this list that exists and that you
can write into: the value (if any) of the environment variable TMPDIR,
/usr/tmp, /tmp, /var/tmp, or the current directory.

`py-custom-temp-directory' will take precedence when setq ")

(defvar py-exec-command nil
  "Internally used. ")
(make-variable-buffer-local 'py-exec-command)

(defvar py-buffer-name nil
  "Internal use. ")

(defvar py-orig-buffer-or-file nil
  "Internal use. ")

(defvar py-python-major-version nil
  "Internally used. ")
(make-variable-buffer-local 'py-python-major-version)

(defvar py-exec-string-command nil
  "Mode commands will set this. ")
(make-variable-buffer-local 'py-exec-string-command)

(defvar py-which-bufname "Python")
(make-variable-buffer-local 'py-which-bufname)

(defvar py-pychecker-history nil)

(defvar py-pep8-history nil)

(defvar py-pyflakespep8-history nil)

(defvar py-pyflakes-history nil)

(defvar py-pylint-history nil)

(defvar ipython-de-input-prompt-regexp "In \\[[0-9]+\\]:\\|^[ ]\\{3\\}[.]\\{3,\\}:"
  "A regular expression to match the IPython input prompt. ")

;; make sure it's set that way
(setq ipython-de-input-prompt-regexp "In \\[[0-9]+\\]:\\|^[ ]\\{3\\}[.]\\{3,\\}:")

(defvar ipython-de-output-prompt-regexp "^Out\\[[0-9]+\\]: "
  "A regular expression to match the output prompt of IPython.")

(defvar py-force-local-shell-p nil
  "Used internally, see `toggle-force-local-shell'. ")

(defvar python-mode-v5-behavior nil)

(defvar py-bol-forms-last-indent nil
  "For internal use. Stores indent from last py-end-of-FORM-bol command.
When this-command is py-beginning-of-FORM-bol, last-command's indent will be considered in order to jump onto right beginning position.")

;; Skip's XE workaround
(unless (fboundp 'string-to-syntax)
  (defun string-to-syntax (s)
    (cond
     ((equal s "|") '(15))
     ((equal s "_") '(3))
     (t (error "Unhandled string: %s" s)))))

(defvar python-mode-syntax-table nil
  "Syntax table for Python files.")

(setq python-mode-syntax-table
      (let ((table (make-syntax-table)))
        ;; Give punctuation syntax to ASCII that normally has symbol
        ;; syntax or has word syntax and isn't a letter.
        (let ((symbol (string-to-syntax "_"))
              (sst (standard-syntax-table)))
          (dotimes (i 128)
            (unless (= i ?_)
              (if (equal symbol (aref sst i))
                  (modify-syntax-entry i "." table)))))
        (modify-syntax-entry ?$ "." table)
        (modify-syntax-entry ?% "." table)
        ;; exceptions
        (modify-syntax-entry ?# "<" table)
        (modify-syntax-entry ?\n ">" table)
        (modify-syntax-entry ?' "\"" table)
        (modify-syntax-entry ?` "$" table)
        (if py-underscore-word-syntax-p
            (modify-syntax-entry ?\_ "w" table)
          (modify-syntax-entry ?\_ "_" table))
        table))

(defvar py-dotted-expression-syntax-table
  (let ((table (make-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?. "_" table)
    table)
  "Syntax table used to identify Python dotted expressions.")

(defvar eldoc-documentation-function)

(defvar py-completion-last-window-configuration nil
  "Internal use: restore py-restore-window-configuration when completion is done resp. abandoned. ")

(defvar ipython-version nil)

(defvaralias 'py-python-command 'py-shell-name)

(defvaralias 'py-jpython-command 'py-shell-name)

(defvaralias 'py-jython-command 'py-shell-name)

(defvaralias 'py-default-interpreter 'py-shell-name)

;; (defvaralias 'python-command 'py-shell-name)

(defvar py-shell-template "
\(defun NAME (&optional argprompt)
  \"Start an DOCNAME interpreter in another window.

With optional \\\\[universal-argument] user is prompted
for options to pass to the DOCNAME interpreter. \"
  (interactive \"P\")
  (let\* ((py-shell-name \"FULLNAME\"))
    (py-shell argprompt)
    (when (interactive-p) (switch-to-buffer (current-buffer))
          (goto-char (point-max)))))
")

(defcustom py-execute-directory nil
  "When set, stores the file's default directory-name py-execute-... functions act upon.

Used by Python-shell for output of `py-execute-buffer' and related commands. See also `py-use-current-dir-when-execute-p'"
  :type 'string
  :group 'python-mode)

(defcustom py-use-current-dir-when-execute-p t
  "When `t', current directory is used by Python-shell for output of `py-execute-buffer' and related commands.

See also `py-execute-directory'"
  :type 'boolean
  :group 'python-mode)

(defcustom py-shell-prompt-output-regexp ""
  "Regular Expression matching output prompt of python shell.
It should not contain a caret (^) at the beginning."
  :type 'string
  :group 'python-mode
  )

(defcustom py-output-buffer "*Python Output*"
  "When `py-enforce-output-buffer-p' is non-nil, provides the
default for output-buffer. "
  :type 'string
  :group 'python-mode)
(make-variable-buffer-local 'py-output-buffer)

(defcustom py-enforce-output-buffer-p nil
  "When non-nil, current value of `py-output-buffer' is used for output,
regardless of environment. Default is nil"

  :type 'boolean
  :group 'python-mode)

(defvar py-exception-buffer nil
  "Set internally, remember source buffer where error might occur. ")

(defvar py-string-delim-re "\\(\"\"\"\\|'''\\|\"\\|'\\)"
  "When looking at beginning of string. ")

(defvar py-labelled-re "[ \\t]*:[[:print:]]+"
  "When looking at label. ")

(defvar py-expression-skip-regexp "[^ (=:#\t\r\n\f]"
  "py-expression assumes chars indicated possible composing a py-expression, skip it. ")

(defvar py-expression-skip-chars "^ (:=#\t\r\n\f"
  "py-expression assumes chars indicated possible composing a py-expression, skip it. ")

(defvar py-expression-re "[^ =:#\t\r\n\f]+"
  "py-expression assumes chars indicated possible composing a py-expression, when looking-at or -back. ")

(defvar py-not-expression-regexp "[ .=:#\t\r\n\f)]"
  "py-expression assumes chars indicated probably will not compose a py-expression. ")

(defvar py-not-expression-chars " .=#\t\r\n\f"
  "py-expression assumes chars indicated probably will not compose a py-expression. ")

(defvar py-partial-expression-backward-chars "^ ,\"'()[]{}:#\t\r\n\f"
  "py-partial-expression assumes chars indicated possible composing a py-partial-expression, skip it. ")
;; (setq py-partial-expression-backward-chars "^ ,\"'([{:#\t\r\n\f")

(defvar py-partial-expression-forward-chars "^ \"')}]:#\t\r\n\f")
;; (setq py-partial-expression-forward-chars "^ \"')}]:#\t\r\n\f")

(defvar py-partial-expression-regexp "[^ .=:#\t\r\n\f]"
  "py-partial-expression assumes chars indicated possible composing a py-partial-expression, when looking-at or -back. ")

(defvar py-not-partial-expression-regexp "[ .=:#\t\r\n\f)]"
  "py-partial-expression assumes chars indicated probably will not compose a py-partial-expression. ")

(defvar py-operator-regexp "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\)[ \t]*"
  "Matches most of Python operators inclusive whitespaces around.

See also `py-assignment-regexp' ")

(defvar py-assignment-regexp "[ \t]*=[^=]"
  "Matches assignment operator inclusive whitespaces around.

See also `py-operator-regexp' ")

(defvar py-delimiter-regexp "\\(,\\|;\\|:\\)[ \t\n]"
  "Delimiting elements of lists or other programming constructs. ")

(defvar py-delimiter-chars ",;."
  "Chars delimiting elements of lists or other programming constructs. ")

(defvar py-line-number-offset 0
  "When an exception occurs as a result of py-execute-region, a
subsequent py-up-exception needs the line number where the region
started, in order to jump to the correct file line.  This variable is
set in py-execute-region and used in py-jump-to-exception.")

(defvar match-paren-no-use-syntax-pps nil)

(defvar py-traceback-line-re
  "^IPython\\|^In \\[[0-9]+\\]: *\\|[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)\\|^[^ \t>]+>[^0-9]+\\([0-9]+\\)"
  "Regular expression that describes tracebacks.
Inludes Python shell-prompt in order to stop further searches. ")

(defvar py-preoutput-result nil
  "Data from last `_emacs_out' line seen by the preoutput filter.")

(defvar py-file-queue nil
  "Queue of Python temp files awaiting execution.
Currently-active file is at the head of the list.")

(defvar python-mode-abbrev-table nil)

(defvar inferior-python-mode-abbrev-table nil
  "Not in use.")

(defvar py-pdbtrack-input-prompt)

(defvar py-pydbtrack-input-prompt)

(defvar py-pdbtrack-is-tracking-p nil)

(defvar py-shell-map nil
  "Keymap used in *Python* shell buffers.")

(defvar py-font-lock-keywords nil
  "Additional expressions to highlight in Python mode.")

(defvar jython-mode-hook nil
  "Hook called by `jython-mode'. `jython-mode' also calls
`python-mode-hook'.")

(defvar py-shell-hook nil
  "Hook called by `py-shell'.")

(defvar ipython-completion-command-string nil
  "Either ipython0.10-completion-command-string or ipython0.11-completion-command-string.

ipython0.11-completion-command-string also covers version 0.12")

(defvar ipython0.10-completion-command-string
  "print(';'.join(__IP.Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions")

(defvar ipython0.11-completion-command-string
  "print(';'.join(get_ipython().Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions")

(defvar py-last-exeption-buffer nil
  "Internal use only - when `py-up-exception' is called in
source-buffer, this will deliver the exception-buffer again. ")

(defvar py-preoutput-result nil
  "Data from last `_emacs_out' line seen by the preoutput filter.")

(defvar py-import nil)

(defvar py-imenu-class-regexp
  (concat                               ; <<classes>>
   "\\("                                ;
   "^[ \t]*"                            ; newline and maybe whitespace
   "\\(class[ \t]+[a-zA-Z0-9_]+\\)"     ; class name
                                        ; possibly multiple superclasses
   "\\([ \t]*\\((\\([a-zA-Z0-9_,. \t\n]\\)*)\\)?\\)"
   "[ \t]*:"                            ; and the final :
   "\\)"                                ; >>classes<<
   )
  "Regexp for Python classes for use with the Imenu package.")

(defvar py-imenu-method-regexp
  (concat                               ; <<methods and functions>>
   "\\("                                ;
   "^[ \t]*"                            ; new line and maybe whitespace
   "\\(def[ \t]+"                       ; function definitions start with def
   "\\([a-zA-Z0-9_]+\\)"                ;   name is here
                                        ;   function arguments...
   ;;   "[ \t]*(\\([-+/a-zA-Z0-9_=,\* \t\n.()\"'#]*\\))"
   "[ \t]*(\\([^:#]*\\))"
   "\\)"                                ; end of def
   "[ \t]*:"                            ; and then the :
   "\\)"                                ; >>methods and functions<<
   )
  "Regexp for Python methods/functions for use with the Imenu package.")

(defvar py-imenu-method-no-arg-parens '(2 8)
  "Indices into groups of the Python regexp for use with Imenu.

Using these values will result in smaller Imenu lists, as arguments to
functions are not listed.

See the variable `py-imenu-show-method-args-p' for more
information.")

(defvar py-imenu-method-arg-parens '(2 7)
  "Indices into groups of the Python regexp for use with imenu.
Using these values will result in large Imenu lists, as arguments to
functions are listed.

See the variable `py-imenu-show-method-args-p' for more
information.")

(defvar py-imenu-generic-expression
  (cons
   (concat
    py-imenu-class-regexp
    "\\|"                               ; or...
    py-imenu-method-regexp)
   py-imenu-method-no-arg-parens)
  "Generic Python expression which may be used directly with Imenu.
Used by setting the variable `imenu-generic-expression' to this value.
Also, see the function \\[py-imenu-create-index] for a better
alternative for finding the index.")

(defvar py-imenu-generic-regexp nil)

(defvar py-imenu-generic-parens nil)

(defvar imenu-max-items)

(defvar py-mode-output-map nil
  "Keymap used in *Python Output* buffers.")

(defvar inferior-python-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (substitute-key-definition 'complete-symbol 'py-shell-complete
                               map global-map)

    (define-key map (kbd "RET") 'comint-send-input)
    (if py-complete-function
        (define-key map [tab] 'py-complete-function)
      (define-key map [tab] 'py-completion-at-point))
    (define-key map "\C-c-" 'py-up-exception)
    (define-key map "\C-c=" 'py-down-exception))
  map)

(defvar py-menu)

(defvar py-already-guessed-indent-offset nil
  "Internal use by py-indent-line.

When `this-command' is `eq' to `last-command', use the guess already computed. ")
(make-variable-buffer-local 'py-already-guessed-indent-offset)

(defvar skeleton-further-elements)

;;           (set (make-local-variable 'beginning-o1f-defun-function) 'py-beginning-of-def-or-class)
;;           (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class))

;; Add a designator to the minor mode strings
(or (assq 'py-pdbtrack-is-tracking-p minor-mode-alist)
    (push '(py-pdbtrack-is-tracking-p py-pdbtrack-minor-mode-string)
          minor-mode-alist))
(defvar inferior-python-mode-syntax-table
  (let ((st (make-syntax-table python-mode-syntax-table)))
    ;; Don't get confused by apostrophes in the process's output (e.g. if
    ;; you execute "help(os)").
    (modify-syntax-entry ?\' "." st)
    ;; Maybe we should do the same for double quotes?
    ;; (modify-syntax-entry ?\" "." st)
    st))

(defvar py-imports nil)

(defvar smart-operator-mode nil)
(defvar autopair-mode nil)

(defvar highlight-indent-active nil)
(defvar highlight-indentation nil
  "Menu  PyEdit fails when not bound")
(make-variable-buffer-local 'highlight-indentation)

;;(eval-when-compile (load (concat (py-normalize-directory py-install-directory) "extensions" py-separator-char "highlight-indentation.el")))

;;; Constants
(defconst py-blank-or-comment-re "[ \t]*\\($\\|#\\)"
  "Regular expression matching a blank or comment line.")

(defconst py-block-closing-keywords-re
  "[ \t]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]"
  "Matches the beginning of a class, method or compound statement. ")

(defconst py-finally-re
  "[ \t]*\\_<finally\\_>[: \n\t]"
  "Regular expression matching keyword which closes a try-block. ")

(defconst py-except-re
  "[ \t]*\\_<except\\_>[:( \n\t]*"
  "Regular expression matching keyword which composes a try-block. ")

(defconst py-else-re
  "[ \t]*\\_<else\\_>[: \n\t]*"
  "Regular expression matching keyword which closes a for- if- or try-block. ")

(defconst py-return-re
  ".*:?[ \t]*\\_<\\(return\\)\\_>[ \n\t]*"
  "Regular expression matching keyword which typically closes a function. ")

(defconst py-no-outdent-re
  (concat
   "[ \t]*\\_<\\("
   (mapconcat 'identity
              (list
               "try:"
               "except"
               "while"
               "for"
               "if"
               "elif"
               )
              "\\|"
              )
   "\\)\\_>[( \t]+.*:[( \t]\\_<\\("
   (mapconcat 'identity
              (list
               "return"
               "raise"
               "break"
               "continue"
               "pass"
               )
              "\\|"
              )
   "\\)\\_>[ )\t]*$")
  "Regular expression matching lines not to augment indent after.")

(defconst py-assignment-re "\\_<\\w+\\_>[ \t]*\\(=\\|+=\\|*=\\|%=\\|&=\\|^=\\|<<=\\|-=\\|/=\\|**=\\||=\\|>>=\\|//=\\)"
  "If looking at the beginning of an assignment. ")

(defconst py-block-re "[ \t]*\\_<\\(class\\|def\\|for\\|if\\|try\\|while\\|with\\)\\_>[:( \n\t]*"
  "Matches the beginning of a compound statement. ")

(defconst py-minor-block-re "[ \t]*\\_<\\(for\\|if\\|try\\|with\\)\\_>[:( \n\t]*"
  "Matches the beginning of an `for', `if', `try' or `with' block. ")

(defconst py-try-block-re "[ \t]*\\_<try\\_>[: \n\t]"
  "Matches the beginning of an `if' or `try' block. ")

(defconst py-class-re "[ \t]*\\_<\\(class\\)\\_>[ \n\t]"
  "Matches the beginning of a class definition. ")

(defconst py-def-or-class-re "[ \t]*\\_<\\(def\\|class\\)\\_>[ \n\t]"
  "Matches the beginning of a class- or functions definition. ")

(defconst py-def-re "[ \t]*\\_<\\(def\\)\\_>[ \n\t]"
  "Matches the beginning of a functions definition. ")

(defconst py-block-or-clause-re "[ \t]*\\_<\\(def\\|class\\|if\\|else\\|elif\\|while\\|for\\|try\\|except\\|finally\\|with\\)\\_>[:( \n\t]*"
  "Matches the beginning of a compound statement or it's clause. ")
;; (setq py-block-or-clause-re "[ \t]*\\_<\\(if\\|else\\|elif\\|while\\|for\\|try\\|except\\|finally\\|with\\)\\_>[: \n\t]")

(defconst py-extended-block-or-clause-re "[ \t]*\\_<\\(def\\|class\\|if\\|else\\|elif\\|while\\|for\\|try\\|except\\|finally\\|with\\)\\_>[:( \n\t]*"
  "Matches the beginning of a compound statement or it's clause.
Includes def and class. ")

(defconst py-clause-re
  (concat
   "[ \t]*\\_<\\("
   (mapconcat 'identity
              (list
               "elif"
               "else"
               "except"
               "finally")
              "\\|")
   "\\)\\_>[( \t]*.*:?")
  "Regular expression matching lines not to augment indent after.")
;; (setq py-clause-re "[ \t]*\\_<\\(else\\|elif\\|except\\|finally\\)\\_>[: \n\t]")

(defconst py-elif-re "[ \t]*\\_<\\elif\\_>[:( \n\t]*"
  "Matches the beginning of a compound if-statement's clause exclusively. ")

(defconst py-try-clause-re
  (concat
   "[ \t]*\\_<\\("
   (mapconcat 'identity
              (list
               "else"
               "except"
               "finally")
              "\\|")
   "\\)\\_>[( \t]*.*:")
  "Matches the beginning of a compound try-statement's clause. ")

(defconst py-if-re "[ \t]*\\_<if\\_>[( \n\t]*"
  "Matches the beginning of a compound statement saying `if'. ")

(defconst py-try-re "[ \t]*\\_<try\\_>[:( \n\t]*"
  "Matches the beginning of a compound statement saying `try'. " )

;;; Macro definitions
(defmacro py-in-string-or-comment-p ()
  "Returns beginning position if inside a string or comment, nil otherwise. "
  `(or (nth 8 (syntax-ppss))
       (when (or (looking-at "\"")(looking-at "[ \t]*#[ \t]*"))
         (match-beginning 0))))

(defmacro py-escaped ()
  "Return t if char is preceded by an odd number of backslashes. "
  `(save-excursion
     (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

(defmacro py-preceding-line-backslashed-p ()
  "Return t if preceding line is a backslashed continuation line. "
  `(save-excursion
     (beginning-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (py-escaped))))

(defmacro py-current-line-backslashed-p ()
  "Return t if current line is a backslashed continuation line. "
  `(save-excursion
     (end-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (py-escaped))))

;;; Toggle
;; py-docstring-style forms
(defun toggle-py-nil-docstring-style (&optional arg)
  "If nil docstring-style should be on or off.

  Returns value of `py-docstring-style' switched to.
To set permanently,  customize this variable "
  (interactive)
  (let ((arg (or arg (if (eq py-docstring-style nil) -1 1))))
    (if (< 0 arg)
        (setq py-docstring-style 'nil)
      (setq py-docstring-style nil))
    (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
    py-docstring-style))

(defun py-nil-docstring-style-on (&optional arg)
  "Make sure, nil docstring-style' is on.

  Returns value of `py-docstring-style'.
To set permanently,  customize this variable "
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-py-nil-docstring-style arg))
  (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
  py-docstring-style)

(defun py-nil-docstring-style-off ()
  "Make sure, nil docstring-style is off.

  Returns value of `py-docstring-style'.
To set permanently,  customize this variable "
  (interactive)
  (toggle-py-nil-docstring-style -1)
  (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
  py-docstring-style)

(defun toggle-py-onetwo-docstring-style (&optional arg)
  "If onetwo docstring-style should be on or off.

  Returns value of `py-docstring-style' switched to.
To set permanently,  customize this variable "
  (interactive)
  (let ((arg (or arg (if (eq py-docstring-style 'onetwo) -1 1))))
    (if (< 0 arg)
        (setq py-docstring-style 'onetwo)
      (setq py-docstring-style nil))
    (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
    py-docstring-style))

(defun py-onetwo-docstring-style-on (&optional arg)
  "Make sure, onetwo docstring-style' is on.

  Returns value of `py-docstring-style'.
To set permanently,  customize this variable "
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-py-onetwo-docstring-style arg))
  (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
  py-docstring-style)

(defun py-onetwo-docstring-style-off ()
  "Make sure, onetwo docstring-style is off.

  Returns value of `py-docstring-style'.
To set permanently,  customize this variable "
  (interactive)
  (toggle-py-onetwo-docstring-style -1)
  (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
  py-docstring-style)

(defun toggle-py-pep-257-docstring-style (&optional arg)
  "If pep-257 docstring-style should be on or off.

  Returns value of `py-pep-257-docstring-style' switched to. "
  (interactive)
  (let ((arg (or arg (if (eq py-docstring-style 'pep-257) -1 1))))
    (if (< 0 arg)
        (setq py-docstring-style 'pep-257)
      (setq py-docstring-style nil))
    (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
    py-docstring-style))

(defun py-pep-257-docstring-style-on (&optional arg)
  "Make sure, pep-257 docstring-style' is on.

Returns value of `py-pep-257-docstring-style'. "
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-py-pep-257-docstring-style arg))
  (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
  py-docstring-style)

(defun py-pep-257-docstring-style-off ()
  "Make sure, pep-257 docstring-style is off.

Returns value of `py-pep-257-docstring-style'. "
  (interactive)
  (toggle-py-pep-257-docstring-style -1)
  (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
  py-docstring-style)

(defun toggle-py-pep-257-nn-docstring-style (&optional arg)
  "If pep-257-nn docstring-style should be on or off.

  Returns value of `py-pep-257-nn-docstring-style' switched to. "
  (interactive)
  (let ((arg (or arg (if (eq py-docstring-style 'pep-257-nn) -1 1))))
    (if (< 0 arg)
        (setq py-docstring-style 'pep-257-nn)
      (setq py-docstring-style nil))
    (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
    py-docstring-style))

(defun py-pep-257-nn-docstring-style-on (&optional arg)
  "Make sure, pep-257-nn docstring-style' is on.

  Returns value of `py-docstring-style'.
To set permanently,  customize this variable "
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-py-pep-257-nn-docstring-style arg))
  (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
  py-docstring-style)

(defun py-pep-257-nn-docstring-style-off ()
  "Make sure, pep-257-nn docstring-style is off.

  Returns value of `py-docstring-style'.
To set permanently,  customize this variable "
  (interactive)
  (toggle-py-pep-257-nn-docstring-style -1)
  (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
  py-docstring-style)

(defun toggle-py-symmetric-docstring-style (&optional arg)
  "If symmetric docstring-style should be on or off.

  Returns value of `py-docstring-style' switched to.
To set permanently,  customize this variable "
  (interactive)
  (let ((arg (or arg (if (eq py-docstring-style 'symmetric) -1 1))))
    (if (< 0 arg)
        (setq py-docstring-style 'symmetric)
      (setq py-docstring-style nil))
    (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
    py-docstring-style))

(defun py-symmetric-docstring-style-on (&optional arg)
  "Make sure, symmetric docstring-style' is on.

  Returns value of `py-docstring-style'.
To set permanently,  customize this variable "
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-py-symmetric-docstring-style arg))
  (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
  py-docstring-style)

(defun py-symmetric-docstring-style-off ()
  "Make sure, symmetric docstring-style is off.

  Returns value of `py-docstring-style'.
To set permanently,  customize this variable "
  (interactive)
  (toggle-py-symmetric-docstring-style -1)
  (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
  py-docstring-style)

(defun toggle-py-django-docstring-style (&optional arg)
  "If django docstring-style should be on or off.

  Returns value of `py-docstring-style' switched to.
To set permanently,  customize this variable "
  (interactive)
  (let ((arg (or arg (if (eq py-docstring-style 'django) -1 1))))
    (if (< 0 arg)
        (setq py-docstring-style 'django)
      (setq py-docstring-style nil))
    (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
    py-docstring-style))

(defun py-django-docstring-style-on (&optional arg)
  "Make sure, django docstring-style' is on.

  Returns value of `py-docstring-style'.
To set permanently,  customize this variable "
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-py-django-docstring-style arg))
  (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
  py-docstring-style)

(defun py-django-docstring-style-off ()
  "Make sure, django docstring-style is off.

  Returns value of `py-docstring-style'.
To set permanently,  customize this variable "
  (interactive)
  (toggle-py-django-docstring-style -1)
  (when (or py-verbose-p (interactive-p)) (message "py-docstring-style: %s" py-docstring-style))
  py-docstring-style)

;; py-underscore-word-syntax-p forms
(defun toggle-py-underscore-word-syntax-p (&optional arg)
  "If `py-underscore-word-syntax-p' should be on or off.

  Returns value of `py-underscore-word-syntax-p' switched to. "
  (interactive)
  (let ((arg (or arg (if py-underscore-word-syntax-p -1 1))))
    (if (< 0 arg)
        (progn
          (setq py-underscore-word-syntax-p t)
          (modify-syntax-entry ?\_ "w" python-mode-syntax-table))
      (setq py-underscore-word-syntax-p nil)
      (modify-syntax-entry ?\_ "_" python-mode-syntax-table))
    (when (or py-verbose-p (interactive-p)) (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
    py-underscore-word-syntax-p))

(defun py-underscore-word-syntax-p-on (&optional arg)
  "Make sure, py-underscore-word-syntax-p' is on.

Returns value of `py-underscore-word-syntax-p'. "
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-py-underscore-word-syntax-p arg))
  (when (or py-verbose-p (interactive-p)) (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
  py-underscore-word-syntax-p)

(defun py-underscore-word-syntax-p-off ()
  "Make sure, `py-underscore-word-syntax-p' is off.

Returns value of `py-underscore-word-syntax-p'. "
  (interactive)
  (toggle-py-underscore-word-syntax-p -1)
  (when (or py-verbose-p (interactive-p)) (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
  py-underscore-word-syntax-p)

(defcustom py-underscore-word-syntax-p t
  "If underscore chars should be of syntax-class `word', not of `symbol'.

Underscores in word-class makes `forward-word' etc. travel the indentifiers. Default is `t'.

See bug report at launchpad, lp:940812 "
  :type 'boolean
  :group 'python-mode
  :set (lambda (symbol value)
         (set-default symbol value)
         (toggle-py-underscore-word-syntax-p (if value 1 0))))

;; py-electric-comment-p forms
(defun toggle-py-electric-comment-p (&optional arg)
  "If `py-electric-comment-p' should be on or off.

  Returns value of `py-electric-comment-p' switched to. "
  (interactive)
  (let ((arg (or arg (if py-electric-comment-p -1 1))))
    (if (< 0 arg)
        (setq py-electric-comment-p t)
      (setq py-electric-comment-p nil))
    (when (or py-verbose-p (interactive-p)) (message "py-electric-comment-p: %s" py-electric-comment-p))
    py-electric-comment-p))

(defun py-electric-comment-p-on (&optional arg)
  "Make sure, py-electric-comment-p' is on.

Returns value of `py-electric-comment-p'. "
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-py-electric-comment-p arg))
  (when (or py-verbose-p (interactive-p)) (message "py-electric-comment-p: %s" py-electric-comment-p))
  py-electric-comment-p)

(defun py-electric-comment-p-off ()
  "Make sure, `py-electric-comment-p' is off.

Returns value of `py-electric-comment-p'. "
  (interactive)
  (toggle-py-electric-comment-p -1)
  (when (or py-verbose-p (interactive-p)) (message "py-electric-comment-p: %s" py-electric-comment-p))
  py-electric-comment-p)

;; toggle-force-local-shell
(defun toggle-force-local-shell (&optional arg)
  "If locally indicated Python shell should be taken and
enforced upon sessions execute commands.

Toggles boolean `py-force-local-shell-p' along with `py-force-py-shell-name-p'
Returns value of `toggle-force-local-shell' switched to.

When on, kind of an option 'follow', local shell sets `py-shell-name', enforces its use afterwards.

See also commands
`py-force-local-shell-on'
`py-force-local-shell-off'
 "
  (interactive (list arg))
  (let ((arg (or arg (if py-force-local-shell-p -1 1))))
    (if (< 0 arg)
        (progn
          (setq py-shell-name (or py-local-command (py-choose-shell)))
          (setq py-force-local-shell-p t))
      (setq py-shell-name (default-value 'py-shell-name))
      (setq py-force-local-shell-p nil))
    (when (interactive-p)
      (if py-force-local-shell-p
          (when py-verbose-p (message "Enforce %s"  py-shell-name))
        (when py-verbose-p (message "py-shell-name default restored to: %s" py-shell-name))))
    py-shell-name))

(defun py-force-local-shell-on ()
  "Make sure, `py-py-force-local-shell-p' is on.

Returns value of `py-force-local-shell-p'.

Kind of an option 'follow', local shell sets `py-shell-name', enforces its use afterwards "
  (interactive)
  (let* ((erg (toggle-force-local-shell 1)))
    (when (or py-verbose-p (interactive-p))
      (message "Enforce %s" py-shell-name))))

(defun py-force-local-shell-off ()
  "Restore `py-shell-name' default value and `behaviour'. "
  (interactive)
  (let* ((erg (toggle-force-local-shell 1)))
    (when (or py-verbose-p (interactive-p))
      (message "py-shell-name default restored to: %s" py-shell-name)
      (message "Enforce %s" py-shell-name))))

;; toggle-force-py-shell-name-p forms
(defun toggle-force-py-shell-name-p (&optional arg)
  "If customized default `py-shell-name' should be enforced upon execution.

If `py-force-py-shell-name-p' should be on or off.
Returns value of `py-force-py-shell-name-p' switched to.

See also commands
force-py-shell-name-p-on
force-py-shell-name-p-off

Caveat: Completion might not work that way.
"
  (interactive "p")
  (let ((arg (or arg (if py-force-py-shell-name-p -1 1))))
    (if (< 0 arg)
        (setq py-force-py-shell-name-p t)
      (setq py-force-py-shell-name-p nil))
    (when (or py-verbose-p (interactive-p)) (message "py-force-py-shell-name-p: %s" py-force-py-shell-name-p))
    py-force-py-shell-name-p))

(defun force-py-shell-name-p-on (&optional arg)
  "Switches `py-force-py-shell-name-p' on.

Customized default `py-shell-name' will be enforced upon execution.
Returns value of `py-force-py-shell-name-p'.

Caveat: Completion might not work that way.
"
  (interactive "p")
  (let ((arg (or arg 1)))
    (toggle-force-py-shell-name-p arg))
  (when (or py-verbose-p (interactive-p)) (message "py-force-py-shell-name-p: %s" py-force-py-shell-name-p))
  py-force-py-shell-name-p)

(defun force-py-shell-name-p-off ()
  "Make sure, `py-force-py-shell-name-p' is off.

Function to use by executes will be guessed from environment.
Returns value of `py-force-py-shell-name-p'. "
  (interactive)
  (toggle-force-py-shell-name-p -1)
  (when (or py-verbose-p (interactive-p)) (message "py-force-py-shell-name-p: %s" py-force-py-shell-name-p))
  py-force-py-shell-name-p)

;; py-toggle-indent-tabs-mode
(defun py-toggle-indent-tabs-mode ()
  "Toggle `indent-tabs-mode'.

Returns value of `indent-tabs-mode' switched to. "
  (interactive)
  (when
      (setq indent-tabs-mode (not indent-tabs-mode))
    (setq tab-width py-indent-offset))
  (when (and py-verbose-p (interactive-p)) (message "indent-tabs-mode %s  py-indent-offset %s" indent-tabs-mode py-indent-offset))
  indent-tabs-mode)

(defun py-indent-tabs-mode-on (arg)
  "Switch `indent-tabs-mode' on. "
  (interactive "p")
  (py-indent-tabs-mode (abs arg)(interactive-p)))

(defun py-indent-tabs-mode-off (arg)
  "Switch `indent-tabs-mode' on. "
  (interactive "p")
  (py-indent-tabs-mode (- (abs arg))(interactive-p)))

;; py-jump-on-exception forms
(defun toggle-py-jump-on-exception (&optional arg)
  "If `py-jump-on-exception' should be on or off.

  Returns value of `py-jump-on-exception' switched to. "
  (interactive)
  (let ((arg (or arg (if py-jump-on-exception -1 1))))
    (if (< 0 arg)
        (setq py-jump-on-exception t)
      (setq py-jump-on-exception nil))
    (when (or py-verbose-p (interactive-p)) (message "py-jump-on-exception: %s" py-jump-on-exception))
    py-jump-on-exception))

(defun py-jump-on-exception-on (&optional arg)
  "Make sure, py-jump-on-exception' is on.

Returns value of `py-jump-on-exception'. "
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-py-jump-on-exception arg))
  (when (or py-verbose-p (interactive-p)) (message "py-jump-on-exception: %s" py-jump-on-exception))
  py-jump-on-exception)

(defun py-jump-on-exception-off ()
  "Make sure, `py-jump-on-exception' is off.

Returns value of `py-jump-on-exception'. "
  (interactive)
  (toggle-py-jump-on-exception -1)
  (when (or py-verbose-p (interactive-p)) (message "py-jump-on-exception: %s" py-jump-on-exception))
  py-jump-on-exception)

;; python-mode-v5-behavior-p forms
(defun toggle-python-mode-v5-behavior-p (&optional arg)
  "If `python-mode-v5-behavior-p' should be on or off.

  Returns value of `python-mode-v5-behavior-p' switched to. "
  (interactive)
  (let ((arg (or arg (if python-mode-v5-behavior-p -1 1))))
    (if (< 0 arg)
        (setq python-mode-v5-behavior-p t)
      (setq python-mode-v5-behavior-p nil))
    (when (or py-verbose-p (interactive-p)) (message "python-mode-v5-behavior-p: %s" python-mode-v5-behavior-p))
    python-mode-v5-behavior-p))

(defun python-mode-v5-behavior-p-on (&optional arg)
  "Make sure, `python-mode-v5-behavior-p' is on.

Returns value of `python-mode-v5-behavior-p'. "
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-python-mode-v5-behavior-p arg))
  (when (or py-verbose-p (interactive-p)) (message "python-mode-v5-behavior-p: %s" python-mode-v5-behavior-p))
  python-mode-v5-behavior-p)

(defun python-mode-v5-behavior-p-off ()
  "Make sure, `python-mode-v5-behavior-p' is off.

Returns value of `python-mode-v5-behavior-p'. "
  (interactive)
  (toggle-python-mode-v5-behavior-p -1)
  (when (or py-verbose-p (interactive-p)) (message "python-mode-v5-behavior-p: %s" python-mode-v5-behavior-p))
  python-mode-v5-behavior-p)

;;
(defalias 'toggle-py-shell-switch-buffers-on-execute 'py-toggle-shell-switch-buffers-on-execute)
(defun py-toggle-shell-switch-buffers-on-execute (&optional arg)
  "If `py-switch-buffers-on-execute-p' should be on or off.

  Returns value of `py-switch-buffers-on-execute-p' switched to. "
  (interactive)
  (let ((arg (or arg (if py-switch-buffers-on-execute-p -1 1))))
    (if (< 0 arg)
        (setq py-switch-buffers-on-execute-p t)
      (setq py-switch-buffers-on-execute-p nil))
    (when (interactive-p) (message "py-shell-switch-buffers-on-execute: %s" py-switch-buffers-on-execute-p))
    py-switch-buffers-on-execute-p))

(defun py-shell-switch-buffers-on-execute-on (&optional arg)
  "Make sure, `py-switch-buffers-on-execute-p' is on.

Returns value of `py-switch-buffers-on-execute-p'. "
  (interactive "p")
  (let ((arg (or arg 1)))
    (toggle-py-shell-switch-buffers-on-execute arg))
  (when (interactive-p) (message "py-shell-switch-buffers-on-execute: %s" py-switch-buffers-on-execute-p))
  py-switch-buffers-on-execute-p)

(defun py-shell-switch-buffers-on-execute-off ()
  "Make sure, `py-switch-buffers-on-execute-p' is off.

Returns value of `py-switch-buffers-on-execute-p'. "
  (interactive)
  (toggle-py-shell-switch-buffers-on-execute -1)
  (when (interactive-p) (message "py-shell-switch-buffers-on-execute: %s" py-switch-buffers-on-execute-p))
  py-switch-buffers-on-execute-p)

;; split-windows-on-execute
(defalias 'toggle-py-split-windows-on-execute 'py-toggle-split-windows-on-execute)
(defun py-toggle-split-windows-on-execute (&optional arg)
  "If `py-split-windows-on-execute-p' should be on or off.

  Returns value of `py-split-windows-on-execute-p' switched to. "
  (interactive)
  (let ((arg (or arg (if py-split-windows-on-execute-p -1 1))))
    (if (< 0 arg)
        (setq py-split-windows-on-execute-p t)
      (setq py-split-windows-on-execute-p nil))
    (when (interactive-p) (message "py-split-windows-on-execute-p: %s" py-split-windows-on-execute-p))
    py-split-windows-on-execute-p))

(defun py-split-windows-on-execute-on (&optional arg)
  "Make sure, `py-split-windows-on-execute-p' is on.

Returns value of `py-split-windows-on-execute-p'. "
  (interactive "p")
  (let ((arg (or arg 1)))
    (toggle-py-split-windows-on-execute arg))
  (when (interactive-p) (message "py-split-windows-on-execute-p: %s" py-split-windows-on-execute-p))
  py-split-windows-on-execute-p)

(defun py-split-windows-on-execute-off ()
  "Make sure, `py-split-windows-on-execute-p' is off.

Returns value of `py-split-windows-on-execute-p'. "
  (interactive)
  (toggle-py-split-windows-on-execute -1)
  (when (interactive-p) (message "py-split-windows-on-execute-p: %s" py-split-windows-on-execute-p))
  py-split-windows-on-execute-p)

;; highlight-indentation
(defun py-toggle-highlight-indentation (&optional indent)
  "If `highlight-indentation-p' should be on or off. "
  (interactive "P")
  (unless (featurep 'highlight-indentation)
    (load (concat (py-normalize-directory py-install-directory) "extensions" py-separator-char "highlight-indentation.el")))
  (highlight-indentation indent)
  (when py-verbose-p (message "highlight-indent-active: %s" highlight-indent-active))
  highlight-indent-active)

(defun py-highlight-indentation-off ()
  "If `highlight-indentation-p' should be on or off. "
  (interactive)
  (unless (featurep 'highlight-indentation)
    (load (concat (py-normalize-directory py-install-directory) "extensions" py-separator-char "highlight-indentation.el")))
  (highlight-indentation-off)
  (when py-verbose-p (message "highlight-indent-active: %s" highlight-indent-active))
  highlight-indent-active)

(defun py-highlight-indentation-on ()
  "If `highlight-indentation-p' should be on or off. "
  (interactive "P")
  (unless (featurep 'highlight-indentation)
    (load (concat (py-normalize-directory py-install-directory) "extensions" py-separator-char "highlight-indentation.el")))
  (highlight-indentation-on)
  (when py-verbose-p (message "highlight-indent-active: %s" highlight-indent-active))
  highlight-indent-active)

;; Smart indentation
(defalias 'toggle-py-smart-indentation 'py-toggle-smart-indentation)
(defun py-toggle-smart-indentation (&optional arg)
  "If `py-smart-indentation' should be on or off.

Returns value of `py-smart-indentation' switched to. "
  (interactive)
  (let ((arg (or arg (if py-smart-indentation -1 1))))
    (if (< 0 arg)
        (progn
          (setq py-smart-indentation t)
          (py-guess-indent-offset))
      (setq py-smart-indentation nil)
      (setq py-indent-offset (default-value 'py-indent-offset)))
    (when (interactive-p) (message "py-smart-indentation: %s" py-smart-indentation))
    py-smart-indentation))

(defun py-smart-indentation-on (&optional arg)
  "Make sure, `py-smart-indentation' is on.

Returns value of `py-smart-indentation'. "
  (interactive "p")
  (let ((arg (or arg 1)))
    (toggle-py-smart-indentation arg))
  (when (interactive-p) (message "py-smart-indentation: %s" py-smart-indentation))
  py-smart-indentation)

(defun py-smart-indentation-off (&optional arg)
  "Make sure, `py-smart-indentation' is off.

Returns value of `py-smart-indentation'. "
  (interactive "p")
  (let ((arg (if arg (- arg) -1)))
    (toggle-py-smart-indentation arg))
  (when (interactive-p) (message "py-smart-indentation: %s" py-smart-indentation))
  py-smart-indentation)

;; Smart operator
(defun toggle-py-smart-operator-mode-p (&optional arg)
  "If `py-smart-operator-mode-p' should be on or off.

  Returns value of `py-smart-operator-mode-p' switched to. "
  (interactive)
  (and (py-smart-operator-check)
       (setq py-smart-operator-mode-p (smart-operator-mode (if smart-operator-mode 0 1)))))

(defun py-smart-operator-mode-p-on ()
  "Make sure, py-smart-operator-mode-p' is on.

Returns value of `py-smart-operator-mode-p'. "
  (interactive)
  (and (py-smart-operator-check)
       (setq py-smart-operator-mode-p (smart-operator-mode 1))))

(defun py-smart-operator-mode-p-off ()
  "Make sure, py-smart-operator-mode-p' is off.

Returns value of `py-smart-operator-mode-p'. "
  (interactive)
  (setq py-smart-operator-mode-p (smart-operator-mode 0)))

;; py-use-current-dir-when-execute-p forms
(defun toggle-py-use-current-dir-when-execute-p (&optional arg)
  "If `py-use-current-dir-when-execute-p' should be on or off.

  Returns value of `py-use-current-dir-when-execute-p' switched to. "
  (interactive)
  (let ((arg (or arg (if py-use-current-dir-when-execute-p -1 1))))
    (if (< 0 arg)
        (setq py-use-current-dir-when-execute-p t)
      (setq py-use-current-dir-when-execute-p nil))
    (when (or py-verbose-p (interactive-p)) (message "py-use-current-dir-when-execute-p: %s" py-use-current-dir-when-execute-p))
    py-use-current-dir-when-execute-p))

(defun py-use-current-dir-when-execute-p-on (&optional arg)
  "Make sure, py-use-current-dir-when-execute-p' is on.

Returns value of `py-use-current-dir-when-execute-p'. "
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-py-use-current-dir-when-execute-p arg))
  (when (or py-verbose-p (interactive-p)) (message "py-use-current-dir-when-execute-p: %s" py-use-current-dir-when-execute-p))
  py-use-current-dir-when-execute-p)

(defun py-use-current-dir-when-execute-p-off ()
  "Make sure, `py-use-current-dir-when-execute-p' is off.

Returns value of `py-use-current-dir-when-execute-p'. "
  (interactive)
  (toggle-py-use-current-dir-when-execute-p -1)
  (when (or py-verbose-p (interactive-p)) (message "py-use-current-dir-when-execute-p: %s" py-use-current-dir-when-execute-p))
  py-use-current-dir-when-execute-p)

;; py-autopair-mode forms
(defalias 'toggle-py-autopair-mode 'py-toggle-autopair-mode)
(defun py-toggle-autopair-mode (&optional arg)
  "If `py-autopair-mode' should be on or off.

  Returns value of `py-autopair-mode' switched to. "
  (interactive)
  (and (py-autopair-check)
       (setq py-autopair-mode (autopair-mode (if autopair-mode 0 1)))))

(defun py-autopair-mode-on ()
  "Make sure, py-autopair-mode' is on.

Returns value of `py-autopair-mode'. "
  (interactive)
  (and (py-autopair-check)
       (setq py-autopair-mode (autopair-mode 1))))

(defun py-autopair-mode-off ()
  "Make sure, py-autopair-mode' is off.

Returns value of `py-autopair-mode'. "
  (interactive)
  (setq py-autopair-mode (autopair-mode 0)))

;; py-switch-buffers-on-execute-p forms
(defun toggle-py-switch-buffers-on-execute-p (&optional arg)
  "If `py-switch-buffers-on-execute-p' should be on or off.

  Returns value of `py-switch-buffers-on-execute-p' switched to. "
  (interactive)
  (let ((arg (or arg (if py-switch-buffers-on-execute-p -1 1))))
    (if (< 0 arg)
        (setq py-switch-buffers-on-execute-p t)
      (setq py-switch-buffers-on-execute-p nil))
    (when (or py-verbose-p (interactive-p)) (message "py-switch-buffers-on-execute-p: %s" py-switch-buffers-on-execute-p))
    py-switch-buffers-on-execute-p))

(defun py-switch-buffers-on-execute-p-on (&optional arg)
  "Make sure, `py-py-switch-buffers-on-execute-p' is on.

Returns value of `py-switch-buffers-on-execute-p'. "
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-py-switch-buffers-on-execute-p arg))
  (when (or py-verbose-p (interactive-p)) (message "py-switch-buffers-on-execute-p: %s" py-switch-buffers-on-execute-p))
  py-switch-buffers-on-execute-p)

(defun py-switch-buffers-on-execute-p-off ()
  "Make sure, `py-switch-buffers-on-execute-p' is off.

Returns value of `py-switch-buffers-on-execute-p'. "
  (interactive)
  (toggle-py-switch-buffers-on-execute-p -1)
  (when (or py-verbose-p (interactive-p)) (message "py-switch-buffers-on-execute-p: %s" py-switch-buffers-on-execute-p))
  py-switch-buffers-on-execute-p)

;; py-split-windows-on-execute-p forms
(defun toggle-py-split-windows-on-execute-p (&optional arg)
  "If `py-split-windows-on-execute-p' should be on or off.

  Returns value of `py-split-windows-on-execute-p' switched to. "
  (interactive)
  (let ((arg (or arg (if py-split-windows-on-execute-p -1 1))))
    (if (< 0 arg)
        (setq py-split-windows-on-execute-p t)
      (setq py-split-windows-on-execute-p nil))
    (when (or py-verbose-p (interactive-p)) (message "py-split-windows-on-execute-p: %s" py-split-windows-on-execute-p))
    py-split-windows-on-execute-p))

(defun py-split-windows-on-execute-p-on (&optional arg)
  "Make sure, `py-py-split-windows-on-execute-p' is on.

Returns value of `py-split-windows-on-execute-p'. "
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-py-split-windows-on-execute-p arg))
  (when (or py-verbose-p (interactive-p)) (message "py-split-windows-on-execute-p: %s" py-split-windows-on-execute-p))
  py-split-windows-on-execute-p)

(defun py-split-windows-on-execute-p-off ()
  "Make sure, `py-split-windows-on-execute-p' is off.

Returns value of `py-split-windows-on-execute-p'. "
  (interactive)
  (toggle-py-split-windows-on-execute-p -1)
  (when (or py-verbose-p (interactive-p)) (message "py-split-windows-on-execute-p: %s" py-split-windows-on-execute-p))
  py-split-windows-on-execute-p)

(defun py-toggle-sexp-function ()
  "Opens customization "
  (interactive)
  (customize-variable 'py-sexp-function))

;;;

;;;

;; Stolen from org-mode
(defun py-util-clone-local-variables (from-buffer &optional regexp)
  "Clone local variables from FROM-BUFFER.
Optional argument REGEXP selects variables to clone and defaults
to \"^python-\"."
  (mapc
   (lambda (pair)
     (and (symbolp (car pair))
          (string-match (or regexp "^python-")
                        (symbol-name (car pair)))
	  (set (make-local-variable (car pair))
	       (cdr pair))))
   (buffer-local-variables from-buffer)))

(defun py-send-shell-setup-code ()
  "Send all setup code for shell.
This function takes the list of setup code to send from the
`py-setup-codes' list."
  (let ((process (get-buffer-process (current-buffer))))
    (accept-process-output process 1)
    (dolist (code py-setup-codes)
      (when code
        (py-send-string (symbol-value code) process)))))

(defun py-shell-get-process-name (py-dedicated-process-p)
  "Calculate the appropiate process name for inferior Python process.
If DEDICATED is t and the variable `buffer-file-name' is non-nil
returns a string with the form
`python-shell-buffer-name'[variable `buffer-file-name'] else
returns the value of `python-shell-buffer-name'.  After
calculating the process name adds the buffer name for the process
in the `same-window-buffer-names' list."
  (let ((process-name
         (if (and py-dedicated-process-p
                  buffer-file-name)
             (format "%s[%s]" python-shell-buffer-name buffer-file-name)
           (format "%s" python-shell-buffer-name))))
    (add-to-list 'same-window-buffer-names (purecopy
                                            (format "*%s*" process-name)))
    process-name))

(defun py-shell-get-process (&optional argprompt py-dedicated-process-p shell switch py-buffer-name done)
  "Get appropriate Python process for current buffer and return it."
  (interactive)
  (let ((erg (get-buffer-process (py-shell argprompt py-dedicated-process-p shell py-buffer-name done))))
    (when (interactive-p) (message "%S" erg))
    erg))

(defun py-shell-send-string (string &optional process msg filename)
  "Send STRING to inferior Python PROCESS.
When `py-verbose-p' and MSG is non-nil messages the first line of STRING."
  (interactive "sPython command: ")
  (let* ((process (or process (get-buffer-process (py-shell))))
         (lines (split-string string "\n"))
         (temp-file-name (concat (with-current-buffer (process-buffer process)
                                   (file-remote-p default-directory))
                                 (py-normalize-directory py-temp-directory)
                                 "psss-temp.py"))
         (file-name (or filename (buffer-file-name) temp-file-name)))
    (if (> (length lines) 1)
        (let* ()
          ;; ((temporary-file-directory
          ;;     (if (file-remote-p default-directory)
          ;;         (concat (file-remote-p default-directory) "/tmp")
          ;;       temporary-file-directory))
          ;;    (temp-file-name (make-temp-file "py"))
          ;;    (file-name (or (buffer-file-name) temp-file-name)))
          (with-temp-file temp-file-name
            (insert string)
            (delete-trailing-whitespace))
          (py-send-file temp-file-name process temp-file-name))
      (comint-send-string process string)
      (when (or (not (string-match "\n$" string))
                (string-match "\n[ \t].*\n?$" string))
        (comint-send-string process "\n")))))

(defun py-send-string-no-output (string &optional process msg)
  "Send STRING to PROCESS and inhibit output.
When MSG is non-nil messages the first line of STRING.  Return
the output."
  (let* ((output-buffer)
         (process (or process (get-buffer-process (py-shell))))
         (comint-preoutput-filter-functions
          (append comint-preoutput-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq output-buffer (concat output-buffer string))
                      "")))))
    (py-shell-send-string string process msg)
    (accept-process-output process 1)
    (when output-buffer
      (replace-regexp-in-string
       (if (> (length py-shell-prompt-output-regexp) 0)
           (format "\n*%s$\\|^%s\\|\n$"
                   py-shell-prompt-regexp
                   (or py-shell-prompt-output-regexp ""))
         (format "\n*$\\|^%s\\|\n$"
                 py-shell-prompt-regexp))
       "" output-buffer))))

(defun py-send-string-return-output (string &optional process msg)
  "Send STRING to PROCESS and return output.

When MSG is non-nil messages the first line of STRING.  Return
the output."
  (let* (output-buffer
         (process (or process (get-buffer-process (py-shell))))
         (comint-preoutput-filter-functions
          (append comint-preoutput-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq output-buffer (concat output-buffer string))
                      "")))))
    (py-shell-send-string string process msg)
    (accept-process-output process 1)
    (when output-buffer
      (setq output-buffer
            (replace-regexp-in-string
             (if (> (length py-shell-prompt-output-regexp) 0)
                 (format "\n*%s$\\|^%s\\|\n$"
                         python-shell-prompt-regexp
                         (or py-shell-prompt-output-regexp ""))
               (format "\n*$\\|^%s\\|\n$"
                       python-shell-prompt-regexp))
             "" output-buffer)))
    output-buffer))

(defun py-shell-send-file (file-name &optional process temp-file-name)
  "Send FILE-NAME to inferior Python PROCESS.
If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME."
  (interactive "fFile to send: ")
  (let* ((process (or process (get-buffer-process (py-shell))))
         (temp-file-name (when temp-file-name
                           (expand-file-name temp-file-name)))
         (file-name (or (expand-file-name file-name) temp-file-name))
         py-python-command-args)
    (when (not file-name)
      (error "If FILE-NAME is nil then TEMP-FILE-NAME must be non-nil"))
    (py-shell-send-string
     (format
      (concat "__pyfile = open('''%s''');"
              "exec(compile(__pyfile.read(), '''%s''', 'exec'));"
              "__pyfile.close()")
      file-name file-name)
     process)))

(defun py-switch-to-shell ()
  "Switch to inferior Python process buffer."
  (interactive)
  (pop-to-buffer (py-shell) t))

;;;

(defun py-shell-completion--get-completions (input process completion-code)
  "Retrieve available completions for INPUT using PROCESS.
Argument COMPLETION-CODE is the python code used to get
completions on the current context."
  (with-current-buffer (process-buffer process)
    (let ((completions
           (py-send-string-no-output
            (format completion-code input) process)))
      (when (> (length completions) 2)
        (split-string completions "^'\\|^\"\\|;\\|'$\\|\"$" t)))))

(defun py-shell--do-completion-at-point (process imports input)
  "Do completion at point for PROCESS."
  (with-syntax-table py-dotted-expression-syntax-table
    (when imports (py-send-string-no-output imports process))
    (let* ((code python-shell-module-completion-string-code)
           (completions
            (py-shell-completion--get-completions
             input process code))
           (completion (when completions
                         (try-completion input completions))))
      (cond ((eq completion t)
             (if (eq this-command last-command)
                 (when py-completion-last-window-configuration
                   (set-window-configuration
                    py-completion-last-window-configuration)))
             ;; (setq py-completion-last-window-configuration nil)
             (if py-no-completion-calls-dabbrev-expand-p
                 (or (ignore-errors (dabbrev-expand nil))(when py-indent-no-completion-p
                                                           (tab-to-tab-stop)))
               (when py-indent-no-completion-p
                 (tab-to-tab-stop)))
             nil)
            ((null completion)
             (if py-no-completion-calls-dabbrev-expand-p
                 (or (dabbrev-expand nil)(when py-indent-no-completion-p
                                           (tab-to-tab-stop))(message "Can't find completion "))
               (when py-indent-no-completion-p
                 (tab-to-tab-stop)))
             nil)
            ((not (string= input completion))
             (progn (delete-char (- (length input)))
                    (insert completion)
                    ;; minibuffer.el expects a list, a bug IMO
                    nil))
            (t
             (unless py-completion-last-window-configuration
               (setq py-completion-last-window-configuration
                     (current-window-configuration)))
             (with-output-to-temp-buffer "*Python Completions*"
               (display-completion-list
                (all-completions input completions)))
             nil)))))

(defun python-shell-completion-complete-or-indent ()
  "Complete or indent depending on the context.
If content before pointer is all whitespace indent.  If not try
to complete."
  (interactive)
  (if (string-match "^[[:space:]]*$"
                    (buffer-substring (comint-line-beginning-position)
                                      (point-marker)))
      (indent-for-tab-command)
    (comint-dynamic-complete)))

(setq py-shell-template "
\(defun NAME (&optional argprompt)
  \"Start an DOCNAME interpreter in another window.

With optional \\\\[universal-argument] user is prompted
for options to pass to the DOCNAME interpreter. \"
  (interactive \"P\")
  (let\* ((py-shell-name \"FULLNAME\"))
    (py-shell argprompt)
    (when (interactive-p) (switch-to-buffer (current-buffer))
          (goto-char (point-max)))))
")

(defsubst py-keep-region-active ()
  "Keep the region active in XEmacs."
  ;; Ignore byte-compiler warnings you might see.  Also note that
  ;; FSF's Emacs does it differently; its policy doesn't require us
  ;; to take explicit action.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

;;; Helper commands
(defun py-guess-pdb-path ()
  "If py-pdb-path isn't set, find location of pdb.py. "
  (interactive)
  (let ((ele (split-string (shell-command-to-string "whereis python")))
        erg)
    (while (or (not erg)(string= "" erg))
      (when (and (string-match "^/" (car ele)) (not (string-match "/man" (car ele))))
        (setq erg (shell-command-to-string (concat "find " (car ele) " -type f -name \"pdb.py\""))))
      (setq ele (cdr ele)))
    (if erg
        (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      (when (interactive-p) (message "%s" "pdb.py not found, please customize `pdb-path'")))
    (concat "'" erg)))

;; Strip CHARS from STRING
(defun string-strip (str &optional chars-before chars-after)
  "Return a copy of STR, CHARS removed.
`CHARS-BEFORE' and `CHARS-AFTER' default is \"[ \t\r\n]*\",
i.e. spaces, tabs, carriage returns, newlines and newpages. "
  (let ((s-c-b (or chars-before
                   strip-chars-before))
        (s-c-a (or chars-after
                   strip-chars-after))
        (erg str))
    (setq erg (replace-regexp-in-string  s-c-b "" erg))
    (setq erg (replace-regexp-in-string  s-c-a "" erg))
    erg))

(defun py-warn-tmp-files-left ()
  "Detect and warn about file of form \"py11046IoE\" in py-temp-directory. "
  (let ((erg1 (file-readable-p (concat py-temp-directory py-separator-char  (car (directory-files  py-temp-directory nil "py[[:alnum:]]+$"))))))
    (when (and py-verbose-p erg1)
      (message "py-warn-tmp-files-left: %s ?" (concat py-temp-directory py-separator-char (car (directory-files  py-temp-directory nil "py[[:alnum:]]*$")))))))

(defun py-forward-line (&optional arg)
  "Goes to end of line after forward move.

Travels right-margin comments. "
  (interactive "p")
  (let ((arg (or arg 1)))
    (forward-line arg)
    (end-of-line)
    (skip-chars-backward " \t")
    (py-beginning-of-comment)
    (skip-chars-backward " \t")))

(defun py-go-to-beginning-of-comment ()
  "Go to the beginning of current line's comment, if any.

From a programm use `py-beginning-of-comment' instead "
  (interactive)
  (let ((erg (py-beginning-of-comment)))
    (when (and py-verbose-p (interactive-p))
      (message "%s" erg))))

(defun py-clause-lookup-keyword (regexp arg &optional indent orig origline)
  "Returns a list, whose car is indentation, cdr position. "
  (let* ((orig (or orig (point)))
         (origline (or origline (py-count-lines)))
         (stop (if (< 0 arg)'(eobp)'(bobp)))
         (function (if (< 0 arg) 'py-end-of-statement 'py-beginning-of-statement))
         (count 1)
         (maxindent (cond (indent indent)
                          ((< (py-count-lines) origline)
                           (current-indentation))
                          (t 0)))
         (complement-re
          (cond ((or (string-match "finally" regexp)
                     (string-match "except" regexp))
                 py-try-re)
                ((string-match "elif" regexp)
                 py-if-re)
                ((string-match "else" regexp)
                 py-minor-block-re)))
         (first t)
         erg done strict)
    (while (and (not (eval stop))
                (< 0 count)
                (or done (setq erg (funcall function))))
      (setq done nil)
      (when (and first (< maxindent (current-indentation)))
        (setq maxindent (current-indentation))
        (setq first nil))
      (when (if strict
                (< (current-indentation) maxindent)
              (<= (current-indentation) maxindent))
        (unless (looking-at py-block-or-clause-re)
          (setq maxindent (current-indentation)))
        ;; (message "%s %s" count indent)
        ;; nesting
        (cond
         ((and (looking-at "\\_<finally\\>[: \n\t]")(save-match-data (string-match regexp "finally")))
          (setq indent (current-indentation))
          (while
              (and
               (not (eval stop))
               (funcall function)
               (setq done t)
               (not (and (eq indent (current-indentation)) (looking-at "try"))))))
         ;; ((and (looking-at "\\<except\\>[: \n\t]")(save-match-data (string-match "else" regexp)))
         ;;  (setq indent (current-indentation))
         ;;  (setq count (1+ count))
         ;;  (while
         ;;      (and
         ;;       (not (eval stop))
         ;;       (funcall function)
         ;;       (setq done t)
         ;;       (not (and (eq indent (current-indentation)) (looking-at "try\\|if"))))))
         ((and (looking-at "\\<else\\>[: \n\t]")(save-match-data (string-match "else" regexp)))
          (setq indent (current-indentation))
          (setq count (1+ count))
          (while
              (and
               (not (eval stop))
               (funcall function)
               (setq done t)
               (not (and (eq indent (current-indentation)) (looking-at "try\\|if"))))))
         ((and (looking-at "\\_<else\\>[: \n\t]")(save-match-data (string-match "else" regexp)))
          (setq indent (current-indentation))
          (setq count (1+ count))
          (while
              (and
               (not (eval stop))
               (funcall function)
               (setq done t)
               (not (and (eq indent (current-indentation)) (looking-at "try\\|if"))))))
         ((and (looking-at "\\_<elif\\>[ \n\t]")(save-match-data (string-match "elif" regexp)))
          (setq indent (current-indentation))
          (while
              (and
               (not (eval stop))
               (funcall function)
               (setq done t)
               ;; doesn't mean nesting yet
               (setq count (1- count))
               (not (and (eq indent (current-indentation)) (looking-at "if"))))))
         ((and (looking-at complement-re)(<= (current-indentation) maxindent))
          (setq count (1- count)))
         (t (cond ((and (string-match "except" regexp)(looking-at py-block-re))
                   (setq count (1- count)))
                  ((and (string-match "else" regexp)(looking-at "except"))
                   (current-indentation))
                  (t
                   (setq strict t)
                   ))))))
    (when erg
      (if (looking-at py-def-or-class-re)
          (setq erg (cons (+ (current-indentation) py-indent-offset) erg))
        (setq erg (cons (current-indentation) erg))))
    erg))

(defun py-leave-comment-or-string-backward (&optional pos)
  "If inside a comment or string, leave it backward. "
  (interactive)
  (let ((pps
         (if (featurep 'xemacs)
             (parse-partial-sexp (point-min) (point))
           (syntax-ppss))))
    (when (nth 8 pps)
      (goto-char (1- (nth 8 pps))))))

(defun py-beginning-of-list-pps (&optional iact last ppstart orig done)
  "Go to the beginning of a list.
Optional ARG indicates a start-position for `parse-partial-sexp'.
Return beginning position, nil if not inside."
  (interactive "p")
  (let* ((orig (or orig (point)))
         (ppstart (or ppstart (re-search-backward "^[a-zA-Z]" nil t 1) (point-min)))
         erg)
    (unless done (goto-char orig))
    (setq done t)
    (if
        (setq erg (nth 1 (if (featurep 'xemacs)
                             (parse-partial-sexp ppstart (point))
                           (syntax-ppss))))
        (progn
          (setq last erg)
          (goto-char erg)
          (py-beginning-of-list-pps iact last ppstart orig done))
      (when iact (message "%s" last))
      last)))

(when (featurep 'thing-at-point-utils)
  (defun py-beginning-of-list (&optional iact orig limit done last)
    "Go to beginning of any parentized, braced or bracketed expression in statement. "
    (interactive "p")
    (save-restriction
      (let ((orig (or orig (point)))
            (done done)
            (limit (or limit (re-search-backward "^[a-zA-Z]" nil t 1)))
            (last last))
        (unless (or done (not limit)) (narrow-to-region limit (point-max)))
        (setq done t)
        (goto-char orig)
        (let* ((pt (car-safe (ar-in-parentized-p-atpt)))
               (br (car-safe (ar-in-braced-p-atpt)))
               (bk (car-safe (ar-in-bracketed-p-atpt)))
               (erg (car (sort (delq nil (list pt br bk)) '<))))
          (if erg
              (progn
                (goto-char (1- erg))
                (setq last erg)
                (py-beginning-of-list iact (1- erg) limit done last))
            (when last
              (goto-char last))
            (when iact (message "%s" last))
            last)))))

  (defun py-end-of-list (&optional iact orig limit done last)
    "Go to end of any parentized, braced or bracketed expression in statement. "
    (interactive "p")
    (save-restriction
      (let ((orig (or orig (point)))
            (done done)
            (limit (or limit (re-search-backward "^[a-zA-Z]" nil t 1)))
            (last last))
        (unless (or done (not limit)) (narrow-to-region limit (point-max)))
        (setq done t)
        (goto-char orig)
        (let* ((pt (car-safe (ar-in-parentized-p-atpt)))
               (br (car-safe (ar-in-braced-p-atpt)))
               (bk (car-safe (ar-in-bracketed-p-atpt)))
               (erg (car (sort (delq nil (list pt br bk)) '<))))
          (if erg
              (progn
                (goto-char (1- erg))
                (setq last erg)
                (py-end-of-list iact (1- erg) limit done last))
            (when last
              (goto-char last)
              (match-paren)
              (setq last (1+ (point)))
              (when iact (message "%s" last))
              last)))))))

(defun empty-line-p ()
  "Returns t if cursor is at an line with nothing but whitespace-characters, nil otherwise."
  (interactive "p")
  (save-excursion
    (progn
      (beginning-of-line)
      (looking-at "\\s-*$"))))

(defun py-count-lines (&optional start end)
  "Count lines in accessible part until current line.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115"
  (interactive)
  (save-excursion
    (let ((count 0)
          (orig (point)))
      (goto-char (point-min))
      (while (and (< (point) orig)(not (eobp)) (skip-chars-forward "^\n" orig))
        (setq count (1+ count))
        (unless (or (not (< (point) orig)) (eobp)) (forward-char 1)
                (setq count (+ count (abs (skip-chars-forward "\n" orig))))))
      (when (bolp) (setq count (1+ count)))
      (when (interactive-p) (message "%s" count))
      count)))

(eval-when-compile
  (defconst python-rx-constituents
    (list
     `(block-start          . ,(rx symbol-start
                                   (or "def" "class" "if" "elif" "else" "try"
                                       "except" "finally" "for" "while" "with")
                                   symbol-end))
     `(decorator            . ,(rx line-start (* space) ?@ (any letter ?_)
                                   (* (any word ?_))))
     `(defun                . ,(rx symbol-start (or "def" "class") symbol-end))
     `(symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
     `(open-paren           . ,(rx (or "{" "[" "(")))
     `(close-paren          . ,(rx (or "}" "]" ")")))
     `(simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
     `(not-simple-operator  . ,(rx (not (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
     `(operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                       "=" "%" "**" "//" "<<" ">>" "<=" "!="
                                       "==" ">=" "is" "not")))
     `(assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                       ">>=" "<<=" "&=" "^=" "|="))))
    "Additional Python specific sexps for `python-rx'"))

(defmacro python-rx (&rest regexps)
  "Python mode specialized rx macro which supports common python named REGEXPS."
  (let ((rx-constituents (append python-rx-constituents rx-constituents)))
    (cond ((null regexps)
           (error "No regexp"))
          ((cdr regexps)
           (rx-to-string `(and ,@regexps) t))
          (t
           (rx-to-string (car regexps) t)))))

;;;
;; GNU's syntax-ppss-context
(unless (functionp 'syntax-ppss-context)
  (defsubst syntax-ppss-context (ppss)
    (cond
     ((nth 3 ppss) 'string)
     ((nth 4 ppss) 'comment)
     (t nil))))

(defun py-history-input-filter (str)
  "`comint-input-filter' function for inferior Python.
Don't save anything for STR matching `py-history-filter-regexp'."
  (not (string-match py-history-filter-regexp str)))

;; Fixme: Loses with quoted whitespace.
(defun py-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((not (= where 0))
           (cons (substring string 0 where)
                 (python-args-to-list (substring string (+ 1 where)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (if pos (python-args-to-list (substring string pos))))))))

;; Using this stops us getting lines in the buffer like
;; >>> ... ... >>>
;; Also look for (and delete) an `_emacs_ok' string and call
;; `python-preoutput-continuation' if we get it.
(defun py-preoutput-filter (s)
  "`comint-preoutput-filter-functions' function: ignore prompts not at bol."
  (when py-preoutput-leftover
    (setq s (concat py-preoutput-leftover s))
    (setq py-preoutput-leftover nil))
  (let ((start 0)
        (res ""))
    ;; First process whole lines.
    (while (string-match "\n" s start)
      (let ((line (substring s start (setq start (match-end 0)))))
        ;; Skip prompt if needed.
        (when (and py-preoutput-skip-next-prompt
                   (string-match comint-prompt-regexp line))
          (setq py-preoutput-skip-next-prompt nil)
          (setq line (substring line (match-end 0))))
        ;; Recognize special _emacs_out lines.
        (if (and (string-match "\\`_emacs_out \\(.*\\)\n\\'" line)
                 (local-variable-p 'py-preoutput-result))
            (progn
              (setq py-preoutput-result (match-string 1 line))
              (set (make-local-variable 'py-preoutput-skip-next-prompt) t))
          (setq res (concat res line)))))
    ;; Then process the remaining partial line.
    (unless (zerop start) (setq s (substring s start)))
    (cond ((and (string-match comint-prompt-regexp s)
                ;; Drop this prompt if it follows an _emacs_out...
                (or py-preoutput-skip-next-prompt
                    ;; ... or if it's not gonna be inserted at BOL.
                    ;; Maybe we could be more selective here.
                    (if (zerop (length res))
                        (not (bolp))
                      (string-match ".\\'" res))))
           ;; The need for this seems to be system-dependent:
           ;; What is this all about, exactly?  --Stef
           ;; (if (and (eq ?. (aref s 0)))
           ;;     (accept-process-output (get-buffer-process (current-buffer)) 1))
           (setq py-preoutput-skip-next-prompt nil)
           res)
          ((let ((end (min (length "_emacs_out ") (length s))))
             (eq t (compare-strings s nil end "_emacs_out " nil end)))
           ;; The leftover string is a prefix of _emacs_out so we don't know
           ;; yet whether it's an _emacs_out or something else: wait until we
           ;; get more output so we can resolve this ambiguity.
           (set (make-local-variable 'py-preoutput-leftover) s)
           res)
          (t (concat res s)))))

(defun py-send-command (command)
  "Like `py-send-string' but resets `compilation-shell-minor-mode'."
  (when (py-check-comint-prompt)
    (with-current-buffer (process-buffer (py-proc))
      (goto-char (point-max))
      (compilation-forget-errors)
      (py-send-string command)
      (setq compilation-last-buffer (current-buffer)))))

(defun py-send-region (start end)
  "Send the region to the inferior Python process."
  ;; The region is evaluated from a temporary file.  This avoids
  ;; problems with blank lines, which have different semantics
  ;; interactively and in files.  It also saves the inferior process
  ;; buffer filling up with interpreter prompts.  We need a Python
  ;; function to remove the temporary file when it has been evaluated
  ;; (though we could probably do it in Lisp with a Comint output
  ;; filter).  This function also catches exceptions and truncates
  ;; tracebacks not to mention the frame of the function itself.
  ;;
  ;; The `compilation-shell-minor-mode' parsing takes care of relating
  ;; the reference to the temporary file to the source.
  ;;
  ;; Fixme: Write a `coding' header to the temp file if the region is
  ;; non-ASCII.
  (interactive "r")
  (let* ((f (make-temp-file "py"))
         (command
          ;; IPython puts the FakeModule module into __main__ so
          ;; emacs.eexecfile becomes useless.
          (if (or (string-match "[iI][pP]ython[^[:alpha:]]*$" (py-choose-shell))
                  (string-match "[pP]ython3[[:alnum:]:]*$" (py-choose-shell)))
              (format "execfile %S" f)
            (format "emacs.eexecfile(%S)" f)))
         (orig-start (copy-marker start)))
    (when (save-excursion
            (goto-char start)
            (/= 0 (current-indentation))) ; need dummy block
      (save-excursion
        (goto-char orig-start)
        ;; Wrong if we had indented code at buffer start.
        (set-marker orig-start (line-beginning-position 0)))
      (write-region "if True:\n" nil f nil 'nomsg))
    (write-region start end f t 'nomsg)
    (py-send-command command)
    (with-current-buffer (process-buffer (py-proc))
      ;; Tell compile.el to redirect error locations in file `f' to
      ;; positions past marker `orig-start'.  It has to be done *after*
      ;; `py-send-command''s call to `compilation-forget-errors'.
      (compilation-fake-loc orig-start f))))

(defun py-send-region-and-go (start end)
  "Send the region to the inferior Python process.

Then switch to the process buffer."
  (interactive "r")
  (py-send-region start end)
  (py-switch-to-python t))

(defun python-send-string (string)
  "Evaluate STRING in inferior Python process."
  (interactive "sPython command: ")
  (comint-send-string (py-proc) string)
  (unless (string-match "\n\\'" string)
    ;; Make sure the text is properly LF-terminated.
    (comint-send-string (py-proc) "\n"))
  (when (string-match "\n[ \t].*\n?\\'" string)
    ;; If the string contains a final indented line, add a second newline so
    ;; as to make sure we terminate the multiline instruction.
    (comint-send-string (py-proc) "\n")))

(defun py-switch-to-python (eob-p)
  "Switch to the Python process buffer, maybe starting new process.

With prefix arg, position cursor at end of buffer."
  (interactive "P")
  (pop-to-buffer (process-buffer (py-proc)) t) ;Runs python if needed.
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun py-proc (&optional dedicated)
  "Return the current Python process.

Start a new process if necessary. "
  (interactive)
  (let (py-split-windows-on-execute-p
        (erg
         (cond ((and (not py-dedicated-process-p) (comint-check-proc (current-buffer)))
                (get-buffer-process (buffer-name (current-buffer))))
               ((not py-dedicated-process-p)
                (get-buffer-process (py-shell)))
               ((py-shell nil py-dedicated-process-p)))))
    (when (interactive-p) (message "%S" erg))
    erg))

(defun py-check-comint-prompt (&optional proc)
  "Return non-nil if and only if there's a normal prompt in the inferior buffer.
If there isn't, it's probably not appropriate to send input to return Eldoc
information etc.  If PROC is non-nil, check the buffer for that process."
  (with-current-buffer (process-buffer (or proc (get-buffer-process (py-shell))))
    (save-excursion
      (save-match-data
        (re-search-backward " *\\=" nil t)))))



(defun py-outline-level ()
  "`outline-level' function for Python mode.
The level is the number of `py-indent-offset' steps of indentation
of current line."
  (let ((erg (1+ (/ (current-indentation) py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

;; FFAP support
(defalias 'py-module-path 'py-ffap-module-path)

(defun py-ffap-module-path (module)
  "Function for `ffap-alist' to return path for MODULE."
  (let ((process (or
                  (and (eq major-mode 'inferior-python-mode)
                       (get-buffer-process (current-buffer)))
                  (py-shell-get-process))))
    (if (not process)
        nil
      (let ((module-file
             (py-send-string-no-output
              (format py-ffap-string-code module) process)))
        (when module-file
          (substring-no-properties module-file 1 -1))))))

(add-hook 'python-mode-hook 'py-set-ffap-form)


;;;
(defconst py-space-backslash-table
  (let ((table (copy-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?\\ " " table)
    table)
  "`python-mode-syntax-table' with backslash given whitespace syntax.")

;; have to bind py-file-queue before installing the kill-emacs-hook

(define-abbrev-table 'python-mode-abbrev-table ())

(define-abbrev-table 'inferior-python-mode-abbrev-table ())

;; pdbtrack constants
(defconst py-pdbtrack-stack-entry-regexp
  (concat ".*\\("py-shell-input-prompt-1-regexp">\\|>\\) *\\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>()]+\\)()")
  "Regular expression pdbtrack uses to find a stack trace entry.")

;; ipython.el
;; Recognize the ipython pdb, whose prompt is 'ipdb>' or  'ipydb>'
;;instead of '(Pdb)'
(setq py-pdbtrack-input-prompt "^[(<]*[Ii]?[Pp]y?db[>)]+ ")
(setq py-pydbtrack-input-prompt "^[(]*ipydb[>)]+ ")

;; pydb-328837.diff
;; (defconst py-pydbtrack-stack-entry-regexp
;;   "^(\\([-a-zA-Z0-9_/.]*\\):\\([0-9]+\\)):[ \t]?\\(.*\n\\)"
;;   "Regular expression pdbtrack uses to find a stack trace entry for pydb.
;;
;; The debugger outputs program-location lines that look like this:
;;    (/usr/bin/zonetab2pot.py:15): makePOT")

(defconst py-pdbtrack-marker-regexp-file-group 2
  "Group position in gud-pydb-marker-regexp that matches the file name.")

(defconst py-pdbtrack-marker-regexp-line-group 3
  "Group position in gud-pydb-marker-regexp that matches the line number.")

(defconst py-pdbtrack-marker-regexp-funcname-group 4
  "Group position in gud-pydb-marker-regexp that matches the function name.")

(defconst py-pdbtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")


;; Bindings
(defconst python-compilation-regexp-alist
  ;; FIXME: maybe these should move to compilation-error-regexp-alist-alist.
  ;;   The first already is (for CAML), but the second isn't.  Anyhow,
  ;;   these are specific to the inferior buffer.  -- fx
  `((,(rx line-start (1+ (any " \t")) "File \""
          (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
          "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
          (group (1+ digit)))
     1 2)
    ;; pdb stack trace
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
          "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "`compilation-error-regexp-alist' for inferior Python.")

(defconst py-font-lock-syntactic-keywords
  ;; Make outer chars of matching triple-quote sequences into generic
  ;; string delimiters.  Fixme: Is there a better way?
  ;; First avoid a sequence preceded by an odd number of backslashes.
  `((,(concat "\\(?:^\\|[^\\]\\(?:\\\\.\\)*\\)" ;Prefix.
              "\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\(?4:\"\\)\\(?5:\"\\)\\(?6:\"\\)\\|\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)\\(?4:'\\)\\(?5:'\\)\\(?6:'\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)\\(?4:'\\)\\(?5:'\\)\\(?6:'\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)")
     (1 (python-quote-syntax 1) t t)
     (2 (python-quote-syntax 2) t t)
     (3 (python-quote-syntax 3) t t)
     (6 (python-quote-syntax 1) t t))))

(defun python-quote-syntax (n)
  "Put `syntax-table' property correctly on triple quote.
Used for syntactic keywords.  N is the match number (1, 2 or 3)."
  ;; Given a triple quote, we have to check the context to know
  ;; whether this is an opening or closing triple or whether it's
  ;; quoted anyhow, and should be ignored.  (For that we need to do
  ;; the same job as `syntax-ppss' to be correct and it seems to be OK
  ;; to use it here despite initial worries.)  We also have to sort
  ;; out a possible prefix -- well, we don't _have_ to, but I think it
  ;; should be treated as part of the string.

  ;; Test cases:
  ;;  ur"""ar""" x='"' # """
  ;; x = ''' """ ' a
  ;; '''
  ;; x '"""' x """ \"""" x
  (save-excursion
    (goto-char (match-beginning 0))
    (cond
     ;; Consider property for the last char if in a fenced string.
     ((= n 3)
      (let* ((font-lock-syntactic-keywords nil)
	     (syntax (syntax-ppss)))
	(when (eq t (nth 3 syntax))	; after unclosed fence
	  (goto-char (nth 8 syntax))	; fence position
	  ;; (skip-chars-forward "uUrR")	; skip any prefix
	  ;; Is it a matching sequence?
	  (if (eq (char-after) (char-after (match-beginning 2)))
	      (eval-when-compile (string-to-syntax "|"))))))
     ;; Consider property for initial char, accounting for prefixes.
     ((or (and (= n 2)			; leading quote (not prefix)
	       (not (match-end 1)))     ; prefix is null
	  (and (= n 1)			; prefix
	       (match-end 1)))          ; non-empty
      (let ((font-lock-syntactic-keywords nil))
	(unless (eq 'string (syntax-ppss-context (syntax-ppss)))
	  (eval-when-compile (string-to-syntax "|")))))
     ;; Otherwise (we're in a non-matching string) the property is
     ;; nil, which is OK.
     )))


;; Keymap and syntax
;; used by py-completion-at-point, the way of python.el
(defun py-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

  bol -- beginning of line
  eol -- end of line
  bod -- beginning of def or class
  eod -- end of def or class
  bob -- beginning of buffer
  eob -- end of buffer
  boi -- back to indentation
  bos -- beginning of statement

This function does not modify point or mark."
  (let (erg)
    (save-excursion
      (setq erg
            (progn
              (cond
               ((eq position 'bol) (beginning-of-line))
               ((eq position 'eol) (end-of-line))
               ((eq position 'bod) (py-beginning-of-def-or-class))
               ((eq position 'eod) (py-end-of-def-or-class))
               ;; Kind of funny, I know, but useful for py-up-exception.
               ((eq position 'bob) (goto-char (point-min)))
               ((eq position 'eob) (goto-char (point-max)))
               ((eq position 'boi) (back-to-indentation))
               ((eq position 'bos) (py-beginning-of-statement))
               (t (error "Unknown buffer position requested: %s" position))) (point))))
    erg))


;; Font-lock and syntax
(setq py-font-lock-keywords
      ;; Keywords
      `(,(rx symbol-start
             (or "and" "del" "from" "not" "while" "as" "elif" "global" "or" "with"
                 "assert" "else" "if" "pass" "yield" "break" "import"
                 "print" "exec" "in" "continue" "finally" "is" "except" "raise"
                 "return" "def" "for" "lambda" "try")
             symbol-end)
        ;; functions
        (,(rx symbol-start "def" (1+ space) (group (1+ (or word ?_))))
         (1 font-lock-function-name-face))
        ;; classes
        (,(rx symbol-start (group "class") (1+ space) (group (1+ (or word ?_))))
         (1 font-lock-keyword-face) (2 py-class-name-face))
        ;; (,(rx symbol-start
        ;; (or "raise" "except")
        ;; symbol-end) . py-exception-name-face)
        ;; already pseudo-keyword
        ;; (,(rx symbol-start
        ;;       (or "None" "True" "False" "__debug__" "NotImplemented")
        ;;       symbol-end) . font-lock-constant-face)
        (,(rx symbol-start
              (or "cls" "self" "cls" "Ellipsis" "True" "False" "None"  "__debug__" "NotImplemented")
              symbol-end) . py-pseudo-keyword-face)
        ;; Decorators.
        (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_))
                                                (0+ "." (1+ (or word ?_)))))
         (1 py-decorators-face))
        ;; Builtin Exceptions
        (,(rx word-start
              (or "ArithmeticError" "AssertionError" "AttributeError"
                  "BaseException" "BufferError" "BytesWarning" "DeprecationWarning"
                  "EOFError" "EnvironmentError" "Exception" "FloatingPointError"
                  "FutureWarning" "GeneratorExit" "IOError" "ImportError"
                  "ImportWarning" "IndentationError" "IndexError" "KeyError"
                  "KeyboardInterrupt" "LookupError" "MemoryError" "NameError" "NoResultFound"
                  "NotImplementedError" "OSError" "OverflowError"
                  "PendingDeprecationWarning" "ReferenceError" "RuntimeError"
                  "RuntimeWarning" "StandardError" "StopIteration" "SyntaxError"
                  "SyntaxWarning" "SystemError" "SystemExit" "TabError" "TypeError"
                  "UnboundLocalError" "UnicodeDecodeError" "UnicodeEncodeError"
                  "UnicodeError" "UnicodeTranslateError" "UnicodeWarning"
                  "UserWarning" "ValueError" "Warning" "ZeroDivisionError")
              word-end) . py-exception-name-face)
        ;; (,(rx (or space line-start) symbol-start "range
        ;; Builtins
        (,(rx (or space line-start) symbol-start
              (or "_" "__doc__" "__import__" "__name__" "__package__" "abs" "all"
                  "any" "apply" "basestring" "bin" "bool" "buffer" "bytearray"
                  "bytes" "callable" "chr" "classmethod" "cmp" "coerce" "compile"
                  "complex" "delattr" "dict" "dir" "divmod" "enumerate" "eval"
                  "execfile" "file" "filter" "float" "format" "frozenset"
                  "getattr" "globals" "hasattr" "hash" "help" "hex" "id" "input"
                  "int" "intern" "isinstance" "issubclass" "iter" "len" "list"
                  "locals" "long" "map" "max" "min" "next" "object" "oct" "open"
                  "ord" "pow" "print" "property" "range" "raw_input" "reduce"
                  "reload" "repr" "reversed" "round" "set" "setattr" "slice"
                  "sorted" "staticmethod" "str" "sum" "super" "tuple" "type"
                  "unichr" "unicode" "vars" "xrange" "zip")
              symbol-end) . py-builtins-face)
        ;; (,(python-rx line-start (* (any " \t"))(group (** 0 2 "_") word (0+ (or word ?_))(** 0 2 "_"))(* (any " \t")) assignment-operator)
        ;; 1 py-variable-name-face)
        ;; asignations
        ;; support for a = b = c = 5
        ("\\([._[:word:]]+\\)\\(?:\\[[^]]+]\\)?[[:space:]]*\\(?:\\(?:\\*\\*\\|//\\|<<\\|>>\\|[%&*+/|^-]\\)?=\\)"
         (1 py-variable-name-face nil nil))
        ;; a, b, c = (1, 2, 3)
        (,(lambda (limit)
            (let ((re (python-rx (group (+ (any word ?. ?_))) (* space)
                                 (* ?, (* space) (+ (any word ?. ?_)) (* space))
                                 ?, (* space) (+ (any word ?. ?_)) (* space)
                                 assignment-operator))
                  (res nil))
              (while (and (setq res (re-search-forward re limit t))
                          (goto-char (match-end 1))
                          (nth 1 (syntax-ppss))
                          ;; (python-syntax-context 'paren)
                          ))
              res))
         (1 py-variable-name-face nil nil))
        ;; Numbers
        (,(rx symbol-start (or (1+ digit) (1+ hex-digit)) symbol-end) . py-number-face)))

(defconst py-font-lock-syntactic-keywords
  '(("[^\\]\\\\\\(?:\\\\\\\\\\)*\\(\\s\"\\)\\1\\(\\1\\)"
     (2
      (7)))
    ("\\([RUBrub]?\\)[Rr]?\\(\\s\"\\)\\2\\(\\2\\)"
     (1
      (py-quote-syntax 1))
     (2
      (py-quote-syntax 2))
     (3
      (py-quote-syntax 3)))))

(defun py-quote-syntax (n)
  "Put `syntax-table' property correctly on triple quote.
Used for syntactic keywords.  N is the match number (1, 2 or 3)."
  ;; Given a triple quote, we have to check the context to know
  ;; whether this is an opening or closing triple or whether it's
  ;; quoted anyhow, and should be ignored.  (For that we need to do
  ;; the same job as `syntax-ppss' to be correct and it seems to be OK
  ;; to use it here despite initial worries.) We also have to sort
  ;; out a possible prefix -- well, we don't _have_ to, but I think it
  ;; should be treated as part of the string.
  ;; Test cases:
  ;;  ur"""ar""" x='"' # """
  ;; x = ''' """ ' a
  ;; '''
  ;; x '"""' x """ \"""" x
  (save-excursion
    (goto-char (match-beginning 0))
    (cond
     ;; Consider property for the last char if in a fenced string.
     ((= n 3)
      (let* ((font-lock-syntactic-keywords nil)
             (syntax (if (featurep 'xemacs)
                         (parse-partial-sexp (point-min) (point))
                       (syntax-ppss))))
        (when (eq t (nth 3 syntax))     ; after unclosed fence
          (goto-char (nth 8 syntax))    ; fence position
          (skip-chars-forward "uUrRbB") ; skip any prefix
          ;; Is it a matching sequence?
          (if (eq (char-after) (char-after (match-beginning 2)))
              (eval-when-compile (string-to-syntax "|"))))))
     ;; Consider property for initial char, accounting for prefixes.
     ((or (and (= n 2)                  ; leading quote (not prefix)
               (= (match-beginning 1) (match-end 1))) ; prefix is null
          (and (= n 1)                  ; prefix
               (/= (match-beginning 1) (match-end 1)))) ; non-empty
      (let ((font-lock-syntactic-keywords nil))
        (unless (eq 'string (syntax-ppss-context (if (featurep 'xemacs)
                                                     (parse-partial-sexp (point-min) (point))
                                                   (syntax-ppss))))
          ;; (eval-when-compile (string-to-syntax "|"))
          (eval-when-compile (string-to-syntax "|")))))
     ;; Otherwise (we're in a non-matching string) the property is
     ;; nil, which is OK.
     )))

;; An auxiliary syntax table which places underscore and dot in the
;; symbol class for simplicity

;; credits to python.el
(defun py-beg-of-defun-function ()
  (set (make-local-variable 'beginning-of-defun-function)
       'py-beginning-of-def-or-class))

(defun py-end-of-defun-function ()
  (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class))

(make-obsolete-variable 'jpython-mode-hook 'jython-mode-hook nil)

;; In previous version of python-mode.el, the hook was incorrectly
;; called py-mode-hook, and was not defvar'd.  Deprecate its use.
(and (fboundp 'make-obsolete-variable)
     (make-obsolete-variable 'py-mode-hook 'python-mode-hook nil))

(defun py-docstring-p (&optional beginning-of-string-position)
  "Check to see if there is a docstring at POS."
  (let* (pps
         (pos (or beginning-of-string-position
                  (and (nth 3 (setq pps (syntax-ppss))) (nth 8 pps)))))
    (save-restriction
      (widen)
      (save-excursion
        (and pos (goto-char pos))
        (if (looking-at-p "'''\\|\"\"\"")
            (progn
              (py-beginning-of-statement)
              (or (bobp)
                  (py-beginning-of-def-or-class-p)))
          nil)))))

(defun py-font-lock-syntactic-face-function (state)
  (if (nth 3 state)
      (if (py-docstring-p (nth 8 state))
          font-lock-doc-face
        font-lock-string-face)
    font-lock-comment-face))

(defun py-insert-default-shebang ()
  "Insert in buffer shebang of installed default Python. "
  (interactive "*")
  (let* ((erg (if py-edit-only-p
                  py-shell-name
                (executable-find py-shell-name)))
         (sheb (concat "#! " erg)))
    (insert sheb)))

(defun py-electric-comment (arg)
  "Insert a comment. If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many \"#\" are inserted
non-electrically.
With \\[universal-argument] \"#\" electric behavior is inhibited inside a string or comment."
  (interactive "*P")
  (if (and py-indent-comments py-electric-comment-p)
      (if (ignore-errors (eq 4 (car-safe arg)))
          (insert "#")
        (when (and (eq last-command 'py-electric-comment) (looking-back " "))
          (forward-char -1))
        (if (interactive-p) (self-insert-command (prefix-numeric-value arg))
          (insert "#"))
        (let ((orig (copy-marker (point)))
              (indent (py-compute-indentation)))
          (unless
              ;; (or
              (eq (current-indentation) indent)
            ;; (looking-back "#[ \t]*"))
            (goto-char orig)
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-to indent)
            (goto-char orig))
          (when py-electric-comment-add-space-p
            (unless (looking-at "[ \t]")
              (insert " "))))
        (setq last-command this-command))
    (self-insert-command (prefix-numeric-value arg))))

(defun py-electric-colon (arg)
  "Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix C-u.

Switched by `py-electric-colon-active-p', default is nil
See also `py-electric-colon-greedy-p' "
  (interactive "*P")
  (cond ((not py-electric-colon-active-p)
         (self-insert-command (prefix-numeric-value arg)))
        ((and py-electric-colon-bobl-only (save-excursion (py-beginning-of-statement) (not (py-beginning-of-block-p))))
         (self-insert-command (prefix-numeric-value arg)))
        ((eq 4 (prefix-numeric-value arg))
         (self-insert-command 1))
        (t (insert ":")
           (unless (py-in-string-or-comment-p)
             (let ((orig (copy-marker (point)))
                   (indent (py-compute-indentation)))
               (unless (or (eq (current-indentation) indent)
                           (and (not py-electric-colon-greedy-p)
                                (eq (current-indentation)(save-excursion (beginning-of-line) (py-beginning-of-block)(current-indentation))))
                           (and (py-top-level-form-p)(< (current-indentation) indent)))
                 (beginning-of-line)
                 (delete-horizontal-space)
                 (indent-to indent))
               (goto-char orig))
             (when py-electric-colon-newline-and-indent-p
               (py-newline-and-indent))))))

(defun py-top-level-form-p ()
  "Return non-nil, if line starts with a top level definition.

Used by `py-electric-colon', which will not indent than. "
  (let (erg)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq erg (looking-at py-extended-block-or-clause-re)
            ;; (or (looking-at py-class-re)
            ;; (looking-at py-def-re))
            ))
    erg))


;; Electric deletion
(defun py-empty-out-list-backward ()
  "Deletes all elements from list before point. "
  (interactive "*")
  (and (member (char-before) (list ?\) ?\] ?\}))
       (let ((orig (point))
             (thischar (char-before))
             pps cn)
         (forward-char -1)
         (setq pps (syntax-ppss))
         (if (and (not (nth 8 pps)) (nth 1 pps))
             (progn
               (goto-char (nth 1 pps))
               (forward-char 1))
           (cond ((or (eq thischar 41)(eq thischar ?\)))
                  (setq cn "("))
                 ((or (eq thischar 125) (eq thischar ?\}))
                  (setq cn "{"))
                 ((or (eq thischar 93)(eq thischar ?\]))
                  (setq cn "[")))
           (skip-chars-backward (concat "^" cn)))
         (delete-region (point) orig)
         (insert-char thischar 1)
         (forward-char -1))))

(defun py-electric-backspace (&optional arg)
  "Delete preceding character or level of indentation.

With ARG do that ARG times.
Returns column reached. "
  (interactive "*p")
  (let ((arg (or arg 1))
        erg)
    (dotimes (i arg)
      (cond ((looking-back "^[ \t]+")
             (let* ((remains (% (current-column) py-indent-offset)))
               (if (< 0 remains)
                   (delete-char (- remains))
                 (indent-line-to (- (current-indentation) py-indent-offset)))))
            ((and py-electric-kill-backward-p (member (char-before) (list ?\) ?\] ?\})))
             (py-empty-out-list-backward))
            (t (delete-char (- 1)))))
    (setq erg (current-column))
    (when (and (interactive-p) py-verbose-p) (message "%s" erg))
    erg))

(defun py-electric-delete (&optional arg)
  "Delete following character or levels of whitespace.

With ARG do that ARG times. "
  (interactive "*p")
  (let ((arg (or arg 1)))
    (dotimes (i arg)
      (if (and (or (bolp)(looking-back "^[ \t]+")) (looking-at "[ \t]+"))
          (let* ((remains (% (+ (current-column) (- (match-end 0)(match-beginning 0))) py-indent-offset)))
            (if (< 0 remains)
                (delete-char remains)
              (delete-char py-indent-offset)))
        (delete-char 1)))))

;; (defun py-electric-delete (arg)
;;   "Delete preceding or following character or levels of whitespace.
;;
;; The behavior of this function depends on the variable
;; `delete-key-deletes-forward'.  If this variable is nil (or does not
;; exist, as in older Emacsen and non-XEmacs versions), then this
;; function behaves identically to \\[c-electric-backspace].
;;
;; If `delete-key-deletes-forward' is non-nil and is supported in your
;; Emacs, then deletion occurs in the forward direction, by calling the
;; function in `py-delete-function'.
;;
;; \\[universal-argument] (programmatically, argument ARG) specifies the
;; number of characters to delete (default is 1)."
;;   (interactive "*p")
;;   (if (or (and (fboundp 'delete-forward-p) ;XEmacs 21
;;                (delete-forward-p))
;;           (and (boundp 'delete-key-deletes-forward) ;XEmacs 20
;;                delete-key-deletes-forward))
;;       (funcall py-delete-function arg)
;;     (py-electric-backspace arg)))

;; required for pending-del and delsel modes
(put 'py-electric-colon 'delete-selection t) ;delsel
(put 'py-electric-colon 'pending-delete t) ;pending-del
(put 'py-electric-backspace 'delete-selection 'supersede) ;delsel
(put 'py-electric-backspace 'pending-delete 'supersede) ;pending-del
(put 'py-electric-delete 'delete-selection 'supersede) ;delsel
(put 'py-electric-delete 'pending-delete 'supersede) ;pending-del


(defun py-indent-line-outmost (&optional arg)
  "Indent the current line to the outmost reasonable indent.

With optional \\[universal-argument] an indent with length `py-indent-offset' is inserted unconditionally "
  (interactive "*P")
  (let* ((need (py-compute-indentation (point)))
         (cui (current-indentation))
         (cuc (current-column)))
    (cond ((eq 4 (prefix-numeric-value arg))
	   (if indent-tabs-mode
	       (insert (make-string 1 9))
	     (insert (make-string py-indent-offset 32))))
          (t
           (if (and (eq need cui)(not (eq cuc cui)))
               (back-to-indentation)
             (beginning-of-line)
             (delete-horizontal-space)
             (indent-to need))))))

(defun py-indent-fix-region-intern (beg end)
  "Used when `py-tab-indents-region-p' is non-nil. "
  (let (indent)
    (save-excursion
      (save-restriction
        (beginning-of-line)
        (narrow-to-region beg end)
        (forward-line 1)
        (narrow-to-region (line-beginning-position) end)
        (beginning-of-line)
        (delete-region (point) (progn (skip-chars-forward " \t\r\n\f") (point)))
        (indent-to (py-compute-indentation))
        (while
            (< (line-end-position) end)
          (forward-line 1)
          (beginning-of-line)
          (delete-region (point) (progn (skip-chars-forward " \t\r\n\f") (point)))
          (indent-to (py-compute-indentation))))))
  ;; (exchange-point-and-mark)
  )

(defun py-indent-line-intern (need cui py-indent-offset col &optional beg end region)
  (if py-tab-indent
      (progn
        (and py-tab-indents-region-p region
             (py-indent-fix-region-intern beg end))
        (cond ((eq need cui)
               (if (or (eq this-command last-command)
                       ;; (eq this-command 'exchange-point-and-mark)
                       (eq this-command 'py-indent-line))
                   (if (and py-tab-shifts-region-p region)
                       (while (save-excursion (goto-char beg) (< 0 (current-indentation)))
                         (py-shift-region-left 1 beg end))
                     (beginning-of-line)
                     (delete-horizontal-space)
                     (if (<= (line-beginning-position) (+ (point) (- col cui)))
                         (forward-char (- col cui))
                       (beginning-of-line)))))
              ((< cui need)
               ;; (if (eq this-command last-command)
               (if (and py-tab-shifts-region-p region)
                   (progn
                     (py-shift-region-right 1))
                 (progn
                   (beginning-of-line)
                   (delete-horizontal-space)
                   (indent-to (+ (* (/ cui py-indent-offset) py-indent-offset) py-indent-offset))
                   (forward-char (- col cui)))))
              ;; (if (and py-tab-shifts-region-p region)
              ;;     (while (< (current-indentation) need)
              ;;       (py-shift-region-right 1))
              ;;   (beginning-of-line)
              ;;   (delete-horizontal-space)
              ;;   (indent-to need)
              ;;   (forward-char (- col cui)))))
              ((< need cui)
               (if (and py-tab-shifts-region-p region)
                   (progn
                     (when (eq (point) (region-end))
                       (exchange-point-and-mark))
                     (while (< 0 (current-indentation))
                       (py-shift-region-left 1)))
                 (beginning-of-line)
                 (delete-horizontal-space)))
              (t
               (if (and py-tab-shifts-region-p region)
                   (progn
                     ;; (when (eq (point) (region-end))
                     ;; (exchange-point-and-mark))
                     (while (< (current-indentation) need)
                       (py-shift-region-right 1)))
                 (beginning-of-line)
                 (delete-horizontal-space)
                 (indent-to need)
                 (back-to-indentation)
                 (if (<= (line-beginning-position) (+ (point) (- col cui)))
                     (forward-char (- col cui))
                   (beginning-of-line))))))
    (insert-tab)))

(defun py-indent-line (&optional arg)
  "Indent the current line according to Python rules.

When called interactivly with \\[universal-argument], ignore dedenting rules for block closing statements
\(e.g. return, raise, break, continue, pass)

An optional \\[universal-argument] followed by a numeric argument neither 1 nor 4 will switch off `py-smart-indentation' for this execution. This permits to correct allowed but unwanted indents.
Similar to `toggle-py-smart-indentation' resp. `py-smart-indentation-off' followed by TAB.

This function is normally used by `indent-line-function' resp.
\\[indent-for-tab-command].
Returns current indentation

When `py-tab-shifts-region-p' is `t', not just the current line,
but the region is shiftet that way.

If `py-tab-indents-region-p' is `t' and first TAB doesn't shift
--as indent is at outmost reasonable--, indent-region is called. "
  (interactive "P")
  (if (interactive-p)
      ;; TAB-leaves-point-in-the-wrong-lp-1178453-test
      ;; (save-excursion
      (let ((orig (copy-marker (point)))
	    (region (use-region-p))
	    beg end)
	(and region (setq beg (region-beginning))
	     (setq end (region-end)))
	(let ((cui (current-indentation))
	      (col (current-column))
	      (this-indent-offset (cond ((and py-smart-indentation (not (eq this-command last-command)))
					 (py-guess-indent-offset))
					((and py-smart-indentation (eq this-command last-command) py-already-guessed-indent-offset)
					 py-already-guessed-indent-offset)
					(t (default-value 'py-indent-offset))))
	      (need (if (and (eq this-command last-command) py-already-guessed-indent-offset)
			(if region
			    (save-excursion
			      ;; if previous command was an indent
			      ;; already, position reached might
			      ;; produce false guesses
			      (goto-char beg) (py-compute-indentation beg nil nil nil nil nil py-already-guessed-indent-offset))
			  (py-compute-indentation beg nil nil nil nil nil py-already-guessed-indent-offset))
		      (if region
			  (save-excursion
			    (goto-char beg)
			    (save-excursion (goto-char beg) (py-compute-indentation)))
			(py-compute-indentation)))))
	  (unless (eq this-command last-command)
	    (setq py-already-guessed-indent-offset this-indent-offset))
	  (cond ((eq 4 (prefix-numeric-value arg))
		 (beginning-of-line)
		 (delete-horizontal-space)
		 (indent-to (+ need py-indent-offset)))
		((not (eq 1 (prefix-numeric-value arg)))
		 (py-smart-indentation-off)
		 (py-indent-line-intern need cui this-indent-offset col beg end region))
		(t (py-indent-line-intern need cui this-indent-offset col beg end region)))
	  (when (and (interactive-p) py-verbose-p)(message "%s" (current-indentation)))
	  (current-indentation))
	(goto-char orig)
	(if region
	    (progn
	      (or py-tab-shifts-region-p
		  py-tab-indents-region-p)
	      (eq (point) end)
	      (not (eq (point) orig))
	      (exchange-point-and-mark))
	  (and (< (current-column) (current-indentation))(back-to-indentation))))
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to (py-compute-indentation))))

(defun py-newline-and-indent ()
  "Add a newline and indent to outmost reasonable indent.
When indent is set back manually, this is honoured in following lines. "
  (interactive "*")
  (let ((orig (point))
        erg pos)
    (newline)
    (when (or py-newline-delete-trailing-whitespace-p py-trailing-whitespace-smart-delete-p)
      (setq pos (copy-marker (point)))
      (save-excursion
        (goto-char orig)
        (if (empty-line-p)
            (if (string-match "23.4" emacs-version)
                (progn (save-restriction
                         (narrow-to-region (point) pos)
                         (delete-trailing-whitespace)))
              (delete-trailing-whitespace (line-beginning-position) pos))
          (skip-chars-backward " \t")
          (if (string-match "23.4" emacs-version)
              (progn (save-restriction
                       (narrow-to-region (point) pos)
                       (delete-trailing-whitespace)))
            (delete-trailing-whitespace (point) (marker-position pos))))))
    (setq erg (indent-to-column (py-compute-indentation)))
    (when (and (interactive-p) py-verbose-p) (message "%s" erg))
    erg))

(defalias 'py-newline-and-close-block 'py-newline-and-dedent)
(defun py-newline-and-dedent ()
  "Add a newline and indent to one level below current.
Returns column. "
  (interactive "*")
  (let ((cui (current-indentation))
        erg)
    (newline)
    (when (< 0 cui)
      (setq erg (- (py-compute-indentation) py-indent-offset))
      (indent-to-column erg))
    (when (and (interactive-p) py-verbose-p) (message "%s" erg))
    erg))

(defun py-indent-tabs-mode (arg &optional iact)
  "With positive ARG switch `indent-tabs-mode' on.

With negative ARG switch `indent-tabs-mode' off.
Returns value of `indent-tabs-mode' switched to. "
  (interactive "p")
  (if (< 0 arg)
      (progn
        (setq indent-tabs-mode t)
        (setq tab-width py-indent-offset))
    (setq indent-tabs-mode nil))
  (when (and py-verbose-p (or iact (interactive-p))) (message "indent-tabs-mode %s   py-indent-offset %s" indent-tabs-mode py-indent-offset))
  indent-tabs-mode)

;; Guess indent offset
(defun py-guessed-sanity-check (guessed)
  (and (>= guessed 2)(<= guessed 8)(eq 0 (% guessed 2))))

(defun py-guess-indent-final (indents orig)
  "Calculate and do sanity-check. "
  (let* ((first (car indents))
         (second (cadr indents))
         (erg (if (and first second)
                  (if (< second first)
                      ;; (< (point) orig)
                      (- first second)
                    (- second first))
                (default-value 'py-indent-offset))))
    (setq erg (and (py-guessed-sanity-check erg) erg))
    erg))

(defun py-guess-indent-forward ()
  "Called when moving to end of a form and `py-smart-indentation' is on. "
  (interactive)
  (let* ((first (if
                    (py-beginning-of-statement-p)
                    (current-indentation)
                  (progn
                    (py-end-of-statement)
                    (py-beginning-of-statement)
                    (current-indentation))))
         (second (if (or (looking-at py-extended-block-or-clause-re)(eq 0 first))
                     (progn
                       (py-end-of-statement)
                       (py-end-of-statement)
                       (py-beginning-of-statement)
                       (current-indentation))
                   ;; when not starting from block, look above
                   (while (and (re-search-backward py-extended-block-or-clause-re nil 'movet 1)
                               (or (>= (current-indentation) first)
                                   (nth 8 (syntax-ppss)))))
                   (current-indentation))))
    (list first second)))

(defun py-guess-indent-backward ()
  "Called when moving to beginning of a form and `py-smart-indentation' is on. "
  (let* ((cui (current-indentation))
         (indent (if (< 0 cui) cui 999))
         (pos (progn (while (and (re-search-backward py-extended-block-or-clause-re nil 'movet 1)
                                 (or (>= (current-indentation) indent)
                                     (nth 8 (syntax-ppss)))))
                     (unless (bobp) (point))))
         (first (and pos (current-indentation)))
         (second (and pos (py-end-of-statement) (py-end-of-statement) (py-beginning-of-statement)(current-indentation))))
    (list first second)))

(defun py-guess-indent-offset (&optional direction)
  "Guess `py-indent-offset'.

Set local value of `py-indent-offset', return it

Might change local value of `py-indent-offset' only when called
downwards from beginning of block followed by a statement. Otherwise default-value is returned.
"
  (interactive)
  (save-excursion
    (let* ((orig (point))
           (indents
            (cond (direction
                   (if (eq 'forward direction)
                       (py-guess-indent-forward)
                     (py-guess-indent-backward)))
                  ;; guess some usable indent is above current position
                  ((eq 0 (current-indentation))
                   (py-guess-indent-forward))
                  (t (py-guess-indent-backward))))
           (erg (py-guess-indent-final indents orig)))
      (if erg (setq py-indent-offset erg)
        (setq py-indent-offset
              (default-value 'py-indent-offset)))
      (when (interactive-p) (message "%s" py-indent-offset))
      py-indent-offset)))

;;;
(defun py-comment-indent-function ()
  "Python version of `comment-indent-function'."
  ;; This is required when filladapt is turned off.  Without it, when
  ;; filladapt is not used, comments which start in column zero
  ;; cascade one character to the right
  (save-excursion
    (beginning-of-line)
    (let ((eol (line-end-position)))
      (and comment-start-skip
           (re-search-forward comment-start-skip eol t)
           (setq eol (match-beginning 0)))
      (goto-char eol)
      (skip-chars-backward " \t")
      (max comment-column (+ (current-column) (if (bolp) 0 1))))))

(defun py-narrow-to-defun ()
  "Make text outside current def or class invisible.

The defun visible is the one that contains point or follows point. "
  (interactive)
  (save-excursion
    (let ((end (py-beginning-of-def-or-class)))
      (py-end-of-def-or-class)
      (narrow-to-region (point) end))))

;; make general form below work also in these cases
(defalias 'py-beginning-of-paragraph 'backward-paragraph)
(defalias 'py-end-of-paragraph 'forward-paragraph)

;;; Shifting

(defalias 'py-shift-region-left 'py-shift-left)
(defun py-shift-left (&optional count start end)
  "Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. "
  (interactive "p")
  (setq count (or count 1))
  (let ((erg (py-shift-intern (- count) start end)))
    (when (and (interactive-p) py-verbose-p) (message "%s" erg))
    erg))

(defalias 'py-shift-region-right 'py-shift-right)
(defun py-shift-right (&optional count beg end)
  "Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. "
  (interactive "p")
  (setq count (or count 1))
  (let ((erg (py-shift-intern count beg end)))
    (when (and (interactive-p) py-verbose-p) (message "%s" erg))
    erg))

(defun py-shift-intern (count &optional start end)
  (save-excursion
    (let* ((inhibit-point-motion-hooks t)
           deactivate-mark
           (beg (cond (start)
                      ((region-active-p)
                       (save-excursion
                         (goto-char
                          (region-beginning))))
                      (t (line-beginning-position))))
           (end (cond (end)
                      ((region-active-p)
                       (save-excursion
                         (goto-char
                          (region-end))))
                      (t (line-end-position))))
           (orig end))
      (setq beg (copy-marker beg))
      (setq end (copy-marker end))
      (if (< 0 count)
          (indent-rigidly beg end py-indent-offset)
        (indent-rigidly beg end (- py-indent-offset)))
      (push-mark beg t)
      (goto-char end)
      (skip-chars-backward " \t\r\n\f"))
    (py-indentation-of-statement)))

(defun py-shift-forms-base (form arg &optional beg end)
  (let* ((begform (intern-soft (concat "py-beginning-of-" form)))
         (endform (intern-soft (concat "py-end-of-" form)))
         (orig (copy-marker (point)))
         (beg (cond (beg)
                    ((region-active-p)
                     (save-excursion
                       (goto-char (region-beginning))
                       (line-beginning-position)))
                    (t (save-excursion
                         (funcall begform)
                         (line-beginning-position)))))
         (end (cond (end)
                    ((region-active-p)
                     (region-end))
                    (t (funcall endform))))
         (erg (py-shift-intern arg beg end)))
    (goto-char orig)
    erg))

(defun py-shift-paragraph-right (&optional arg)
  "Indent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "paragraph" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-paragraph-left (&optional arg)
  "Dedent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "paragraph" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-block-right (&optional arg)
  "Indent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "block" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-block-left (&optional arg)
  "Dedent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "block" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-clause-right (&optional arg)
  "Indent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "clause" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-clause-left (&optional arg)
  "Dedent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "clause" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-block-or-clause-right (&optional arg)
  "Indent block-or-clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "block-or-clause" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-block-or-clause-left (&optional arg)
  "Dedent block-or-clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "block-or-clause" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-def-right (&optional arg)
  "Indent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "def" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-def-left (&optional arg)
  "Dedent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "def" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-class-right (&optional arg)
  "Indent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "class" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-class-left (&optional arg)
  "Dedent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "class" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-def-or-class-right (&optional arg)
  "Indent def-or-class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "def-or-class" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-def-or-class-left (&optional arg)
  "Dedent def-or-class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "def-or-class" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-line-right (&optional arg)
  "Indent line by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "line" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-line-left (&optional arg)
  "Dedent line by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "line" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-statement-right (&optional arg)
  "Indent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "statement" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-statement-left (&optional arg)
  "Dedent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "statement" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

;;
(defun py-indent-and-forward ()
  "Indent current line according to mode, move one line forward. "
  (interactive "*")
  (beginning-of-line)
  (fixup-whitespace)
  (indent-to (py-compute-indentation))
  (if (eobp)
      (newline-and-indent)
    (forward-line 1))
  (back-to-indentation))

(defun py-indent-region (start end &optional indent-offset recursive)
  "Reindent a region of Python code.

With optional INDENT-OFFSET specify a different value than `py-indent-offset' at place.

Guesses the outmost reasonable indent
Returns and keeps relative position "
  (interactive "*r\nP")
  (let ((orig (copy-marker (point)))
        (beg start)
        (end (copy-marker end))
        (py-indent-offset (prefix-numeric-value
                           (or indent-offset py-indent-offset))))
    (goto-char beg)
    (while (< (line-end-position) end)
      (if (empty-line-p)
          (forward-line 1)
        (py-indent-and-forward)))
    (unless (empty-line-p) (py-indent-line))
    (goto-char orig)))

;;; Positions

(defun py-def-or-class-beginning-position ()
  "Returns beginning position of function or class definition. "
  (interactive)
  (let ((here (point))
        (pos (progn (py-beginning-of-def-or-class)(point))))
    (prog1
        (point)
      (when (and py-verbose-p (interactive-p)) (message "%s" pos))
      (goto-char here))))

(defun py-def-or-class-end-position ()
  "Returns end position of function or class definition. "
  (interactive)
  (let ((here (point))
        (pos (progn (py-end-of-def-or-class) (point))))
    (prog1
        (point)
      (when (and py-verbose-p (interactive-p)) (message "%s" pos))
      (goto-char here))))

(defun py-statement-beginning-position ()
  "Returns beginning position of statement. "
  (interactive)
  (let ((here (point))
        (pos (progn (py-beginning-of-statement)(point))))
    (prog1
        (point)
      (when (and py-verbose-p (interactive-p)) (message "%s" pos))
      (goto-char here))))

(defun py-statement-end-position ()
  "Returns end position of statement. "
  (interactive)
  (let (erg)
    (save-excursion
      (setq erg (py-end-of-statement)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-current-indentation ()
  "Returns beginning position of code in line. "
  (interactive)
  (let ((here (point))
        (pos (progn (back-to-indentation)(point))))
    (prog1
        (point)
      (when (and py-verbose-p (interactive-p)) (message "%s" pos))
      (goto-char here))))

(defun py-beginning-of-paragraph-position ()
  "Returns beginning of paragraph position. "
  (interactive)
  (save-excursion
    (let ((erg (progn (py-beginning-of-paragraph) (point))))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-paragraph-position ()
  "Returns end of paragraph position. "
  (interactive)
  (save-excursion
    (let ((erg (progn (py-end-of-paragraph) (point))))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-block-position ()
  "Returns beginning of block position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-block)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-block-position ()
  "Returns end of block position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-block)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning-of-clause-position ()
  "Returns beginning of clause position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-clause)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-clause-position ()
  "Returns end of clause position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-clause)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning-of-block-or-clause-position ()
  "Returns beginning of block-or-clause position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-block-or-clause)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-block-or-clause-position ()
  "Returns end of block-or-clause position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-block-or-clause)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning-of-def-position ()
  "Returns beginning of def position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-def)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-def-position ()
  "Returns end of def position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-def)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning-of-class-position ()
  "Returns beginning of class position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-class)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-class-position ()
  "Returns end of class position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-class)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning-of-def-or-class-position ()
  "Returns beginning of def-or-class position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-def-or-class)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-def-or-class-position ()
  "Returns end of def-or-class position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-def-or-class)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning-of-line-position ()
  "Returns beginning of line position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-line)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-line-position ()
  "Returns end of line position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-line)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning-of-statement-position ()
  "Returns beginning of statement position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-statement)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-statement-position ()
  "Returns end of statement position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-statement)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning-of-expression-position ()
  "Returns beginning of expression position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-expression)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-expression-position ()
  "Returns end of expression position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-expression)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning-of-partial-expression-position ()
  "Returns beginning of partial-expression position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-partial-expression)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-partial-expression-position ()
  "Returns end of partial-expression position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-partial-expression)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defalias 'py-in-list-p 'py-list-beginning-position)
(defun py-list-beginning-position (&optional start)
  "Return lists beginning position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'."
  (interactive)
  (let* ((ppstart (or start (point-min)))
         (erg (nth 1 (syntax-ppss))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-end-of-list-position (&optional arg)
  "Return end position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'."
  (interactive)
  (let* ((ppstart (or arg (point-min)))
         (erg (syntax-ppss))
         (beg (nth 1 erg))
         end)
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-list 1)
        (setq end (point))))
    (when (and py-verbose-p (interactive-p)) (message "%s" end))
    end))

(defun py-in-comment-p ()
  "Return the beginning of current line's comment, if inside. "
  (save-restriction
    (widen)
    (let* ((pps (syntax-ppss))
           (erg (when (nth 4 pps) (nth 8 pps))))
      (unless erg
        (when (looking-at (concat "^[ \t]*" comment-start-skip))
          (setq erg (point))))
      erg)))

(defun py-in-triplequoted-string-p ()
  "Returns character address of start tqs-string, nil if not inside. "
  (interactive)
  (let* ((pps (syntax-ppss))
         (erg (when (and (nth 3 pps) (nth 8 pps))(nth 2 pps))))
    (save-excursion
      (unless erg (setq erg
                        (progn
                          (when (looking-at "\"\"\"\\|''''")
                            (goto-char (match-end 0))
                            (setq pps (syntax-ppss))
                            (when (and (nth 3 pps) (nth 8 pps)) (nth 2 pps)))))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-in-string-p ()
  "Returns character address of start of string, nil if not inside. "
  (interactive)
  (let* ((pps (syntax-ppss))
         (erg (when (nth 3 pps) (nth 8 pps))))
    (save-excursion
      (unless erg (setq erg
                        (progn
                          (when (looking-at "\"\\|'")
                            (forward-char 1)
                            (setq pps (syntax-ppss))
                            (when (nth 3 pps) (nth 8 pps)))))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-in-statement-p ()
  "Returns list of beginning and end-position if inside.

Result is useful for booleans too: (when (py-in-statement-p)...)
will work.
"
  (interactive)
  (let ((orig (point))
        beg end erg)
    (save-excursion
      (setq end (py-end-of-statement))
      (setq beg (py-beginning-of-statement))
      (when (and (<= beg orig)(<= orig end))
        (setq erg (cons beg end))
        (when (and py-verbose-p (interactive-p)) (message "%s" erg))
        erg))))

;;; Bounds
(defun py-bounds-of-statement (&optional position)
  "Returns bounds of statement at point.

With optional POSITION, a number, report bounds of statement at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-statement-position))
            (end (py-end-of-statement-position)))
        (if (and beg end)
            (when (and py-verbose-p (interactive-p)) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-statements ()
  "Bounds of consecutive multitude of statements around point.

Indented same level, which don't open blocks. "
  (interactive)
  (let* ((orig-indent (progn
                        (back-to-indentation)
                        (unless (py-beginning-of-statement-p)
                          (py-beginning-of-statement))
                        (unless (py-beginning-of-block-p)
                          (current-indentation))))
         (orig (point))
         last beg end)
    (when orig-indent
      (setq beg (point))
      (while (and (setq last beg)
                  (setq beg
                        (when (py-beginning-of-statement)
                          (line-beginning-position)))
                  (not (py-in-string-p))
                  (not (py-beginning-of-block-p))
                  (eq (current-indentation) orig-indent)))
      (setq beg last)
      (goto-char orig)
      (setq end (line-end-position))
      (while (and (setq last (line-end-position))
                  (setq end (py-down-statement))
                  (not (py-beginning-of-block-p))
                  (not (py-in-string-p))
                  (eq (py-indentation-of-statement) orig-indent)))
      (setq end last)
      (goto-char orig)
      (if (and beg end)
          (progn
            (when (and py-verbose-p (interactive-p)) (message "%s %s" beg end))
            (cons beg end))
        (when (and py-verbose-p (interactive-p)) (message "%s" nil))
        nil))))

(defun py-bounds-of-block (&optional position)
  "Returns bounds of block at point.

With optional POSITION, a number, report bounds of block at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-block-position))
            (end (py-end-of-block-position)))
        (if (and beg end)
            (when (and py-verbose-p (interactive-p)) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-clause (&optional position)
  "Returns bounds of clause at point.

With optional POSITION, a number, report bounds of clause at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-clause-position))
            (end (py-end-of-clause-position)))
        (if (and beg end)
            (when (and py-verbose-p (interactive-p)) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-block-or-clause (&optional position)
  "Returns bounds of block-or-clause at point.

With optional POSITION, a number, report bounds of block-or-clause at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-block-or-clause-position))
            (end (py-end-of-block-or-clause-position)))
        (if (and beg end)
            (when (and py-verbose-p (interactive-p)) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-def (&optional position)
  "Returns bounds of def at point.

With optional POSITION, a number, report bounds of def at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-def-position))
            (end (py-end-of-def-position)))
        (if (and beg end)
            (when (and py-verbose-p (interactive-p)) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-class (&optional position)
  "Returns bounds of class at point.

With optional POSITION, a number, report bounds of class at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-class-position))
            (end (py-end-of-class-position)))
        (if (and beg end)
            (when (and py-verbose-p (interactive-p)) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-region ()
  "Returns bounds of region at point.

Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((beg (region-beginning))
            (end (region-end)))
        (if (and beg end)
            (when (and py-verbose-p (interactive-p)) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-beginning-of-buffer-position ()
  (point-min))

(defun py-end-of-buffer-position ()
  (point-max))

(defun py-bounds-of-buffer (&optional position)
  "Returns bounds of buffer at point.

With optional POSITION, a number, report bounds of buffer at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-buffer-position))
            (end (py-end-of-buffer-position)))
        (if (and beg end)
            (when (and py-verbose-p (interactive-p)) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-expression (&optional position)
  "Returns bounds of expression at point.

With optional POSITION, a number, report bounds of expression at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-expression-position))
            (end (py-end-of-expression-position)))
        (if (and beg end)
            (when (and py-verbose-p (interactive-p)) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-partial-expression (&optional position)
  "Returns bounds of partial-expression at point.

With optional POSITION, a number, report bounds of partial-expression at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-partial-expression-position))
            (end (py-end-of-partial-expression-position)))
        (if (and beg end)
            (when (and py-verbose-p (interactive-p)) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-declarations ()
  "Bounds of consecutive multitude of assigments resp. statements around point.

Indented same level, which don't open blocks.
Typically declarations resp. initialisations of variables following
a class or function definition.
See also py-bounds-of-statements "
  (interactive)
  (let* ((orig-indent (progn
                        (back-to-indentation)
                        (unless (py-beginning-of-statement-p)
                          (py-beginning-of-statement))
                        (unless (py-beginning-of-block-p)
                          (current-indentation))))
         (orig (point))
         last beg end)
    (when orig-indent
      (setq beg (line-beginning-position))
      ;; look upward first
      (while (and
              (progn
                (unless (py-beginning-of-statement-p)
                  (py-beginning-of-statement))
                (line-beginning-position))
              (py-beginning-of-statement)
              (not (py-beginning-of-block-p))
              (eq (current-indentation) orig-indent))
        (setq beg (line-beginning-position)))
      (goto-char orig)
      (while (and (setq last (line-end-position))
                  (setq end (py-down-statement))
                  (not (py-beginning-of-block-p))
                  (eq (py-indentation-of-statement) orig-indent)))
      (setq end last)
      (goto-char beg)
      (if (and beg end)
          (progn
            (when (and py-verbose-p (interactive-p)) (message "%s %s" beg end))
            (cons beg end))
        (when (and py-verbose-p (interactive-p)) (message "%s" nil))
        nil))))

;;; Comments, Filling
(defun py-beginning-of-comment ()
  "Go to the beginning of current line's comment, if any.

Returns position if succesful. "
  (interactive)
  (save-restriction
    (widen)
    (let ((pps (syntax-ppss)))
      (when (nth 4 pps)
        (goto-char
         (nth 8 pps))))))

(defun py-end-of-comment ()
  "Go to the end of comment at point.

Returns position, nil if not in comment."

  (interactive)
  (let ((orig (point))
        last)
    (while (and (not (eobp)) (or (looking-at (concat "[ \t]*" comment-start))(nth 4 (syntax-ppss))(empty-line-p)))
      (unless (empty-line-p)
        (setq last (point)))
      (forward-line 1))
    (if last
        (progn
          (goto-char last)
          (end-of-line)
          (skip-chars-backward " \t\r\n\f")
          (point))
      last)))

;;; Comment forms
(defun py-comment-region (beg end &optional arg)
  "Like `comment-region' but uses double hash (`#') comment starter."
  (interactive "r\nP")
  (let ((comment-start (if py-block-comment-prefix-p
                           py-block-comment-prefix
                         comment-start)))
    (comment-region beg end arg)))

(defun py-comment-block (&optional beg end arg)
  "Comments block at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py-beginning-of-block-position)))
          (end (or end (py-end-of-block-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-clause (&optional beg end arg)
  "Comments clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py-beginning-of-clause-position)))
          (end (or end (py-end-of-clause-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-block-or-clause (&optional beg end arg)
  "Comments block-or-clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py-beginning-of-block-or-clause-position)))
          (end (or end (py-end-of-block-or-clause-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-def (&optional beg end arg)
  "Comments def at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py-beginning-of-def-position)))
          (end (or end (py-end-of-def-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-class (&optional beg end arg)
  "Comments class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py-beginning-of-class-position)))
          (end (or end (py-end-of-class-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-def-or-class (&optional beg end arg)
  "Comments def-or-class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py-beginning-of-def-or-class-position)))
          (end (or end (py-end-of-def-or-class-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-statement (&optional beg end arg)
  "Comments statement at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py-beginning-of-statement-position)))
          (end (or end (py-end-of-statement-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

;; Comment forms
(defun py-uncomment-intern (beg end)
  (uncomment-region beg end)
  (when py-uncomment-indents-p
    (py-indent-region beg end)))

(defun py-uncomment (&optional beg end)
  "Uncomment commented lines at point.

If region is active, restrict uncommenting at region "
  (interactive "*")
  (save-excursion
    (save-restriction
      (when (use-region-p)
        (narrow-to-region (region-beginning) (region-end)))
      (let ((beg (or beg (progn (goto-char (point-min))
                                (search-forward comment-start)
                                (match-beginning 0))))
            end)
        (setq end (or end (py-end-of-comment)))
        (py-uncomment-intern beg (point))))))

(defun py-delete-comments-in-def-or-class ()
  "Delete all commented lines in def-or-class at point"
  (interactive "*")
  (save-excursion
    (let ((beg (py-beginning-of-def-or-class-position))
          (end (py-end-of-def-or-class-position)))
      (and beg end (py--delete-comments-intern beg end)))))

(defun py-delete-comments-in-class ()
  "Delete all commented lines in class at point"
  (interactive "*")
  (save-excursion
    (let ((beg (py-beginning-of-class-position))
          (end (py-end-of-class-position)))
      (and beg end (py--delete-comments-intern beg end)))))

(defun py-delete-comments-in-block ()
  "Delete all commented lines in block at point"
  (interactive "*")
  (save-excursion
    (let ((beg (py-beginning-of-block-position))
          (end (py-end-of-block-position)))
      (and beg end (py--delete-comments-intern beg end)))))

(defun py-delete-comments-in-region (beg end)
  "Delete all commented lines in region. "
  (interactive "r*")
  (save-excursion
    (py--delete-comments-intern beg end)))

(defun py--delete-comments-intern (beg end)
  (save-restriction
    (narrow-to-region beg end)
    (goto-char beg)
    (while (and (< (line-end-position) end) (not (eobp)))
      (beginning-of-line)
      (if (looking-at (concat "[ \t]*" comment-start))
          (delete-region (point) (1+ (line-end-position)))
        (forward-line 1)))))
;;;
(defun py-join-words-wrapping (words separator line-prefix line-length)
  (let ((lines ())
        (current-line line-prefix))
    (while words
      (let* ((word (car words))
             (maybe-line (concat current-line word separator)))
        (if (> (length maybe-line) line-length)
            (setq lines (cons (substring current-line 0 -1) lines)
                  current-line (concat line-prefix word separator " "))
          (setq current-line (concat maybe-line " "))))
      (setq words (cdr words)))
    (setq lines (cons (substring
                       current-line 0 (- 0 (length separator) 1)) lines))
    (mapconcat 'identity (nreverse lines) "\n")))

(defun py-fix-this-indent (indent)
  (unless (and (eq (current-indentation) (current-column))
               (eq (current-column) indent))
    (beginning-of-line)
    (indent-to-column indent)
    (delete-region
     (point)
     (progn (skip-chars-forward " \t") (point)))))

(defun py-fill-comment (&optional justify)
  "Fill the comment paragraph at point"
  (interactive "*P")
  (let (;; Non-nil if the current line contains a comment.
        has-comment

        ;; If has-comment, the appropriate fill-prefix for the comment.
        comment-fill-prefix)

    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond
       ;; A line with nothing but a comment on it?
       ((looking-at "[ \t]*#[# \t]*")
        (setq has-comment t
              comment-fill-prefix (buffer-substring (match-beginning 0)
                                                    (match-end 0))))

       ;; A line with some code, followed by a comment? Remember that the hash
       ;; which starts the comment shouldn't be part of a string or character.
       ((progn
          (while (not (looking-at "#\\|$"))
            (skip-chars-forward "^#\n\"'\\")
            (cond
             ((eq (char-after (point)) ?\\) (forward-char 2))
             ((memq (char-after (point)) '(?\" ?')) (forward-sexp 1))))
          (looking-at "#+[\t ]*"))
        (setq has-comment t)
        (setq comment-fill-prefix
              (concat (make-string (current-column) ? )
                      (buffer-substring (match-beginning 0) (match-end 0)))))))

    (if (not has-comment)
        (fill-paragraph justify)

      ;; Narrow to include only the comment, and then fill the region.
      (save-restriction
        (narrow-to-region

         ;; Find the first line we should include in the region to fill.
         (save-excursion
           (while (and (zerop (forward-line -1))
                       (looking-at "^[ \t]*#")))

           ;; We may have gone to far.  Go forward again.
           (or (looking-at "^[ \t]*#")
               (forward-line 1))
           (point))

         ;; Find the beginning of the first line past the region to fill.
         (save-excursion
           (while (progn (forward-line 1)
                         (looking-at "^[ \t]*#")))
           (point)))

        ;; Lines with only hashes on them can be paragraph boundaries.
        (let ((paragraph-start (concat paragraph-start "\\|[ \t#]*$"))
              (paragraph-separate (concat paragraph-separate "\\|[ \t#]*$"))
              (fill-prefix comment-fill-prefix))
          ;;(message "paragraph-start %S paragraph-separate %S"
          ;;paragraph-start paragraph-separate)
          (fill-paragraph justify))))
    t))

(defun py-until-found (search-string liste)
  "Search liste for search-string until found. "
  (let ((liste liste) element)
    (while liste
      (if (member search-string (car liste))
          (setq element (car liste) liste nil))
      (setq liste (cdr liste)))
    element))

(defun py-end-of-string (&optional beginning-of-string-position)
  "Go to end of string at point, return position.

Takes the result of (syntax-ppss)"
  (interactive)
  (let ((beginning-of-string-position (or beginning-of-string-position (and (nth 3 (syntax-ppss))(nth 8 (syntax-ppss))))))
    (goto-char beginning-of-string-position)
    ;; (and (looking-at "\"\"\"\\|'''\\|\"\\|\'")
    (goto-char (scan-sexps (point) 1)))
  (point))

(defun py-fill-this-paragraph (justify style)
  "Fill just the paragraph at point. "
  (interactive "*")
  (py-fill-string justify style (if (py-beginning-of-paragraph-p) (point) (progn (py-beginning-of-paragraph)(point))) (progn (py-end-of-paragraph)(point))))

(defun py-fill-paragraph (&optional justify style start end docstring)
  "`fill-paragraph-function'

If `py-paragraph-fill-docstring-p' and inside a docstring, the whole docstring is formatted.
See also `py-fill-string' "
  (interactive "P")
  (or (fill-comment-paragraph justify)
      (let* ((orig (copy-marker (point)))
             (pps (syntax-ppss))
             (docstring (and
                         ;; py-paragraph-fill-docstring-p

                         (or docstring (py-docstring-p (nth 8 pps)))))
             (beg (cond (start)
                        ((use-region-p)
                         (region-beginning))
                        ((and docstring
                              ;; (or py-paragraph-fill-docstring-p
                              ;; pep-257-nn, delimiters are on first line
                              ;; (and
                              ;; (eq py-docstring-style 'pep-257-nn)
                              (ignore-errors (<= (py-beginning-of-paragraph-position)(nth 8 pps))))
                         (nth 8 pps))
                        (t (py-beginning-of-paragraph-position))))
             (end (copy-marker
                   (cond (end)
                         ((use-region-p) (region-end))
                         (docstring (py-end-of-string (nth 8 pps)))
                         (t (if (or (looking-at paragraph-start)(re-search-forward paragraph-start nil t 1))
                                (progn (skip-chars-backward " \t\r\n\f")(point))
                              (point))))))
             (style (or style py-docstring-style))
             (this-end (point-min))
             last)
        ;; (when (and (nth 3 pps) (< beg (nth 8 pps))
        ;; docstring
        ;; (setq beg (nth 8 pps)))
        ;; (setq end (py-end-of-string (nth 8 pps))))
        (save-excursion
          (save-restriction
            (narrow-to-region beg end)
            (cond
             ;; Comments
             ((nth 4 pps)
              (py-fill-comment justify))
             ;; Strings/Docstrings
             ((or (nth 3 pps)
                  (equal (string-to-syntax "|")
                         (syntax-after (point)))
                  (looking-at py-string-delim-re))
              (goto-char beg)
              (if (and py-paragraph-fill-docstring-p docstring
                       ;; (re-search-forward (concat "^" py-labelled-re) nil t)
                       )
                  (progn
                    (goto-char beg)
                    ;; must process one by one
                    (while (and (not (eobp)) (setq last (point)) (forward-paragraph) (< last (point))(< (point) end)(setq this-end (point)))
                      (save-restriction
                        (narrow-to-region last this-end)
                        (goto-char last)
                        ;; (if (re-search-forward (concat "^" py-labelled-re) nil t this-end)
                        ;; (py-fill-labelled-string last this-end)

                        (py-fill-string justify style last this-end pps 'no)
                        ;;)
                        (goto-char this-end)
                        (widen))))
                (goto-char orig)
                (py-fill-this-paragraph justify style)))
             ;; Decorators
             ((save-excursion
                (and (py-beginning-of-statement)
                     (equal (char-after)
                            ;; (back-to-indentation)
                            ;; (point))
                            ?\@)))
              (py-fill-decorator justify))
             (t (goto-char orig)
                (py-fill-string justify style (if (py-beginning-of-paragraph-p) (point) (py-beginning-of-paragraph)) (py-end-of-paragraph))))))
        (goto-char orig)
        (back-to-indentation))
      (recenter-top-bottom)
      ;; fill-paragraph expexts t
      t))

(defun py-fill-labelled-string (beg end)
  "Fill string or paragraph containing lines starting with label

See lp:1066489 "
  (interactive "r*")
  (let ((end (copy-marker end))
        (last (copy-marker (point)))
        this-beg this-end)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (skip-chars-forward " \t\r\n\f")
        (if (looking-at py-labelled-re)
            (progn
              (setq this-beg (line-beginning-position))
              (goto-char (match-end 0))
              (while (and (not (eobp)) (re-search-forward py-labelled-re end t 1)(< last (match-beginning 0))(setq last (match-beginning 0)))
                (save-match-data (fill-region this-beg (1- (line-beginning-position))))
                (setq this-beg (line-beginning-position))
                (goto-char (match-end 0)))))))))

(defun py-fill-string (&optional justify style beg end pps docstring)
  "String fill function for `py-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'.

DOCSTRING is either a boolean or 'no
If `py-paragraph-fill-docstring-p' is `t', `M-q` fills the
complete docstring according to setting of `py-docstring-style' "
  (interactive "P")
  (save-excursion
    (save-restriction
      (let* ((style (or style py-docstring-style))
             (fill-column (if (integerp py-docstring-fill-column)
                              py-docstring-fill-column
                            fill-column))
             ;; unset python-mode value this time
             forward-sexp-function
             (orig (point-marker))
             (pps (or pps (syntax-ppss)))
             ;; if beginning of string is closer than arg beg, use this
             (beg (or (and (numberp beg)
                           (ignore-errors (copy-marker beg)))
                      (cond ((and (nth 3 pps) (nth 8 pps))
                             (goto-char (nth 8 pps))
                             (skip-chars-forward "\"'")
                             (copy-marker (point)))
                            ((equal (string-to-syntax "|")
                                    (syntax-after (point)))
                             (point-marker)))))
             ;; Assume docstrings at BOL resp. indentation
             (docstring (and (not (eq 'no docstring))(py-docstring-p (nth 8 pps))))
             (end (or (ignore-errors (and end (goto-char end) (skip-chars-backward "\"' \t\f\n")(copy-marker (point))))
                      (progn (goto-char (nth 8 pps)) (scan-sexps (point) 1) (skip-chars-backward "\"'") (point-marker))))
             multi-line-p
             delimiters-style
             erg)
        ;; whitespace and newline will be added according to mode again
        (goto-char beg)
        (setq beg (progn (skip-chars-forward "\"'") (copy-marker (point))))
        (and docstring
             (delete-region (point) (progn (skip-chars-forward " \t\r\n\f") (skip-chars-forward " \t\r\n\f")(point))))
        (goto-char end)
        (and docstring
             (delete-region (point) (progn (skip-chars-backward " \t\r\n\f")(point))))
        (cond
         ((and docstring
               (string-match (concat "^" py-labelled-re) (buffer-substring-no-properties beg end)))
          (py-fill-labelled-string beg end))
         ((and docstring)
          (narrow-to-region beg end)
          (fill-region (point-min) (point-max)))
         (t (narrow-to-region beg end)
            (sit-for 0.1)
            (fill-region beg end)))
        (and docstring (setq multi-line-p
                             (> (count-matches "\n" beg end) 0)))
        (and docstring
             (setq delimiters-style
                   (case style
                     ;; delimiters-style is a cons cell with the form
                     ;; (START-NEWLINES .  END-NEWLINES). When any of the sexps
                     ;; is NIL means to not add any newlines for start or end
                     ;; of docstring.  See `py-docstring-style' for a
                     ;; graphic idea of each style.
                     (django (cons 1 1))
                     (onetwo (and multi-line-p (cons 1 2)))
                     (pep-257 (and multi-line-p (cons nil 2)))
                     (pep-257-nn (and multi-line-p (cons nil 1)))
                     (symmetric (and multi-line-p (cons 1 1))))))
        (and docstring py-verbose-p (message "%s" delimiters-style))
        (widen)
        (save-excursion
          (when (and docstring style)
            ;; Add the number of newlines indicated by the selected style
            ;; at the start of the docstring.
            (goto-char beg)
            (and
             (car delimiters-style)
             (unless (or (empty-line-p) (save-excursion (forward-line -1)(empty-line-p)))
               (or (newline (car delimiters-style)) t))
             (indent-region beg end))
            ;; Add the number of newlines indicated by the selected style
            ;; at the end of the docstring.
            (goto-char end)
            (unless (eq (char-after) ?\n)
              (and
               (cdr delimiters-style)
               (or (newline (cdr delimiters-style)) t)))
            (setq end (progn (skip-chars-forward " \t\r\n\f")(skip-chars-forward "\"'")(copy-marker (point))))
            (setq beg (progn (goto-char beg) (skip-chars-backward " \t\r\n\f")(skip-chars-backward "\"'") (copy-marker (point)))))
          (indent-region beg end)
          (goto-char end)
          (beginning-of-line)
          (unless (eq (current-indentation) (setq erg (py-compute-indentation)))
            (fixup-whitespace)
            (indent-to erg)))))))

(defun py-fill-decorator (&optional justify)
  "Decorator fill function for `py-fill-paragraph'.
"
  ;; (interactive "*P")
  t)

(defun py-fill-string-django (&optional justify)
  "Fill docstring according to Django's coding standards style.

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'django))

(defun py-fill-string-onetwo (&optional justify)
  "One newline and start and Two at end style.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'onetwo))

(defun py-fill-string-pep-257 (&optional justify)
  "PEP-257 with 2 newlines at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'pep-257))

(defun py-fill-string-pep-257-nn (&optional justify)
  "PEP-257 with 1 newline at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'pep-257-nn))

(defun py-fill-string-symmetric (&optional justify)
  "Symmetric style.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'symmetric))

;; (defun py-fill-paragraph (&optional justify)
;;   "Like \\[fill-paragraph], but handle Python comments and strings.
;;
;; If any of the current line is a comment, fill the comment or the
;; paragraph of it that point is in, preserving the comment's indentation
;; and initial `#'s.
;; If point is inside a string, narrow to that string and fill.
;; "
;;   (interactive "P")
;;   (save-excursion
;;     (save-restriction
;;       (widen)
;;       (let ((pps
;;              (if (featurep 'xemacs)
;;                  (parse-partial-sexp (point-min) (point))
;;                (syntax-ppss))))
;;         (cond
;;          ;; inside a comment
;;          ((nth 4 pps)
;;           (py-fill-comment justify))
;;          ;; only whitespace before the comment start
;;          ((save-excursion (beginning-of-line) (looking-at "[ \t]*#"))
;;           (py-fill-comment justify))
;;          ;; inside a string
;;          ((nth 3 pps)
;;           (py-fill-string (nth 8 pps)))
;;          ;; opening quote of a string
;;          ((progn (save-excursion (forward-char 1)(nth 3 pps)))
;;           (save-excursion
;;             (forward-char 1)
;;             (py-fill-string (nth 8 pps)))))))))

;; Beginning-of- p
(defun py-beginning-of-line-p ()
  "Returns position, if cursor is at the beginning of a line, nil otherwise. "
  (when (bolp)(point)))

(defun py-beginning-of-buffer-p ()
  "Returns position, if cursor is at the beginning of buffer, nil otherwise. "
  (when (bobp)(point)))

(defun py-beginning-of-paragraph-p ()
  "Returns position, if cursor is at the beginning of a paragraph, nil otherwise. "
  (let ((orig (point))
        erg)
    (if (and (bolp) (looking-at paragraph-separate))
        (setq erg (point))
      (save-excursion
        (py-end-of-paragraph)
        (py-beginning-of-paragraph)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py-beginning-of-statement-p ()
  "Returns position, if cursor is at the beginning of a statement, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-end-of-statement)
      (py-beginning-of-statement)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py-beginning-of-expression-p ()
  "Returns position, if cursor is at the beginning of a expression, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-end-of-expression)
      (py-beginning-of-expression)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py-beginning-of-partial-expression-p ()
  "Returns position, if cursor is at the beginning of a partial-expression, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-end-of-partial-expression)
      (py-beginning-of-partial-expression)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py-beginning-of-block-p ()
  "Returns position, if cursor is at the beginning of a block, nil otherwise. "
  (when (and (looking-at py-block-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defun py-beginning-of-clause-p ()
  "Returns position, if cursor is at the beginning of a clause, nil otherwise. "
  (when (and (looking-at py-clause-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defun py-beginning-of-block-or-clause-p ()
  "Returns position, if cursor is at the beginning of a block-or-clause, nil otherwise. "
  (when (and (looking-at py-block-or-clause-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defun py-beginning-of-def-p ()
  "Returns position, if cursor is at the beginning of a def, nil otherwise. "
  (when (and (looking-at py-def-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defun py-beginning-of-class-p ()
  "Returns position, if cursor is at the beginning of a class, nil otherwise. "
  (when (and (looking-at py-class-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defun py-beginning-of-def-or-class-p ()
  "Returns position, if cursor is at the beginning of a def-or-class, nil otherwise. "
  (when (and (looking-at py-def-or-class-re)
             (not (py-in-string-or-comment-p)))
    (point)))

;; End-of- p
(defun py-end-of-line-p ()
  "Returns position, if cursor is at the end of a line, nil otherwise. "
  (when (eolp)(point)))

(defun py-end-of-buffer-p ()
  "Returns position, if cursor is at the end of buffer, nil otherwise. "
  (when (eobp)(point)))

(defun py-end-of-paragraph-p ()
  "Returns position, if cursor is at the end of a paragraph, nil otherwise. "
  (let ((orig (point))
        erg)
    (if (and (eolp) (looking-at paragraph-separate))
        (setq erg (point))
      (save-excursion
        (py-beginning-of-paragraph)
        (py-end-of-paragraph)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py-end-of-statement-p ()
  "Returns position, if cursor is at the end of a statement, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-statement)
      (py-end-of-statement)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py-end-of-expression-p ()
  "Returns position, if cursor is at the end of a expression, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-expression)
      (py-end-of-expression)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py-end-of-partial-expression-p ()
  "Returns position, if cursor is at the end of a partial-expression, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-partial-expression)
      (py-end-of-partial-expression)
      (when (eq orig (point))
        (setq erg orig)))
    erg))

(defun py-end-of-block-p ()
  "Returns position, if cursor is at the end of a block, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-block)
      (py-end-of-block)
      (when (eq orig (point))
        (setq erg orig)))
    erg))

(defun py-end-of-clause-p ()
  "Returns position, if cursor is at the end of a clause, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-clause)
      (py-end-of-clause)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py-end-of-block-or-clause-p ()
  "Returns position, if cursor is at the end of a block-or-clause, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-block-or-clause)
      (py-end-of-block-or-clause)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py-end-of-def-p ()
  "Returns position, if cursor is at the end of a def, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-def)
      (py-end-of-def)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py-end-of-class-p ()
  "Returns position, if cursor is at the end of a class, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-class)
      (py-end-of-class)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py-end-of-def-or-class-p ()
  "Returns position, if cursor is at the end of a def-or-class, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-def-or-class)
      (py-end-of-def-or-class)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

;;; Opens-p
(defun py-statement-opens-block-p (&optional regexp)
  "Return position if the current statement opens a block
in stricter or wider sense.

For stricter sense specify regexp. "
  (interactive)
  (let* ((regexp (or regexp py-block-or-clause-re))
         (erg (py-statement-opens-base regexp)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-statement-opens-base (regexp)
  (let ((orig (point))
        erg)
    (save-excursion
      (back-to-indentation)
      (py-end-of-statement)
      (py-beginning-of-statement)
      (when (and
             (<= (line-beginning-position) orig)(looking-back "^[ \t]*")(looking-at regexp))
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-statement-opens-clause-p ()
  "Return position if the current statement opens block or clause. "
  (interactive)
  (py-statement-opens-base py-clause-re))

(defun py-statement-opens-block-or-clause-p ()
  "Return position if the current statement opens block or clause. "
  (interactive)
  (py-statement-opens-base py-block-or-clause-re))

(defun py-statement-opens-class-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (interactive)
  (py-statement-opens-base py-class-re))

(defun py-statement-opens-def-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (interactive)
  (py-statement-opens-base py-def-re))

(defun py-statement-opens-def-or-class-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (interactive)
  (py-statement-opens-base py-def-or-class-re))

(defun py-statement-closes-block-p ()
  "Return t iff the current statement closes a block.
I.e., if the line starts with `return', `raise', `break', `continue',
and `pass'.  This doesn't catch embedded statements."
  (let ((here (point)))
    (unless (py-beginning-of-statement-p) (py-beginning-of-statement))
    (prog1
        (looking-at py-block-closing-keywords-re)
      (goto-char here))))

(defun py-end-of-clause-intern (&optional regexp orig)
  "Used internal by functions going to the end of a current clause. "
  (unless (eobp)
    (let* ((regexp (or regexp py-block-or-clause-re))
           (orig (or orig (point)))
           (erg (if (py-statement-opens-block-p regexp)
                    (point)
                  (py-go-to-keyword py-extended-block-or-clause-re)
                  (when (py-statement-opens-block-p regexp)
                    (point))))
           ind res)
      (if erg
          (progn
            (setq ind (+ py-indent-offset (current-indentation)))
            (py-end-of-statement)
            (forward-line 1)
            (setq erg (py-travel-current-indent ind)))
        (goto-char orig)
        (py-look-downward-for-clause))
      (when (empty-line-p)
        (skip-chars-backward " \t\r\n\f")
        (py-beginning-of-comment)
        (skip-chars-backward " \t\r\n\f"))
      (when (eq (point) orig)
        (py-look-downward-for-clause))
      (when (< orig (point))
        (setq res (point)))
      res)))

(defun py-end-base (regexp &optional orig decorator)
  "Used internal by functions going to the end forms. "
  (unless (eobp)
    (let* ((orig (or orig (point)))
           (regexp (or regexp 'py-extended-block-or-clause-re))
           (this (progn (back-to-indentation)
                        (cond ((and (py-beginning-of-statement-p)(eq regexp 'py-clause-re)(looking-at py-extended-block-or-clause-re))
                               (point))
                              ((and (py-beginning-of-statement-p)(looking-at (symbol-value regexp)))
                               (point))
                              (t
                               (when
                                   (cdr-safe
                                    (py-go-to-keyword
                                     (cond ((eq regexp 'py-def-or-class-re)
                                            py-def-or-class-re)
                                           ((eq regexp 'py-def-re)
                                            py-def-re)
                                           ((eq regexp 'py-class-re)
                                            py-class-re)
                                           (t py-extended-block-or-clause-re))))
                                 (when (py-statement-opens-block-p py-extended-block-or-clause-re)
                                   (point)))))))
           ind erg last pps)
      (if this
          (progn
            (setq py-bol-forms-last-indent (cons this-command (current-indentation)))
            (setq ind (+ (progn (if py-smart-indentation (py-guess-indent-offset) py-indent-offset)) (current-indentation)))
            (py-end-of-statement)
            (setq last (point))
            (skip-chars-forward " \t\r\n\f")
            (if (eobp)
                (goto-char last)
              (if (and (< (current-indentation) ind) (looking-at (symbol-value regexp)))
                  ;; clause matched
                  (skip-chars-backward " \t\r\n\f")
                (py-travel-current-indent ind (point))
                (cond ((or (eq 'py-clause-re regexp) (eq 'py-block-or-clause-re regexp))
                       (unless (< orig (point))
                         (if
                             (and (setq last (point)) (prog1 (py-end-of-statement)(beginning-of-line))(looking-at py-clause-re))
                             (py-travel-current-indent (+ (current-indentation) py-indent-offset) (point))
                           (goto-char last))))
                      ((eq 'py-block-re regexp)
                       (while
                           (and (setq last (point)) (prog1 (py-end-of-statement)(py-beginning-of-statement))
                                (or (and (looking-at py-clause-re) (<= ind (+ (current-indentation) py-indent-offset))(py-end-of-statement) (py-end-of-statement))
                                    (<= ind (current-indentation))))
                         (py-travel-current-indent ind (point)))
                       (goto-char last))))))
        (goto-char orig))
      (when (and (<= (point) orig)(not (looking-at (symbol-value regexp))))
        ;; found the end above
        ;; py-travel-current-indent will stop of clause at equal indent
        (when (py-look-downward-for-beginning (symbol-value regexp))
          (py-end-base regexp orig)))
      (setq pps (syntax-ppss))
      (if (and (< orig (point)) (not (or (looking-at comment-start) (nth 8 pps) (nth 1 pps))))
          (point)
        (goto-char (point-max))
        nil))))

(defun py-look-downward-for-beginning (regexp)
  "When above any beginning of FORM, search downward. "
  (let* ((orig (point))
         (erg orig)
         (last orig)
         pps)
    (while (and (setq last (point)) (not (eobp)) (re-search-forward regexp nil t 1)(setq erg (match-beginning 0)) (setq pps (syntax-ppss))
                (or (nth 8 pps) (nth 1 pps))))
    (cond ((not (or (nth 8 pps) (nth 1 pps) (or (looking-at comment-start))))
           (when (ignore-errors (< orig erg))
             erg)))))

(defun py-look-downward-for-clause (&optional ind orig regexp)
  "If beginning of other clause exists downward in current block.

If succesful return position. "
  (interactive)
  (unless (eobp)
    (let ((ind (or ind
                   (save-excursion
                     (py-beginning-of-statement)
                     (if (py-statement-opens-block-p)
                         (current-indentation)
                       (- (current-indentation) py-indent-offset)))))
          (orig (or orig (point)))
          (regexp (or regexp py-extended-block-or-clause-re))
          erg last)
      (end-of-line)
      (when (re-search-forward regexp nil t 1)
        (when (nth 8 (syntax-ppss))
          (while (and (re-search-forward regexp nil t 1)
                      (nth 8 (syntax-ppss)))))
        (setq last (point))
        (back-to-indentation)
        (unless (and (looking-at py-clause-re)
                     (not (nth 8 (syntax-ppss))) (eq (current-indentation) ind))
          (progn (setq ind (current-indentation))
                 (while (and (py-end-of-statement-bol)(not (looking-at py-clause-re))(<= ind (current-indentation)))))
          (if (and (looking-at py-clause-re)
                   (not (nth 8 (syntax-ppss)))
                   (< orig (point)))
              (setq erg (point))
            (goto-char orig))))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-current-defun (&optional iact)
  "Go to the outermost method or class definition in current scope.

Python value for `add-log-current-defun-function'.
This tells add-log.el how to find the current function/method/variable.
Returns name of class or methods definition, if found, nil otherwise.

See customizable variables `py-current-defun-show' and `py-current-defun-delay'."
  (interactive "p")
  (save-restriction
    (widen)
    (save-excursion
      (let ((erg (when (py-beginning-of-def-or-class)
                   (forward-word 1)
                   (skip-chars-forward " \t")
                   (prin1-to-string (symbol-at-point)))))
        (when (and erg py-current-defun-show (push-mark (point) t t) (skip-chars-forward "^ (")
                   (exchange-point-and-mark)
                   (sit-for py-current-defun-delay)))
        (when iact (message (prin1-to-string erg)))
        erg))))

(defun py-outdent-p ()
  "Returns non-nil if the current line should dedent one level."
  (save-excursion
    (and (progn (back-to-indentation)
                (looking-at py-clause-re))
         ;; short circuit infloop on illegal construct
         (not (bobp))
         (progn (forward-line -1)
                (py-beginning-of-statement)
                (back-to-indentation)
                (when (looking-at py-blank-or-comment-re)
                  (backward-to-indentation 1))
                (not (looking-at py-no-outdent-re))))))

(defun py-sort-imports ()
  "Sort multiline imports.

Put point inside the parentheses of a multiline import and hit
\\[py-sort-imports] to sort the imports lexicographically"
  (interactive)
  (save-excursion
    (let ((open-paren (save-excursion (progn (up-list -1) (point))))
          (close-paren (save-excursion (progn (up-list 1) (point))))
          sorted-imports)
      (goto-char (1+ open-paren))
      (skip-chars-forward " \n\t")
      (setq sorted-imports
            (sort
             (delete-dups
              (split-string (buffer-substring
                             (point)
                             (save-excursion (goto-char (1- close-paren))
                                             (skip-chars-backward " \n\t")
                                             (point)))
                            ", *\\(\n *\\)?"))
             ;; XXX Should this sort case insensitively?
             'string-lessp))
      ;; Remove empty strings.
      (delete-region open-paren close-paren)
      (goto-char open-paren)
      (insert "(\n")
      (insert (py-join-words-wrapping (remove "" sorted-imports) "," "    " 78))
      (insert ")"))))

(defun py-in-literal (&optional lim)
  "Return non-nil if point is in a Python literal (a comment or string).
Optional argument LIM indicates the beginning of the containing form,
i.e. the limit on how far back to scan."
  (let* ((lim (or lim (point-min)))
         (state (syntax-ppss)))
    (cond
     ((nth 3 state) 'string)
     ((nth 4 state) 'comment))))

(defun py-which-def-or-class ()
  "Returns concatenated `def' and `class' names in hierarchical order, if cursor is inside.

Returns \"???\" otherwise
Used by variable `which-func-functions' "
  (interactive)
  (let* ((orig (point))
         (first t)
         def-or-class
         done last erg name)
    (and first (looking-at "[ \t]*\\_<\\(def\\|class\\)\\_>[ \n\t]\\([[:alnum:]_]+\\)")(not (nth 8 (syntax-ppss)))
         (add-to-list 'def-or-class (match-string-no-properties 2)))
    (while
        (and (not (bobp)) (not done) (or (< 0 (current-indentation)) first))
      (py-beginning-of-def-or-class)
      (looking-at "[ \t]*\\_<\\(def\\|class\\)\\_>[ \n\t]\\([[:alnum:]_]+\\)")
      (setq last (point))
      (setq name (match-string-no-properties 2))
      (if first
          (progn
            (setq first nil)
            (py-end-of-def-or-class)
            (if
                (<= orig (point))
                (goto-char last)
              (setq done t)
              (goto-char orig)))
        t)
      (unless done (add-to-list 'def-or-class name)))
    (unless done (setq def-or-class (mapconcat 'identity def-or-class ".")))
    (goto-char orig)
    (or def-or-class (setq def-or-class "???"))
    (when (interactive-p) (message "%s" def-or-class))
    def-or-class))

(defun py-which-function ()
  "Return the name of the function or class, if curser is in, return nil otherwise. "
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((orig (point))
            (erg (if (and (looking-at (concat py-def-or-class-re " +\\([^(]+\\)(.+")) (not (py-in-string-or-comment-p)))
                     (match-string-no-properties 2)
                   (progn
                     (py-beginning-of-def-or-class)
                     (when (looking-at (concat py-def-or-class-re " +\\([^(]+\\)(.+"))
                       (match-string-no-properties 2))))))
        (if (and erg (< orig (py-end-of-def-or-class)))
            (when (and py-verbose-p (interactive-p)) (message "%s" erg))
          (setq erg nil)
          (when (and py-verbose-p (interactive-p)) (message "%s" "Not inside a function or class"))
          erg)))))

(defconst py-help-address "python-mode@python.org"
  "List dealing with usage and developing python-mode.

Also accepts submission of bug reports, whilst a ticket at
http://launchpad.net/python-mode
is preferable for that. ")

;;; Beginning/End
(defalias 'py-backward-statements 'py-beginning-of-statements)
(defun py-beginning-of-statements ()
  "Got to the beginning of statements in current level which don't open blocks. "
  (interactive)
  (let* ((bounds (py-bounds-of-statements))
         (erg (car bounds)))
    (when erg (goto-char erg))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-forward-of-statements 'py-end-of-statements)
(defun py-end-of-statements ()
  "Got to the end of statements in current level which don't open blocks. "
  (interactive)
  (let* ((bounds (py-bounds-of-statements))
         (erg (cdr bounds)))
    (when erg (goto-char erg))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-backward-expression 'py-beginning-of-expression)
(defun py-beginning-of-expression (&optional arg)
  "Go to the beginning of a compound python expression.

With numeric ARG do it that many times.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

If already at the beginning or before a expression, go to next expression in buffer upwards

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes.
"
  (interactive "p")
  (or arg (setq arg 1))
  (let (erg)
    (if (< 0 arg)
        (save-restriction
          (widen)
          (setq erg (py-beginning-of-expression-intern)))
      (setq arg (abs arg))
      (setq erg (py-end-of-expression arg)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-beginning-of-expression-intern (&optional orig)
  (unless (bobp)
    (let ((orig (or orig (point)))
          (pps (syntax-ppss))
          erg)
      (cond
       ((empty-line-p)
        (while
            (and (empty-line-p)(not (bobp)))
          (forward-line -1)
          (end-of-line))
        (py-beginning-of-expression-intern orig))
       ;; lists
       ((nth 1 pps)
        (goto-char (nth 1 pps))
        (skip-chars-backward py-expression-skip-chars)
        (while (or (looking-back (concat py-string-delim-re py-expression-re py-string-delim-re py-operator-regexp) (line-beginning-position) t)
                   (looking-back (concat "[[:alnum:]_]*" py-operator-regexp "[ \t]*") (line-beginning-position) t))
          (goto-char (match-beginning 0))))
       ;; listed elements
       ((looking-back (concat "[^ \t\n\r\f]+" py-delimiter-regexp))
        (goto-char (match-beginning 0))
        (while (looking-back (concat "[^ \t\n\r\f]+" py-delimiter-regexp))
          (goto-char (match-beginning 0)))
        (unless (or (looking-back py-assignment-regexp) (looking-back "^[ \t]*"))
          (py-beginning-of-expression-intern orig)))
       ;; strings
       ((and (nth 3 pps)(nth 8 pps)
             (goto-char (nth 8 pps)))
        (cond (;; consider expression a string starting at BOL
               (bolp))
              ((looking-back py-assignment-regexp))
              ((looking-back py-operator-regexp)
               (when (nth 2 pps)
                 (goto-char (nth 2 pps))))
              (t (py-beginning-of-expression-intern orig))))
       ;; comments left
       ((nth 8 pps)
        (goto-char (nth 8 pps))
        (unless (bobp)
          (py-beginning-of-expression-intern orig)))
       ;; concatenated strings
       ((looking-back (concat py-string-delim-re py-expression-re py-string-delim-re py-operator-regexp py-string-delim-re py-expression-re py-string-delim-re))
        (goto-char (match-beginning 0))
        (while (looking-back (concat py-string-delim-re py-expression-re py-string-delim-re py-operator-regexp) (line-beginning-position) t)
          (goto-char (match-beginning 0)))
        (skip-chars-backward py-expression-skip-chars))
       ;; before comment
       ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
        (forward-line -1)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f")
        (unless (bobp)
          (forward-char -1)
          (py-beginning-of-expression-intern orig)))
       ;; before assignment
       ((looking-back py-assignment-regexp)
        (goto-char (1- (match-beginning 0)))
        (forward-char -1)
        (py-beginning-of-expression-intern orig))
       ((looking-back py-operator-regexp)
        (goto-char (1- (match-beginning 0)))
        (forward-char -1)
        (unless (< 0 (abs (skip-chars-backward py-expression-skip-chars)))
          (py-beginning-of-expression-intern orig)))
       ((looking-back "\"\\|'")
        (forward-char -1)
        (skip-chars-backward "\"'")
        (unless (looking-back py-assignment-regexp)
          (py-beginning-of-expression-intern orig)))
       ((looking-back "(\\|\\[")
        (forward-char -1)
        (unless (looking-back py-assignment-regexp)
          (py-beginning-of-expression-intern orig)))
       ((looking-back "[\])}]")
        (forward-char -1)
        (unless (looking-back py-assignment-regexp)
          (py-beginning-of-expression-intern orig)))
       ;; inside expression
       ((looking-back py-expression-re)
        (skip-chars-backward py-expression-skip-chars)
        (unless (or (looking-back "^[ \t]*") (looking-back py-assignment-regexp))
          (py-beginning-of-expression-intern orig)))
       ((looking-back (concat "[ \t]*" "[[:alnum:]_]*" py-operator-regexp "[[:alnum:]_]*") (line-beginning-position) t)
        (goto-char (match-beginning 0))
        (unless (looking-back "^[ \t]*")
          (py-beginning-of-expression-intern orig)))
       ((and (eq (point) orig) (looking-back "[ \t\r\n\f]"))
        (skip-chars-backward " \t\r\n\f")
        (unless (bobp)
          (forward-char -1)
          (py-beginning-of-expression-intern orig)))
       ((and (eq (point) orig) (not (bobp)) (looking-back py-expression-re))
        (forward-char -1)
        (when (< 0 (abs (skip-chars-backward py-expression-skip-chars)))
          (py-beginning-of-expression-intern orig)))
       ((and (looking-at py-expression-re) (not (looking-back "[ \t\r\n\f]")))
        (unless (< 0 (abs (skip-chars-backward py-expression-skip-chars)))
          (py-beginning-of-expression-intern orig)))
       ((and (eq (point) orig)(looking-back "[ \t]*="))
        (goto-char (match-beginning 0))
        (skip-chars-backward " \t\r\n\f")
        (py-beginning-of-expression-intern orig)))
      (unless (or (eq (point) orig)(looking-at "[ \t]*#"))
        (setq erg (point)))
      erg)))

(defun py-end-of-expression (&optional arg)
  "Go to the end of a compound python expression.

With numeric ARG do it that many times.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference

Operators however are left aside resp. limit py-expression designed for edit-purposes. "
  (interactive "p")
  (or arg (setq arg 1))
  (let (erg)
    (if (< 0 arg)
        (save-restriction
          (widen)
          (while (< 0 arg)
            (setq erg (py-end-of-expression-intern))
            (setq arg (1- arg))))
      (setq arg (abs arg))
      (setq erg (py-beginning-of-expression arg)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-expression-intern (&optional orig)
  (unless (eobp)
    (let* ((orig (or orig (point)))
           (pps (syntax-ppss))
           erg
           ;; use by scan-lists
           parse-sexp-ignore-comments)
      (cond
       ((nth 1 pps)
        (goto-char (nth 1 pps))
        (let ((parse-sexp-ignore-comments t))
          (forward-list))
        (unless (or (looking-at "[ \t]*$")(looking-at py-assignment-regexp))
          (py-end-of-expression-intern orig)))
       ;; in comment
       ((nth 4 pps)
        (or (< (point) (progn (forward-comment 1)(point)))(forward-line 1))
        (py-end-of-expression-intern orig))
       ((empty-line-p)
        (while
            (and (empty-line-p)(not (eobp)))
          (forward-line 1))
        (py-end-of-expression-intern orig))
       ((looking-at (concat py-string-delim-re py-expression-re py-string-delim-re py-operator-regexp py-string-delim-re py-expression-re py-string-delim-re))
        (goto-char (match-end 0))
        (while (looking-at (concat py-operator-regexp py-string-delim-re py-expression-re py-string-delim-re))
          (goto-char (match-end 0))))
       ;; inside string
       ((py-in-string-p)
        (when (looking-at "\"\"\"\\|'''\\|\"\\|'")
          (goto-char (match-end 0)))
        (while
            (nth 3 (syntax-ppss))
          (forward-char 1))
        ;; (if (looking-at ":")
        ;; (forward-char 1)
        (unless (looking-at "[ \t]*$")
          (py-end-of-expression-intern orig)))
       ((looking-at "[(\[]")
        (forward-list)
        ;; (if (looking-at ":")
        ;; (forward-char 1)
        (unless (looking-at "[ \t]*$")
          (py-end-of-expression-intern orig)))
       ;; ((looking-at ":")
       ;; (forward-char 1)
       ;; (unless (or (looking-at "[ \t]*$")(looking-at py-assignment-regexp))
       ;; (py-end-of-expression-intern orig)))
       ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
        (while (and (looking-at "[ \t]*#") (not (eobp)))
          (forward-line 1))
        (py-end-of-expression-intern orig))
       ((looking-at py-assignment-regexp)
        (goto-char (match-end 0))
        (if (looking-at "[(\[]")
            (forward-list 1)
          (py-end-of-expression-intern orig)))
       ((looking-at (concat "[^ \t\n\r\f]+" py-delimiter-regexp))
        (goto-char (match-end 0))
        (while (looking-at (concat "[^ \t\n\r\f]+" py-delimiter-regexp))
          (goto-char (match-end 0)))
        (forward-char -1)
        (unless (looking-at (concat py-assignment-regexp "\\|[ \t]*$\\|" py-delimiter-regexp))
          (py-end-of-expression-intern orig)))
       ((looking-at (concat "[ \t]*" "[^ (\t\n\r\f]+" py-operator-regexp "[^ \t\n\r\f]+"))
        (goto-char (match-end 0))
        (while (looking-at (concat "[ \t]*" "[^ (\t]+" py-operator-regexp "[^ \t]+"))
          (goto-char (match-end 0)))
        (unless (or (looking-at "[ \t]*$")(looking-at py-assignment-regexp))
          (py-end-of-expression-intern orig)))
       ((looking-at py-not-expression-regexp)
        (skip-chars-forward py-not-expression-chars)
        (unless (or (looking-at "[ \t]*$")(looking-at py-assignment-regexp))
          (py-end-of-expression-intern orig)))
       ((looking-at py-expression-skip-regexp)
        (skip-chars-forward py-expression-skip-chars)
        (unless (or (looking-at "[ \n\t\r\f]*$")(looking-at py-assignment-regexp))
          (py-end-of-expression-intern orig)))
       ;; ((looking-at ":")
       ;; (forward-char 1))
       )
      (unless (or (eq (point) orig)(and (eobp)(bolp)))
        (setq erg (point)))
      erg)))

;; Partial- or Minor Expression
(defalias 'py-backward-partial-expression 'py-beginning-of-partial-expression)
(defun py-beginning-of-partial-expression (&optional orig)
  (interactive)
  (let (erg)
    (skip-chars-backward py-partial-expression-forward-chars)
    (setq erg (point))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-forward-partial-expression 'py-end-of-partial-expression)
(defun py-end-of-partial-expression (&optional orig)
  (interactive)
  (let (erg)
    (skip-chars-forward py-partial-expression-backward-chars)
    ;; group arg
    (and
     (looking-at "[\[{(]")
     (goto-char (scan-sexps (point) 1)))
    (setq erg (point))
    (when (interactive-p) (message "%s" erg))
    erg))

;; Line
(defun py-beginning-of-line ()
  "Go to beginning-of-line, return position.

If already at beginning-of-line and not at BOB, go to beginning of previous line. "
  (interactive)
  (let ((erg (unless (bobp)
               (if (bolp)
                   (progn
                     (forward-line -1)
                     (progn (beginning-of-line)(point)))
                 (progn (beginning-of-line)(point))))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-line ()
  "Go to end-of-line, return position.

If already at end-of-line and not at EOB, go to end of next line. "
  (interactive)
  (let ((erg (unless (eobp)
               (if (eolp)
                   (progn
                     (forward-line 1)
                     (progn (end-of-line)(point)))
                 (progn (end-of-line)(point))))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

;; Statement
(defalias 'py-backward-statement 'py-beginning-of-statement)
(defalias 'py-previous-statement 'py-beginning-of-statement)
(defalias 'py-statement-backward 'py-beginning-of-statement)
(defun py-beginning-of-statement (&optional orig done)
  "Go to the initial line of a simple statement.

For beginning of compound statement use py-beginning-of-block.
For beginning of clause py-beginning-of-clause.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html
"
  (interactive)
  (unless (bobp)
    (let ((orig (or orig (point)))
          (cui (current-indentation))
          (pps (syntax-ppss))
          (done done)
          erg)
      (cond
       ((empty-line-p)
        (skip-chars-backward " \t\r\n\f")
        (py-beginning-of-statement orig done))
       ((nth 8 pps)
        (and (nth 3 pps) (setq done t))
        (goto-char (nth 8 pps))
        (py-beginning-of-statement orig done))
       ((nth 1 pps)
        (goto-char (1- (nth 1 pps)))
        (setq done t)
        (py-beginning-of-statement orig done))
       ((py-preceding-line-backslashed-p)
        (forward-line -1)
        (back-to-indentation)
        (setq done t)
        (py-beginning-of-statement orig done))
       ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
        (forward-comment -1)
        (while (and (not (bobp)) (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
          (forward-comment -1))
        (unless (bobp)
          (py-beginning-of-statement orig done)))
       ((looking-at "[ \t]*#")
        (skip-chars-backward (concat "^" comment-start) (line-beginning-position))
        (back-to-indentation)
        (unless (bobp)
          (py-beginning-of-statement orig done)))
       ((looking-at py-string-delim-re)
        (unless done
          (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
            (setq done t))
          (back-to-indentation)
          (py-beginning-of-statement orig done)))
       ((and (not (eq (point) orig))(looking-back "^[ \t]*"))
        (setq erg (point)))
       ((and (not done) (not (eq 0 (skip-chars-backward " \t\r\n\f"))))
        ;; (setq done t)
        (py-beginning-of-statement orig done))
       ((not (eq (current-column) (current-indentation)))
        (if (< 0 (abs (skip-chars-backward "^\t\r\n\f")))
            (progn
              (setq done t)
              (back-to-indentation)
              (py-beginning-of-statement orig done))
          (back-to-indentation)
          (setq done t)
          (py-beginning-of-statement orig done))))
      (unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
        (when (< (point) orig)(setq erg (point))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defalias 'py-backward-declarations 'py-beginning-of-declarations)
(defun py-beginning-of-declarations ()
  "Got to the beginning of assigments resp. statements in current level which don't open blocks.
"
  (interactive)
  (let* ((bounds (py-bounds-of-declarations))
         (erg (car bounds)))
    (when erg (goto-char erg))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-forward-of-declarations 'py-end-of-declarations)
(defun py-end-of-declarations ()
  "Got to the end of assigments resp. statements in current level which don't open blocks. "
  (interactive)
  (let* ((bounds (py-bounds-of-declarations))
         (erg (cdr bounds)))
    (when erg (goto-char erg))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-beginning-of-top-level ()
  "Go to beginning of block until level of indentation is null.

Returns beginning of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-block)
  (unless (or (bobp) (bolp))
    (py-beginning-of-top-level)))

;;; Beginning of forms
(defun py-beginning-of-form-intern (regexp &optional iact indent orig lc)
  "Go to beginning of FORM.

With INDENT, go to beginning one level above.
Whit IACT, print result in message buffer.

Returns beginning of FORM if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let (erg)
    (unless (bobp)
      (let* ((orig (or orig (point)))
             (indent (or indent (progn
                                  (back-to-indentation)
                                  (or (py-beginning-of-statement-p)
                                      (py-beginning-of-statement))
                                  (current-indentation)))))
        (setq erg (cond ((and (< (point) orig) (looking-at (symbol-value regexp)))
                         (point))
                        ((and (eq 0 (current-column)) (numberp indent) (< 0 indent))
                         (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
                           (py-beginning-of-statement)
                           (unless (looking-at (symbol-value regexp))
                             (cdr (py-go-to-keyword (symbol-value regexp) (current-indentation))))))
                        ;; indent from first beginning of clause matters
                        ((not (looking-at py-extended-block-or-clause-re))
                         (py-go-to-keyword py-extended-block-or-clause-re indent)
                         (if (looking-at (symbol-value regexp))
                             (setq erg (point))
                           (py-beginning-of-form-intern regexp iact (current-indentation) orig)))
                        ((numberp indent)
                         (ignore-errors
                           (cdr (py-go-to-keyword (symbol-value regexp) indent))))
                        (t (ignore-errors
                             (cdr (py-go-to-keyword (symbol-value regexp)
                                                    (- (progn (if (py-beginning-of-statement-p) (current-indentation) (save-excursion (py-beginning-of-statement) (current-indentation)))) py-indent-offset)))))))
        (when lc (beginning-of-line) (setq erg (point)))))
    (when (and py-verbose-p iact) (message "%s" erg))
    erg))

(defun py-beginning-of-prepare (indent final-re &optional inter-re iact lc)
  (let ((orig (point))
        (indent
         (or indent
             (progn (back-to-indentation)
                    (or (py-beginning-of-statement-p)
                        (py-beginning-of-statement))
                    (cond ((eq 0 (current-indentation))
                           (current-indentation))
                          ((looking-at (symbol-value inter-re))
                           (current-indentation))
                          (t
                           (if (<= py-indent-offset (current-indentation))
                               (- (current-indentation) (if py-smart-indentation (py-guess-indent-offset) py-indent-offset))
                             py-indent-offset))))))
        erg)
    (if (and (< (point) orig) (looking-at (symbol-value final-re)))
        (progn
          (and lc (beginning-of-line))
          (setq erg (point))
          (when (and py-verbose-p iact) (message "%s" erg))
          erg)
      (py-beginning-of-form-intern final-re iact indent orig lc))))

(defun py-beginning-of-block (&optional indent)
  "Go to beginning block, skip whitespace at BOL.

Returns beginning of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-block-re 'py-clause-re (interactive-p)))

(defun py-beginning-of-clause (&optional indent)
  "Go to beginning clause, skip whitespace at BOL.

Returns beginning of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-block-or-clause (&optional indent)
  "Go to beginning block-or-clause, skip whitespace at BOL.

Returns beginning of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-def (&optional indent)
  "Go to beginning def, skip whitespace at BOL.

Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-def-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-class (&optional indent)
  "Go to beginning class, skip whitespace at BOL.

Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-class-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-def-or-class (&optional indent)
  "Go to beginning def-or-class, skip whitespace at BOL.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-def-or-class-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-if-block (&optional indent)
  "Go to beginning if-block, skip whitespace at BOL.

Returns beginning of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-if-block-re 'py-clause-re (interactive-p)))

(defun py-beginning-of-try-block (&optional indent)
  "Go to beginning try-block, skip whitespace at BOL.

Returns beginning of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-try-block-re 'py-clause-re (interactive-p)))

(defun py-beginning-of-minor-block (&optional indent)
  "Go to beginning minor-block, skip whitespace at BOL.

Returns beginning of minor-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-minor-block-re 'py-clause-re (interactive-p)))

(defalias 'py-beginning-of-block-bol 'py-beginning-of-block-lc)
(defun py-beginning-of-block-lc (&optional indent)
  "Go to beginning block, go to BOL.

Returns beginning of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-beginning-of-clause-bol 'py-beginning-of-clause-lc)
(defun py-beginning-of-clause-lc (&optional indent)
  "Go to beginning clause, go to BOL.

Returns beginning of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-block-or-clause-bol 'py-beginning-of-block-or-clause-lc)
(defun py-beginning-of-block-or-clause-lc (&optional indent)
  "Go to beginning block-or-clause, go to BOL.

Returns beginning of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-def-bol 'py-beginning-of-def-lc)
(defun py-beginning-of-def-lc (&optional indent)
  "Go to beginning def, go to BOL.

Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-def-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-class-bol 'py-beginning-of-class-lc)
(defun py-beginning-of-class-lc (&optional indent)
  "Go to beginning class, go to BOL.

Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-class-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-def-or-class-bol 'py-beginning-of-def-or-class-lc)
(defun py-beginning-of-def-or-class-lc (&optional indent)
  "Go to beginning def-or-class, go to BOL.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-def-or-class-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-if-block-bol 'py-beginning-of-if-block-lc)
(defun py-beginning-of-if-block-lc (&optional indent)
  "Go to beginning if-block, go to BOL.

Returns beginning of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-if-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-beginning-of-try-block-bol 'py-beginning-of-try-block-lc)
(defun py-beginning-of-try-block-lc (&optional indent)
  "Go to beginning try-block, go to BOL.

Returns beginning of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-try-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-beginning-of-minor-block-bol 'py-beginning-of-minor-block-lc)
(defun py-beginning-of-minor-block-lc (&optional indent)
  "Go to beginning minor-block, go to BOL.

Returns beginning of minor-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-minor-block-re 'py-clause-re (interactive-p) t))

;;;
(defun py-beginning ()
  "Go to beginning of compound statement or definition at point.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-form-intern 'py-extended-block-or-clause-re (interactive-p)))

(defun py-end (&optional indent)
  "Go to end of of compound statement or definition at point.

Returns position block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py-end-base 'py-extended-block-or-clause-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-up (&optional indent)
  "Go up or to beginning of form if inside.

If inside a delimited form --string or list-- go to it's beginning.
If not at beginning of a statement or block, go to it's beginning.
If at beginning of a statement or block, go to beginning one level above of compound statement or definition at point.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let ((pps (syntax-ppss)))
    (cond ((nth 8 pps) (goto-char (nth 8 pps)))
          ((nth 1 pps) (goto-char (nth 1 pps)))
          ((py-beginning-of-statement-p) (py-beginning-of-form-intern 'py-extended-block-or-clause-re (interactive-p)))
          (t (py-beginning-of-statement)))))

(defun py-down (&optional indent)

  "Go to beginning one level below of compound statement or definition at point.

If no statement or block below, but a delimited form --string or list-- go to it's beginning. Repeated call from there will behave like down-list.

Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         erg
         (indent (if
                     (py-beginning-of-statement-p)
                     (current-indentation)
                   (progn
                     (py-beginning-of-statement)
                     (current-indentation))))
         last)
    (while (and (setq last (point)) (py-end-of-statement) (py-end-of-statement) (py-beginning-of-statement) (eq (current-indentation) indent)))
    (if (< indent (current-indentation))
        (setq erg (point))
      (goto-char last))
    (when (< (point) orig)
      (goto-char orig))
    (when (and (eq (point) orig)
               (progn (forward-char 1)
                      (skip-chars-forward "^\"'[({" (line-end-position))
                      (member (char-after) (list ?\( ?\" ?\' ?\[ ?\{)))
               (setq erg (point))))
    (unless erg
      (goto-char orig))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-block (&optional indent)
  "Go to end of block.

Returns end of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py-end-base 'py-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-clause (&optional indent)
  "Go to end of clause.

Returns end of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py-end-base 'py-clause-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-block-or-clause (&optional indent)
  "Go to end of block-or-clause.

Returns end of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py-end-base 'py-block-or-clause-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-def (&optional indent)
  "Go to end of def.

Returns end of def if successful, nil otherwise

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py-end-base 'py-def-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-class (&optional indent)
  "Go to end of class.

Returns end of class if successful, nil otherwise

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py-end-base 'py-class-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-def-or-class (&optional indent)
  "Go to end of def-or-class.

Returns end of def-or-class if successful, nil otherwise

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py-end-base 'py-def-or-class-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-if-block (&optional indent)
  "Go to end of if-block.

Returns end of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py-end-base 'py-if-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-try-block (&optional indent)
  "Go to end of try-block.

Returns end of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py-end-base 'py-try-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-minor-block (&optional indent)
  "Go to end of minor-block.

Returns end of minor-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py-end-base 'py-minor-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

;; Buffer
(defun py-beginning-of-buffer ()
  "Go to beginning-of-buffer, return position. "
  (let ((erg (unless (bobp)
               (goto-char (point-min)))))
    erg))

(defun py-end-of-buffer ()
  "Go to end-of-buffer, return position.

  If already at end-of-buffer and not at EOB, go to end of next line. "
  (let ((erg (unless (eobp)
               (goto-char (point-max)))))
    erg))

(defalias 'py-forward-block 'py-end-of-block)
(defalias 'py-forward-block-or-clause 'py-end-of-block-or-clause)
(defalias 'py-forward-class 'py-end-of-class)
(defalias 'py-forward-clause 'py-end-of-clause)
(defalias 'end-of-def-or-class 'py-end-of-def-or-class)
(defalias 'py-forward-def-or-class 'py-end-of-def-or-class)
(defalias 'py-previous-block 'py-beginning-of-block)
(defalias 'py-goto-block-up 'py-beginning-of-block)
(defalias 'py-backward-block 'py-beginning-of-block)
(defalias 'py-previous-block-or-clause 'py-beginning-of-block-or-clause)
(defalias 'py-goto-block-or-clause-up 'py-beginning-of-block-or-clause)
(defalias 'py-backward-block-or-clause 'py-beginning-of-block-or-clause)
(defalias 'beginning-of-class 'py-beginning-of-class)
(defalias 'py-backward-class 'py-beginning-of-class)
(defalias 'py-previous-class 'py-beginning-of-class)
(defalias 'py-previous-clause 'py-beginning-of-clause)
(defalias 'py-goto-clause-up 'py-beginning-of-clause)
(defalias 'py-backward-clause 'py-beginning-of-clause)
(defalias 'py-backward-def-or-class 'py-beginning-of-def-or-class)
(defalias 'py-previous-def-or-class 'py-beginning-of-def-or-class)

;;; Forms
;; Declarations
(defun py-declarations ()
  "Copy and mark assigments resp. statements in current level which don't open blocks or start with a keyword.

See also `py-statements', which is more general, taking also simple statements starting with a keyword. "
  (interactive)
  (let* ((bounds (py-bounds-of-declarations))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (exchange-point-and-mark))))

;; Statements
(defun py-statements ()
  "Copy and mark simple statements in current level which don't open blocks.

More general than py-declarations, which would stop at keywords like a print-statement. "
  (interactive)
  (let* ((bounds (py-bounds-of-statements))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (exchange-point-and-mark))))

(defun py-go-to-keyword (regexp &optional maxindent)
  "Returns a list, whose car is indentation, cdr position. "
  (let ((orig (point))
        (maxindent (or maxindent (and (< 0 (current-indentation))(current-indentation))
                       ;; make maxindent large enough if not set
                       (* 99 py-indent-offset)))
        (first t)
        done erg cui)
    (while (and (not done) (not (bobp)))
      (py-beginning-of-statement)
      (if (and (looking-at regexp)(if maxindent
                                      (<= (current-indentation) maxindent) t))
          (progn
            (setq erg (point))
            (setq done t))
        (when (and first (not maxindent))
          (setq maxindent (current-indentation))
          (setq first nil))))
    (when erg (setq erg (cons (current-indentation) erg)))
    erg))

(defun py-go-to-keyword-above (regexp &optional maxindent)
  "Returns a list, whose car is indentation, cdr position. "
  (let ((orig (point))
        (maxindent (or maxindent (and (< 0 (current-indentation))(current-indentation))
                       ;; make maxindent large enough if not set
                       (* 99 py-indent-offset)))
        (first t)
        done erg cui)
    (while (and (not done) (not (bobp)))
      (py-beginning-of-statement)
      (if (and (looking-at regexp)(if maxindent
                                      (< (current-indentation) maxindent) t))
          (progn
            (setq erg (point))
            (setq done t))
        (when (and first (not maxindent))
          (setq maxindent (current-indentation))
          (setq first nil))))
    (when erg
      (if (looking-at regexp)
          (setq erg (cons (current-indentation) erg))
        (setq erg nil
              )))
    erg))

(defun py-eos-handle-comment-start ()
  (end-of-line)
  (forward-comment 99999)
  ;; (skip-chars-forward (concat "^" comment-start) (line-end-position))
  ;; (skip-chars-backward " \t\r\n\f" (line-beginning-position))
  )

(defun py-eos-handle-doublequoted-string-start (this)
  "Internal use, find possible end of statement from string start. "
  (when
      (and (setq this (point)) (progn (while (and (not (eobp)) (search-forward (match-string-no-properties 0) nil t 1) (nth 8 (syntax-ppss)))) (< this (point))))
    (skip-chars-forward (concat "^" comment-start) (line-end-position))
    (skip-chars-backward " \t\r\n\f")))

(defun py-eos-handle-singlequoted-string-start (this)
  "Internal use, find possible end of statement from string start. "
  (when
      (and (setq this (point)) (progn (ignore-errors (goto-char (scan-sexps (point) 1))) (< this (point))))
    (skip-chars-forward (concat "^" comment-start) (line-end-position))
    (skip-chars-backward " \t\r\n\f")))

(defun py-handle-eol ()
  (skip-chars-backward " \t\r\n\f" (line-beginning-position))
  (when (py-beginning-of-comment)
    (skip-chars-backward " \t\r\n\f" (line-beginning-position))))

(defun py-eos-handle-string-start (this)
  "Internal use, find possible end of statement from string start. "
  (when
      (and (setq this (point)) (progn (or (if (save-match-data (string-match "'" (match-string-no-properties 0))) (ignore-errors (goto-char (scan-sexps (point) 1)))) (while (and (search-forward (match-string-no-properties 0) nil t 1) (nth 8 (syntax-ppss))))) (< this (point))))
    (skip-chars-forward (concat "^" comment-start) (line-end-position))
    (skip-chars-backward " \t\r\n\f")))

(defalias 'py-statement-forward 'py-end-of-statement)
(defalias 'py-next-statement 'py-end-of-statement)
(defalias 'py-forward-statement 'py-end-of-statement)
(defun py-end-of-statement (&optional orig done origline)
  "Go to the last char of current statement.

To go just beyond the final line of the current statement, use `py-down-statement-bol'. "
  (interactive)
  (unless (eobp)
    (let ((pps (syntax-ppss))
          (origline (or origline (py-count-lines)))
          (orig (or orig (point)))
          erg this
          ;; use by scan-lists
          parse-sexp-ignore-comments
          forward-sexp-function
          stringchar stm)

      (cond
       ((nth 1 pps)
        (when (< orig (point))
          (setq orig (point)))
        (goto-char (nth 1 pps))
        (let ((parse-sexp-ignore-comments t))
          (if (ignore-errors (forward-list))
              (progn
                (when (looking-at ":[ \t]*$")
                  (forward-char 1))
                (setq done t)
                (skip-chars-forward (concat "^" comment-start) (line-end-position))
                (skip-chars-backward " \t\r\n\f" (line-beginning-position))
                (py-end-of-statement orig done origline))
            (goto-char orig))))
       ((and (nth 8 pps)(nth 3 pps))
        (goto-char (nth 8 pps))
        (unless (looking-back "^[ \t]*")
          (setq stm t))
        (when (looking-at "'''\\|'")
          (py-eos-handle-singlequoted-string-start this))
        (when (looking-at "\"\"\"\\|\"")
          (py-eos-handle-doublequoted-string-start this))
        (when stm (setq done t))
        (setq stm nil)
        (unless (nth 3 (syntax-ppss))
          (py-end-of-statement orig done origline)))
       ;; in comment
       ((nth 4 pps)
        (unless (eobp)
          (skip-chars-forward (concat "^" comment-start) (line-end-position))
          (forward-comment 99999)
          (py-handle-eol)
          (py-end-of-statement orig done origline)))
       ((py-current-line-backslashed-p)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f" (line-beginning-position))
        (while (and (eq (char-before (point)) ?\\ )
                    (py-escaped))
          (forward-line 1)
          (end-of-line)
          (skip-chars-backward " \t\r\n\f" (line-beginning-position)))
        (unless (eobp)
          (py-end-of-statement orig done origline)))
       ((and (not done)(looking-at "[ \t]*#"))
        (py-eos-handle-comment-start)
        (py-end-of-statement orig done origline))
       ((looking-at "'''\\|'")
        (py-eos-handle-singlequoted-string-start this)
        ;; string not terminated
        (unless (nth 3 (syntax-ppss))
          (py-end-of-statement orig done origline)))
       ((looking-at "\"\"\"\\|\"")
        (py-eos-handle-doublequoted-string-start this)
        ;; string not terminated
        (unless (nth 3 (syntax-ppss))
          (py-end-of-statement orig done origline)))
       ((looking-at py-string-delim-re)
        (py-eos-handle-string-start this)
        (py-end-of-statement orig done origline))
       ((and (looking-at py-no-outdent-re)(not (nth 8 pps)))
        (end-of-line)
        (py-handle-eol))
       ((and (eq (point) orig) (< (current-column) (current-indentation)))
        (back-to-indentation)
        (py-end-of-statement orig done origline))
       ((and (not done)
             (or (eq (current-column) (current-indentation))
                 (eq origline (py-count-lines)))
             (< 0 (abs (skip-chars-forward (concat "^" comment-start) (line-end-position)))))
        (py-handle-eol)
        ;; with trailing whitespaces at orig
        (if (and (< orig (point)) (not (progn (setq pps (syntax-ppss))(or (nth 8 pps)(nth 1 pps)))))
            (setq done t)
          (if (or (nth 8 pps)(nth 1 pps))
              (py-end-of-statement orig done origline)
            (forward-line 1)
            (py-handle-eol)))
        (py-end-of-statement orig done origline))
       ((and (not done)
             (or (eq (current-column) (current-indentation))
                 (eq origline (py-count-lines)))
             (< 0 (skip-chars-forward " \t\r\n\f")))
        (when (looking-at "[ \t]*#")
          (py-eos-handle-comment-start))
        (py-end-of-statement orig done origline))
       ((and (not done) (eq (point) orig)(looking-at ";"))
        (skip-chars-forward ";" (line-end-position))
        (when (< 0 (skip-chars-forward (concat "^" comment-start) (line-end-position)))
          (py-beginning-of-comment)
          (skip-chars-backward " \t\r\n\f")
          (setq done t))
        (py-end-of-statement orig done origline))
       ((bolp)
        (end-of-line)
        (py-beginning-of-comment)
        (skip-chars-backward " \t\r\n\f")
        (setq done t)
        (py-end-of-statement orig done origline))
       ((and (not (ignore-errors (eq (point) done)))(looking-back py-string-delim-re) (progn (goto-char (match-beginning 0))(and (nth 8 (syntax-ppss))(nth 3 (syntax-ppss)))))
        (end-of-line)
        (py-beginning-of-comment)
        (skip-chars-backward " \t\r\n\f")
        (setq done (point))
        (py-end-of-statement orig done origline))
       ((and done (eq (current-column) (current-indentation)))
        (skip-chars-forward (concat "^" comment-start) (line-end-position))
        (skip-chars-backward " \t\r\n\f")
        (py-beginning-of-comment)
        (skip-chars-backward " \t\r\n\f" (line-beginning-position))
        (setq done t)
        (py-end-of-statement orig done origline)))
      (unless
          (or
           (eq (point) orig)
           (empty-line-p))
        (setq erg (point)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))
;;;
(defun py-goto-statement-below ()
  "Goto beginning of next statement. "
  (interactive)
  (let ((orig (point))
        (erg (py-end-of-statement)))
    (py-beginning-of-statement)
    (when (< (point) orig)
      (goto-char erg)
      (py-end-of-statement)
      (py-beginning-of-statement))))

;; Decorator
(defun py-beginning-of-decorator ()
  "Go to the beginning of a decorator.

Returns position if succesful "
  (interactive)
  (back-to-indentation)
  (while (and (not (looking-at "@\\w+"))(not (empty-line-p))(not (bobp))(forward-line -1))
    (back-to-indentation))
  (let ((erg (when (looking-at "@\\w+")(match-beginning 0))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-decorator ()
  "Go to the end of a decorator.

Returns position if succesful "
  (interactive)
  (let ((orig (point)) erg)
    (unless (looking-at "@\\w+")
      (setq erg (py-beginning-of-decorator)))
    (when erg
      (if
          (re-search-forward py-def-or-class-re nil t)
          (progn
            (back-to-indentation)
            (skip-chars-backward " \t\r\n\f")
            (py-leave-comment-or-string-backward)
            (skip-chars-backward " \t\r\n\f")
            (setq erg (point)))
        (goto-char orig)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f")
        (when (ignore-errors (goto-char (py-in-list-p)))
          (forward-list))
        (when (< orig (point))
          (setq erg (point))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

;;; Mark
(defun py-mark-base (form &optional py-mark-decorators)
  (let* ((begform (intern-soft (concat "py-beginning-of-" form)))
         (endform (intern-soft (concat "py-end-of-" form)))
         (begcheckform (intern-soft (concat "py-beginning-of-" form "-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when py-mark-decorators
      (save-excursion
        (when (setq erg (py-beginning-of-decorator))
          (setq beg erg))))
    (push-mark beg t t)
    (setq end (funcall endform))
    (unless end (when (< beg (point))
                  (setq end (point))))
    (when (interactive-p) (message "%s %s" beg end))
    (cons beg end)))

(defun py-mark-paragraph ()
  "Mark paragraph at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "paragraph"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-block ()
  "Mark block at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-clause ()
  "Mark clause at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "clause"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-block-or-clause ()
  "Mark block-or-clause at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "block-or-clause"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-def (&optional arg)
  "Mark def at point.

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base "def" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-class (&optional arg)
  "Mark class at point.

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base "class" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-def-or-class (&optional arg)
  "Mark def-or-class at point.

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base "def-or-class" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-line ()
  "Mark line at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "line"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-statement ()
  "Mark statement at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "statement"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-expression ()
  "Mark expression at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "expression"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-partial-expression ()
  "Mark partial-expression at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "partial-expression"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

;;; Copyin
(defalias 'py-copy-declarations 'py-declarations)
(defalias 'py-copy-statements 'py-statements)

(defalias 'py-expression 'py-copy-expression)
(defun py-copy-expression ()
  "Mark expression at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "expression")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-minor-expression 'py-copy-partial-expression)
(defalias 'py-partial-expression 'py-copy-partial-expression)
(defun py-copy-partial-expression ()
  "Mark partial-expression at point.

Returns beginning and end positions of marked area, a cons.

\".\" operators delimit a partial-expression expression on it's level, that's the difference to compound expressions.

Given the function below, `py-partial-expression'
called at pipe symbol would copy and return:

def usage():
    print \"\"\"Usage: %s
    ....\"\"\" % (
        os.path.basename(sys.argv[0]))
------------|-------------------------
==> path

        os.path.basename(sys.argv[0]))
------------------|-------------------
==> basename(sys.argv[0]))

        os.path.basename(sys.argv[0]))
--------------------------|-----------
==> sys

        os.path.basename(sys.argv[0]))
------------------------------|-------
==> argv[0]

while `py-expression' would copy and return

\(
        os.path.basename(sys.argv[0]))

\;;

Also for existing commands a shorthand is defined:

\(defalias 'py-statement 'py-copy-statement)"

  (interactive)
  (let ((erg (py-mark-base "partial-expression")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-statement 'py-copy-statement)
(defun py-copy-statement ()
  "Mark statement at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "statement")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-block 'py-copy-block)
(defun py-copy-block ()
  "Mark block at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "block")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-block-or-clause 'py-copy-block-or-clause)
(defun py-copy-block-or-clause ()
  "Mark block-or-clause at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "block-or-clause")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-def 'py-copy-def)
(defun py-copy-def (&optional arg)
  "Mark def at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons."

  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py-mark-base "def" py-mark-decorators)))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defun py-copy-def-or-class (&optional arg)
  "Mark def-or-class at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons."
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py-mark-base "def-or-class" py-mark-decorators)))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-class 'py-copy-class)
(defun py-copy-class (&optional arg)
  "Mark class at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons."

  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py-mark-base "class" py-mark-decorators)))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-clause 'py-copy-clause)
(defun py-copy-clause ()
  "Mark clause at point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "clause")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

;;; Deleting
(defun py-kill-statements ()
  "Delete statements declared in current level.

Store deleted statements in kill-ring "
  (interactive "*")
  (let* ((bounds (py-bounds-of-statements))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (delete-region beg end))))

(defun py-kill-declarations ()
  "Delete variables declared in current level.

Store deleted variables in kill-ring "
  (interactive "*")
  (let* ((bounds (py-bounds-of-declarations))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (delete-region beg end))))

(defun py-kill-expression ()
  "Delete expression at point.
  Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive)
  (let ((erg (py-mark-base "expression")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-partial-expression ()
  "Delete partial-expression at point.
  Stores data in kill ring. Might be yanked back using `C-y'.

\".\" operators delimit a partial-expression expression on it's level, that's the difference to compound expressions."
  (interactive)
  (let ((erg (py-mark-base "partial-expression")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-statement ()
  "Delete statement at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "statement")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block ()
  "Delete block at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block-or-clause ()
  "Delete block-or-clause at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "block-or-clause")))
    (kill-region (region-beginning) (region-end))))

(defun py-kill-def-or-class ()
  "Delete def-or-class at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "def-or-class")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-class ()
  "Delete class at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "class")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-def ()
  "Delete def at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "def")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-clause ()
  "Delete clause at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "clause")))
    (kill-region (car erg) (cdr erg))))

(defalias 'py-kill-minor-expression 'py-kill-partial-expression)
;;; Beginning of line forms
(defun py-mark-base-bol (form &optional py-mark-decorators)
  (let* ((begform (intern-soft (concat "py-beginning-of-" form "-bol")))
         (endform (intern-soft (concat "py-end-of-" form "-bol")))
         (begcheckform (intern-soft (concat "py-beginning-of-" form "-bol-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when py-mark-decorators
      (save-excursion
        (when (setq erg (py-beginning-of-decorator-bol))
          (setq beg erg))))
    (setq end (funcall endform))
    (push-mark beg t t)
    (unless end (when (< beg (point))
                  (setq end (point))))
    (when (interactive-p) (message "%s %s" beg end))
    (cons beg end)))

(defun py-beginning-of-block-bol-p ()
  "Returns position, if cursor is at the beginning of block, at beginning of line, nil otherwise. "
  (interactive)
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-block-bol)
      (py-beginning-of-block-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defalias 'py-down-block-lc 'py-end-of-block-bol)
(defun py-end-of-block-bol ()
  "Goto beginning of line following end of block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-block': down from current definition to next beginning of block below. "
  (interactive)
  (let ((erg (py-end-of-block)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-mark-block-bol ()
  "Mark block, take beginning of line positions.

Returns beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base-bol "block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-block-bol ()
  "Delete block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-block-bol ()
  "Delete block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-block-bol ()
  "Delete block bol at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-beginning-of-clause-bol-p ()
  "Returns position, if cursor is at the beginning of clause, at beginning of line, nil otherwise. "
  (interactive)
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-clause-bol)
      (py-beginning-of-clause-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defalias 'py-down-clause-lc 'py-end-of-clause-bol)
(defun py-end-of-clause-bol ()
  "Goto beginning of line following end of clause.
  Returns position reached, if successful, nil otherwise.

See also `py-down-clause': down from current definition to next beginning of clause below. "
  (interactive)
  (let ((erg (py-end-of-clause)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-mark-clause-bol ()
  "Mark clause, take beginning of line positions.

Returns beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base-bol "clause"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-clause-bol ()
  "Delete clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "clause")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-clause-bol ()
  "Delete clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-clause-bol ()
  "Delete clause bol at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-beginning-of-block-or-clause-bol-p ()
  "Returns position, if cursor is at the beginning of block-or-clause, at beginning of line, nil otherwise. "
  (interactive)
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-block-or-clause-bol)
      (py-beginning-of-block-or-clause-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defalias 'py-down-block-or-clause-lc 'py-end-of-block-or-clause-bol)
(defun py-end-of-block-or-clause-bol ()
  "Goto beginning of line following end of block-or-clause.
  Returns position reached, if successful, nil otherwise.

See also `py-down-block-or-clause': down from current definition to next beginning of block-or-clause below. "
  (interactive)
  (let ((erg (py-end-of-block-or-clause)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-mark-block-or-clause-bol ()
  "Mark block-or-clause, take beginning of line positions.

Returns beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base-bol "block-or-clause"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-block-or-clause-bol ()
  "Delete block-or-clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block-or-clause")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-block-or-clause-bol ()
  "Delete block-or-clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-block-or-clause-bol ()
  "Delete block-or-clause bol at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-beginning-of-def-bol-p ()
  "Returns position, if cursor is at the beginning of def, at beginning of line, nil otherwise. "
  (interactive)
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-def-bol)
      (py-beginning-of-def-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defalias 'py-down-def-lc 'py-end-of-def-bol)
(defun py-end-of-def-bol ()
  "Goto beginning of line following end of def.
  Returns position reached, if successful, nil otherwise.

See also `py-down-def': down from current definition to next beginning of def below. "
  (interactive)
  (let ((erg (py-end-of-def)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-mark-def-bol (&optional arg)
  "Mark def, take beginning of line positions.

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of region, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base-bol "def" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-def-bol ()
  "Delete def bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "def")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-def-bol ()
  "Delete def bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-def-bol ()
  "Delete def bol at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-beginning-of-class-bol-p ()
  "Returns position, if cursor is at the beginning of class, at beginning of line, nil otherwise. "
  (interactive)
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-class-bol)
      (py-beginning-of-class-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defalias 'py-down-class-lc 'py-end-of-class-bol)
(defun py-end-of-class-bol ()
  "Goto beginning of line following end of class.
  Returns position reached, if successful, nil otherwise.

See also `py-down-class': down from current definition to next beginning of class below. "
  (interactive)
  (let ((erg (py-end-of-class)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-mark-class-bol (&optional arg)
  "Mark class, take beginning of line positions.

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of region, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base-bol "class" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-class-bol ()
  "Delete class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "class")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-class-bol ()
  "Delete class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-class-bol ()
  "Delete class bol at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-beginning-of-def-or-class-bol-p ()
  "Returns position, if cursor is at the beginning of def-or-class, at beginning of line, nil otherwise. "
  (interactive)
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-def-or-class-bol)
      (py-beginning-of-def-or-class-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defalias 'py-down-def-or-class-lc 'py-end-of-def-or-class-bol)
(defun py-end-of-def-or-class-bol ()
  "Goto beginning of line following end of def-or-class.
  Returns position reached, if successful, nil otherwise.

See also `py-down-def-or-class': down from current definition to next beginning of def-or-class below. "
  (interactive)
  (let ((erg (py-end-of-def-or-class)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-mark-def-or-class-bol (&optional arg)
  "Mark def-or-class, take beginning of line positions.

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of region, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base-bol "def-or-class" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-def-or-class-bol ()
  "Delete def-or-class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "def-or-class")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-def-or-class-bol ()
  "Delete def-or-class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-def-or-class-bol ()
  "Delete def-or-class bol at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-beginning-of-statement-bol-p ()
  "Returns position, if cursor is at the beginning of statement, at beginning of line, nil otherwise. "
  (interactive)
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-statement-bol)
      (py-beginning-of-statement-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defalias 'py-beginning-of-statement-lc 'py-beginning-of-statement-bol)
(defun py-beginning-of-statement-bol (&optional indent)
  "Goto beginning of line where statement starts.
  Returns position reached, if successful, nil otherwise.

See also `py-up-statement': up from current definition to next beginning of statement above. "
  (interactive)
  (let* ((indent (or indent (when (eq 'py-end-of-statement-bol (car py-bol-forms-last-indent))(cdr py-bol-forms-last-indent))))
         erg)
    (if indent
        (while (and (setq erg (py-beginning-of-statement)) (< indent (current-indentation))(not (bobp))))
      (setq erg (py-beginning-of-statement)))
    ;; reset
    (setq py-bol-forms-last-indent nil)
    (when erg
      (unless (eobp)
        (beginning-of-line)
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-down-statement-lc 'py-end-of-statement-bol)
(defun py-end-of-statement-bol ()
  "Goto beginning of line following end of statement.
  Returns position reached, if successful, nil otherwise.

See also `py-down-statement': down from current definition to next beginning of statement below. "
  (interactive)
  (let ((erg (py-end-of-statement)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-mark-statement-bol ()
  "Mark statement, take beginning of line positions.

Returns beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base-bol "statement"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-statement-bol ()
  "Delete statement bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "statement")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-statement-bol ()
  "Delete statement bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-statement-bol ()
  "Delete statement bol at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

;;; Up/Down
(defun py-up-statement ()
  "Go to the beginning of next statement upwards in buffer.

Return position if statement found, nil otherwise. "
  (interactive)
  (let ((orig (point))
        erg)
    (if (py-beginning-of-statement-p)
        (setq erg (py-beginning-of-statement))
      (setq erg (and (py-beginning-of-statement) (py-beginning-of-statement))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-down-statement ()
  "Go to the beginning of next statement downwards in buffer.

Return position if statement found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         (erg
          (cond ((py-end-of-statement-p)
                 (and (py-end-of-statement) (py-beginning-of-statement)))
                ((ignore-errors (< orig (progn (py-end-of-statement) (py-beginning-of-statement))))
                 (point))
                (t (goto-char orig) (and (py-end-of-statement) (py-end-of-statement)(py-beginning-of-statement))))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-up-base (regexp)
  "Go to the beginning of next form upwards in buffer.

Return position if form found, nil otherwise. "
  (let* ((orig (point))
         erg)
    (if (bobp)
        (setq erg nil)
      (while (and (re-search-backward regexp nil t 1)
                  (nth 8 (syntax-ppss))))
      (back-to-indentation)
      (when (looking-at regexp) (setq erg (point)))
      (when py-verbose-p (message "%s" erg))
      erg)))

(defun py-down-base (regexp)
  "Go to the beginning of next form below in buffer.

Return position if form found, nil otherwise. "
  (unless (eobp)
    (forward-line 1)
    (beginning-of-line)
    (let* ((orig (point))
           erg)
      (if (eobp)
          (setq erg nil)
        (while (and (re-search-forward regexp nil t 1)
                    (nth 8 (syntax-ppss))))
        (back-to-indentation)
        (when (looking-at regexp) (setq erg (point)))
        (when py-verbose-p (message "%s" erg))
        erg))))

(defun py-up-base-bol (regexp)
  "Go to the beginning of next form upwards in buffer.

Return position if form found, nil otherwise. "
  (let* ((orig (point))
         erg)
    (if (bobp)
        (setq erg nil)
      (while (and (re-search-backward regexp nil t 1)
                  (nth 8 (syntax-ppss))))
      (beginning-of-line)
      (when (looking-at regexp) (setq erg (point)))
      (when py-verbose-p (message "%s" erg))
      erg)))

(defun py-down-base-bol (regexp)
  "Go to the beginning of next form below in buffer.

Return position if form found, nil otherwise. "
  (unless (eobp)
    (forward-line 1)
    (beginning-of-line)
    (let* ((orig (point))
           erg)
      (if (eobp)
          (setq erg nil)
        (while (and (re-search-forward regexp nil t 1)
                    (nth 8 (syntax-ppss))))
        (beginning-of-line)
        (when (looking-at regexp) (setq erg (point)))
        (when py-verbose-p (message "%s" erg))
        erg))))

(defun py-up-block ()
  "Go to the beginning of next block upwards in buffer.

Return position if block found, nil otherwise. "
  (interactive)
  (py-up-base py-block-re))

(defun py-up-minor-block ()
  "Go to the beginning of next minor-block upwards in buffer.

Return position if minor-block found, nil otherwise. "
  (interactive)
  (py-up-base py-minor-block-re))

(defun py-up-clause ()
  "Go to the beginning of next clause upwards in buffer.

Return position if clause found, nil otherwise. "
  (interactive)
  (py-up-base py-clause-re))

(defun py-up-block-or-clause ()
  "Go to the beginning of next block-or-clause upwards in buffer.

Return position if block-or-clause found, nil otherwise. "
  (interactive)
  (py-up-base py-block-or-clause-re))

(defun py-up-def ()
  "Go to the beginning of next def upwards in buffer.

Return position if def found, nil otherwise. "
  (interactive)
  (py-up-base py-def-re))

(defun py-up-class ()
  "Go to the beginning of next class upwards in buffer.

Return position if class found, nil otherwise. "
  (interactive)
  (py-up-base py-class-re))

(defun py-up-def-or-class ()
  "Go to the beginning of next def-or-class upwards in buffer.

Return position if def-or-class found, nil otherwise. "
  (interactive)
  (py-up-base py-def-or-class-re))

(defun py-down-block ()
  "Go to the beginning of next block below in buffer.

Return position if block found, nil otherwise. "
  (interactive)
  (py-down-base py-block-re))

(defun py-down-minor-block ()
  "Go to the beginning of next minor-block below in buffer.

Return position if minor-block found, nil otherwise. "
  (interactive)
  (py-down-base py-minor-block-re))

(defun py-down-clause ()
  "Go to the beginning of next clause below in buffer.

Return position if clause found, nil otherwise. "
  (interactive)
  (py-down-base py-clause-re))

(defun py-down-block-or-clause ()
  "Go to the beginning of next block-or-clause below in buffer.

Return position if block-or-clause found, nil otherwise. "
  (interactive)
  (py-down-base py-block-or-clause-re))

(defun py-down-def ()
  "Go to the beginning of next def below in buffer.

Return position if def found, nil otherwise. "
  (interactive)
  (py-down-base py-def-re))

(defun py-down-class ()
  "Go to the beginning of next class below in buffer.

Return position if class found, nil otherwise. "
  (interactive)
  (py-down-base py-class-re))

(defun py-down-def-or-class ()
  "Go to the beginning of next def-or-class below in buffer.

Return position if def-or-class found, nil otherwise. "
  (interactive)
  (py-down-base py-def-or-class-re))

(defun py-up-block-bol ()
  "Go to the beginning of next block upwards in buffer.

Go to beginning of line.
Return position if block found, nil otherwise. "
  (interactive)
  (py-up-base-bol py-block-re))

(defun py-up-minor-block-bol ()
  "Go to the beginning of next minor-block upwards in buffer.

Go to beginning of line.
Return position if minor-block found, nil otherwise. "
  (interactive)
  (py-up-base-bol py-minor-block-re))

(defun py-up-clause-bol ()
  "Go to the beginning of next clause upwards in buffer.

Go to beginning of line.
Return position if clause found, nil otherwise. "
  (interactive)
  (py-up-base-bol py-clause-re))

(defun py-up-block-or-clause-bol ()
  "Go to the beginning of next block-or-clause upwards in buffer.

Go to beginning of line.
Return position if block-or-clause found, nil otherwise. "
  (interactive)
  (py-up-base-bol py-block-or-clause-re))

(defun py-up-def-bol ()
  "Go to the beginning of next def upwards in buffer.

Go to beginning of line.
Return position if def found, nil otherwise. "
  (interactive)
  (py-up-base-bol py-def-re))

(defun py-up-class-bol ()
  "Go to the beginning of next class upwards in buffer.

Go to beginning of line.
Return position if class found, nil otherwise. "
  (interactive)
  (py-up-base-bol py-class-re))

(defun py-up-def-or-class-bol ()
  "Go to the beginning of next def-or-class upwards in buffer.

Go to beginning of line.
Return position if def-or-class found, nil otherwise. "
  (interactive)
  (py-up-base-bol py-def-or-class-re))

(defun py-down-block-bol ()
  "Go to the beginning of next block below in buffer.

Go to beginning of line
Return position if block found, nil otherwise "
  (interactive)
  (py-down-base-bol py-block-re))

(defun py-down-minor-block-bol ()
  "Go to the beginning of next minor-block below in buffer.

Go to beginning of line
Return position if minor-block found, nil otherwise "
  (interactive)
  (py-down-base-bol py-minor-block-re))

(defun py-down-clause-bol ()
  "Go to the beginning of next clause below in buffer.

Go to beginning of line
Return position if clause found, nil otherwise "
  (interactive)
  (py-down-base-bol py-clause-re))

(defun py-down-block-or-clause-bol ()
  "Go to the beginning of next block-or-clause below in buffer.

Go to beginning of line
Return position if block-or-clause found, nil otherwise "
  (interactive)
  (py-down-base-bol py-block-or-clause-re))

(defun py-down-def-bol ()
  "Go to the beginning of next def below in buffer.

Go to beginning of line
Return position if def found, nil otherwise "
  (interactive)
  (py-down-base-bol py-def-re))

(defun py-down-class-bol ()
  "Go to the beginning of next class below in buffer.

Go to beginning of line
Return position if class found, nil otherwise "
  (interactive)
  (py-down-base-bol py-class-re))

(defun py-down-def-or-class-bol ()
  "Go to the beginning of next def-or-class below in buffer.

Go to beginning of line
Return position if def-or-class found, nil otherwise "
  (interactive)
  (py-down-base-bol py-def-or-class-re))

;; ripped from cc-mode
(defun py-forward-into-nomenclature (&optional arg iact)
  "Move forward to end of a nomenclature section or word.

With \\[universal-argument] (programmatically, optional argument ARG), do it that many times.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((case-fold-search nil)
        (orig (point))
        erg)
    (if (> arg 0)
        (while (and (not (eobp)) (> arg 0))
          ;; (setq erg (re-search-forward "\\(\\W+[_[:lower:][:digit:]ß]+\\)" nil t 1))
          (cond
           ((or (not (eq 0 (skip-chars-forward "[[:blank:][:punct:]\n\r]")))
                (not (eq 0 (skip-chars-forward "_"))))
            (when (or
                   (< 1 (skip-chars-forward "[:upper:]"))
                   (not (eq 0 (skip-chars-forward "[[:lower:][:digit:]ß]")))
                   (not (eq 0 (skip-chars-forward "[[:lower:][:digit:]]"))))
              (setq arg (1- arg))))
           ((or
             (< 1 (skip-chars-forward "[:upper:]"))
             (not (eq 0 (skip-chars-forward "[[:lower:][:digit:]ß]")))
             (not (eq 0 (skip-chars-forward "[[:lower:][:digit:]]"))))
            (setq arg (1- arg)))))
      (while (and (not (bobp)) (< arg 0))
        (when (not (eq 0 (skip-chars-backward "[[:blank:][:punct:]\n\r\f_]")))

          (forward-char -1))
        (or
         (not (eq 0 (skip-chars-backward "[:upper:]")))
         (not (eq 0 (skip-chars-backward "[[:lower:][:digit:]ß]")))
         (skip-chars-backward "[[:lower:][:digit:]ß]"))
        (setq arg (1+ arg))))
    (if (< (point) orig)
        (progn
          (when (looking-back "[[:upper:]]")
            ;; (looking-back "[[:blank:]]"
            (forward-char -1))
          (if (looking-at "[[:alnum:]ß]")
              (setq erg (point))
            (setq erg nil)))
      (if (and (< orig (point)) (not (eobp)))
          (setq erg (point))
        (setq erg nil)))
    (when (and py-verbose-p (or iact (interactive-p))) (message "%s" erg))
    erg))

(defun py-backward-into-nomenclature (&optional arg)
  "Move backward to beginning of a nomenclature section or word.

With optional ARG, move that many times.  If ARG is negative, move
forward.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (setq arg (or arg 1))
  (py-forward-into-nomenclature (- arg) arg))

(defun match-paren (&optional arg)
  "Go to the matching brace, bracket or parenthesis if on its counterpart.

Otherwise insert the character, the key is assigned to, here `%'.
With universal arg \C-u insert a `%'. "
  (interactive "P")
  (let ((parse-sexp-ignore-comments t))
    (if arg
        (self-insert-command (if (numberp arg) arg 1))
      (cond
       ((and (not match-paren-no-use-syntax-pps) (looking-at "\\s("))
        (forward-list 1)
        (backward-char 1))
       ((and (not match-paren-no-use-syntax-pps)(looking-at "\\s)"))
        (forward-char 1) (backward-list 1))
       ;; if match-paren-no-syntax-pps
       ((looking-at "(")
        (ar-parentized-end-atpt))
       ((looking-at ")")
        (ar-parentized-beginning-atpt))
       ((looking-at "\\\[")
        (ar-bracketed-end-atpt))
       ((looking-at "]")
        (ar-bracketed-beginning-atpt))
       ((looking-at "{")
        (ar-braced-end-atpt))
       ((looking-at "}")
        (ar-braced-beginning-atpt))
       (t (self-insert-command 1))))))

(defun py-travel-current-indent (indent &optional orig)
  "Moves down until clause is closed, i.e. current indentation is reached.

Takes a list, INDENT and START position. "
  (unless (eobp)
    (let ((orig (or orig (point)))
          last)
      (while (and (setq last (point))(not (eobp))(py-end-of-statement)
                  (save-excursion (or (<= indent (progn  (py-beginning-of-statement)(current-indentation)))(eq last (line-beginning-position))))
                  (py-end-of-statement-p)))
      (goto-char last)
      (when (< orig last)
        last))))

(defalias 'iyp 'ipython)
(defalias 'ipy 'ipython)
;;; Python named shells
(defun python (&optional argprompt)
  "Start an Python interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "python"))

(defun ipython (&optional argprompt)
  "Start an IPython interpreter.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "ipython"))

(defun python3 (&optional argprompt)
  "Start an Python3 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "python3"))

(defun python2 (&optional argprompt)
  "Start an Python2 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "python2"))

(defun python2.7 (&optional argprompt)
  "Start an Python2.7 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "python2.7"))

(defun jython (&optional argprompt)
  "Start an Jython interpreter.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "jython"))

(defun python3.2 (&optional argprompt)
  "Start an Python3.2 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "python3.2"))

(defun python3.3 (&optional argprompt)
  "Start an Python3.3 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "python3.3"))

(defun bpython (&optional argprompt)
  "Start an Bpython interpreter.

Optional \\[universal-argument] prompts for options to pass to the Bpython interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "bpython"))

;; dedicated
(defun python-dedicated (&optional argprompt switch)
  "Start an unique Python interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python")))

(defun ipython-dedicated (&optional argprompt switch)
  "Start an unique IPython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "ipython")))

(defun python3-dedicated (&optional argprompt switch)
  "Start an unique Python3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python3")))

(defun python2-dedicated (&optional argprompt switch)
  "Start an unique Python2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python2")))

(defun python2.7-dedicated (&optional argprompt switch)
  "Start an unique Python2.7 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python2.7")))

(defun jython-dedicated (&optional argprompt switch)
  "Start an unique Jython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "jython")))

(defun python3.2-dedicated (&optional argprompt switch)
  "Start an unique Python3.2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python3.2")))

(defun python3.3-dedicated (&optional argprompt switch)
  "Start an unique Python3.3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python3.3")))

(defun bpython-dedicated (&optional argprompt switch)
  "Start an unique Bpython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Bpython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "bpython")))

;; switch
(defun python-switch (&optional argprompt)
  "Switch to Python interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python")))

(defun ipython-switch (&optional argprompt)
  "Switch to IPython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "ipython")))

(defun python3-switch (&optional argprompt)
  "Switch to Python3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python3")))

(defun python2-switch (&optional argprompt)
  "Switch to Python2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python2")))

(defun python2.7-switch (&optional argprompt)
  "Switch to Python2.7 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python2.7")))

(defun jython-switch (&optional argprompt)
  "Switch to Jython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "jython")))

(defun python3.2-switch (&optional argprompt)
  "Switch to Python3.2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python3.2")))

(defun python3.3-switch (&optional argprompt)
  "Switch to Python3.3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python3.3")))

(defun bpython-switch (&optional argprompt)
  "Switch to Bpython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Bpython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "bpython")))

;; no-switch
(defun python-no-switch (&optional argprompt)
  "Open an Python interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python")))

(defun ipython-no-switch (&optional argprompt)
  "Open an IPython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "ipython")))

(defun python3-no-switch (&optional argprompt)
  "Open an Python3 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python3")))

(defun python2-no-switch (&optional argprompt)
  "Open an Python2 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python2")))

(defun python2.7-no-switch (&optional argprompt)
  "Open an Python2.7 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python2.7")))

(defun jython-no-switch (&optional argprompt)
  "Open an Jython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "jython")))

(defun python3.2-no-switch (&optional argprompt)
  "Open an Python3.2 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python3.2")))

(defun python3.3-no-switch (&optional argprompt)
  "Open an Python3.3 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python3.3")))

(defun bpython-no-switch (&optional argprompt)
  "Open an Bpython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Bpython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "bpython")))

;; dedicated switch
(defalias 'python-dedicated-switch 'python-switch-dedicated)
(defun python-switch-dedicated (&optional argprompt)
  "Switch to an unique Python interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python")))

(defalias 'ipython-dedicated-switch 'ipython-switch-dedicated)
(defun ipython-switch-dedicated (&optional argprompt)
  "Switch to an unique IPython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "ipython")))

(defalias 'python3-dedicated-switch 'python3-switch-dedicated)
(defun python3-switch-dedicated (&optional argprompt)
  "Switch to an unique Python3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python3")))

(defalias 'python2-dedicated-switch 'python2-switch-dedicated)
(defun python2-switch-dedicated (&optional argprompt)
  "Switch to an unique Python2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python2")))

(defalias 'python2.7-dedicated-switch 'python2.7-switch-dedicated)
(defun python2.7-switch-dedicated (&optional argprompt)
  "Switch to an unique Python2.7 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python2.7")))

(defalias 'jython-dedicated-switch 'jython-switch-dedicated)
(defun jython-switch-dedicated (&optional argprompt)
  "Switch to an unique Jython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "jython")))

(defalias 'python3.2-dedicated-switch 'python3.2-switch-dedicated)
(defun python3.2-switch-dedicated (&optional argprompt)
  "Switch to an unique Python3.2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python3.2")))

(defalias 'python3.3-dedicated-switch 'python3.3-switch-dedicated)
(defun python3.3-switch-dedicated (&optional argprompt)
  "Switch to an unique Python3.3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python3.3")))

(defalias 'bpython-dedicated-switch 'bpython-switch-dedicated)
(defun bpython-switch-dedicated (&optional argprompt)
  "Switch to an unique Bpython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Bpython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "bpython")))


;;;
(defalias 'Python 'python)
(defalias 'Python2 'python2)
(defalias 'Python3 'python3)
(defalias 'IPython 'ipython)
(defalias 'Ipython 'ipython)
;;; Code execution
(declare-function compilation-shell-minor-mode "compile" (&optional arg))

(defun py-which-execute-file-command (filename)
  "Return the command appropriate to Python version.

Per default it's \"(format \"execfile(r'%s') # PYTHON-MODE\\n\" filename)\" for Python 2 series."
  (interactive)
  (let* ((erg (py-which-python))
         (cmd (if (< erg 3)
                  (format "execfile(r'%s') # PYTHON-MODE\n" filename)
                (format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" filename filename))))
    (when (interactive-p) (message "%s" (prin1-to-string cmd)))
    cmd))

(defun py-execute-region-no-switch (start end)
  "Send the region to a Python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', buffer with region stays current.
 "
  (interactive "r")
  (let (py-switch-buffers-on-execute-p)
    (py-execute-base start end)))

(defun py-execute-region-switch (start end)
  "Send the region to a Python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to.
"
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p t))
    (py-execute-base start end)))

(defun py-execute-region (start end &optional shell dedicated)
  "Send the region to a Python interpreter.

When called with \\[universal-argument], execution through `default-value' of `py-shell-name' is forced.
When called with \\[universal-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional DEDICATED (boolean)
"
  (interactive "r\nP")
  (save-excursion
    (let ((py-shell-name (cond ((or py-force-py-shell-name-p (eq 4 (prefix-numeric-value shell))) (default-value 'py-shell-name))
                               ((and (numberp shell) (not (eq 1 (prefix-numeric-value shell))))
                                (read-from-minibuffer "(path-to-)shell-name: " (default-value 'py-shell-name)))
                               (t shell)))
          (py-dedicated-process-p (or dedicated py-dedicated-process-p)))
      (py-execute-base start end))))

(defun py-execute-region-default (start end)
  "Send the region to the systems default Python interpreter. "
  (interactive "r")
  (let ((py-dedicated-process-p (default-value 'py-dedicated-process-p))
        (py-shell-name (default-value 'py-shell-name)))
    (py-execute-base start end)))

(defun py-execute-region-dedicated (start end &optional shell)
  "Get the region processed by an unique Python interpreter.

When called with \\[universal-argument], execution through `default-value' of `py-shell-name' is forced.
When called with \\[universal-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument. "
  (interactive "r\nP")
  (let ((py-shell-name (cond ((eq 4 (prefix-numeric-value shell)) (default-value 'py-shell-name))
                             ((and (numberp shell) (not (eq 1 (prefix-numeric-value shell))))
                              (read-from-minibuffer "(path-to-)shell-name: " (default-value 'py-shell-name)))
                             (t shell)))
        (py-dedicated-process-p t))
    (py-execute-base start end)))

(defalias 'py-execute-region-dedicated-default 'py-execute-region-default-dedicated)
(defun py-execute-region-default-dedicated (start end)
  "Send the region to an unique shell of systems default Python. "
  (interactive "r")
  (let ((py-dedicated-process-p t))
    (py-execute-base start end (default-value 'py-shell-name))))

(defun py-delete-temporary (&optional file localname filebuf)
  (when (file-readable-p file)
    (delete-file file))
  (when (buffer-live-p filebuf)
    (set-buffer filebuf)
    (set-buffer-modified-p 'nil)
    (kill-buffer filebuf))
  (when (buffer-live-p localname)
    (kill-buffer localname)))

(defun py-execute-buffer-finally (start end execute-directory)
  (let* ((strg (buffer-substring-no-properties start end))
         (temp (make-temp-name
                (concat (replace-regexp-in-string py-separator-char "-" (replace-regexp-in-string (concat "^" py-separator-char) "" (replace-regexp-in-string ":" "-" py-shell-name))) "-")))
         (tempfile (concat (expand-file-name py-temp-directory) py-separator-char (replace-regexp-in-string py-separator-char "-" temp) ".py"))
         (tempbuf (get-buffer-create temp))
         (wholebuf (when (boundp 'wholebuf) wholebuf))
         erg err-p lineadd output-buffer)
    ;; (message "%s" strg)
    (set-buffer tempbuf)
    (erase-buffer)
    (unless py-if-name-main-permission-p
      (setq strg (replace-regexp-in-string
                  "if[( ]*__name__[) ]*==[( ]*['\"]\\{1,3\\}__main__['\"]\\{1,3\\}[) ]*:"
                  ;; space after __main__, i.e. will not be executed
                  "if __name__ == '__main__ ':" strg)))
    (insert strg)
    (py-fix-start (point-min)(point-max))
    ;; (py-if-needed-insert-shell)
    ;; (unless wholebuf (py-insert-coding))
    ;; (unless (string-match "[jJ]ython" py-shell-name) (py-insert-execute-directory execute-directory))
    ;; fix offline amount, make erorr point at the correct line
    ;; (setq lineadd (- line (+ 2 (count-lines (point-min) (point)))))
    ;; (and (< 0 lineadd) (insert (make-string lineadd 10)))
    (set-buffer tempbuf)
    (write-region (point-min) (point-max) tempfile nil t nil 'ask)
    (set-buffer-modified-p 'nil)
    (unwind-protect
        (py-execute-file-base proc tempfile nil py-buffer-name py-orig-buffer-or-file execute-directory)
      (sit-for 0.1)
      (and py-cleanup-temporary
           (py-delete-temporary tempfile tempbuf)))
    (and py-store-result-p (kill-new erg))
    erg))

(defun py-execute-python-mode-v5 (start end)
  (interactive "r")
  (let ((py-exception-buffer (current-buffer))
        (pcmd (concat py-shell-name (if (string-equal py-which-bufname
                                                      "Jython")
                                        " -"
                                      ;; " -c "
                                      ""))))
    (save-excursion
      (shell-command-on-region start end
                               pcmd py-output-buffer))
    (if (not (get-buffer py-output-buffer))
        (message "No output.")

      (let* ((err-p (py-postprocess-output-buffer py-output-buffer))
             (line (cadr err-p)))
        (if err-p
            (when (and py-jump-on-exception line)
              (pop-to-buffer py-exception-buffer))
          (pop-to-buffer py-output-buffer)
          (goto-char (point-max))
          (copy-marker (point)))))))

(defun py-execute-ge24.3 (start end file execute-directory)
  "Select the handler. "
  (and (buffer-file-name) buffer-offer-save (buffer-modified-p) (y-or-n-p "Save buffer before executing? ")
       (write-file (buffer-file-name)))
  (let* ((start (copy-marker start))
         (end (copy-marker end))
         (py-exception-buffer (current-buffer))
         (line (count-lines (point-min) start))
         (strg (buffer-substring-no-properties start end))
         (tempfile (or (buffer-file-name) (concat (expand-file-name py-temp-directory) py-separator-char (replace-regexp-in-string py-separator-char "-" "temp") ".py")))

         (proc (if py-dedicated-process-p
                   (get-buffer-process (py-shell nil py-dedicated-process-p py-shell-name py-buffer-name t))
                 (or (get-buffer-process py-buffer-name)
                     (get-buffer-process (py-shell nil py-dedicated-process-p py-shell-name py-buffer-name t)))))
         (procbuf (process-buffer proc))
         (file (or file (with-current-buffer py-buffer-name
                          (concat (file-remote-p default-directory) tempfile))))
         (filebuf (get-buffer-create file))
         err-p)
    (set-buffer filebuf)
    (erase-buffer)
    (newline line)
    (save-excursion
      (insert strg))
    (py-fix-start (point) (point-max))
    (unless (string-match "[jJ]ython" py-shell-name)
      (when (and execute-directory py-use-current-dir-when-execute-p
                 (not (string= execute-directory default-directory)))
        (message "Warning: options `execute-directory' and `py-use-current-dir-when-execute-p' may conflict"))
      (and execute-directory
           (process-send-string proc (concat "import os; os.chdir(\"" execute-directory "\")\n"))
           ;; (py-send-string-no-output (concat "import os; os.chdir(\"" execute-directory "\")\n") proc)
           ))
    (set-buffer filebuf)
    (process-send-string proc
                         (buffer-substring-no-properties
                          (point-min) (point-max)))
    (sit-for 0.1)
    (if (and (setq err-p (save-excursion (py-postprocess-output-buffer procbuf)))
             (car err-p)
             (not (markerp err-p)))
        (py-jump-to-exception err-p)
      (py-shell-manage-windows py-exception-buffer py-buffer-name)
      (unless (string= (buffer-name (current-buffer)) (buffer-name procbuf))
        (when py-verbose-p (message "Output buffer: %s" procbuf))))))

(defun py-execute-base (&optional start end shell filename proc file)
  "Select the handler.

When optional FILE is `t', no temporary file is needed. "
  (let* ((windows-config (window-configuration-to-register 313465889))
         (origline
          (save-restriction
            (widen)
            (count-lines
             (point-min)
             ;; count-lines doesn't honor current line when at BOL
             (or (and (eq start (line-beginning-position)) (not (eobp)) (1+ start)) start))))
         (py-shell-name (or shell (py-choose-shell)))
         (py-exception-buffer (current-buffer))
         (execute-directory
          (cond ((ignore-errors (file-name-directory (file-remote-p (buffer-file-name) 'localname))))
                ((and py-use-current-dir-when-execute-p (buffer-file-name))
                 (file-name-directory (buffer-file-name)))
                ((and py-use-current-dir-when-execute-p
                      py-fileless-buffer-use-default-directory-p)
                 (expand-file-name default-directory))
                ((stringp py-execute-directory)
                 py-execute-directory)
                ((getenv "VIRTUAL_ENV"))
                (t (getenv "HOME"))))
         (py-buffer-name (or py-buffer-name (py-buffer-name-prepare)))
         (filename (and filename (expand-file-name filename)))
         (py-orig-buffer-or-file (or filename (current-buffer)))
         (proc (or proc (if py-dedicated-process-p
                            (get-buffer-process (py-shell nil py-dedicated-process-p py-shell-name py-buffer-name t))
                          (or (and (boundp 'py-buffer-name) (get-buffer-process py-buffer-name))
                              (get-buffer-process (py-shell nil py-dedicated-process-p py-shell-name (and (boundp 'py-buffer-name) py-buffer-name) t))))))
         err-p)
    (set-buffer py-exception-buffer)
    (py-update-execute-directory proc py-buffer-name execute-directory)
    (cond (;; enforce proceeding as python-mode.el v5
           python-mode-v5-behavior-p
           (py-execute-python-mode-v5 start end))
          (py-execute-no-temp-p
           (py-execute-ge24.3 start end filename execute-directory))
          ;; No need for a temporary filename than
          ((or file (and (not (buffer-modified-p)) filename))
           (py-execute-file-base proc filename nil py-buffer-name filename execute-directory))
          (t
           ;; (message "%s" (current-buffer))
           (py-execute-buffer-finally start end execute-directory)))))

(defun py-execute-string (&optional string shell)
  "Send the argument STRING to a Python interpreter.

See also `py-execute-region'. "
  (interactive)
  (let ((string (or string (read-from-minibuffer "String: ")))
        (shell (or shell (default-value 'py-shell-name))))
    (with-temp-buffer
      (insert string)
      (py-execute-region (point-min) (point-max) shell))))

(defun py-execute-string-dedicated (&optional string shell)
  "Send the argument STRING to an unique Python interpreter.

See also `py-execute-region'. "
  (interactive)
  (let ((string (or string (read-from-minibuffer "String: ")))
        (shell (or shell (default-value 'py-shell-name)))
        (py-dedicated-process-p t))
    (with-temp-buffer
      (insert string)
      (py-execute-region (point-min) (point-max) shell))))

(defun py-if-needed-insert-shell ()
  (let ((erg (or (py-choose-shell-by-shebang)
                 (py-choose-shell-by-import)
                 py-shell-name)))
    (when (string-match " " erg) (setq erg (substring erg (1+ (string-match " " erg))))
          ;; closing ">"
          (setq erg (substring erg 0 (1- (length erg)))))
    (goto-char (point-min))
    (while (empty-line-p) (delete-region (point) (1+ (line-end-position))))
    (unless (looking-at py-shebang-regexp)
      (if (string-match (concat "^" erg) "ipython")
          (progn
            (shell-command "type ipython" t)
            (when (looking-at "[^/\n\r]+")
              (replace-match "#! ")))
        (if (string-match py-separator-char erg)
            (insert (concat "#! " erg "\n"))
          (insert (concat py-shebang-startstring " " erg "\n")))))))

(defun py-insert-execute-directory (directory &optional orig done)
  (let ((orig (or orig (point)))
        (done done))
    (if done (goto-char done) (goto-char (point-min)))
    (cond ((re-search-forward "^from __future__ import " nil t 1)
           (py-end-of-statement)
           (setq done (point))
           (py-insert-execute-directory directory orig done))
          ((re-search-forward py-encoding-string-re nil t 1)
           (setq done (point))
           (py-insert-execute-directory directory orig done))
          ((re-search-forward py-shebang-regexp nil t 1)
           (setq done (point))
           (py-insert-execute-directory directory orig done))
          (t (forward-line 1)
             (unless (empty-line-p) (newline))
             (insert (concat "import os; os.chdir(\"" directory "\")\n"))))))

(defun py-insert-coding ()
  (goto-char (point-min))
  (unless (re-search-forward py-encoding-string-re nil t)
    (goto-char (point-min))
    (if (re-search-forward py-shebang-regexp nil t 1)
        (progn
          (newline)
          (insert (concat py-encoding-string "\n")))
      (insert (concat py-encoding-string "\n")))))

(defun py-if-needed-insert-if ()
  "Internal use by py-execute... functions.
Inserts an incentive true form \"if 1:\\n.\" "
  (let ((needs-if (/= (py-point 'bol) (py-point 'boi))))
    (when needs-if
      (insert "if 1:\n")
      (setq py-line-number-offset (- py-line-number-offset 1)))))

(defun py-fix-start (start end)
  "Internal use by py-execute... functions.
Avoid empty lines at the beginning. "
  (python-mode)
  (goto-char start)
  (while (empty-line-p)
    (delete-region (line-beginning-position) (1+ (line-end-position))))
  (back-to-indentation)
  (unless (py-beginning-of-statement-p)
    (py-down-statement))
  (while (not (eq (current-indentation) 0))
    (py-shift-left py-indent-offset start end))
  (goto-char (point-max))
  (unless (empty-line-p)
    (newline)))

(defun py-fetch-py-master-file ()
  "Lookup if a `py-master-file' is specified.

See also doku of variable `py-master-file' "
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^ *# Local Variables:" nil (quote move) 1)
        (when
            (re-search-forward (concat "^\\( *# py-master-file: *\\)\"\\([^ \t]+\\)\" *$") nil t 1)
          (setq py-master-file (match-string-no-properties 2))))))
  (when (and py-verbose-p (interactive-p)) (message "%s" py-master-file)))

(defun py-execute-import-or-reload (&optional argprompt shell)
  "Import the current buffer's file in a Python interpreter.

If the file has already been imported, then do reload instead to get
the latest version.

If the file's name does not end in \".py\", then do execfile instead.

If the current buffer is not visiting a file, do `py-execute-buffer'
instead.

If the file local variable `py-master-file' is non-nil, import or
reload the named file instead of the buffer's file.  The file may be
saved based on the value of `py-execute-import-or-reload-save-p'.

See also `\\[py-execute-region]'.

This may be preferable to `\\[py-execute-buffer]' because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE.NAME).

 - The Python debugger gets line number information about the functions."
  (interactive "P")
  ;; Check file local variable py-master-file
  (when py-master-file
    (let* ((filename (expand-file-name py-master-file))
           (buffer (or (get-file-buffer filename)
                       (find-file-noselect filename))))
      (set-buffer buffer)))
  (let ((py-shell-name (or shell (py-choose-shell argprompt shell)))
        (file (buffer-file-name (current-buffer))))
    (if file
        (let ((proc (or
                     (ignore-errors (get-process (file-name-directory shell)))
                     (get-buffer-process (py-shell argprompt py-dedicated-process-p shell (or shell (default-value 'py-shell-name)))))))
          ;; Maybe save some buffers
          (save-some-buffers (not py-ask-about-save) nil)
          (py-execute-file-base proc file
                                (if (string-match "\\.py$" file)
                                    (let ((m (py-qualified-module-name (expand-file-name file))))
                                      (if (string-match "python2" (file-name-nondirectory shell))
                                          (format "import sys\nif sys.modules.has_key('%s'):\n reload(%s)\nelse:\n import %s\n" m m m)
                                        (format "import sys,imp\nif'%s' in sys.modules:\n imp.reload(%s)\nelse:\n import %s\n" m m m)))
                                  ;; (format "execfile(r'%s')\n" file)
                                  (py-which-execute-file-command file))))
      (py-execute-buffer))))

(defun py-qualified-module-name (file)
  "Find the qualified module name for filename FILE.

Basically, this goes down the directory tree as long as there are __init__.py files there."
  (let ((rec #'(lambda (d f)
                 (let* ((dir (file-name-directory d))
                        (initpy (concat dir "__init__.py")))
                   (if (file-exists-p initpy)
                       (let ((d2 (directory-file-name d)))
                         (funcall rec (file-name-directory d2)
                                  (concat (file-name-nondirectory d2) "." f)))
                     f)))))
    (funcall rec (file-name-directory file)
             (file-name-sans-extension (file-name-nondirectory file)))))

(defun py-execute-buffer-dedicated ()
  "Send the contents of the buffer to a unique Python interpreter. "
  (interactive)
  (let ((py-dedicated-process-p t))
    (py-execute-buffer-base)))

(defun py-execute-buffer-switch ()
  "Send the contents of the buffer to a Python interpreter and switches to output. "
  (interactive)
  (let ((py-switch-buffers-on-execute-p t))
    (py-execute-buffer-base)))

(defalias 'py-execute-buffer-switch-dedicated 'py-execute-buffer-dedicated-switch)
(defun py-execute-buffer-dedicated-switch ()
  "Send the contents of the buffer to an unique Python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p'. "
  (interactive)
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-execute-buffer-base)))

(defun py-execute-buffer ()
  "Send the contents of the buffer to a Python interpreter. "
  (interactive)
  (if (and py-prompt-on-changed-p (buffer-file-name) (interactive-p) (buffer-modified-p))
      (if (y-or-n-p "Buffer changed, save first? ")
          (progn
            (write-file (buffer-file-name))
            (py-execute-buffer-base))
        (py-execute-region (point-min) (point-max)))
    (if (buffer-file-name)
        (py-execute-buffer-base)
      (py-execute-region (point-min) (point-max)))))

(defun py-execute-buffer-base ()
  "Honor `py-master-file'. "
  (let* ((py-master-file (or py-master-file (py-fetch-py-master-file)))
         (file
          (if py-master-file
              (expand-file-name py-master-file)
            (buffer-file-name))))
    (py-execute-file file)))

(defun py-execute-buffer-no-switch ()
  "Send the contents of the buffer to a Python interpreter but don't switch to output. "
  (interactive)
  (let (py-switch-buffers-on-execute-p)
    (py-execute-buffer-base)))

;; Fixme: Try to define the function or class within the relevant
;; module, not just at top level.
(defun py-execute-defun ()
  "Send the current defun (class or method) to the inferior Python process."
  (interactive)
  (save-excursion (py-execute-region (progn (beginning-of-defun) (point))
                                     (progn (end-of-defun) (point)))))

(defun py-process-file (filename &optional output-buffer error-buffer)
  "Process \"python filename\".

Optional OUTPUT-BUFFER and ERROR-BUFFER might be given. "
  (interactive "fDatei:")
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (output-buffer (or output-buffer (make-temp-name "py-process-file-output")))
        (pcmd (py-choose-shell)))
    (unless (buffer-live-p output-buffer)
      (set-buffer (get-buffer-create output-buffer)))
    (shell-command (concat pcmd " " filename) output-buffer error-buffer)
    (when (interactive-p) (switch-to-buffer output-buffer))))

;;;
(defun py-execute-line ()
  "Send current line from beginning of indent to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (progn (back-to-indentation)
                      (point))))
      (py-execute-region beg (line-end-position)))))

(defun py-output-filter (string)
  "Clear output buffer from py-shell-input prompt etc. "
  (interactive "*")
  (replace-regexp-in-string
   (concat "\\(\n\\|" py-shell-input-prompt-1-regexp "\\|" py-shell-input-prompt-2-regexp "\\|" "^In \\[[0-9]+\\]: *" "\\)") "" string))

(defun py-execute-file (file)
  "When called interactively, user is prompted for filename. "
  (interactive "fFilename: ")
  (let (erg)
    (if (file-readable-p file)
        (setq erg (py-execute-base nil nil nil file nil (or (and (boundp 'py-orig-buffer-or-file) py-orig-buffer-or-file) file)))
      (message "%s not readable. %s" file "Do you have write permissions?"))
    erg))

(defun py-update-separator-char ()
  "Return the file-path separator char from current machine.

When `py-separator-char' is customized, its taken.
Returns char found. "
  (let ((erg (cond ((characterp py-separator-char)
                    (char-to-string py-separator-char))
                   ;; epd hack
                   ((and
                     (string-match "[Ii][Pp]ython" py-shell-name)
                     (string-match "epd\\|EPD" py-shell-name))
                    (replace-regexp-in-string "\n" ""
                                              (shell-command-to-string (concat py-shell-name " -c \"import os; print(os.sep)\"")))))))
    (if (and erg (string-match "^$" erg))
        (setq erg (substring erg (string-match "^$" erg)))
      (setq erg (replace-regexp-in-string "\n" "" (shell-command-to-string (concat py-shell-name " -W ignore" " -c \"import os; print(os.sep)\"")))))
    erg))

(defun py-current-working-directory (&optional shell)
  "Return the directory of current `py-shell'."
  (replace-regexp-in-string "\n" "" (shell-command-to-string (concat (or shell py-shell-name) " -c \"import os; print(os.getcwd())\""))))

(defun py-update-execute-directory-intern (dir proc)
  (comint-send-string proc (concat "import os;os.chdir(\"" dir "\")\n")))

(defun py-update-execute-directory (proc procbuf execute-directory)
  (let ((oldbuf (current-buffer))
        orig cwd)
    (set-buffer procbuf)
    (setq cwd (py-current-working-directory))
    (setq orig (point))
    (unless (string= execute-directory (concat cwd "/"))
      (py-update-execute-directory-intern (or py-execute-directory execute-directory) proc)
      (delete-region orig (point-max)))
    (set-buffer oldbuf)))

(defun py-execute-file-base (&optional proc filename cmd procbuf origfile execute-directory)
  "Send to Python interpreter process PROC, in Python version 2.. \"execfile('FILENAME')\".

Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
`kill-output-from-shell' does The Right Thing.
Returns position where output starts. "
  (let* ((cmd (or cmd (format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" filename filename)))
         (msg (and py-verbose-p (format "## executing %s...\n" (or origfile filename))))
         erg orig err-p)
    (set-buffer procbuf)
    (setq orig (point))
    (comint-send-string proc cmd)
    (if
        (setq err-p (save-excursion (py-postprocess-output-buffer procbuf)))
        (py-shell-manage-windows py-buffer-name nil windows-config)
      (setq erg
            (py-output-filter
             (buffer-substring-no-properties orig (point))))
      (py-shell-manage-windows (current-buffer) nil windows-config)
      erg)))

;;; Pdb
;; Autoloaded.
(declare-function compilation-shell-minor-mode "compile" (&optional arg))

(defun py-pdbtrack-overlay-arrow (activation)
  "Activate or de arrow at beginning-of-line in current buffer."
  ;; This was derived/simplified from edebug-overlay-arrow
  (cond (activation
         (setq overlay-arrow-position (make-marker))
         (setq overlay-arrow-string "=>")
         (set-marker overlay-arrow-position (line-beginning-position) (current-buffer))
         (setq py-pdbtrack-is-tracking-p t))
        (overlay-arrow-position
         (setq overlay-arrow-position nil)
         (setq py-pdbtrack-is-tracking-p nil))))

(defun py-pdbtrack-track-stack-file (text)
  "Show the file indicated by the pdb stack entry line, in a separate window.

Activity is disabled if the buffer-local variable
`py-pdbtrack-do-tracking-p' is nil.

We depend on the pdb input prompt matching `py-pdbtrack-input-prompt'
at the beginning of the line.

If the traceback target file path is invalid, we look for the most
recently visited python-mode buffer which either has the name of the
current function \(or class) or which defines the function \(or
class).  This is to provide for remote scripts, eg, Zope's 'Script
\(Python)' - put a _copy_ of the script in a buffer named for the
script, and set to python-mode, and pdbtrack will find it.)"
  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next pdb prompt, and then
  ;; check all text from comint-last-input-end to process-mark.
  ;;
  ;; Also, we're very conservative about clearing the overlay arrow,
  ;; to minimize residue.  This means, for instance, that executing
  ;; other pdb commands wipe out the highlight.  You can always do a
  ;; 'where' (aka 'w') command to reveal the overlay arrow.
  (let* ((origbuf (current-buffer))
         (currproc (get-buffer-process origbuf)))

    (if (not (and currproc py-pdbtrack-do-tracking-p))
        (py-pdbtrack-overlay-arrow nil)

      (let* ((procmark (process-mark currproc))
             (block (buffer-substring (max comint-last-input-end
                                           (- procmark
                                              py-pdbtrack-track-range))
                                      procmark))
             target target_fname target_lineno target_buffer)

        (if (not (string-match (concat py-pdbtrack-input-prompt "$") block))
            (py-pdbtrack-overlay-arrow nil)

          (setq target (py-pdbtrack-get-source-buffer block))

          (if (stringp target)
              (message "pdbtrack: %s" target)

            (setq target_lineno (car target))
            (setq target_buffer (cadr target))
            (setq target_fname (buffer-file-name target_buffer))
            (switch-to-buffer-other-window target_buffer)
            (goto-char (point-min))
            (forward-line (1- target_lineno))
            (message "pdbtrack: line %s, file %s" target_lineno target_fname)
            (py-pdbtrack-overlay-arrow t)
            (pop-to-buffer origbuf t)))))))

(defun py-pdbtrack-map-filename (filename)

  (let
      ((replacement-val (assoc-default
                         filename py-pdbtrack-filename-mapping
                         (lambda (mapkey path)
                           (string-match
                            (concat "^" (regexp-quote mapkey))
                            path)))
                        ))
    (if (not (eq replacement-val nil))
        (replace-match replacement-val 't 't filename)
      filename)))

(defun py-pdbtrack-get-source-buffer (block)
  "Return line number and buffer of code indicated by block's traceback text.

We look first to visit the file indicated in the trace.

Failing that, we look for the most recently visited python-mode buffer
with the same name or having the named function.

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (if (and (not (string-match py-pdbtrack-stack-entry-regexp block))
           ;; pydb integration still to be done
	   ;; (not (string-match py-pydbtrack-stack-entry-regexp block))
           )
      "Traceback cue not found"
    (let* ((filename (match-string
		      py-pdbtrack-marker-regexp-file-group block))
           (lineno (string-to-number (match-string
                                      py-pdbtrack-marker-regexp-line-group
                                      block)))
           (funcname (match-string py-pdbtrack-marker-regexp-funcname-group
				   block))
           funcbuffer)

      (cond ((file-exists-p filename)
             (list lineno (find-file-noselect filename)))

            ((file-exists-p (py-pdbtrack-map-filename filename))
             (list lineno (find-file-noselect (py-pdbtrack-map-filename filename))))

            ((setq funcbuffer (py-pdbtrack-grub-for-buffer funcname lineno))
             (if (string-match "/Script (Python)$" filename)
                 ;; Add in number of lines for leading '##' comments:
                 (setq lineno
                       (+ lineno
                          (save-excursion
                            (set-buffer funcbuffer)
                            (count-lines
                             (point-min)
                             (max (point-min)
                                  (string-match "^\\([^#]\\|#[^#]\\|#$\\)"
                                                (buffer-substring (point-min)
                                                                  (point-max)))))))))
             (list lineno funcbuffer))

            ((= (elt filename 0) ?\<)
             (format "(Non-file source: '%s')" filename))

            (t (format "Not found: %s(), %s" funcname filename))))))

(defun py-pdbtrack-grub-for-buffer (funcname lineno)
  "Find most recent buffer itself named or having function funcname.

We walk the buffer-list history for python-mode buffers that are
named for funcname or define a function funcname."
  (let ((buffers (buffer-list))
        buf
        got)
    (while (and buffers (not got))
      (setq buf (car buffers)
            buffers (cdr buffers))
      (if (and (save-excursion (set-buffer buf)
                               (string= major-mode "python-mode"))
               (or (string-match funcname (buffer-name buf))
                   (string-match (concat "^\\s-*\\(def\\|class\\)\\s-+"
                                         funcname "\\s-*(")
                                 (save-excursion
                                   (set-buffer buf)
                                   (buffer-substring (point-min)
                                                     (point-max))))))
          (setq got buf)))
    got))


;; pdbtrack functions
(defun py-pdbtrack-toggle-stack-tracking (arg)
  "Set variable `py-pdbtrack-do-tracking-p'. "
  (interactive "P")
  (if (not (get-buffer-process (current-buffer)))
      (error "No process associated with buffer '%s'" (current-buffer)))
  ;; missing or 0 is toggle, >0 turn on, <0 turn off
  (if (or (not arg)
          (zerop (setq arg (prefix-numeric-value arg))))
      (setq py-pdbtrack-do-tracking-p (not py-pdbtrack-do-tracking-p))
    (setq py-pdbtrack-do-tracking-p (> arg 0)))
  (message "%sabled Python's pdbtrack"
           (if py-pdbtrack-do-tracking-p "En" "Dis")))

(defun turn-on-pdbtrack ()
  (interactive)
  (py-pdbtrack-toggle-stack-tracking 1))

(defun turn-off-pdbtrack ()
  (interactive)
  (py-pdbtrack-toggle-stack-tracking 0))

;;; Documentation
(defun py-documentation (w)
  "Launch PyDOC on the Word at Point"
  (interactive
   (list (let* ((word (thing-at-point 'word))
                (input (read-string
                        (format "pydoc entry%s: "
                                (if (not word) "" (format " (default %s)" word))))))
           (if (string= input "")
               (if (not word) (error "No pydoc args given")
                 word) ;sinon word
             input)))) ;sinon input
  (shell-command (concat py-shell-name " -c \"from pydoc import help;help(\'" w "\')\"") "*PYDOCS*")
  (view-buffer-other-window "*PYDOCS*" t 'kill-buffer-and-window))

(defun py-fetch-docu ()
  "Lookup in current buffer for the doku for the symbol at point.

Useful for newly defined symbol, not known to python yet. "
  (interactive)
  (let* ((symb (prin1-to-string (symbol-at-point)))
         (args (py-expression))
         erg)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward (concat py-def-or-class-re " *" symb) nil (quote move) 1)
        (forward-line 1)
        (when (looking-at "[ \t]*\"\"\"\\|[ \t]*'''\\|[ \t]*'[^]+\\|[ \t]*\"[^\"]+")
          (goto-char (match-end 0))
          (setq erg (buffer-substring-no-properties (match-beginning 0) (re-search-forward "\"\"\"\\|'''" nil 'move)))
          (when erg
            (set-buffer (get-buffer-create "*Python-Help*"))
            (erase-buffer)
            (when (and py-verbose-p (interactive-p)) (switch-to-buffer (current-buffer)))
            (insert erg)))))))

(defun py-find-imports ()
  "Find top-level imports.

Returns imports "
  (interactive)
  (let (imports)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^import *[A-Za-z_][A-Za-z_0-9].*\\|^from +[A-Za-z_][A-Za-z_0-9.]+ +import .*" nil t)
        (unless (py-end-of-statement-p)
          (py-end-of-statement))
        (setq imports
              (concat
               imports
               (replace-regexp-in-string
                "[\\]\r?\n?\s*" ""
                (buffer-substring-no-properties (match-beginning 0) (point))) ";"))))
    ;; (and imports
    ;; (setq imports (replace-regexp-in-string ";$" "" imports)))
    (when (and py-verbose-p (interactive-p)) (message "%s" imports))
    imports))

(defun py-eldoc-function ()
  "Print help on symbol at point. "
  (interactive)
  (if (unless (looking-at " ")
        (or

         (eq (get-char-property (point) 'face) 'font-lock-keyword-face)
         (eq (get-char-property (point) 'face) 'py-builtins-face)
         (eq (get-char-property (point) 'face) 'py-exception-name-face)
         (eq (get-char-property (point) 'face) 'py-class-name-face)

         ))

      (lexical-let* ((sym (prin1-to-string (symbol-at-point)))
                     (origfile (buffer-file-name))
                     (temp (make-temp-name (buffer-name)))
                     (file (concat (expand-file-name temp py-temp-directory) ".py"))
                     (cmd (py-find-imports))
                     (no-quotes (save-excursion
                                  (skip-chars-backward "A-Za-z_0-9.")
                                  (and (looking-at "[A-Za-z_0-9.]+")
                                       (string-match "\\." (match-string-no-properties 0))))))
        (setq cmd (concat "import pydoc\n"
                          cmd))
        (if no-quotes
            (setq cmd (concat cmd
                              "try: pydoc.help(" sym ")\n"))
          (setq cmd (concat cmd "try: pydoc.help('" sym "')\n")))
        (setq cmd (concat cmd
                          "except:
    print 'No help available on:', \"" sym "\""))
        (with-temp-buffer
          (insert cmd)
          (write-file file))
        (py-process-file file "*Python-Help*")
        (when (file-readable-p file)
          (delete-file file)))
    (delete-other-windows)))

(defalias 'py-help-at-point 'py-describe-symbol)
(defun py-describe-symbol (&optional debug)
  "Print help on symbol at point.

If symbol is defined in current buffer, jump to it's definition
Optional \\[universal-argument] used for debugging, will prevent deletion of temp file. "
  (interactive "P")
  (let* ((orig (point))
         (beg (progn (when (and (looking-back "(")(not (looking-at "\\sw"))) (forward-char -1)) (skip-chars-backward "a-zA-Z0-9_." (line-beginning-position))(point)))
         (end (progn (skip-chars-forward "a-zA-Z0-9_." (line-end-position))(point)))
         (sym (buffer-substring-no-properties beg end))
         (origfile (buffer-file-name))
         (temp (make-temp-name (buffer-name)))
         (file (concat (expand-file-name temp py-temp-directory) ".py"))
         (cmd (py-find-imports))
         ;; if symbol is defined in current buffer, go to
         (erg (progn (goto-char (point-min))
                     (when
                         (re-search-forward (concat "^[ \t]*def " sym "(") nil t 1)
                       (forward-char -2)
                       (point)))))
    (if erg
        (progn (push-mark orig)(push-mark (point))
               (when (and (interactive-p) py-verbose-p) (message "Jump to previous position with %s" "C-u C-<SPC> C-u C-<SPC>")))
      (goto-char orig)
      (when cmd
        (setq cmd (mapconcat
                   (lambda (arg) (concat "try: " arg "\nexcept: pass\n"))
                   (split-string cmd ";" t)
                   "")))
      (setq cmd (concat "import pydoc\n"
                        cmd))
      (when (not py-remove-cwd-from-path)
        (setq cmd (concat cmd "import sys\n"
                          "sys.path.insert(0, '"
                          (file-name-directory origfile) "')\n")))
      (setq cmd (concat cmd "pydoc.help('" sym "')\n"))
      (with-temp-buffer
        (insert cmd)
        (write-file file))
      (setq erg (py-process-file file "*Python-Help*"))
      (message "%s" erg)
      (when (file-readable-p file)
        (unless (eq 4 (prefix-numeric-value debug)) (delete-file file))))))


(defun py-describe-mode ()
  "Dump long form of `python-mode' docs."
  (interactive)
  (py-dump-help-string "Major mode for editing Python files.
Knows about Python indentation, tokens, comments and continuation lines.
Paragraphs are separated by blank lines only.

Major sections below begin with the string `@'; specific function and
variable docs begin with `->'.

@EXECUTING PYTHON CODE

\\[py-execute-import-or-reload]\timports or reloads the file in the Python interpreter
\\[py-execute-buffer]\tsends the entire buffer to the Python interpreter
\\[py-execute-region]\tsends the current region
\\[py-execute-def-or-class]\tsends the current function or class definition
\\[py-execute-string]\tsends an arbitrary string
\\[py-shell]\tstarts a Python interpreter window; this will be used by
\tsubsequent Python execution commands
%c:py-execute-import-or-reload
%c:py-execute-buffer
%c:py-execute-region
%c:py-execute-def-or-class
%c:py-execute-string
%c:py-shell

@VARIABLES

py-install-directory\twherefrom `python-mode' looks for extensions
py-indent-offset\tindentation increment
py-block-comment-prefix\tcomment string used by comment-region

py-shell-name\tshell command to invoke Python interpreter
py-temp-directory\tdirectory used for temp files (if needed)

py-beep-if-tab-change\tring the bell if tab-width is changed
%v:py-install-directory
%v:py-indent-offset
%v:py-block-comment-prefix
%v:py-shell-name
%v:py-temp-directory
%v:py-beep-if-tab-change

@KINDS OF LINES

Each physical line in the file is either a `continuation line' (the
preceding line ends with a backslash that's not part of a comment, or
the paren/bracket/brace nesting level at the start of the line is
non-zero, or both) or an `initial line' (everything else).

An initial line is in turn a `blank line' (contains nothing except
possibly blanks or tabs), a `comment line' (leftmost non-blank
character is `#'), or a `code line' (everything else).

Comment Lines

Although all comment lines are treated alike by Python, Python mode
recognizes two kinds that act differently with respect to indentation.

An `indenting comment line' is a comment line with a blank, tab or
nothing after the initial `#'.  The indentation commands (see below)
treat these exactly as if they were code lines: a line following an
indenting comment line will be indented like the comment line.  All
other comment lines (those with a non-whitespace character immediately
following the initial `#') are `non-indenting comment lines', and
their indentation is ignored by the indentation commands.

Indenting comment lines are by far the usual case, and should be used
whenever possible.  Non-indenting comment lines are useful in cases
like these:

\ta = b # a very wordy single-line comment that ends up being
\t #... continued onto another line

\tif a == b:
##\t\tprint 'panic!' # old code we've `commented out'
\t\treturn a

Since the `#...' and `##' comment lines have a non-whitespace
character following the initial `#', Python mode ignores them when
computing the proper indentation for the next line.

Continuation Lines and Statements

The `python-mode' commands generally work on statements instead of on
individual lines, where a `statement' is a comment or blank line, or a
code line and all of its following continuation lines (if any)
considered as a single logical unit.  The commands in this mode
generally (when it makes sense) automatically move to the start of the
statement containing point, even if point happens to be in the middle
of some continuation line.

@INDENTATION

Primarily for entering new code:
\t\\[indent-for-tab-command]\t indent line appropriately
\t\\[py-newline-and-indent]\t insert newline, then indent
\t\\[py-electric-backspace]\t reduce indentation, or delete single character

Primarily for reindenting existing code:
\t\\[py-guess-indent-offset]\t guess py-indent-offset from file content; change locally
\t\\[universal-argument] \\[py-guess-indent-offset]\t ditto, but change globally

\t\\[py-indent-region]\t reindent region to match its context
\t\\[py-shift-left]\t shift line or region left by py-indent-offset
\t\\[py-shift-right]\t shift line or region right by py-indent-offset

Unlike most programming languages, Python uses indentation, and only
indentation, to specify block structure.  Hence the indentation supplied
automatically by `python-mode' is just an educated guess:  only you know
the block structure you intend, so only you can supply correct
indentation.

The \\[indent-for-tab-command] and \\[py-newline-and-indent] keys try to suggest plausible indentation, based on
the indentation of preceding statements.  E.g., assuming
py-indent-offset is 4, after you enter
\tif a > 0: \\[py-newline-and-indent]
the cursor will be moved to the position of the `_' (_ is not a
character in the file, it's just used here to indicate the location of
the cursor):
\tif a > 0:
\t _
If you then enter `c = d' \\[py-newline-and-indent], the cursor will move
to
\tif a > 0:
\t c = d
\t _
`python-mode' cannot know whether that's what you intended, or whether
\tif a > 0:
\t c = d
\t_
was your intent.  In general, `python-mode' either reproduces the
indentation of the (closest code or indenting-comment) preceding
statement, or adds an extra py-indent-offset blanks if the preceding
statement has `:' as its last significant (non-whitespace and non-
comment) character.  If the suggested indentation is too much, use
\\[py-electric-backspace] to reduce it.

Continuation lines are given extra indentation.  If you don't like the
suggested indentation, change it to something you do like, and Python-
mode will strive to indent later lines of the statement in the same way.

If a line is a continuation line by virtue of being in an unclosed
paren/bracket/brace structure (`list', for short), the suggested
indentation depends on whether the current line contains the first item
in the list.  If it does, it's indented py-indent-offset columns beyond
the indentation of the line containing the open bracket.  If you don't
like that, change it by hand.  The remaining items in the list will mimic
whatever indentation you give to the first item.

If a line is a continuation line because the line preceding it ends with
a backslash, the third and following lines of the statement inherit their
indentation from the line preceding them.  The indentation of the second
line in the statement depends on the form of the first (base) line:  if
the base line is an assignment statement with anything more interesting
than the backslash following the leftmost assigning `=', the second line
is indented two columns beyond that `='.  Else it's indented to two
columns beyond the leftmost solid chunk of non-whitespace characters on
the base line.

Warning:  indent-region should not normally be used!  It calls \\[indent-for-tab-command]
repeatedly, and as explained above, \\[indent-for-tab-command] can't guess the block
structure you intend.
%c:indent-for-tab-command
%c:py-newline-and-indent
%c:py-electric-backspace

The next function may be handy when editing code you didn't write:
%c:py-guess-indent-offset

The remaining `indent' functions apply to a region of Python code.  They
assume the block structure (equals indentation, in Python) of the region
is correct, and alter the indentation in various ways while preserving
the block structure:
%c:py-indent-region
%c:py-shift-left
%c:py-shift-right

@MARKING & MANIPULATING REGIONS OF CODE

\\[py-mark-block]\t mark block of lines
\\[py-mark-def-or-class]\t mark smallest enclosing def
\\[universal-argument] \\[py-mark-def-or-class]\t mark smallest enclosing class
\\[comment-region]\t comment out region of code
\\[universal-argument] \\[comment-region]\t uncomment region of code
%c:py-mark-block
%c:py-mark-def-or-class
%c:comment-region

@MOVING POINT

\\[py-previous-statement]\t move to statement preceding point
\\[py-next-statement]\t move to statement following point
\\[py-goto-block-up]\t move up to start of current block
\\[py-beginning-of-def-or-class]\t move to start of def
\\[universal-argument] \\[py-beginning-of-def-or-class]\t move to start of class
\\[py-end-of-def-or-class]\t move to end of def
\\[universal-argument] \\[py-end-of-def-or-class]\t move to end of class

The first two move to one statement beyond the statement that contains
point.  A numeric prefix argument tells them to move that many
statements instead.  Blank lines, comment lines, and continuation lines
do not count as `statements' for these commands.  So, e.g., you can go
to the first code statement in a file by entering
\t\\[beginning-of-buffer]\t to move to the top of the file
\t\\[py-next-statement]\t to skip over initial comments and blank lines
Or do `\\[py-previous-statement]' with a huge prefix argument.
%c:py-previous-statement
%c:py-next-statement
%c:py-goto-block-up
%c:py-beginning-of-def-or-class
%c:py-end-of-def-or-class

@LITTLE-KNOWN EMACS COMMANDS PARTICULARLY USEFUL IN PYTHON MODE

`\\[indent-new-comment-line]' is handy for entering a multi-line comment.

`\\[set-selective-display]' with a `small' prefix arg is ideally suited for viewing the
overall class and def structure of a module.

`\\[back-to-indentation]' moves point to a line's first non-blank character.

`\\[indent-relative]' is handy for creating odd indentation.

@OTHER EMACS HINTS

If you don't like the default value of a variable, change its value to
whatever you do like by putting a `setq' line in your .emacs file.
E.g., to set the indentation increment to 4, put this line in your
.emacs:
\t(setq py-indent-offset 4)
To see the value of a variable, do `\\[describe-variable]' and enter the variable
name at the prompt.

When entering a key sequence like `C-c C-n', it is not necessary to
release the CONTROL key after doing the `C-c' part -- it suffices to
press the CONTROL key, press and release `c' (while still holding down
CONTROL), press and release `n' (while still holding down CONTROL), &
then release CONTROL.

Entering Python mode calls with no arguments the value of the variable
`python-mode-hook', if that value exists and is not nil; for backward
compatibility it also tries `py-mode-hook'; see the `Hooks' section of
the Elisp manual for details.

Obscure:  When python-mode is first loaded, it looks for all bindings
to newline-and-indent in the global keymap, and shadows them with
local bindings to py-newline-and-indent."))

;; (require 'info-look)
;; The info-look package does not always provide this function (it
;; appears this is the case with XEmacs 21.1)
(when (fboundp 'info-lookup-maybe-add-help)
  (info-lookup-maybe-add-help
   :mode 'python-mode
   :regexp "[a-zA-Z0-9_]+"
   :doc-spec '(("(python-lib)Module Index")
               ("(python-lib)Class-Exception-Object Index")
               ("(python-lib)Function-Method-Variable Index")
               ("(python-lib)Miscellaneous Index"))))

(declare-function info-lookup-maybe-add-help "info-look" (&rest arg))

(defun py-after-info-look ()
  "Set up info-look for Python.
Used with `eval-after-load'."
  (let* ((version (let ((s (shell-command-to-string (concat py-shell-name
                                                            " -V"))))
                    (string-match "^Python \\([0-9]+\\.[0-9.]+\\_>\\)" s)
                    (match-string 1 s)))
         ;; Whether info files have a Python version suffix, e.g. in Debian.
         (versioned
          (with-temp-buffer
            (with-no-warnings (Info-mode))
            (condition-case ()
                ;; Don't use `info' because it would pop-up a *info* buffer.
                (with-no-warnings
                  (Info-goto-node (format "(python%s-lib)Miscellaneous Index"
                                          version))
                  t)
              (error nil)))))
    (info-lookup-maybe-add-help
     :mode 'python-mode
     :regexp "[[:alnum:]_]+"
     :doc-spec
     ;; Fixme: Can this reasonably be made specific to indices with
     ;; different rules?  Is the order of indices optimal?
     ;; (Miscellaneous in -ref first prefers lookup of keywords, for
     ;; instance.)
     (if versioned
         ;; The empty prefix just gets us highlighted terms.
         `((,(concat "(python" version "-ref)Miscellaneous Index") nil "")
           (,(concat "(python" version "-ref)Module Index" nil ""))
           (,(concat "(python" version "-ref)Function-Method-Variable Index"
                     nil ""))
           (,(concat "(python" version "-ref)Class-Exception-Object Index"
                     nil ""))
           (,(concat "(python" version "-lib)Module Index" nil ""))
           (,(concat "(python" version "-lib)Class-Exception-Object Index"
                     nil ""))
           (,(concat "(python" version "-lib)Function-Method-Variable Index"
                     nil ""))
           (,(concat "(python" version "-lib)Miscellaneous Index" nil "")))
       '(("(python-ref)Miscellaneous Index" nil "")
         ("(python-ref)Module Index" nil "")
         ("(python-ref)Function-Method-Variable Index" nil "")
         ("(python-ref)Class-Exception-Object Index" nil "")
         ("(python-lib)Module Index" nil "")
         ("(python-lib)Class-Exception-Object Index" nil "")
         ("(python-lib)Function-Method-Variable Index" nil "")
         ("(python-lib)Miscellaneous Index" nil ""))))))
(eval-after-load "info-look" '(py-after-info-look))

;;;
(defun py-send-receive (string)
  "Send STRING to inferior Python (if any) and return result.

This is a no-op if `py-check-comint-prompt' returns nil."
  (or (py-send-string-no-output string)
      (let ((proc (py-proc)))
        (with-current-buffer (process-buffer proc)
          (when (py-check-comint-prompt proc)
            (set (make-local-variable 'py-preoutput-result) nil)
            (accept-process-output proc 5)
            (prog1 py-preoutput-result
              (kill-local-variable 'py-preoutput-result)))))))

(defun py-load-file (file-name)
  "Load a Python file FILE-NAME into the inferior Python process.

If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names."
  (interactive "f")
  (py-execute-file-base (get-buffer-process (get-buffer (py-shell))) file-name))

(defalias 'py-find-function 'py-find-definition)
(defun py-find-definition (&optional symbol)
  "Find source of definition of SYMBOL.

Interactively, prompt for SYMBOL."
  (interactive)
  (set-register 98888888 (list (current-window-configuration) (point-marker)))
  (let* ((oldbuf (current-buffer))
         (imports (py-find-imports))
         (symbol (or symbol (with-syntax-table py-dotted-expression-syntax-table
                              (current-word))))
         (enable-recursive-minibuffers t)
         (symbol
          (if (interactive-p)
              (read-string (if symbol
                               (format "Find location of (default %s): " symbol)
                             "Find location of: ")
                           nil nil symbol)
            symbol))
         (orig (point))
         (local (or
                 (py-until-found (concat "class " symbol) imenu--index-alist)
                 (py-until-found symbol imenu--index-alist)))
         source sourcefile path)
    ;; ismethod(), isclass(), isfunction() or isbuiltin()
    ;; ismethod isclass isfunction isbuiltin)
    (if local
        (if (numberp local)
            (progn
              (goto-char local)
              (search-forward symbol (line-end-position) nil 1)
              (push-mark)
              (goto-char (match-beginning 0))
              (exchange-point-and-mark))
          (error "%s" "local not a number"))
      (setq source (py-send-string-return-output (concat imports "import inspect;inspect.getmodule(" symbol ")")))
      (cond ((string-match "SyntaxError" source)
             (setq source (substring-no-properties source (match-beginning 0)))
             (jump-to-register 98888888)
             (message "Can't get source: %s" source))
            ((and source (string-match "builtin" source))
             (progn (jump-to-register 98888888)
                    (message "%s" source)))
            ((and source (setq path (replace-regexp-in-string "'" "" (py-send-string-return-output "import os;os.getcwd()")))
                  (setq sourcefile (replace-regexp-in-string "'" "" (py-send-string-return-output (concat "inspect.getsourcefile(" symbol ")"))))
                  (interactive-p) (message "sourcefile: %s" sourcefile)
                  (find-file (concat path py-separator-char sourcefile))
                  (goto-char (point-min))
                  (re-search-forward (concat py-def-or-class-re symbol) nil nil 1))
             (push-mark)
             (goto-char (match-beginning 0))
             (exchange-point-and-mark)
             (display-buffer oldbuf)))
      sourcefile)))

;;; Miscellanus
(defun py-insert-super ()
  "Insert a function \"super()\" from current environment.

As example given in Python v3.1 documentation » The Python Standard Library »

class C(B):
    def method(self, arg):
        super().method(arg) # This does the same thing as:
                               # super(C, self).method(arg)

Returns the string inserted. "
  (interactive "*")
  (let* ((orig (point))
         (funcname (progn
                     (py-beginning-of-def)
                     (when (looking-at (concat py-def-re " *\\([^(]+\\) *(\\(?:[^),]*\\),? *\\([^)]*\\))"))
                       (match-string-no-properties 2))))
         (args (match-string-no-properties 3))
         (ver (py-which-python))
         classname erg)
    (if (< ver 3)
        (progn
          (py-beginning-of-class)
          (when (looking-at (concat py-class-re " *\\([^( ]+\\)"))
            (setq classname (match-string-no-properties 2)))
          (goto-char orig)
          (setq erg (concat "super(" classname ", self)." funcname "(" args ")"))
          ;; super(C, self).method(arg)"
          (insert erg))
      (goto-char orig)
      (setq erg (concat "super()." funcname "(" args ")"))
      (insert erg))
    erg))

(defun py-nesting-level (&optional pps)
  "Accepts the output of `parse-partial-sexp'. "
  (interactive)
  (let* ((pps (or (ignore-errors (nth 0 pps))
                  (if (featurep 'xemacs)
                      (parse-partial-sexp (point-min) (point))
                    (syntax-ppss))))
         (erg (nth 0 pps)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-beginning-of-commented-section (&optional last)
  "Leave upwards comments and/or empty lines. "
  (interactive)
  (let ((pps (syntax-ppss))
        (last (or last (point))))
    (if (and (or (and (nth 4 pps)(goto-char (nth 8 pps)))(looking-at comment-start))
             (looking-back "^[ \t]*")(not (bobp)))
        (progn
          (skip-chars-backward " \t\r\n\f")
          (py-beginning-of-commented-section last))
      (goto-char last))))

(defun py-empty-arglist-indent (nesting py-indent-offset indent-offset)
  "Internally used by `py-compute-indentation'"
  (if
      (and (eq 1 nesting)
           (save-excursion
             (back-to-indentation)
             (looking-at py-extended-block-or-clause-re)))
      (progn
        (back-to-indentation)
        (+ (current-column) (* 2 (or indent-offset py-indent-offset))))
    (+ (current-indentation) py-indent-offset)))

(defun py-line-backward-maybe ()
  (skip-chars-backward " \t\f" (line-beginning-position))
  (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
    (setq line t)))

(defun py-fetch-previous-indent (orig)
  "Report the preceding indent. "
  (save-excursion
    (goto-char orig)
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (current-indentation)))


(defun py-continuation-offset (&optional arg)
  "With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. "
  (interactive "p")
  (let ((erg (if (eq 1 arg)
                 py-continuation-offset
               (when (numberp arg)
                 (prog1
                     arg
                   (setq py-continuation-offset arg))))))
    (when (and py-verbose-p (interactive-p)) (message "%s" py-continuation-offset))
    py-continuation-offset))

(defalias 'py-count-indentation 'py-compute-indentation)
(defun py-compute-indentation (&optional orig origline closing line nesting repeat indent-offset)
  "Compute Python indentation.

When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of dedenting.

Optional arguments are flags resp. values set and used by `py-compute-indentation' internally
"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let* ((orig (or orig (point)))
             (origline (or origline (py-count-lines)))
             ;; closing indicates: when started, looked
             ;; at a single closing parenthesis
             ;; line: moved already a line backward
             (line line)
             (pps (syntax-ppss))
             (closing (or closing (and (nth 1 pps) (looking-at ".*\\s)")(nth 0 pps))))

             ;; in a recursive call already
             (repeat repeat)
             ;; nesting: started nesting a list
             (nesting nesting)
             erg indent this-line)
        (unless repeat (setq nesting (nth 0 pps))
                (setq repeat t))
        (setq indent
              (cond
               ((and (bobp)
                     (and (not line)(eq origline (py-count-lines))))
                (current-indentation))
               ((and (bobp)(py-statement-opens-block-p py-extended-block-or-clause-re))
                (+ (if py-smart-indentation (py-guess-indent-offset) indent-offset) (current-indentation)))
               ((and (bobp)(not (py-statement-opens-block-p py-extended-block-or-clause-re)))
                (current-indentation))
               ;; in string
               ((and (nth 3 pps)(nth 8 pps))
                (if (and (not line)(eq origline (py-count-lines)))
                    (progn
                      (forward-line -1)
                      (end-of-line)
                      (skip-chars-backward " \t\r\n\f")
                      (if (ignore-errors (< (nth 8 (syntax-ppss)) (line-beginning-position)))
                          (current-indentation)
                        (ignore-errors (goto-char (nth 8 pps)))
                        (py-line-backward-maybe)
                        (back-to-indentation)
                        (py-compute-indentation orig origline closing line nesting repeat indent-offset)))
                  (goto-char (nth 8 pps))
                  (current-indentation)))
               ((and (looking-at "\"\"\"\\|'''")(not (bobp)))
                (py-beginning-of-statement)
                (py-compute-indentation orig origline closing line nesting repeat indent-offset))
               ;; comments
               ((nth 8 pps)
                (if (and (not line)(eq origline (py-count-lines)))
                    (progn
                      (goto-char (nth 8 pps))
                      (py-line-backward-maybe)
                      (skip-chars-backward " \t")
                      (py-compute-indentation orig origline closing line nesting repeat indent-offset))
                  (goto-char (nth 8 pps))
                  (if
                      line
                      (if py-indent-honors-inline-comment
                          (current-column)
                        (if py-indent-comments
                            (progn
                              (py-beginning-of-commented-section)
                              (py-compute-indentation orig origline closing line nesting repeat indent-offset))
                          0))
                    (forward-char -1)
                    (py-compute-indentation orig origline closing line nesting repeat indent-offset))))
               ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*")(not line)(eq origline (py-count-lines)))
                (if py-indent-comments
                    (progn
                      (setq line t)
                      (skip-chars-backward " \t\r\n\f")
                      ;; as previous comment-line might
                      ;; be wrongly unindented, travel
                      ;; whole commented section
                      (py-beginning-of-commented-section)

                      (py-compute-indentation orig origline closing line nesting repeat indent-offset))
                  0))
               ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*")(not (eq origline (py-count-lines))))
                (current-indentation))
               ;; ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*")(not (eq (line-beginning-position) (point-min))))
               ;;  (skip-chars-backward " \t\r\n\f")
               ;;  (setq line t)
               ;;  (py-compute-indentation orig origline closing line nesting repeat indent-offset))
               ((and (eq ?\# (char-after)) line py-indent-honors-inline-comment)
                (current-column))
               ;; lists
               ((nth 1 pps)
                (cond
                 ((and nesting (not line))
                  ;; still at original line
                  (save-excursion
                    (goto-char (nth 1 pps))
                    (setq this-line (py-count-lines))
                    (cond
                     ((< 0 (- origline this-line))
                      (if (< 1 (- origline this-line))
                          (cond
                           (closing
                            (cond ((looking-back "^[ \t]*")
                                   (current-column))
                                  ((and (eq 1 closing) (looking-at "\\s([ \t]*$") py-closing-list-dedents-bos)
                                   (current-indentation))
                                  ((and (eq 1 closing) (looking-at "\\s([ \t]*$") py-closing-list-keeps-space)
                                   (+ (current-column) py-closing-list-space))
                                  ((and (eq 1 closing)(looking-at "\\s([ \t]*$"))
                                   (py-empty-arglist-indent nesting py-indent-offset indent-offset))
                                  (t (py-fetch-previous-indent orig))))
                           ;; already behind a dedented element in list
                           ((<= 2 (- origline this-line))
                            (py-fetch-previous-indent orig))
                           ((< (current-indentation) (current-column))
                            (+ (current-indentation) py-indent-offset))
                           (t (py-fetch-previous-indent orig)))
                        (cond ((looking-at "\\s([ \t]*$")
                               (py-empty-arglist-indent nesting py-indent-offset indent-offset))
                              ((looking-at "\\s([ \t]*\\([^ \t]+.*\\)$")
                               (goto-char (match-beginning 1))
                               (if py-indent-paren-spanned-multilines-p
                                   (+ (current-column) py-indent-offset)
                                 (current-column)))
                              (t (+ (current-column) (* (nth 0 pps)))))))
                     ((nth 1 (syntax-ppss))
                      (goto-char (nth 1 (syntax-ppss)))
                      (setq line (< (py-count-lines) origline))
                      (py-compute-indentation orig origline closing line nesting repeat indent-offset))
                     ((not (py-beginning-of-statement-p))
                      (py-beginning-of-statement)
                      (py-compute-indentation orig origline closing line nesting repeat indent-offset))
                     (t (1+ (current-column))))))
                 ((and (not nesting) line)
                  (py-beginning-of-statement)
                  (py-compute-indentation orig origline closing line nesting repeat indent-offset))
                 ((not nesting)
                  (progn (goto-char (+ py-lhs-inbound-indent (nth 1 pps)))
                         (when (looking-at "[ \t]+")
                           (goto-char (match-end 0)))
                         (current-column)))
                 (t
                  (goto-char (nth 1 pps))
                  (py-compute-indentation orig origline closing line nesting repeat indent-offset))))
               ((and (eq (char-after) (or ?\( ?\{ ?\[)) line)
                (1+ (current-column)))
               ((py-preceding-line-backslashed-p)
                (progn
                  (py-beginning-of-statement)
                  (setq this-line (py-count-lines))
                  (if (< 1 (- origline this-line))
                      (py-fetch-previous-indent orig)
                    (if (looking-at "from +\\([^ \t\n]+\\) +import")
                        py-backslashed-lines-indent-offset
                      (+ (current-indentation) py-continuation-offset)))))
               ((and (looking-at py-block-closing-keywords-re)(eq (py-count-lines) origline))
                (skip-chars-backward "[ \t\r\n\f]")
                (py-beginning-of-statement)
                (cond ((looking-at py-extended-block-or-clause-re)
                       (+
                        (if py-smart-indentation (py-guess-indent-offset) indent-offset)
                        (current-indentation)))
                      ((looking-at py-block-closing-keywords-re)

                       (- (current-indentation) py-indent-offset))
                      (t (current-column))))
               ((looking-at py-block-closing-keywords-re)
                (if (< (line-end-position) orig)
                    (- (current-indentation) py-indent-offset)
                  (py-beginning-of-block-or-clause (current-indentation))
                  (current-indentation)))
               ((looking-at py-no-outdent-re)
                (if (eq (py-count-lines) origline)
                    (progn
                      (back-to-indentation)
                      (py-line-backward-maybe)
                      (py-compute-indentation orig origline closing line nesting repeat indent-offset))
                  (current-indentation)))
               ((and (looking-at py-elif-re) (eq (py-count-lines) origline))
                (py-line-backward-maybe)
                (car (py-clause-lookup-keyword py-elif-re -1 nil orig origline)))
               ((and (looking-at py-clause-re)(not line)(eq origline (py-count-lines)))
                (cond ((looking-at py-finally-re)
                       (car (py-clause-lookup-keyword py-finally-re -1 nil orig origline)))
                      ((looking-at py-except-re)
                       (car (py-clause-lookup-keyword py-except-re -1 nil orig origline)))
                      ((looking-at py-else-re)
                       (car (py-clause-lookup-keyword py-else-re -1 nil orig origline)))
                      ((looking-at py-elif-re)
                       (car (py-clause-lookup-keyword py-elif-re -1 nil orig origline)))
                      ;; maybe at if, try, with
                      (t (car (py-clause-lookup-keyword py-block-or-clause-re -1 nil orig origline)))))
               ((looking-at py-block-or-clause-re)
                (cond ((and (not line)(eq origline (py-count-lines)))
                       (py-line-backward-maybe)
                       (setq line t)
                       (py-compute-indentation orig origline closing line nesting t indent-offset))
                      (t (+
                          (cond (indent-offset)
                                (py-smart-indentation
                                 (py-guess-indent-offset))
                                (t py-indent-offset))
                          (current-indentation)))))
               ((looking-at py-block-closing-keywords-re)
                (py-beginning-of-block)
                (current-indentation))
               ((and (< (py-count-lines) origline)
                     (eq (current-column) (current-indentation)))
                (and
                 (looking-at py-assignment-re)
                 (goto-char (match-end 0)))
                ;; multiline-assignment
                (if (and nesting (looking-at " *[[{(]")(not (looking-at ".+[]})][ \t]*$")))
                    (+ (current-indentation) py-indent-offset)
                  (current-indentation)))
               ((looking-at py-assignment-re)
                (py-beginning-of-statement)
                (py-compute-indentation orig origline closing line nesting repeat indent-offset))
               ((and (< (current-indentation) (current-column))(not line))
                (back-to-indentation)
                (unless line
                  (setq nesting (nth 0 (syntax-ppss))))
                (py-compute-indentation orig origline closing line nesting repeat indent-offset))
               ((and (not (py-beginning-of-statement-p)) (not (and line (eq ?\# (char-after)))))
                (if (bobp)
                    (current-column)
                  (if (eq (point) orig)
                      (progn
                        (py-line-backward-maybe)
                        (py-compute-indentation orig origline closing line nesting repeat indent-offset))
                    (py-beginning-of-statement)
                    (py-compute-indentation orig origline closing line nesting repeat indent-offset))))
               ((py-statement-opens-block-p py-extended-block-or-clause-re)
                (if (< (py-count-lines) origline)
                    (+ (if py-smart-indentation (py-guess-indent-offset) indent-offset) (current-indentation))
                  (skip-chars-backward " \t\r\n\f")
                  (setq line t)
                  (back-to-indentation)
                  (py-compute-indentation orig origline closing line nesting t indent-offset)))
               ((and (not line)(eq origline (py-count-lines))
                     (save-excursion
                       (and (setq erg (py-go-to-keyword py-extended-block-or-clause-re))
                            (if py-smart-indentation (setq indent (py-guess-indent-offset)) t)
                            (ignore-errors (< orig (or (py-end-of-block-or-clause)(point)))))))
                (+ (car erg) (if py-smart-indentation
                                 ;; (py-guess-indent-offset)
                                 (or indent (py-guess-indent-offset))
                               indent-offset)))
               ((and (not line)(eq origline (py-count-lines))
                     (py-beginning-of-statement-p))
                (py-beginning-of-statement)
                (py-compute-indentation orig origline closing line nesting repeat indent-offset))
               (t (current-indentation))))
        (when (and py-verbose-p (interactive-p)) (message "%s" indent))
        indent))))

(defalias 'pios 'py-indentation-of-statement)
(defalias 'ios 'py-indentation-of-statement)
(defun py-indentation-of-statement ()
  "Returns the indenation of the statement at point. "
  (interactive)
  (let ((erg (save-excursion
               (back-to-indentation)
               (or (py-beginning-of-statement-p)
                   (py-beginning-of-statement))
               (current-indentation))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

;; (defun py-comint-output-filter-function (string)
;;   "Watch output for Python prompt and exec next file waiting in queue.
;; This function is appropriate for `comint-output-filter-functions'."
;;   ;;remove ansi terminal escape sequences from string
;;   (setq string (ansi-color-filter-apply string))
;;   (when (and (string-match py-shell-input-prompt-1-regexp string)
;;              py-file-queue)
;;     (if py-switch-buffers-on-execute-p
;;         (pop-to-buffer (current-buffer)))
;;     (ignore-errors (delete-file (car py-file-queue)))
;;     (setq py-file-queue (cdr py-file-queue))
;;     (if py-file-queue
;;         (let ((pyproc (get-buffer-process (current-buffer))))
;;           (py-execute-file-base pyproc (car py-file-queue))))))

(defun py-guess-default-python ()
  "Defaults to \"python\", if guessing didn't succeed. "
  (interactive)
  (let* ((ptn (or py-shell-name (py-choose-shell) "python"))
         (erg (if py-edit-only-p ptn (executable-find ptn))))
    (when (interactive-p)
      (if erg
          (message "%s" ptn)
        (message "%s" "Could not detect Python on your system")))))

(defun py-separator-char ()
  "Return the file-path separator char from current machine.

When `py-separator-char' is customized, its taken.
Returns char found. "
  (let ((erg (cond ((characterp py-separator-char)
                    (char-to-string py-separator-char))
                   ;; epd hack
                   ((and
                     (string-match "[Ii][Pp]ython" py-shell-name)
                     (string-match "epd\\|EPD" py-shell-name))
                    (replace-regexp-in-string "\n" ""
                                              (shell-command-to-string (concat py-shell-name " -c \"import os; print(os.sep)\"")))))))
    (if (and erg (string-match "^$" erg))
        (setq erg (substring erg (string-match "^$" erg)))
      (setq erg (replace-regexp-in-string "\n" "" (shell-command-to-string (concat py-shell-name " -W ignore" " -c \"import os; print(os.sep)\"")))))
    erg))

(unless py-separator-char (setq py-separator-char (py-separator-char)))

(defun py-process-name (&optional name)
  "Return the name of the running Python process, `get-process' willsee it. "
  (let* ((thisname (if name
                       (if (string-match py-separator-char name)
                           (substring name (progn (string-match (concat "\\(.+\\)"py-separator-char "\\(.+\\)$") name) (match-beginning 2)))

                         name)
                     (substring py-shell-name (or (string-match (concat py-separator-char ".+$") py-shell-name) 0))))
         (nname (cond (py-dedicated-process-p
                       (make-temp-name (concat thisname "-")))
                      ;; ((string-match "\*" (buffer-name))
                      ;; (replace-regexp-in-string "\*" "" (buffer-name)))
                      (t thisname)))
         (erg (cond ((or (string-match "ipython" nname)
                         (string-match "IPython" nname))
                     "IPython")
                    (nname))))
    (unless (string-match "^\*" erg)(setq erg (concat "*" erg "*")))
    erg))

;; (make-variable-buffer-local 'ipython-completion-command-string)

;; (setq ipython0.10-completion-command-string "print(';'.join(__IP.Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n")

;; from ipython.el
(defun py-dirstack-hook ()
  ;; the following is to synchronize dir-changes
  (make-local-variable 'shell-dirstack)
  (setq shell-dirstack nil)
  (make-local-variable 'shell-last-dir)
  (setq shell-last-dir nil)
  (make-local-variable 'shell-dirtrackp)
  (setq shell-dirtrackp t)
  (add-hook 'comint-input-filter-functions 'shell-directory-tracker nil t))

(defun py-set-ipython-completion-command-string ()
  "Set and return `ipython-completion-command-string'. "
  (interactive)
  (let* ((ipython-version
          (if (string-match "ipython" py-shell-name)
              (string-to-number (substring (shell-command-to-string (concat py-shell-name " -V")) 2 -1))
            ;; choose default installed IPython
            (string-to-number (substring (shell-command-to-string (concat "ipython" " -V")) 2 -1))
            )))
    (when ipython-version
      (setq ipython-completion-command-string (if (< ipython-version 11) ipython0.10-completion-command-string ipython0.11-completion-command-string))
      ipython-completion-command-string)))

(defalias 'py-dedicated-shell 'py-shell-dedicated)
(defun py-shell-dedicated (&optional argprompt)
  "Start an interactive Python interpreter in another window.

With optional \\[universal-argument] user is prompted by
`py-choose-shell' for command and options to pass to the Python
interpreter.
"
  (interactive "P")
  (py-shell argprompt t))

(defun py-buffer-name-prepare ()
  "Return an appropriate name to display in modeline.
SEPCHAR is the file-path separator of your system. "
  (let ((name (capitalize py-shell-name))
        prefix erg suffix liste)
    (when (string-match py-separator-char name)
      (unless py-modeline-acronym-display-home-p
        (when (string-match (concat "^" (expand-file-name "~")) name)
          (setq name (replace-regexp-in-string (concat "^" (expand-file-name "~")) "" name))))
      (save-match-data
        (setq liste (split-string name py-separator-char)))
      (dolist (ele liste)
        (unless (string= "" ele)
          (setq prefix (concat prefix (char-to-string (aref ele 0))))))
      (unless py-modeline-display-full-path-p

        (setq name (substring name (1+ (string-match (concat py-separator-char "[^" py-separator-char "]+$") name))))))
    (setq erg
          (cond ((string= "ipython" name)
                 (replace-regexp-in-string "ipython" "IPython" name))
                ((string= "jython" name)
                 (replace-regexp-in-string "jython" "Jython" name))
                ((string= "python" name)
                 (replace-regexp-in-string "python" "Python" name))
                ((string-match "python2" name)
                 (replace-regexp-in-string "python2" "Python2" name))
                ((string-match "python3" name)
                 (replace-regexp-in-string "python3" "Python3" name))
                (t name)))
    (when py-dedicated-process-p
      (setq erg (make-temp-name (concat erg "-"))))
    (cond ((and prefix (string-match "^\*" erg))
           (setq erg (replace-regexp-in-string "^\*" (concat "*" prefix " ") erg)))
          (prefix
           (setq erg (concat "*" prefix " " erg "*")))

          (t (setq erg (concat "*" erg "*"))))
    erg))

(defun py-delete-numbers-and-stars-from-string (string)
  "Delete numbering and star chars from string, return result.

Needed when file-path names are contructed from maybe numbered buffer names like \"\*Python\*<2> \""
  (replace-regexp-in-string
   "<\\([0-9]+\\)>" ""
   (replace-regexp-in-string
    "\*" ""
    string)))

(defun py-shell-manage-windows (output-buffer &optional windows-displayed windows-config)
  (cond ((and (boundp 'err-p) err-p)
         (py-jump-to-exception err-p py-exception-buffer)
         ;; (and windows-displayed (eq 1 (length windows-displayed))
         ;; (funcall py-split-windows-on-execute-function)
         (display-buffer output-buffer)
         (goto-char (point-max)) )

        ;; split and switch
        ((and py-split-windows-on-execute-p
              py-switch-buffers-on-execute-p)
         (when (< (count-windows) py-max-split-windows)
           (funcall py-split-windows-on-execute-function))
         (set-buffer output-buffer)
         (goto-char (point-max))
         (switch-to-buffer (current-buffer))
         (display-buffer py-exception-buffer))
        ;; split, not switch
        ((and
          py-split-windows-on-execute-p
          (not py-switch-buffers-on-execute-p))
         (delete-other-windows)
         (if (< (count-windows) py-max-split-windows)
             (progn
               (funcall py-split-windows-on-execute-function)
               (set-buffer output-buffer)
               (goto-char (point-max))
               (and (bufferp py-exception-buffer)(set-buffer py-exception-buffer)
                    (switch-to-buffer (current-buffer))
                    (display-buffer output-buffer 'display-buffer-reuse-window))
               (display-buffer output-buffer 'display-buffer-reuse-window))))
        ;; no split, switch
        ((and
          py-switch-buffers-on-execute-p
          (not py-split-windows-on-execute-p))
         (let (pop-up-windows)
           (set-buffer output-buffer)
           (goto-char (point-max))
           (switch-to-buffer (current-buffer))))
        ;; no split, no switch
        ((not py-switch-buffers-on-execute-p)
         ;; (if (equal (window-list-1) windows-displayed)
         ;; (jump-to-register 313465889)
         (let (pop-up-windows)
           (set-buffer py-exception-buffer)
           (switch-to-buffer (current-buffer)))
         ;;)
         )))

(defun py-report-executable (py-buffer-name)
  (let ((erg (downcase (replace-regexp-in-string
                        "<\\([0-9]+\\)>" ""
                        (replace-regexp-in-string
                         "\*" ""
                         (if
                             (string-match " " py-buffer-name)
                             (substring py-buffer-name (1+ (string-match " " py-buffer-name)))
                           py-buffer-name))))))
    (when (string-match "-" erg)
      (setq erg (substring erg 0 (string-match "-" erg))))
    erg))

(defun py-shell-send-setup-code (process)
  "Send all setup code for shell.
This function takes the list of setup code to send from the
`py-setup-codes' list."
  (accept-process-output process 1)
  (dolist (code py-setup-codes)
    (py-send-string-no-output
     (symbol-value code) process)
    (sit-for 0.1)))

(defun py-shell (&optional argprompt dedicated shell buffer-name done)
  "Start an interactive Python interpreter in another window.
Interactively, \\[universal-argument] 4 prompts for a buffer.
\\[universal-argument] 2 prompts for `py-python-command-args'.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

Returns py-shell's buffer-name.
Optional string PYSHELLNAME overrides default `py-shell-name'.
BUFFER allows specifying a name, the Python process is connected to
When DONE is `t', `py-shell-manage-windows' is omitted
"
  (interactive "P")
  (let* ((dedicated (or dedicated py-dedicated-process-p))
         (py-exception-buffer (or py-exception-buffer (current-buffer)))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (args py-python-command-args)
         (path (getenv "PYTHONPATH"))
         (py-shell-name (or shell py-shell-name (py-choose-shell)))

         ;; reset later on
         (py-buffer-name
          (cond (buffer-name)
                (t (and (not dedicated) argprompt
                        (cond
                         ((eq 4 (prefix-numeric-value argprompt))
                          (prog1
                              (read-buffer "Py-Shell buffer: "
                                           (generate-new-buffer-name (py-buffer-name-prepare)))
                            (when (file-remote-p default-directory)
                              ;; It must be possible to declare a local default-directory.
                              (setq default-directory
                                    (expand-file-name
                                     (read-file-name
                                      "Default directory: " default-directory default-directory
                                      t nil 'file-directory-p)))
                              (setq py-separator-char (py-separator-char)))))
                         ((and (eq 2 (prefix-numeric-value argprompt))
                               (fboundp 'split-string))
                          (setq args (split-string
                                      (read-string "Py-Shell arguments: "
                                                   (concat
                                                    (mapconcat 'identity py-python-command-args " ") " "))))))))))
         ;; If we use a pipe, Unicode characters are not printed
         ;; correctly (Bug#5794) and IPython does not work at
         ;; all (Bug#5390). python.el
         (process-connection-type t)
         ;; (comint-scroll-to-bottom-on-output t)
         ;; already in py-choose-shell
         (py-use-local-default
          (if (not (string= "" py-shell-local-path))
              (expand-file-name py-shell-local-path)
            (when py-use-local-default
              (error "Abort: `py-use-local-default' is set to `t' but `py-shell-local-path' is empty. Maybe call `py-toggle-local-default-use'"))))
         (py-buffer-name (or py-buffer-name (py-buffer-name-prepare)))
         (executable (cond (py-shell-name)
                           (py-buffer-name
                            (py-report-executable py-buffer-name))))
         proc py-smart-indentation)
    ;; lp:1169687, if called from within an existing py-shell, open a new one
    (and (bufferp py-exception-buffer)(string= py-buffer-name (buffer-name py-exception-buffer))
         (setq py-buffer-name (generate-new-buffer-name py-buffer-name)))

    (unless (comint-check-proc py-buffer-name)
      (set-buffer (apply 'make-comint-in-buffer executable py-buffer-name executable nil args))
      (unless (interactive-p) (sit-for 0.3))
      (set (make-local-variable 'comint-prompt-regexp)
           (cond ((string-match "[iI][pP]ython[[:alnum:]*-]*$" py-buffer-name)
                  (concat "\\("
                          (mapconcat 'identity
                                     (delq nil (list py-shell-input-prompt-1-regexp py-shell-input-prompt-2-regexp ipython-de-input-prompt-regexp ipython-de-output-prompt-regexp py-pdbtrack-input-prompt py-pydbtrack-input-prompt))
                                     "\\|")
                          "\\)"))
                 (t (concat "\\("
                            (mapconcat 'identity
                                       (delq nil (list py-shell-input-prompt-1-regexp py-shell-input-prompt-2-regexp py-pdbtrack-input-prompt py-pydbtrack-input-prompt))
                                       "\\|")
                            "\\)"))))
      (set (make-local-variable 'comint-input-filter) 'py-history-input-filter)
      (set (make-local-variable 'comint-prompt-read-only) py-shell-prompt-read-only)
      (set (make-local-variable 'comint-use-prompt-regexp) nil)
      (set (make-local-variable 'compilation-error-regexp-alist)
           python-compilation-regexp-alist)
      ;; (setq completion-at-point-functions nil)
      (and py-fontify-shell-buffer-p
           (set (make-local-variable 'font-lock-defaults)
                '(py-font-lock-keywords nil nil nil nil
                                        (font-lock-syntactic-keywords
                                         . py-font-lock-syntactic-keywords))))
      (set (make-local-variable 'comment-start) "# ")
      (set (make-local-variable 'comment-start-skip) "^[ \t]*#+ *")
      (set (make-local-variable 'comment-column) 40)
      (set (make-local-variable 'comment-indent-function) #'py-comment-indent-function)
      (set (make-local-variable 'indent-region-function) 'py-indent-region)
      (set (make-local-variable 'indent-line-function) 'py-indent-line)
      (setq proc (get-buffer-process (current-buffer)))
      (py-shell-send-setup-code proc)
      (and py-set-pager-cat-p (comint-simple-send proc "import os;os.environ['PAGER'] = 'cat'"))
      (compilation-shell-minor-mode 1)
      (setq comint-input-sender 'py-shell-simple-send)
      ;; (sit-for 0.1)
      (setq comint-input-ring-file-name
            (cond ((string-match "[iI][pP]ython[[:alnum:]*-]*$" py-buffer-name)
                   (if py-honor-IPYTHONDIR-p
                       (if (getenv "IPYTHONDIR")
                           (concat (getenv "IPYTHONDIR") "/history")
                         py-ipython-history)
                     py-ipython-history))
                  (t
                   (if py-honor-PYTHONHISTORY-p
                       (if (getenv "PYTHONHISTORY")
                           (concat (getenv "PYTHONHISTORY") "/" (py-report-executable py-buffer-name) "_history")
                         py-ipython-history)
                     py-ipython-history))))
      (comint-read-input-ring t)
      (set-process-sentinel (get-buffer-process py-buffer-name)
                            #'shell-write-history-on-exit)
      (add-hook 'comint-output-filter-functions
                'ansi-color-process-output)
      (use-local-map py-shell-map)
      ;; pdbtrack
      (and py-pdbtrack-do-tracking-p
           (add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file t)
           (remove-hook 'comint-output-filter-functions 'python-pdbtrack-track-stack-file t))
      (set-syntax-table python-mode-syntax-table))
    ;; (goto-char (point-max))
    ;; (add-hook 'py-shell-hook 'py-dirstack-hook)
    (and py-fontify-shell-buffer-p (font-lock-fontify-buffer))
    ;; (py-send-string-return-output

    (unless done (py-shell-manage-windows py-buffer-name))
    (when py-shell-hook (run-hooks 'py-shell-hook))
    py-buffer-name))

(defun py-indent-forward-line (&optional arg)
  "Indent and move one line forward to next indentation.
Returns column of line reached.

If `py-kill-empty-line' is non-nil, delete an empty line.
When closing a form, use py-close-block et al, which will move and indent likewise.
With \\[universal argument] just indent.
"
  (interactive "*P")
  (let ((orig (point))
        erg)
    (unless (eobp)
      (if (and (py-in-comment-p)(not py-indent-comments))
          (forward-line 1)
        (py-indent-line-outmost)
        (unless (eq 4 (prefix-numeric-value arg))
          (if (eobp) (newline)
            (progn (forward-line 1))
            (when (and py-kill-empty-line (empty-line-p) (not (looking-at "[ \t]*\n[[:alpha:]]")) (not (eobp)))
              (delete-region (line-beginning-position) (line-end-position)))))))
    (back-to-indentation)
    (when (or (eq 4 (prefix-numeric-value arg)) (< orig (point))) (setq erg (current-column)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-dedent-forward-line (&optional arg)
  "Dedent line and move one line forward. "
  (interactive "*p")
  (py-dedent arg)
  (forward-line 1)
  (end-of-line)
  (skip-chars-backward " \t\r\n\f"))

(defun py-dedent (&optional arg)
  "Dedent line according to `py-indent-offset'.

With arg, do it that many times.
If point is between indent levels, dedent to next level.
Return indentation reached, if dedent done, nil otherwise.

Affected by `py-dedent-keep-relative-column'. "
  (interactive "*p")
  (let ((orig (copy-marker (point)))
        erg)
    (dotimes (i arg)
      (let* ((cui (current-indentation))
             (remain (% cui py-indent-offset))
             (indent (* py-indent-offset (/ cui py-indent-offset))))
        (beginning-of-line)
        (fixup-whitespace)
        (if (< 0 remain)
            (indent-to-column indent)
          (indent-to-column (- cui py-indent-offset)))))
    (when (< (point) orig)
      (setq erg (current-column)))
    (if py-dedent-keep-relative-column
        (goto-char orig)
      (end-of-line)
      (skip-chars-backward " \t\r\n\f"))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-close-intern (regexp)
  "Core function, internal used only. "
  (let ((cui (ignore-errors (car (py-go-to-keyword (symbol-value regexp))))))
    (py-end-base regexp (point))
    (forward-line 1)
    (if py-close-provides-newline
        (unless (empty-line-p) (split-line))
      (fixup-whitespace))
    (indent-to-column cui)
    cui))

(defun py-close-def ()
  "Set indent level to that of beginning of function definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py-close-intern py-def-re)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-close-class ()
  "Set indent level to that of beginning of class definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py-close-intern py-class-re)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-close-clause ()
  "Set indent level to that of beginning of clause definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py-close-intern py-clause-re)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-close-block ()
  "Set indent level to that of beginning of block definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py-close-intern 'py-block-re)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-class-at-point ()
  "Return class definition as string.

With interactive call, send it to the message buffer too. "
  (interactive)
  (save-excursion
    (let* ((beg (py-beginning-of-class))
	   (end (py-end-of-class))
	   (res (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))
      (when (and py-verbose-p (interactive-p)) (message "%s" res))
      res)))

(defun py-line-at-point ()
  "Return line as string.
  With interactive call, send it to the message buffer too. "
  (interactive)
  (let* ((beg (line-beginning-position))
	 (end (line-end-position))
	 (res (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))
    (when (and py-verbose-p (interactive-p)) (message "%s" res))
    res))

(defun py-looking-at-keywords-p ()
  "If looking at a python keyword. Returns t or nil. "
  (interactive)
  (let* ((kwds1 (car (nth 1 (eval (eval (quote (car font-lock-defaults)))))))
         (kwds3 (car (nth 3 (eval (eval (quote (car font-lock-defaults)))))))
	 (res
	  (or
           (looking-at kwds1)
           (looking-at kwds3))))
    (when (and py-verbose-p (interactive-p)) (message "looking-at keywords: %s" res))
    res))

(defun py-match-paren-mode (&optional arg)
  "py-match-paren-mode nil oder t"
  (interactive "P")
  (if (or (eq 4 (prefix-numeric-value arg)) (not py-match-paren-mode))
      (setq py-match-paren-mode t)
    (setq py-match-paren-mode nil)))

(defun py-match-paren ()
  "Goto to the opening or closing of block before or after point.

With arg, do it that many times.
 Closes unclosed block if jumping from beginning. "
  (interactive)
  (let ((cuc (current-column))
	(cid (current-indentation)))
    (py-beginning-of-block-or-clause)
    (if (< cuc (current-indentation))
	(goto-char cuc)
      (back-to-indentation)
      (when (eq (point) cuc)
	(py-end-of-block)))))

;;;

(defalias 'druck 'py-printform-insert)
(defun py-printform-insert (&optional arg)
  "Inserts a print statement out of current `(car kill-ring)' by default, inserts ARG instead if delivered. "
  (interactive "*")
  (let* ((name (string-strip (or arg (car kill-ring))))
         ;; guess if doublequotes or parentesis are needed
         (numbered (and (string-match "^[0-9]" name) (string-match "^[ \t]*[0-9]" name)(string-match "[0-9][ \t]*$" name)))
         (form (cond ((or (eq major-mode 'python-mode)(eq major-mode 'inferior-python-mode))
                      (if numbered
                          (concat "print(\"" name ": %s \" % (" name "))")
                        (concat "print(\"" name ": %s \" % \"" name "\")"))))))
    (insert form)))

(defun eva ()
  "Put \"eval(...)\" forms around strings at point. "
  (interactive "*")
  (skip-chars-forward " \t\r\n\f")
  (let* ((bounds (ar-bounds-of-word-atpt))
         (beg (car bounds))
         (end (cdr bounds)))
    (goto-char end)
    (insert ")")
    (goto-char beg)
    (insert "eval(")))

(defun pst-here ()
  "Kill previous \"pdb.set_trace()\" and insert it at point. "
  (interactive "*")
  (let ((orig (copy-marker (point))))
    (search-backward "pdb.set_trace()")
    (replace-match "")
    (when (empty-line-p)
      (delete-region (line-beginning-position) (line-end-position)))
    (goto-char orig)
    (insert "pdb.set_trace()")))

(defun py-line-to-printform-python2 (&optional arg)
  "Transforms the item on current in a print statement. "
  (interactive "*")
  (let* ((name (thing-at-point 'word))
         (form (cond ((or (eq major-mode 'python-mode)(eq major-mode 'inferior-python-mode))
                      (concat "print \"" name ": %s \" % " name)))))
    (delete-region (line-beginning-position) (line-end-position))
    (insert form))
  (forward-line 1)
  (back-to-indentation))

;;; Imenu

;; Note that in this format, this variable can still be used with the
;; imenu--generic-function. Otherwise, there is no real reason to have
;; it.

;; These next two variables are used when searching for the Python
;; class/definitions. Just saving some time in accessing the
;; generic-python-expression, really.

(defun py-switch-imenu-index-function ()
  "Switch between series 5. index machine `py-imenu-create-index' and `py-imenu-create-index-new', which also lists modules variables "
  (interactive)
  (if (eq major-mode 'python-mode)
      (progn
        (if (eq py-imenu-create-index-function 'py-imenu-create-index-new)
            ;; (setq py-imenu-create-index-function 'py-imenu-create-index)
            (set (make-local-variable 'py-imenu-create-index-function) 'py-imenu-create-index)
          ;; (setq py-imenu-create-index-function 'py-imenu-create-index-new)
          (set (make-local-variable 'py-imenu-create-index-function) 'py-imenu-create-index-new))
        (when py-menu
          (easy-menu-add py-menu))
        (when py-verbose-p (message "imenu-create-index-function: %s" (prin1-to-string py-imenu-create-index-function)))
        (funcall imenu-create-index-function))
    (error "%s" "Only available in buffers set to python-mode")))

(defalias 'py-imenu-create-index-function 'py-imenu-create-index)
(defun py-imenu-create-index ()
  "Python interface function for the Imenu package.
Finds all Python classes and functions/methods. Calls function
\\[py-imenu-create-index-engine].  See that function for the details
of how this works."
  (setq py-imenu-generic-regexp (car py-imenu-generic-expression)
        py-imenu-generic-parens (if py-imenu-show-method-args-p
                                    py-imenu-method-arg-parens
                                  py-imenu-method-no-arg-parens))
  (goto-char (point-min))
  ;; Warning: When the buffer has no classes or functions, this will
  ;; return nil, which seems proper according to the Imenu API, but
  ;; causes an error in the XEmacs port of Imenu.  Sigh.
  (py-imenu-create-index-engine nil))

(defun py-imenu-create-index-engine (&optional start-indent)
  "Function for finding Imenu definitions in Python.

Finds all definitions (classes, methods, or functions) in a Python
file for the Imenu package.

Returns a possibly nested alist of the form

        (INDEX-NAME . INDEX-POSITION)

The second element of the alist may be an alist, producing a nested
list as in

        (INDEX-NAME . INDEX-ALIST)

This function should not be called directly, as it calls itself
recursively and requires some setup.  Rather this is the engine for
the function \\[py-imenu-create-index-function].

It works recursively by looking for all definitions at the current
indention level.  When it finds one, it adds it to the alist.  If it
finds a definition at a greater indentation level, it removes the
previous definition from the alist. In its place it adds all
definitions found at the next indentation level.  When it finds a
definition that is less indented then the current level, it returns
the alist it has created thus far.

The optional argument START-INDENT indicates the starting indentation
at which to continue looking for Python classes, methods, or
functions.  If this is not supplied, the function uses the indentation
of the first definition found."
  (let (index-alist
        sub-method-alist
        looking-p
        def-name prev-name
        cur-indent def-pos
        (class-paren (first py-imenu-generic-parens))
        (def-paren (second py-imenu-generic-parens)))
    (setq looking-p
          (re-search-forward py-imenu-generic-regexp (point-max) t))
    (while looking-p
      (save-excursion
        ;; used to set def-name to this value but generic-extract-name
        ;; is new to imenu-1.14. this way it still works with
        ;; imenu-1.11
        ;;(imenu--generic-extract-name py-imenu-generic-parens))
        (let ((cur-paren (if (match-beginning class-paren)
                             class-paren def-paren)))
          (setq def-name
                (buffer-substring-no-properties (match-beginning cur-paren)
                                                (match-end cur-paren))))
        (save-match-data
          (py-beginning-of-def-or-class))
        (beginning-of-line)
        (setq cur-indent (current-indentation)))
      ;; HACK: want to go to the next correct definition location.  We
      ;; explicitly list them here but it would be better to have them
      ;; in a list.
      (setq def-pos
            (or (match-beginning class-paren)
                (match-beginning def-paren)))
      ;; if we don't have a starting indent level, take this one
      (or start-indent
          (setq start-indent cur-indent))
      ;; if we don't have class name yet, take this one
      (or prev-name
          (setq prev-name def-name))
      ;; what level is the next definition on?  must be same, deeper
      ;; or shallower indentation
      (cond
       ;; Skip code in comments and strings
       ((py-in-literal))
       ;; at the same indent level, add it to the list...
       ((= start-indent cur-indent)
        (push (cons def-name def-pos) index-alist))
       ;; deeper indented expression, recurse
       ((< start-indent cur-indent)
        ;; the point is currently on the expression we're supposed to
        ;; start on, so go back to the last expression. The recursive
        ;; call will find this place again and add it to the correct
        ;; list
        (re-search-backward py-imenu-generic-regexp (point-min) 'move)
        (setq sub-method-alist (py-imenu-create-index-engine cur-indent))
        (if sub-method-alist
            ;; we put the last element on the index-alist on the start
            ;; of the submethod alist so the user can still get to it.
            (let ((save-elmt (pop index-alist)))
              (push (cons prev-name
                          (cons save-elmt sub-method-alist))
                    index-alist))))
       ;; found less indented expression, we're done.
       (t
        (setq looking-p nil)
        (re-search-backward py-imenu-generic-regexp (point-min) t)))
      ;; end-cond
      (setq prev-name def-name)
      (and looking-p
           (setq looking-p
                 (re-search-forward py-imenu-generic-regexp
                                    (point-max) 'move))))
    (nreverse index-alist)))

(defun py-imenu-create-index-new-intern (&optional thisend end)
  (let* ((pos (match-beginning 0))
         (name (match-string-no-properties 2))
         (classname (concat "class " name))
         (thisend (or thisend (save-match-data (py-end-of-def-or-class-position))))
         sublist)
    (while (and (re-search-forward "^[ \t]*\\(?:\\(def\\|class\\)\\)[ \t]+\\(?:\\(\\sw+\\)\\)" (or thisend end) t 1)(not (nth 8 (syntax-ppss))))
      (let* ((pos (match-beginning 0))
             (name (match-string-no-properties 2))
             (classname (concat "class " name))
             (thisend (or thisend (save-match-data (py-end-of-def-or-class-position)))))
        (if (string= "class" (match-string-no-properties 1))
            (py-imenu-create-index-new-intern (save-match-data (py-end-of-def-or-class-position) end))
          (push (cons (concat " " name) pos) sublist))))
    (if classname
        (progn
          (setq sublist (nreverse sublist))
          (push (cons classname pos) sublist)
          (push (cons classname sublist) index-alist))
      (push sublist index-alist))))

(defun py-imenu-create-index-new (&optional beg end)
  "`imenu-create-index-function' for Python. "
  (set (make-local-variable 'imenu-max-items) 99)
  (let ((orig (point))
        (beg (or beg (point-min)))
        (end (or end (point-max)))
        index-alist vars thisend sublist classname pos name)
    (goto-char beg)
    (while (and (re-search-forward "^[ \t]*\\(def\\|class\\)[ \t]+\\(\\sw+\\)" end t 1)(not (nth 8 (syntax-ppss))))
      (if (save-match-data (string= "class" (match-string-no-properties 1)))
          (progn
            (setq pos (match-beginning 0)
                  name (match-string-no-properties 2)
                  classname (concat "class " name)
                  thisend (save-match-data (py-end-of-def-or-class-position))
                  sublist '())
            (while (and (re-search-forward "^[ \t]*\\(def\\|class\\)[ \t]+\\(\\sw+\\)" (or thisend end) t 1)(not (nth 8 (syntax-ppss))))
              (let* ((pos (match-beginning 0))
                     (name (match-string-no-properties 2))
                     (classname (concat "class " name))
                     (thisend (or thisend (save-match-data (py-end-of-def-or-class-position)))))
                (if (string= "class" (match-string-no-properties 1))
                    (py-imenu-create-index-new-intern (save-match-data (py-end-of-def-or-class-position)) end)
                  (push (cons (concat " " name) pos) sublist))))
            (if classname
                (progn
                  (setq sublist (nreverse sublist))
                  (push (cons classname pos) sublist)
                  (push (cons classname sublist) index-alist))
              (push sublist index-alist)))

        (let ((pos (match-beginning 0))
              (name (match-string-no-properties 2)))
          (push (cons name pos) index-alist))))
    ;; Look for module variables.
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\sw+\\)[ \t]*=" end t)
      (unless (nth 8 (syntax-ppss))
        (let ((pos (match-beginning 1))
              (name (match-string-no-properties 1)))
          (push (cons name pos) vars))))
    (setq index-alist (nreverse index-alist))
    (when vars
      (push (cons "Module variables"
                  (nreverse vars))
            index-alist))
    (goto-char orig)
    index-alist))

;;; py-shell
(if py-mode-output-map
    nil
  (setq py-mode-output-map (make-sparse-keymap))
  (define-key py-mode-output-map [button2]  'py-mouseto-exception)
  (define-key py-mode-output-map "\C-c\C-c" 'py-goto-exception)
  ;; TBD: Disable all self-inserting keys.  This is bogus, we should
  ;; really implement this as *Python Output* buffer being read-only
  (mapc #' (lambda (key)
             (define-key py-mode-output-map key
               #'(lambda () (interactive) (beep))))
           (where-is-internal 'self-insert-command)))

(setq py-shell-map
      (let ((map (copy-keymap comint-mode-map)))
        (define-key map (kbd "RET") 'comint-send-input)
        (define-key map "\C-c-" 'py-up-exception)
        (define-key map "\C-c=" 'py-down-exception)
        ;; defined three times... one should succed
        (define-key map (kbd "TAB") 'py-shell-complete)
        (define-key map [(meta tab)] 'py-shell-complete)
        map))

(defun py-toggle-local-default-use ()
  (interactive)
  "Toggle boolean value of `py-use-local-default'.

Returns `py-use-local-default'

See also `py-install-local-shells'
Installing named virualenv shells is the preffered way,
as it leaves your system default unchanged."
  (setq py-use-local-default (not py-use-local-default))
  (when (interactive-p) (message "py-use-local-default set to %s" py-use-local-default))
  py-use-local-default)

(defun py-choose-shell-by-path (&optional py-separator-char)
  "Select Python executable according to version desplayed in path, current buffer-file is selected from.

Returns versioned string, nil if nothing appropriate found "
  (interactive)
  (lexical-let ((path (buffer-file-name))
                (py-separator-char (or py-separator-char py-separator-char))
                erg)
    (when (and path py-separator-char
               (string-match (concat py-separator-char "[iI]?[pP]ython[0-9.]+" py-separator-char) path))
      (setq erg (substring path
                           (1+ (string-match (concat py-separator-char "[iI]?[pP]ython[0-9.]+" py-separator-char) path)) (1- (match-end 0)) )))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-choose-shell-by-shebang ()
  "Choose shell by looking at #! on the first line.

Returns the specified Python resp. Jython shell command name. "
  (interactive)
  ;; look for an interpreter specified in the first line
  (let* (erg res)
    (save-excursion
      (goto-char (point-min))
      (when (looking-at py-shebang-regexp)
        (setq erg (split-string (match-string-no-properties 0) "[#! \t]"))
        (dolist (ele erg)
          (when (string-match "[bijp]+ython" ele)
            (setq res ele)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" res))
    res))

(defun py-choose-shell-by-import ()
  "Choose CPython or Jython mode based imports.

If a file imports any packages in `py-jython-packages', within
`py-import-check-point-max' characters from the start of the file,
return `jython', otherwise return nil."
  (let (mode)
    (save-excursion
      (goto-char (point-min))
      (while (and (not mode)
                  (search-forward-regexp
                   "^\\(\\(from\\)\\|\\(import\\)\\) \\([^ \t\n.]+\\)"
                   py-import-check-point-max t))
        (setq mode (and (member (match-string 4) py-jython-packages)
                        'jython))))
    mode))

(defalias 'py-version 'py-which-python)
(defun py-which-python ()
  "Returns version of Python of current environment, a number. "
  (interactive)
  (let* (treffer (cmd (py-choose-shell))
                 version erg)
    (setq treffer (string-match "\\([23]*\\.?[0-9\\.]*\\)$" cmd))
    (if treffer
        ;; if a number if part of python name, assume it's the version
        (setq version (substring-no-properties cmd treffer))
      (setq erg (shell-command-to-string (concat cmd " --version")))
      ;; Result: "bpython version 0.9.7.1 on top of Python 2.7\n(C) 2008-2010 Bob Farrell, Andreas Stuehrk et al. See AUTHORS for detail.\n"

      (setq version (cond ((string-match (concat "\\(on top of Python \\)" "\\([0-9]\\.[0-9]+\\)") erg)
                           (match-string-no-properties 2 erg))
                          ((string-match "\\([0-9]\\.[0-9]+\\)" erg)
                           (substring erg 7 (1- (length erg)))))))
    (when (interactive-p)
      (if version
          (when py-verbose-p (message "%s" version))
        (message "%s" "Could not detect Python on your system")))
    (string-to-number version)))

(defun py-python-current-environment ()
  "Returns path of current Python installation. "
  (interactive)
  (let* ((cmd (py-choose-shell))
         (denv (shell-command-to-string (concat "type " cmd)))
         (erg (substring denv (string-match "/" denv))))
    (when (interactive-p)
      (if erg
          (when py-verbose-p (message "%s" erg))
        (message "%s" "Could not detect Python on your system")))
    erg))

;; backward compatibility
(defalias 'py-switch-shells 'py-switch-shell)
(defalias 'python-toggle-shells 'py-switch-shell)
(defalias 'py-toggle-shell 'py-switch-shell)
(defun py-switch-shell (&optional arg)
  "Toggles between the interpreter customized in `py-shell-toggle-1' resp. `py-shell-toggle-2'. Was hard-coded CPython and Jython in earlier versions, now starts with Python2 and Python3 by default.

ARG might be a python-version string to set to.

\\[universal-argument] `py-toggle-shell' prompts to specify a reachable Python command.
\\[universal-argument] followed by numerical arg 2 or 3, `py-toggle-shell' opens a respective Python shell.
\\[universal-argument] followed by numerical arg 5 opens a Jython shell.

Should you need more shells to select, extend this command by adding inside the first cond:

                    ((eq NUMBER (prefix-numeric-value arg))
                     \"MY-PATH-TO-SHELL\")
"
  (interactive "P")
  (let ((name (cond ((eq 2 (prefix-numeric-value arg))
                     "python2")
                    ((eq 3 (prefix-numeric-value arg))
                     "python3")
                    ((eq 4 (prefix-numeric-value arg))
                     (string-strip
                      (read-from-minibuffer "Python Shell: " py-shell-name) "\" " "\" "
                      ))
                    ((eq 5 (prefix-numeric-value arg))
                     "jython")
                    (t (if (string-match py-shell-name
                                         py-shell-toggle-1)
                           py-shell-toggle-2
                         py-shell-toggle-1))))
        erg msg)
    (cond ((or (string= "ipython" name)
               (string= "IPython" name))
           (setq py-shell-name name
                 py-which-bufname "IPython"
                 msg "IPython"
                 mode-name "IPython"))
          ((string-match "python3" name)
           (setq py-shell-name name
                 py-which-bufname (py-buffer-name-prepare)
                 msg "CPython"
                 mode-name (py-buffer-name-prepare)))
          ((string-match "jython" name)
           (setq py-shell-name name
                 py-which-bufname (py-buffer-name-prepare)
                 msg "Jython"
                 mode-name (py-buffer-name-prepare)))
          ((string-match "python" name)
           (setq py-shell-name name
                 py-which-bufname (py-buffer-name-prepare)
                 msg "CPython"
                 mode-name py-which-bufname))
          (t
           (setq py-shell-name name
                 py-which-bufname name
                 msg name
                 mode-name name)))
    ;; py-edit-only-p has no interpreter
    ;; (if py-edit-only-p
    ;; (setq erg py-shell-name)
    (setq erg (executable-find py-shell-name))
    ;;)
    (if erg
        (progn
          (force-mode-line-update)
          (when (interactive-p)
            (message "Using the %s shell, %s" msg erg))
          (setq py-output-buffer (format "*%s Output*" py-which-bufname)))
      (error (concat "Could not detect " py-shell-name " on your sys
tem")))))

(defalias 'py-toggle-shells 'py-choose-shell)
(defalias 'py-which-shell 'py-choose-shell)
(defun py-choose-shell (&optional arg pyshell dedicated py-edit-only-p)
  "Return an appropriate executable as a string.

Returns nil, if no executable found.

This does the following:
 - look for an interpreter with `py-choose-shell-by-shebang'
 - examine imports using `py-choose-shell-by-import'
 - look if Path/To/File indicates a Python version
 - if not successful, return default value of `py-shell-name'

When interactivly called, messages the shell name, Emacs would in the given circtumstances.

With \\[universal-argument] 4 is called `py-switch-shell' see docu there.
"
  (interactive "P")
  (if (eq 4 (prefix-numeric-value arg))
      (py-switch-shell '(4))
    (let* ((erg (cond (py-force-py-shell-name-p
                       py-shell-name)
                      (py-use-local-default
                       (if (not (string= "" py-shell-local-path))
                           (expand-file-name py-shell-local-path)
                         (message "Abort: `py-use-local-default' is set to `t' but `py-shell-local-path' is empty. Maybe call `py-toggle-local-default-use'")))
                      ((and (comint-check-proc (current-buffer))
                            (string-match "ython" (process-name (get-buffer-process (current-buffer)))))
                       (process-name (get-buffer-process (current-buffer))))
                      ((py-choose-shell-by-shebang))
                      ((py-choose-shell-by-import))
                      ((py-choose-shell-by-path))
                      (t (or
                          (default-value 'py-shell-name)
                          "python"))))
           (cmd (if py-edit-only-p erg
                  (executable-find erg))))
      (if cmd
          (when (interactive-p)
            (message "%s" cmd))
        (when (interactive-p) (message "%s" "Could not detect Python on your system. Maybe set `py-edit-only-p'?")))
      erg)))

;;;

(defun py-normalize-directory (directory)
  "Make sure DIRECTORY ends with a file-path separator char.

Returns DIRECTORY"
  (let ((erg (cond ((string-match (concat py-separator-char "$") directory)
                    directory)
                   ((not (string= "" directory))
                    (concat directory py-separator-char)))))
    (unless erg (when py-verbose-p (message "Warning: directory is empty")))
    erg))

(defun py-install-directory-check ()
  "Do some sanity check for `py-install-directory'.

Returns `t' if successful. "
  (interactive)
  (let ((erg (and (boundp 'py-install-directory) (stringp py-install-directory) (< 1 (length py-install-directory)))))
    (when (interactive-p) (message "py-install-directory-check: %s" erg))
    erg))

(defun py-guess-py-install-directory ()
  "Takes value of user directory aka $HOME
if `(locate-library \"python-mode\")' is not succesful.

Used only, if `py-install-directory' is empty. "
  (interactive)
  (let ((erg (cond ((locate-library "python-mode")
                    (file-name-directory (locate-library "python-mode")))
                   ((and (buffer-file-name)(string-match "python-mode" (buffer-file-name)))
                    (file-name-directory (buffer-file-name)))
                   ((string-match "python-mode" (buffer-name))
                    default-directory))))
    (if erg
        (setq py-install-directory erg)
      (setq py-install-directory (expand-file-name "~/")))
    (when (and py-verbose-p (interactive-p)) (message "Setting py-install-directory to: %s" py-install-directory))
    py-install-directory))

(defun py-set-load-path ()
  "Include needed subdirs of python-mode directory. "
  (interactive)
  (let ((py-install-directory (py-normalize-directory py-install-directory)))
    (cond ((and (not (string= "" py-install-directory))(stringp py-install-directory))
           (add-to-list 'load-path (expand-file-name py-install-directory))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "completion"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "extensions"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "test"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "tools"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "autopair")))
          ((when py-guess-py-install-directory-p
             (let ((guessed-py-install-directory (py-guess-py-install-directory)))
               (when guessed-py-install-directory
                 (add-to-list 'load-path guessed-py-install-directory)))))
          (t (error "Please set `py-install-directory', see INSTALL"))
          (when (interactive-p) (message "%s" load-path)))))

;;;
(defvar python-mode-map)
(setq python-mode-map
      (let ((map (make-sparse-keymap)))
        ;; electric keys
        (define-key map [(:)] 'py-electric-colon)
        (define-key map [(\#)] 'py-electric-comment)
        (define-key map [(delete)] 'py-electric-delete)
        (define-key map [(backspace)] 'py-electric-backspace)
        (define-key map [(control backspace)] 'py-hungry-delete-backwards)
        (define-key map [(control c) (delete)] 'py-hungry-delete-forward)
        ;; (define-key map [(control y)] 'py-electric-yank)
        ;; moving point
        (define-key map [(control c)(control p)] 'py-beginning-of-statement)
        (define-key map [(control c)(control n)] 'py-end-of-statement)
        (define-key map [(control c)(control u)] 'py-beginning-of-block)
        (define-key map [(control c)(control q)] 'py-end-of-block)
        (define-key map [(control meta a)] 'py-beginning-of-def-or-class)
        (define-key map [(control meta e)] 'py-end-of-def-or-class)

        ;; (define-key map [(meta i)] 'py-indent-forward-line)
        (define-key map [(control j)] 'py-newline-and-indent)
        ;; Most Pythoneers expect RET `py-newline-and-indent'
        ;; (define-key map (kbd "RET") 'py-newline-and-dedent)
        (define-key map (kbd "RET") py-return-key)
        ;; (define-key map (kbd "RET") 'newline)
        (define-key map [(super backspace)] 'py-dedent)
        ;; (define-key map [(control return)] 'py-newline-and-dedent)
        ;; indentation level modifiers
        (define-key map [(control c)(control l)] 'py-shift-left)
        (define-key map [(control c)(control r)] 'py-shift-right)
        (define-key map [(control c)(<)] 'py-shift-left)
        (define-key map [(control c)(>)] 'py-shift-right)
        (define-key map [(control c)(tab)] 'py-indent-region)
        (define-key map [(control c)(:)] 'py-guess-indent-offset)
        ;; subprocess commands
        (define-key map [(control c)(control c)] 'py-execute-buffer)
        (define-key map [(control c)(control m)] 'py-execute-import-or-reload)
        (define-key map [(control c)(control s)] 'py-execute-string)
        (define-key map [(control c)(|)] 'py-execute-region)
        (define-key map [(control meta x)] 'py-execute-def-or-class)
        (define-key map [(control c)(!)] 'py-shell)
        (define-key map [(control c)(control t)] 'py-toggle-shell)
        (define-key map [(control meta h)] 'py-mark-def-or-class)
        (define-key map [(control c)(control k)] 'py-mark-block-or-clause)
        (define-key map [(control c)(.)] 'py-expression)
        ;; Miscellaneous
        (define-key map [(super q)] 'py-copy-statement)
        (define-key map [(control c)(control d)] 'py-pdbtrack-toggle-stack-tracking)
        (define-key map [(control c)(control f)] 'py-sort-imports)
        (define-key map [(control c)(\#)] 'py-comment-region)
        (define-key map [(control c)(\?)] 'py-describe-mode)
        (define-key map [(control c)(control e)] 'py-describe-symbol)
        (define-key map [(control c)(-)] 'py-up-exception)
        (define-key map [(control c)(=)] 'py-down-exception)
        (define-key map [(control x) (n) (d)] 'py-narrow-to-defun)
        ;; information
        (define-key map [(control c)(control b)] 'py-submit-bug-report)
        (define-key map [(control c)(control v)] 'py-version)
        (define-key map [(control c)(control w)] 'py-pychecker-run)
        (define-key map (kbd "TAB") 'py-indent-line)
        ;; (if py-complete-function
        ;;     (progn
        ;;       (define-key map [(meta tab)] py-complete-function)
        ;;       (define-key map [(esc) (tab)] py-complete-function))
        ;;   (define-key map [(meta tab)] 'py-shell-complete)
        ;;   (define-key map [(esc) (tab)] 'py-shell-complete))
        (substitute-key-definition 'complete-symbol 'completion-at-point
                                   map global-map)
        (substitute-key-definition 'backward-up-list 'py-up
                                   map global-map)
        (substitute-key-definition 'down-list 'py-down
                                   map global-map)

        (and (ignore-errors (require 'easymenu) t)
             ;; (easy-menu-define py-menu map "Python Tools"
             ;;           `("PyTools"
             (easy-menu-define
               py-menu map "Python Mode menu"
               `("Python"
                 ("Interpreter"
                  ["Default interpreter..." py-shell
                   :help " `py-shell'

Start an interactive Python interpreter.

Interactively, C-u 4 prompts for a buffer.
C-u 2 prompts for `py-python-command-args'.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg\.

. "]
                  ("Other"
                   :help "Alternative Python Shells"

                   ["ipython" ipython
                    :help "`ipython'
Start an IPython interpreter.

Optional C-u prompts for options to pass to the IPython interpreter. See `py-python-command-args'."]
                   ["python3" python3
                    :help "`python3'
Start an Python3 interpreter.

Optional C-u prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."]
                   ["python2" python2
                    :help "`python2'
Start an Python2 interpreter.

Optional C-u prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."]
                   ["python2.7" python2.7
                    :help "`python2.7'
Start an Python2.7 interpreter.

Optional C-u prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."]
                   ["jython" jython
                    :help "`jython'
Start an Jython interpreter.

Optional C-u prompts for options to pass to the Jython interpreter. See `py-python-command-args'."]
                   ["python3.2" python3.2
                    :help "`python3.2'
Start an Python3.2 interpreter.

Optional C-u prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."]

                   ["python3.3" python3.3
                    :help "`python3.3'
Start an Python3.3 interpreter.

Optional C-u prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'."]

                   "-"
                   ["python-dedicated" python-dedicated
                    :help "`python-dedicated'
Start an unique Python interpreter in another window.

Optional C-u prompts for options to pass to the Python interpreter. See `py-python-command-args'."]
                   ["ipython-dedicated" ipython-dedicated
                    :help "`ipython-dedicated'
Start an unique IPython interpreter in another window.

Optional C-u prompts for options to pass to the IPython interpreter. See `py-python-command-args'."]
                   ["python3-dedicated" python3-dedicated
                    :help "`python3-dedicated'
Start an unique Python3 interpreter in another window.

Optional C-u prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."]
                   ["python2-dedicated" python2-dedicated
                    :help "`python2-dedicated'
Start an unique Python2 interpreter in another window.

Optional C-u prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."]
                   ["python2.7-dedicated" python2.7-dedicated
                    :help "`python2'.7-dedicated
Start an unique Python2.7 interpreter in another window.

Optional C-u prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."]
                   ["jython-dedicated" jython-dedicated
                    :help "`jython-dedicated'
Start an unique Jython interpreter in another window.

Optional C-u prompts for options to pass to the Jython interpreter. See `py-python-command-args'."]
                   ["python3.2-dedicated" python3.2-dedicated
                    :help "`python3.2-dedicated'
Start an unique Python3.2 interpreter in another window.

Optional C-u prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."]
                   "-"
                   ("Ignoring defaults "
                    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"

                    ["Execute file python switch" py-execute-file-python-switch
                     :help " `py-execute-file-python-switch'
Send file to a Python interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python no-switch" py-execute-file-python-no-switch
                     :help " `py-execute-file-python-no-switch'
Send file to a Python interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python dedicated" py-execute-file-python-dedicated
                     :help " `py-execute-file-python-dedicated'
Send file to a Python interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file python dedicated switch" py-execute-file-python-dedicated-switch
                     :help " `py-execute-file-python-dedicated-switch'
Send file to a Python interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file ipython switch" py-execute-file-ipython-switch
                     :help " `py-execute-file-ipython-switch'
Send file to a Ipython interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file ipython no-switch" py-execute-file-ipython-no-switch
                     :help " `py-execute-file-ipython-no-switch'
Send file to a Ipython interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file ipython dedicated" py-execute-file-ipython-dedicated
                     :help " `py-execute-file-ipython-dedicated'
Send file to a Ipython interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file ipython dedicated switch" py-execute-file-ipython-dedicated-switch
                     :help " `py-execute-file-ipython-dedicated-switch'
Send file to a Ipython interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3 switch" py-execute-file-python3-switch
                     :help " `py-execute-file-python3-switch'
Send file to a Python3 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3 no-switch" py-execute-file-python3-no-switch
                     :help " `py-execute-file-python3-no-switch'
Send file to a Python3 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python3 dedicated" py-execute-file-python3-dedicated
                     :help " `py-execute-file-python3-dedicated'
Send file to a Python3 interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file python3 dedicated switch" py-execute-file-python3-dedicated-switch
                     :help " `py-execute-file-python3-dedicated-switch'
Send file to a Python3 interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2 switch" py-execute-file-python2-switch
                     :help " `py-execute-file-python2-switch'
Send file to a Python2 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2 no-switch" py-execute-file-python2-no-switch
                     :help " `py-execute-file-python2-no-switch'
Send file to a Python2 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python2 dedicated" py-execute-file-python2-dedicated
                     :help " `py-execute-file-python2-dedicated'
Send file to a Python2 interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file python2 dedicated switch" py-execute-file-python2-dedicated-switch
                     :help " `py-execute-file-python2-dedicated-switch'
Send file to a Python2 interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2.7 switch" py-execute-file-python2.7-switch
                     :help " `py-execute-file-python2.7-switch'
Send file to a Python2\.7 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2.7 no-switch" py-execute-file-python2.7-no-switch
                     :help " `py-execute-file-python2.7-no-switch'
Send file to a Python2\.7 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python2.7 dedicated" py-execute-file-python2.7-dedicated
                     :help " `py-execute-file-python2.7-dedicated'
Send file to a Python2\.7 interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file python2.7 dedicated switch" py-execute-file-python2.7-dedicated-switch
                     :help " `py-execute-file-python2.7-dedicated-switch'
Send file to a Python2\.7 interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file jython switch" py-execute-file-jython-switch
                     :help " `py-execute-file-jython-switch'
Send file to a Jython interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file jython no-switch" py-execute-file-jython-no-switch
                     :help " `py-execute-file-jython-no-switch'
Send file to a Jython interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file jython dedicated" py-execute-file-jython-dedicated
                     :help " `py-execute-file-jython-dedicated'
Send file to a Jython interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file jython dedicated switch" py-execute-file-jython-dedicated-switch
                     :help " `py-execute-file-jython-dedicated-switch'
Send file to a Jython interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.2 switch" py-execute-file-python3.2-switch
                     :help " `py-execute-file-python3.2-switch'
Send file to a Python3\.2 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.2 no-switch" py-execute-file-python3.2-no-switch
                     :help " `py-execute-file-python3.2-no-switch'
Send file to a Python3\.2 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python3.2 dedicated" py-execute-file-python3.2-dedicated
                     :help " `py-execute-file-python3.2-dedicated'
Send file to a Python3\.2 interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file python3.2 dedicated switch" py-execute-file-python3.2-dedicated-switch
                     :help " `py-execute-file-python3.2-dedicated-switch'
Send file to a Python3\.2 interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.3 switch" py-execute-file-python3.3-switch
                     :help " `py-execute-file-python3.3-switch'
Send file to a Python3\.3 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.3 no-switch" py-execute-file-python3.3-no-switch
                     :help " `py-execute-file-python3.3-no-switch'
Send file to a Python3\.3 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python3.3 dedicated" py-execute-file-python3.3-dedicated
                     :help " `py-execute-file-python3.3-dedicated'
Send file to a Python3\.3 interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file python3.3 dedicated switch" py-execute-file-python3.3-dedicated-switch
                     :help " `py-execute-file-python3.3-dedicated-switch'
Send file to a Python3\.3 interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file bpython switch" py-execute-file-bpython-switch
                     :help " `py-execute-file-bpython-switch'
Send file to a Bpython interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file bpython no-switch" py-execute-file-bpython-no-switch
                     :help " `py-execute-file-bpython-no-switch'
Send file to a Bpython interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file bpython dedicated" py-execute-file-bpython-dedicated
                     :help " `py-execute-file-bpython-dedicated'
Send file to a Bpython interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file bpython dedicated switch" py-execute-file-bpython-dedicated-switch
                     :help " `py-execute-file-bpython-dedicated-switch'
Send file to a Bpython interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]
                    )

                   )
                  )

                 "-"
                 ("Mark"

                  ["Mark block" py-mark-block
                   :help " `py-mark-block'

Mark block at point\.

Returns beginning and end positions of marked area, a cons\. "]

                  ["Mark def or class" py-mark-def-or-class
                   :help " `py-mark-def-or-class'

Mark def-or-class at point\.

Returns beginning and end positions of marked area, a cons\. "]

                  ["Mark statement" py-mark-statement
                   :help "`py-mark-statement'
Mark statement at point"]

                  ["Mark clause" py-mark-clause
                   :help "`py-mark-clause'
Mark innermost compound statement at point"]

                  ["Mark def" py-mark-def
                   :help "`py-mark-def'
Mark innermost definition at point"]
                  ["Mark expression" py-mark-expression
                   :help "`py-mark-expression'
Mark expression at point"]
                  ["Mark partial expression" py-mark-partial-expression
                   :help "`py-mark-partial-expression'
\".\" operators delimit a partial-expression expression on it's level"]

                  ["Mark class" py-mark-class
                   :help "`py-mark-class'
Mark innermost definition at point"]

                  ["Mark comment" py-mark-comment
                   :help "`py-mark-comment'
Mark commented section at point"]

                  "-"

                  ["Mark block bol" py-mark-block-bol
                   :help "`py-mark-block-bol'
Mark block at point reaching beginning-of-line. "]

                  ["Mark clause bol" py-mark-clause-bol
                   :help "`py-mark-clause-bol'
Mark clause at point reaching beginning-of-line. "]

                  ["Mark block-or-clause bol" py-mark-block-or-clause-bol
                   :help "`py-mark-block-or-clause-bol'
Mark block-or-clause at point reaching beginning-of-line. "]

                  ["Mark def bol" py-mark-def-bol
                   :help "`py-mark-def-bol'
Mark def at point reaching beginning-of-line. "]

                  ["Mark class bol" py-mark-class-bol
                   :help "`py-mark-class-bol'
Mark class at point reaching beginning-of-line. "]

                  ["Mark def-or-class bol" py-mark-def-or-class-bol
                   :help "`py-mark-def-or-class-bol'
Mark def-or-class at point reaching beginning-of-line. "]

                  ["Mark if-block bol" py-mark-if-block-bol
                   :help "`py-mark-if-block-bol'
Mark if-block at point reaching beginning-of-line. "]

                  ["Mark try-block bol" py-mark-try-block-bol
                   :help "`py-mark-try-block-bol'
Mark try-block at point reaching beginning-of-line. "]

                  ["Mark minor-block bol" py-mark-minor-block-bol
                   :help "`py-mark-minor-block-bol'
Mark minor-block at point reaching beginning-of-line. "]

                  )
                 "-"
                 ["Shift region left"    py-shift-region-left]
                 ["Shift region right"   py-shift-region-right]
                 "-"
                 ("Comment"
                  ["Comment Region"   py-comment-region (point) (mark)
                   :help "Like `comment-region' but uses double hash (`#') comment starter." ]
                  ["Uncomment" py-uncomment
                   :help " `py-uncomment'

Uncomment commented lines at point\.

If region is active, restrict uncommenting at region . "]

                  ["Uncomment Region"     (py-comment-region (point) (mark) '(4))
                   :help "(py-comment-region (point) (mark) '(4))" ]
                  "-"
                  ["Comment block" py-comment-block
                   :help " `py-comment-block'
Comments block at point\.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment clause" py-comment-clause
                   :help " `py-comment-clause'
Comments clause at point\.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment block or clause" py-comment-block-or-clause
                   :help " `py-comment-block-or-clause'
Comments block-or-clause at point\.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment def" py-comment-def
                   :help " `py-comment-def'
Comments def at point\.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment class" py-comment-class
                   :help " `py-comment-class'
Comments class at point\.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment def or class" py-comment-def-or-class
                   :help " `py-comment-def-or-class'
Comments def-or-class at point\.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment statement" py-comment-statement
                   :help " `py-comment-statement'
Comments statement at point\.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  )
                 "-"
                 ("Move"

                  ["Beginning of top level" py-beginning-of-top-level
                   :help " `py-beginning-of-top-level'

Go to the very beginning of current block. "]

                  ["Beginning of block" py-beginning-of-block
                   :help " `py-beginning-of-block'

Go to beginning block, skip whitespace at BOL\. "]
                  ["Go to end of block" py-end-of-block]
                  "-"

                  ["Beginning of statement" py-beginning-of-statement
                   :help " `py-beginning-of-statement'

Go to the initial line of a simple statement. "]


                  ["End of statement" py-end-of-statement
                   :help " `py-end-of-statement'

Go to the last char of current statement.

To go just beyond the final line of the current statement, use `py-down-statement-bol'. . "]

                  "-"
                  ["Go to start of def or class" (py-beginning-of-def-or-class t) t]
                  ["Move to end of def or class" (py-end-of-def-or-class t) t]
                  "-"
                  ["Move to start of def" py-beginning-of-def t]
                  ["Move to end of def"   py-end-of-def t]
                  "-"

                  ["Beginning of clause" py-beginning-of-clause
                   :help " `py-beginning-of-clause'

Go to beginning clause, skip whitespace at BOL\. "]

                  ["End of clause" py-end-of-clause
                   :help " `py-end-of-clause'

Go to end of clause\. "]
                  "-"
                  ["Beginning of comment" py-beginning-of-comment
                   :help " `py-beginning-of-comment'
Go to beginning of comment at point. "]
                  ["End of comment" py-end-of-comment
                   :help " `py-end-of-comment'

Go to end of comment at point. "]

                  "-"
                  ["Backward into nomenclature" py-backward-into-nomenclature
                   :help " `py-backward-into-nomenclature'
Go backward into nomenclature

A nomenclature is a fancy way of saying AWordWithMixedCaseNotUnderscores. "]
                  ["Forward into nomenclature" py-forward-into-nomenclature
                   :help " `py-forward-into-nomenclature'
Go forward into nomenclature

A nomenclature is a fancy way of saying AWordWithMixedCaseNotUnderscores. "]
                  "-"
                  ["Go to start of expression" (py-beginning-of-expression t) t]
                  ["Move to end of expression" (py-end-of-expression t) t]
                  "-"
                  "-"
                  ["Go to start of minor-expression" (py-beginning-of-minor-expression t) t]
                  ["Move to end of minor-expression" (py-end-of-minor-expression t) t]
                  "-"

                  ["Up level" py-up
                   :help " `py-up'
Go to beginning one level above of compound statement or definition at point. "]
                  "-"
                  ["Down level" py-down
                   :help " `py-down'
Go to beginning one level below of compound statement or definition at point. "]

                  )
                 "-"
                 ("Copy "
                  ["Copy statement" py-copy-statement
                   :help "`py-copy-statement'
Copy statement at point"]
                  ["Copy clause" py-copy-clause
                   :help "`py-copy-clause'
Copy innermost compound statement at point"]

                  ["Copy block" py-copy-block
                   :help "`py-copy-block'
Copy innermost compound statement at point"]

                  ["Copy def" py-copy-def
                   :help "`py-copy-def'
Copy innermost definition at point"]
                  ["Copy expression" py-copy-expression
                   :help "`py-copy-expression'
Copy expression at point"]
                  ["Copy partial expression" py-copy-partial-expression
                   :help "`py-copy-partial-expression'
\".\" operators delimit a partial-expression expression on it's level"]
                  ["Copy class" py-copy-class
                   :help "`py-copy-class'
Copy innermost definition at point"]

                  ["Copy Def-or-Class" py-copy-def-or-class
                   :help "`py-copy-def-or-class'
Copy innermost definition at point"])
                 "-"

                 ["Execute region" py-execute-region
                  :help " `py-execute-region'

Send the region to a Python interpreter\.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell\. This might be the name of a system-wide shell or include the path to a virtual environment\. "]

                 ["Execute buffer" py-execute-buffer
                  :help " `py-execute-buffer'

Send the contents of the buffer to a Python interpreter\.

When called with C-u, execution through `default-value' of `py-shell-name' is forced\.
When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell\. This might be the name of a system-wide shell or include the path to a virtual environment\.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file\."]

                 ["Execute def or class" py-execute-def-or-class
                  :help " `py-execute-def-or-class'

Send def-or-class at point to a Python interpreter\.

When called with C-u, execution through `default-value' of `py-shell-name' is forced\.
See also `py-force-py-shell-name-p'\.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell\. This might be the name of a system-wide shell or include the path to a virtual environment\."]

                 ["Execute statement" py-execute-statement
                  :help " `py-execute-statement'

Send statement at point to a Python interpreter\.

When called with C-u, execution through `default-value' of `py-shell-name' is forced\.
See also `py-force-py-shell-name-p'\.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell\. This might be the name of a system-wide shell or include the path to a virtual environment\."]

                 ["Execute string" py-execute-string
                  :help " `py-execute-string'

Send the argument STRING to a Python interpreter\.

See also `py-execute-region'\. . "]
                 ("More... "
                  :help "Python-specific features"

                  ["Execute block" py-execute-block
                   :help "`py-execute-block'
       Send block at point to Python interpreter. "]

                  ["Execute def" py-execute-def
                   :help "`py-execute-def'
       Send def at point to Python interpreter. "]

                  ["Execute class" py-execute-class
                   :help "`py-execute-class'
       Send class at point to Python interpreter. "]

                  ["Execute file" py-execute-file
                   :help "`py-execute-file'
       Send file at point to Python interpreter. "]

                  ;; statement
                  ("Execute statement "
                   :help "Execute statement functions"

                   ["py-execute-statement-python" py-execute-statement-python
                    :help "Execute statement through a Python interpreter.
        With \\[universal-argument] use an unique Python interpreter. "]

                   ["py-execute-statement-ipython" py-execute-statement-ipython
                    :help "Execute statement through an IPython interpreter.
        With \\[universal-argument] use an unique IPython interpreter. "]

                   ["py-execute-statement-python3" py-execute-statement-python3
                    :help "Execute statement through a Python3 interpreter.
        With \\[universal-argument] use an unique Python3 interpreter. "]

                   ["py-execute-statement-python2" py-execute-statement-python2
                    :help "Execute statement through a Python2 interpreter.
        With \\[universal-argument] use an unique Python2 interpreter. "]

                   ["py-execute-statement-python2.7" py-execute-statement-python2.7
                    :help "Execute statement through a Python2.7 interpreter.
        With \\[universal-argument] use an unique Python2.7 interpreter. "]

                   ["py-execute-statement-jython" py-execute-statement-jython
                    :help "Execute statement through a Jython interpreter.
        With \\[universal-argument] use an unique Jython interpreter. "]

                   ["py-execute-statement-python3.2" py-execute-statement-python3.2
                    :help "Execute statement through a Python3.2 interpreter.
        With \\[universal-argument] use an unique Python3.2 interpreter. "]

                   ["py-execute-statement-python3.3" py-execute-statement-python3.3
                    :help "Execute statement through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-statement-bpython" py-execute-statement-bpython
                    :help "Execute statement through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ;; dedicated

                   ["py-execute-statement-python-dedicated" py-execute-statement-python-dedicated
                    :help "Execute statement through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-statement-ipython-dedicated" py-execute-statement-ipython-dedicated
                    :help "Execute statement through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-statement-python3-dedicated" py-execute-statement-python3-dedicated
                    :help "Execute statement through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-statement-python2-dedicated" py-execute-statement-python2-dedicated
                    :help "Execute statement through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-statement-python2.7-dedicated" py-execute-statement-python2.7-dedicated
                    :help "Execute statement through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-statement-jython-dedicated" py-execute-statement-jython-dedicated
                    :help "Execute statement through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-statement-python3.2-dedicated" py-execute-statement-python3.2-dedicated
                    :help "Execute statement through a unique Python3.2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-statement-python3.3-dedicated" py-execute-statement-python3.3-dedicated
                    :help "Execute statement through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-statement-bpython-dedicated" py-execute-statement-bpython-dedicated
                    :help "Execute statement through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ("Ignoring defaults "
                    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"            ;; switch

                    ["py-execute-statement-python-switch" py-execute-statement-python-switch
                     :help "Execute statement through a Python interpreter.
With \\[universal-argument] use an unique Python interpreter. "]

                    ["py-execute-statement-ipython-switch" py-execute-statement-ipython-switch
                     :help "Execute statement through an IPython interpreter.
With \\[universal-argument] use an unique IPython interpreter. "]

                    ["py-execute-statement-python3-switch" py-execute-statement-python3-switch
                     :help "Execute statement through a Python3 interpreter.
With \\[universal-argument] use an unique Python3 interpreter. "]

                    ["py-execute-statement-python2-switch" py-execute-statement-python2-switch
                     :help "Execute statement through a Python2 interpreter.
With \\[universal-argument] use an unique Python2 interpreter. "]

                    ["py-execute-statement-python2.7-switch" py-execute-statement-python2.7-switch
                     :help "Execute statement through a Python2.7 interpreter.
With \\[universal-argument] use an unique Python2.7 interpreter. "]

                    ["py-execute-statement-jython-switch" py-execute-statement-jython-switch
                     :help "Execute statement through a Jython interpreter.
With \\[universal-argument] use an unique Jython interpreter. "]

                    ["py-execute-statement-python3.2-switch" py-execute-statement-python3.2-switch
                     :help "Execute statement through a Python3.2 interpreter.
With \\[universal-argument] use an unique Python3.2 interpreter. "]

                    ["py-execute-statement-python3.3-switch" py-execute-statement-python3.3-switch
                     :help "Execute statement through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

                    ["py-execute-statement-bpython-switch" py-execute-statement-bpython-switch
                     :help "Execute statement through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
                    ;; dedicated-switch

                    ["py-execute-statement-python-dedicated-switch" py-execute-statement-python-dedicated-switch
                     :help "Execute statement through a unique Python interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-ipython-dedicated-switch" py-execute-statement-ipython-dedicated-switch
                     :help "Execute statement through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-python3-dedicated-switch" py-execute-statement-python3-dedicated-switch
                     :help "Execute statement through a unique Python3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-python2-dedicated-switch" py-execute-statement-python2-dedicated-switch
                     :help "Execute statement through a unique Python2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-python2.7-dedicated-switch" py-execute-statement-python2.7-dedicated-switch
                     :help "Execute statement through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-jython-dedicated-switch" py-execute-statement-jython-dedicated-switch
                     :help "Execute statement through a unique Jython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-python3.2-dedicated-switch" py-execute-statement-python3.2-dedicated-switch
                     :help "Execute statement through a unique Python3.2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-python3.3-dedicated-switch" py-execute-statement-python3.3-dedicated-switch
                     :help "Execute statement through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-bpython-dedicated-switch" py-execute-statement-bpython-dedicated-switch
                     :help "Execute statement through a unique Bpython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]
                    ))

                  ;; block
                  ("Execute block "
                   :help "Execute block functions"

                   ["py-execute-block-python" py-execute-block-python
                    :help "Execute block through a Python interpreter.
        With \\[universal-argument] use an unique Python interpreter. "]

                   ["py-execute-block-ipython" py-execute-block-ipython
                    :help "Execute block through an IPython interpreter.
        With \\[universal-argument] use an unique IPython interpreter. "]

                   ["py-execute-block-python3" py-execute-block-python3
                    :help "Execute block through a Python3 interpreter.
        With \\[universal-argument] use an unique Python3 interpreter. "]

                   ["py-execute-block-python2" py-execute-block-python2
                    :help "Execute block through a Python2 interpreter.
        With \\[universal-argument] use an unique Python2 interpreter. "]

                   ["py-execute-block-python2.7" py-execute-block-python2.7
                    :help "Execute block through a Python2.7 interpreter.
        With \\[universal-argument] use an unique Python2.7 interpreter. "]

                   ["py-execute-block-jython" py-execute-block-jython
                    :help "Execute block through a Jython interpreter.
        With \\[universal-argument] use an unique Jython interpreter. "]

                   ["py-execute-block-python3.2" py-execute-block-python3.2
                    :help "Execute block through a Python3.2 interpreter.
        With \\[universal-argument] use an unique Python3.2 interpreter. "]

                   ["py-execute-block-python3.3" py-execute-block-python3.3
                    :help "Execute block through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-block-bpython" py-execute-block-bpython
                    :help "Execute block through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ;; dedicated

                   ["py-execute-block-python-dedicated" py-execute-block-python-dedicated
                    :help "Execute block through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-block-ipython-dedicated" py-execute-block-ipython-dedicated
                    :help "Execute block through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-block-python3-dedicated" py-execute-block-python3-dedicated
                    :help "Execute block through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-block-python2-dedicated" py-execute-block-python2-dedicated
                    :help "Execute block through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-block-python2.7-dedicated" py-execute-block-python2.7-dedicated
                    :help "Execute block through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-block-jython-dedicated" py-execute-block-jython-dedicated
                    :help "Execute block through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-block-python3.2-dedicated" py-execute-block-python3.2-dedicated
                    :help "Execute block through a unique Python3.2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-block-python3.3-dedicated" py-execute-block-python3.3-dedicated
                    :help "Execute block through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-block-bpython-dedicated" py-execute-block-bpython-dedicated
                    :help "Execute block through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]
                   ("Ignoring defaults "
                    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"            ;; switch

                    ["py-execute-block-python-switch" py-execute-block-python-switch
                     :help "Execute block through a Python interpreter.
With \\[universal-argument] use an unique Python interpreter. "]

                    ["py-execute-block-ipython-switch" py-execute-block-ipython-switch
                     :help "Execute block through an IPython interpreter.
With \\[universal-argument] use an unique IPython interpreter. "]

                    ["py-execute-block-python3-switch" py-execute-block-python3-switch
                     :help "Execute block through a Python3 interpreter.
With \\[universal-argument] use an unique Python3 interpreter. "]

                    ["py-execute-block-python2-switch" py-execute-block-python2-switch
                     :help "Execute block through a Python2 interpreter.
With \\[universal-argument] use an unique Python2 interpreter. "]

                    ["py-execute-block-python2.7-switch" py-execute-block-python2.7-switch
                     :help "Execute block through a Python2.7 interpreter.
With \\[universal-argument] use an unique Python2.7 interpreter. "]

                    ["py-execute-block-jython-switch" py-execute-block-jython-switch
                     :help "Execute block through a Jython interpreter.
With \\[universal-argument] use an unique Jython interpreter. "]

                    ["py-execute-block-python3.2-switch" py-execute-block-python3.2-switch
                     :help "Execute block through a Python3.2 interpreter.
With \\[universal-argument] use an unique Python3.2 interpreter. "]

                    ["py-execute-block-python3.3-switch" py-execute-block-python3.3-switch
                     :help "Execute block through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

                    ["py-execute-block-bpython-switch" py-execute-block-bpython-switch
                     :help "Execute block through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
                    ;; dedicated-switch

                    ["py-execute-block-python-dedicated-switch" py-execute-block-python-dedicated-switch
                     :help "Execute block through a unique Python interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-ipython-dedicated-switch" py-execute-block-ipython-dedicated-switch
                     :help "Execute block through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-python3-dedicated-switch" py-execute-block-python3-dedicated-switch
                     :help "Execute block through a unique Python3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-python2-dedicated-switch" py-execute-block-python2-dedicated-switch
                     :help "Execute block through a unique Python2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-python2.7-dedicated-switch" py-execute-block-python2.7-dedicated-switch
                     :help "Execute block through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-jython-dedicated-switch" py-execute-block-jython-dedicated-switch
                     :help "Execute block through a unique Jython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-python3.2-dedicated-switch" py-execute-block-python3.2-dedicated-switch
                     :help "Execute block through a unique Python3.2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-python3.3-dedicated-switch" py-execute-block-python3.3-dedicated-switch
                     :help "Execute block through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-bpython-dedicated-switch" py-execute-block-bpython-dedicated-switch
                     :help "Execute block through a unique Bpython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]
                    ))

                  ;; def
                  ("Execute def "
                   :help "Execute def functions"

                   ["py-execute-def-python" py-execute-def-python
                    :help "Execute def through a Python interpreter.
        With \\[universal-argument] use an unique Python interpreter. "]

                   ["py-execute-def-ipython" py-execute-def-ipython
                    :help "Execute def through an IPython interpreter.
        With \\[universal-argument] use an unique IPython interpreter. "]

                   ["py-execute-def-python3" py-execute-def-python3
                    :help "Execute def through a Python3 interpreter.
        With \\[universal-argument] use an unique Python3 interpreter. "]

                   ["py-execute-def-python2" py-execute-def-python2
                    :help "Execute def through a Python2 interpreter.
        With \\[universal-argument] use an unique Python2 interpreter. "]

                   ["py-execute-def-python2.7" py-execute-def-python2.7
                    :help "Execute def through a Python2.7 interpreter.
        With \\[universal-argument] use an unique Python2.7 interpreter. "]

                   ["py-execute-def-jython" py-execute-def-jython
                    :help "Execute def through a Jython interpreter.
        With \\[universal-argument] use an unique Jython interpreter. "]

                   ["py-execute-def-python3.2" py-execute-def-python3.2
                    :help "Execute def through a Python3.2 interpreter.
        With \\[universal-argument] use an unique Python3.2 interpreter. "]

                   ["py-execute-def-python3.3" py-execute-def-python3.3
                    :help "Execute def through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-def-bpython" py-execute-def-bpython
                    :help "Execute def through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ;; dedicated

                   ["py-execute-def-python-dedicated" py-execute-def-python-dedicated
                    :help "Execute def through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-def-ipython-dedicated" py-execute-def-ipython-dedicated
                    :help "Execute def through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-def-python3-dedicated" py-execute-def-python3-dedicated
                    :help "Execute def through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-def-python2-dedicated" py-execute-def-python2-dedicated
                    :help "Execute def through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-def-python2.7-dedicated" py-execute-def-python2.7-dedicated
                    :help "Execute def through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-def-jython-dedicated" py-execute-def-jython-dedicated
                    :help "Execute def through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-def-python3.2-dedicated" py-execute-def-python3.2-dedicated
                    :help "Execute def through a unique Python3.2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-def-python3.3-dedicated" py-execute-def-python3.3-dedicated
                    :help "Execute def through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-def-bpython-dedicated" py-execute-def-bpython-dedicated
                    :help "Execute def through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]
                   ("Ignoring defaults "
                    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"            ;; switch

                    ["py-execute-def-python-switch" py-execute-def-python-switch
                     :help "Execute def through a Python interpreter.
With \\[universal-argument] use an unique Python interpreter. "]

                    ["py-execute-def-ipython-switch" py-execute-def-ipython-switch
                     :help "Execute def through an IPython interpreter.
With \\[universal-argument] use an unique IPython interpreter. "]

                    ["py-execute-def-python3-switch" py-execute-def-python3-switch
                     :help "Execute def through a Python3 interpreter.
With \\[universal-argument] use an unique Python3 interpreter. "]

                    ["py-execute-def-python2-switch" py-execute-def-python2-switch
                     :help "Execute def through a Python2 interpreter.
With \\[universal-argument] use an unique Python2 interpreter. "]

                    ["py-execute-def-python2.7-switch" py-execute-def-python2.7-switch
                     :help "Execute def through a Python2.7 interpreter.
With \\[universal-argument] use an unique Python2.7 interpreter. "]

                    ["py-execute-def-jython-switch" py-execute-def-jython-switch
                     :help "Execute def through a Jython interpreter.
With \\[universal-argument] use an unique Jython interpreter. "]

                    ["py-execute-def-python3.2-switch" py-execute-def-python3.2-switch
                     :help "Execute def through a Python3.2 interpreter.
With \\[universal-argument] use an unique Python3.2 interpreter. "]

                    ["py-execute-def-python3.3-switch" py-execute-def-python3.3-switch
                     :help "Execute def through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

                    ["py-execute-def-bpython-switch" py-execute-def-bpython-switch
                     :help "Execute def through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
                    ;; dedicated-switch

                    ["py-execute-def-python-dedicated-switch" py-execute-def-python-dedicated-switch
                     :help "Execute def through a unique Python interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-ipython-dedicated-switch" py-execute-def-ipython-dedicated-switch
                     :help "Execute def through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-python3-dedicated-switch" py-execute-def-python3-dedicated-switch
                     :help "Execute def through a unique Python3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-python2-dedicated-switch" py-execute-def-python2-dedicated-switch
                     :help "Execute def through a unique Python2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-python2.7-dedicated-switch" py-execute-def-python2.7-dedicated-switch
                     :help "Execute def through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-jython-dedicated-switch" py-execute-def-jython-dedicated-switch
                     :help "Execute def through a unique Jython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-python3.2-dedicated-switch" py-execute-def-python3.2-dedicated-switch
                     :help "Execute def through a unique Python3.2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-python3.3-dedicated-switch" py-execute-def-python3.3-dedicated-switch
                     :help "Execute def through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-bpython-dedicated-switch" py-execute-def-bpython-dedicated-switch
                     :help "Execute def through a unique Bpython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]
                    ))

                  ;; class
                  ("Execute class "
                   :help "Execute class functions"

                   ["py-execute-class-python" py-execute-class-python
                    :help "Execute class through a Python interpreter.
        With \\[universal-argument] use an unique Python interpreter. "]

                   ["py-execute-class-ipython" py-execute-class-ipython
                    :help "Execute class through an IPython interpreter.
        With \\[universal-argument] use an unique IPython interpreter. "]

                   ["py-execute-class-python3" py-execute-class-python3
                    :help "Execute class through a Python3 interpreter.
        With \\[universal-argument] use an unique Python3 interpreter. "]

                   ["py-execute-class-python2" py-execute-class-python2
                    :help "Execute class through a Python2 interpreter.
        With \\[universal-argument] use an unique Python2 interpreter. "]

                   ["py-execute-class-python2.7" py-execute-class-python2.7
                    :help "Execute class through a Python2.7 interpreter.
        With \\[universal-argument] use an unique Python2.7 interpreter. "]

                   ["py-execute-class-jython" py-execute-class-jython
                    :help "Execute class through a Jython interpreter.
        With \\[universal-argument] use an unique Jython interpreter. "]

                   ["py-execute-class-python3.2" py-execute-class-python3.2
                    :help "Execute class through a Python3.2 interpreter.
        With \\[universal-argument] use an unique Python3.2 interpreter. "]

                   ["py-execute-class-python3.3" py-execute-class-python3.3
                    :help "Execute class through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-class-bpython" py-execute-class-bpython
                    :help "Execute class through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ;; dedicated

                   ["py-execute-class-python-dedicated" py-execute-class-python-dedicated
                    :help "Execute class through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-class-ipython-dedicated" py-execute-class-ipython-dedicated
                    :help "Execute class through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-class-python3-dedicated" py-execute-class-python3-dedicated
                    :help "Execute class through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-class-python2-dedicated" py-execute-class-python2-dedicated
                    :help "Execute class through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-class-python2.7-dedicated" py-execute-class-python2.7-dedicated
                    :help "Execute class through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-class-jython-dedicated" py-execute-class-jython-dedicated
                    :help "Execute class through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-class-python3.2-dedicated" py-execute-class-python3.2-dedicated
                    :help "Execute class through a unique Python3.2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-class-python3.3-dedicated" py-execute-class-python3.3-dedicated
                    :help "Execute class through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-class-bpython-dedicated" py-execute-class-bpython-dedicated
                    :help "Execute class through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]
                   ("Ignoring defaults "
                    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"            ;; switch

                    ["py-execute-class-python-switch" py-execute-class-python-switch
                     :help "Execute class through a Python interpreter.
With \\[universal-argument] use an unique Python interpreter. "]

                    ["py-execute-class-ipython-switch" py-execute-class-ipython-switch
                     :help "Execute class through an IPython interpreter.
With \\[universal-argument] use an unique IPython interpreter. "]

                    ["py-execute-class-python3-switch" py-execute-class-python3-switch
                     :help "Execute class through a Python3 interpreter.
With \\[universal-argument] use an unique Python3 interpreter. "]

                    ["py-execute-class-python2-switch" py-execute-class-python2-switch
                     :help "Execute class through a Python2 interpreter.
With \\[universal-argument] use an unique Python2 interpreter. "]

                    ["py-execute-class-python2.7-switch" py-execute-class-python2.7-switch
                     :help "Execute class through a Python2.7 interpreter.
With \\[universal-argument] use an unique Python2.7 interpreter. "]

                    ["py-execute-class-jython-switch" py-execute-class-jython-switch
                     :help "Execute class through a Jython interpreter.
With \\[universal-argument] use an unique Jython interpreter. "]

                    ["py-execute-class-python3.2-switch" py-execute-class-python3.2-switch
                     :help "Execute class through a Python3.2 interpreter.
With \\[universal-argument] use an unique Python3.2 interpreter. "]

                    ["py-execute-class-python3.3-switch" py-execute-class-python3.3-switch
                     :help "Execute class through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

                    ["py-execute-class-bpython-switch" py-execute-class-bpython-switch
                     :help "Execute class through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
                    ;; dedicated-switch

                    ["py-execute-class-python-dedicated-switch" py-execute-class-python-dedicated-switch
                     :help "Execute class through a unique Python interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-ipython-dedicated-switch" py-execute-class-ipython-dedicated-switch
                     :help "Execute class through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-python3-dedicated-switch" py-execute-class-python3-dedicated-switch
                     :help "Execute class through a unique Python3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-python2-dedicated-switch" py-execute-class-python2-dedicated-switch
                     :help "Execute class through a unique Python2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-python2.7-dedicated-switch" py-execute-class-python2.7-dedicated-switch
                     :help "Execute class through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-jython-dedicated-switch" py-execute-class-jython-dedicated-switch
                     :help "Execute class through a unique Jython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-python3.2-dedicated-switch" py-execute-class-python3.2-dedicated-switch
                     :help "Execute class through a unique Python3.2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-python3.3-dedicated-switch" py-execute-class-python3.3-dedicated-switch
                     :help "Execute class through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-bpython-dedicated-switch" py-execute-class-bpython-dedicated-switch
                     :help "Execute class through a unique Bpython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]
                    ))

                  ;; region
                  ("Execute region "
                   :help "Execute region functions"

                   ["py-execute-region-python" py-execute-region-python
                    :help "Execute region through a Python interpreter.
        With \\[universal-argument] use an unique Python interpreter. "]

                   ["py-execute-region-ipython" py-execute-region-ipython
                    :help "Execute region through an IPython interpreter.
        With \\[universal-argument] use an unique IPython interpreter. "]

                   ["py-execute-region-python3" py-execute-region-python3
                    :help "Execute region through a Python3 interpreter.
        With \\[universal-argument] use an unique Python3 interpreter. "]

                   ["py-execute-region-python2" py-execute-region-python2
                    :help "Execute region through a Python2 interpreter.
        With \\[universal-argument] use an unique Python2 interpreter. "]

                   ["py-execute-region-python2.7" py-execute-region-python2.7
                    :help "Execute region through a Python2.7 interpreter.
        With \\[universal-argument] use an unique Python2.7 interpreter. "]

                   ["py-execute-region-jython" py-execute-region-jython
                    :help "Execute region through a Jython interpreter.
        With \\[universal-argument] use an unique Jython interpreter. "]

                   ["py-execute-region-python3.2" py-execute-region-python3.2
                    :help "Execute region through a Python3.2 interpreter.
        With \\[universal-argument] use an unique Python3.2 interpreter. "]

                   ["py-execute-region-python3.3" py-execute-region-python3.3
                    :help "Execute region through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-region-bpython" py-execute-region-bpython
                    :help "Execute region through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ;; dedicated

                   ["py-execute-region-python-dedicated" py-execute-region-python-dedicated
                    :help "Execute region through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-region-ipython-dedicated" py-execute-region-ipython-dedicated
                    :help "Execute region through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-region-python3-dedicated" py-execute-region-python3-dedicated
                    :help "Execute region through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-region-python2-dedicated" py-execute-region-python2-dedicated
                    :help "Execute region through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-region-python2.7-dedicated" py-execute-region-python2.7-dedicated
                    :help "Execute region through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-region-jython-dedicated" py-execute-region-jython-dedicated
                    :help "Execute region through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-region-python3.2-dedicated" py-execute-region-python3.2-dedicated
                    :help "Execute region through a unique Python3.2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-region-python3.3-dedicated" py-execute-region-python3.3-dedicated
                    :help "Execute region through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

                   ["py-execute-region-bpython-dedicated" py-execute-region-bpython-dedicated
                    :help "Execute region through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]
                   ("Ignoring defaults "
                    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"            ;; switch

                    ["py-execute-region-python-switch" py-execute-region-python-switch
                     :help "Execute region through a Python interpreter.
With \\[universal-argument] use an unique Python interpreter. "]

                    ["py-execute-region-ipython-switch" py-execute-region-ipython-switch
                     :help "Execute region through an IPython interpreter.
With \\[universal-argument] use an unique IPython interpreter. "]

                    ["py-execute-region-python3-switch" py-execute-region-python3-switch
                     :help "Execute region through a Python3 interpreter.
With \\[universal-argument] use an unique Python3 interpreter. "]

                    ["py-execute-region-python2-switch" py-execute-region-python2-switch
                     :help "Execute region through a Python2 interpreter.
With \\[universal-argument] use an unique Python2 interpreter. "]

                    ["py-execute-region-python2.7-switch" py-execute-region-python2.7-switch
                     :help "Execute region through a Python2.7 interpreter.
With \\[universal-argument] use an unique Python2.7 interpreter. "]

                    ["py-execute-region-jython-switch" py-execute-region-jython-switch
                     :help "Execute region through a Jython interpreter.
With \\[universal-argument] use an unique Jython interpreter. "]

                    ["py-execute-region-python3.2-switch" py-execute-region-python3.2-switch
                     :help "Execute region through a Python3.2 interpreter.
With \\[universal-argument] use an unique Python3.2 interpreter. "]

                    ["py-execute-region-python3.3-switch" py-execute-region-python3.3-switch
                     :help "Execute region through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

                    ["py-execute-region-bpython-switch" py-execute-region-bpython-switch
                     :help "Execute region through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
                    ;; dedicated-switch

                    ["py-execute-region-python-dedicated-switch" py-execute-region-python-dedicated-switch
                     :help "Execute region through a unique Python interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-ipython-dedicated-switch" py-execute-region-ipython-dedicated-switch
                     :help "Execute region through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-python3-dedicated-switch" py-execute-region-python3-dedicated-switch
                     :help "Execute region through a unique Python3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-python2-dedicated-switch" py-execute-region-python2-dedicated-switch
                     :help "Execute region through a unique Python2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-python2.7-dedicated-switch" py-execute-region-python2.7-dedicated-switch
                     :help "Execute region through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-jython-dedicated-switch" py-execute-region-jython-dedicated-switch
                     :help "Execute region through a unique Jython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-python3.2-dedicated-switch" py-execute-region-python3.2-dedicated-switch
                     :help "Execute region through a unique Python3.2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-python3.3-dedicated-switch" py-execute-region-python3.3-dedicated-switch
                     :help "Execute region through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-bpython-dedicated-switch" py-execute-region-bpython-dedicated-switch
                     :help "Execute region through a unique Bpython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]
                    ))

                  ;; file
                  ("Execute file "
                   :help "Execute file functions"

                   ["Execute file python" py-execute-file-python
                    :help " `py-execute-file-python'
Send file to a Python interpreter\.. "]

                   ["Execute file ipython" py-execute-file-ipython
                    :help " `py-execute-file-ipython'
Send file to a Ipython interpreter\.. "]

                   ["Execute file python3" py-execute-file-python3
                    :help " `py-execute-file-python3'
Send file to a Python3 interpreter\.. "]

                   ["Execute file python2" py-execute-file-python2
                    :help " `py-execute-file-python2'
Send file to a Python2 interpreter\.. "]

                   ["Execute file python2.7" py-execute-file-python2.7
                    :help " `py-execute-file-python2.7'
Send file to a Python2\.7 interpreter\.. "]

                   ["Execute file jython" py-execute-file-jython
                    :help " `py-execute-file-jython'
Send file to a Jython interpreter\.. "]

                   ["Execute file python3.2" py-execute-file-python3.2
                    :help " `py-execute-file-python3.2'
Send file to a Python3\.2 interpreter\.. "]

                   ["Execute file python3.3" py-execute-file-python3.3
                    :help " `py-execute-file-python3.3'
Send file to a Python3\.3 interpreter\.. "]

                   ["Execute file bpython" py-execute-file-bpython
                    :help " `py-execute-file-bpython'
Send file to a Bpython interpreter\.. "]

                   ("Ignoring defaults "
                    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"

                    ["Execute file python switch" py-execute-file-python-switch
                     :help " `py-execute-file-python-switch'
Send file to a Python interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python no-switch" py-execute-file-python-no-switch
                     :help " `py-execute-file-python-no-switch'
Send file to a Python interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python dedicated" py-execute-file-python-dedicated
                     :help " `py-execute-file-python-dedicated'
Send file to a Python interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file python dedicated switch" py-execute-file-python-dedicated-switch
                     :help " `py-execute-file-python-dedicated-switch'
Send file to a Python interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file ipython switch" py-execute-file-ipython-switch
                     :help " `py-execute-file-ipython-switch'
Send file to a Ipython interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file ipython no-switch" py-execute-file-ipython-no-switch
                     :help " `py-execute-file-ipython-no-switch'
Send file to a Ipython interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file ipython dedicated" py-execute-file-ipython-dedicated
                     :help " `py-execute-file-ipython-dedicated'
Send file to a Ipython interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file ipython dedicated switch" py-execute-file-ipython-dedicated-switch
                     :help " `py-execute-file-ipython-dedicated-switch'
Send file to a Ipython interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3 switch" py-execute-file-python3-switch
                     :help " `py-execute-file-python3-switch'
Send file to a Python3 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3 no-switch" py-execute-file-python3-no-switch
                     :help " `py-execute-file-python3-no-switch'
Send file to a Python3 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python3 dedicated" py-execute-file-python3-dedicated
                     :help " `py-execute-file-python3-dedicated'
Send file to a Python3 interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file python3 dedicated switch" py-execute-file-python3-dedicated-switch
                     :help " `py-execute-file-python3-dedicated-switch'
Send file to a Python3 interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2 switch" py-execute-file-python2-switch
                     :help " `py-execute-file-python2-switch'
Send file to a Python2 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2 no-switch" py-execute-file-python2-no-switch
                     :help " `py-execute-file-python2-no-switch'
Send file to a Python2 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python2 dedicated" py-execute-file-python2-dedicated
                     :help " `py-execute-file-python2-dedicated'
Send file to a Python2 interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file python2 dedicated switch" py-execute-file-python2-dedicated-switch
                     :help " `py-execute-file-python2-dedicated-switch'
Send file to a Python2 interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2.7 switch" py-execute-file-python2.7-switch
                     :help " `py-execute-file-python2.7-switch'
Send file to a Python2\.7 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2.7 no-switch" py-execute-file-python2.7-no-switch
                     :help " `py-execute-file-python2.7-no-switch'
Send file to a Python2\.7 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python2.7 dedicated" py-execute-file-python2.7-dedicated
                     :help " `py-execute-file-python2.7-dedicated'
Send file to a Python2\.7 interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file python2.7 dedicated switch" py-execute-file-python2.7-dedicated-switch
                     :help " `py-execute-file-python2.7-dedicated-switch'
Send file to a Python2\.7 interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file jython switch" py-execute-file-jython-switch
                     :help " `py-execute-file-jython-switch'
Send file to a Jython interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file jython no-switch" py-execute-file-jython-no-switch
                     :help " `py-execute-file-jython-no-switch'
Send file to a Jython interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file jython dedicated" py-execute-file-jython-dedicated
                     :help " `py-execute-file-jython-dedicated'
Send file to a Jython interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file jython dedicated switch" py-execute-file-jython-dedicated-switch
                     :help " `py-execute-file-jython-dedicated-switch'
Send file to a Jython interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.2 switch" py-execute-file-python3.2-switch
                     :help " `py-execute-file-python3.2-switch'
Send file to a Python3\.2 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.2 no-switch" py-execute-file-python3.2-no-switch
                     :help " `py-execute-file-python3.2-no-switch'
Send file to a Python3\.2 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python3.2 dedicated" py-execute-file-python3.2-dedicated
                     :help " `py-execute-file-python3.2-dedicated'
Send file to a Python3\.2 interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file python3.2 dedicated switch" py-execute-file-python3.2-dedicated-switch
                     :help " `py-execute-file-python3.2-dedicated-switch'
Send file to a Python3\.2 interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.3 switch" py-execute-file-python3.3-switch
                     :help " `py-execute-file-python3.3-switch'
Send file to a Python3\.3 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.3 no-switch" py-execute-file-python3.3-no-switch
                     :help " `py-execute-file-python3.3-no-switch'
Send file to a Python3\.3 interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python3.3 dedicated" py-execute-file-python3.3-dedicated
                     :help " `py-execute-file-python3.3-dedicated'
Send file to a Python3\.3 interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file python3.3 dedicated switch" py-execute-file-python3.3-dedicated-switch
                     :help " `py-execute-file-python3.3-dedicated-switch'
Send file to a Python3\.3 interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file bpython switch" py-execute-file-bpython-switch
                     :help " `py-execute-file-bpython-switch'
Send file to a Bpython interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file bpython no-switch" py-execute-file-bpython-no-switch
                     :help " `py-execute-file-bpython-no-switch'
Send file to a Bpython interpreter\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file bpython dedicated" py-execute-file-bpython-dedicated
                     :help " `py-execute-file-bpython-dedicated'
Send file to a Bpython interpreter\.

Uses a dedicated shell\.. "]

                    ["Execute file bpython dedicated switch" py-execute-file-bpython-dedicated-switch
                     :help " `py-execute-file-bpython-dedicated-switch'
Send file to a Bpython interpreter\.

Uses a dedicated shell\.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "])))
                 "-"
                 ("Virtualenv"

                  ["Virtualenv workon" virtualenv-workon
                   :help " runs `virtualenv-workon'

Make sure virtualenv is provided

"]

                  ["Virtualenv activate" virtualenv-activate
                   :help " `virtualenv-activate'

Activate the virtualenv located in DIR. "]

                  ["Virtualenv deactivate" virtualenv-deactivate
                   :help " `virtualenv-deactivate'

Deactivate the current virtual enviroment. "]

                  ["Virtualenv p" virtualenv-p
                   :help " `virtualenv-p'

Check if a directory is a virtualenv. "]

                  )

                 ["Execute import or reload" py-execute-import-or-reload
                  :help " `py-execute-import-or-reload'

Import the current buffer's file in a Python interpreter\.

If the file has already been imported, then do reload instead to get
the latest version\.

If the file's name does not end in "\.py", then do execfile instead\.

If the current buffer is not visiting a file, do `py-execute-buffer'
instead\.

If the file local variable `py-master-file' is non-nil, import or
reload the named file instead of the buffer's file\.  The file may be
saved based on the value of `py-execute-import-or-reload-save-p'\.

See also `M-x py-execute-region'\.

This may be preferable to `M-x py-execute-buffer' because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE\.NAME)\.

 - The Python debugger gets line number information about the functions\.. "]

                 ("Help"

                  ["Describe mode"        py-describe-mode t]

                  ["Help on symbol" py-describe-symbol
                   :help "`py-describe-symbol'\n
Use pydoc on symbol at point"]

                  ;;          ["py-complete-help" py-complete-help
                  ;;           :help " `py-complete-help'
                  ;; Get help on a Python expression.\n
                  ;; Needs Pymacs "]
                  ;;
                  ;;          ["Help thing at point" py-complete-help-thing-at-point
                  ;;           :help " `py-complete-help-thing-at-point'\n
                  ;; Needs Pymacs "]

                  ;;          ["Signature" py-complete-signature-expr
                  ;;           :help " Print object's signature\n
                  ;; Needs Pymacs"]

                  )

                 ["Debugger" pdb :help "`pdb' Run pdb under GUD"]
                 ("Checks"

                  ["pychecker-run" py-pychecker-run
                   :help "`py-pychecker-run'
Run pychecker

Call `easy_install pyflakes' resp. `pip... 'if not available"]

                  ("Pylint "
                   :help "Extendet report options

Call `easy_install pylint' resp. `pip...' if not available"

                   ["py-pylint-run" py-pylint-run
                    :help "`py-pylint-run'
Pylint will display a number of messages as it analyzes the code,
as well as some statistics about the number of warnings and
errors found in different files - unless called with arg \"--errors-only\". The messages are classified
under various categories such as errors and warnings

Pylint checks length of lines of code, if variable names are
well-formed according to your coding standard, if declared
interfaces are truly implemented, and much more. Additionally, it
is possible to write plugins.

Call `easy_install pylint' resp. `pip...' if not available
"]

                   ["py-pylint-help" py-pylint-help
                    :help "`py-pylint-help'
List extendet report options
"]
                   ["pylint-flymake-mode" pylint-flymake-mode
                    :help "`pylint-flymake-mode'
Toggle flymake-mode running `pylint'
"])

                  ("pep8 "
                   :help "Check formatting

Call `easy_install pep8' resp. `pip...' if not available"

                   ["pep8-run" py-pep8-run
                    :help "`py-pep8-run'
Check formatting (default on the file currently visited)

Call `easy_install pep8' resp. `pip...' if not available
"]

                   ["pep8-help" py-pep8-help
                    :help "`py-pep8-help'
Display help for pep8 format checker)
"]

                   ["pep8-flymake-mode" pep8-flymake-mode
                    :help "`pep8-flymake-mode'
Toggle flymake-mode running `pep8'
"])

                  ("Pyflakes " :help "Non intrusive code checker

Call `easy_install pyflakes' resp. `pip...' if not available"

                   ["pyflakes-run" py-pyflakes-run :help
                    "`py-pyflakes-run' Run pyflakes

Call `easy_install pyflakes' resp. `pip...' if not available"]

                   ["pyflakes-help" py-pyflakes-help :help
                    "`py-pyflakes-help' Display help for
              Pyflakes "]

                   ["pyflakes-flymake-mode" pyflakes-flymake-mode :help
                    "`pyflakes-flymake-mode'
Toggle flymake-mode running `pyflakes' "])

                  ("Pyflakes-pep8 " :help
                   "Non intrusive code checker running `pyflakes' and `pep8'
call `easy_install pyflakes' resp. `pip...' and `easy_install pep8' if basics not available"

                   ["pyflakespep8-run" py-pyflakespep8-run :help
                    "`py-pyflakespep8-run' Run `pyflakespep8'

Call `easy_install pyflakes' resp. `pip...' if not available"]

                   ["pyflakespep8-help" py-pyflakespep8-help :help
                    "`py-pyflakespep8-help' Display help for
              Pyflakespep8 "]

                   ["pyflakespep8-flymake-mode" pyflakespep8-flymake-mode :help
                    "`pyflakespep8-flymake-mode'
Toggle flymake-mode running `pyflakespep8' "])

                  )
                 ("Customize"

                  ["Python-mode customize group" (customize-group 'python-mode)
                   :help "Open the customization buffer for Python mode"]
                  ("Switches"
                   :help "Toggle useful modes like `highlight-indentation'"

                   ("Interpreter"

                    ["Execute without temporary file"
                     (setq py-execute-no-temp-p
                           (not py-execute-no-temp-p))
                     :help " `py-execute-no-temp-p'
Seems Emacs-24\.3 provided a way executing stuff without temporary files.
In experimental state yet "
                     :style toggle :selected py-execute-no-temp-p]

                    ["Enforce py-shell-name" force-py-shell-name-p-on
                     :help "Enforce customized default `py-shell-name' should upon execution. "]

                    ["Don't enforce default interpreter" force-py-shell-name-p-off
                     :help "Make execute commands guess interpreter from environment"]

                    ["Enforce local Python shell " py-force-local-shell-on
                     :help "Locally indicated Python being enforced upon sessions execute commands. "]

                    ["Remove local Python shell enforcement, restore default" py-force-local-shell-off
                     :help "Restore `py-shell-name' default value and `behaviour'. "]


                    ["Run `py-shell' at start"
                     (setq py-start-run-py-shell
                           (not py-start-run-py-shell))
                     :help "If `python-mode' should start a python-shell, `py-shell'\.

Default is `nil'\. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-start-run-py-shell]

                    )

                   ("Execute"
                    ["Enforce py-output-buffer"
                     (setq py-enforce-output-buffer-p
                           (not py-enforce-output-buffer-p))
                     :help " `py-enforce-output-buffer-p'

When non-nil, value of `py-output-buffer' is used for output,
regardless of environment\. Default is nil."
                     :style toggle :selected py-enforce-output-buffer-p]

                    ["Execute \"if name == main\" blocks p"
                     (setq py-if-name-main-permission-p
                           (not py-if-name-main-permission-p))
                     :help " `py-if-name-main-permission-p'

Allow execution of code inside blocks delimited by
if __name__ == '__main__'

Default is non-nil. "
                     :style toggle :selected py-if-name-main-permission-p]

                    ["Store result" py-store-result-p
                     :help " `py-store-result-p'

When non-nil, put resulting string of `py-execute-\.\.\.' into kill-ring, so it might be yanked\. . "
                     :style toggle :selected py-store-result-p]

                    )

                   ("TAB related"

                    ["indent-tabs-mode"
                     (setq indent-tabs-mode
                           (not indent-tabs-mode))
                     :help "Indentation can insert tabs if this is non-nil\.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected indent-tabs-mode]

                    ["Tab shifts region "
                     (setq py-tab-shifts-region-p
                           (not py-tab-shifts-region-p))
                     :help "If `t', TAB will indent/cycle the region, not just the current line\.

Default is nil
See also `py-tab-indents-region-p'

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-tab-shifts-region-p]

                    ["Tab indents region "
                     (setq py-tab-indents-region-p
                           (not py-tab-indents-region-p))
                     :help "When `t' and first TAB doesn't shift, indent-region is called\.

Default is nil
See also `py-tab-shifts-region-p'

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-tab-indents-region-p]

                    )

                   ("Filling"

                    ("Docstring styles"
                     :help "Toggle values of `py-docstring-style'
In order to set permanently customize this variable"

                     ("Nil"
                      :help "Toggle nil value of `py-docstring-style'
Use `M-x customize-variable' to set it permanently"

                      ["Toggle nil docstring style" toggle-py-nil-docstring-style
                       :help "If nil docstring-style should be on or off
  Returns value of `py-docstring-style' switched to

Use `M-x customize-variable' to set it permanently"]

                      ["Nil on" py-nil-docstring-style-on
                       :help "Make sure, nil docstring-style is on

Use `M-x customize-variable' to set it permanently"]

                      ["Nil off" py-nil-docstring-style-off
                       :help "Restores default value of `py-docstring-style'

Use `M-x customize-variable' to set it permanently"])

                     ("Onetwo"
                      :help "Toggle onetwo value of `py-docstring-style'
In order to set permanently customize this variable"

                      ["Toggle onetwo docstring style" toggle-py-onetwo-docstring-style
                       :help "If onetwo docstring-style should be on or off
  Returns value of `py-docstring-style' switched to

Use `M-x customize-variable' to set it permanently"]

                      ["Onetwo on" py-onetwo-docstring-style-on
                       :help "Make sure, onetwo docstring-style is on

Use `M-x customize-variable' to set it permanently"]

                      ["Onetwo off" py-onetwo-docstring-style-off
                       :help " Restores default value of `py-docstring-style'

Use `M-x customize-variable' to set it permanently"])

                     ("Pep 257"
                      :help "Toggle pep-257 value of `py-docstring-style'
In order to set permanently customize this variable"

                      ["Toggle pep 257 docstring style" toggle-py-pep-257-docstring-style
                       :help "If pep-257 docstring-style should be on or off
  Returns value of `py-docstring-style' switched to

Use `M-x customize-variable' to set it permanently"]

                      ["Pep 257 on" py-pep-257-docstring-style-on
                       :help "Make sure, pep-257 docstring-style is on

Use `M-x customize-variable' to set it permanently"]

                      ["Pep 257 off" py-pep-257-docstring-style-off
                       :help " Restores default value of `py-docstring-style'

Use `M-x customize-variable' to set it permanently"])

                     ("Pep 257 nn"
                      :help "Toggle pep-257-nn value of `py-docstring-style'
In order to set permanently customize this variable"

                      ["Toggle pep 257 nn docstring style" toggle-py-pep-257-nn-docstring-style
                       :help "If pep-257-nn docstring-style should be on or off
  Returns value of `py-docstring-style' switched to

Use `M-x customize-variable' to set it permanently"]

                      ["Pep 257 nn on" py-pep-257-nn-docstring-style-on
                       :help "Make sure, pep-257-nn docstring-style is on

Use `M-x customize-variable' to set it permanently"]

                      ["Pep 257 nn off" py-pep-257-nn-docstring-style-off
                       :help " Restores default value of `py-docstring-style'

Use `M-x customize-variable' to set it permanently"])

                     ("Symmetric"
                      :help "Toggle symmetric value of `py-docstring-style'
In order to set permanently customize this variable"

                      ["Toggle symmetric docstring style" toggle-py-symmetric-docstring-style
                       :help "If symmetric docstring-style should be on or off
  Returns value of `py-docstring-style' switched to

Use `M-x customize-variable' to set it permanently"]

                      ["Symmetric on" py-symmetric-docstring-style-on
                       :help "Make sure, symmetric docstring-style is on

Use `M-x customize-variable' to set it permanently"]

                      ["Symmetric off" py-symmetric-docstring-style-off
                       :help " Restores default value of `py-docstring-style'

Use `M-x customize-variable' to set it permanently"])

                     ("Django"
                      :help "Toggle django value of `py-docstring-style'
In order to set permanently customize this variable"

                      ["Toggle django docstring style" toggle-py-django-docstring-style
                       :help "If django docstring-style should be on or off
  Returns value of `py-docstring-style' switched to

Use `M-x customize-variable' to set it permanently"]

                      ["Django on" py-django-docstring-style-on
                       :help "Make sure, django docstring-style is on

Use `M-x customize-variable' to set it permanently"]

                      ["Django off" py-django-docstring-style-off
                       :help "Restores default value of `py-docstring-style'

Use `M-x customize-variable' to set it permanently"])
                     )

                    ["Fill-paragraph fill docstring "
                     (setq py-paragraph-fill-docstring-p
                           (not py-paragraph-fill-docstring-p))
                     :help "If `py-fill-paragraph', when inside a docstring, should fill the complete string\.

Default is nil\.

Convenient use of `M-q' inside docstrings
See also `py-docstring-style'
Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-paragraph-fill-docstring-p]

                    ["Auto-fill mode"
                     (setq py-set-fill-column-p
                           (not py-set-fill-column-p))
                     :help "Set Python specific `fill-column' according to `py-docstring-fill-column' and `py-comment-fill-column'

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-set-fill-column-p]

                    ["Use current dir when execute"
                     (setq py-use-current-dir-when-execute-p
                           (not py-use-current-dir-when-execute-p))
                     :help " `toggle-py-use-current-dir-when-execute-p'

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-use-current-dir-when-execute-p]

                    )

                   ("Indent"

                    ["Electric colon"
                     (setq py-electric-colon-active-p
                           (not py-electric-colon-active-p))
                     :help " `py-electric-colon-active-p'

`py-electric-colon' feature\.  Default is `nil'\. See lp:837065 for discussions\. . "
                     :style toggle :selected py-electric-colon-active-p]

                    ["Electric colon at beginning of block only"
                     (setq py-electric-colon-bobl-only
                           (not py-electric-colon-bobl-only))
                     :help "When inserting a colon, do not indent lines unless at beginning of block.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-electric-colon-bobl-only]

                    ["Indent comment "
                     (setq py-indent-comments
                           (not py-indent-comments))
                     :help "If comments should be indented like code. Default is `nil'.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-indent-comments]

                    ["Indent honors inline comment"
                     (setq py-indent-honors-inline-comment
                           (not py-indent-honors-inline-comment))
                     :help "If non-nil, indents to column of inlined comment start\.
Default is nil\. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-indent-honors-inline-comment]

                    ("Smart indentation"
                     :help "Toggle py-smart-indentation'

Use `M-x customize-variable' to set it permanently"

                     ["Toggle py-smart-indentation" toggle-py-smart-indentation
                      :help "Toggles py-smart-indentation

Use `M-x customize-variable' to set it permanently"]

                     ["py-smart-indentation on" py-smart-indentation-on
                      :help "Switches py-smart-indentation on

Use `M-x customize-variable' to set it permanently"]

                     ["py-smart-indentation off" py-smart-indentation-off
                      :help "Switches py-smart-indentation off

Use `M-x customize-variable' to set it permanently"]

                     )

                    ["Highlight indentation" highlight-indentation
                     :help "Toggle highlight indentation\.

Use `M-x customize-variable' to set it permanently

Make sure `highlight-indentation' is  installed"

                     ]

                    ["Electric comment "
                     (setq py-electric-comment-p
                           (not py-electric-comment-p))
                     :help "If \"#\" should call `py-electric-comment'\. Default is `nil'\.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-electric-comment-p]

                    )

                   ("Underscore word syntax"
                    :help "Toggle `py-underscore-word-syntax-p'"

                    ["Toggle underscore word syntax" toggle-py-underscore-word-syntax-p
                     :help " `toggle-py-underscore-word-syntax-p'

If `py-underscore-word-syntax-p' should be on or off\.

  Returns value of `py-underscore-word-syntax-p' switched to\. .

Use `M-x customize-variable' to set it permanently"]

                    ["Underscore word syntax on" py-underscore-word-syntax-p-on
                     :help " `py-underscore-word-syntax-p-on'

Make sure, py-underscore-word-syntax-p' is on\.

Returns value of `py-underscore-word-syntax-p'\. .

Use `M-x customize-variable' to set it permanently"]

                    ["Underscore word syntax off" py-underscore-word-syntax-p-off
                     :help " `py-underscore-word-syntax-p-off'

Make sure, `py-underscore-word-syntax-p' is off\.

Returns value of `py-underscore-word-syntax-p'\. .

Use `M-x customize-variable' to set it permanently"]

                    )

                   ("Exception"

                    ["Jump on exception"
                     (setq py-jump-on-exception
                           (not py-jump-on-exception))
                     :help "Jump to innermost exception frame in Python output buffer\.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame\.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-jump-on-exception]


                    ["Highlight error in source "
                     (setq py-highlight-error-source-p
                           (not py-highlight-error-source-p           ))
                     :help "Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-highlight-error-source-p]

                    )

                   ["Switch buffers on execute"
                    (setq py-switch-buffers-on-execute-p
                          (not py-switch-buffers-on-execute-p))
                    :help "When non-nil switch to the Python output buffer\.

Use `M-x customize-variable' to set it permanently"
                    :style toggle :selected py-switch-buffers-on-execute-p]

                   ["Split windows on execute"
                    (setq py-split-windows-on-execute-p
                          (not py-split-windows-on-execute-p))
                    :help "When non-nil split windows\.

Use `M-x customize-variable' to set it permanently"
                    :style toggle :selected py-split-windows-on-execute-p]

                   ["Python mode v5 behavior"
                    (setq python-mode-v5-behavior-p
                          (not python-mode-v5-behavior-p))
                    :help "Execute region through `shell-command-on-region' as
v5 did it - lp:990079\. This might fail with certain chars - see UnicodeEncodeError lp:550661

Use `M-x customize-variable' to set it permanently"
                    :style toggle :selected python-mode-v5-behavior-p]

                   ("Autopair mode"
                    :help "Toggle `autopair-mode'"

                    ["Toggle autopair mode" toggle-py-autopair-mode
                     :help " `toggle-autopair-mode'

If `autopair-mode' should be on or off\.

  Returns value of `autopair-mode ' switched to\. . "]

                    ["Autopair mode on" py-autopair-mode-on
                     :help " `autopair-mode on'

Make sure, `autopair-mode' is on\.

Returns value of `autopair-mode'\. . "]

                    ["Autopair mode off" py-autopair-mode-off
                     :help " `autopair-mode' off

Make sure, `autopair-mode' is off\.

Returns value of `autopair-mode'\. . "]

                    )

                   ["Switch index-function" py-switch-imenu-index-function
                    :help "`py-switch-imenu-index-function'
Switch between `py-imenu-create-index' from 5.1 series and `py-imenu-create-index-new'."]

                   ;; py-smart-operator-mode-p forms
                   ("Smart operator mode"
                    :help "Toggle `smart-operator-mode'"

                    ["Toggle smart operator mode" toggle-py-smart-operator-mode-p
                     :help " `toggle-smart-operator-mode'

If `smart-operator-mode' should be on or off\.

  Returns value of `smart-operator-mode ' switched to\. . "]

                    ["Smart operator mode on" py-smart-operator-mode-p-on
                     :help " `smart-operator-mode -on'

Make sure, `smart-operator-mode' is on\.

Returns value of `smart-operator-mode'\. . "]

                    ["Smart operator mode off" py-smart-operator-mode-p-off
                     :help " `smart-operator-mode' off

Make sure, `smart-operator-mode' is off\.

Returns value of `smart-operator-mode'\. . "]

                    )
                   )
                  )
                 ("More... "
                  ("Edit commands "

                   ("Kill "

                    ["Kill statement" py-kill-statement
                     :help "`py-kill-statement'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill clause" py-kill-clause
                     :help "`py-kill-clause'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill block" py-kill-block
                     :help "`py-kill-block'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill def-or-class" py-kill-def-or-class
                     :help "`py-kill-def-or-class'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill expression" py-kill-expression
                     :help "`py-kill-expression'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill partial-expression" py-kill-partial-expression
                     :help "`py-kill-partial-expression'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill class" py-kill-class
                     :help "`py-kill-class'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill def" py-kill-def
                     :help "`py-kill-def'
Delete innermost compound statement at point, store deleted string in kill-ring"])
                   ("Delete "
                    ["Delete block" py-delete-block
                     :help "`py-delete-block'
Delete innermost compound statement at point, don't store deleted string in kill-ring"]

                    ["Delete def-or-class" py-delete-def-or-class
                     :help "`py-delete-def-or-class'
Delete def-or-class at point, don't store deleted string in kill-ring"]

                    ["Delete clause" py-delete-clause
                     :help "`py-delete-clause'
Delete innermost compound statement at point, don't store deleted string in kill-ring"]
                    ["Delete statement" py-delete-statement
                     :help "`py-delete-statement'
Delete statement at point, don't store deleted string in kill-ring"]

                    ["Delete expression" py-delete-expression
                     :help "`py-delete-expression'
Delete expression at point, don't store deleted string in kill-ring"]

                    ["Delete partial-expression" py-delete-partial-expression
                     :help "`py-delete-partial-expression'
Delete partial-expression at point, don't store deleted string in kill-ring"]

                    ["Delete class" py-delete-class
                     :help "`py-delete-class'
Delete class at point, don't store deleted string in kill-ring"]

                    ["Delete def" py-delete-def
                     :help "`py-delete-def'
Delete def at point, don't store deleted string in kill-ring"])

                   ("Shift right "
                    ["Shift block right" py-shift-block-right
                     :help "`py-shift-block-right'
Shift block right. "]

                    ["Shift clause right" py-shift-clause-right
                     :help "`py-shift-clause-right'
Shift clause right. "]

                    ["Shift statement right" py-shift-statement-right
                     :help "`py-shift-statement-right'
Shift statement right. "]

                    ["Shift def-or-class right" py-shift-def-or-class-right
                     :help "`py-shift-def-or-class-right'
Shift def-or-class right. "]

                    ["Shift class right" py-shift-class-right
                     :help "`py-shift-class-right'
Shift class right. "]

                    ["Shift def right" py-shift-def-right
                     :help "`py-shift-def-right'
Shift def right. "]

                    ["Shift block-or-clause right" py-shift-block-or-clause-right
                     :help "`py-shift-block-or-clause-right'
Shift block-or-clause right. "])

                   ("Shift left "
                    ["Shift block left" py-shift-block-left
                     :help "`py-shift-block-left'
Shift block left. "]

                    ["Shift clause left" py-shift-clause-left
                     :help "`py-shift-clause-left'
Shift clause left. "]

                    ["Shift statement left" py-shift-statement-left
                     :help "`py-shift-statement-left'
Shift statement left. "]

                    ["Shift def-or-class left" py-shift-def-or-class-left
                     :help "`py-shift-def-or-class-left'
Shift def-or-class left. "]

                    ["Shift class left" py-shift-class-left
                     :help "`py-shift-class-left'
Shift class left. "]

                    ["Shift def left" py-shift-def-left
                     :help "`py-shift-def-left'
Shift def left. "]

                    ["Shift block-or-clause left" py-shift-block-or-clause-left
                     :help "`py-shift-block-or-clause-left'
Shift block-or-clause left. "]

                    )
                   ("More"
                    :help "extended edit commands'"

                    ["Empty out list backward" py-empty-out-list-backward
                     :help " `py-empty-out-list-backward'
Deletes all elements from list before point. "]

                    ["Revert boolean assignent" py-boolswitch
                     :help " `py-boolswitch'
Edit the assigment of a boolean variable, rever them.

I.e. switch it from \"True\" to \"False\" and vice versa "]

                    ["Remove overlays at point" py-remove-overlays-at-point
                     :help " `py-remove-overlays-at-point'

Remove overlays as set when `py-highlight-error-source-p' is non-nil\. . "]

                    )

                   )

                  "-"
                  ("Forms "
                   ("Comment"

                    ["Beginning of comment" py-beginning-of-comment
                     :help " `py-beginning-of-comment'
Go to beginning of comment at point. "]

                    ["End of comment" py-end-of-comment
                     :help " `py-end-of-comment'

Go to end of comment at point. "]
                    )
                   ("Block"
                    ["Beginning of block" py-beginning-of-block
                     :help "`py-beginning-of-block'
Go to start of innermost compound statement at point"]
                    ["End of block" py-end-of-block
                     :help "`py-end-of-block'
Go to end of innermost compound statement at point"]

                    ["Down block" py-down-block
                     :help "`py-down-block'

Go to the beginning of next block below in buffer.

Returns indentation if block found, nil otherwise. "]

                    ["Up block" py-up-block
                     :help "`py-up-block'

Go upwards to the beginning of next block below in buffer.

Returns indentation if block found, nil otherwise. "]

                    ["Copy block" py-copy-block
                     :help "`py-copy-block'
Copy innermost compound statement at point"]

                    ["Kill block" py-kill-block
                     :help "`py-kill-block'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Delete block" py-delete-block
                     :help "`py-delete-block'
Delete innermost compound statement at point, don't store deleted string in kill-ring"]

                    ["Shift block right" py-shift-block-right
                     :help "`py-shift-block-right'
Shift block right. "]

                    ["Shift block left" py-shift-block-left
                     :help "`py-shift-block-left'
Shift block left. "]

                    ["Comment block" py-comment-block
                     :help " `py-comment-block'

Comments block at point\.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]
                    )

                   ("Def-or-class "

                    ["Beginning of Def-or-Class" py-beginning-of-def-or-class
                     :help "`py-beginning-of-def-or-class'
Go to start of innermost definition at point"]

                    ["End of Def-or-Class" py-end-of-def-or-class
                     :help "`py-end-of-def-or-class'
Go to end of innermost function definition at point"]

                    ["Down def-or-class" py-down-def-or-class
                     :help "`py-down-def-or-class'

Go to the beginning of next def-or-class below in buffer.

Returns indentation if def-or-class found, nil otherwise. "]

                    ["Up def-or-class" py-up-def-or-class
                     :help "`py-up-def-or-class'

Go upwards to the beginning of next def-or-class below in buffer.

Returns indentation if def-or-class found, nil otherwise. "]

                    ["Copy Def-or-Class" py-copy-def-or-class
                     :help "`py-copy-def-or-class'
Copy innermost definition at point"]

                    ["Kill def-or-class" py-kill-def-or-class
                     :help "`py-kill-def-or-class'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Delete def-or-class" py-delete-def-or-class
                     :help "`py-delete-def-or-class'
Delete def-or-class at point, don't store deleted string in kill-ring"]

                    ["Shift def-or-class right" py-shift-def-or-class-right
                     :help "`py-shift-def-or-class-right'
Shift def-or-class right. "]

                    ["Shift def-or-class left" py-shift-def-or-class-left
                     :help "`py-shift-def-or-class-left'
Shift def-or-class left. "]

                    ["Comment def or class" py-comment-def-or-class
                     :help " `py-comment-def-or-class'

Comments def-or-class at point\.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Clause "

                    ["Beginning of clause" py-beginning-of-clause
                     :help "`py-beginning-of-clause'
Go to start of innermost compound statement at point"]
                    ["End of clause" py-end-of-clause
                     :help "`py-end-of-clause'
Go to end of innermost compound statement at point"]

                    ["Down clause" py-down-clause
                     :help "`py-down-clause'

Go to the beginning of next clause below in buffer.

Returns indentation if clause found, nil otherwise. "]

                    ["Up clause" py-up-clause
                     :help "`py-up-clause'

Go upwards to the beginning of next clause below in buffer.

Returns indentation if clause found, nil otherwise. "]

                    ["Copy clause" py-copy-clause
                     :help "`py-copy-clause'
Copy innermost compound statement at point"]

                    ["Kill clause" py-kill-clause
                     :help "`py-kill-clause'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Delete clause" py-delete-clause
                     :help "`py-delete-clause'
Delete innermost compound statement at point, don't store deleted string in kill-ring"]

                    ["Shift clause right" py-shift-clause-right
                     :help "`py-shift-clause-right'
Shift clause right. "]

                    ["Shift clause left" py-shift-clause-left
                     :help "`py-shift-clause-left'
Shift clause left. "]

                    ["Comment clause" py-comment-clause
                     :help " `py-comment-clause'

Comments clause at point\.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Statement "

                    ["Beginning of Statement" py-beginning-of-statement
                     :help "`py-beginning-of-statement'
Go to start of innermost definition at point"]

                    ["End of Statement" py-end-of-statement
                     :help "`py-end-of-statement'
Go to end of innermost function definition at point"]

                    ["Copy statement" py-copy-statement
                     :help "`py-copy-statement'
Copy innermost definition at point"]

                    ["Kill statement" py-kill-statement
                     :help "`py-kill-statement'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Delete statement" py-delete-statement
                     :help "`py-delete-statement'
Delete statement at point, don't store deleted string in kill-ring"]

                    ["Shift statement right" py-shift-statement-right
                     :help "`py-shift-statement-right'
Shift statement right. "]

                    ["Shift statement left" py-shift-statement-left
                     :help "`py-shift-statement-left'
Shift statement left. "]

                    ["Comment statement" py-comment-statement
                     :help " `py-comment-statement'

Comments statement at point\.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Expression"

                    ["Beginning of expression" py-beginning-of-expression
                     :help "Go to the beginning of a compound python expression.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes."]

                    ["End of expression" py-end-of-expression
                     :help "`py-end-of-expression'
Go to the end of a compound python expression.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes."]

                    ["Beginning of expression" py-beginning-of-expression
                     :help "`py-beginning-of-expression'
Go to start of a Python expression"]

                    ["End of expression" py-end-of-expression
                     :help "`py-end-of-expression'
Go to end of a Python expression"]

                    ["Copy expression" py-copy-expression
                     :help "`py-copy-expression'
Copy expression at point"]

                    ["Kill expression" py-kill-expression
                     :help "`py-kill-expression'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Delete expression" py-delete-expression
                     :help "`py-delete-expression'
Delete expression at point, don't store deleted string in kill-ring"])

                   ("Partial expression"

                    ["Beginning of minor expression" py-beginning-of-partial-expression
                     :help "`py-beginning-of-partial-expression'
Go to start of an minor expression

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes."]

                    ["End of partial-expression" py-end-of-partial-expression
                     :help "`py-end-of-partial-expression'
Go to end of an partial-expression

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes."]

                    ["Copy partial expression" py-copy-partial-expression
                     :help "`py-copy-partial-expression'
\".\" operators delimit a partial-expression expression on it's level"]

                    ["Kill partial-expression" py-kill-partial-expression
                     :help "`py-kill-partial-expression'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Delete partial-expression" py-delete-partial-expression
                     :help "`py-delete-partial-expression'
Delete partial-expression at point, don't store deleted string in kill-ring"])

                   ("Class "

                    ["Beginning of Class" py-beginning-of-class
                     :help "`py-beginning-of-class'
Go to start of innermost definition at point"]

                    ["End of Class" py-end-of-class
                     :help "`py-end-of-class'
Go to end of innermost function definition at point"]

                    ["Down class" py-down-class
                     :help "`py-down-class'

Go to the beginning of next class below in buffer.

Returns indentation if class found, nil otherwise. "]

                    ["Up class" py-up-class
                     :help "`py-up-class'

Go upwards to the beginning of next class below in buffer.

Returns indentation if class found, nil otherwise. "]

                    ["Copy class" py-copy-class
                     :help "`py-copy-class'
Copy innermost definition at point"]

                    ["Kill class" py-kill-class
                     :help "`py-kill-class'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Delete class" py-delete-class
                     :help "`py-delete-class'
Delete class at point, don't store deleted string in kill-ring"]

                    ["Shift class right" py-shift-class-right
                     :help "`py-shift-class-right'
Shift class right. "]

                    ["Shift class left" py-shift-class-left
                     :help "`py-shift-class-left'
Shift class left. "]

                    ["Comment class" py-comment-class
                     :help " `py-comment-class'

Comments class at point\.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Def "

                    ["Beginning of Def" py-beginning-of-def
                     :help "`py-beginning-of-def'
Go to start of innermost definition at point"]

                    ["End of Def" py-end-of-def
                     :help "`py-end-of-def'
Go to end of innermost function definition at point"]

                    ["Down def" py-down-def
                     :help "`py-down-def'

Go to the beginning of next def below in buffer.

Returns indentation if def found, nil otherwise. "]

                    ["Up def" py-up-def
                     :help "`py-up-def'

Go upwards to the beginning of next def below in buffer.

Returns indentation if def found, nil otherwise. "]

                    ["Copy def" py-copy-def
                     :help "`py-copy-def'
Copy innermost definition at point"]

                    ["Kill def" py-kill-def
                     :help "`py-kill-def'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Delete def" py-delete-def
                     :help "`py-delete-def'
Delete def at point, don't store deleted string in kill-ring"]

                    ["Shift def right" py-shift-def-right
                     :help "`py-shift-def-right'
Shift def right. "]

                    ["Shift def left" py-shift-def-left
                     :help "`py-shift-def-left'
Shift def left. "]

                    ["Comment def" py-comment-def
                     :help " `py-comment-def'

Comments def at point\.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                    )

                   (" Block bol "

                    ["Beginning of block bol" py-beginning-of-block-bol
                     :help "`py-beginning-of-block-bol'
Go to beginning of line at beginning of block.

Returns position reached, if successful, nil otherwise. "]

                    ["End of block bol" py-end-of-block-bol
                     :help "`py-end-of-block-bol'
Go to beginning of line following end of block.

Returns position reached, if successful, nil otherwise. "]

                    ["Up block bol" py-up-block-bol
                     :help "`py-up-block-bol'
Go to next block upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

                    ["Down block bol" py-down-block-bol
                     :help "`py-down-block-bol'
Go to next block downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

                    ["Mark block bol" py-mark-block-bol
                     :help "`py-mark-block-bol'
Mark block at point. "]

                    ["Copy block bol" py-copy-block-bol
                     :help "`py-copy-block-bol'
Copy block at point. "]

                    ["Kill block bol" py-kill-block-bol
                     :help "`py-kill-block-bol'
Kill block at point. "]

                    ["Delete block bol" py-delete-block-bol
                     :help "`py-delete-block-bol'
Delete block at point. "]

                    ["Shift block right" py-shift-block-right
                     :help "`py-shift-block-right'
Shift block right. "]

                    ["Shift block left" py-shift-block-left
                     :help "`py-shift-block-left'
Shift block left. "])

                   (" Clause bol "

                    ["Beginning of clause bol" py-beginning-of-clause-bol
                     :help "`py-beginning-of-clause-bol'
Go to beginning of line at beginning of clause.

Returns position reached, if successful, nil otherwise. "]

                    ["End of clause bol" py-end-of-clause-bol
                     :help "`py-end-of-clause-bol'
Go to beginning of line following end of clause.

Returns position reached, if successful, nil otherwise. "]

                    ["Up clause bol" py-up-clause-bol
                     :help "`py-up-clause-bol'
Go to next clause upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

                    ["Down clause bol" py-down-clause-bol
                     :help "`py-down-clause-bol'
Go to next clause downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

                    ["Mark clause bol" py-mark-clause-bol
                     :help "`py-mark-clause-bol'
Mark clause at point. "]

                    ["Copy clause bol" py-copy-clause-bol
                     :help "`py-copy-clause-bol'
Copy clause at point. "]

                    ["Kill clause bol" py-kill-clause-bol
                     :help "`py-kill-clause-bol'
Kill clause at point. "]

                    ["Delete clause bol" py-delete-clause-bol
                     :help "`py-delete-clause-bol'
Delete clause at point. "]

                    ["Shift clause right" py-shift-clause-right
                     :help "`py-shift-clause-right'
Shift clause right. "]

                    ["Shift clause left" py-shift-clause-left
                     :help "`py-shift-clause-left'
Shift clause left. "])

                   (" Block-Or-Clause bol "

                    ["Beginning of block-or-clause bol" py-beginning-of-block-or-clause-bol
                     :help "`py-beginning-of-block-or-clause-bol'
Go to beginning of line at beginning of block-or-clause.

Returns position reached, if successful, nil otherwise. "]

                    ["End of block-or-clause bol" py-end-of-block-or-clause-bol
                     :help "`py-end-of-block-or-clause-bol'
Go to beginning of line following end of block-or-clause.

Returns position reached, if successful, nil otherwise. "]

                    ["Up block-or-clause bol" py-up-block-or-clause-bol
                     :help "`py-up-block-or-clause-bol'
Go to next block-or-clause upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

                    ["Down block-or-clause bol" py-down-block-or-clause-bol
                     :help "`py-down-block-or-clause-bol'
Go to next block-or-clause downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

                    ["Mark block-or-clause bol" py-mark-block-or-clause-bol
                     :help "`py-mark-block-or-clause-bol'
Mark block-or-clause at point. "]

                    ["Copy block-or-clause bol" py-copy-block-or-clause-bol
                     :help "`py-copy-block-or-clause-bol'
Copy block-or-clause at point. "]

                    ["Kill block-or-clause bol" py-kill-block-or-clause-bol
                     :help "`py-kill-block-or-clause-bol'
Kill block-or-clause at point. "]

                    ["Delete block-or-clause bol" py-delete-block-or-clause-bol
                     :help "`py-delete-block-or-clause-bol'
Delete block-or-clause at point. "]

                    ["Shift block-or-clause right" py-shift-block-or-clause-right
                     :help "`py-shift-block-or-clause-right'
Shift block-or-clause right. "]

                    ["Shift block-or-clause left" py-shift-block-or-clause-left
                     :help "`py-shift-block-or-clause-left'
Shift block-or-clause left. "])

                   (" Def bol "

                    ["Beginning of def bol" py-beginning-of-def-bol
                     :help "`py-beginning-of-def-bol'
Go to beginning of line at beginning of def.

Returns position reached, if successful, nil otherwise. "]

                    ["End of def bol" py-end-of-def-bol
                     :help "`py-end-of-def-bol'
Go to beginning of line following end of def.

Returns position reached, if successful, nil otherwise. "]

                    ["Up def bol" py-up-def-bol
                     :help "`py-up-def-bol'
Go to next def upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

                    ["Down def bol" py-down-def-bol
                     :help "`py-down-def-bol'
Go to next def downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

                    ["Mark def bol" py-mark-def-bol
                     :help "`py-mark-def-bol'
Mark def at point. "]

                    ["Copy def bol" py-copy-def-bol
                     :help "`py-copy-def-bol'
Copy def at point. "]

                    ["Kill def bol" py-kill-def-bol
                     :help "`py-kill-def-bol'
Kill def at point. "]

                    ["Delete def bol" py-delete-def-bol
                     :help "`py-delete-def-bol'
Delete def at point. "]

                    ["Shift def right" py-shift-def-right
                     :help "`py-shift-def-right'
Shift def right. "]

                    ["Shift def left" py-shift-def-left
                     :help "`py-shift-def-left'
Shift def left. "])

                   (" Class bol "
                    ["Beginning of class bol" py-beginning-of-class-bol
                     :help "`py-beginning-of-class-bol'
Go to beginning of line at beginning of class.

Returns position reached, if successful, nil otherwise. "]

                    ["End of class bol" py-end-of-class-bol
                     :help "`py-end-of-class-bol'
Go to beginning of line following end of class.

Returns position reached, if successful, nil otherwise. "]

                    ["Up class bol" py-up-class-bol
                     :help "`py-up-class-bol'
Go to next class upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

                    ["Down class bol" py-down-class-bol
                     :help "`py-down-class-bol'
Go to next class downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

                    ["Mark class bol" py-mark-class-bol
                     :help "`py-mark-class-bol'
Mark class at point. "]

                    ["Copy class bol" py-copy-class-bol
                     :help "`py-copy-class-bol'
Copy class at point. "]

                    ["Kill class bol" py-kill-class-bol
                     :help "`py-kill-class-bol'
Kill class at point. "]

                    ["Delete class bol" py-delete-class-bol
                     :help "`py-delete-class-bol'
Delete class at point. "]

                    ["Shift class right" py-shift-class-right
                     :help "`py-shift-class-right'
Shift class right. "]

                    ["Shift class left" py-shift-class-left
                     :help "`py-shift-class-left'
Shift class left. "])

                   (" Def-Or-Class bol "
                    ["Beginning of def-or-class bol" py-beginning-of-def-or-class-bol
                     :help "`py-beginning-of-def-or-class-bol'
Go to beginning of line at beginning of def-or-class.

Returns position reached, if successful, nil otherwise. "]

                    ["End of def-or-class bol" py-end-of-def-or-class-bol
                     :help "`py-end-of-def-or-class-bol'
Go to beginning of line following end of def-or-class.

Returns position reached, if successful, nil otherwise. "]

                    ["Up def-or-class bol" py-up-def-or-class-bol
                     :help "`py-up-def-or-class-bol'
Go to next def-or-class upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

                    ["Down def-or-class bol" py-down-def-or-class-bol
                     :help "`py-down-def-or-class-bol'
Go to next def-or-class downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

                    ["Mark def-or-class bol" py-mark-def-or-class-bol
                     :help "`py-mark-def-or-class-bol'
Mark def-or-class at point. "]

                    ["Copy def-or-class bol" py-copy-def-or-class-bol
                     :help "`py-copy-def-or-class-bol'
Copy def-or-class at point. "]

                    ["Kill def-or-class bol" py-kill-def-or-class-bol
                     :help "`py-kill-def-or-class-bol'
Kill def-or-class at point. "]

                    ["Delete def-or-class bol" py-delete-def-or-class-bol
                     :help "`py-delete-def-or-class-bol'
Delete def-or-class at point. "]

                    ["Shift def-or-class right" py-shift-def-or-class-right
                     :help "`py-shift-def-or-class-right'
Shift def-or-class right. "]

                    ["Shift def-or-class left" py-shift-def-or-class-left
                     :help "`py-shift-def-or-class-left'
Shift def-or-class left. "])

                   (" Statement bol "
                    ["Beginning of statement bol" py-beginning-of-statement-bol
                     :help "`py-beginning-of-statement-bol'
Go to beginning of line at beginning of statement.

Returns position reached, if successful, nil otherwise. "]

                    ["End of statement bol" py-end-of-statement-bol
                     :help "`py-end-of-statement-bol'
Go to beginning of line following end of statement.

Returns position reached, if successful, nil otherwise. "]

                    ["Mark statement bol" py-mark-statement-bol
                     :help "`py-mark-statement-bol'
Mark statement at point. "]

                    ["Copy statement bol" py-copy-statement-bol
                     :help "`py-copy-statement-bol'
Copy statement at point. "]

                    ["Kill statement bol" py-kill-statement-bol
                     :help "`py-kill-statement-bol'
Kill statement at point. "]

                    ["Delete statement bol" py-delete-statement-bol
                     :help "`py-delete-statement-bol'
Delete statement at point. "]

                    ["Shift statement right" py-shift-statement-right
                     :help "`py-shift-statement-right'
Shift statement right. "]

                    ["Shift statement left" py-shift-statement-left
                     :help "`py-shift-statement-left'
Shift statement left. "])
                   )
                  "-"
                  ("Filling"
                   :help "see also customizable `py-docstring-style'"

                   ["Fill string" py-fill-string
                    :help " `py-fill-string'

Uses value of `py-docstring-style', if set. "]

                   ["Fill paragraph" py-fill-paragraph
                    :help " `py-fill-paragraph'

Uses value of `py-docstring-style', if set. "]

                   ["Fill comment" py-fill-comment
                    :help " `py-fill-comment'

Fill comment at point. "]

                   ["Fill string django-style " py-fill-string-django
                    :help " `py-fill-string-django'

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
 "]

                   ["py fill string onetwo" py-fill-string-onetwo
                    :help " `py-fill-string-onetwo'
One newline and start and Two at end style.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'"]

                   ["py fill string pep 257" py-fill-string-pep-257
                    :help " `py-fill-string-pep-257'

PEP-257 with 2 newlines at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'"]

                   ["py fill string pep 257 nn" py-fill-string-pep-257-nn
                    :help " `py-fill-string-pep-257-nn'

PEP-257 with 1 newline at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'"]

                   ["py fill string symmetric" py-fill-string-symmetric
                    :help " `py-fill-string-symmetric'

Symmetric style.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'"])

                  ("Electric "
                   :help "electric commands'"

                   ["Hungry delete backwards" py-hungry-delete-backwards
                    :help " `py-hungry-delete-backwards'

Delete the preceding character or all preceding whitespace
back to the previous non-whitespace character\.
See also C-c <delete>\.. "]

                   ["Hungry delete forward" py-hungry-delete-forward
                    :help " `py-hungry-delete-forward'

Delete the following character or all following whitespace
up to the next non-whitespace character\.
See also C-c <C-backspace>\.. "]

                   ["Electric colon" py-electric-colon
                    :help " `py-electric-colon'
Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix C-u.

Switched by `py-electric-colon-active-p', default is nil
See also `py-electric-colon-greedy-p' "]

                   ["Electric delete" py-electric-delete
                    :help " `py-electric-delete'
Delete following character or levels of whitespace\.

With ARG do that ARG times\. . "]

                   ["Electric backspace" py-electric-backspace
                    :help " `py-electric-backspace'
Delete preceding character or level of indentation\.

With ARG do that ARG times\.
Returns column reached\. . "]

                   ["Electric comment" py-electric-comment
                    :help " `py-electric-comment'
Insert a comment. If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many \"#\" are inserted
non-electrically.
With C-u \"#\" electric behavior is inhibited inside a string or comment.. "]

                   ["Electric left paren" py-complete-electric-lparen
                    :help " `py-complete-electric-lparen'
electricly insert '(', and try to get a signature for the stuff to the left.\n
Needs Pymacs"]

                   ["Complete electric comma" py-complete-electric-comma
                    :help " `py-complete-electric-comma'
electricly insert ',', and redisplay latest signature.\n
Needs Pymacs"]

                   ["Electric yank" py-electric-yank
                    :help " `py-electric-yank'
Perform command `yank' followed by an `indent-according-to-mode' . "])

                  ("Abbrevs"
                   :help "see also `py-add-abbrev'"
                   :filter (lambda (&rest junk)
                             (abbrev-table-menu python-mode-abbrev-table)))
                  ["add-abbrev" py-add-abbrev
                   :help "Defines python-mode specific abbrev for last expressions before point.
Argument is how many `py-partial-expression's form the expansion; or zero means the region is the expansion. "]

                  ("Skeletons"
                   :help "See also templates in YASnippet"

                   ["if" py-if
                    :help "Inserts if-statement"]
                   ["py-else" py-else
                    :help "Inserts else-statement"]
                   ["py-while" py-while
                    :help "Inserts while-statement"]
                   ["py-for" py-for
                    :help "Inserts for-statement"]
                   ["py-try/finally" py-try/finally
                    :help "Inserts py-try/finally-statement"]
                   ["py-try/except" py-try/except
                    :help "Inserts py-try/except-statement"]

                   )

                  ("Completion"
                   :help "Completion options"

                   ["Complete symbol" py-shell-complete
                    :help "`py-shell-complete'
Complete (qualified) symbol before point"]

                   ["Complete" py-complete
                    :help " `py-complete'
Complete symbol before point using Pymacs . "])

                  ["Find function" py-find-function
                   :help "`py-find-function'
Try to find source definition of function at point"]

                  )

                 )

               )

             )

        map))

(defvaralias 'py-mode-map 'python-mode-map)

(defun py-dump-help-string (str)
  (with-output-to-temp-buffer "*Help*"
    (let ((locals (buffer-local-variables))
          funckind funcname func funcdoc
          (start 0) mstart end
          keys)
      (while (string-match "^%\\([vc]\\):\\(.+\\)\n" str start)
        (setq mstart (match-beginning 0) end (match-end 0)
              funckind (substring str (match-beginning 1) (match-end 1))
              funcname (substring str (match-beginning 2) (match-end 2))
              func (intern funcname))
        (princ (substitute-command-keys (substring str start mstart)))
        (cond
         ((equal funckind "c")          ; command
          (setq funcdoc (documentation func)
                keys (concat
                      "Key(s): "
                      (mapconcat 'key-description
                                 (where-is-internal func python-mode-map)
                                 ", "))))
         ((equal funckind "v")          ; variable
          (setq funcdoc (documentation-property func 'variable-documentation)
                keys (if (assq func locals)
                         (concat
                          "Local/Global values: "
                          (prin1-to-string (symbol-value func))
                          " / "
                          (prin1-to-string (default-value func)))
                       (concat
                        "Value: "
                        (prin1-to-string (symbol-value func))))))
         (t                             ; unexpected
          (error "Error in py-dump-help-string, tag `%s'" funckind)))
        (princ (format "\n-> %s:\t%s\t%s\n\n"
                       (if (equal funckind "c") "Command" "Variable")
                       funcname keys))
        (princ funcdoc)
        (terpri)
        (setq start end))
      (princ (substitute-command-keys (substring str start))))
    (if (featurep 'xemacs) (print-help-return-message)
      (help-print-return-message))))

;;; Abbrevs
(defun py-edit-abbrevs ()
  "Jumps to `python-mode-abbrev-table' in a buffer containing lists of abbrev definitions.
You can edit them and type \\<edit-abbrevs-map>\\[edit-abbrevs-redefine] to redefine abbrevs
according to your editing.
Buffer contains a header line for each abbrev table,
 which is the abbrev table name in parentheses.
This is followed by one line per abbrev in that table:
NAME   USECOUNT   EXPANSION   HOOK
where NAME and EXPANSION are strings with quotes,
USECOUNT is an integer, and HOOK is any valid function
or may be omitted (it is usually omitted).  "
  (interactive)
  (save-excursion
    (let ((mat (abbrev-table-name local-abbrev-table)))
      (prepare-abbrev-list-buffer)
      (set-buffer "*Abbrevs*")
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (search-forward (concat "(" (format "%s" mat))))))

(defun py-add-abbrev-propose (table type arg &optional dont-ask)
  (save-excursion
    (let ((orig (point))
          proposal exp name)
      (while (< 0 arg)
        (py-beginning-of-partial-expression)
        (when (looking-at "[[:alpha:]]")
          (setq proposal (concat (downcase (match-string-no-properties 0)) proposal)))
        (setq arg (1- arg)))
      (setq exp (buffer-substring-no-properties (point) orig))
      (setq name
            ;; ask only when interactive
            (if dont-ask
                proposal
              (read-string (format (if exp "%s abbrev for \"%s\": "
                                     "Undefine %s abbrev: ")
                                   type exp) proposal)))
      (set-text-properties 0 (length name) nil name)
      (when (or (null exp)
                (not (abbrev-expansion name table))
                (y-or-n-p (format "%s expands to \"%s\"; redefine? "
                                  name (abbrev-expansion name table))))
        (define-abbrev table (downcase name) exp)))))

(defun py-add-abbrev (arg)
  "Defines python-mode specific abbrev for last expressions before point.
Argument is how many `py-partial-expression's form the expansion; or zero means the region is the expansion.

Reads the abbreviation in the minibuffer; with numeric arg it displays a proposal for an abbrev.
Proposal is composed from the initial character(s) of the
expansion.

Don't use this function in a Lisp program; use `define-abbrev' instead."
  (interactive "p")
  (save-excursion
    (py-add-abbrev-propose
     (if only-global-abbrevs
         global-abbrev-table
       (or local-abbrev-table
           (error "No per-mode abbrev table")))
     "Mode" arg)))

;;;

(add-to-list 'hs-special-modes-alist
             (list
              'python-mode
              ;; start regex
              (concat (if py-hide-show-hide-docstrings
                          "^\\s-*\"\"\"\\|" "")
                      (mapconcat 'identity
                                 (mapcar #'(lambda (x) (concat "^\\s-*" x "\\_>"))
                                         py-hide-show-keywords)
                                 "\\|"))
              ;; end regex
              nil
              ;; comment-start regex
              "#"
              ;; forward-sexp function
              (lambda (arg)
                (py-end-of-block-or-clause))
              nil))

(setq imenu-generic-expression 'py-imenu-generic-regexp)

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
                            (file-name-nondirectory buffer-file-name)))))

(defalias 'py-hungry-delete-forward 'c-hungry-delete-forward)
(defalias 'py-hungry-delete-backwards 'c-hungry-delete-backwards)

(defun py-kill-emacs-hook ()
  "Delete files in `py-file-queue'.
These are Python temporary files awaiting execution."
  (mapc #'(lambda (filename)
            (ignore-errors (delete-file filename)))
        py-file-queue))

(defun py-python-version (&optional executable verbose)
  "Returns versions number of a Python EXECUTABLE, string.

If no EXECUTABLE given, `py-shell-name' is used.
Interactively output of `--version' is displayed. "
  (interactive)
  (let* ((executable (or executable py-shell-name))
         (erg (string-strip (shell-command-to-string (concat executable " --version")))))
    (when (interactive-p) (message "%s" erg))
    (unless verbose (setq erg (cadr (split-string erg))))
    erg))

(defun py-version ()
  "Echo the current version of `python-mode' in the minibuffer."
  (interactive)
  (message "Using `python-mode' version %s" py-version)
  (py-keep-region-active))

(defun py-install-search-local ()
  (interactive)
  (let ((erg (split-string (shell-command-to-string (concat "find " default-directory " -maxdepth 9 -type f -name \"*python\"")))))))

;; (defun py-install-local-epdfree ()
;;   (interactive)
;;   (py-install-local-shells "MY-PATH/epdfree"))

(defun py-install-local-shells (&optional local path-prefix)
  "Builds Python-shell commands from executable found in LOCAL.

If LOCAL is empty, shell-command `find' searches beneath current directory.
Eval resulting buffer to install it, see customizable `py-extensions'. "
  (interactive)
  (let* ((local-dir (if local
                        (expand-file-name local)
                      (read-from-minibuffer "Virtualenv directory: " default-directory)))
         (path-separator (if (string-match "/" local-dir)
                             "/"
                           "\\" t))
         (shells (split-string (shell-command-to-string (concat "find " local-dir " -maxdepth 9 -type f -executable -name \"*python\""))))
         erg newshell prefix akt end orig curexe aktpath)
    (set-buffer (get-buffer-create py-extensions))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (dolist (elt shells)
      (setq prefix "")
      (setq curexe (substring elt (1+ (string-match "/[^/]+$" elt))))
      (setq aktpath (substring elt 0 (1+ (string-match "/[^/]+$" elt))))
      (dolist (prf (split-string aktpath (regexp-quote path-separator)))
        (unless (string= "" prf)
          (setq prefix (concat prefix (substring prf 0 1)))))
      (setq orig (point))
      (insert py-shell-template)
      (setq end (point))
      (goto-char orig)
      (when (re-search-forward "\\<NAME\\>" end t 1)
        (replace-match (concat prefix "-" (substring elt (1+ (save-match-data (string-match "/[^/]+$" elt)))))t))
      (goto-char orig)
      (while (search-forward "DOCNAME" end t 1)
        (replace-match (if (string= "ipython" curexe)
                           "IPython"
                         (capitalize curexe)) t))
      (goto-char orig)
      (when (search-forward "FULLNAME" end t 1)
        (replace-match elt t))
      (goto-char (point-max)))
    (emacs-lisp-mode)
    (if (file-readable-p (concat py-install-directory "/" py-extensions))
        (find-file (concat py-install-directory "/" py-extensions)))))

(defun py-input-filter (str)
  "`comint-input-filter' function for inferior Python.
Don't save anything for STR matching `inferior-python-filter-regexp'."
  (not (string-match inferior-python-filter-regexp str)))

;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-01/msg00076.html
(defalias
  'py-shell-redirect-send-command-to-process
  'comint-redirect-send-command-to-process)
(defalias
  'py-shell-dynamic-simple-complete
  'comint-dynamic-simple-complete)

(defun py-restore-window-configuration ()
  "Restore py-restore-window-configuration when completion is done resp. abandoned. "
  (set-window-configuration py-completion-last-window-configuration))

(defun py-shell-simple-send (proc string)
  (comint-simple-send proc string))

(defun py-shell-execute-string-now (string &optional shell buffer proc)
  "Send to Python interpreter process PROC \"exec STRING in {}\".
and return collected output"
  (let* ((procbuf (or buffer (process-buffer proc) (py-shell nil nil shell)))
         (proc (or proc (get-buffer-process procbuf)))
	 (cmd (format "exec '''%s''' in {}"
		      (mapconcat 'identity (split-string string "\n") "\\n")))
         (outbuf (get-buffer-create " *pyshellcomplete-output*"))
         ;; (lines (reverse py-shell-input-lines))
         )
    ;; (when proc
    (unwind-protect
        (condition-case nil
            (progn
              ;; (if lines
              ;;     (with-current-buffer procbuf
              ;;       (comint-redirect-send-command-to-process
              ;;        "\C-c" outbuf proc nil t)
              ;;       ;; wait for output
              ;;       (while (not comint-redirect-completed)
              ;;         (accept-process-output proc 1))))
              (with-current-buffer outbuf
                (delete-region (point-min) (point-max)))
              (with-current-buffer procbuf
                (comint-redirect-send-command-to-process
                 cmd outbuf proc nil t)
                (while (not comint-redirect-completed) ; wait for output
                  (accept-process-output proc 1)))
              (with-current-buffer outbuf
                (buffer-substring (point-min) (point-max))))
          (quit (with-current-buffer procbuf
                  (interrupt-process proc comint-ptyp)
                  (while (not comint-redirect-completed) ; wait for output
                    (accept-process-output proc 1)))
                (signal 'quit nil)))
      ;; (if (with-current-buffer procbuf comint-redirect-completed)
      ;;     (while lines
      ;;       (with-current-buffer procbuf
      ;;         (comint-redirect-send-command-to-process
      ;;          (car lines) outbuf proc nil t))
      ;;       (accept-process-output proc 1)
      ;;       (setq lines (cdr lines))))
      )))

;;; Completion
(defun py-dot-word-before-point ()
  (buffer-substring
   (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point))
   (point)))

(defun py-completion-at-point ()
  "An alternative completion, similar the way python.el does it. "
  (interactive "*")
  (let* ((start (when (skip-chars-backward "[[:alnum:]_]")(point)))
         (end (progn (skip-chars-forward "[[:alnum:]_]")(point)))
         (completion (when start
                       (py-symbol-completions (buffer-substring-no-properties start end)))))
    (if completion
        (progn
          (delete-region start end)
          (insert (car completion)))
      (tab-to-tab-stop))))

;; started from python.el
(defalias 'py-script-complete 'py-shell-complete)

(defun py-python-script-complete (&optional shell imports beg end word)
  "Complete word before point, if any.

When `py-no-completion-calls-dabbrev-expand-p' is non-nil, try dabbrev-expand. Otherwise, when `py-indent-no-completion-p' is non-nil, call `tab-to-tab-stop'. "
  (interactive)
  (let* (py-split-windows-on-execute-p
         py-switch-buffers-on-execute-p
         (orig (point))
         (shell (or shell py-local-versioned-command (py-choose-shell)))
         (beg (or beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point))))
         (end (or end (point)))
         (word (or word (buffer-substring-no-properties beg end)))
         (imports (or imports (py-find-imports)))
         proc)
    (cond ((string= word "")
           (if py-indent-no-completion-p
               (tab-to-tab-stop)
             (message "%s" "Nothing to complete. ")))
          (t (or (setq proc (get-buffer-process (py-buffer-name-prepare)))
                 (setq proc (get-buffer-process (py-shell nil nil shell))))
             (if (processp proc)
                 (progn
                   ;; when completing instances, make them known
                   (when (string-match "^\\(^[a-zA-Z0-9_]+\\)\\.\\([a-zA-Z0-9_]+\\)$" word)
                     ;; (message "%s" (match-string 1 word))
                     (save-excursion
                       (save-match-data
                         (goto-char (point-min))
                         (when (re-search-forward (concat "^[ \t]*" (match-string-no-properties 1 word) "[ \t]*=[ \t]*[^ \n\r\f\t]+") nil t 1)
                           (when py-verbose-p (message "%s" (match-string-no-properties 0)))
                           (if imports
                               (setq imports (concat imports (match-string-no-properties 0) ";"))
                             (setq imports (match-string-no-properties 0)))))))
                   (py-shell--do-completion-at-point proc imports word))
               (error "No completion process at proc"))))))

(defun py-python2-shell-complete (&optional shell)
  (interactive)
  (let* (py-split-windows-on-execute-p
         py-switch-buffers-on-execute-p
         (shell (or shell py-local-versioned-command))
         (orig (point))
         (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
         (end (point))
         (word (buffer-substring-no-properties beg end))
         proc)
    (cond ((string= word "")
           (message "%s" "Nothing to complete. ")
           (tab-to-tab-stop))
          (t (or (setq proc (get-buffer-process shell))
                 (setq proc (get-buffer-process (py-shell nil nil shell))))
             (py-shell--do-completion-at-point proc nil word))))
  nil)

(defun py-python3-shell-complete (&optional shell)
  "Complete word before point, if any. Otherwise insert TAB. "
  (interactive)
  (let* ((shell (or shell py-local-versioned-command))
         (orig (point))
         (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
         (end (point))
         (word (buffer-substring-no-properties beg end)))
    (cond ((string= word "")
           (message "%s" "Nothing to complete. ")
           (tab-to-tab-stop))
          (t
           (py-shell--do-completion-at-point (get-buffer-process (current-buffer)) nil word)
           nil))))

(defun py-shell-complete (&optional shell debug)
  "Complete word before point, if any. Otherwise insert TAB. "
  (interactive)
  ;; (window-configuration-to-register 313465889)
  ;; (save-window-excursion
  (when debug (setq py-shell-complete-debug nil))
  (unless (buffer-live-p (get-buffer "*Python Completions*"))
    (setq py-completion-last-window-configuration
          (current-window-configuration)))
  (let ((orig (point)))
    ;; (ignore-errors (comint-dynamic-complete))
    (when (eq (point) orig)
      (if (or (eq major-mode 'comint-mode)(eq major-mode 'inferior-python-mode))
          ;;  kind of completion resp. to shell
          (let (py-fontify-shell-buffer-p
                (shell (or shell (py-report-executable (buffer-name (current-buffer)))))
                (imports (py-find-imports)))
            (if (string-match "[iI][pP]ython" shell)
                (ipython-complete nil nil nil nil nil shell debug imports)
              (let* ((orig (point))
                     (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
                     (end (point))
                     (word (buffer-substring-no-properties beg end))
                     (proc (get-buffer-process (current-buffer))))
                (cond ((string= word "")
                       (tab-to-tab-stop))
                      ((string-match "[pP]ython3[^[:alpha:]]*$" shell)
                       (py-shell--do-completion-at-point proc imports word))
                      (t (py-shell-complete-intern word beg end shell imports proc))))))
        ;; complete in script buffer
        (let* (
               ;; (a (random 999999999))
               (shell (or shell (py-choose-shell)))
               py-split-windows-on-execute-p
               py-switch-buffers-on-execute-p
               (proc (or (get-process shell)
                         (get-buffer-process (py-shell nil nil shell 'no-switch nil))))
               (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
               (end (point))
               (word (buffer-substring-no-properties beg end))
               (imports (py-find-imports)))
          ;; (window-configuration-to-register a)
          (cond ((string= word "")
                 (tab-to-tab-stop))
                ((string-match "[iI][pP]ython" shell)
                 (ipython-complete nil nil beg end word nil debug imports))
                ((string-match "[pP]ython3[^[:alpha:]]*$" shell)
                 (py-shell--do-completion-at-point proc (buffer-substring-no-properties beg end) word))
                ;; deals better with imports
                ;; (imports
                ;; (py-python-script-complete shell imports beg end word))
                (t (py-shell-complete-intern word beg end shell imports proc debug))))))))

(defun py-shell-complete-intern (word &optional beg end shell imports proc debug)
  (when imports
    (py-send-string-no-output imports proc))
  (let ((result (py-shell-execute-string-now (format "
def print_completions(namespace, text, prefix=''):
   for name in namespace:
       if name.startswith(text):
           print(prefix + name)

def complete(text):
    import __builtin__
    import __main__
    if '.' in text:
        terms = text.split('.')
        try:
            if hasattr(__main__, terms[0]):
                obj = getattr(__main__, terms[0])
            else:
                obj = getattr(__builtin__, terms[0])
            for term in terms[1:-1]:
                obj = getattr(obj, term)
            print_completions(dir(obj), terms[-1], text[:text.rfind('.') + 1])
        except AttributeError:
            pass
    else:
        import keyword
        print_completions(keyword.kwlist, text)
        print_completions(dir(__builtin__), text)
        print_completions(dir(__main__), text)
complete('%s')" word) shell nil proc)))
    (if (or (eq result nil)(string= "" result))
        (progn
          (if py-no-completion-calls-dabbrev-expand-p
              (or (ignore-errors (dabbrev-expand nil)) (message "Can't complete"))
            (message "No completion found")))

      (setq result (replace-regexp-in-string comint-prompt-regexp "" result))
      (let ((comint-completion-addsuffix nil)
            (completions
             (sort
              (delete-dups (if (split-string "\n" "\n")
                               (split-string result "\n" t) ; XEmacs
                             (split-string result "\n")))
              #'string<)))
        (when debug (setq py-shell-complete-debug completions))
        (if (and completions (not (string= "" (car completions))))
            (cond ((eq completions t)
                   (if (eq this-command last-command)
                       (when py-completion-last-window-configuration
                         (set-window-configuration
                          py-completion-last-window-configuration)))
                   (setq py-completion-last-window-configuration nil)
                   (message "Can't find completion for \"%s\"" word)
                   (ding)
                   nil)
                  ((< 1 (length completions))
                   (with-output-to-temp-buffer "*Python Completions*"
                     (display-completion-list
                      (all-completions word completions)
                      word))
                   nil)
                  ((not (string= word (car completions)))
                   (completion-in-region beg end completions)
                   ;; (progn (delete-char (- (length word)))
                   ;; (insert (car completions))
                   (py-restore-window-configuration)
                   nil))
          (when py-no-completion-calls-dabbrev-expand-p
            (ignore-errors (dabbrev-expand nil)))
          (when py-indent-no-completion-p
            (tab-to-tab-stop)))))))

;; ipython shell complete
;; see also
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-01/msg00076.html
(defun ipython-complete (&optional done completion-command-string beg end word shell debug imports)
  "Complete the python symbol before point.

If no completion available, insert a TAB.
Returns the completed symbol, a string, if successful, nil otherwise. "

  (interactive "*")
  (let* (
         py-split-windows-on-execute-p
         py-switch-buffers-on-execute-p
         (beg (or beg (progn (save-excursion (skip-chars-backward "a-z0-9A-Z_." (point-at-bol))
                                             (point)))))
         (end (or end (point)))
         (pattern (or word (buffer-substring-no-properties beg end)))
         (sep ";")
         (shell (or shell (py-choose-shell)))
         (processlist (process-list))
         (imports (or imports (py-find-imports)))
         done
         (process
          (if ipython-complete-use-separate-shell-p
              (unless (and (buffer-live-p " *IPython-Complete*")
                           (comint-check-proc (process-name (get-buffer-process " *IPython-Complete*"))))
                (get-buffer-process (py-shell nil nil py-shell-name " *IPython-Complete*")))
            (progn
              (while (and processlist (not done))
                (when (and
                       (string= py-shell-name (process-name (car processlist)))
                       (processp (car processlist))
                       (setq done (car processlist))))
                (setq processlist (cdr processlist)))
              done)))
         (python-process (or process
                             (get-buffer-process (py-shell nil nil (if (string-match "[iI][pP]ython[^[:alpha:]]*$"  py-shell-name) "ipython") nil))))
         (comint-output-filter-functions
          (delq 'py-comint-output-filter-function comint-output-filter-functions))
         (comint-output-filter-functions
          (append comint-output-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq ugly-return (concat ugly-return string))
                      (delete-region comint-last-output-start
                                     (process-mark (get-buffer-process (current-buffer))))))))

         (ccs (or completion-command-string
                  (if imports
                      (concat imports (py-set-ipython-completion-command-string))
                    (py-set-ipython-completion-command-string))))
         completion completions completion-table ugly-return)
    (if (string= pattern "")
        (tab-to-tab-stop)
      (process-send-string python-process (format ccs pattern))
      (accept-process-output python-process 0.1)
      (if ugly-return
          (progn
            (setq completions
                  (split-string (substring ugly-return 0 (position ?\n ugly-return)) sep))
            (when debug (setq py-shell-complete-debug completions))
            (if (and completions (not (string= "" (car completions))))
                (cond ((eq completions t)
                       (if (eq this-command last-command)
                           (when py-completion-last-window-configuration
                             (set-window-configuration
                              py-completion-last-window-configuration)))
                       (setq py-completion-last-window-configuration nil)
                       (message "Can't find completion for \"%s\"" pattern)
                       (ding)
                       nil)
                      ((< 1 (length completions))
                       (unless py-completion-last-window-configuration
                         (setq py-completion-last-window-configuration
                               (current-window-configuration)))
                       (with-output-to-temp-buffer "*IPython Completions*"
                         (display-completion-list
                          (all-completions pattern completions)))
                       (recenter))
                      ((not (string= pattern (car completions)))
                       (progn (delete-char (- (length pattern)))
                              (insert (car completions))
                              nil)))
              (when py-no-completion-calls-dabbrev-expand-p
                (ignore-errors (dabbrev-expand nil)))
              (when py-indent-no-completion-p
                (tab-to-tab-stop))))
        (message "%s" "No response from Python process. Please check your configuration. If config is okay, please file a bug-regport at http://launchpad.net/python-mode")))))

;;; Checker
;; flymake
;; (defun clear-flymake-allowed-file-name-masks (&optional suffix)
;;   "Remove entries with SUFFIX from `flymake-allowed-file-name-masks'.

;; Default is \"\\.py\\'\" "
;;   (interactive "P")
;;   (let ((suffix (cond ((eq 4 (prefix-numeric-value suffix))
;;                        (read-from-minibuffer "Suffix: " "\\\\.py\\\\'"))
;;                       (suffix suffix)
;;                       (t "\\\\.py\\\\'")))
;;         (erg flymake-allowed-file-name-masks)
;;         (newlist '()))
;;     (dolist (ele flymake-allowed-file-name-masks)
;;       (unless
;;           ;; (string-match "\\\\.py\\\\'" (car ele))
;;           (string-match suffix (car ele))
;;         (add-to-list 'newlist ele t)))
;;     (setq flymake-allowed-file-name-masks newlist)
;;     (when (and py-verbose-p (interactive-p)) (message "%s" flymake-allowed-file-name-masks))
;;     flymake-allowed-file-name-masks))

(defun py-toggle-flymake-intern (name command)
  ;; (clear-flymake-allowed-file-name-masks)
  (unless (string-match "pyflakespep8" name)
    (unless (executable-find name)
      (when py-verbose-p (message "Don't see %s. Use `easy_install' %s? " name name))))
  (if (buffer-file-name)
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (add-to-list 'flymake-allowed-file-name-masks (car (read-from-string (concat "(\"\\.py\\'\" flymake-" name ")"))))
        (list command (list local-file)))
    (message "%s" "flymake needs a `buffer-file-name'. Please save before calling.")))

(defun pylint-flymake-mode ()
  "Toggle `pylint' `flymake-mode'. "
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode 0)
    (py-toggle-flymake-intern "pylint" "pylint")
    (flymake-mode 1)))

(defun pyflakes-flymake-mode ()
  "Toggle `pyflakes' `flymake-mode'. "
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pyflakes" "pyflakes")
    (flymake-mode)))

(defun pychecker-flymake-mode ()
  "Toggle `pychecker' `flymake-mode'. "
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pychecker" "pychecker")
    (flymake-mode)))

(defun pep8-flymake-mode ()
  "Toggle `pep8' `flymake-mode'. "
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pep8" "pep8")
    (flymake-mode)))

(defun pyflakespep8-flymake-mode ()
  "Toggle `pyflakespep8' `flymake-mode'.

Joint call to pyflakes and pep8 as proposed by

Keegan Carruthers-Smith

"
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pyflakespep8" "pyflakespep8")
    (flymake-mode)))

;; pep8
(defun py-pep8-run (command)
  "Run pep8, check formatting (default on the file currently visited).
"
  (interactive
   (let ((default
           (if (buffer-file-name)
               (format "%s %s %s" py-pep8-command
                       (mapconcat 'identity py-pep8-command-args " ")
                       (buffer-file-name))
             (format "%s %s" py-pep8-command
                     (mapconcat 'identity py-pep8-command-args " "))))
         (last (when py-pep8-history
                 (let* ((lastcmd (car py-pep8-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (buffer-file-name) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pep8 like this: "
                              (if last
                                  last
                                default)
                              'py-pep8-history)
        (read-string "Run pep8 like this: "
                     (if last
                         last
                       default)
                     'py-pep8-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defun py-pep8-help ()
  "Display pep8 command line help messages. "
  (interactive)
  (set-buffer (get-buffer-create "*pep8-Help*"))
  (erase-buffer)
  (shell-command "pep8 --help" "*pep8-Help*"))

;; Pylint
(defalias 'pylint 'py-pylint-run)
(defun py-pylint-run (command)
  "Run pylint (default on the file currently visited).

For help see M-x pylint-help resp. M-x pylint-long-help.
Home-page: http://www.logilab.org/project/pylint "
  (interactive
   (let ((default
           (if (buffer-file-name)
               (format "%s %s %s" py-pylint-command
                       (mapconcat 'identity py-pylint-command-args " ")
                       (buffer-file-name))
             (format "%s %s %s" py-pylint-command
                     (mapconcat 'identity py-pylint-command-args " ")
                     (buffer-name (current-buffer)))))
         (last (and py-pylint-history (car py-pylint-history)))
         erg)

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pylint like this: "
                              (if py-pylint-offer-current-p
                                  (or default last)
                                (or last default))
                              'py-pylint-history)
        (read-string "Run pylint like this: "
                     (if py-pylint-offer-current-p
                         (or default last)
                       (or last default))
                     'py-pylint-history)))))
  (save-some-buffers (not py-ask-about-save))
  (unless (file-readable-p buffer-file-name)
    (message "Warning: %s" "pylint needs a file"))
  (shell-command (concat command " " buffer-file-name)))

(defalias 'pylint-help 'py-pylint-help)
(defun py-pylint-help ()
  "Display Pylint command line help messages.

Let's have this until more Emacs-like help is prepared "
  (interactive)
  (set-buffer (get-buffer-create "*Pylint-Help*"))
  (erase-buffer)
  (shell-command "pylint --long-help" "*Pylint-Help*"))

(defalias 'pylint-doku 'py-pylint-doku)
(defun py-pylint-doku ()
  "Display Pylint Documentation.

Calls `pylint --full-documentation'"
  (interactive)
  (set-buffer (get-buffer-create "*Pylint-Documentation*"))
  (erase-buffer)
  (shell-command "pylint --full-documentation" "*Pylint-Documentation*"))

;; Pyflakes
(defalias 'pyflakes 'py-pyflakes-run)
(defun py-pyflakes-run (command)
  "Run pyflakes (default on the file currently visited).

For help see M-x pyflakes-help resp. M-x pyflakes-long-help.
Home-page: http://www.logilab.org/project/pyflakes "
  (interactive
   (let ((default
           (if (buffer-file-name)
               (format "%s %s %s" py-pyflakes-command
                       (mapconcat 'identity py-pyflakes-command-args " ")
                       (buffer-file-name))
             (format "%s %s" py-pyflakes-command
                     (mapconcat 'identity py-pyflakes-command-args " "))))
         (last (when py-pyflakes-history
                 (let* ((lastcmd (car py-pyflakes-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (buffer-file-name) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pyflakes like this: "
                              (if last
                                  last
                                default)
                              'py-pyflakes-history)
        (read-string "Run pyflakes like this: "
                     (if last
                         last
                       default)
                     'py-pyflakes-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defalias 'pyflakes-help 'py-pyflakes-help)
(defun py-pyflakes-help ()
  "Display Pyflakes command line help messages.

Let's have this until more Emacs-like help is prepared "
  (interactive)
  ;; (set-buffer (get-buffer-create "*Pyflakes-Help*"))
  ;; (erase-buffer)
  (with-help-window "*Pyflakes-Help*"
    (with-current-buffer standard-output
      (insert "       pyflakes [file-or-directory ...]

       Pyflakes is a simple program which checks Python
       source files for errors. It is similar to
       PyChecker in scope, but differs in that it does
       not execute the modules to check them. This is
       both safer and faster, although it does not
       perform as many checks. Unlike PyLint, Pyflakes
       checks only for logical errors in programs; it
       does not perform any checks on style.

       All commandline arguments are checked, which
       have to be either regular files or directories.
       If a directory is given, every .py file within
       will be checked.

       When no commandline arguments are given, data
       will be read from standard input.

       The exit status is 0 when no warnings or errors
       are found. When errors are found the exit status
       is 2. When warnings (but no errors) are found
       the exit status is 1.

Extracted from http://manpages.ubuntu.com/manpages/natty/man1/pyflakes.1.html
"))))

;; Pyflakes-pep8
(defalias 'pyflakespep8 'py-pyflakespep8-run)
(defun py-pyflakespep8-run (command)
  "Run pyflakespep8, check formatting (default on the file currently visited).
"
  (interactive
   (let ((default
           (if (buffer-file-name)
               (format "%s %s %s" py-pyflakespep8-command
                       (mapconcat 'identity py-pyflakespep8-command-args " ")
                       (buffer-file-name))
             (format "%s %s" py-pyflakespep8-command
                     (mapconcat 'identity py-pyflakespep8-command-args " "))))
         (last (when py-pyflakespep8-history
                 (let* ((lastcmd (car py-pyflakespep8-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (buffer-file-name) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pyflakespep8 like this: "
                              (if last
                                  last
                                default)
                              'py-pyflakespep8-history)
        (read-string "Run pyflakespep8 like this: "
                     (if last
                         last
                       default)
                     'py-pyflakespep8-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defun py-pyflakespep8-help ()
  "Display pyflakespep8 command line help messages. "
  (interactive)
  (set-buffer (get-buffer-create "*pyflakespep8-Help*"))
  (erase-buffer)
  (shell-command "pyflakespep8 --help" "*pyflakespep8-Help*"))

;; flakes8
(defalias 'flakes8 'py-flakes8-run)
(defun py-flakes8-run (command)
  "Run flakes8, check formatting (default on the file currently visited).
"
  (interactive
   (let ((default
           (if (buffer-file-name)
               (format "%s %s %s" py-flakes8-command
                       (mapconcat 'identity py-flakes8-command-args " ")
                       (buffer-file-name))
             (format "%s %s" py-flakes8-command
                     (mapconcat 'identity py-flakes8-command-args " "))))
         (last (when py-flakes8-history
                 (let* ((lastcmd (car py-flakes8-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (buffer-file-name) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run flakes8 like this: "
                              (if last
                                  last
                                default)
                              'py-flakes8-history)
        (read-string "Run flakes8 like this: "
                     (if last
                         last
                       default)
                     'py-flakes8-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defun py-flakes8-help ()
  "Display flakes8 command line help messages. "
  (interactive)
  (set-buffer (get-buffer-create "*flakes8-Help*"))
  (erase-buffer)
  (shell-command "flakes8 --help" "*flakes8-Help*"))

;; Pychecker
(defun py-pychecker-run (command)
  "Run pychecker (default on the file currently visited)."
  (interactive
   (let ((default
           (if (buffer-file-name)
               (format "%s %s %s" py-pychecker-command
                       (mapconcat 'identity py-pychecker-command-args " ")
                       (buffer-file-name))
             (format "%s %s" py-pychecker-command
                     (mapconcat 'identity py-pychecker-command-args " "))))
         (last (when py-pychecker-history
                 (let* ((lastcmd (car py-pychecker-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (buffer-file-name) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pychecker like this: "
                              (if last
                                  last
                                default)
                              'py-pychecker-history)
        (read-string "Run pychecker like this: "
                     (if last
                         last
                       default)
                     'py-pychecker-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

;;; Skeletons
;; Derived from python.el, where it's instrumented as abbrev
;; Original code authored by Dave Love AFAIK

(defun py-load-skeletons ()
  "These skeletons are loaded by python-mode, if `py-load-skeletons-p' is non-nil. "
  (interactive)
  (define-skeleton py-else
    "Auxiliary skeleton."
    nil
    (unless (eq ?y (read-char "Add `else' clause? (y for yes or RET for no) "))
      (signal 'quit t))
    < "else:" \n)

  (define-skeleton py-if
    "If condition "
    "if " "if " str ":" \n
    _ \n
    ("other condition, %s: "
     < "elif " str ":" \n
     > _ \n nil)
    '(py-else) | ^)

  (define-skeleton py-else
    "Auxiliary skeleton."
    nil
    (unless (eq ?y (read-char "Add `else' clause? (y for yes or RET for no) "))
      (signal 'quit t))
    "else:" \n
    > _ \n)

  (define-skeleton py-while
    "Condition: "
    "while " "while " str ":" \n
    > -1 _ \n
    '(py-else) | ^)

  (define-skeleton py-for
    "Target, %s: "
    "for " "for " str " in " (skeleton-read "Expression, %s: ") ":" \n
    > -1 _ \n
    '(py-else) | ^)

  (define-skeleton py-try/except
    "Py-try/except skeleton "
    "try:" "try:" \n
    > -1 _ \n
    ("Exception, %s: "
     < "except " str '(python-target) ":" \n
     > _ \n nil)
    < "except:" \n
    > _ \n
    '(py-else) | ^)

  (define-skeleton py-target
    "Auxiliary skeleton."
    "Target, %s: " ", " str | -2)

  (define-skeleton py-try/finally
    "Py-try/finally skeleton "
    "try:" \n
    > -1 _ \n
    < "finally:" \n
    > _ \n)

  (define-skeleton py-def
    "Name: "
    "def " str " (" ("Parameter, %s: " (unless (equal ?\( (char-before)) ", ")
                     str) "):" \n
                     "\"\"\"" - "\"\"\"" \n     ; Fixme:  extra space inserted -- why?).
                     > _ \n)

  (define-skeleton py-class
    "Name: "
    "class " str " (" ("Inheritance, %s: "
                       (unless (equal ?\( (char-before)) ", ")
                       str)
    & ")" | -2				; close list or remove opening
    ":" \n
    "\"\"\"" - "\"\"\"" \n
    > _ \n)
  )

;;; Virtualenv
;; Thanks Gabriele Lanaro and all working on that
;; The installation is fairly easy, you have the load option, put this
;; in your .emacs:

;; (load-file "/path/to/virtualenv.el")
;;
;; Otherwise you can do it with the load path:

;; (add-to-list 'load-path "Path/to/virtualenv.el/containing/directory/"
;; (require 'virtualenv)

;; The usage is very intuitive, to activate a virtualenv use

;; M-x virtualenv-activate

;; It will prompt you for the virtual environment path.
;; If you want to deactivate a virtual environment, use:

;; M-x virtualenv-deactivate

(defvar virtualenv-workon-home nil)

(defvar virtualenv-name nil)

(if (getenv "WORKON_HOME")
    (setq virtualenv-workon-home (getenv "WORKON_HOME"))
  (setq virtualenv-workon-home "~/.virtualenvs"))

(setq virtualenv-name nil)

;;TODO: Move to a generic UTILITY or TOOL package
(defun virtualenv-filter (predicate sequence)
  "Apply to each element of SEQUENCE the PREDICATE, if FUNCTION
  returns non-nil append the element to the return value of
  virtualenv-filter: a list"
  (let ((retlist '()))
    (dolist (element sequence)
      (when (funcall predicate element)
        (push element retlist)))
    (nreverse retlist)))

(defun virtualenv-append-path (dir var)
  "Append DIR to a path-like varibale VAR, for example:
 (virtualenv-append-path /usr/bin:/bin /home/test/bin) -> /home/test/bin:/usr/bin:/bin"
  (concat (expand-file-name dir)
          path-separator
          var))

(defun virtualenv-add-to-path (dir)
  "Add the specified path element to the Emacs PATH"
  (setenv "PATH"
          (virtualenv-append-path dir
                                  (getenv "PATH"))))

(defun virtualenv-current ()
  "Barfs the current activated virtualenv"
  (interactive)
  (message virtualenv-name))

(defun virtualenv-activate (dir)
  "Activate the virtualenv located in DIR"
  (interactive "DVirtualenv Directory: ")

  ;; Eventually deactivate previous virtualenv
  (when virtualenv-name
    (virtualenv-deactivate))

  ;; Storing old variables
  (setq virtualenv-old-path (getenv "PATH"))
  (setq virtualenv-old-exec-path exec-path)

  (setenv "VIRTUAL_ENV" dir)
  (virtualenv-add-to-path (concat (py-normalize-directory dir) "bin"))
  (add-to-list 'exec-path (concat (py-normalize-directory dir) "bin"))

  (setq virtualenv-name dir)

  (message (concat "Virtualenv '" virtualenv-name "' activated.")))

(defun virtualenv-deactivate ()
  "Deactivate the current virtual enviroment"
  (interactive)

  ;; Restoring old variables
  (setenv "PATH" virtualenv-old-path)
  (setq exec-path virtualenv-old-exec-path)

  (message (concat "Virtualenv '" virtualenv-name "' deactivated."))

  (setq virtualenv-name nil))

(defun virtualenv-p (dir)
  "Check if a directory is a virtualenv"
  (file-exists-p (concat dir "/bin/activate")))

(defun virtualenv-workon-complete ()
  "return available completions for virtualenv-workon"
  (let
      ;;Varlist
      ((filelist (directory-files virtualenv-workon-home t)))
    ;; Get only the basename from the list of the virtual environments
    ;; paths
    (mapcar 'file-name-nondirectory
            ;; Filter the directories and then the virtual environments
            (virtualenv-filter 'virtualenv-p
                               (virtualenv-filter 'file-directory-p filelist)))))

(defun virtualenv-workon (name)
  "Issue a virtualenvwrapper-like virtualenv-workon command"
  (interactive (list (completing-read "Virtualenv: " (virtualenv-workon-complete))))
  (if (getenv "WORKON_HOME")
      (virtualenv-activate (concat (py-normalize-directory (getenv "WORKON_HOME")) name))
    (virtualenv-activate (concat (py-normalize-directory virtualenv-workon-home) name))))

;;; Execute forms at point
(defun py-execute-statement ()
  "Send statement at point to a Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-statement-p)
                       (py-beginning-of-statement))))
          (end (py-end-of-statement)))
      (py-execute-region beg end))))

(defun py-execute-block ()
  "Send block at point to a Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-block-p)
                       (py-beginning-of-block))))
          (end (py-end-of-block)))
      (py-execute-region beg end))))

(defun py-execute-block-or-clause ()
  "Send block-or-clause at point to a Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-block-or-clause-p)
                       (py-beginning-of-block-or-clause))))
          (end (py-end-of-block-or-clause)))
      (py-execute-region beg end))))

(defun py-execute-def ()
  "Send def at point to a Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-def-p)
                       (py-beginning-of-def))))
          (end (py-end-of-def)))
      (py-execute-region beg end))))

(defun py-execute-class ()
  "Send class at point to a Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-class-p)
                       (py-beginning-of-class))))
          (end (py-end-of-class)))
      (py-execute-region beg end))))

(defun py-execute-def-or-class ()
  "Send def-or-class at point to a Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-def-or-class-p)
                       (py-beginning-of-def-or-class))))
          (end (py-end-of-def-or-class)))
      (py-execute-region beg end))))

(defun py-execute-expression ()
  "Send expression at point to a Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-expression-p)
                       (py-beginning-of-expression))))
          (end (py-end-of-expression)))
      (py-execute-region beg end))))

(defun py-execute-partial-expression ()
  "Send partial-expression at point to a Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-partial-expression-p)
                       (py-beginning-of-partial-expression))))
          (end (py-end-of-partial-expression)))
      (py-execute-region beg end))))

;;; Execute line
(defalias 'ipython-send-and-indent 'py-execute-line-ipython)
(defalias 'py-execute-region-in-shell 'py-execute-region)
(defalias 'py-shell-command-on-region 'py-execute-region)
(defalias 'py-send-region-ipython 'py-execute-region-ipython)
(defalias 'py-ipython-shell-command-on-region 'py-execute-region-ipython)

;;; Execute file commands
(defun py-execute-file-python (&optional filename)
  "Send file to a Python interpreter."
  (interactive "fFile: ")
  (py-execute-prepare filename "python" nil nil nil nil t))

(defun py-execute-file-python-switch (&optional filename)
  "Send file to a Python interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python" nil 'switch nil nil t))

(defun py-execute-file-python-no-switch (&optional filename)
  "Send file to a Python interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python" nil 'no-switch nil nil t))

(defun py-execute-file-python-dedicated (&optional filename)
  "Send file to a Python interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-prepare filename "python" 'dedicated nil nil nil t))

(defun py-execute-file-python-dedicated-switch (&optional filename)
  "Send file to a Python interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python" 'dedicated 'switch nil nil t))

(defun py-execute-file-ipython (&optional filename)
  "Send file to a Ipython interpreter."
  (interactive "fFile: ")
  (py-execute-prepare filename "ipython" nil nil nil nil t))

(defun py-execute-file-ipython-switch (&optional filename)
  "Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "ipython" nil 'switch nil nil t))

(defun py-execute-file-ipython-no-switch (&optional filename)
  "Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "ipython" nil 'no-switch nil nil t))

(defun py-execute-file-ipython-dedicated (&optional filename)
  "Send file to a Ipython interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-prepare filename "ipython" 'dedicated nil nil nil t))

(defun py-execute-file-ipython-dedicated-switch (&optional filename)
  "Send file to a Ipython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "ipython" 'dedicated 'switch nil nil t))

(defun py-execute-file-python3 (&optional filename)
  "Send file to a Python3 interpreter."
  (interactive "fFile: ")
  (py-execute-prepare filename "python3" nil nil nil nil t))

(defun py-execute-file-python3-switch (&optional filename)
  "Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python3" nil 'switch nil nil t))

(defun py-execute-file-python3-no-switch (&optional filename)
  "Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python3" nil 'no-switch nil nil t))

(defun py-execute-file-python3-dedicated (&optional filename)
  "Send file to a Python3 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-prepare filename "python3" 'dedicated nil nil nil t))

(defun py-execute-file-python3-dedicated-switch (&optional filename)
  "Send file to a Python3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python3" 'dedicated 'switch nil nil t))

(defun py-execute-file-python2 (&optional filename)
  "Send file to a Python2 interpreter."
  (interactive "fFile: ")
  (py-execute-prepare filename "python2" nil nil nil nil t))

(defun py-execute-file-python2-switch (&optional filename)
  "Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python2" nil 'switch nil nil t))

(defun py-execute-file-python2-no-switch (&optional filename)
  "Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python2" nil 'no-switch nil nil t))

(defun py-execute-file-python2-dedicated (&optional filename)
  "Send file to a Python2 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-prepare filename "python2" 'dedicated nil nil nil t))

(defun py-execute-file-python2-dedicated-switch (&optional filename)
  "Send file to a Python2 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python2" 'dedicated 'switch nil nil t))

(defun py-execute-file-python2.7 (&optional filename)
  "Send file to a Python2.7 interpreter."
  (interactive "fFile: ")
  (py-execute-prepare filename "python2.7" nil nil nil nil t))

(defun py-execute-file-python2.7-switch (&optional filename)
  "Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python2.7" nil 'switch nil nil t))

(defun py-execute-file-python2.7-no-switch (&optional filename)
  "Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python2.7" nil 'no-switch nil nil t))

(defun py-execute-file-python2.7-dedicated (&optional filename)
  "Send file to a Python2.7 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-prepare filename "python2.7" 'dedicated nil nil nil t))

(defun py-execute-file-python2.7-dedicated-switch (&optional filename)
  "Send file to a Python2.7 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python2.7" 'dedicated 'switch nil nil t))

(defun py-execute-file-jython (&optional filename)
  "Send file to a Jython interpreter."
  (interactive "fFile: ")
  (py-execute-prepare filename "jython" nil nil nil nil t))

(defun py-execute-file-jython-switch (&optional filename)
  "Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "jython" nil 'switch nil nil t))

(defun py-execute-file-jython-no-switch (&optional filename)
  "Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "jython" nil 'no-switch nil nil t))

(defun py-execute-file-jython-dedicated (&optional filename)
  "Send file to a Jython interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-prepare filename "jython" 'dedicated nil nil nil t))

(defun py-execute-file-jython-dedicated-switch (&optional filename)
  "Send file to a Jython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "jython" 'dedicated 'switch nil nil t))

(defun py-execute-file-python3.2 (&optional filename)
  "Send file to a Python3.2 interpreter."
  (interactive "fFile: ")
  (py-execute-prepare filename "python3.2" nil nil nil nil t))

(defun py-execute-file-python3.2-switch (&optional filename)
  "Send file to a Python3.2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python3.2" nil 'switch nil nil t))

(defun py-execute-file-python3.2-no-switch (&optional filename)
  "Send file to a Python3.2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python3.2" nil 'no-switch nil nil t))

(defun py-execute-file-python3.2-dedicated (&optional filename)
  "Send file to a Python3.2 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-prepare filename "python3.2" 'dedicated nil nil nil t))

(defun py-execute-file-python3.2-dedicated-switch (&optional filename)
  "Send file to a Python3.2 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python3.2" 'dedicated 'switch nil nil t))

(defun py-execute-file-python3.3 (&optional filename)
  "Send file to a Python3.3 interpreter."
  (interactive "fFile: ")
  (py-execute-prepare filename "python3.3" nil nil nil nil t))

(defun py-execute-file-python3.3-switch (&optional filename)
  "Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python3.3" nil 'switch nil nil t))

(defun py-execute-file-python3.3-no-switch (&optional filename)
  "Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python3.3" nil 'no-switch nil nil t))

(defun py-execute-file-python3.3-dedicated (&optional filename)
  "Send file to a Python3.3 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-prepare filename "python3.3" 'dedicated nil nil nil t))

(defun py-execute-file-python3.3-dedicated-switch (&optional filename)
  "Send file to a Python3.3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "python3.3" 'dedicated 'switch nil nil t))

(defun py-execute-file-bpython (&optional filename)
  "Send file to a Bpython interpreter."
  (interactive "fFile: ")
  (py-execute-prepare filename "bpython" nil nil nil nil t))

(defun py-execute-file-bpython-switch (&optional filename)
  "Send file to a Bpython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "bpython" nil 'switch nil nil t))

(defun py-execute-file-bpython-no-switch (&optional filename)
  "Send file to a Bpython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "bpython" nil 'no-switch nil nil t))

(defun py-execute-file-bpython-dedicated (&optional filename)
  "Send file to a Bpython interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-prepare filename "bpython" 'dedicated nil nil nil t))

(defun py-execute-file-bpython-dedicated-switch (&optional filename)
  "Send file to a Bpython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-prepare filename "bpython" 'dedicated 'switch nil nil t))

;;; Extended executes
;; created by `write-extended-execute-forms'
(defun py-masterfile ()
  "Internal use. Set master-file, if given. "
  (and (or py-master-file (py-fetch-py-master-file))
       (let* ((filename (expand-file-name py-master-file))
              (buffer (or (get-file-buffer filename)
                          (find-file-noselect filename))))
         (set-buffer buffer))))

(defun py-execute-prepare (form &optional shell dedicated switch beg end file)
  "Used by python-extended-executes ."
  (save-excursion
    (let ((beg (unless file
                 (prog1
                     (or beg (funcall (intern-soft (concat "py-beginning-of-" form "-p")))

                         (funcall (intern-soft (concat "py-beginning-of-" form)))
                         (push-mark)))))
          (end (unless file
                 (or end (funcall (intern-soft (concat "py-end-of-" form))))))
          (py-shell-name shell)
          (py-dedicated-process-p dedicated)
          (py-switch-buffers-on-execute-p (cond ((eq 'switch switch)
                                                 t)
                                                ((eq 'no-switch switch)
                                                 nil)
                                                (t py-switch-buffers-on-execute-p)))
          filename erg)
      (if file
          (progn
            (setq filename (expand-file-name form))
            (if (file-readable-p filename)
                (setq erg (py-execute-file-base nil filename nil nil (or (and (boundp 'py-orig-buffer-or-file) py-orig-buffer-or-file) filename)))
              (message "%s not readable. %s" file "Do you have write permissions?")))
        (py-execute-base beg end)))))

(defun py-execute-statement-python ()
  "Send statement at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python" nil nil))

(defun py-execute-statement-python-switch ()
  "Send statement at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "python" nil 'switch))

(defun py-execute-statement-python-no-switch ()
  "Send statement at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "python" nil 'no-switch))

(defun py-execute-statement-python-dedicated ()
  "Send statement at point to Python unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python" t nil))

(defun py-execute-statement-python-dedicated-switch ()
  "Send statement at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "python" t 'switch))

(defun py-execute-statement-ipython ()
  "Send statement at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "statement" "ipython" nil nil))

(defun py-execute-statement-ipython-switch ()
  "Send statement at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "ipython" nil 'switch))

(defun py-execute-statement-ipython-no-switch ()
  "Send statement at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "ipython" nil 'no-switch))

(defun py-execute-statement-ipython-dedicated ()
  "Send statement at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "ipython" t nil))

(defun py-execute-statement-ipython-dedicated-switch ()
  "Send statement at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "ipython" t 'switch))

(defun py-execute-statement-python3 ()
  "Send statement at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python3" nil nil))

(defun py-execute-statement-python3-switch ()
  "Send statement at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "python3" nil 'switch))

(defun py-execute-statement-python3-no-switch ()
  "Send statement at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "python3" nil 'no-switch))

(defun py-execute-statement-python3-dedicated ()
  "Send statement at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python3" t nil))

(defun py-execute-statement-python3-dedicated-switch ()
  "Send statement at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "python3" t 'switch))

(defun py-execute-statement-python2 ()
  "Send statement at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python2" nil nil))

(defun py-execute-statement-python2-switch ()
  "Send statement at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "python2" nil 'switch))

(defun py-execute-statement-python2-no-switch ()
  "Send statement at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "python2" nil 'no-switch))

(defun py-execute-statement-python2-dedicated ()
  "Send statement at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python2" t nil))

(defun py-execute-statement-python2-dedicated-switch ()
  "Send statement at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "python2" t 'switch))

(defun py-execute-statement-python2.7 ()
  "Send statement at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python2.7" nil nil))

(defun py-execute-statement-python2.7-switch ()
  "Send statement at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "python2.7" nil 'switch))

(defun py-execute-statement-python2.7-no-switch ()
  "Send statement at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "python2.7" nil 'no-switch))

(defun py-execute-statement-python2.7-dedicated ()
  "Send statement at point to Python2.7 unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python2.7" t nil))

(defun py-execute-statement-python2.7-dedicated-switch ()
  "Send statement at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "python2.7" t 'switch))

(defun py-execute-statement-jython ()
  "Send statement at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "statement" "jython" nil nil))

(defun py-execute-statement-jython-switch ()
  "Send statement at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "jython" nil 'switch))

(defun py-execute-statement-jython-no-switch ()
  "Send statement at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "jython" nil 'no-switch))

(defun py-execute-statement-jython-dedicated ()
  "Send statement at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "jython" t nil))

(defun py-execute-statement-jython-dedicated-switch ()
  "Send statement at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "jython" t 'switch))

(defun py-execute-statement-python3.2 ()
  "Send statement at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python3.2" nil nil))

(defun py-execute-statement-python3.2-switch ()
  "Send statement at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "python3.2" nil 'switch))

(defun py-execute-statement-python3.2-no-switch ()
  "Send statement at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "python3.2" nil 'no-switch))

(defun py-execute-statement-python3.2-dedicated ()
  "Send statement at point to Python3.2 unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python3.2" t nil))

(defun py-execute-statement-python3.2-dedicated-switch ()
  "Send statement at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "python3.2" t 'switch))

(defun py-execute-statement-python3.3 ()
  "Send statement at point to Python3.3 interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python3.3" nil nil))

(defun py-execute-statement-python3.3-switch ()
  "Send statement at point to Python3.3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "python3.3" nil 'switch))

(defun py-execute-statement-python3.3-no-switch ()
  "Send statement at point to Python3.3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "python3.3" nil 'no-switch))

(defun py-execute-statement-python3.3-dedicated ()
  "Send statement at point to Python3.3 unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python3.3" t nil))

(defun py-execute-statement-python3.3-dedicated-switch ()
  "Send statement at point to Python3.3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "python3.3" t 'switch))

(defun py-execute-statement-bpython ()
  "Send statement at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "statement" "bpython" nil nil))

(defun py-execute-statement-bpython-switch ()
  "Send statement at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "bpython" nil 'switch))

(defun py-execute-statement-bpython-no-switch ()
  "Send statement at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "bpython" nil 'no-switch))

(defun py-execute-statement-bpython-dedicated ()
  "Send statement at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "bpython" t nil))

(defun py-execute-statement-bpython-dedicated-switch ()
  "Send statement at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "bpython" t 'switch))

(defun py-execute-block-python ()
  "Send block at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "block" "python" nil nil))

(defun py-execute-block-python-switch ()
  "Send block at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "python" nil 'switch))

(defun py-execute-block-python-no-switch ()
  "Send block at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "python" nil 'no-switch))

(defun py-execute-block-python-dedicated ()
  "Send block at point to Python unique interpreter. "
  (interactive)
  (py-execute-prepare "block" "python" t nil))

(defun py-execute-block-python-dedicated-switch ()
  "Send block at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "python" t 'switch))

(defun py-execute-block-ipython ()
  "Send block at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "block" "ipython" nil nil))

(defun py-execute-block-ipython-switch ()
  "Send block at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "ipython" nil 'switch))

(defun py-execute-block-ipython-no-switch ()
  "Send block at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "ipython" nil 'no-switch))

(defun py-execute-block-ipython-dedicated ()
  "Send block at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "block" "ipython" t nil))

(defun py-execute-block-ipython-dedicated-switch ()
  "Send block at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "ipython" t 'switch))

(defun py-execute-block-python3 ()
  "Send block at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "block" "python3" nil nil))

(defun py-execute-block-python3-switch ()
  "Send block at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "python3" nil 'switch))

(defun py-execute-block-python3-no-switch ()
  "Send block at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "python3" nil 'no-switch))

(defun py-execute-block-python3-dedicated ()
  "Send block at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "block" "python3" t nil))

(defun py-execute-block-python3-dedicated-switch ()
  "Send block at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "python3" t 'switch))

(defun py-execute-block-python2 ()
  "Send block at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "block" "python2" nil nil))

(defun py-execute-block-python2-switch ()
  "Send block at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "python2" nil 'switch))

(defun py-execute-block-python2-no-switch ()
  "Send block at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "python2" nil 'no-switch))

(defun py-execute-block-python2-dedicated ()
  "Send block at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "block" "python2" t nil))

(defun py-execute-block-python2-dedicated-switch ()
  "Send block at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "python2" t 'switch))

(defun py-execute-block-python2.7 ()
  "Send block at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "block" "python2.7" nil nil))

(defun py-execute-block-python2.7-switch ()
  "Send block at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "python2.7" nil 'switch))

(defun py-execute-block-python2.7-no-switch ()
  "Send block at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "python2.7" nil 'no-switch))

(defun py-execute-block-python2.7-dedicated ()
  "Send block at point to Python2.7 unique interpreter. "
  (interactive)
  (py-execute-prepare "block" "python2.7" t nil))

(defun py-execute-block-python2.7-dedicated-switch ()
  "Send block at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "python2.7" t 'switch))

(defun py-execute-block-jython ()
  "Send block at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "block" "jython" nil nil))

(defun py-execute-block-jython-switch ()
  "Send block at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "jython" nil 'switch))

(defun py-execute-block-jython-no-switch ()
  "Send block at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "jython" nil 'no-switch))

(defun py-execute-block-jython-dedicated ()
  "Send block at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "block" "jython" t nil))

(defun py-execute-block-jython-dedicated-switch ()
  "Send block at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "jython" t 'switch))

(defun py-execute-block-python3.2 ()
  "Send block at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "block" "python3.2" nil nil))

(defun py-execute-block-python3.2-switch ()
  "Send block at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "python3.2" nil 'switch))

(defun py-execute-block-python3.2-no-switch ()
  "Send block at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "python3.2" nil 'no-switch))

(defun py-execute-block-python3.2-dedicated ()
  "Send block at point to Python3.2 unique interpreter. "
  (interactive)
  (py-execute-prepare "block" "python3.2" t nil))

(defun py-execute-block-python3.2-dedicated-switch ()
  "Send block at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "python3.2" t 'switch))

(defun py-execute-block-python3.3 ()
  "Send block at point to Python3.3 interpreter. "
  (interactive)
  (py-execute-prepare "block" "python3.3" nil nil))

(defun py-execute-block-python3.3-switch ()
  "Send block at point to Python3.3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "python3.3" nil 'switch))

(defun py-execute-block-python3.3-no-switch ()
  "Send block at point to Python3.3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "python3.3" nil 'no-switch))

(defun py-execute-block-python3.3-dedicated ()
  "Send block at point to Python3.3 unique interpreter. "
  (interactive)
  (py-execute-prepare "block" "python3.3" t nil))

(defun py-execute-block-python3.3-dedicated-switch ()
  "Send block at point to Python3.3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "python3.3" t 'switch))

(defun py-execute-block-bpython ()
  "Send block at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "block" "bpython" nil nil))

(defun py-execute-block-bpython-switch ()
  "Send block at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "bpython" nil 'switch))

(defun py-execute-block-bpython-no-switch ()
  "Send block at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "bpython" nil 'no-switch))

(defun py-execute-block-bpython-dedicated ()
  "Send block at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "block" "bpython" t nil))

(defun py-execute-block-bpython-dedicated-switch ()
  "Send block at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "bpython" t 'switch))

(defun py-execute-clause-python ()
  "Send clause at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python" nil nil))

(defun py-execute-clause-python-switch ()
  "Send clause at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "python" nil 'switch))

(defun py-execute-clause-python-no-switch ()
  "Send clause at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "python" nil 'no-switch))

(defun py-execute-clause-python-dedicated ()
  "Send clause at point to Python unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python" t nil))

(defun py-execute-clause-python-dedicated-switch ()
  "Send clause at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "python" t 'switch))

(defun py-execute-clause-ipython ()
  "Send clause at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "clause" "ipython" nil nil))

(defun py-execute-clause-ipython-switch ()
  "Send clause at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "ipython" nil 'switch))

(defun py-execute-clause-ipython-no-switch ()
  "Send clause at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "ipython" nil 'no-switch))

(defun py-execute-clause-ipython-dedicated ()
  "Send clause at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "ipython" t nil))

(defun py-execute-clause-ipython-dedicated-switch ()
  "Send clause at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "ipython" t 'switch))

(defun py-execute-clause-python3 ()
  "Send clause at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python3" nil nil))

(defun py-execute-clause-python3-switch ()
  "Send clause at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "python3" nil 'switch))

(defun py-execute-clause-python3-no-switch ()
  "Send clause at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "python3" nil 'no-switch))

(defun py-execute-clause-python3-dedicated ()
  "Send clause at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python3" t nil))

(defun py-execute-clause-python3-dedicated-switch ()
  "Send clause at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "python3" t 'switch))

(defun py-execute-clause-python2 ()
  "Send clause at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python2" nil nil))

(defun py-execute-clause-python2-switch ()
  "Send clause at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "python2" nil 'switch))

(defun py-execute-clause-python2-no-switch ()
  "Send clause at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "python2" nil 'no-switch))

(defun py-execute-clause-python2-dedicated ()
  "Send clause at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python2" t nil))

(defun py-execute-clause-python2-dedicated-switch ()
  "Send clause at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "python2" t 'switch))

(defun py-execute-clause-python2.7 ()
  "Send clause at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python2.7" nil nil))

(defun py-execute-clause-python2.7-switch ()
  "Send clause at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "python2.7" nil 'switch))

(defun py-execute-clause-python2.7-no-switch ()
  "Send clause at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "python2.7" nil 'no-switch))

(defun py-execute-clause-python2.7-dedicated ()
  "Send clause at point to Python2.7 unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python2.7" t nil))

(defun py-execute-clause-python2.7-dedicated-switch ()
  "Send clause at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "python2.7" t 'switch))

(defun py-execute-clause-jython ()
  "Send clause at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "clause" "jython" nil nil))

(defun py-execute-clause-jython-switch ()
  "Send clause at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "jython" nil 'switch))

(defun py-execute-clause-jython-no-switch ()
  "Send clause at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "jython" nil 'no-switch))

(defun py-execute-clause-jython-dedicated ()
  "Send clause at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "jython" t nil))

(defun py-execute-clause-jython-dedicated-switch ()
  "Send clause at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "jython" t 'switch))

(defun py-execute-clause-python3.2 ()
  "Send clause at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python3.2" nil nil))

(defun py-execute-clause-python3.2-switch ()
  "Send clause at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "python3.2" nil 'switch))

(defun py-execute-clause-python3.2-no-switch ()
  "Send clause at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "python3.2" nil 'no-switch))

(defun py-execute-clause-python3.2-dedicated ()
  "Send clause at point to Python3.2 unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python3.2" t nil))

(defun py-execute-clause-python3.2-dedicated-switch ()
  "Send clause at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "python3.2" t 'switch))

(defun py-execute-clause-python3.3 ()
  "Send clause at point to Python3.3 interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python3.3" nil nil))

(defun py-execute-clause-python3.3-switch ()
  "Send clause at point to Python3.3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "python3.3" nil 'switch))

(defun py-execute-clause-python3.3-no-switch ()
  "Send clause at point to Python3.3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "python3.3" nil 'no-switch))

(defun py-execute-clause-python3.3-dedicated ()
  "Send clause at point to Python3.3 unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python3.3" t nil))

(defun py-execute-clause-python3.3-dedicated-switch ()
  "Send clause at point to Python3.3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "python3.3" t 'switch))

(defun py-execute-clause-bpython ()
  "Send clause at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "clause" "bpython" nil nil))

(defun py-execute-clause-bpython-switch ()
  "Send clause at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "bpython" nil 'switch))

(defun py-execute-clause-bpython-no-switch ()
  "Send clause at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "bpython" nil 'no-switch))

(defun py-execute-clause-bpython-dedicated ()
  "Send clause at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "bpython" t nil))

(defun py-execute-clause-bpython-dedicated-switch ()
  "Send clause at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "bpython" t 'switch))

(defun py-execute-block-or-clause-python ()
  "Send block-or-clause at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python" nil nil))

(defun py-execute-block-or-clause-python-switch ()
  "Send block-or-clause at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python" nil 'switch))

(defun py-execute-block-or-clause-python-no-switch ()
  "Send block-or-clause at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "python" nil 'no-switch))

(defun py-execute-block-or-clause-python-dedicated ()
  "Send block-or-clause at point to Python unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python" t nil))

(defun py-execute-block-or-clause-python-dedicated-switch ()
  "Send block-or-clause at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python" t 'switch))

(defun py-execute-block-or-clause-ipython ()
  "Send block-or-clause at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "ipython" nil nil))

(defun py-execute-block-or-clause-ipython-switch ()
  "Send block-or-clause at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "ipython" nil 'switch))

(defun py-execute-block-or-clause-ipython-no-switch ()
  "Send block-or-clause at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "ipython" nil 'no-switch))

(defun py-execute-block-or-clause-ipython-dedicated ()
  "Send block-or-clause at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "ipython" t nil))

(defun py-execute-block-or-clause-ipython-dedicated-switch ()
  "Send block-or-clause at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "ipython" t 'switch))

(defun py-execute-block-or-clause-python3 ()
  "Send block-or-clause at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3" nil nil))

(defun py-execute-block-or-clause-python3-switch ()
  "Send block-or-clause at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3" nil 'switch))

(defun py-execute-block-or-clause-python3-no-switch ()
  "Send block-or-clause at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3" nil 'no-switch))

(defun py-execute-block-or-clause-python3-dedicated ()
  "Send block-or-clause at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3" t nil))

(defun py-execute-block-or-clause-python3-dedicated-switch ()
  "Send block-or-clause at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3" t 'switch))

(defun py-execute-block-or-clause-python2 ()
  "Send block-or-clause at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2" nil nil))

(defun py-execute-block-or-clause-python2-switch ()
  "Send block-or-clause at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2" nil 'switch))

(defun py-execute-block-or-clause-python2-no-switch ()
  "Send block-or-clause at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2" nil 'no-switch))

(defun py-execute-block-or-clause-python2-dedicated ()
  "Send block-or-clause at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2" t nil))

(defun py-execute-block-or-clause-python2-dedicated-switch ()
  "Send block-or-clause at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2" t 'switch))

(defun py-execute-block-or-clause-python2.7 ()
  "Send block-or-clause at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2.7" nil nil))

(defun py-execute-block-or-clause-python2.7-switch ()
  "Send block-or-clause at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2.7" nil 'switch))

(defun py-execute-block-or-clause-python2.7-no-switch ()
  "Send block-or-clause at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2.7" nil 'no-switch))

(defun py-execute-block-or-clause-python2.7-dedicated ()
  "Send block-or-clause at point to Python2.7 unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2.7" t nil))

(defun py-execute-block-or-clause-python2.7-dedicated-switch ()
  "Send block-or-clause at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2.7" t 'switch))

(defun py-execute-block-or-clause-jython ()
  "Send block-or-clause at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "jython" nil nil))

(defun py-execute-block-or-clause-jython-switch ()
  "Send block-or-clause at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "jython" nil 'switch))

(defun py-execute-block-or-clause-jython-no-switch ()
  "Send block-or-clause at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "jython" nil 'no-switch))

(defun py-execute-block-or-clause-jython-dedicated ()
  "Send block-or-clause at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "jython" t nil))

(defun py-execute-block-or-clause-jython-dedicated-switch ()
  "Send block-or-clause at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "jython" t 'switch))

(defun py-execute-block-or-clause-python3.2 ()
  "Send block-or-clause at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.2" nil nil))

(defun py-execute-block-or-clause-python3.2-switch ()
  "Send block-or-clause at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.2" nil 'switch))

(defun py-execute-block-or-clause-python3.2-no-switch ()
  "Send block-or-clause at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.2" nil 'no-switch))

(defun py-execute-block-or-clause-python3.2-dedicated ()
  "Send block-or-clause at point to Python3.2 unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.2" t nil))

(defun py-execute-block-or-clause-python3.2-dedicated-switch ()
  "Send block-or-clause at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.2" t 'switch))

(defun py-execute-block-or-clause-python3.3 ()
  "Send block-or-clause at point to Python3.3 interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.3" nil nil))

(defun py-execute-block-or-clause-python3.3-switch ()
  "Send block-or-clause at point to Python3.3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.3" nil 'switch))

(defun py-execute-block-or-clause-python3.3-no-switch ()
  "Send block-or-clause at point to Python3.3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.3" nil 'no-switch))

(defun py-execute-block-or-clause-python3.3-dedicated ()
  "Send block-or-clause at point to Python3.3 unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.3" t nil))

(defun py-execute-block-or-clause-python3.3-dedicated-switch ()
  "Send block-or-clause at point to Python3.3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.3" t 'switch))

(defun py-execute-block-or-clause-bpython ()
  "Send block-or-clause at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "bpython" nil nil))

(defun py-execute-block-or-clause-bpython-switch ()
  "Send block-or-clause at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "bpython" nil 'switch))

(defun py-execute-block-or-clause-bpython-no-switch ()
  "Send block-or-clause at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "bpython" nil 'no-switch))

(defun py-execute-block-or-clause-bpython-dedicated ()
  "Send block-or-clause at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "bpython" t nil))

(defun py-execute-block-or-clause-bpython-dedicated-switch ()
  "Send block-or-clause at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "bpython" t 'switch))

(defun py-execute-def-python ()
  "Send def at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "def" "python" nil nil))

(defun py-execute-def-python-switch ()
  "Send def at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "python" nil 'switch))

(defun py-execute-def-python-no-switch ()
  "Send def at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "python" nil 'no-switch))

(defun py-execute-def-python-dedicated ()
  "Send def at point to Python unique interpreter. "
  (interactive)
  (py-execute-prepare "def" "python" t nil))

(defun py-execute-def-python-dedicated-switch ()
  "Send def at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "python" t 'switch))

(defun py-execute-def-ipython ()
  "Send def at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "def" "ipython" nil nil))

(defun py-execute-def-ipython-switch ()
  "Send def at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "ipython" nil 'switch))

(defun py-execute-def-ipython-no-switch ()
  "Send def at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "ipython" nil 'no-switch))

(defun py-execute-def-ipython-dedicated ()
  "Send def at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "def" "ipython" t nil))

(defun py-execute-def-ipython-dedicated-switch ()
  "Send def at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "ipython" t 'switch))

(defun py-execute-def-python3 ()
  "Send def at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "def" "python3" nil nil))

(defun py-execute-def-python3-switch ()
  "Send def at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "python3" nil 'switch))

(defun py-execute-def-python3-no-switch ()
  "Send def at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "python3" nil 'no-switch))

(defun py-execute-def-python3-dedicated ()
  "Send def at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "def" "python3" t nil))

(defun py-execute-def-python3-dedicated-switch ()
  "Send def at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "python3" t 'switch))

(defun py-execute-def-python2 ()
  "Send def at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "def" "python2" nil nil))

(defun py-execute-def-python2-switch ()
  "Send def at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "python2" nil 'switch))

(defun py-execute-def-python2-no-switch ()
  "Send def at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "python2" nil 'no-switch))

(defun py-execute-def-python2-dedicated ()
  "Send def at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "def" "python2" t nil))

(defun py-execute-def-python2-dedicated-switch ()
  "Send def at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "python2" t 'switch))

(defun py-execute-def-python2.7 ()
  "Send def at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "def" "python2.7" nil nil))

(defun py-execute-def-python2.7-switch ()
  "Send def at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "python2.7" nil 'switch))

(defun py-execute-def-python2.7-no-switch ()
  "Send def at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "python2.7" nil 'no-switch))

(defun py-execute-def-python2.7-dedicated ()
  "Send def at point to Python2.7 unique interpreter. "
  (interactive)
  (py-execute-prepare "def" "python2.7" t nil))

(defun py-execute-def-python2.7-dedicated-switch ()
  "Send def at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "python2.7" t 'switch))

(defun py-execute-def-jython ()
  "Send def at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "def" "jython" nil nil))

(defun py-execute-def-jython-switch ()
  "Send def at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "jython" nil 'switch))

(defun py-execute-def-jython-no-switch ()
  "Send def at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "jython" nil 'no-switch))

(defun py-execute-def-jython-dedicated ()
  "Send def at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "def" "jython" t nil))

(defun py-execute-def-jython-dedicated-switch ()
  "Send def at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "jython" t 'switch))

(defun py-execute-def-python3.2 ()
  "Send def at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "def" "python3.2" nil nil))

(defun py-execute-def-python3.2-switch ()
  "Send def at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "python3.2" nil 'switch))

(defun py-execute-def-python3.2-no-switch ()
  "Send def at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "python3.2" nil 'no-switch))

(defun py-execute-def-python3.2-dedicated ()
  "Send def at point to Python3.2 unique interpreter. "
  (interactive)
  (py-execute-prepare "def" "python3.2" t nil))

(defun py-execute-def-python3.2-dedicated-switch ()
  "Send def at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "python3.2" t 'switch))

(defun py-execute-def-python3.3 ()
  "Send def at point to Python3.3 interpreter. "
  (interactive)
  (py-execute-prepare "def" "python3.3" nil nil))

(defun py-execute-def-python3.3-switch ()
  "Send def at point to Python3.3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "python3.3" nil 'switch))

(defun py-execute-def-python3.3-no-switch ()
  "Send def at point to Python3.3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "python3.3" nil 'no-switch))

(defun py-execute-def-python3.3-dedicated ()
  "Send def at point to Python3.3 unique interpreter. "
  (interactive)
  (py-execute-prepare "def" "python3.3" t nil))

(defun py-execute-def-python3.3-dedicated-switch ()
  "Send def at point to Python3.3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "python3.3" t 'switch))

(defun py-execute-def-bpython ()
  "Send def at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "def" "bpython" nil nil))

(defun py-execute-def-bpython-switch ()
  "Send def at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "bpython" nil 'switch))

(defun py-execute-def-bpython-no-switch ()
  "Send def at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "bpython" nil 'no-switch))

(defun py-execute-def-bpython-dedicated ()
  "Send def at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "def" "bpython" t nil))

(defun py-execute-def-bpython-dedicated-switch ()
  "Send def at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "bpython" t 'switch))

(defun py-execute-class-python ()
  "Send class at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "class" "python" nil nil))

(defun py-execute-class-python-switch ()
  "Send class at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "python" nil 'switch))

(defun py-execute-class-python-no-switch ()
  "Send class at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "python" nil 'no-switch))

(defun py-execute-class-python-dedicated ()
  "Send class at point to Python unique interpreter. "
  (interactive)
  (py-execute-prepare "class" "python" t nil))

(defun py-execute-class-python-dedicated-switch ()
  "Send class at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "python" t 'switch))

(defun py-execute-class-ipython ()
  "Send class at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "class" "ipython" nil nil))

(defun py-execute-class-ipython-switch ()
  "Send class at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "ipython" nil 'switch))

(defun py-execute-class-ipython-no-switch ()
  "Send class at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "ipython" nil 'no-switch))

(defun py-execute-class-ipython-dedicated ()
  "Send class at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "class" "ipython" t nil))

(defun py-execute-class-ipython-dedicated-switch ()
  "Send class at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "ipython" t 'switch))

(defun py-execute-class-python3 ()
  "Send class at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "class" "python3" nil nil))

(defun py-execute-class-python3-switch ()
  "Send class at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "python3" nil 'switch))

(defun py-execute-class-python3-no-switch ()
  "Send class at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "python3" nil 'no-switch))

(defun py-execute-class-python3-dedicated ()
  "Send class at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "class" "python3" t nil))

(defun py-execute-class-python3-dedicated-switch ()
  "Send class at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "python3" t 'switch))

(defun py-execute-class-python2 ()
  "Send class at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "class" "python2" nil nil))

(defun py-execute-class-python2-switch ()
  "Send class at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "python2" nil 'switch))

(defun py-execute-class-python2-no-switch ()
  "Send class at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "python2" nil 'no-switch))

(defun py-execute-class-python2-dedicated ()
  "Send class at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "class" "python2" t nil))

(defun py-execute-class-python2-dedicated-switch ()
  "Send class at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "python2" t 'switch))

(defun py-execute-class-python2.7 ()
  "Send class at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "class" "python2.7" nil nil))

(defun py-execute-class-python2.7-switch ()
  "Send class at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "python2.7" nil 'switch))

(defun py-execute-class-python2.7-no-switch ()
  "Send class at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "python2.7" nil 'no-switch))

(defun py-execute-class-python2.7-dedicated ()
  "Send class at point to Python2.7 unique interpreter. "
  (interactive)
  (py-execute-prepare "class" "python2.7" t nil))

(defun py-execute-class-python2.7-dedicated-switch ()
  "Send class at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "python2.7" t 'switch))

(defun py-execute-class-jython ()
  "Send class at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "class" "jython" nil nil))

(defun py-execute-class-jython-switch ()
  "Send class at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "jython" nil 'switch))

(defun py-execute-class-jython-no-switch ()
  "Send class at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "jython" nil 'no-switch))

(defun py-execute-class-jython-dedicated ()
  "Send class at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "class" "jython" t nil))

(defun py-execute-class-jython-dedicated-switch ()
  "Send class at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "jython" t 'switch))

(defun py-execute-class-python3.2 ()
  "Send class at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "class" "python3.2" nil nil))

(defun py-execute-class-python3.2-switch ()
  "Send class at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "python3.2" nil 'switch))

(defun py-execute-class-python3.2-no-switch ()
  "Send class at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "python3.2" nil 'no-switch))

(defun py-execute-class-python3.2-dedicated ()
  "Send class at point to Python3.2 unique interpreter. "
  (interactive)
  (py-execute-prepare "class" "python3.2" t nil))

(defun py-execute-class-python3.2-dedicated-switch ()
  "Send class at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "python3.2" t 'switch))

(defun py-execute-class-python3.3 ()
  "Send class at point to Python3.3 interpreter. "
  (interactive)
  (py-execute-prepare "class" "python3.3" nil nil))

(defun py-execute-class-python3.3-switch ()
  "Send class at point to Python3.3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "python3.3" nil 'switch))

(defun py-execute-class-python3.3-no-switch ()
  "Send class at point to Python3.3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "python3.3" nil 'no-switch))

(defun py-execute-class-python3.3-dedicated ()
  "Send class at point to Python3.3 unique interpreter. "
  (interactive)
  (py-execute-prepare "class" "python3.3" t nil))

(defun py-execute-class-python3.3-dedicated-switch ()
  "Send class at point to Python3.3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "python3.3" t 'switch))

(defun py-execute-class-bpython ()
  "Send class at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "class" "bpython" nil nil))

(defun py-execute-class-bpython-switch ()
  "Send class at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "bpython" nil 'switch))

(defun py-execute-class-bpython-no-switch ()
  "Send class at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "bpython" nil 'no-switch))

(defun py-execute-class-bpython-dedicated ()
  "Send class at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "class" "bpython" t nil))

(defun py-execute-class-bpython-dedicated-switch ()
  "Send class at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "bpython" t 'switch))

(defun py-execute-region-python (beg end)
  "Send region at point to Python interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "python" nil nil beg end))

(defun py-execute-region-python-switch (beg end)
  "Send region at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-prepare "region" "python" nil 'switch beg end))

(defun py-execute-region-python-no-switch (beg end)
  "Send region at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-prepare "region" "python" nil 'no-switch beg end))

(defun py-execute-region-python-dedicated (beg end)
  "Send region at point to Python unique interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "python" t nil beg end))

(defun py-execute-region-python-dedicated-switch (beg end)
  "Send region at point to Python unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-prepare "region" "python" t 'switch beg end))

(defun py-execute-region-ipython (beg end)
  "Send region at point to IPython interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "ipython" nil nil beg end))

(defun py-execute-region-ipython-switch (beg end)
  "Send region at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-prepare "region" "ipython" nil 'switch beg end))

(defun py-execute-region-ipython-no-switch (beg end)
  "Send region at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-prepare "region" "ipython" nil 'no-switch beg end))

(defun py-execute-region-ipython-dedicated (beg end)
  "Send region at point to IPython unique interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "ipython" t nil beg end))

(defun py-execute-region-ipython-dedicated-switch (beg end)
  "Send region at point to IPython unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-prepare "region" "ipython" t 'switch beg end))

(defun py-execute-region-python3 (beg end)
  "Send region at point to Python3 interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "python3" nil nil beg end))

(defun py-execute-region-python3-switch (beg end)
  "Send region at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-prepare "region" "python3" nil 'switch beg end))

(defun py-execute-region-python3-no-switch (beg end)
  "Send region at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-prepare "region" "python3" nil 'no-switch beg end))

(defun py-execute-region-python3-dedicated (beg end)
  "Send region at point to Python3 unique interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "python3" t nil beg end))

(defun py-execute-region-python3-dedicated-switch (beg end)
  "Send region at point to Python3 unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-prepare "region" "python3" t 'switch beg end))

(defun py-execute-region-python2 (beg end)
  "Send region at point to Python2 interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "python2" nil nil beg end))

(defun py-execute-region-python2-switch (beg end)
  "Send region at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-prepare "region" "python2" nil 'switch beg end))

(defun py-execute-region-python2-no-switch (beg end)
  "Send region at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-prepare "region" "python2" nil 'no-switch beg end))

(defun py-execute-region-python2-dedicated (beg end)
  "Send region at point to Python2 unique interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "python2" t nil beg end))

(defun py-execute-region-python2-dedicated-switch (beg end)
  "Send region at point to Python2 unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-prepare "region" "python2" t 'switch beg end))

(defun py-execute-region-python2.7 (beg end)
  "Send region at point to Python2.7 interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "python2.7" nil nil beg end))

(defun py-execute-region-python2.7-switch (beg end)
  "Send region at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-prepare "region" "python2.7" nil 'switch beg end))

(defun py-execute-region-python2.7-no-switch (beg end)
  "Send region at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-prepare "region" "python2.7" nil 'no-switch beg end))

(defun py-execute-region-python2.7-dedicated (beg end)
  "Send region at point to Python2.7 unique interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "python2.7" t nil beg end))

(defun py-execute-region-python2.7-dedicated-switch (beg end)
  "Send region at point to Python2.7 unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-prepare "region" "python2.7" t 'switch beg end))

(defun py-execute-region-jython (beg end)
  "Send region at point to Jython interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "jython" nil nil beg end))

(defun py-execute-region-jython-switch (beg end)
  "Send region at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-prepare "region" "jython" nil 'switch beg end))

(defun py-execute-region-jython-no-switch (beg end)
  "Send region at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-prepare "region" "jython" nil 'no-switch beg end))

(defun py-execute-region-jython-dedicated (beg end)
  "Send region at point to Jython unique interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "jython" t nil beg end))

(defun py-execute-region-jython-dedicated-switch (beg end)
  "Send region at point to Jython unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-prepare "region" "jython" t 'switch beg end))

(defun py-execute-region-python3.2 (beg end)
  "Send region at point to Python3.2 interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "python3.2" nil nil beg end))

(defun py-execute-region-python3.2-switch (beg end)
  "Send region at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-prepare "region" "python3.2" nil 'switch beg end))

(defun py-execute-region-python3.2-no-switch (beg end)
  "Send region at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-prepare "region" "python3.2" nil 'no-switch beg end))

(defun py-execute-region-python3.2-dedicated (beg end)
  "Send region at point to Python3.2 unique interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "python3.2" t nil beg end))

(defun py-execute-region-python3.2-dedicated-switch (beg end)
  "Send region at point to Python3.2 unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-prepare "region" "python3.2" t 'switch beg end))

(defun py-execute-region-python3.3 (beg end)
  "Send region at point to Python3.3 interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "python3.3" nil nil beg end))

(defun py-execute-region-python3.3-switch (beg end)
  "Send region at point to Python3.3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-prepare "region" "python3.3" nil 'switch beg end))

(defun py-execute-region-python3.3-no-switch (beg end)
  "Send region at point to Python3.3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-prepare "region" "python3.3" nil 'no-switch beg end))

(defun py-execute-region-python3.3-dedicated (beg end)
  "Send region at point to Python3.3 unique interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "python3.3" t nil beg end))

(defun py-execute-region-python3.3-dedicated-switch (beg end)
  "Send region at point to Python3.3 unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-prepare "region" "python3.3" t 'switch beg end))

(defun py-execute-region-bpython (beg end)
  "Send region at point to Bpython interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "bpython" nil nil beg end))

(defun py-execute-region-bpython-switch (beg end)
  "Send region at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-prepare "region" "bpython" nil 'switch beg end))

(defun py-execute-region-bpython-no-switch (beg end)
  "Send region at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-prepare "region" "bpython" nil 'no-switch beg end))

(defun py-execute-region-bpython-dedicated (beg end)
  "Send region at point to Bpython unique interpreter. "
  (interactive "r")
  (py-execute-prepare "region" "bpython" t nil beg end))

(defun py-execute-region-bpython-dedicated-switch (beg end)
  "Send region at point to Bpython unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-prepare "region" "bpython" t 'switch beg end))

(defun py-execute-buffer-python ()
  "Send buffer at point to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python" nil nil (point-min) (point-max)))))

(defun py-execute-buffer-python-switch ()
  "Send buffer at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python" nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python-no-switch ()
  "Send buffer at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python" nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-python-dedicated ()
  "Send buffer at point to Python unique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python" t nil (point-min) (point-max)))))

(defun py-execute-buffer-python-dedicated-switch ()
  "Send buffer at point to Python unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python" t 'switch (point-min) (point-max)))))

(defun py-execute-buffer-ipython ()
  "Send buffer at point to IPython interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "ipython" nil nil (point-min) (point-max)))))

(defun py-execute-buffer-ipython-switch ()
  "Send buffer at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "ipython" nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-ipython-no-switch ()
  "Send buffer at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "ipython" nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-ipython-dedicated ()
  "Send buffer at point to IPython unique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "ipython" t nil (point-min) (point-max)))))

(defun py-execute-buffer-ipython-dedicated-switch ()
  "Send buffer at point to IPython unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "ipython" t 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python3 ()
  "Send buffer at point to Python3 interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3" nil nil (point-min) (point-max)))))

(defun py-execute-buffer-python3-switch ()
  "Send buffer at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3" nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python3-no-switch ()
  "Send buffer at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3" nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-python3-dedicated ()
  "Send buffer at point to Python3 unique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3" t nil (point-min) (point-max)))))

(defun py-execute-buffer-python3-dedicated-switch ()
  "Send buffer at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3" t 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python2 ()
  "Send buffer at point to Python2 interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python2" nil nil (point-min) (point-max)))))

(defun py-execute-buffer-python2-switch ()
  "Send buffer at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python2" nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python2-no-switch ()
  "Send buffer at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python2" nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-python2-dedicated ()
  "Send buffer at point to Python2 unique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python2" t nil (point-min) (point-max)))))

(defun py-execute-buffer-python2-dedicated-switch ()
  "Send buffer at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python2" t 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python2.7 ()
  "Send buffer at point to Python2.7 interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python2.7" nil nil (point-min) (point-max)))))

(defun py-execute-buffer-python2.7-switch ()
  "Send buffer at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python2.7" nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python2.7-no-switch ()
  "Send buffer at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python2.7" nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-python2.7-dedicated ()
  "Send buffer at point to Python2.7 unique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python2.7" t nil (point-min) (point-max)))))

(defun py-execute-buffer-python2.7-dedicated-switch ()
  "Send buffer at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python2.7" t 'switch (point-min) (point-max)))))

(defun py-execute-buffer-jython ()
  "Send buffer at point to Jython interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "jython" nil nil (point-min) (point-max)))))

(defun py-execute-buffer-jython-switch ()
  "Send buffer at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "jython" nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-jython-no-switch ()
  "Send buffer at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "jython" nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-jython-dedicated ()
  "Send buffer at point to Jython unique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "jython" t nil (point-min) (point-max)))))

(defun py-execute-buffer-jython-dedicated-switch ()
  "Send buffer at point to Jython unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "jython" t 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python3.2 ()
  "Send buffer at point to Python3.2 interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3.2" nil nil (point-min) (point-max)))))

(defun py-execute-buffer-python3.2-switch ()
  "Send buffer at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3.2" nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python3.2-no-switch ()
  "Send buffer at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3.2" nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-python3.2-dedicated ()
  "Send buffer at point to Python3.2 unique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3.2" t nil (point-min) (point-max)))))

(defun py-execute-buffer-python3.2-dedicated-switch ()
  "Send buffer at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3.2" t 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python3.3 ()
  "Send buffer at point to Python3.3 interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3.3" nil nil (point-min) (point-max)))))

(defun py-execute-buffer-python3.3-switch ()
  "Send buffer at point to Python3.3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3.3" nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python3.3-no-switch ()
  "Send buffer at point to Python3.3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3.3" nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-python3.3-dedicated ()
  "Send buffer at point to Python3.3 unique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3.3" t nil (point-min) (point-max)))))

(defun py-execute-buffer-python3.3-dedicated-switch ()
  "Send buffer at point to Python3.3 unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "python3.3" t 'switch (point-min) (point-max)))))

(defun py-execute-buffer-bpython ()
  "Send buffer at point to Bpython interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "bpython" nil nil (point-min) (point-max)))))

(defun py-execute-buffer-bpython-switch ()
  "Send buffer at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "bpython" nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-bpython-no-switch ()
  "Send buffer at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "bpython" nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-bpython-dedicated ()
  "Send buffer at point to Bpython unique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "bpython" t nil (point-min) (point-max)))))

(defun py-execute-buffer-bpython-dedicated-switch ()
  "Send buffer at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (py-execute-prepare "buffer" "bpython" t 'switch (point-min) (point-max)))))

(defun py-execute-expression-python ()
  "Send expression at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python" nil nil))

(defun py-execute-expression-python-switch ()
  "Send expression at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "python" nil 'switch))

(defun py-execute-expression-python-no-switch ()
  "Send expression at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "python" nil 'no-switch))

(defun py-execute-expression-python-dedicated ()
  "Send expression at point to Python unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python" t nil))

(defun py-execute-expression-python-dedicated-switch ()
  "Send expression at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "python" t 'switch))

(defun py-execute-expression-ipython ()
  "Send expression at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "expression" "ipython" nil nil))

(defun py-execute-expression-ipython-switch ()
  "Send expression at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "ipython" nil 'switch))

(defun py-execute-expression-ipython-no-switch ()
  "Send expression at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "ipython" nil 'no-switch))

(defun py-execute-expression-ipython-dedicated ()
  "Send expression at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "ipython" t nil))

(defun py-execute-expression-ipython-dedicated-switch ()
  "Send expression at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "ipython" t 'switch))

(defun py-execute-expression-python3 ()
  "Send expression at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python3" nil nil))

(defun py-execute-expression-python3-switch ()
  "Send expression at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "python3" nil 'switch))

(defun py-execute-expression-python3-no-switch ()
  "Send expression at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "python3" nil 'no-switch))

(defun py-execute-expression-python3-dedicated ()
  "Send expression at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python3" t nil))

(defun py-execute-expression-python3-dedicated-switch ()
  "Send expression at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "python3" t 'switch))

(defun py-execute-expression-python2 ()
  "Send expression at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python2" nil nil))

(defun py-execute-expression-python2-switch ()
  "Send expression at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "python2" nil 'switch))

(defun py-execute-expression-python2-no-switch ()
  "Send expression at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "python2" nil 'no-switch))

(defun py-execute-expression-python2-dedicated ()
  "Send expression at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python2" t nil))

(defun py-execute-expression-python2-dedicated-switch ()
  "Send expression at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "python2" t 'switch))

(defun py-execute-expression-python2.7 ()
  "Send expression at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python2.7" nil nil))

(defun py-execute-expression-python2.7-switch ()
  "Send expression at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "python2.7" nil 'switch))

(defun py-execute-expression-python2.7-no-switch ()
  "Send expression at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "python2.7" nil 'no-switch))

(defun py-execute-expression-python2.7-dedicated ()
  "Send expression at point to Python2.7 unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python2.7" t nil))

(defun py-execute-expression-python2.7-dedicated-switch ()
  "Send expression at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "python2.7" t 'switch))

(defun py-execute-expression-jython ()
  "Send expression at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "expression" "jython" nil nil))

(defun py-execute-expression-jython-switch ()
  "Send expression at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "jython" nil 'switch))

(defun py-execute-expression-jython-no-switch ()
  "Send expression at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "jython" nil 'no-switch))

(defun py-execute-expression-jython-dedicated ()
  "Send expression at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "jython" t nil))

(defun py-execute-expression-jython-dedicated-switch ()
  "Send expression at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "jython" t 'switch))

(defun py-execute-expression-python3.2 ()
  "Send expression at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python3.2" nil nil))

(defun py-execute-expression-python3.2-switch ()
  "Send expression at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "python3.2" nil 'switch))

(defun py-execute-expression-python3.2-no-switch ()
  "Send expression at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "python3.2" nil 'no-switch))

(defun py-execute-expression-python3.2-dedicated ()
  "Send expression at point to Python3.2 unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python3.2" t nil))

(defun py-execute-expression-python3.2-dedicated-switch ()
  "Send expression at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "python3.2" t 'switch))

(defun py-execute-expression-python3.3 ()
  "Send expression at point to Python3.3 interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python3.3" nil nil))

(defun py-execute-expression-python3.3-switch ()
  "Send expression at point to Python3.3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "python3.3" nil 'switch))

(defun py-execute-expression-python3.3-no-switch ()
  "Send expression at point to Python3.3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "python3.3" nil 'no-switch))

(defun py-execute-expression-python3.3-dedicated ()
  "Send expression at point to Python3.3 unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python3.3" t nil))

(defun py-execute-expression-python3.3-dedicated-switch ()
  "Send expression at point to Python3.3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "python3.3" t 'switch))

(defun py-execute-expression-bpython ()
  "Send expression at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "expression" "bpython" nil nil))

(defun py-execute-expression-bpython-switch ()
  "Send expression at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "bpython" nil 'switch))

(defun py-execute-expression-bpython-no-switch ()
  "Send expression at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "bpython" nil 'no-switch))

(defun py-execute-expression-bpython-dedicated ()
  "Send expression at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "bpython" t nil))

(defun py-execute-expression-bpython-dedicated-switch ()
  "Send expression at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "bpython" t 'switch))

(defun py-execute-partial-expression-python ()
  "Send partial-expression at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python" nil nil))

(defun py-execute-partial-expression-python-switch ()
  "Send partial-expression at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "python" nil 'switch))

(defun py-execute-partial-expression-python-no-switch ()
  "Send partial-expression at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "python" nil 'no-switch))

(defun py-execute-partial-expression-python-dedicated ()
  "Send partial-expression at point to Python unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python" t nil))

(defun py-execute-partial-expression-python-dedicated-switch ()
  "Send partial-expression at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "python" t 'switch))

(defun py-execute-partial-expression-ipython ()
  "Send partial-expression at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "ipython" nil nil))

(defun py-execute-partial-expression-ipython-switch ()
  "Send partial-expression at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "ipython" nil 'switch))

(defun py-execute-partial-expression-ipython-no-switch ()
  "Send partial-expression at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "ipython" nil 'no-switch))

(defun py-execute-partial-expression-ipython-dedicated ()
  "Send partial-expression at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "ipython" t nil))

(defun py-execute-partial-expression-ipython-dedicated-switch ()
  "Send partial-expression at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "ipython" t 'switch))

(defun py-execute-partial-expression-python3 ()
  "Send partial-expression at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3" nil nil))

(defun py-execute-partial-expression-python3-switch ()
  "Send partial-expression at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3" nil 'switch))

(defun py-execute-partial-expression-python3-no-switch ()
  "Send partial-expression at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "python3" nil 'no-switch))

(defun py-execute-partial-expression-python3-dedicated ()
  "Send partial-expression at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3" t nil))

(defun py-execute-partial-expression-python3-dedicated-switch ()
  "Send partial-expression at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3" t 'switch))

(defun py-execute-partial-expression-python2 ()
  "Send partial-expression at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2" nil nil))

(defun py-execute-partial-expression-python2-switch ()
  "Send partial-expression at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2" nil 'switch))

(defun py-execute-partial-expression-python2-no-switch ()
  "Send partial-expression at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "python2" nil 'no-switch))

(defun py-execute-partial-expression-python2-dedicated ()
  "Send partial-expression at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2" t nil))

(defun py-execute-partial-expression-python2-dedicated-switch ()
  "Send partial-expression at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2" t 'switch))

(defun py-execute-partial-expression-python2.7 ()
  "Send partial-expression at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2.7" nil nil))

(defun py-execute-partial-expression-python2.7-switch ()
  "Send partial-expression at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2.7" nil 'switch))

(defun py-execute-partial-expression-python2.7-no-switch ()
  "Send partial-expression at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "python2.7" nil 'no-switch))

(defun py-execute-partial-expression-python2.7-dedicated ()
  "Send partial-expression at point to Python2.7 unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2.7" t nil))

(defun py-execute-partial-expression-python2.7-dedicated-switch ()
  "Send partial-expression at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2.7" t 'switch))

(defun py-execute-partial-expression-jython ()
  "Send partial-expression at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "jython" nil nil))

(defun py-execute-partial-expression-jython-switch ()
  "Send partial-expression at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "jython" nil 'switch))

(defun py-execute-partial-expression-jython-no-switch ()
  "Send partial-expression at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "jython" nil 'no-switch))

(defun py-execute-partial-expression-jython-dedicated ()
  "Send partial-expression at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "jython" t nil))

(defun py-execute-partial-expression-jython-dedicated-switch ()
  "Send partial-expression at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "jython" t 'switch))

(defun py-execute-partial-expression-python3.2 ()
  "Send partial-expression at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.2" nil nil))

(defun py-execute-partial-expression-python3.2-switch ()
  "Send partial-expression at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.2" nil 'switch))

(defun py-execute-partial-expression-python3.2-no-switch ()
  "Send partial-expression at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.2" nil 'no-switch))

(defun py-execute-partial-expression-python3.2-dedicated ()
  "Send partial-expression at point to Python3.2 unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.2" t nil))

(defun py-execute-partial-expression-python3.2-dedicated-switch ()
  "Send partial-expression at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.2" t 'switch))

(defun py-execute-partial-expression-python3.3 ()
  "Send partial-expression at point to Python3.3 interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.3" nil nil))

(defun py-execute-partial-expression-python3.3-switch ()
  "Send partial-expression at point to Python3.3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.3" nil 'switch))

(defun py-execute-partial-expression-python3.3-no-switch ()
  "Send partial-expression at point to Python3.3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.3" nil 'no-switch))

(defun py-execute-partial-expression-python3.3-dedicated ()
  "Send partial-expression at point to Python3.3 unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.3" t nil))

(defun py-execute-partial-expression-python3.3-dedicated-switch ()
  "Send partial-expression at point to Python3.3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.3" t 'switch))

(defun py-execute-partial-expression-bpython ()
  "Send partial-expression at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "bpython" nil nil))

(defun py-execute-partial-expression-bpython-switch ()
  "Send partial-expression at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "bpython" nil 'switch))

(defun py-execute-partial-expression-bpython-no-switch ()
  "Send partial-expression at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "bpython" nil 'no-switch))

(defun py-execute-partial-expression-bpython-dedicated ()
  "Send partial-expression at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "bpython" t nil))

(defun py-execute-partial-expression-bpython-dedicated-switch ()
  "Send partial-expression at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "bpython" t 'switch))

(defun py-execute-line-python ()
  "Send line at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "line" "python" nil nil))

(defun py-execute-line-python-switch ()
  "Send line at point to Python interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "python" nil 'switch))

(defun py-execute-line-python-no-switch ()
  "Send line at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "python" nil 'no-switch))

(defun py-execute-line-python-dedicated ()
  "Send line at point to Python unique interpreter. "
  (interactive)
  (py-execute-prepare "line" "python" t nil))

(defun py-execute-line-python-dedicated-switch ()
  "Send line at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "python" t 'switch))

(defun py-execute-line-ipython ()
  "Send line at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "line" "ipython" nil nil))

(defun py-execute-line-ipython-switch ()
  "Send line at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "ipython" nil 'switch))

(defun py-execute-line-ipython-no-switch ()
  "Send line at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "ipython" nil 'no-switch))

(defun py-execute-line-ipython-dedicated ()
  "Send line at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "line" "ipython" t nil))

(defun py-execute-line-ipython-dedicated-switch ()
  "Send line at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "ipython" t 'switch))

(defun py-execute-line-python3 ()
  "Send line at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "line" "python3" nil nil))

(defun py-execute-line-python3-switch ()
  "Send line at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "python3" nil 'switch))

(defun py-execute-line-python3-no-switch ()
  "Send line at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "python3" nil 'no-switch))

(defun py-execute-line-python3-dedicated ()
  "Send line at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "line" "python3" t nil))

(defun py-execute-line-python3-dedicated-switch ()
  "Send line at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "python3" t 'switch))

(defun py-execute-line-python2 ()
  "Send line at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "line" "python2" nil nil))

(defun py-execute-line-python2-switch ()
  "Send line at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "python2" nil 'switch))

(defun py-execute-line-python2-no-switch ()
  "Send line at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "python2" nil 'no-switch))

(defun py-execute-line-python2-dedicated ()
  "Send line at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "line" "python2" t nil))

(defun py-execute-line-python2-dedicated-switch ()
  "Send line at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "python2" t 'switch))

(defun py-execute-line-python2.7 ()
  "Send line at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "line" "python2.7" nil nil))

(defun py-execute-line-python2.7-switch ()
  "Send line at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "python2.7" nil 'switch))

(defun py-execute-line-python2.7-no-switch ()
  "Send line at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "python2.7" nil 'no-switch))

(defun py-execute-line-python2.7-dedicated ()
  "Send line at point to Python2.7 unique interpreter. "
  (interactive)
  (py-execute-prepare "line" "python2.7" t nil))

(defun py-execute-line-python2.7-dedicated-switch ()
  "Send line at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "python2.7" t 'switch))

(defun py-execute-line-jython ()
  "Send line at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "line" "jython" nil nil))

(defun py-execute-line-jython-switch ()
  "Send line at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "jython" nil 'switch))

(defun py-execute-line-jython-no-switch ()
  "Send line at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "jython" nil 'no-switch))

(defun py-execute-line-jython-dedicated ()
  "Send line at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "line" "jython" t nil))

(defun py-execute-line-jython-dedicated-switch ()
  "Send line at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "jython" t 'switch))

(defun py-execute-line-python3.2 ()
  "Send line at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "line" "python3.2" nil nil))

(defun py-execute-line-python3.2-switch ()
  "Send line at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "python3.2" nil 'switch))

(defun py-execute-line-python3.2-no-switch ()
  "Send line at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "python3.2" nil 'no-switch))

(defun py-execute-line-python3.2-dedicated ()
  "Send line at point to Python3.2 unique interpreter. "
  (interactive)
  (py-execute-prepare "line" "python3.2" t nil))

(defun py-execute-line-python3.2-dedicated-switch ()
  "Send line at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "python3.2" t 'switch))

(defun py-execute-line-python3.3 ()
  "Send line at point to Python3.3 interpreter. "
  (interactive)
  (py-execute-prepare "line" "python3.3" nil nil))

(defun py-execute-line-python3.3-switch ()
  "Send line at point to Python3.3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "python3.3" nil 'switch))

(defun py-execute-line-python3.3-no-switch ()
  "Send line at point to Python3.3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "python3.3" nil 'no-switch))

(defun py-execute-line-python3.3-dedicated ()
  "Send line at point to Python3.3 unique interpreter. "
  (interactive)
  (py-execute-prepare "line" "python3.3" t nil))

(defun py-execute-line-python3.3-dedicated-switch ()
  "Send line at point to Python3.3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "python3.3" t 'switch))

(defun py-execute-line-bpython ()
  "Send line at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "line" "bpython" nil nil))

(defun py-execute-line-bpython-switch ()
  "Send line at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "bpython" nil 'switch))

(defun py-execute-line-bpython-no-switch ()
  "Send line at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "bpython" nil 'no-switch))

(defun py-execute-line-bpython-dedicated ()
  "Send line at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "line" "bpython" t nil))

(defun py-execute-line-bpython-dedicated-switch ()
  "Send line at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "bpython" t 'switch))

;;; Subprocess utilities and filters

(defun py-postprocess-output-buffer (buf)
  "Highlight exceptions found in BUF.
If an exception occurred return error-string, otherwise return nil.  BUF must exist.

Indicate LINE if code wasn't run from a file, thus remember line of source buffer "
  (let (file bol err-p estring ecode limit)
    (set-buffer py-buffer-name)
    ;; (switch-to-buffer py-buffer-name)
    (goto-char (point-max))
    (sit-for 1)
    ;; (message "%s" origline)
    (save-excursion
      (forward-line -1)
      (end-of-line)
      (when (re-search-backward py-shell-prompt-regexp nil t 1)
        ;; not a useful message, delete it - please tell when thinking otherwise
        (and (re-search-forward "File \"<stdin>\", line 1,.*\n" nil t)
             (replace-match ""))
        (when (and (re-search-forward py-traceback-line-re limit t)
                   (or (match-string 1) (match-string 3)))
          (when (match-string-no-properties 1)
            (replace-match (buffer-name py-exception-buffer) nil nil nil 1)
            (setq file py-exception-buffer)
            (and origline
                 (replace-match (number-to-string origline) nil nil nil 2))
            (goto-char (match-beginning 0))
            ;; if no buffer-file exists, signal "Buffer", not "File"
            (save-match-data
              (and (not (buffer-file-name
                         (or
                          (get-buffer py-exception-buffer)
                          (get-buffer (file-name-nondirectory py-exception-buffer))))) (string-match "^[ \t]*File" (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
                          (looking-at "[ \t]*File")
                          (replace-match "Buffer")))
            (add-to-list 'err-p origline)
            (add-to-list 'err-p file)
            (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                         'face 'highlight))
          ;; If not file exists, just a buffer, correct message
          (forward-line 1)
          (when (looking-at "[ \t]*\\([^\t\n\r\f]+\\)[ \t]*$")
            (setq estring (match-string-no-properties 1))
            (add-to-list 'err-p estring t)
            (setq ecode (buffer-substring-no-properties (line-end-position)
                                                        (progn (re-search-forward comint-prompt-regexp nil t 1)(match-beginning 0))))
            ;; (setq ecode (concat (split-string ecode "[ \n\t\f\r^]" t)))
            (setq ecode (replace-regexp-in-string "[ \n\t\f\r^]+" " " ecode))
            (add-to-list 'err-p ecode t)))
        ;; (and py-verbose-p (message "%s" (nth 2 err-p)))
        err-p))))

(defun py-remove-overlays-at-point ()
  "Remove overlays as set when `py-highlight-error-source-p' is non-nil. "
  (interactive "*")
  (delete-overlay (car (overlays-at (point)))))

(defun py-jump-to-exception-intern (action exception-buffer)
  (let (erg)
    (set-buffer exception-buffer)
    ;; (message "%s" (current-buffer))
    (goto-char (point-min))
    (forward-line (1- origline))
    (switch-to-buffer (current-buffer))
    (push-mark)
    (and (search-forward action (line-end-position) t)
         (and py-verbose-p (message "Exception in file %s on line %d" py-exception-buffer origline))
         (and py-highlight-error-source-p
              (setq erg (make-overlay (match-beginning 0) (match-end 0)))
              (overlay-put erg
                           'face 'highlight))
         ;; (remove-overlays (match-beginning 0) (match-end 0) 'face 'highlight)
         )))

(defun py-jump-to-exception (err-p &optional file)
  "Jump to the Python code in FILE at LINE."
  (let (
        ;; (inhibit-point-motion-hooks t)
        (file (or file (car err-p)))
        (line (cadr err-p))
        (action (nth 2 err-p))
        (errm (nth 3 err-p)))
    (cond ((and py-exception-buffer
                (buffer-live-p py-exception-buffer))
           ;; (pop-to-buffer procbuf)
           (py-jump-to-exception-intern action py-exception-buffer))
          ((ignore-errors (file-readable-p file))
           (find-file file)
           (py-jump-to-exception-intern action (get-buffer (file-name-nondirectory file))))
          ((buffer-live-p (get-buffer file))
           (set-buffer file)
           (switch-to-buffer (current-buffer))
           (py-jump-to-exception-intern action file))
          (t (setq file (find-file (read-file-name "Exception file: "
                                                   nil
                                                   file t)))
             (py-jump-to-exception-intern action file)))))

(defun py-down-exception (&optional bottom)
  "Go to the next line down in the traceback.

With \\[universal-argument] (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack."
  (interactive "P")
  (py-find-next-exception-prepare 'down (when (eq 4 (prefix-numeric-value bottom)) "BOTTOM")))

(defun py-up-exception (&optional top)
  "Go to the previous line up in the traceback.

With \\[universal-argument] (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack."
  (interactive "P")
  (unless py-last-exeption-buffer (setq py-last-exeption-buffer (current-buffer)))
  (py-find-next-exception-prepare 'up (when (eq 4 (prefix-numeric-value top)) "TOP")))

(defun py-find-next-exception-prepare (direction start)
  "Setup exception regexps depending from kind of Python shell. "
  (let* ((name (get-process (substring (buffer-name (current-buffer)) 1 -1)))
         (buffer (cond (name (buffer-name (current-buffer)))
                       ((buffer-live-p (get-buffer py-output-buffer))
                        py-output-buffer)
                       (py-last-exeption-buffer (buffer-name py-last-exeption-buffer))
                       (t (error "Don't see exeption buffer")))))
    (when buffer (set-buffer (get-buffer buffer)))
    (switch-to-buffer (current-buffer))
    (if (eq direction 'up)
        (if (string= start "TOP")
            (py-find-next-exception 'bob buffer 're-search-forward "Top")
          (py-find-next-exception 'bol buffer 're-search-backward "Top"))
      (if (string= start "BOTTOM")
          (py-find-next-exception 'eob buffer 're-search-backward "Bottom")
        (py-find-next-exception 'eol buffer 're-search-forward "Bottom")))))

(defun py-find-next-exception (start buffer searchdir errwhere)
  "Find the next Python exception and jump to the code that caused it.
START is the buffer position in BUFFER from which to begin searching
for an exception.  SEARCHDIR is a function, either
`re-search-backward' or `re-search-forward' indicating the direction
to search.  ERRWHERE is used in an error message if the limit (top or
bottom) of the trackback stack is encountered."
  (let (file line)
    (save-excursion
      (set-buffer buffer)
      (goto-char (py-point start))
      (if (funcall searchdir py-traceback-line-re nil t)
          (setq file (match-string 1)
                line (string-to-number (match-string 2)))))
    (if (and file line)
        (py-goto-exception file line)
      (error "%s of traceback" errwhere))))

(defun py-mouseto-exception (event)
  "Jump to the code which caused the Python exception at EVENT.
EVENT is usually a mouse click."
  (interactive "e")
  (cond
   ((fboundp 'event-point)
    ;; XEmacs
    (let* ((point (event-point event))
           (buffer (event-buffer event))
           (e (and point buffer (extent-at point buffer 'py-exc-info)))
           (info (and e (extent-property e 'py-exc-info))))
      (message "Event point: %d, info: %s" point info)
      (and info
           (py-jump-to-exception (car info) (cdr info)))))
   ;; Emacs -- Please port this!
   ))

(defun py-goto-exception (&optional file line)
  "Go to the line indicated by the traceback."
  (interactive)
  (let ((file file)
        (line line))
    (unless (and file line)
      (save-excursion
        (beginning-of-line)
        (if (looking-at py-traceback-line-re)
            (setq file (substring-no-properties (match-string 1))
                  line (string-to-number (match-string 2))))))
    (if (not file)
        (error "Not on a traceback line"))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;; python-mode-send.el
(defun py-output-buffer-filter (&optional beg end)
  "Clear output buffer from py-shell-input prompt etc. "
  (interactive "*")
  (let ((beg (cond (beg)
                   ((region-active-p)
                    (region-beginning))
                   (t (point-min))))
        (end (cond (end (copy-marker end))
                   ((region-active-p)
                    (copy-marker (region-end)))
                   (t (copy-marker (point-max))))))
    (goto-char beg)
    (while (re-search-forward (concat "\\(" py-shell-input-prompt-1-regexp "\\|" py-shell-input-prompt-2-regexp "\\|" "^In \\[[0-9]+\\]: *" "\\)") nil (quote move) 1)
      (replace-match ""))
    (goto-char beg)))

(defun py-send-string (string &optional process)
  "Evaluate STRING in inferior Python process."
  (interactive "sPython command: ")
  (let ((proc (or process (py-shell))))
    (comint-send-string proc string)
    (unless (string-match "\n\\'" string)
      ;; Make sure the text is properly LF-terminated.
      (comint-send-string proc "\n"))
    (when (string-match "\n[ \t].*\n?\\'" string)
      ;; If the string contains a final indented line, add a second newline so
      ;; as to make sure we terminate the multiline instruction.
      (comint-send-string proc "\n"))))

(defun py-send-file (file-name &optional process temp-file-name)
  "Send FILE-NAME to inferior Python PROCESS.
If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME."
  (interactive "fFile to send: ")
  (let* ((process (or process (get-buffer-process (py-shell))))
         (temp-file-name (when temp-file-name
                           (expand-file-name temp-file-name)))
         (file-name (or (expand-file-name file-name) temp-file-name))
         py-python-command-args)
    (when (not file-name)
      (error "If FILE-NAME is nil then TEMP-FILE-NAME must be non-nil"))
    (py-shell-send-string
     (format
      (concat "__pyfile = open('''%s''');"
              "exec(compile(__pyfile.read(), '''%s''', 'exec'));"
              "__pyfile.close()")
      ;; (or (file-remote-p temp-file-name 'localname) file-name) file-name)
      (or (file-remote-p temp-file-name 'localname) file-name) "Fehlerdatei")
     process)))
;;;

;; Pymacs
;; (defun py-load-pymacs ()
;;   "Load Pymacs as delivered with python-mode.el.
;;
;; Pymacs has been written by François Pinard and many others.
;; See original source: http://pymacs.progiciels-bpi.ca"
;;   (interactive)
;;   (let* ((pyshell (py-choose-shell))
;;          (path (getenv "PYTHONPATH"))
;;          (py-install-directory (cond ((string= "" py-install-directory)
;;                                       (py-guess-py-install-directory))
;;                                      (t (py-normalize-directory py-install-directory))))
;;          (pymacs-installed-p
;;           (ignore-errors (string-match (expand-file-name (concat py-install-directory "Pymacs")) path))))
;;     ;; Python side
;;     (unless pymacs-installed-p
;;       (setenv "PYTHONPATH" (concat
;;                             (expand-file-name py-install-directory)
;;                             path-separator
;;                             (expand-file-name py-install-directory) "completion"
;;                             (if path (concat path-separator path)))))
;;
;;     (if (py-install-directory-check)
;;         (progn
;;           ;; don't interfere with already installed Pymacs
;;           (unless (featurep 'pymacs)
;;             (load (concat py-install-directory "pymacs.el") nil t))
;;           (setenv "PYMACS_PYTHON" (if (string-match "IP" pyshell)
;;                                       "python"
;;                                     pyshell))
;;           (autoload 'pymacs-apply "pymacs")
;;           (autoload 'pymacs-call "pymacs")
;;           (autoload 'pymacs-eval "pymacs")
;;           (autoload 'pymacs-exec "pymacs")
;;           (autoload 'pymacs-load "pymacs")
;;           (require 'pymacs)
;;           (load (concat py-install-directory "completion/pycomplete.el") nil t)
;;           (add-hook 'python-mode-hook 'py-complete-initialize))
;;       (error "`py-install-directory' not set, see INSTALL"))))
;;
;; (when py-load-pymacs-p (py-load-pymacs))

(when (or py-load-pymacs-p (featurep 'pymacs))
  (defun py-load-pycomplete ()
    "Load Pymacs based pycomplete."
    (interactive)
    (let* ((path (getenv "PYTHONPATH"))
           (py-install-directory (cond ((string= "" py-install-directory)
                                        (py-guess-py-install-directory))
                                       (t (py-normalize-directory py-install-directory))))
           (pycomplete-directory (concat (expand-file-name py-install-directory) "completion")))
      (if (py-install-directory-check)
          (progn
            ;; If the Pymacs process is already running, augment its path.
            (when (and (get-process "pymacs") (fboundp 'pymacs-exec))
              (pymacs-exec (concat "sys.path.insert(0, '" pycomplete-directory "')")))
            (require 'pymacs)
            (setenv "PYTHONPATH" (concat
                                  pycomplete-directory
                                  (if path (concat path-separator path))))
            (add-to-list 'load-path pycomplete-directory)
            (require 'pycomplete)
            (add-hook 'python-mode-hook 'py-complete-initialize))
        (error "`py-install-directory' not set, see INSTALL")))))

(and (or (eq py-complete-function 'py-complete-completion-at-point) py-load-pymacs-p (featurep 'pymacs))
     (py-load-pycomplete))

;; Hooks
(add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)

;; arrange to kill temp files when Emacs exists
(add-hook 'kill-emacs-hook 'py-kill-emacs-hook)
(add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)

(add-hook 'inferior-python-mode-hook 'py-send-shell-setup-code)

(remove-hook 'python-mode-hook 'python-setup-brm)
;; (add-hook 'python-mode-hook
;;           #'(lambda ()
;;               (when py-smart-indentation
;;                 (if (bobp)
;;                     (save-excursion
;;                       (save-restriction
;;                         (widen)
;;                         (while (and (not (eobp))
;;                                     (or
;;                                      (let ((erg (syntax-ppss)))
;;                                        (or (nth 1 erg) (nth 8 erg)))
;;                                      (eq 0 (current-indentation))))
;;                           (forward-line 1))
;;                         (back-to-indentation)
;;                         (py-guess-indent-offset)))
;;                   (py-guess-indent-offset)))))

;; (add-hook 'comint-output-filter-functions
;;           'py-comint-output-filter-function)

(when py-warn-tmp-files-left-p
  (add-hook 'python-mode-hook 'py-warn-tmp-files-left))

;; FixMe: for unknown reasons this is not done by mode
(if (file-readable-p abbrev-file-name)
    (add-hook 'python-mode-hook '(lambda () (load abbrev-file-name nil t)))
  (message "Warning: %s" "no abbrev-file found, customize `abbrev-file-name' in order to make mode-specific abbrevs work. "))

(when py-sexp-function
  (add-hook 'python-mode-hook
            (lambda ()
              (set (make-local-variable 'forward-sexp-function) py-sexp-function))))

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode py-indent-tabs-mode)
            (set (make-local-variable 'beginning-of-defun-function) 'py-beginning-of-def-or-class)
            (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class)
            ;; (orgstruct-mode 1)
            ))

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (when py-load-highlight-indentation-p
;;               (unless (featurep 'highlight-indentation)
;;                 (load (concat (py-normalize-directory py-install-directory) "extensions" py-separator-char "highlight-indentation.el"))))))

(add-to-list 'same-window-buffer-names (purecopy "*Python*"))
(add-to-list 'same-window-buffer-names (purecopy "*IPython*"))

;; interpreter-mode-alist
;; It's handy to add recognition of Python files to the
;; interpreter-mode-alist and to auto-mode-alist.  With the former, we
;; can specify different `derived-modes' based on the #! line, but
;; with the latter, we can't.  So we just won't add them if they're
;; already added.

(let ((modes '(("jython" . jython-mode)
               ("python" . python-mode)
               ("python2" . python-mode)
               ("python2.6" . python-mode)
               ("python2.7" . python-mode)
               ("python3" . python-mode)
               ("python3.0" . python-mode)
               ("python3.1" . python-mode)
               ("python3.2" . python-mode)
               ("python3.3" . python-mode))))
  (while modes
    (when (not (assoc (car modes) interpreter-mode-alist))
      (push (car modes) interpreter-mode-alist))
    (setq modes (cdr modes))))

(when (not (or (rassq 'python-mode auto-mode-alist)
               (rassq 'jython-mode auto-mode-alist)))
  (push '("\\.py$" . python-mode) auto-mode-alist))

;; (add-to-list 'auto-mode-alist (cons (purecopy "\\.py\\'")  'python-mode))
;; (add-to-list 'interpreter-mode-alist (cons (purecopy "python") 'python-mode))
;; (add-to-list 'interpreter-mode-alist (cons (purecopy "jython") 'jython-mode))

(defun py-set-auto-fill-values ()
  "Internal use by `py-run-auto-fill-timer'"
  (let ((pps (syntax-ppss)))
    (cond ((and (nth 4 pps)(numberp py-comment-fill-column))
           (set (make-local-variable 'fill-column) py-comment-fill-column))
          ((and (nth 3 pps)(numberp py-docstring-fill-column))
           (set (make-local-variable 'fill-column) py-docstring-fill-column))
          (t (set (make-local-variable 'fill-column) py-fill-column-orig)))))

(defun py-run-auto-fill-timer ()
  "Set fill-column to values of `py-docstring-fill-column' resp. to `py-comment-fill-column' according to environment. "
  (when py-set-fill-column-p
    (unless py-autofill-timer
      (setq py-autofill-timer
            (run-with-idle-timer
             py-autofill-timer-delay t
             'py-set-auto-fill-values)))))

;;;
(define-derived-mode inferior-python-mode comint-mode "Inferior Python"
  "Major mode for interacting with an inferior Python process.
A Python process can be started with \\[py-shell].

Hooks `comint-mode-hook' and `inferior-python-mode-hook' are run in
that order.

You can send text to the inferior Python process from other buffers
containing Python source.
 * \\[py-switch-to-shell] switches the current buffer to the Python
    process buffer.
 * \\[py-execute-region] sends the current region to the Python process.
 * \\[py-send-region-and-go] switches to the Python process buffer
    after sending the text.

\\{inferior-python-mode-map}"
  :group 'python
  (require 'ansi-color) ; for ipython
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'comint-input-filter) 'py-input-filter)
  (add-hook 'comint-preoutput-filter-functions #'py-preoutput-filter
	    nil t)
  (set (make-local-variable 'compilation-error-regexp-alist)
       python-compilation-regexp-alist)
  (set (make-local-variable 'comint-prompt-regexp)
       (cond ((string-match "[iI][pP]ython[[:alnum:]*-]*$" py-buffer-name)
              (concat "\\("
                      (mapconcat 'identity
                                 (delq nil (list py-shell-input-prompt-1-regexp py-shell-input-prompt-2-regexp ipython-de-input-prompt-regexp ipython-de-output-prompt-regexp py-pdbtrack-input-prompt py-pydbtrack-input-prompt))
                                 "\\|")
                      "\\)"))
             (t (concat "\\("
                        (mapconcat 'identity
                                   (delq nil (list py-shell-input-prompt-1-regexp py-shell-input-prompt-2-regexp py-pdbtrack-input-prompt py-pydbtrack-input-prompt))
                                   "\\|")
                        "\\)"))))
  (if py-complete-function
      (add-hook 'completion-at-point-functions
                py-complete-function nil 'local)
    (add-hook 'completion-at-point-functions
              'py-completion-at-point nil 'local))
  (compilation-shell-minor-mode 1))

(define-derived-mode python-mode fundamental-mode python-mode-modeline-display
  "Major mode for editing Python files.

To submit a problem report, enter `\\[py-submit-bug-report]' from a
`python-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS

`py-shell'\tStart an interactive Python interpreter in another window
`py-execute-statement'\tSend statement at point to a Python interpreter
`py-beginning-of-statement'\tGo to the initial line of a simple statement

etc.

See available commands listed in files commands-python-mode at directory doc

VARIABLES

`py-indent-offset'	indentation increment
`py-shell-name'		shell command to invoke Python interpreter
`py-split-windows-on-execute-p'		When non-nil split windows
`py-switch-buffers-on-execute-p'	When non-nil switch to the Python output buffer

See available customizations listed in files variables-python-mode at directory doc

\\{python-mode-map}"
  :group 'python-mode
  ;; Local vars
  (set (make-local-variable 'outline-regexp)
       (concat (mapconcat 'identity
                          (mapcar #'(lambda (x) (concat "^\\s-*" x "\\_>"))
                                  py-outline-mode-keywords)
                          "\\|")))
  (if py-use-font-lock-doc-face-p
      (set (make-local-variable 'font-lock-defaults)
           '(py-font-lock-keywords nil nil nil nil
                                   (font-lock-syntactic-keywords
                                    . py-font-lock-syntactic-keywords)
                                   (font-lock-syntactic-face-function
                                    . py-font-lock-syntactic-face-function)))
    (set (make-local-variable 'font-lock-defaults)
         '(py-font-lock-keywords nil nil nil nil
                                 (font-lock-syntactic-keywords
                                  . py-font-lock-syntactic-keywords))))
  (set (make-local-variable 'which-func-functions) 'py-which-def-or-class)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start) "#")
  (if empty-comment-line-separates-paragraph-p
      (progn
        (set (make-local-variable 'paragraph-separate) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$")
        (set (make-local-variable 'paragraph-start) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$"))
    (set (make-local-variable 'paragraph-separate) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$")
    (set (make-local-variable 'paragraph-start) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$"))
  (set (make-local-variable 'comment-start-skip) "^[ \t]*#+ *")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-indent-function) #'py-comment-indent-function)
  (set (make-local-variable 'indent-region-function) 'py-indent-region)
  (set (make-local-variable 'indent-line-function) 'py-indent-line)
  (set (make-local-variable 'hs-hide-comments-when-hiding-all) 'py-hide-comments-when-hiding-all)
  (set (make-local-variable 'outline-heading-end-regexp) ":\\s-*\n")
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (set (make-local-variable 'add-log-current-defun-function) 'py-current-defun)
  (set (make-local-variable 'fill-paragraph-function) 'py-fill-paragraph)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'tab-width) py-indent-offset)
  (set (make-local-variable 'eldoc-documentation-function)
       #'py-eldoc-function)
  (and py-load-skeletons-p
       (py-load-skeletons)
       (set (make-local-variable 'skeleton-further-elements)
            '((< '(backward-delete-char-untabify (min py-indent-offset
                                                      (current-column))))
              (^ '(- (1+ (current-indentation)))))))
  (set (make-local-variable 'imenu-create-index-function) 'py-imenu-create-index-function)
  (and py-guess-py-install-directory-p (py-set-load-path))
  ;; (add-to-list 'load-path py-install-directory)
  ;; (add-to-list 'load-path (concat py-install-directory "extensions"))
  (and py-autopair-mode
       ;; (py-autopair-check)
       (load-library "autopair")
       (add-hook 'python-mode-hook
                 #'(lambda ()
                     (setq autopair-handle-action-fns
                           (list #'autopair-default-handle-action
                                 #'autopair-python-triple-quote-action))))
       (py-autopair-mode-on))
  (when py-trailing-whitespace-smart-delete-p
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local))
  (cond
   (py-complete-function
    (add-hook 'completion-at-point-functions
              py-complete-function nil 'local))
   (py-load-pymacs-p
    (add-hook 'completion-at-point-functions
              'py-complete-completion-at-point nil 'local))
   (t
    (add-hook 'completion-at-point-functions
              'py-shell-complete nil 'local)))
  (if py-set-fill-column-p
      (add-hook 'python-mode-hook 'py-run-auto-fill-timer)
    (remove-hook 'python-mode-hook 'py-run-auto-fill-timer))
  (when (and py-imenu-create-index-p
             (fboundp 'imenu-add-to-menubar)
             (ignore-errors (require 'imenu)))
    (imenu-add-to-menubar "PyIndex"))
  ;; add the menu
  (when py-menu
    (easy-menu-add py-menu))
  (when py-hide-show-minor-mode-p (hs-minor-mode 1))

  (when py-start-run-py-shell
    ;; py-shell may split window, provide restore
    (window-configuration-to-register 213465879)
    (unless (get-process (py-process-name))
      (let ((oldbuf (current-buffer)))
        (save-excursion
          (py-shell)
          (set-buffer py-exception-buffer))))
    ;; (jump-to-register 213465879)
    )
  ;; (run-mode-hooks 'python-mode-hook)
  (when py-outline-minor-mode-p (outline-minor-mode 1))
  (when (interactive-p) (message "python-mode loaded from: %s" python-mode-message-string)))

(define-derived-mode python2-mode python-mode "Python2"
  "Edit and run code used by Python version 2 series. "
  :group 'Python
  :abbrev nil
  (set (make-local-variable 'py-exec-command) '(format "execfile(r'%s') # PYTHON-MODE\n" filename))
  (set (make-local-variable 'py-exec-string-command) '(format "exec(r'%s') # PYTHON-MODE\n" string))
  (py-toggle-shell "python2"))

(define-derived-mode python3-mode python-mode "Python3"
  "Edit and run code used by Python version 3 series. "
  :group 'Python
  :abbrev nil
  (set (make-local-variable 'py-exec-command) '(format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" file file))
  (set (make-local-variable 'py-exec-string-command) '(format "exec(r'(%s)') # PYTHON-MODE\n" string))
  (py-toggle-shell "python3"))

(make-obsolete 'jpython-mode 'jython-mode nil)
(define-derived-mode jython-mode python-mode  "Jython"
  "Major mode for editing Jython files.
Like `python-mode', but sets up parameters for Jython subprocesses.
Runs `jython-mode-hook' after `python-mode-hook'."
  :group 'python-mode
  (py-toggle-shell "jython"))

(provide 'python-mode)
;; python-mode.el ends here
