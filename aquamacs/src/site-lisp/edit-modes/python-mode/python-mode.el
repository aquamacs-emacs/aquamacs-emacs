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
(require 'cl)
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

(defconst py-version "6.2.0")

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

(defconst py-windows-config-register 313465889
  "Internal used")

(defcustom py-backslashed-lines-indent-offset 5
  "Amount of offset per level of indentation of backslashed.
No semantic indent,  which diff to `py-indent-offset' indicates "
  :type 'integer
  :group 'python-mode)
;; (make-variable-buffer-local 'py-backslashed-lines-indent-offset)

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

(defcustom py-max-help-buffer-p nil
 "If \"\*Python-Help\*\"-buffer should appear as the only visible.

Default is nil. In help-buffer, \"q\" will close it.  "

:type 'boolean
:group 'python-mode)

(defcustom py-store-result-p nil
  "When non-nil, put resulting string of `py-execute-...' into kill-ring, so it might be yanked.

Default is nil"

  :type 'boolean
  :group 'python-mode)

(defvar py-return-result-p t
 "Internally used. When non-nil, return resulting string of `py-execute-...' functions. Imports will use it with nil.

Default is t")

(defvar py-new-session-p t
 "Internally used. See lp:1393882.

Restart py-shell once with new Emacs/python-mode. ")

(defcustom py-fast-process-p nil
  "Use `py-fast-process'.

Commands prefixed \"py-fast-...\" suitable for large output

See: large output makes Emacs freeze, lp:1253907

Results arrive in output buffer, which is not in comint-mode"

  :type 'boolean
  :group 'python-mode)

(defvar py-result nil
  "Internally used. May store result from Python process. ")

(defvar py--timer nil
  "Used by `py--run-unfontify-timer'")
(make-variable-buffer-local 'py--timer)

(defvar py--timer-delay nil
  "Used by `py--run-unfontify-timer'")
(make-variable-buffer-local 'py--timer-delay)

(defcustom py-shell-unfontify-p t
 "Run `py--run-unfontify-timer' unfontifying the shell banner-text.

Default is nil "

:type 'boolean
:group 'python-mode)

(defcustom py-load-skeletons-p nil
  "If skeleton definitions should be loaded, default is nil.

If non-nil and abbrev-mode on, block-skeletons will inserted.
Pressing \"if<SPACE>\" for example will prompt for the if-condition.
"

  :type 'boolean
  :group 'python-mode)

(defcustom py-load-pymacs-p nil
  "If Pymacs related stuff should be loaded.

Default is nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca"

  :type 'boolean
  :group 'python-mode)

(defun py---emacs-version-greater-23 ()
  "Return `t' if emacs major version is above 23"
  (< 23 (string-to-number (car (split-string emacs-version "\\.")))))

(defun py-smart-operator-check ()
  "Check, if smart-operator-mode is loaded resp. available.

Give some hints, if not."
  (interactive)
  (if (featurep 'smart-operator)
      't
    (progn
      (and (boundp 'py-smart-operator-mode-p) py-smart-operator-mode-p (message "%s" "Don't see smart-operator.el. Make sure, it's installed. See in menu Options, Manage Emacs Packages. Or get it from source: URL: http://xwl.appspot.com/ref/smart-operator.el")
           nil))))

(defcustom py-empty-line-closes-p nil
  "When non-nil, dedent after empty line following block

if True:
    print(\"Part of the if-statement\")

print(\"Not part of the if-statement\")

Default is nil

If non-nil, a C-j from empty line dedents."

  :type 'boolean
  :group 'python-mode)

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
  "If python-mode calls `smart-operator-mode-on'

Default is nil. "

  :type 'boolean
  :group 'python-mode
  :set (lambda (symbol value)
         (and (py-smart-operator-check)
              (set-default symbol value)
              (smart-operator-mode (if value 1 0)))))
;; (make-variable-buffer-local 'py-smart-operator-mode-p)

(defcustom py-sexp-function nil
  "When set, it's value is called instead of `forward-sexp', `backward-sexp'

Default is nil. "

  :type '(choice
          (const :tag "default" nil)
          (const :tag "py-end-of-partial-expression" py-end-of-partial-expression)
          (const :tag "py-end-of-expression" py-end-of-expression))
  :group 'python-mode)
;; (make-variable-buffer-local 'py-sexp-function)

(defvar py-autopair-mode nil)

(defvar py-error nil
  "Internally used. Takes the error-messages from Python process. ")

(defvar highlight-indent-active nil)
(defvar smart-operator-mode nil)

(defvar py-fill-column-orig fill-column)
(defvar py-autofill-timer nil)

(defvar py-python-completions "*Python Completions*"
  "Buffer name for Python-shell completions, internally used")

(defvar py-ipython-completions "*IPython Completions*"
  "Buffer name for IPython-shell completions, internally used")

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

(defcustom py-indent-no-completion-p nil
  "If completion function should insert a TAB when no completion found. Default is `nil'

See also `py-no-completion-calls-dabbrev-expand-p'"
  :type 'boolean
  :group 'python-mode)

(defcustom py-auto-fill-mode nil
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

(defcustom py-defun-use-top-level-p nil
  "When non-nil, keys C-M-a, C-M-e address top-level form.

Default is nil.

Beginning- end-of-defun forms use
commands `py-beginning-of-top-level', `py-end-of-top-level'

mark-defun marks top-level form at point etc."

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

(defcustom py-hide-comments-when-hiding-all t
  "Hide the comments too when you do an `hs-hide-all'."
  :type 'boolean
  :group 'python-mode)

(defcustom py-company-pycomplete-p nil
  "Load company-pycomplete stuff. Default is  nil"

  :type 'boolean
  :group 'python-mode)

(defcustom py-close-provides-newline t
  "If a newline is inserted, when line after block isn't empty. Default is non-nil. "
  :type 'boolean
  :group 'python-mode)
;; (make-variable-buffer-local 'py-close-provides-newline)

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
  "When non-nil, indent list's closing delimiter like start-column.

It will be lined up under the first character of
 the line that starts the multi-line construct, as in:

my_list = [
    1, 2, 3,
    4, 5, 6,
]

result = some_function_that_takes_arguments(
    'a', 'b', 'c',
    'd', 'e', 'f',
)

Default is nil, i.e.

my_list = [
    1, 2, 3,
    4, 5, 6,
    ]
result = some_function_that_takes_arguments(
    'a', 'b', 'c',
    'd', 'e', 'f',
    )

Examples from PEP8"

  :type 'boolean
  :group 'python-mode)

(defcustom py-closing-list-space 1
  "Number of chars, closing parenthesis outdent from opening, default is 1 "
  :type 'number
  :group 'python-mode)

(defcustom py-closing-list-keeps-space nil
  "If non-nil, closing parenthesis dedents onto column of opening plus `py-closing-list-space', default is nil "
  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-yank-active-p nil
  " When non-nil, `yank' will be followed by an `indent-according-to-mode'.

Default is nil"
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

(defcustom py-complete-function 'py-fast-complete
  "When set, enforces function todo completion, default is `py-fast-complete'.

Might not affect IPython, as `py-shell-complete' is the only known working here.
Normally python-mode knows best which function to use. "
  :type '(choice
          (const :tag "default" nil)
          (const :tag "Pymacs and company based py-complete" py-complete)
          (const :tag "py-shell-complete" py-shell-complete)
          (const :tag "py-indent-or-complete" py-indent-or-complete)
	  (const :tag "py-fast-complete" py-fast-complete)
          )
  :group 'python-mode)

(defcustom ipython-complete-function 'ipython-complete
  "Function used for completion in IPython shell buffers. "
  :type '(choice (const :tag "py-shell-complete" py-shell-complete)
                 (const :tag "Pymacs based py-complete" py-complete)
                 (const :tag "IPython's ipython-complete" ipython-complete))
  :group 'python-mode)
;; (make-variable-buffer-local 'ipython-complete-function)

(defcustom py-encoding-string " # -*- coding: utf-8 -*-"
  "Default string specifying encoding of a Python file. "
  :type 'string
  :group 'python-mode)

(defcustom py-shebang-startstring "#! /bin/env"
  "Detecting the shell in head of file. "
  :type 'string
  :group 'python-mode)

(defcustom py-flake8-command ""
  "Which command to call flakes8.

If empty, python-mode will guess some "
  :type 'string
  :group 'python-mode)

(make-obsolete-variable 'py-jpython-command-args 'py-jython-command-args nil)

(defcustom py-flake8-command-args ""
  "Arguments used by flake8.

Default is the empty string. "
  :type 'string
  :group 'python-mode)

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
;; (make-variable-buffer-local 'py-lhs-inbound-indent)

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
;; (make-variable-buffer-local 'py-smart-indentation)

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
  "Jump to innermost exception frame in Python output buffer.
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

(defcustom py-fast-completion-delay 0.1
  "Used by py--fast-send-string-intern. "

  :type 'float
  :group 'python-mode)

(defcustom py-new-shell-delay
    (if (eq system-type 'windows-nt)
      2.0
    1.0)

  "If a new comint buffer is connected to Python, commands like completion might need some delay. "

  :type 'float
  :group 'python-mode)

(defcustom py-python-send-delay 5
  "Seconds to wait for output, used by `py--send-...' functions.

See also py-ipython-send-delay"

  :type 'number
  :group 'python-mode)

(defcustom py-ipython-send-delay 9
  "Seconds to wait for output, used by `py--send-...' functions.

See also py-python-send-delay"

  :type 'number
  :group 'python-mode)

(defvar py-auto-completion-mode-p nil
  "Internally used by `py-auto-completion-mode'")

(defvar py-complete-last-modified nil
  "Internally used by `py-auto-completion-mode'")

(defvar py--auto-complete-timer nil
  "Internally used by `py-auto-completion-mode'")

(defvar py-auto-completion-buffer nil
  "Internally used by `py-auto-completion-mode'")

(defvar py--auto-complete-timer nil)
;; (make-variable-buffer-local 'py--auto-complete-timer)

(defcustom py--auto-complete-timer-delay 1
  "Seconds Emacs must be idle to trigger auto-completion.

See `py-auto-completion-mode'"

  :type 'number
  :group 'python-mode)

(defcustom py-auto-complete-p nil
  "Run python-mode's built-in auto-completion via py-complete-function. Default is  nil"

  :type 'boolean
  :group 'python-mode)
(make-variable-buffer-local 'py-auto-complete-p)

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
    # End:"
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

(defcustom py-shell-input-prompt-1-regexp ">>> "
  "A regular expression to match the input prompt of the shell."
  :type 'string
  :group 'python-mode)

(defcustom py-shell-input-prompt-2-regexp "[.][.][.] "
  "A regular expression to match the input prompt of the shell after the
  first line of input."
  :type 'string
  :group 'python-mode)

(defcustom py-max-specpdl-size max-specpdl-size
  "Heuristic exit. Limiting number of recursive calls by py-end-of-statement and related functions. Default is max-specpdl-size.

This threshold is just an approximation. It might set far higher maybe.

See lp:1235375. In case code is not to navigate due to errors, `which-function-mode' and others might make Emacs hang. Rather exit than. "

  :type 'number
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
  "When non-nil switch to the Python output buffer.

If `py-keep-windows-configuration' is t, this will take precedence over setting here. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-split-window-on-execute 'just-two
  "When non-nil split windows.

Default is just-two - when code is send to interpreter, split screen into source-code buffer and current py-shell result.

Other buffer will be hidden that way.

When set to `t', python-mode tries to reuse existing windows and will split only if needed.

With 'always, results will displayed in a new window.

Both `t' and `always' is experimental still.

For the moment: If a multitude of python-shells/buffers should be
visible, open them manually and set `py-keep-windows-configuration' to `t'.

"
      :type '(choice
          (const :tag "default" just-two)
	  (const :tag "Reuse" t)
          (const :tag "No split" nil)
	  (const :tag "just-two" just-two)
          (const :tag "always" always))

  :group 'python-mode)

(defcustom py-shell-manage-windows-p t
 "If `t', open output buffers, split windows according to
settings of `py-split-window-on-execute' and `py-switch-buffers-on-execute-p'.

Default is `t' "

:type 'boolean
:group 'python-mode)

(defcustom py-split-windows-on-execute-function 'split-window-vertically
  "How window should get splitted to display results of py-execute-... functions. "
  :type '(choice (const :tag "split-window-vertically" split-window-vertically)
		 (const :tag "split-window-horizontally" split-window-horizontally)
                 )
  :group 'python-mode)

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

(defcustom python-mode-hook nil
  "Hook run when entering Python mode."
  :group 'python-mode
  :type 'hook)

(defcustom py--imenu-create-index-p nil
  "Non-nil means Python mode creates and displays an index menu of functions and global variables. "
  :type 'boolean
  :group 'python-mode)

(defcustom py--imenu-create-index-function 'py--imenu-create-index-new
  "Switch between `py--imenu-create-index-new', which also lists modules variables,  and series 5. index-machine"
  :type '(choice (const :tag "'py--imenu-create-index-new, also lists modules variables " py--imenu-create-index-new)
                 (const :tag "py--imenu-create-index, series 5. index-machine" py--imenu-create-index-function))
  :group 'python-mode)

;;; Default shells
(defcustom py-shell-name
  (if (eq system-type 'windows-nt)
      "C:/Python27/python"
    ;; "python"
    "python")

  "A PATH/TO/EXECUTABLE or default value `py-shell' may look for, if no shell is specified by command.

On Windows default is C:/Python27/python
--there is no garantee it exists, please check your system--

Else python"
  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-shell-name)
(defvar py-default-interpreter py-shell-name)

(defcustom py-python-command
  (if (eq system-type 'windows-nt)
      ;; "C:\\Python27\\python.exe"
      "python"
   ;; "C:/Python33/Lib/site-packages/IPython"
    "python")

  "Make sure, the directory where python.exe resides in in the PATH-variable.

Windows: If needed, edit in \"Advanced System Settings/Environment Variables\" Commonly \"C:\\\\Python27\\\\python.exe\"
With Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\python.exe\"

Else /usr/bin/python"

  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-python-command)

(defcustom py-python-command-args '("-i")
  "List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python-mode)
;; (make-variable-buffer-local 'py-python-command-args)

(defcustom py-python2-command
  (if (eq system-type 'windows-nt)
      "C:\\Python27\\python"
    ;; "python2"
    "python2")

  "Make sure, the directory where python.exe resides in in the PATH-variable.

Windows: If needed, edit in \"Advanced System Settings/Environment Variables\" Commonly \"C:\\\\Python27\\\\python.exe\"
With Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\python.exe\"

Else /usr/bin/python"

  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-python2-command)

(defcustom py-python2-command-args '("-i")
  "List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python-mode)
;; (make-variable-buffer-local 'py-python2-command-args)

(defcustom py-python3-command
  (if (eq system-type 'windows-nt)
      ;; "python3"
    "C:/Python33/python"
    ;; "/usr/bin/python3"
    "python3")

  "A PATH/TO/EXECUTABLE or default value `py-shell' may look for, if
  no shell is specified by command.

On Windows see C:/Python3/python.exe
--there is no garantee it exists, please check your system--

At GNU systems see /usr/bin/python3"

  :type 'string
  :group 'python-mode)

(defcustom py-python3-command-args '("-i")
  "List of string arguments to be used when starting a Python3 shell."
  :type '(repeat string)
  :group 'python-mode)
;; (make-variable-buffer-local 'py-python3-command-args)

(defcustom py-ipython-command
  (if (eq system-type 'windows-nt)
      ;; "ipython"
    "C:\\Python27\\python"
    ;; "C:/Python33/Lib/site-packages/IPython"
    ;; "/usr/bin/ipython"
    "ipython")

  "A PATH/TO/EXECUTABLE or default value `M-x IPython RET' may look for, if no IPython-shell is specified by command.

On Windows default is \"C:\\\\Python27\\\\python.exe\"
While with Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\ipython.exe\"

Else /usr/bin/ipython"

  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-ipython-command)

(defcustom py-ipython-command-args
  (if (eq system-type 'windows-nt)
      '("-i" "C:\\Python27\\Scripts\\ipython-script.py")
    '("--pylab"))
  "List of string arguments to be used when starting a Python shell.
At Windows make sure ipython-script.py is PATH. Also setting PATH/TO/SCRIPT here should work, for example;
C:\\Python27\\Scripts\\ipython-script.py
With Anaconda the following is known to work:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\ipython-script-py\"
"
  :type '(repeat string)
  :group 'python-mode)
;; (make-variable-buffer-local 'py-ipython-command-args)

(defcustom py-jython-command
  (if (eq system-type 'windows-nt)
      "jython"
    "/usr/bin/jython")

  "A PATH/TO/EXECUTABLE or default value `M-x Jython RET' may look for, if no Jython-shell is specified by command.

Not known to work at windows
Default /usr/bin/jython"

  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-jython-command)

(defcustom py-jython-command-args '("")
  "List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python-mode)
;; (make-variable-buffer-local 'py-jython-command-args)

(defcustom py-bpython-command
  (if (eq system-type 'windows-nt)
      ;; not known to work at windows
      ""
    "/usr/bin/bpython")

  "A PATH/TO/EXECUTABLE or default value `M-x Bpython RET' may look for, if no Bpython-shell is specified by command.

Not known to work at windows
Default /usr/bin/bpython"

  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-bpython-command)

(defcustom py-bpython-command-args '("")
  "List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python-mode)
;; (make-variable-buffer-local 'py-bpython-command-args)

(defcustom py-shell-toggle-1 py-python2-command
  "A PATH/TO/EXECUTABLE or default value used by `py-toggle-shell'. "
  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-shell-toggle-1)

(defcustom py-shell-toggle-2 py-python3-command
  "A PATH/TO/EXECUTABLE or default value used by `py-toggle-shell'. "
  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-shell-toggle-2)

;;;

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
a Python process.  This is the default, for security
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

(defcustom py-input-filter-re "\\`\\s-*\\S-?\\S-?\\s-*\\'"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters."
  :type 'regexp
  :group 'python-mode)
(defvaralias 'inferior-python-filter-regexp 'py-input-filter-re)

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

M-x `py-remove-overlays-at-point' removes that highlighting."
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

(defcustom py--warn-tmp-files-left-p nil
  "Messages a warning, when `py-temp-directory' contains files susceptible being left by previous Python-mode sessions. See also lp:987534 "
  :type 'boolean
  :group 'python-mode)

(defcustom py-ipython-execute-delay 0.3
  "Delay needed by execute functions when no IPython shell is running. "
  :type 'float
  :group 'python-mode)

(defvar py-ffap-p nil)
(defvar py-ffap nil)
(defvar ffap-alist nil)

(defun py--set-ffap-form ()
  (cond ((and py-ffap-p py-ffap)
         (eval-after-load "ffap"
           '(push '(python-mode . py-module-path) ffap-alist))
         (setq ffap-alist (remove '(python-mode . py-ffap-module-path) ffap-alist))
         (setq ffap-alist (remove '(py-shell-mode . py-ffap-module-path)
                                  ffap-alist)))
        (t (setq ffap-alist (remove '(python-mode . py-ffap-module-path) ffap-alist))
           (setq ffap-alist (remove '(py-shell-mode . py-ffap-module-path)
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
         (py--set-ffap-form)))

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

(defcustom py-setup-codes '(py-shell-completion-setup-code
                            python-ffap-setup-code
                            py-eldoc-setup-code)
  "List of code run by `py--shell-send-setup-codes'."
  :type '(repeat symbol)
  :group 'python-mode)

(defcustom py-shell-prompt-regexp ">>> "
  "Regular Expression matching top\-level input prompt of python shell.
It should not contain a caret (^) at the beginning."
  :type 'string
  :group 'python-mode)
(defvar py-shell-prompt-regexp ">>> ")

(defcustom py-shell-completion-setup-code
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
  "Code used to setup completion in Python processes."
  :type 'string
  :group 'python-mode)

(defcustom python-shell-module-completion-string-code "';'.join(__COMPLETER_all_completions('''%s'''))"
  "Python code used to get completions separated by semicolons for imports.

For IPython v0.11, add the following line to
`py-shell-completion-setup-code':

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
  "Face for pseudo keywords in Python mode, like self, True, False,
  Ellipsis.

See also `py-object-reference-face'"
  :group 'python-mode)

(defvar py-pseudo-keyword-face 'py-pseudo-keyword-face)

(defface py-variable-name-face
  '((t (:inherit default)))
  ;; '((t (:inherit 'font-lock-variable-name-face)))
  "Face method decorators."
  :group 'python-mode)
(defvar py-variable-name-face 'py-variable-name-face)

(defvar py-object-reference-face 'py-object-reference-face)
(defface py-object-reference-face
  '((t (:inherit py-pseudo-keyword-face)))
  "Face when referencing object members from its class resp. method., commonly \"cls\" and \"self\""
  :group 'python-mode)

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

(defvar py-try-if-face 'py-try-if-face)

(defvar py-import-from-face 'py-import-from-face)

(defvar py-def-class-face 'py-def-class-face)

(defface py-try-if-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords. "
  :group 'python-mode)

(defface py-import-from-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords. "
  :group 'python-mode)

(defface py-def-class-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords. "
  :group 'python-mode)

;; XXX, TODO, and FIXME comments and such
(defface py-exception-name-face
  '((t (:inherit font-lock-builtin-face)))
  "."
  :group 'python-mode)
(defvar py-exception-name-face 'py-exception-name-face)

(defvar virtualenv-old-path nil)

(defvar virtualenv-old-exec-path nil)

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

(defvar py-this-abbrevs-changed nil
  "Internally used by python-mode-hook")

(defvar py-local-versioned-command nil
  "Returns locally used executable-name including its version. ")
(make-variable-buffer-local 'py-local-versioned-command)

(defvar py-shell-complete-debug nil
  "For interal use when debugging." )

(defcustom py-debug-p nil
  "When non-nil, keep resp. store information useful for debugging.

Temporary files are not deleted. Other functions might implement
some logging etc. "
  :type 'boolean
  :group 'python-mode)

(defvar py-encoding-string-re "^[ \t]*#[ \t]*-\\*-[ \t]*coding:.+-\\*-"
  "Matches encoding string of a Python file. ")

(defvar symbol-definition-start-re)
(setq symbol-definition-start-re "^[ \t]*(\\(defun\\|defvar\\|defcustom\\)")

(defvar py-shebang-regexp "#![ \t]?\\([^ \t\n]+\\)[ \t]*\\([biptj]+ython[^ \t\n]*\\)"
  "Detecting the shell in head of file. ")

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
;; (make-variable-buffer-local 'py-exec-command)

(defvar py-buffer-name nil
  "Internal use. ")

(defvar py-orig-buffer-or-file nil
  "Internal use. ")

(defvar py-which-bufname "Python")
;; (make-variable-buffer-local 'py-which-bufname)

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

(defcustom py-keep-windows-configuration nil
  "Takes precedence over `py-split-window-on-execute' and `py-switch-buffers-on-execute-p'.

See lp:1239498

To suppres window-changes due to error-signaling also, set `py-keep-windows-configuration' onto 'force

Default is nil "

  :type '(choice
          (const :tag "nil" nil)
          (const :tag "t" t)
          (const :tag "force" 'force))
  :group 'python-mode)

(defvar py-output-buffer "*Python Output*"
    "Currently unused.

Output buffer is created dynamically according to Python version and kind of process-handling")
(make-variable-buffer-local 'py-output-buffer)

(defvar py-exception-buffer nil
  "Will be set internally, let-bound, remember source buffer where error might occur. ")

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

(defvar py-partial-expression-backward-chars "^ =,\"'()[]{}:#\t\r\n\f"
  "py-partial-expression assumes chars indicated possible composing a py-partial-expression, skip it. ")
;; (setq py-partial-expression-backward-chars "^ ,\"'([{:#\t\r\n\f")

(defvar py-partial-expression-forward-chars "^ \"')}]:#\t\r\n\f")
;; (setq py-partial-expression-forward-chars "^ \"')}]:#\t\r\n\f")

(defvar py-operator-regexp "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\)[ \t]*"
  "Matches most of Python operators inclusive whitespaces around.

See also `py-assignment-regexp' ")

(defvar py-assignment-regexp "[ \t]*=[^=]"
  "Matches assignment operator inclusive whitespaces around.

See also `py-operator-regexp' ")

(defvar py-delimiter-regexp "\\(\\.[[:alnum:]]\\|,\\|;\\|:\\)[ \t\n]"
  "Delimiting elements of lists or other programming constructs. ")

(defvar py-line-number-offset 0
  "When an exception occurs as a result of py-execute-region, a
subsequent py-up-exception needs the line number where the region
started, in order to jump to the correct file line.  This variable is
set in py-execute-region and used in py--jump-to-exception.")

(defvar match-paren-no-use-syntax-pps nil)

(defvar py-traceback-line-re
  "[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.")

(defvar py-file-queue nil
  "Queue of Python temp files awaiting execution.
Currently-active file is at the head of the list.")

(defvar python-mode-abbrev-table nil)

(defvar py-pdbtrack-input-prompt "^[(<]*[Ii]?[Pp]y?db[>)]+ "
  "Recognize the prompt. ")

(defvar py-pydbtrack-input-prompt "^[(]*ipydb[>)]+ "
  "Recognize the pydb-prompt. ")

;; (setq py-pdbtrack-input-prompt "^[(<]*[Ii]?[Pp]y?db[>)]+ ")
;; (setq py-pydbtrack-input-prompt "^[(]*ipydb[>)]+ ")

;; pydb-328837.diff

;; prevent ipython.el's setting
(setq ipython-de-input-prompt-regexp "In \\[[0-9]+\\]:\\|^[ ]\\{3\\}[.]\\{3,\\}:" )

(defvar py-pdbtrack-is-tracking-p nil)

(defvar python-font-lock-keywords nil
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
Also, see the function \\[py--imenu-create-index] for a better
alternative for finding the index.")

(defvar py-imenu-generic-regexp nil)

(defvar py-imenu-generic-parens nil)

(defvar imenu-max-items)

(defvar py-mode-output-map nil
  "Keymap used in *Python Output* buffers.")

(defvar py-menu)

(defvar py-already-guessed-indent-offset nil
  "Internal use by py-indent-line.

When `this-command' is `eq' to `last-command', use the guess already computed. ")
(make-variable-buffer-local 'py-already-guessed-indent-offset)

(defvar skeleton-further-elements)

;; Add a designator to the minor mode strings
(or (assq 'py-pdbtrack-is-tracking-p minor-mode-alist)
    (push '(py-pdbtrack-is-tracking-p py-pdbtrack-minor-mode-string)
          minor-mode-alist))

(defvar smart-operator-mode nil)
(defvar autopair-mode nil)

(defvar highlight-indent-active nil)
(defvar highlight-indentation nil
  "Menu  PyEdit fails when not bound")
;; (make-variable-buffer-local 'highlight-indentation)

;;; Constants
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

(defconst py-no-outdent-1-re-raw
  (list
   "elif"
   "else"
   "except"
   "for"
   "if"
   "try"
   "while"
   ))

(defconst py-no-outdent-2-re-raw
  (list
   "break"
   "continue"
   "pass"
   "raise"
   "return"
   ))

(defconst py-no-outdent-re
  (concat
   "[ \t]*\\_<\\("
   (regexp-opt py-no-outdent-1-re-raw)
   "\\)\\_>.*:[( \t]\\_<\\("
   (regexp-opt py-no-outdent-2-re-raw)
   "\\)\\_>[)\t]*$")
  "Regular expression matching lines not to augment indent after.

See py-no-outdent-1-re-raw, py-no-outdent-2-re-raw for better readable content ")

(defconst py-assignment-re "\\_<\\w+\\_>[ \t]*\\(=\\|+=\\|*=\\|%=\\|&=\\|^=\\|<<=\\|-=\\|/=\\|**=\\||=\\|>>=\\|//=\\)"
  "If looking at the beginning of an assignment. ")

(defconst py-block-re "[ \t]*\\_<\\(class\\|def\\|for\\|if\\|try\\|while\\|with\\)\\_>[:( \n\t]*"
  "Matches the beginning of a compound statement. ")

(defconst py-minor-block-re "[ \t]*\\_<\\(for\\|if\\|try\\|with\\)\\_>[:( \n\t]*"
  "Matches the beginning of an `for', `if', `try' or `with' block. ")

(defconst py-try-block-re "[ \t]*\\_<try\\_>[: \n\t]"
  "Matches the beginning of a `try' block. ")

(defconst py-if-block-re "[ \t]*\\_<if\\_>[: \n\t]"
  "Matches the beginning of an `if' block. ")

(defconst py-class-re "[ \t]*\\_<\\(class\\)\\_>[ \n\t]"
  "Matches the beginning of a class definition. ")

(defconst py-def-or-class-re "[ \t]*\\_<\\(def\\|class\\)\\_>[ \n\t]"
  "Matches the beginning of a class- or functions definition. ")

(defconst py-def-re "[ \t]*\\_<\\(def\\)\\_>[ \n\t]"
  "Matches the beginning of a functions definition. ")

(defconst py-block-or-clause-re-raw
  (list
   "elif"
   "else"
   "except"
   "finally"
   "for"
   "if"
   "try"
   "while"
   "with")
  "Matches the beginning of a compound statement or it's clause. ")

(defvar py-block-or-clause-re
  (concat
   "[ \t]*\\_<\\("
   (regexp-opt  py-block-or-clause-re-raw)
   "\\)\\_>[( \t]*.*:?")
  "See py-block-or-clause-re-raw, which it reads. ")

(defvar py-fast-filter-re (concat "\\("
			       (mapconcat 'identity
					  (delq nil (list py-shell-input-prompt-1-regexp py-shell-input-prompt-2-regexp ipython-de-input-prompt-regexp ipython-de-output-prompt-regexp py-pdbtrack-input-prompt py-pydbtrack-input-prompt "\\.\\.\\." "\\.\\.\\.\\.:"))
					  "\\|")
			       "\\)")
  "Internally used by `py-fast-filter'. ")

(defconst py-extended-block-or-clause-re-raw
  (list
   "class"
   "def"
   "elif"
   "else"
   "except"
   "finally"
   "for"
   "if"
   "try"
   "while"
   "with")
  "Matches the beginning of a compound statement or it's clause. ")

(defconst py-extended-block-or-clause-re
  (concat
   "[ \t]*\\_<\\("
   (regexp-opt  py-extended-block-or-clause-re-raw)
   "\\)\\_>[( \t]*.*:?")
  "See py-block-or-clause-re-raw, which it reads. ")

(defconst py-top-level-form-re
  (concat
   "^\\_<[a-zA-Z_]\\|^\\_<\\("
   (regexp-opt  py-extended-block-or-clause-re-raw)
   "\\)\\_>[( \t]*.*:?")
  "A form which starts at zero indent level, but is not a comment. ")

(defconst py-block-keywords
  (concat
   "\\_<\\("
   (regexp-opt py-block-or-clause-re-raw)
   "\\)\\_>")
  "Matches known keywords opening a block. ")

(defconst py-clause-re-raw
  (list
   "elif"
   "else"
   "except"
   "finally"
   )
  "Matches the beginning of a clause. ")

(defconst py-clause-re
  (concat
   "[ \t]*\\_<\\("
   (regexp-opt  py-clause-re-raw)
   "\\)\\_>[( \t]*.*:?")
  "See py-clause-re-raw, which it reads. ")

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
(defun py-set-nil-docstring-style ()
  "Set py-docstring-style to 'nil"
  (interactive)
  (setq py-docstring-style 'nil)
  (when (and (interactive-p) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-pep-257-nn-docstring-style ()
  "Set py-docstring-style to 'pep-257-nn"
  (interactive)
  (setq py-docstring-style 'pep-257-nn)
  (when (and (interactive-p) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-pep-257-docstring-style ()
  "Set py-docstring-style to 'pep-257"
  (interactive)
  (setq py-docstring-style 'pep-257)
  (when (and (interactive-p) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-django-docstring-style ()
  "Set py-docstring-style to 'django"
  (interactive)
  (setq py-docstring-style 'django)
  (when (and (interactive-p) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-symmetric-docstring-style ()
  "Set py-docstring-style to 'symmetric"
  (interactive)
  (setq py-docstring-style 'symmetric)
  (when (and (interactive-p) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-onetwo-docstring-style ()
  "Set py-docstring-style to 'onetwo"
  (interactive)
  (setq py-docstring-style 'onetwo)
  (when (and (interactive-p) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

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
`py-force-local-shell-off' "
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

Caveat: Completion might not work that way."
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

Caveat: Completion might not work that way."
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
  "Switch `indent-tabs-mode' off. "
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
  "If `py-split-window-on-execute' should be on or off.

  Returns value of `py-split-window-on-execute' switched to. "
  (interactive)
  (let ((arg (or arg (if py-split-window-on-execute -1 1))))
    (if (< 0 arg)
        (setq py-split-window-on-execute t)
      (setq py-split-window-on-execute nil))
    (when (interactive-p) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
    py-split-window-on-execute))

(defun py-split-windows-on-execute-on (&optional arg)
  "Make sure, `py-split-window-on-execute' is on.

Returns value of `py-split-window-on-execute'. "
  (interactive "p")
  (let ((arg (or arg 1)))
    (toggle-py-split-windows-on-execute arg))
  (when (interactive-p) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  py-split-window-on-execute)

(defun py-split-windows-on-execute-off ()
  "Make sure, `py-split-window-on-execute' is off.

Returns value of `py-split-window-on-execute'. "
  (interactive)
  (toggle-py-split-windows-on-execute -1)
  (when (interactive-p) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  py-split-window-on-execute)

;; highlight-indentation
(defun py-toggle-highlight-indentation (&optional indent)
  "If `highlight-indentation-p' should be on or off. "
  (interactive "P")
  (unless (featurep 'highlight-indentation)
    (load (concat (py--normalize-directory py-install-directory) "extensions" py-separator-char "highlight-indentation.el")))
  (highlight-indentation indent)
  (when py-verbose-p (message "highlight-indent-active: %s" highlight-indent-active))
  highlight-indent-active)

(defun py-highlight-indentation-off ()
  "If `highlight-indentation-p' should be on or off. "
  (interactive)
  (unless (featurep 'highlight-indentation)
    (load (concat (py--normalize-directory py-install-directory) "extensions" py-separator-char "highlight-indentation.el")))
  (highlight-indentation-off)
  (when py-verbose-p (message "highlight-indent-active: %s" highlight-indent-active))
  highlight-indent-active)

(defun py-highlight-indentation-on ()
  "If `highlight-indentation-p' should be on or off. "
  (interactive "P")
  (unless (featurep 'highlight-indentation)
    (load (concat (py--normalize-directory py-install-directory) "extensions" py-separator-char "highlight-indentation.el")))
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

;; py-split-window-on-execute forms
(defun toggle-py-split-window-on-execute (&optional arg)
  "If `py-split-window-on-execute' should be on or off.

  Returns value of `py-split-window-on-execute' switched to. "
  (interactive)
  (let ((arg (or arg (if py-split-window-on-execute -1 1))))
    (if (< 0 arg)
        (setq py-split-window-on-execute t)
      (setq py-split-window-on-execute nil))
    (when (or py-verbose-p (interactive-p)) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
    py-split-window-on-execute))

(defun py-split-window-on-execute-on (&optional arg)
  "Make sure, `py-py-split-window-on-execute' is on.

Returns value of `py-split-window-on-execute'. "
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-py-split-window-on-execute arg))
  (when (or py-verbose-p (interactive-p)) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  py-split-window-on-execute)

(defun py-split-window-on-execute-off ()
  "Make sure, `py-split-window-on-execute' is off.

Returns value of `py-split-window-on-execute'. "
  (interactive)
  (toggle-py-split-window-on-execute -1)
  (when (or py-verbose-p (interactive-p)) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  py-split-window-on-execute)

(defun py-toggle-sexp-function ()
  "Opens customization "
  (interactive)
  (customize-variable 'py-sexp-function))

;;;
(defun py-send-shell-setup-code ()
  "Send all setup code for shell.
This function takes the list of setup code to send from the
`py-setup-codes' list."
  (let ((process (get-buffer-process (current-buffer))))
    (accept-process-output process 1)
    (dolist (code py-setup-codes)
      (when code
        (py-send-string (symbol-value code) process)))))

(defun py-shell-get-process (&optional argprompt py-dedicated-process-p shell switch py-buffer-name)
  "Get appropriate Python process for current buffer and return it."
  (interactive)
  (let ((erg (get-buffer-process (py-shell argprompt py-dedicated-process-p shell py-buffer-name))))
    (when (interactive-p) (message "%S" erg))
    erg))

(defun py-shell-send-string (string &optional process msg filename)
  "Send STRING to Python PROCESS.
When `py-verbose-p' and MSG is non-nil messages the first line of STRING."
  (interactive "sPython command: ")
  (let* ((process (or process (get-buffer-process (py-shell))))
         (lines (split-string string "\n"))
         (temp-file-name (concat (with-current-buffer (process-buffer process)
                                   (file-remote-p default-directory))
                                 (py--normalize-directory py-temp-directory)
				 ;; (md5 (user-login-name))
                                 (md5 (concat (user-login-name)(prin1-to-string (current-time))))
				 "-psss-temp.py"))
         (file-name (or filename (buffer-file-name) temp-file-name)))
    (if (> (length lines) 1)
	(with-temp-file temp-file-name
	  (insert string)
	  (delete-trailing-whitespace)
	  (py-send-file temp-file-name process temp-file-name))
      (comint-send-string process string)
      (when (or (not (string-match "\n$" string))
                (string-match "\n[ \t].*\n?$" string))
        (comint-send-string process "\n")))
    (unless py-debug-p (when (file-readable-p temp-file-name)(delete-file temp-file-name)))))

(defun py--delay-process-dependent (process)
  "Call a `py-ipython-send-delay' or `py-python-send-delay' according to process"
  (if (string-match "ipython" (prin1-to-string process))
      (accept-process-output process py-ipython-send-delay)
    (accept-process-output process py-python-send-delay)))

(defun py--send-string-no-output (string &optional process msg)
  "Send STRING to PROCESS and inhibit output display.
When MSG is non-nil messages the first line of STRING.  Return
the output."
  (let* (output
         (process (or process (get-buffer-process (py-shell))))
         (comint-preoutput-filter-functions
          (append comint-preoutput-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq output string)
                      "")))))
    (py-shell-send-string string process msg)
    (sit-for 0.1 t)
    ;; (py--delay-process-dependent process)
    (when (and output (not (string= "" output)))
      (setq output
	    (replace-regexp-in-string
	     (format "[ \n]*%s[ \n]*" py-fast-filter-re)
	     "" output)))
    output))

(defun py--send-string-return-output (string &optional process msg)
  "Send STRING to PROCESS and return output.

When MSG is non-nil messages the first line of STRING.  Return
the output."
  (with-current-buffer (process-buffer process)
    (let* (output
	   (process (or process (get-buffer-process (py-shell))))
	   (comint-preoutput-filter-functions
	    (append comint-preoutput-filter-functions
		    '(ansi-color-filter-apply
		      (lambda (string)
			(setq output (concat output string))
			"")))))
      (py-shell-send-string string process msg)
      (accept-process-output process 5)
      (when (and output (not (string= "" output)))
	(setq output
	      (replace-regexp-in-string
	       (format "[ \n]*%s[ \n]*" py-fast-filter-re)
	       "" output)))
      output)))

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
  "Switch to Python process buffer."
  (interactive)
  (pop-to-buffer (py-shell) t))

;;;
(defun py--shell-completion-get-completions (input process completion-code)
  "Retrieve available completions for INPUT using PROCESS.
Argument COMPLETION-CODE is the python code used to get
completions on the current context."
  (let ((completions
	 (py--send-string-no-output
	  (format completion-code input) process)))
    (sit-for 0.2 t)
    (when (> (length completions) 2)
      (split-string completions "^'\\|^\"\\|;\\|'$\\|\"$" t))))

(defun py--try-completion-intern (input completion)
  (let (erg)
    (when (and (stringp (setq erg (try-completion input completion)))
	       (looking-back input)
	       (not (string= input erg)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert erg))
    erg))

(defun py--try-completion (input completion)
  "Repeat `try-completion' as long as matches are found. "
  (let (erg newlist)
    (setq erg (py--try-completion-intern input completion))
    (when erg
      (dolist (elt completion)
	(unless (string= erg elt)
	  (add-to-list 'newlist elt)))
      (if (< 1 (length newlist))
	  (with-output-to-temp-buffer py-python-completions
	    (display-completion-list
	     (all-completions input (or newlist completion))))
	(when newlist (py--try-completion erg newlist)))
      (skip-chars-forward "^ \t\r\n\f")
      ;; (move-marker orig (point))
      nil)))

(defun py--shell--do-completion-at-point (process imports input orig py-exception-buffer code)
  "Do completion at point for PROCESS."
  (py--send-string-no-output py-shell-completion-setup-code process)
  (when imports
    (py--send-string-no-output imports process))
  ;; (py--delay-process-dependent process)
  (sit-for 0.1 t)
  (let* ((completion
	  (py--shell-completion-get-completions
	   input process code))
	 ;; (completion (when completions
	 ;; (try-completion input completions)))
	 newlist erg)
    (set-buffer py-exception-buffer)
    ;; (py--delay-process-dependent process)
    ;; (sit-for 1 t)
    (cond ((eq completion t)
	   (and py-verbose-p (message "py--shell--do-completion-at-point %s" "`t' is returned, not completion. Might be a bug."))
	   nil)
	  ((null completion)
	   (and py-verbose-p (message "py--shell--do-completion-at-point %s" "Don't see a completion"))
	   nil)
	  ((and completion
		(or (and (listp completion)
			 (string= input (car completion)))
		    (and (stringp completion)
			 (string= input completion))))
	   nil)
	  ((and completion (stringp completion)(not (string= input completion)))
	   (progn (delete-char (- (length input)))
		  (insert completion)
		  ;; (move-marker orig (point))
		  ;; minibuffer.el expects a list, a bug IMO
		  nil))
	  (t (py--try-completion input completion)))

    nil))

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

(defun py--warn-tmp-files-left ()
  "Detect and warn about file of form \"py11046IoE\" in py-temp-directory. "
  (let ((erg1 (file-readable-p (concat py-temp-directory py-separator-char  (car (directory-files  py-temp-directory nil "py[[:alnum:]]+$"))))))
    (when (and py-verbose-p erg1)
      (message "py--warn-tmp-files-left: %s ?" (concat py-temp-directory py-separator-char (car (directory-files  py-temp-directory nil "py[[:alnum:]]*$")))))))

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

(defun py--clause-lookup-keyword (regexp arg &optional indent orig origline)
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
      (save-match-data
	(if (or (eq major-mode 'comint-mode)
		(eq major-mode 'py-shell-mode))
	    (if
		(re-search-backward py-fast-filter-re nil t 1)
		(goto-char (match-end 0))
	      (when py-debug-p (message "%s"  "py-count-lines: Don't see a prompt here"))
	      (goto-char (point-min)))
	  (goto-char (point-min))))
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
(defun py--args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((not (= where 0))
           (cons (substring string 0 where)
                 (py--args-to-list (substring string (+ 1 where)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (if pos (py--args-to-list (substring string pos))))))))

(defun python-send-string (string)
  "Evaluate STRING in Python process."
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

(defun py-proc (&optional argprompt)
  "Return the current Python process.

Start a new process if necessary. "
  (interactive "p")
  (let ((erg
         (cond ((comint-check-proc (current-buffer))
		(get-buffer-process (buffer-name (current-buffer))))
	       (t (py-shell argprompt)))))
    (when (interactive-p) (message "%S" erg))
    erg))

;; FFAP support
(defalias 'py-module-path 'py-ffap-module-path)

(defun py-ffap-module-path (module)
  "Function for `ffap-alist' to return path for MODULE."
  (let ((process (or
                  (and (eq major-mode 'py-shell-mode)
                       (get-buffer-process (current-buffer)))
                  (py-shell-get-process))))
    (if (not process)
        nil
      (let ((module-file
             (py--send-string-no-output
              (format py-ffap-string-code module) process)))
        (when module-file
          (substring-no-properties module-file 1 -1))))))

(add-hook 'python-mode-hook 'py--set-ffap-form)


;;;
(define-abbrev-table 'python-mode-abbrev-table ())

(define-abbrev-table 'py-shell-mode-abbrev-table ())

;; pdbtrack constants
(defconst py-pdbtrack-stack-entry-regexp
  (concat ".*\\("py-shell-input-prompt-1-regexp">\\|>\\) *\\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>()]+\\)()")
  "Regular expression pdbtrack uses to find a stack trace entry.")

(defconst py-pdbtrack-marker-regexp-file-group 2
  "Group position in gud-pydb-marker-regexp that matches the file name.")

(defconst py-pdbtrack-marker-regexp-line-group 3
  "Group position in gud-pydb-marker-regexp that matches the line number.")

(defconst py-pdbtrack-marker-regexp-funcname-group 4
  "Group position in gud-pydb-marker-regexp that matches the function name.")

(defconst py-pdbtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")


;; Bindings
(defcustom py-compilation-regexp-alist
  `((,(rx line-start (1+ (any " \t")) "File \""
          (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
          "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
          (group (1+ digit)))
     1 2)
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
          "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "`compilation-error-regexp-alist' for Python-shell. "
  :type '(alist string)
  :group 'python-mode)


(defun py--point (position)
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


;;; Keymap and syntax
;; Font-lock and syntax
(setq python-font-lock-keywords
      ;; Keywords
      `(,(rx symbol-start
             (or
	      "if" "and" "del"  "not" "while" "as" "elif" "global"
	      "or" "with" "assert" "else"  "pass" "yield" "break"
	      "exec" "in" "continue" "finally" "is" "except" "raise"
	      "return"  "for" "lambda")
             symbol-end)
        (,(rx symbol-start (or "def" "class") symbol-end) . py-def-class-face)
        (,(rx symbol-start (or "import" "from") symbol-end) . py-import-from-face)
        (,(rx symbol-start (or "try" "if") symbol-end) . py-try-if-face)
        ;; functions
        (,(rx symbol-start "def" (1+ space) (group (1+ (or word ?_))))
         (1 font-lock-function-name-face))
        ;; classes
        (,(rx symbol-start (group "class") (1+ space) (group (1+ (or word ?_))))
         (1 py-def-class-face) (2 py-class-name-face))
        (,(rx symbol-start
              (or "Ellipsis" "True" "False" "None"  "__debug__" "NotImplemented")
              symbol-end) . py-pseudo-keyword-face)
        ;; Decorators.
        (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_))
                                                (0+ "." (1+ (or word ?_)))))
         (1 py-decorators-face))
	(,(rx symbol-start (or "cls" "self")
	      symbol-end) . py-object-reference-face)

        ;; Exceptions
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
        (,(rx
	   (or space line-start (not (any ".")))
	   symbol-start
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
;;        (,(rx symbol-start (or (1+ digit) (1+ hex-digit)) symbol-end) . py-number-face)
	(,(rx symbol-start (1+ digit) symbol-end) . py-number-face)
	))

(defconst py-font-lock-syntactic-keywords
  ;; Make outer chars of matching triple-quote sequences into generic
  ;; string delimiters.  Fixme: Is there a better way?
  ;; First avoid a sequence preceded by an odd number of backslashes.
  `((,(concat "\\(?:^\\|[^\\]\\(?:\\\\.\\)*\\)" ;Prefix.
              "\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\(?4:\"\\)\\(?5:\"\\)\\(?6:\"\\)\\|\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)\\(?4:'\\)\\(?5:'\\)\\(?6:'\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)\\(?4:'\\)\\(?5:'\\)\\(?6:'\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)")
     (1 (py--quote-syntax 1) t t)
     (2 (py--quote-syntax 2) t t)
     (3 (py--quote-syntax 3) t t)
     (6 (py--quote-syntax 1) t t))))

(defun py--quote-syntax (n)
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

(defun py--docstring-p (&optional beginning-of-string-position)
  "Check to see if there is a docstring at POS."
  (let* (pps
         (pos (or beginning-of-string-position
                  (and (nth 3 (setq pps (syntax-ppss))) (nth 8 pps)))))
    (save-restriction
      (widen)
      (save-excursion
	(py-beginning-of-statement)
        (and (looking-at "'''\\|\"\"\"")
	     (point))))))

(defun py--font-lock-syntactic-face-function (state)
  (if (nth 3 state)
      (if (py--docstring-p (nth 8 state))
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
        ((and py-electric-colon-bobl-only (save-excursion (py-beginning-of-statement) (not (py--beginning-of-block-p))))
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
                           (and (py--top-level-form-p)(< (current-indentation) indent)))
                 (beginning-of-line)
                 (delete-horizontal-space)
                 (indent-to indent))
               (goto-char orig))
             (when py-electric-colon-newline-and-indent-p
               (py-newline-and-indent))))))

(defun py--top-level-form-p ()
  "Return non-nil, if line starts with a top level definition.

Used by `py-electric-colon', which will not indent than. "
  (let (erg)
    (save-excursion
      (beginning-of-line)
      (setq erg (or (looking-at py-class-re)
                    (looking-at py-def-re))))
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
  (let ((arg (or arg 1))
	(orig (point))
	delchars)
    (dotimes (i arg)
      (setq delchars 1)
      (when (<= py-indent-offset (skip-chars-forward " \t"))
	(setq delchars py-indent-offset))
      (goto-char orig)
      (delete-char delchars))))

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

(defun py--indent-fix-region-intern (beg end)
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
          (indent-to (py-compute-indentation)))))))

(defun py--indent-line-intern (need cui py-indent-offset col &optional beg end region)
  (let (erg)
    (if py-tab-indent
	(progn
	  (and py-tab-indents-region-p region
	       (py--indent-fix-region-intern beg end))
	  (cond ((eq need cui)
		 (if (or (eq this-command last-command)
			 (eq this-command 'py-indent-line))
		     (if (and py-tab-shifts-region-p region)
			 (while (and (goto-char beg) (< 0 (current-indentation)))
			   (py-shift-region-left 1 beg end))
		       (beginning-of-line)
		       (delete-horizontal-space)
		       (if (<= (line-beginning-position) (+ (point) (- col cui)))
			   (forward-char (- col cui))
			 (beginning-of-line)))))
		((< cui need)
		 (if (and py-tab-shifts-region-p region)
		     (progn
		       (py-shift-region-right 1))
		   (progn
		     (beginning-of-line)
		     (delete-horizontal-space)
		     ;; indent one py-indent-offset only if goal < need
		     (setq erg (+ (* (/ cui py-indent-offset) py-indent-offset) py-indent-offset))
		     (if (< need erg)
			 (indent-to need)
		       (indent-to erg))
		     (forward-char (- col cui)))))
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
		       (while (< (current-indentation) need)
			 (py-shift-region-right 1)))
		   (beginning-of-line)
		   (delete-horizontal-space)
		   (indent-to need)
		   (back-to-indentation)
		   (if (<= (line-beginning-position) (+ (point) (- col cui)))
		       (forward-char (- col cui))
		     (beginning-of-line))))))
      (insert-tab))))

(defun py--indent-line-base (beg end region cui need arg this-indent-offset col)
  (unless (and (not (eq this-command last-command))
               (eq cui need))
    (cond ((eq 4 (prefix-numeric-value arg))
           (if (and (eq cui (current-indentation))
                    (<= need cui))
               (if indent-tabs-mode (insert "\t")(insert (make-string py-indent-offset 32)))
             (beginning-of-line)
             (delete-horizontal-space)
             (indent-to (+ need py-indent-offset))))
          ((not (eq 1 (prefix-numeric-value arg)))
           (py-smart-indentation-off)
           (py--indent-line-intern need cui this-indent-offset col beg end region))
          (t (py--indent-line-intern need cui this-indent-offset col beg end region)))))

(defun py-indent-line (&optional arg)
  "Indent the current line according to Python rules.

When called interactivly with \\[universal-argument], ignore dedenting rules for block closing statements
\(e.g. return, raise, break, continue, pass)

An optional \\[universal-argument] followed by a numeric argument neither 1 nor 4 will switch off `py-smart-indentation' for this execution. This permits to correct allowed but unwanted indents.
Similar to `toggle-py-smart-indentation' resp. `py-smart-indentation-off' followed by TAB.

This function is normally used by `indent-line-function' resp.
\\[indent-for-tab-command].

When bound to TAB, C-q TAB inserts a TAB.

When `py-tab-shifts-region-p' is `t', not just the current line,
but the region is shiftet that way.

If `py-tab-indents-region-p' is `t' and first TAB doesn't shift
--as indent is at outmost reasonable--, indent-region is called.

C-q TAB inserts a literal TAB-character."
  (interactive "P")
  (let ((orig (copy-marker (point)))
        (cui (current-indentation))
        need)
    ;; this-command might be `py-indent-or-complete'
    (if (or (interactive-p) (eq this-command last-command))
        ;; TAB-leaves-point-in-the-wrong-lp-1178453-test
        (let ((region (use-region-p))
              col beg end done)
          (unless (eq this-command last-command)
            (setq py-already-guessed-indent-offset nil))
          (and region
               (setq beg (region-beginning))
               (setq end (region-end))
               (save-excursion
                 (goto-char beg)
                 (setq cui (current-indentation))
                 (setq col (current-column))))
          (let* ((col (or col (current-column)))
                 (this-indent-offset
                  (cond ((and py-smart-indentation (not (eq this-command last-command)))
                         (py-guess-indent-offset))
                        ((and py-smart-indentation (eq this-command last-command) py-already-guessed-indent-offset)
                         py-already-guessed-indent-offset)
                        (t (default-value 'py-indent-offset)))))
            (setq need (if (and (eq this-command last-command) py-already-guessed-indent-offset)
                           (if region
                               (save-excursion
                                 ;; if previous command was an indent
                                 ;; already, position reached might
                                 ;; produce false guesses
                                 (goto-char beg) (py-compute-indentation beg nil nil nil nil nil py-already-guessed-indent-offset))
                             (py-compute-indentation nil nil nil nil nil nil py-already-guessed-indent-offset))
                         (if region
                             (save-excursion (goto-char beg) (py-compute-indentation nil nil nil nil nil nil this-indent-offset))
                           (py-compute-indentation nil nil nil nil nil nil this-indent-offset))))
            (py--indent-line-base beg end region cui need arg this-indent-offset col)
            (if region
                (and (or py-tab-shifts-region-p
                         py-tab-indents-region-p)
                     (not (eq (point) orig))
                     (exchange-point-and-mark))
              (and (< (current-column) (current-indentation))(back-to-indentation)))
            (when (and (interactive-p) py-verbose-p)(message "%s" (current-indentation)))
            (current-indentation)))
      (setq need (py-compute-indentation))
      (unless (eq cui need)
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to need)
        (if (< (point) orig) (goto-char orig)(back-to-indentation))))))

(defun py--delete-trailing-whitespace ()
  "Delete trailing whitespace if either `py-newline-delete-trailing-whitespace-p' or `py-trailing-whitespace-smart-delete-p' are `t' "
  (when (or py-newline-delete-trailing-whitespace-p py-trailing-whitespace-smart-delete-p)
    (setq pos (copy-marker (point)))
    (save-excursion
      (goto-char orig)
      (if (empty-line-p)
	  (if (py---emacs-version-greater-23)
	      (delete-trailing-whitespace (line-beginning-position) pos)
	    (save-restriction
	      (narrow-to-region (line-beginning-position) pos)
	      (delete-trailing-whitespace)))
	(skip-chars-backward " \t")
	(if (py---emacs-version-greater-23)
	    (delete-trailing-whitespace (line-beginning-position) pos)
	  (save-restriction
	    (narrow-to-region (point) pos)
	    (delete-trailing-whitespace)))))))

(defun py-newline-and-indent ()
  "Add a newline and indent to outmost reasonable indent.
When indent is set back manually, this is honoured in following lines. "
  (interactive "*")
  (let* ((orig (point))
	 (lkmd (prin1-to-string last-command))
	 ;; lp:1280982, deliberatly dedented by user
	 (this-dedent
	  (when (and (or (eq 10 (char-after))(eobp))(looking-back "^[ \t]*"))
	    (current-column)))
	 erg pos)
    (newline)
    (py--delete-trailing-whitespace)
    (setq erg
	  (cond (this-dedent
		 (indent-to-column this-dedent))
		((and py-empty-line-closes-p (or (eq this-command last-command)(py--after-empty-line)))
		 (indent-to-column (save-excursion (py-beginning-of-statement)(- (current-indentation) py-indent-offset))))
		(t
		 (fixup-whitespace)
		 (indent-to-column (py-compute-indentation)))))
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

(defun py--guess-indent-final (indents orig)
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

(defun py--guess-indent-forward ()
  "Called when moving to end of a form and `py-smart-indentation' is on. "
  (let* ((first (if
                    (py--beginning-of-statement-p)
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

(defun py--guess-indent-backward ()
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
downwards from beginning of block followed by a statement. Otherwise default-value is returned."
  (interactive)
  (save-excursion
    (let* ((orig (point))
           (indents
            (cond (direction
                   (if (eq 'forward direction)
                       (py--guess-indent-forward)
                     (py--guess-indent-backward)))
                  ;; guess some usable indent is above current position
                  ((eq 0 (current-indentation))
                   (py--guess-indent-forward))
                  (t (py--guess-indent-backward))))
           (erg (py--guess-indent-final indents orig)))
      (if erg (setq py-indent-offset erg)
        (setq py-indent-offset
              (default-value 'py-indent-offset)))
      (when (interactive-p) (message "%s" py-indent-offset))
      py-indent-offset)))

;;;
(defun py--comment-indent-function ()
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
  (let ((erg (py--shift-intern (- count) start end)))
    (when (and (interactive-p) py-verbose-p) (message "%s" erg))
    erg))

(defalias 'py-shift-region-right 'py-shift-right)
(defun py-shift-right (&optional count beg end)
  "Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. "
  (interactive "p")
  (setq count (or count 1))
  (let ((erg (py--shift-intern count beg end)))
    (when (and (interactive-p) py-verbose-p) (message "%s" erg))
    erg))

(defun py--shift-intern (count &optional start end)
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

(defun py--shift-forms-base (form arg &optional beg end)
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
         (erg (py--shift-intern arg beg end)))
    (goto-char orig)
    erg))

(defun py-shift-paragraph-right (&optional arg)
  "Indent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "paragraph" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-paragraph-left (&optional arg)
  "Dedent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "paragraph" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-block-right (&optional arg)
  "Indent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "block" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-block-left (&optional arg)
  "Dedent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "block" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-minor-block-left (&optional arg)
  "Dedent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached.
A minor block is started by a `for', `if', `try' or `with'. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "minor-block" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-minor-block-right (&optional arg)
  "Indent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached.
A minor block is started by a `for', `if', `try' or `with'. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "minor-block" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-clause-right (&optional arg)
  "Indent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "clause" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-clause-left (&optional arg)
  "Dedent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "clause" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-block-or-clause-right (&optional arg)
  "Indent block-or-clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "block-or-clause" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-block-or-clause-left (&optional arg)
  "Dedent block-or-clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "block-or-clause" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-def-right (&optional arg)
  "Indent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "def" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-def-left (&optional arg)
  "Dedent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "def" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-class-right (&optional arg)
  "Indent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "class" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-class-left (&optional arg)
  "Dedent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "class" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-def-or-class-right (&optional arg)
  "Indent def-or-class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "def-or-class" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-def-or-class-left (&optional arg)
  "Dedent def-or-class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "def-or-class" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-line-right (&optional arg)
  "Indent line by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "line" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-line-left (&optional arg)
  "Dedent line by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "line" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-statement-right (&optional arg)
  "Indent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "statement" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-statement-left (&optional arg)
  "Dedent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "statement" (- (or arg py-indent-offset)))))
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
      (if (and (bolp)(eolp))
          (forward-line 1)
        (py-indent-and-forward)))
    (unless (and (bolp)(eolp)) (py-indent-line))
    (goto-char orig)))

;;; Positions
(defun py--beginning-of-paragraph-position ()
  "Returns beginning of paragraph position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
		 (py-beginning-of-paragraph)
		 (point))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-paragraph-position ()
  "Returns end of paragraph position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-paragraph)
		 (point))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-block-position ()
  "Returns beginning of block position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-block)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-block-position ()
  "Returns end of block position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-block))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning-of-minor-block-position ()
  "Returns beginning of minor-block position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-minor-block)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-minor-block-position ()
  "Returns end of minor-block position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-minor-block))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-clause-position ()
  "Returns beginning of clause position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-clause)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-clause-position ()
  "Returns end of clause position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-clause))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-block-or-clause-position ()
  "Returns beginning of block-or-clause position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-block-or-clause)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-block-or-clause-position ()
  "Returns end of block-or-clause position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-block-or-clause))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-def-position ()
  "Returns beginning of def position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-def)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-def-position ()
  "Returns end of def position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-def))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-class-position ()
  "Returns beginning of class position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-class)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-class-position ()
  "Returns end of class position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-class))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-def-or-class-position ()
  "Returns beginning of def-or-class position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-def-or-class)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-def-or-class-position ()
  "Returns end of def-or-class position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-def-or-class))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-line-position ()
  "Returns beginning of line position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-line)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-line-position ()
  "Returns end of line position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-line))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-statement-position ()
  "Returns beginning of statement position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-statement)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-statement-position ()
  "Returns end of statement position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-statement))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning-of-comment-position ()
  "Returns beginning of comment position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-comment)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-comment-position ()
  "Returns end of comment position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-comment))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning-of-top-level-position ()
  "Returns beginning of top-level position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-top-level)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-top-level-position ()
  "Returns end of top-level position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-top-level))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-partial-expression-position ()
  "Returns beginning of partial-expression position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-partial-expression)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-partial-expression-position ()
  "Returns end of partial-expression position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-partial-expression))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-expression-position ()
  "Returns beginning of expression position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-expression)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-expression-position ()
  "Returns end of expression position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-expression))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

;;; some more Positions not generated by
;; `py-write-beg-end-position-forms'
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

(defun py--in-comment-p ()
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
will work."
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
      (let ((beg (py--beginning-of-statement-position))
            (end (py--end-of-statement-position)))
        (if (and beg end)
            (when (and py-verbose-p (interactive-p)) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-statements ()
  "Bounds of consecutive multitude of statements around point.

Indented same level, which don't open blocks. "
  (interactive)
  (let* ((orig-indent (progn
                        (back-to-indentation)
                        (unless (py--beginning-of-statement-p)
                          (py-beginning-of-statement))
                        (unless (py--beginning-of-block-p)
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
                  (not (py--beginning-of-block-p))
                  (eq (current-indentation) orig-indent)))
      (setq beg last)
      (goto-char orig)
      (setq end (line-end-position))
      (while (and (setq last (line-end-position))
                  (setq end (py-down-statement))
                  (not (py--beginning-of-block-p))
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
      (let ((beg (py--beginning-of-block-position))
            (end (py--end-of-block-position)))
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
      (let ((beg (py--beginning-of-clause-position))
            (end (py--end-of-clause-position)))
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
      (let ((beg (py--beginning-of-block-or-clause-position))
            (end (py--end-of-block-or-clause-position)))
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
      (let ((beg (py--beginning-of-def-position))
            (end (py--end-of-def-position)))
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
      (let ((beg (py--beginning-of-class-position))
            (end (py--end-of-class-position)))
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

(defun py--beginning-of-buffer-position ()
  (point-min))

(defun py--end-of-buffer-position ()
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
      (let ((beg (py--beginning-of-buffer-position))
            (end (py--end-of-buffer-position)))
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
      (let ((beg (py--beginning-of-expression-position))
            (end (py--end-of-expression-position)))
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
      (let ((beg (py--beginning-of-partial-expression-position))
            (end (py--end-of-partial-expression-position)))
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
                        (unless (py--beginning-of-statement-p)
                          (py-beginning-of-statement))
                        (unless (py--beginning-of-block-p)
                          (current-indentation))))
         (orig (point))
         last beg end)
    (when orig-indent
      (setq beg (line-beginning-position))
      ;; look upward first
      (while (and
              (progn
                (unless (py--beginning-of-statement-p)
                  (py-beginning-of-statement))
                (line-beginning-position))
              (py-beginning-of-statement)
              (not (py--beginning-of-block-p))
              (eq (current-indentation) orig-indent))
        (setq beg (line-beginning-position)))
      (goto-char orig)
      (while (and (setq last (line-end-position))
                  (setq end (py-down-statement))
                  (not (py--beginning-of-block-p))
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
    (while (and (not (eobp)) (or (looking-at (concat "[ \t]*" comment-start))(nth 4 (syntax-ppss))(and (bolp)(eolp))))
      (unless (and (bolp)(eolp))
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
          (beg (or beg (py--beginning-of-block-position)))
          (end (or end (py--end-of-block-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-minor-block (&optional beg end arg)
  "Comments minor-block at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py-beginning-of-minor-block-position)))
          (end (or end (py-end-of-minor-block-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

(defun py-comment-top-level (&optional beg end arg)
  "Comments top-level form at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is  `t',
the default"
  (interactive "*")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py-beginning-of-top-level-position)))
          (end (or end (py-end-of-top-level-position))))
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
          (beg (or beg (py--beginning-of-clause-position)))
          (end (or end (py--end-of-clause-position))))
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
          (beg (or beg (py--beginning-of-block-or-clause-position)))
          (end (or end (py--end-of-block-or-clause-position))))
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
          (beg (or beg (py--beginning-of-def-position)))
          (end (or end (py--end-of-def-position))))
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
          (beg (or beg (py--beginning-of-class-position)))
          (end (or end (py--end-of-class-position))))
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
          (beg (or beg (py--beginning-of-def-or-class-position)))
          (end (or end (py--end-of-def-or-class-position))))
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
          (beg (or beg (py--beginning-of-statement-position)))
          (end (or end (py--end-of-statement-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))

;; Comment forms
(defun py--uncomment-intern (beg end)
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
      (let* (last
             (beg (or beg (save-excursion
                            (while (and (py-beginning-of-comment) (setq last (point))(prog1 (forward-line -1)(end-of-line))))
                            last))))
        (and (py-end-of-comment))
        (py--uncomment-intern beg (point))))))

(defun py-delete-comments-in-def-or-class ()
  "Delete all commented lines in def-or-class at point"
  (interactive "*")
  (save-excursion
    (let ((beg (py--beginning-of-def-or-class-position))
          (end (py--end-of-def-or-class-position)))
      (and beg end (py--delete-comments-intern beg end)))))

(defun py-delete-comments-in-class ()
  "Delete all commented lines in class at point"
  (interactive "*")
  (save-excursion
    (let ((beg (py--beginning-of-class-position))
          (end (py--end-of-class-position)))
      (and beg end (py--delete-comments-intern beg end)))))

(defun py-delete-comments-in-block ()
  "Delete all commented lines in block at point"
  (interactive "*")
  (save-excursion
    (let ((beg (py--beginning-of-block-position))
          (end (py--end-of-block-position)))
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
(defun py--join-words-wrapping (words separator line-prefix line-length)
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

(defun py--until-found (search-string liste)
  "Search liste for search-string until found. "
  (let ((liste liste) element)
    (while liste
      (if (member search-string (car liste))
          (setq element (car liste) liste nil))
      (setq liste (cdr liste)))
    element))

(defun py-end-of-string (&optional beginning-of-string-position)
  "Go to end of string at point if any, if successful return position. "
  (interactive)
  ;; (when py-debug-p (message "(current-buffer): %s" (current-buffer)))
  ;; (when py-debug-p (message "major-mode): %s" major-mode))
  (let ((orig (point))
	(beginning-of-string-position (or beginning-of-string-position (and (nth 3 (parse-partial-sexp 1 (point)))(nth 8 (parse-partial-sexp 1 (point))))
                                          (and (looking-at "\"\"\"\\|'''\\|\"\\|\'")(match-beginning 0))))
        erg)
    (if beginning-of-string-position
        (progn
          (goto-char beginning-of-string-position)
	  (when
	      ;; work around parse-partial-sexp error
	      (and (nth 3 (parse-partial-sexp 1 (point)))(nth 8 (parse-partial-sexp 1 (point))))
	    (goto-char (nth 3 (parse-partial-sexp 1 (point)))))
          (if (ignore-errors (setq erg (scan-sexps (point) 1)))
			      (goto-char erg)
	    (goto-char orig)))

      (error (concat "py-end-of-string: don't see end-of-string at " (buffer-name (current-buffer)) "at pos " (point))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py--in-or-behind-or-before-a-docstring ()
  (save-excursion
    (let* ((raw-pps (nth 8 (syntax-ppss)))
	   ;; ;; maybe just behind a string
	   (n8 (or raw-pps
		   ;; maybe in front of a string
		   (back-to-indentation)
		   (nth 8 (syntax-ppss))))
	   (n8pps (or n8
		      (when
			  (equal (string-to-syntax "|")
				 (syntax-after (point)))
			(progn
			  (< 0 (skip-chars-forward "\"'"))
			  (nth 8 (syntax-ppss)))))))
      (and n8pps (py--docstring-p n8pps)))))

(defun py--string-fence-delete-spaces (&optional start)
  "Delete spaces following or preceding delimiters of string at point. "
  (interactive "*")
  (let ((beg (or start (nth 8 (syntax-ppss)))))
    (save-excursion
      (goto-char beg)
      (skip-chars-forward "\"'")
      (delete-region (point) (progn (skip-chars-forward " \t\r\n\f")(point)))
      (goto-char beg)
      (forward-char 1)
      (skip-syntax-forward "^\|")
      (skip-chars-backward "\"'")
      (delete-region (point) (progn (skip-chars-backward " \t\r\n\f")(point))))))

(defun py--fill-fix-end (thisend orig docstring delimiters-style)
  ;; Add the number of newlines indicated by the selected style
  ;; at the end.
  (widen)
  (goto-char thisend)
  (skip-chars-backward "\"'\n ")
  (delete-region (point) (progn (skip-chars-forward " \t\r\n\f") (point)))
  (unless (eq (char-after) ?\n)
    (and
     (cdr delimiters-style)
     (or (newline (cdr delimiters-style)) t)))
  (indent-region docstring thisend)
  (goto-char orig))

(defun py--fill-docstring-base (thisbeg thisend style multi-line-p first-line-p beg end)
  (widen)
  (narrow-to-region thisbeg thisend)
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
          (symmetric (and multi-line-p (cons 1 1)))))
  ;;  (save-excursion
  (when style
    ;; Add the number of newlines indicated by the selected style
    ;; at the start.
    (goto-char thisbeg)
    (skip-chars-forward "\'\"")
    (when
	(car delimiters-style)
      (unless (or (empty-line-p)(eolp))
	(newline (car delimiters-style))))
    (indent-region beg end py-current-indent))
  (when multi-line-p
    (goto-char thisbeg)
    (skip-chars-forward "\'\"")
    (skip-chars-forward " \t\r\n\f")
    (forward-line 1)
    (beginning-of-line)
    (unless (empty-line-p) (newline)))
  (py--fill-fix-end thisend orig docstring delimiters-style))

(defun py--fill-docstring-last-line (thisbeg thisend beg end style)
  (widen)
  (narrow-to-region thisbeg thisend)
  (goto-char thisend)
  (skip-chars-backward "\"'")
  (delete-region (point) (progn (skip-chars-backward " \t\r\n\f")(point)))
  (narrow-to-region beg end)
  (fill-region beg end)
  (setq multi-line-p (string-match "\n" (buffer-substring-no-properties beg end)))
  (when multi-line-p
    ;; adjust the region to fill according to style
    (goto-char end)
    (py--fill-docstring-base thisbeg thisend style multi-line-p first-line-p beg end))
  (goto-char orig))

(defun py--fill-docstring-first-line ()
  "Refill first line after newline maybe. "
  (fill-region beg (line-end-position))
  (forward-line 1)
  (fill-region (line-beginning-position) end)
  (save-restriction
    (widen)
    (setq multi-line-p (string-match "\n" (buffer-substring-no-properties thisbeg thisend))))
  (when multi-line-p
    ;; adjust the region to fill according to style
    (goto-char beg)
    (skip-chars-forward "\"'")
    ;; style might be nil
    (when style
      (unless (or (eq style 'pep-257-nn)(eq style 'pep-257)(eq (char-after) ?\n))
	(newline-and-indent)
	;; if TQS is at a single line, re-fill remaining line
	(setq beg (point))
	(fill-region beg end)))))

(defun py--fill-docstring (justify style docstring orig)
  ;; Delete spaces after/before string fence
  (py--string-fence-delete-spaces docstring)
  (let* ((thisbeg (copy-marker docstring))
         (thisend (copy-marker
                   (progn
                     (goto-char thisbeg)
		     ;; (py-end-of-string)
		     (forward-char 1)
		     (skip-syntax-forward "^\|")
		     (skip-chars-forward "\"'")
                     (point))))
         (parabeg (progn (goto-char orig) (py--beginning-of-paragraph-position)))
         (paraend (progn (goto-char orig) (py--end-of-paragraph-position)))
         ;; if paragraph is a substring, take it
         (beg (copy-marker (if (< thisbeg parabeg) parabeg thisbeg)))
         (end (copy-marker (if (< thisend paraend) thisend paraend)))
	 (multi-line-p (string-match "\n" (buffer-substring-no-properties thisbeg thisend)))
	 erg
         first-line-p)
    ;;    (narrow-to-region beg end)
    (goto-char beg)
    (setq first-line-p (member (char-after) (list ?\" ?\')))
    (cond ((string-match (concat "^" py-labelled-re) (buffer-substring-no-properties beg end))
           (py-fill-labelled-string beg end))
          (first-line-p
           (py--fill-docstring-first-line))
          ((save-excursion (goto-char end)
			   (or (member (char-after) (list ?\" ?\'))
			       (member (char-before) (list ?\" ?\'))))
           (py--fill-docstring-last-line thisbeg thisend beg end style))
          (t (narrow-to-region beg end)
	     (fill-region beg end justify)))
    (py--fill-docstring-base thisbeg thisend style multi-line-p first-line-p beg end)))

(defun py-fill-string (&optional justify style docstring)
  "String fill function for `py-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'.

Fill according to `py-docstring-style' "
  (interactive
   (list
    (progn
      (barf-if-buffer-read-only)
      (list (if current-prefix-arg 'full) t))
    py-docstring-style
    (py--in-or-behind-or-before-a-docstring)))
  (let ((py-current-indent (save-excursion (or (py--beginning-of-statement-p) (py-beginning-of-statement)) (current-indentation)))
	;; fill-paragraph sets orig
	(orig (if (boundp 'orig) (copy-marker orig) (copy-marker (point))))
	(docstring (if (and docstring (not (number-or-marker-p docstring)))
		       (py--in-or-behind-or-before-a-docstring)
		     docstring)))
    (if docstring
	(py--fill-docstring justify style docstring orig)
      (fill-paragraph justify))))

(defun py-fill-paragraph (&optional justify)
  (interactive "*")
  (save-excursion
    (save-restriction
      (window-configuration-to-register py-windows-config-register)
      (let* ((orig (copy-marker (point)))
	     (docstring (py--in-or-behind-or-before-a-docstring)))
	(cond (docstring
	       (setq fill-column py-docstring-fill-column)
	       (py-fill-string justify py-docstring-style docstring))
	      ((let ((fill-column py-comment-fill-column))
		 (fill-comment-paragraph justify)))
	      ((save-excursion
		 (and (py-beginning-of-statement)
		      (equal (char-after) ?\@)))
	       (py-fill-decorator justify))
	      (t (fill-paragraph justify)))
	(widen))
      (jump-to-register py-windows-config-register))))

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
  (py-fill-string justify 'django t))

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
  (py-fill-string justify 'onetwo t))

(defun py-fill-string-pep-257 (&optional justify)
  "PEP-257 with 2 newlines at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'pep-257 t))

(defun py-fill-string-pep-257-nn (&optional justify)
  "PEP-257 with 1 newline at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'pep-257-nn t))

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
  (py-fill-string justify 'symmetric t))

;; Beginning-of- p
(defun py-beginning-of-top-level-p ()
  "Returns position, if cursor is at the beginning of a top-level, nil otherwise. "
  (interactive)
  (let (erg)
    (and (py--beginning-of-statement-p)
         (eq 0 (current-column))
         (setq erg (point))
         erg)))

(defun py--beginning-of-line-p ()
  "Returns position, if cursor is at the beginning of a line, nil otherwise. "
  (when (bolp)(point)))

(defun py--beginning-of-buffer-p ()
  "Returns position, if cursor is at the beginning of buffer, nil otherwise. "
  (when (bobp)(point)))

(defun py--beginning-of-paragraph-p ()
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

(defun py--beginning-of-statement-p ()
  "Returns position, if cursor is at the beginning of a statement, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-statement))
      (py-beginning-of-statement)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-expression-p ()
  "Returns position, if cursor is at the beginning of a expression, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-end-of-expression)
      (py-beginning-of-expression)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-partial-expression-p ()
  "Returns position, if cursor is at the beginning of a partial-expression, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-end-of-partial-expression)
      (py-beginning-of-partial-expression)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-block-p ()
  "Returns position, if cursor is at the beginning of a block, nil otherwise. "
  (when (and (looking-at py-block-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defun py--beginning-of-clause-p ()
  "Returns position, if cursor is at the beginning of a clause, nil otherwise. "
  (when (and (looking-at py-clause-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defun py--beginning-of-block-or-clause-p ()
  "Returns position, if cursor is at the beginning of a block-or-clause, nil otherwise. "
  (when (and (looking-at py-block-or-clause-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defun py--beginning-of-def-p ()
  "Returns position, if cursor is at the beginning of a def, nil otherwise. "
  (when (and (looking-at py-def-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defun py--beginning-of-class-p ()
  "Returns position, if cursor is at the beginning of a class, nil otherwise. "
  (when (and (looking-at py-class-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defun py--beginning-of-def-or-class-p ()
  "Returns position, if cursor is at the beginning of a def-or-class, nil otherwise. "
  (when (and (looking-at py-def-or-class-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defun py-beginning-of-minor-block-p ()
  "Returns position, if cursor is at the beginning of a minor-block, nil otherwise.

A minor block is started by a `for', `if', `try' or `with'."
  (when (and (looking-at py-minor-block-re)
             (not (py-in-string-or-comment-p)))
    (point)))

;; End-of- p
(defun py--end-of-line-p ()
  "Returns position, if cursor is at the end of a line, nil otherwise. "
  (when (eolp)(point)))

(defun py--end-of-buffer-p ()
  "Returns position, if cursor is at the end of buffer, nil otherwise. "
  (when (eobp)(point)))

(defun py--end-of-paragraph-p ()
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

(defun py--end-of-statement-p ()
  "Returns position, if cursor is at the end of a statement, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-statement)
      (py-end-of-statement)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--end-of-expression-p ()
  "Returns position, if cursor is at the end of a expression, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-expression)
      (py-end-of-expression)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--end-of-partial-expression-p ()
  "Returns position, if cursor is at the end of a partial-expression, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-partial-expression)
      (py-end-of-partial-expression)
      (when (eq orig (point))
        (setq erg orig)))
    erg))

(defun py--end-of-block-p ()
  "Returns position, if cursor is at the end of a block, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-block)
      (py-end-of-block)
      (when (eq orig (point))
        (setq erg orig)))
    erg))

(defun py--end-of-clause-p ()
  "Returns position, if cursor is at the end of a clause, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-clause)
      (py-end-of-clause)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--end-of-block-or-clause-p ()
  "Returns position, if cursor is at the end of a block-or-clause, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-block-or-clause)
      (py-end-of-block-or-clause)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--end-of-def-p ()
  "Returns position, if cursor is at the end of a def, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-def)
      (py-end-of-def)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--end-of-class-p ()
  "Returns position, if cursor is at the end of a class, nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (py-beginning-of-class)
      (py-end-of-class)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--end-of-def-or-class-p ()
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
(defun py--statement-opens-block-p (&optional regexp)
  "Return position if the current statement opens a block
in stricter or wider sense.

For stricter sense specify regexp. "
  (let* ((regexp (or regexp py-block-or-clause-re))
         (erg (py--statement-opens-base regexp)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py--statement-opens-base (regexp)
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

(defun py--statement-opens-clause-p ()
  "Return position if the current statement opens block or clause. "
  (py--statement-opens-base py-clause-re))

(defun py--statement-opens-block-or-clause-p ()
  "Return position if the current statement opens block or clause. "
  (py--statement-opens-base py-block-or-clause-re))

(defun py--statement-opens-class-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (py--statement-opens-base py-class-re))

(defun py--statement-opens-def-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (py--statement-opens-base py-def-re))

(defun py--statement-opens-def-or-class-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (py--statement-opens-base py-def-or-class-re))

(defun py--statement-closes-block-p ()
  "Return t iff the current statement closes a block.
I.e., if the line starts with `return', `raise', `break', `continue',
and `pass'.  This doesn't catch embedded statements."
  (let ((here (point)))
    (unless (py--beginning-of-statement-p) (py-beginning-of-statement))
    (prog1
        (looking-at py-block-closing-keywords-re)
      (goto-char here))))

(defun py--record-list-error (pps)
  "When encountering a missing parenthesis, store its line, position. `py-verbose-p'  must be t

Unclosed-string errors are not handled here, as made visible by fontification already.
"
  (let ((this-err
         (save-excursion
           (list
            (nth 1 pps)
            (progn
              (goto-char (nth 1 pps))
              (py-count-lines (point-min) (point)))))))
    this-err))

(defun py--skip-to-semicolon-backward (&optional limit)
  "Fetch the beginning of statement after a semicolon.

Returns position reached if point was moved. "
  (let ((orig (point)))
    (and (< 0 (abs (skip-chars-backward "^;" (or limit (line-beginning-position)))))
	 (skip-chars-forward " \t" (line-end-position))
	 (setq done t)
	 (and (< (point) orig) (point)))))

(defun py--message-error (err)
  "Receives a list (position line) "
  (message "Closing parent missed: line %s pos %s" (cadr err) (car err)))

(defun py--end-base (regexp &optional orig decorator)
  "Used internal by functions going to the end forms. "
  (unless (eobp)
    (catch 'exit
      (let* ((orig (or orig (point)))
             (regexp (or regexp 'py-extended-block-or-clause-re))
             (thisregexp
              (cond ((eq regexp 'py-def-or-class-re)
                     py-def-or-class-re)
                    ((eq regexp 'py-def-re)
                     py-def-re)
		    ((eq regexp 'py-class-re)
		     py-class-re)
		    ((eq regexp 'py-minor-block-re)
		     py-minor-block-re)
		    (t py-extended-block-or-clause-re)))
             bofst
             (this (progn (back-to-indentation)
                          (setq bofst (py--beginning-of-statement-p))
                          (cond ((and bofst (eq regexp 'py-clause-re)(looking-at py-extended-block-or-clause-re))
                                 (point))
                                ((and bofst (looking-at thisregexp))
                                 (point))
                                (t
                                 (when
                                     (cdr-safe
                                      (py--go-to-keyword
                                       thisregexp))
                                   (when (py--statement-opens-block-p py-extended-block-or-clause-re)
                                     (point)))))))
             ind erg last pps thisindent done err)
        (cond (this
               (setq thisindent (current-indentation))
               (cond ((and py-close-provides-newline
                           (or (eq regexp 'py-def-re)(eq regexp 'py-class-re)(eq regexp 'py-def-or-class-re)))
                      (while
                          (and
                           ;; lp:1294478 py-mark-def hangs
                           (if last
                               (if (< last (point))
                                   t
                                 (when (nth 1 pps)
                                   (if py-verbose-p
                                       (throw 'exit (py--message-error (py--record-list-error pps)))
                                     (throw 'exit nil))))

                             t)
                           (setq last (point))(re-search-forward "^$" nil t)(skip-chars-forward " \t\r\n\f")(or (nth 8 (setq pps (syntax-ppss))) (nth 1 pps) (< thisindent (current-column)))))
                      ;; (goto-char last)
                      (skip-chars-backward " \t\r\n\f")
                      (setq done t)
                      (and (nth 8 (setq pps (syntax-ppss)))
                           (py-beginning-of-statement)
                           (py-end-of-statement)))
                     (t (while
                            (and (py-down-statement)
                                 (or (< thisindent (current-indentation))
                                     (and (eq thisindent (current-indentation))
                                          (or (eq regexp 'py-minor-block-re)
                                              (eq regexp 'py-block-re))
                                          (looking-at py-clause-re)))
                                 (py-end-of-statement)(setq last (point))))
                        (and last (goto-char last)))))
              (t (goto-char orig)))
        (when (and (<= (point) orig)(not (looking-at thisregexp)))
          ;; found the end above
          ;; py--travel-current-indent will stop of clause at equal indent
          (when (py--look-downward-for-beginning thisregexp)
            (py--end-base regexp orig)))
        (setq pps (syntax-ppss))
        ;; (catch 'exit)
        (and err py-verbose-p (py--message-error err))
        (if (and (< orig (point)) (not (or (looking-at comment-start) (nth 8 pps) (nth 1 pps))))
            (point)
          (goto-char (point-max))
          nil)))))

(defun py--look-downward-for-beginning (regexp)
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
                     (if (py--statement-opens-block-p)
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
      (insert (py--join-words-wrapping (remove "" sorted-imports) "," "    " 78))
      (insert ")"))))

(defun py--in-literal (&optional lim)
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
Operators however are left aside resp. limit py-expression designed for edit-purposes."
  (interactive "p")
  (or arg (setq arg 1))
  (let (erg)
    (if (< 0 arg)
        (save-restriction
          (widen)
          (setq erg (py--beginning-of-expression-intern)))
      (setq arg (abs arg))
      (setq erg (py-end-of-expression arg)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py--beginning-of-expression-intern (&optional orig)
  (unless (bobp)
    (let ((orig (or orig (point)))
          (pps (syntax-ppss))
          erg)
      (cond
       ( ;; (empty-line-p)
        (eq 9 (char-after))
        (while
            (and  ;; (empty-line-p)
             (eq 9 (char-after))(not (bobp)))
          (forward-line -1)
          (end-of-line))
        (py--beginning-of-expression-intern orig))
       ;; lists
       ((nth 1 pps)
        (goto-char (nth 1 pps))
        (skip-chars-backward py-expression-skip-chars))
       ;; (while (or (looking-back (concat py-string-delim-re py-expression-re py-string-delim-re py-operator-regexp) (line-beginning-position) t)
       ;;             (looking-back (concat "[[:alnum:]_]*" py-operator-regexp "[ \t]*") (line-beginning-position) t))
       ;;    (goto-char (match-beginning 0)))
       ;;)
       ;; listed elements
       ;; strings
       ((and (nth 3 pps)(nth 8 pps)
             (goto-char (nth 8 pps)))
        (cond (;; consider expression a string starting at BOL
               (bolp))
              ((looking-back py-assignment-regexp))
              ((looking-back py-operator-regexp)
               (when (nth 2 pps)
                 (goto-char (nth 2 pps))))
              (t (py--beginning-of-expression-intern orig))))
       ;; comments left
       ((nth 8 pps)
        (goto-char (nth 8 pps))
        (unless (bobp)
          (py--beginning-of-expression-intern orig)))
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
          (py--beginning-of-expression-intern orig)))
       ((and (< (point) orig)(looking-at (concat py-expression-re py-delimiter-regexp))))
       ((looking-back (concat "[^ \t\n\r\f]+" py-delimiter-regexp))
        (goto-char (match-beginning 0))
	(skip-chars-backward py-expression-skip-chars)
        (unless (or (looking-back py-assignment-regexp) (looking-back "^[ \t]*"))
          (py--beginning-of-expression-intern orig)))
       ;; before assignment
       ((looking-back py-assignment-regexp)
        (goto-char (1- (match-beginning 0)))
        (forward-char -1)
        (py--beginning-of-expression-intern orig))
       ((looking-back py-operator-regexp)
        (goto-char (1- (match-beginning 0)))
        (forward-char -1)
        (unless (< 0 (abs (skip-chars-backward py-expression-skip-chars)))
          (py--beginning-of-expression-intern orig)))
       ((looking-back "\"\\|'")
        (forward-char -1)
        (skip-chars-backward "\"'")
        (unless (looking-back py-assignment-regexp)
          (py--beginning-of-expression-intern orig)))
       ((looking-back "(\\|\\[")
        (forward-char -1)
        (unless (looking-back py-assignment-regexp)
          (py--beginning-of-expression-intern orig)))
       ((looking-back "[\])}]")
        (forward-char -1)
        (unless (looking-back py-assignment-regexp)
          (py--beginning-of-expression-intern orig)))
       ;; inside expression
       ((looking-back py-expression-re)
        (skip-chars-backward py-expression-skip-chars)
        (unless (or (looking-back "^[ \t]*") (looking-back py-assignment-regexp))
          (py--beginning-of-expression-intern orig)))
       ((looking-back (concat "[ \t]*" "[[:alnum:]_]*" py-operator-regexp "[[:alnum:]_]*") (line-beginning-position) t)
        (goto-char (match-beginning 0))
        (unless (looking-back "^[ \t]*")
          (py--beginning-of-expression-intern orig)))
       ((and (eq (point) orig) (looking-back "[ \t\r\n\f]"))
        (skip-chars-backward " \t\r\n\f")
        (unless (bobp)
          (forward-char -1)
          (py--beginning-of-expression-intern orig)))
       ((and (eq (point) orig) (not (bobp)) (looking-back py-expression-re))
        (forward-char -1)
        (when (< 0 (abs (skip-chars-backward py-expression-skip-chars)))
          (py--beginning-of-expression-intern orig)))
       ((and (looking-at py-expression-re) (not (looking-back "[ \t\r\n\f]")))
        (unless (< 0 (abs (skip-chars-backward py-expression-skip-chars)))
          (py--beginning-of-expression-intern orig)))
       ((and (eq (point) orig)(looking-back "[ \t]*="))
        (goto-char (match-beginning 0))
        (skip-chars-backward " \t\r\n\f")
        (py--beginning-of-expression-intern orig)))
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
            (setq erg (py--end-of-expression-intern))
            (setq arg (1- arg))))
      (setq arg (abs arg))
      (setq erg (py-beginning-of-expression arg)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py--end-of-expression-intern (&optional orig)
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
          (py--end-of-expression-intern orig)))
       ;; in comment
       ((nth 4 pps)
        (or (< (point) (progn (forward-comment 1)(point)))(forward-line 1))
        (py--end-of-expression-intern orig))
       ( ;; (empty-line-p)
	(eq 9 (char-after))
        (while
            (and  ;; (empty-line-p)
	     (eq 9 (char-after))(not (eobp)))
          (forward-line 1))
        (py--end-of-expression-intern orig))
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
        (unless (looking-at "[ \t]*$")
          (py--end-of-expression-intern orig)))
       ((looking-at "[(\[]")
        (forward-list)
        (unless (looking-at "[ \t]*$")
          (py--end-of-expression-intern orig)))
       ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
        (while (and (looking-at "[ \t]*#") (not (eobp)))
          (forward-line 1))
        (py--end-of-expression-intern orig))
       ((and (eq orig (point)) (looking-at py-assignment-regexp))
        (goto-char (match-end 0))
        (if (looking-at "[(\[]")
            (forward-list 1)
          (py--end-of-expression-intern orig)))
       ((looking-at (concat "[^ \t\n\r\f]*" py-delimiter-regexp))
        (goto-char (match-end 0))
        (while (looking-at (concat "[^ \t\n\r\f]*" py-delimiter-regexp))
          (goto-char (match-end 0)))
        (forward-char -1)
        (unless (looking-at (concat py-assignment-regexp "\\|[ \t]*$\\|" py-delimiter-regexp))
          (py--end-of-expression-intern orig)))
       ((looking-at (concat "\\([[:alnum:] ]+ \\)" py-assignment-regexp))
	(goto-char (match-end 1))
	(skip-chars-backward " \t\r\n\f"))
       ((and (eq orig (point)) (looking-at (concat "[ \t]*" "[^(\t\n\r\f]+" py-operator-regexp)))
	(skip-chars-forward " \t\r\n\f")
	(when (< 0 (skip-chars-forward py-expression-skip-chars))
	  (py--end-of-expression-intern orig)))
       ((and (eq orig (point)) (looking-at py-not-expression-regexp))
        (skip-chars-forward py-not-expression-chars)
        (unless (or (looking-at "[ \t]*$")(looking-at py-assignment-regexp))
          (py--end-of-expression-intern orig)))
       ((looking-at py-expression-skip-regexp)
        (skip-chars-forward py-expression-skip-chars)
        (unless (or (looking-at "[ \n\t\r\f]*$")(looking-at py-assignment-regexp))
          (py--end-of-expression-intern orig)))
       ((and (eq (point) orig)
	     (skip-chars-forward " \t\r\n\f")
	     (< 0 (skip-chars-forward py-expression-skip-chars)))
	(py--end-of-expression-intern orig)))

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

(defun py-beginning-of-statement (&optional orig done limit)
  "Go to the initial line of a simple statement.

For beginning of compound statement use py-beginning-of-block.
For beginning of clause py-beginning-of-clause.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (save-restriction
    (unless (bobp)
      (let* ((orig (or orig (point)))
             (this (point))
             (cui (current-indentation))

             (pps (progn (goto-char this)
                         (parse-partial-sexp (or limit (point-min))(point))))
             (done done)
             erg)
	;; lp:1382788
	(unless done (skip-chars-backward " \t\r\n\f"))
        (cond
         ((and (bolp)(eolp))
          (skip-chars-backward " \t\r\n\f")
          (py-beginning-of-statement orig done limit))
         ((nth 8 pps)
          (and (nth 3 pps) (setq done t))
          (goto-char (nth 8 pps))
          (py-beginning-of-statement orig done limit))
         ((nth 1 pps)
          (goto-char (1- (nth 1 pps)))
	  (py--skip-to-semicolon-backward (save-excursion (back-to-indentation)(point)))
          (setq done t)
          (py-beginning-of-statement orig done limit))
         ((py-preceding-line-backslashed-p)
          (forward-line -1)
          (back-to-indentation)
          (setq done t)
          (py-beginning-of-statement orig done limit))
         ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
          (forward-comment -1)
          (while (and (not (bobp)) (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
            (forward-comment -1))
          (unless (bobp)
            (py-beginning-of-statement orig done limit)))
         ((looking-at "[ \t]*#")
	  (when (py--skip-to-semicolon-backward (save-excursion (back-to-indentation)(point)))
	    ;; (skip-chars-backward (concat "^" comment-start) (line-beginning-position))
	    ;; (back-to-indentation)
	    (skip-chars-forward " \t")
	    (unless (bobp)
	      (py-beginning-of-statement orig done limit))))
         ((and (not done) (looking-at py-string-delim-re))
          (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
            (setq done t))
          (back-to-indentation)
          (py-beginning-of-statement orig done limit))
	 ((and (not done) (eq (char-before) ?\;))
	  (skip-chars-backward ";")
	  (py-beginning-of-statement orig done limit))
	 ((and (not done) (py--skip-to-semicolon-backward (save-excursion (back-to-indentation)(point))))
	  ;; (back-to-indentation)
	  (py-beginning-of-statement orig done limit))
	 ((and (not done) (not (eq 0 (skip-chars-backward " \t\r\n\f"))))
          ;; (setq done t)
          (py-beginning-of-statement orig done limit))
	 ((and (not done)(not (eq (current-column) (current-indentation))))
	  (if (ignore-errors
		(< 0
		   (abs
		    (py--skip-to-semicolon-backward (save-excursion (back-to-indentation)(point))))
		   ;; (skip-chars-backward "^\t\r\n\f" (line-beginning-position))
))
	      (progn
		(setq done t)
		;; (back-to-indentation)
		(py-beginning-of-statement orig done limit))
	    (back-to-indentation)
	    (setq done t)
	    (py-beginning-of-statement orig done limit))))
	(unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
	  (when (< (point) orig)(setq erg (point))))
	(when (and py-verbose-p (interactive-p)) (message "%s" erg))
	erg))))

(defalias 'py-backward-declarations 'py-beginning-of-declarations)
(defun py-beginning-of-declarations ()
  "Got to the beginning of assigments resp. statements in current level which don't open blocks. "
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

;;; Beginning of forms
(defun py--beginning-of-form-intern (regexp &optional iact indent orig lc)
  "Go to beginning of FORM.

With INDENT, go to beginning one level above.
Whit IACT, print result in message buffer.

Returns beginning of FORM if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (let (erg)
    (unless (bobp)
      (let* ((orig (or orig (point)))
             (indent (or indent (progn
                                  (back-to-indentation)
                                  (or (py--beginning-of-statement-p)
                                      (py-beginning-of-statement))
                                  (current-indentation)))))
        (setq erg (cond ((and (< (point) orig) (looking-at (symbol-value regexp)))
                         (point))
                        ((and (eq 0 (current-column)) (numberp indent) (< 0 indent))
                         (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
                           (py-beginning-of-statement)
                           (unless (looking-at (symbol-value regexp))
                             (cdr (py--go-to-keyword (symbol-value regexp) (current-indentation))))))
                        ;; indent from first beginning of clause matters
                        ;; ((not (looking-at py-extended-block-or-clause-re))
                        ;;  (py--go-to-keyword py-extended-block-or-clause-re indent)
                        ;;  (if (looking-at (symbol-value regexp))
                        ;;      (setq erg (point))
                        ;;    (py--beginning-of-form-intern regexp iact (current-indentation) orig)))
                        ((numberp indent)
                         (ignore-errors
                           (cdr (py--go-to-keyword (symbol-value regexp) indent))))
                        (t (ignore-errors
                             (cdr (py--go-to-keyword (symbol-value regexp)
                                                    (- (progn (if (py--beginning-of-statement-p) (current-indentation) (save-excursion (py-beginning-of-statement) (current-indentation)))) py-indent-offset)))))))
        (when lc (beginning-of-line) (setq erg (point)))))
    (when (and py-verbose-p iact) (message "%s" erg))
    erg))

(defun py--narrow-in-comint-modes (&optional done limit)
  "In comint-modes, limit region to previous prompt. "
  (let ((pos (point))
        (limit
         (or limit
             (and
              (or (eq major-mode 'comint-mode)(eq major-mode 'py-shell-mode))
              (if (re-search-backward comint-prompt-regexp nil t 1)
                  (match-end 0)
                (error (format "py-beginning-of-statement: No prompt found in %s mode" major-mode)))))))
    (and limit (not done) (goto-char (match-end 0)) (skip-chars-forward " \t") (narrow-to-region (point) pos))))

(defun py--fetch-first-python-buffer ()
  "Returns first (I)Python-buffer found in `buffer-list'"
  (let ((buli (buffer-list))
        erg)
    (while (and buli (not erg))
      (if (string-match "Python" (prin1-to-string (car buli)))
          (setq erg (car buli))
        (setq buli (cdr buli))))
    erg))

(defun py--beginning-of-prepare (indent final-re &optional inter-re iact lc)
  (let ((orig (point))
        (indent
         (or indent
             (progn (back-to-indentation)
                    (or (py--beginning-of-statement-p)
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
      (py--beginning-of-form-intern final-re iact indent orig lc))))

(defun py-beginning-of-block (&optional indent)
  "Go to beginning block, skip whitespace at BOL.

Returns beginning of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-block-re 'py-clause-re (interactive-p)))

(defun py-beginning-of-clause (&optional indent)
  "Go to beginning clause, skip whitespace at BOL.

Returns beginning of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-block-or-clause (&optional indent)
  "Go to beginning block-or-clause, skip whitespace at BOL.

Returns beginning of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-def (&optional indent)
  "Go to beginning def, skip whitespace at BOL.

Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-def-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-class (&optional indent)
  "Go to beginning class, skip whitespace at BOL.

Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-class-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-def-or-class (&optional indent)
  "Go to beginning def-or-class, skip whitespace at BOL.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. "
  (interactive)
  (py--beginning-of-prepare indent 'py-def-or-class-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-if-block (&optional indent)
  "Go to beginning if-block, skip whitespace at BOL.

Returns beginning of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-if-block-re 'py-clause-re (interactive-p)))

(defun py-beginning-of-try-block (&optional indent)
  "Go to beginning try-block, skip whitespace at BOL.

Returns beginning of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-try-block-re 'py-clause-re (interactive-p)))

(defun py-beginning-of-minor-block (&optional indent)
  "Go to beginning minor-block, skip whitespace at BOL.

Returns beginning of minor-block if successful, nil otherwise
A minor block is started by a `for', `if', `try' or `with'."
  (interactive)
  (py--beginning-of-prepare indent 'py-minor-block-re 'py-clause-re (interactive-p)))

(defalias 'py-beginning-of-block-lc 'py-beginning-of-block-bol)
(defun py-beginning-of-block-bol (&optional indent)
  "Go to beginning block, go to beginning-of-line.

Returns beginning of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-beginning-of-clause-lc 'py-beginning-of-clause-bol)
(defun py-beginning-of-clause-bol (&optional indent)
  "Go to beginning clause, go to beginning-of-line.

Returns beginning of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-block-or-clause-lc 'py-beginning-of-block-or-clause-bol)
(defun py-beginning-of-block-or-clause-bol (&optional indent)
  "Go to beginning block-or-clause, go to beginning-of-line.

Returns beginning of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-def-lc 'py-beginning-of-def-bol)
(defun py-beginning-of-def-bol (&optional indent)
  "Go to beginning def, go to beginning-of-line.

Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-def-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-class-lc 'py-beginning-of-class-bol)
(defun py-beginning-of-class-bol (&optional indent)
  "Go to beginning class, go to beginning-of-line.

Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-class-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-def-or-class-lc 'py-beginning-of-def-or-class-bol)
(defun py-beginning-of-def-or-class-bol (&optional indent)
  "Go to beginning def-or-class, go to beginning-of-line.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-def-or-class-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-if-block-lc 'py-beginning-of-if-block-bol)
(defun py-beginning-of-if-block-bol (&optional indent)
  "Go to beginning if-block, go to beginning-of-line.

Returns beginning of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-if-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-beginning-of-try-block-lc 'py-beginning-of-try-block-bol)
(defun py-beginning-of-try-block-bol (&optional indent)
  "Go to beginning try-block, go to beginning-of-line.

Returns beginning of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-try-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-beginning-of-minor-block-lc 'py-beginning-of-minor-block-bol)
(defun py-beginning-of-minor-block-bol (&optional indent)
  "Go to beginning minor-block, go to beginning-of-line.

Returns beginning of minor-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-prepare indent 'py-minor-block-re 'py-clause-re (interactive-p) t))

;;;
(defun py-beginning-of-top-level ()
  "Go up to beginning of statments until level of indentation is null.

Returns position if successful, nil otherwise "
  (interactive)
  (let (erg)
    (unless (bobp)
      (while (and (not (bobp)) (setq erg (py-beginning-of-statement))
                  (< 0 (current-indentation))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-top-level ()
  "Go to end of top-level form at point.

Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((orig (point))
        erg)
    (unless (eobp)
      (unless (py--beginning-of-statement-p)
        (py-beginning-of-statement))
      (unless (eq 0 (current-column))
        (py-beginning-of-top-level))
      (cond ((looking-at py-def-re)
             (setq erg (py-end-of-def)))
            ((looking-at py-class-re)
             (setq erg (py-end-of-class)))
            ((looking-at py-block-re)
             (setq erg (py-end-of-block)))
             (t (setq erg (py-end-of-statement))))
      (unless (< orig (point))
        (while (and (not (eobp)) (py-down-statement)(< 0 (current-indentation))))
        (if (looking-at py-block-re)
            (setq erg (py-end-of-block))
          (setq erg (py-end-of-statement))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning ()
  "Go to beginning of compound statement or definition at point.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py--beginning-of-form-intern 'py-extended-block-or-clause-re (interactive-p)))

(defun py-end (&optional indent)
  "Go to end of of compound statement or definition at point.

Returns position block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-extended-block-or-clause-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-up (&optional indent)
  "Go up or to beginning of form if inside.

If inside a delimited form --string or list-- go to its beginning.
If not at beginning of a statement or block, go to its beginning.
If at beginning of a statement or block, go to beginning one level above of compound statement or definition at point.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let ((pps (syntax-ppss)))
    (cond ((nth 8 pps) (goto-char (nth 8 pps)))
          ((nth 1 pps) (goto-char (nth 1 pps)))
          ((py--beginning-of-statement-p) (py--beginning-of-form-intern 'py-extended-block-or-clause-re (interactive-p) t))
          (t (py-beginning-of-statement)))))

(defun py-down (&optional indent)

  "Go to beginning one level below of compound statement or definition at point.

If no statement or block below, but a delimited form --string or list-- go to its beginning. Repeated call from there will behave like down-list.

Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         erg
         (indent (if
                     (py--beginning-of-statement-p)
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

(defun py-backward-same-level ()
  "Go form backward keeping indent level if possible.

If inside a delimited form --string or list-- go to its beginning.
If not at beginning of a statement or block, go to its beginning.
If at beginning of a statement or block, go to previous beginning of compound statement or definition at point.
If no further element at same level, go one level up.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((pps (syntax-ppss)))
    (cond ((nth 8 pps) (goto-char (nth 8 pps)))
          ((nth 1 pps) (goto-char (nth 1 pps)))
          ((py--beginning-of-statement-p) (py--beginning-of-form-intern 'py-extended-block-or-clause-re (interactive-p)))
          (t (py-beginning-of-statement)))))

(defun py-end-of-block (&optional indent)
  "Go to end of block.

Returns end of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-clause (&optional indent)
  "Go to end of clause.

Returns end of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-clause-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-block-or-clause (&optional indent)
  "Go to end of block-or-clause.

Returns end of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-block-or-clause-re orig)))
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
         (erg (py--end-base 'py-def-re orig)))
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
         (erg (py--end-base 'py-class-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-def-or-class (&optional indent)
  "Go to end of def-or-class.

Returns end of def-or-class if successful, nil otherwise

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too. "
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-def-or-class-re orig)))
    (when (and py-verbose-p (interactive-p))
      (message "%s" erg))
    erg))

(defun py-end-of-if-block (&optional indent)
  "Go to end of if-block.

Returns end of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-if-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-try-block (&optional indent)
  "Go to end of try-block.

Returns end of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-try-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-minor-block (&optional indent)
  "Go to end of minor-block.

Returns end of minor-block if successful, nil otherwise

A minor block is started by a `for', `if', `try' or `with'."
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-minor-block-re orig)))
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

(defun py--go-to-keyword (regexp &optional maxindent)
  "Returns a list, whose car is indentation, cdr position. "
  (let ((orig (point))
        (maxindent (or maxindent (and (< 0 (current-indentation))(current-indentation))
                       ;; make maxindent large enough if not set
                       (* 99 py-indent-offset)))
        (first t)
        done erg cui)
    (while (and (not done) (not (bobp)))
      (while (and (re-search-backward regexp nil 'move 1)(nth 8 (syntax-ppss))))
      ;; (or (< (point) orig) (py-beginning-of-statement))
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

(defalias 'py-statement-forward 'py-end-of-statement)
(defalias 'py-next-statement 'py-end-of-statement)
(defalias 'py-forward-statement 'py-end-of-statement)
(defun py--skip-to-comment-or-semicolon ()
  "Returns position if comment or semicolon found. "
  (let ((orig (point)))
    (cond ((and done (< 0 (abs (skip-chars-forward "^#;" (line-end-position))))
		(member (char-after) (list ?# ?\;)))
	   (when (eq ?\; (char-after))
	     (skip-chars-forward ";" (line-end-position))))
	  ((and (< 0 (abs (skip-chars-forward "^#;" (line-end-position))))
		(member (char-after) (list ?# ?\;)))
	   (when (eq ?\; (char-after))
	     (skip-chars-forward ";" (line-end-position))))
	  ((not done)
	   (end-of-line)))
    (skip-chars-backward " \t" (line-beginning-position))
    (and (< orig (point))(setq done t)
	 done)))

(defun py--eos-in-string (pps)
  "Return stm, i.e. if string is part of a (print)-statement. "
  (let ((orig (point))
        pos stm)
    (goto-char (nth 8 pps))
    (unless (looking-back "^[ \t]*")
      (setq stm t))
    ;; go to end of string
    (and (member (char-after) (list ?' ?\"))
         (ignore-errors (setq pos (scan-sexps (point) 1)))
         (goto-char pos))
    ;; if no closing string delimiter, pos doesn't exist
    (unless (or stm (not pos))
      (setq done t)
      (unless (eq 10 (char-after))
        (and (< 0 (abs (skip-chars-forward "^;#" (line-end-position))))
             (eq ?\; (char-after))
             (skip-chars-forward ";"))))
    stm))

(defun py--end-of-comment-intern (pos)
  (while (and (not (eobp))
              (forward-comment 99999)))
  ;; forward-comment fails sometimes
  (and (eq pos (point)) (prog1 (forward-line 1) (back-to-indentation))
       (while (member (char-after) (list ?# 10))(forward-line 1)(back-to-indentation))))

(defun py-end-of-statement (&optional orig done repeat)
  "Go to the last char of current statement.

To go just beyond the final line of the current statement, use `py-down-statement-bol'.

Optional argument REPEAT, the number of loops done already, is checked for py-max-specpdl-size error. Avoid eternal loops due to missing string delimters etc. "
  (interactive)
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
          (orig (or orig (point)))
          erg pos last
          ;; use by scan-lists
          parse-sexp-ignore-comments
          forward-sexp-function
          stringchar stm pps err)
      (unless done (py--skip-to-comment-or-semicolon))
      (setq pps (parse-partial-sexp (point-min) (point)))
      ;; (origline (or origline (py-count-lines)))
      (cond
       ;; wich-function-mode, lp:1235375
       ((< py-max-specpdl-size repeat)
        (error "py-end-of-statement reached loops max. If no error, customize `py-max-specpdl-size'"))
       ;; list
       ((nth 1 pps)
        (if (<= orig (point))
	    (progn
	      (setq orig (point))
	      ;; do not go back at a possible unclosed list
	      (goto-char (nth 1 pps))
	      (let ((parse-sexp-ignore-comments t))
		(if
		    (ignore-errors (forward-list))
		    (progn
		      (when (looking-at ":[ \t]*$")
			(forward-char 1))
		      (setq done t)
		      (skip-chars-forward "^#" (line-end-position))
		      (skip-chars-backward " \t\r\n\f" (line-beginning-position))
		      (py-end-of-statement orig done repeat))
		  (setq err (py--record-list-error pps))
		  (goto-char orig))))))
       ;; string
       ((nth 3 pps)
	(when (py-end-of-string)
	  (end-of-line)
	  (skip-chars-backward " \t\r\n\f")
	  (setq pps (parse-partial-sexp (point-min) (point)))
	  (unless (and done (not (or (nth 1 pps) (nth 8 pps))) (eolp)) (py-end-of-statement orig done repeat))))
       ;; in non-terminated string

       ;; in comment
       ((nth 4 pps)
	(py--end-of-comment-intern (point))
	(py--skip-to-comment-or-semicolon)
	(while (and (eq (char-before (point)) ?\\ )
		    (py-escaped)(setq last (point)))
	  (forward-line 1)(end-of-line))
	(and last (goto-char last)
	     (forward-line 1)
	     (back-to-indentation))
	(py-end-of-statement orig done repeat))
       ((py-current-line-backslashed-p)
	(end-of-line)
	(skip-chars-backward " \t\r\n\f" (line-beginning-position))
	(while (and (eq (char-before (point)) ?\\ )
		    (py-escaped))
	  (forward-line 1)
	  (end-of-line)
	  (skip-chars-backward " \t\r\n\f" (line-beginning-position)))
	(unless (eobp)
	  (py-end-of-statement orig done repeat)))
       ((eq orig (point))
	(skip-chars-forward " \t\r\n\f#'\"")
	(py--skip-to-comment-or-semicolon)
	(py-end-of-statement orig done repeat))
       ((eq (current-indentation) (current-column))
	(py--skip-to-comment-or-semicolon)
	;; (setq pps (parse-partial-sexp (point-min) (point)))
	(unless done
	  (py-end-of-statement orig done repeat)))

       ((and (looking-at "[[:print:]]+$") (not done) (py--skip-to-comment-or-semicolon))
	(py-end-of-statement orig done repeat)))
      (unless
	  (or
	   (eq (point) orig)
	   (member (char-before) (list 10 32 9)))
	(setq erg (point)))
      (if (and py-verbose-p err)
	  (py--message-error err)
        (and py-verbose-p (interactive-p) (message "%s" erg)))
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
  (while (and (not (looking-at "@\\w+"))(not (and (bolp)(eolp)))(not (bobp))(forward-line -1))
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

(defun py--base (form &optional py-mark-decorators)
  "Returns boundaries of FORM, a cons. "
  (let* ((begform (intern-soft (concat "py-beginning-of-" form)))
         (endform (intern-soft (concat "py-end-of-" form)))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (and py-mark-decorators
         (and (setq erg (py-beginning-of-decorator))
              (setq beg erg)))
    (setq end (funcall endform))
    (unless end (when (< beg (point))
                  (setq end (point))))
    (if (and beg end (<= beg orig) (<= orig end))
	(progn
	  (when (interactive-p) (message "%s %s" beg end))
	  (cons beg end))
      (when (interactive-p) (message "%s" "nil"))
      (goto-char orig)
      nil)))

;;; Forms
(defun py-statement ()
  "Statement at point.

Return code of `py-statement' at point, a string. "
  (interactive)
  (let ((erg (py--base "statement")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-top-level ()
  "Top-Level at point.

Return code of `py-top-level' at point, a string. "
  (interactive)
  (let ((erg (py--base "top-level")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-block ()
  "Block at point.

Return code of `py-block' at point, a string. "
  (interactive)
  (let ((erg (py--base "block")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-clause ()
  "Clause at point.

Return code of `py-clause' at point, a string. "
  (interactive)
  (let ((erg (py--base "clause")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-block-or-clause ()
  "Block-Or-Clause at point.

Return code of `py-block-or-clause' at point, a string. "
  (interactive)
  (let ((erg (py--base "block-or-clause")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-def ()
  "Def at point.

Return code of `py-def' at point, a string. "
  (interactive)
  (let ((erg (py--base "def")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-class ()
  "Class at point.

Return code of `py-class' at point, a string. "
  (interactive)
  (let ((erg (py--base "class")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-def-or-class ()
  "Def-Or-Class at point.

Return code of `py-def-or-class' at point, a string. "
  (interactive)
  (let ((erg (py--base "def-or-class")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-expression ()
  "Expression at point.

Return code of `py-expression' at point, a string. "
  (interactive)
  (let ((erg (py--base "expression")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-partial-expression ()
  "Partial-Expression at point.

Return code of `py-partial-expression' at point, a string. "
  (interactive)
  (let ((erg (py--base "partial-expression")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-minor-block ()
  "Minor-Block at point.

Return code of `py-minor-block' at point, a string. "
  (interactive)
  (let ((erg (py--base "minor-block")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

;;; Mark
(defun py-mark-base (form &optional py-mark-decorators)
  "Calls py--base, returns bounds of form, a cons. "
  (let* ((bounds (py--base form py-mark-decorators))
         (beg (car bounds)))
    (push-mark beg t t)
    bounds))

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

(defun py-mark-minor-block ()
  "Mark minor-block at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "minor-block"))
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

(defun py-mark-top-level ()
  "Mark top-level form at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "top-level"))
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

;;; Copy

(defun py-copy-statement ()
  "Copy statement at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "statement")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-top-level ()
  "Copy top-level at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "top-level")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-block ()
  "Copy block at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "block")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-clause ()
  "Copy clause at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "clause")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-block-or-clause ()
  "Copy block-or-clause at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "block-or-clause")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-def ()
  "Copy def at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "def")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-class ()
  "Copy class at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "class")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-def-or-class ()
  "Copy def-or-class at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "def-or-class")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-expression ()
  "Copy expression at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "expression")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-partial-expression ()
  "Copy partial-expression at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "partial-expression")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-minor-block ()
  "Copy minor-block at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "minor-block")))
    (copy-region-as-kill (car erg) (cdr erg))))

;;; Hide-Show
(defun py-hide-base (form &optional beg end)
  "Hide visibility of existing form at point. "
  (hs-minor-mode 1)
  (save-excursion
    (let* ((form (prin1-to-string form))
	   (beg (or beg (or (funcall (intern-soft (concat "py--beginning-of-" form "-p")))
			    (funcall (intern-soft (concat "py-beginning-of-" form))))))
	   (end (or end (funcall (intern-soft (concat "py-end-of-" form)))))
	   (modified (buffer-modified-p))
	   (inhibit-read-only t))
      (if (and beg end)
	  (progn
	    (hs-make-overlay beg end 'code)
	    (set-buffer-modified-p modified))
	(error (concat "No " (format "%s" form) " at point!"))))))

(defun py-show-base (form &optional beg end)
  "Remove invisibility of existing form at point. "
  (save-excursion
    (let* ((form (prin1-to-string form))
	   (beg (or beg (or (funcall (intern-soft (concat "py--beginning-of-" form "-p")))
			    (funcall (intern-soft (concat "py-beginning-of-" form))))))
	   (end (or end (funcall (intern-soft (concat "py-end-of-" form)))))
	   (modified (buffer-modified-p))
	   (inhibit-read-only t))
      (if (and beg end)
	  (progn
	    (hs-discard-overlays beg end)
	    (set-buffer-modified-p modified))
	(error (concat "No " (format "%s" form) " at point!"))))))

(defun py-hide-show (&optional form beg end)
  "Toggle visibility of existing forms at point. "
  (interactive)
  (save-excursion
    (let* ((form (prin1-to-string form))
	   (beg (or beg (or (funcall (intern-soft (concat "py--beginning-of-" form "-p")))
			    (funcall (intern-soft (concat "py-beginning-of-" form))))))
	   (end (or end (funcall (intern-soft (concat "py-end-of-" form)))))
	   (modified (buffer-modified-p))
	   (inhibit-read-only t))
      (if (and beg end)
	  (if (overlays-in beg end)
	      (hs-discard-overlays beg end)
	    (hs-make-overlay beg end 'code))
	(error (concat "No " (format "%s" form) " at point!")))
      (set-buffer-modified-p modified))))

(defun py-hide-region (beg end)
  "Hide active region. "
  (interactive
   (list
    (and (use-region-p) (region-beginning))(and (use-region-p) (region-end))))
  (py-hide-base 'region beg end))

(defun py-show-region (beg end)
  "Un-hide active region. "
  (interactive
   (list
    (and (use-region-p) (region-beginning))(and (use-region-p) (region-end))))
  (py-show-base 'region beg end))

(defun py-hide-statement ()
  "Hide statement at point. "
  (interactive)
  (py-hide-base 'statement))

(defun py-show-statement ()
  "Show statement at point. "
  (interactive)
  (py-show-base 'statement))

(defun py-hide-block ()
  "Hide block at point. "
  (interactive)
  (py-hide-base 'block))

(defun py-show-block ()
  "Show block at point. "
  (interactive)
  (py-show-base 'block))

(defun py-hide-clause ()
  "Hide clause at point. "
  (interactive)
  (py-hide-base 'clause))

(defun py-show-clause ()
  "Show clause at point. "
  (interactive)
  (py-show-base 'clause))

(defun py-hide-block-or-clause ()
  "Hide block-or-clause at point. "
  (interactive)
  (py-hide-base 'block-or-clause))

(defun py-show-block-or-clause ()
  "Show block-or-clause at point. "
  (interactive)
  (py-show-base 'block-or-clause))

(defun py-hide-def ()
  "Hide def at point. "
  (interactive)
  (py-hide-base 'def))

(defun py-show-def ()
  "Show def at point. "
  (interactive)
  (py-show-base 'def))

(defun py-hide-class ()
  "Hide class at point. "
  (interactive)
  (py-hide-base 'class))

(defun py-show-class ()
  "Show class at point. "
  (interactive)
  (py-show-base 'class))

(defun py-hide-expression ()
  "Hide expression at point. "
  (interactive)
  (py-hide-base 'expression))

(defun py-show-expression ()
  "Show expression at point. "
  (interactive)
  (py-show-base 'expression))

(defun py-hide-partial-expression ()
  "Hide partial-expression at point. "
  (interactive)
  (py-hide-base 'partial-expression))

(defun py-show-partial-expression ()
  "Show partial-expression at point. "
  (interactive)
  (py-show-base 'partial-expression))

(defun py-hide-line ()
  "Hide line at point. "
  (interactive)
  (py-hide-base 'line))

(defun py-show-line ()
  "Show line at point. "
  (interactive)
  (py-show-base 'line))

(defun py-hide-top-level ()
  "Hide top-level at point. "
  (interactive)
  (py-hide-base 'top-level))

(defun py-show-top-level ()
  "Show top-level at point. "
  (interactive)
  (py-show-base 'top-level))

;;; minor-block BOL forms
(defun py-mark-minor-block-bol ()
  "Mark minor block, take beginning of line positions.

Returns beginning and end positions of region, a cons.

See `py-minor-block-re'"
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol "minor-block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-minor-block-bol ()
  "Delete minor block, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'.

See `py-minor-block-re'"
  (interactive "*")
  (let ((erg (py--mark-base-bol "minor-block")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-minor-block-bol ()
  "Delete minor block, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'.

See `py-minor-block-re'"
  (interactive "*")
  (let ((erg (py--mark-base-bol "minor-block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-minor-block-bol ()
  "Delete minor block, use position from beginning-of-line.

Don't store data in kill ring.

See `py-minor-block-re'"
  (interactive "*")
  (let ((erg (py--mark-base-bol "minor-block")))
    (delete-region (car erg) (cdr erg))))

;;; Delete
(defun py-delete-statement ()
  "Delete STATEMENT at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "statement")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-top-level ()
  "Delete TOP-LEVEL at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "top-level")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-block ()
  "Delete BLOCK at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-block-or-clause ()
  "Delete BLOCK-OR-CLAUSE at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "block-or-clause")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-def ()
  "Delete DEF at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "def")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-class ()
  "Delete CLASS at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "class")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-def-or-class ()
  "Delete DEF-OR-CLASS at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "def-or-class")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-expression ()
  "Delete EXPRESSION at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "expression")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-partial-expression ()
  "Delete PARTIAL-EXPRESSION at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "partial-expression")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-minor-block ()
  "Delete minor-BLOCK at point.

Don't store data in kill ring.
A minor minor block is started by a `for', `if', `try' or `with'."
  (interactive "*")
  (let ((erg (py-mark-base "minor-block")))
    (delete-region (car erg) (cdr erg))))

;;; Kill
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

(defun py-kill-top-level ()
  "Delete top-level form at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "top-level")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block ()
  "Delete block at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-minor-block ()
  "Delete minor-block at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "minor-block")))
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
(defun py--mark-base-bol (form &optional py-mark-decorators)
  (let* ((begform (intern-soft (concat "py-beginning-of-" form "-bol")))
         (endform (intern-soft (concat "py-end-of-" form "-bol")))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-bol-p")))
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
    (setq erg (py--mark-base-bol "block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-block-bol ()
  "Delete block, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-block-bol ()
  "Delete block, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-block-bol ()
  "Delete block, use position from beginning-of-line.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
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
    (setq erg (py--mark-base-bol "clause"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-clause-bol ()
  "Delete clause, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "clause")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-clause-bol ()
  "Delete clause, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-clause-bol ()
  "Delete clause, use position from beginning-of-line.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
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
    (setq erg (py--mark-base-bol "block-or-clause"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-block-or-clause-bol ()
  "Delete block-or-clause, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block-or-clause")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-block-or-clause-bol ()
  "Delete block-or-clause, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-block-or-clause-bol ()
  "Delete block-or-clause, use position from beginning-of-line.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
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
    (py--mark-base-bol "def" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-def-bol ()
  "Delete def, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "def")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-def-bol ()
  "Delete def, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-def-bol ()
  "Delete def, use position from beginning-of-line.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
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
    (py--mark-base-bol "class" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-class-bol ()
  "Delete class, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "class")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-class-bol ()
  "Delete class, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-class-bol ()
  "Delete class, use position from beginning-of-line.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
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
    (py--mark-base-bol "def-or-class" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-def-or-class-bol ()
  "Delete def-or-class, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "def-or-class")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-def-or-class-bol ()
  "Delete def-or-class, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-def-or-class-bol ()
  "Delete def-or-class, use position from beginning-of-line.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
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
    (setq erg (py--mark-base-bol "statement"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-statement-bol ()
  "Delete statement, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "statement")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-statement-bol ()
  "Delete statement, use position from beginning-of-line.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-statement-bol ()
  "Delete statement, use position from beginning-of-line.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

;;; Up/Down
(defun py-up-statement ()
  "Go to the beginning of next statement upwards in buffer.

Return position if statement found, nil otherwise. "
  (interactive)
  (let ((orig (point))
        erg)
    (if (py--beginning-of-statement-p)
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
          (cond ((py--end-of-statement-p)
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
        (while (and (re-search-forward regexp nil 'move 1)
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

(defun py-beginning-of-block-current-column ()
  "Reach next beginning of block upwards which starts at current column.

Return position"
  (interactive)
  (let* ((orig (point))
         (cuco (current-column))
         (str (make-string cuco ?\s))
         pps erg)
    (while (and (not (bobp))(re-search-backward (concat "^" str py-block-keywords) nil t)(or (nth 8 (setq pps (syntax-ppss))) (nth 1 pps))))
    (back-to-indentation)
    (and (< (point) orig)(setq erg (point)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py--travel-current-indent (indent &optional orig)
  "Moves down until clause is closed, i.e. current indentation is reached.

Takes a list, INDENT and START position. "
  (unless (eobp)
    (let ((orig (or orig (point)))
          last)
      (while (and (setq last (point))(not (eobp))(py-end-of-statement)
                  (save-excursion (or (<= indent (progn  (py-beginning-of-statement)(current-indentation)))(eq last (line-beginning-position))))
                  (py--end-of-statement-p)))
      (goto-char last)
      (when (< orig last)
        last))))

;;; Python named shells
(defun python (&optional argprompt)
  "Start an Python interpreter.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "python"))

(defun ipython (&optional argprompt)
  "Start an IPython interpreter.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "ipython"))

(defun python2 (&optional argprompt)
  "Start an Python2 interpreter.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "python2"))

(defun jython (&optional argprompt)
  "Start an Jython interpreter.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "jython"))

(defun python3 (&optional argprompt)
  "Start an Python3 interpreter.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "python3"))

(defun bpython (&optional argprompt)
  "Start an Bpython interpreter.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "bpython"))

;; dedicated
(defun python-dedicated (&optional argprompt switch)
  "Start an unique Python interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python")))

(defun ipython-dedicated (&optional argprompt switch)
  "Start an unique IPython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "ipython")))

(defun python2-dedicated (&optional argprompt switch)
  "Start an unique Python2 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python2")))

(defun jython-dedicated (&optional argprompt switch)
  "Start an unique Jython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "jython")))

(defun python3-dedicated (&optional argprompt switch)
  "Start an unique Python3 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python3")))

(defun bpython-dedicated (&optional argprompt switch)
  "Start an unique Bpython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "bpython")))

;; switch
(defun python-switch (&optional argprompt)
  "Switch to Python interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python")))

(defun ipython-switch (&optional argprompt)
  "Switch to IPython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "ipython")))

(defun python2-switch (&optional argprompt)
  "Switch to Python2 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python2")))

(defun jython-switch (&optional argprompt)
  "Switch to Jython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "jython")))

(defun python3-switch (&optional argprompt)
  "Switch to Python3 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python3")))

(defun bpython-switch (&optional argprompt)
  "Switch to Bpython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "bpython")))

;; no-switch
(defun python-no-switch (&optional argprompt)
  "Open an Python interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python")))

(defun ipython-no-switch (&optional argprompt)
  "Open an IPython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "ipython")))

(defun python2-no-switch (&optional argprompt)
  "Open an Python2 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python2")))

(defun jython-no-switch (&optional argprompt)
  "Open an Jython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "jython")))

(defun python3-no-switch (&optional argprompt)
  "Open an Python3 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python3")))

(defun bpython-no-switch (&optional argprompt)
  "Open an Bpython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "bpython")))

;; dedicated switch
(defalias 'python-dedicated-switch 'python-switch-dedicated)
(defun python-switch-dedicated (&optional argprompt)
  "Switch to an unique Python interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python")))

(defalias 'ipython-dedicated-switch 'ipython-switch-dedicated)
(defun ipython-switch-dedicated (&optional argprompt)
  "Switch to an unique IPython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "ipython")))

(defalias 'python2-dedicated-switch 'python2-switch-dedicated)
(defun python2-switch-dedicated (&optional argprompt)
  "Switch to an unique Python2 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python2")))

(defalias 'jython-dedicated-switch 'jython-switch-dedicated)
(defun jython-switch-dedicated (&optional argprompt)
  "Switch to an unique Jython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "jython")))

(defalias 'python3-dedicated-switch 'python3-switch-dedicated)
(defun python3-switch-dedicated (&optional argprompt)
  "Switch to an unique Python3 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python3")))

(defalias 'bpython-dedicated-switch 'bpython-switch-dedicated)
(defun bpython-switch-dedicated (&optional argprompt)
  "Switch to an unique Bpython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "bpython")))

(defalias 'Python 'python)
(defalias 'pyhotn 'python)
(defalias 'pyhton 'python)
(defalias 'pyt 'python)
(defalias 'Python2 'python2)
(defalias 'Python3 'python3)
(defalias 'IPython 'ipython)
(defalias 'Ipython 'ipython)
(defalias 'iyp 'ipython)
(defalias 'ipy 'ipython)

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

Ignores setting of `py-switch-buffers-on-execute-p', buffer with region stays current."
  (interactive "r")
  (let (py-switch-buffers-on-execute-p)
    (py--execute-base start end)))

(defun py-execute-region-switch (start end)
  "Send the region to a Python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to."
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p t))
    (py--execute-base start end)))

(defun py-execute-region (start end &optional shell dedicated)
  "Send the region to a Python interpreter.

When called with \\[universal-argument], execution through
`default-value' of `py-shell-name' is forced.

When called with \\[universal-argument] followed by a number
different from 4 and 1, user is prompted to specify a shell. This
might be the name of a system-wide shell or include the path to a
virtual environment.

When called from a programm, it accepts a string specifying a
shell which will be forced upon execute as argument.

Optional DEDICATED "
  (interactive "r\nP")
  ;; (when py-debug-p (message "run: %s" "py-execute-region"))
  (save-excursion
    (let ((orig (point))
	  (py-shell-name (cond ((or py-force-py-shell-name-p (eq 4 (prefix-numeric-value shell))) (default-value 'py-shell-name))
			       ((and (numberp shell) (not (eq 1 (prefix-numeric-value shell))))
				(read-from-minibuffer "(path-to-)shell-name: " (default-value 'py-shell-name)))
			       (t (or shell py-shell-name))))
	  (py-dedicated-process-p (or dedicated py-dedicated-process-p)))
      (py--execute-base start end))))

(defun py-execute-region-default (start end)
  "Send the region to the systems default Python interpreter. "
  (interactive "r")
  (save-excursion
    (let ((py-dedicated-process-p (default-value 'py-dedicated-process-p))
	  (py-shell-name (default-value 'py-shell-name)))
      (py--execute-base start end))))

(defun py-execute-region-dedicated (start end &optional shell)
  "Get the region processed by an unique Python interpreter.

When called with \\[universal-argument], execution through
`default-value' of `py-shell-name' is forced.

When called with \\[universal-argument] followed by a number
different from 4 and 1, user is prompted to specify a shell. This
might be the name of a system-wide shell or include the path to a
virtual environment.

When called from a programm, it accepts a string specifying a
shell which will be forced upon execute as argument. "

  (interactive "r\nP")
  (save-excursion
    (let ((py-shell-name (cond ((eq 4 (prefix-numeric-value shell)) (default-value 'py-shell-name))
			       ((and (numberp shell) (not (eq 1 (prefix-numeric-value shell))))
				(read-from-minibuffer "(path-to-)shell-name: " (default-value 'py-shell-name)))
			       (t shell)))
	  (py-dedicated-process-p t))
      (py--execute-base start end))))

(defalias 'py-execute-region-dedicated-default 'py-execute-region-default-dedicated)
(defun py-execute-region-default-dedicated (start end)
  "Send the region to an unique shell of systems default Python. "
  (interactive "r")
  (save-excursion
    (let ((py-dedicated-process-p t))
      (py--execute-base start end (default-value 'py-shell-name)))))

(defun py-delete-temporary (&optional file localname filebuf)
  (when (file-readable-p file)
    (delete-file file))
  (when (buffer-live-p filebuf)
    (set-buffer filebuf)
    (set-buffer-modified-p 'nil)
    (kill-buffer filebuf))
  (when (buffer-live-p localname)
    (kill-buffer localname)))

(defun py--execute-file-base (&optional proc filename cmd procbuf origfile execute-directory py-exception-buffer)
  "Send to Python interpreter process PROC, in Python version 2.. \"execfile('FILENAME')\".

Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
`kill-output-from-shell' does The Right Thing.
Returns position where output starts. "
  (when py-debug-p (message "py--execute-file-base: py-split-window-on-execute: %s" py-split-window-on-execute))
  (let* ((cmd (or cmd (format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" filename filename)))
         (msg (and py-verbose-p (format "## executing %s...\n" (or origfile filename))))
         (buffer (or procbuf (py-shell nil nil nil procbuf)))
         (proc (or proc (get-buffer-process buffer)))
         erg orig)
    (with-current-buffer buffer
      ;; (when py-debug-p (switch-to-buffer (current-buffer)))
      (goto-char (point-max))
      (setq orig (point))
      (comint-send-string proc cmd)
      (setq erg (py--postprocess-comint buffer origline windows-config py-exception-buffer orig))
      (if py-error
	  (progn
	    (setq py-error (prin1-to-string py-error))
	    ;; keep the temporary file in case of error
	    (when py-debug-p
	      (message "py--execute-file-base, py-error:%s" py-error)))
	erg))))

(defun py--execute-buffer-finally (strg execute-directory wholebuf which-shell proc procbuf)
  (let* ((temp (make-temp-name
		;; FixMe: that should be simpler
                (concat (replace-regexp-in-string py-separator-char "-" (replace-regexp-in-string (concat "^" py-separator-char) "" (replace-regexp-in-string ":" "-" (if (stringp which-shell) which-shell (prin1-to-string which-shell))))) "-")))
         (tempfile (concat (expand-file-name py-temp-directory) py-separator-char (replace-regexp-in-string py-separator-char "-" temp) ".py"))
         (tempbuf (get-buffer-create temp)))
    (with-current-buffer tempbuf
      (when py-debug-p (message "py--execute-buffer-finally: py-split-window-on-execute: %s" py-split-window-on-execute))
      ;; (and py-verbose-p (message "%s" "py--execute-buffer-finally"))
      (insert strg)
      (write-file tempfile))
    (unwind-protect
	(setq erg (py--execute-file-base proc tempfile nil procbuf py-orig-buffer-or-file execute-directory py-exception-buffer)))
    (sit-for 0.1 t)
    (py--close-execution tempbuf)
    erg))
  ;; )

(defun py-execute-python-mode-v5 (start end &optional py-exception-buffer)
  (interactive "r")
  (let ((py-exception-buffer (or py-exception-buffer (current-buffer)))
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
      (setq py-error (py--postprocess-intern py-output-buffer origline py-exception-buffer))
      (let* ((line (cadr py-error)))
        (if py-error
            (when (and py-jump-on-exception line)
              (pop-to-buffer py-exception-buffer))
          (pop-to-buffer py-output-buffer)
          (goto-char (point-max))
          (copy-marker (point)))))))

(defun py--execute-ge24.3 (start end filename execute-directory which-shell &optional py-exception-buffer proc)
  "An alternative way to do it.

May we get rid of the temporary file? "
  (and (buffer-file-name) buffer-offer-save (buffer-modified-p) (y-or-n-p "Save buffer before executing? ")
       (write-file (buffer-file-name)))
  (let* ((start (copy-marker start))
         (end (copy-marker end))
         (py-exception-buffer (or py-exception-buffer (current-buffer)))
         (line (count-lines (point-min) (if (eq start (line-beginning-position)) (1+ start) start)))
         (strg (buffer-substring-no-properties start end))
         (tempfile (or (buffer-file-name) (concat (expand-file-name py-temp-directory) py-separator-char (replace-regexp-in-string py-separator-char "-" "temp") ".py")))

         (proc (or proc (if py-dedicated-process-p
                            (get-buffer-process (py-shell nil py-dedicated-process-p which-shell py-buffer-name))
                          (or (get-buffer-process py-buffer-name)
                              (get-buffer-process (py-shell nil py-dedicated-process-p which-shell py-buffer-name))))))
         (procbuf (process-buffer proc))
         (file (or file (with-current-buffer py-buffer-name
                          (concat (file-remote-p default-directory) tempfile))))
         (filebuf (get-buffer-create file)))
    (set-buffer filebuf)
    (erase-buffer)
    (newline line)
    (save-excursion
      (insert strg))
    (py--fix-start (buffer-substring-no-properties (point) (point-max)))
    (unless (string-match "[jJ]ython" which-shell)
      ;; (when (and execute-directory py-use-current-dir-when-execute-p
      ;; (not (string= execute-directory default-directory)))
      ;; (message "Warning: options `execute-directory' and `py-use-current-dir-when-execute-p' may conflict"))
      (and execute-directory
           (process-send-string proc (concat "import os; os.chdir(\"" execute-directory "\")\n"))
	   ))
    (set-buffer filebuf)
    (process-send-string proc
                         (buffer-substring-no-properties
                          (point-min) (point-max)))
    (sit-for 0.1 t)
    (if (and (setq py-error (save-excursion (py--postprocess-intern procbuf origline py-exception-buffer)))
             (car py-error)
             (not (markerp py-error)))
        (py--jump-to-exception py-error origline)
      (unless (string= (buffer-name (current-buffer)) (buffer-name procbuf))
        (when py-verbose-p (message "Output buffer: %s" procbuf))))))

(defun py--execute-base (&optional start end shell filename proc file wholebuf)
  "Update variables. "
  ;; (when py-debug-p (message "run: %s" "py--execute-base"))
  (setq py-error nil)
  (when py-debug-p (message "py--execute-base: py-split-window-on-execute: %s" py-split-window-on-execute))

  (let* ((py-exception-buffer (current-buffer))
	 (py-exception-window (selected-window))
	 (start (or start (and (use-region-p) (region-beginning)) (point-min)))
	 (end (or end (and (use-region-p) (region-end)) (point-max)))
	 (strg-raw (if py-if-name-main-permission-p
                       (buffer-substring-no-properties start end)
                     (py--fix-if-name-main-permission (buffer-substring-no-properties start end))))
         (strg (py--fix-start strg-raw))
         (wholebuf (unless file (or wholebuf (and (eq (buffer-size) (- end start))))))
	 (windows-config (window-configuration-to-register py-windows-config-register))
	 (origline
	  (save-restriction
	    (widen)
	    (count-lines
	     (point-min)
	     ;; count-lines doesn't honor current line when at BOL
	     end)))
	 ;; argument SHELL might be a string like "python", "IPython" "python3", a symbol holding PATH/TO/EXECUTABLE or just a symbol like 'python3
	 (which-shell
	  (if shell
	      ;; shell might be specified in different ways
	      (or (and (stringp shell) shell)
		  (ignore-errors (eval shell))
		  (and (symbolp shell) (prin1-to-string shell)))
	    (py-choose-shell)))
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
	 (buffer (py--choose-buffer-name which-shell))
	 (filename (or (and filename (expand-file-name filename)) (and (not (buffer-modified-p)) (buffer-file-name))))
	 (py-orig-buffer-or-file (or filename (current-buffer)))
	 (proc (cond (proc)
		     ;; will deal with py-dedicated-process-p also
		     (py-fast-process-p (get-buffer-process (py-fast-process buffer)))
		     (py-dedicated-process-p
		      (get-buffer-process (py-shell nil py-dedicated-process-p which-shell buffer)))
		     (t (or (get-buffer-process buffer)
			    (get-buffer-process (py-shell nil py-dedicated-process-p which-shell buffer)))))))
    (setq py-buffer-name buffer)
    (py--execute-base-intern strg shell filename proc file wholebuf buffer origline)
    (when py-debug-p (message "py--execute-base: py-split-window-on-execute: %s" py-split-window-on-execute))
    (when (or py-split-window-on-execute py-switch-buffers-on-execute-p)
      (py--shell-manage-windows buffer windows-config py-exception-buffer))))

(defun py--send-to-fast-process (strg proc output-buffer)
  "Called inside of `py--execute-base-intern' "
  (with-current-buffer (setq output-buffer (process-buffer proc))
    (sit-for 0.2 t)
    (erase-buffer)
    (py--fast-send-string-intern strg
				 proc
				 output-buffer py-store-result-p py-return-result-p)
    (sit-for 0.1)))

(defun py--postprocess-comint (output-buffer origline windows-config py-exception-buffer orig)
  "Provide return values, check result for error, manage windows. "
  ;; py--fast-send-string doesn't set origline
  (setq py-result nil
	py-result-raw nil
	py-error nil)
  (when py-debug-p (message "py--postprocess-comint: py-split-window-on-execute: %s" py-split-window-on-execute))
  ;; py-ert-wrong-python-test fails otherwise
  (sit-for 0.1 t)
  (with-current-buffer output-buffer
    ;; (when py-debug-p (switch-to-buffer (current-buffer)))
    (setq py-result (py--fetch-result orig))
    (sit-for 1 t))
  (when py-debug-p (message "py-result: %s" py-result))
  (and (string-match "\n$" py-result)
       (setq py-result (substring py-result 0 (match-beginning 0))))
  (sit-for 0.1 t)
  (if py-result
      (progn
	(if (string-match "^Traceback" py-result)
	    (progn
	      (sit-for 1 t)
	      (with-temp-buffer
		(when py-debug-p (message "py-result: %s" py-result))
		(insert py-result)
		(setq py-error (py--fetch-error (current-buffer) origline)))
	      (sit-for 0.1 t)
	      (with-current-buffer output-buffer
		(when py-debug-p (switch-to-buffer (current-buffer))
		      (message "py-error: %s" py-error))
		(delete-region (point) (car comint-last-prompt))
		(sit-for 0.1 t)
		(insert py-error)
		(newline)
		(goto-char (point-max))))
	  ;; position no longer needed, no need to correct
	  (when py-store-result-p
	    (when (and py-result (not (string= "" py-result))(not (string= (car kill-ring) py-result))) (kill-new py-result))))
	(or py-error py-result))
    (message "py--postprocess-comint: %s" "Don't see any result")))

(defun py--execute-base-intern (strg shell filename proc file wholebuf buffer origline)
  "Select the handler.

When optional FILE is `t', no temporary file is needed. "
  ;; (when py-debug-p (message "run: %s" "py--execute-base-intern"))
  ;; (when py-debug-p (message "py--execute-base-intern: py-split-window-on-execute: %s" py-split-window-on-execute))
  (let (output-buffer erg)
    (setq py-error nil)
    ;; (when py-debug-p
    ;;   (with-temp-file "/tmp/py-buffer-name.txt" (insert py-buffer-name)))
    (set-buffer py-exception-buffer)
    (py--update-execute-directory proc buffer execute-directory)
    (cond (py-fast-process-p (py--send-to-fast-process strg proc output-buffer))
	  ;; enforce proceeding as python-mode.el v5
	  (python-mode-v5-behavior-p
	   (py-execute-python-mode-v5 start end py-exception-buffer))
	  (py-execute-no-temp-p
	   (py--execute-ge24.3 start end filename execute-directory which-shell py-exception-buffer proc))
	  ((and filename wholebuf)
	   (py--execute-file-base proc filename nil buffer filename execute-directory py-exception-buffer))
	  (t (py--execute-buffer-finally strg execute-directory wholebuf which-shell proc buffer)))))

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

(defun py--insert-execute-directory (directory &optional orig done)
  (let ((orig (or orig (point)))
        (done done))
    (if done (goto-char done) (goto-char (point-min)))
    (cond ((re-search-forward "^from __future__ import " nil t 1)
           (py-end-of-statement)
           (setq done (point))
           (py--insert-execute-directory directory orig done))
          ((re-search-forward py-encoding-string-re nil t 1)
           (setq done (point))
           (py--insert-execute-directory directory orig done))
          ((re-search-forward py-shebang-regexp nil t 1)
           (setq done (point))
           (py--insert-execute-directory directory orig done))
          (t (forward-line 1)
             (unless (and (bolp)(eolp)) (newline))
             (insert (concat "import os; os.chdir(\"" directory "\")\n"))))))

(defun py--fix-if-name-main-permission (string)
  "Remove \"if __name__ == '__main__ '\" from code to execute.

See `py-if-name-main-permission-p'"
  (let ((strg (if py-if-name-main-permission-p string
		(replace-regexp-in-string
		 "if[( ]*__name__[) ]*==[( ]*['\"]\\{1,3\\}__main__['\"]\\{1,3\\}[) ]*:"
		 ;; space after __main__, i.e. will not be executed
		 "if __name__ == '__main__ ':" string))))
    strg))

(defun py--fix-start-intern (start end)
  (goto-char start)
  (while
      (member (char-after) (list 9 32))
    (delete-char 1))
  (unless (py--beginning-of-statement-p)
    (py-down-statement))
  (while (not (eq (current-indentation) 0))
    (py-shift-left py-indent-offset start end))
  (goto-char (point-max))
  (unless (empty-line-p)
    (newline)))

(defun py--fix-start (string)
  "Internal use by py-execute... functions.

Avoid empty lines at the beginning. "
  ;; (when py-debug-p (message "py--fix-start:"))
  (with-temp-buffer
    (insert string)
    (goto-char 1)
    ;; (when py-debug-p (message "start: %s" (point))
    ;; (setq buffer-read-only nil)
    ;; (message "buffer-read-only: %s" buffer-read-only))
    (while
	(member (char-after) (list 9 32))
      (delete-char 1))
    (unless (py--beginning-of-statement-p)
      (py-down-statement))
    (while (not (eq (current-indentation) 0))
      (py-shift-left py-indent-offset start end))
    (goto-char (point-max))
    (unless (empty-line-p)
      (newline))
    ;; (when py-debug-p (message "end: %s" (point)))
    ;; (py--fix-start-intern (point-min) (point-max))
    ;; FixMe: Maybe conditial from from some use-tempfile var?
    ;; (and (ignore-errors tempfile)
    ;; (write-region (point-min) (point-max) tempfile nil t nil 'ask))
    (buffer-substring-no-properties 1 (point-max))))

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
  (interactive "p")
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
          (py--execute-file-base proc file
                                (if (string-match "\\.py$" file)
                                    (let ((m (py--qualified-module-name (expand-file-name file))))
                                      (if (string-match "python2" (file-name-nondirectory shell))
                                          (format "import sys\nif sys.modules.has_key('%s'):\n reload(%s)\nelse:\n import %s\n" m m m)
                                        (format "import sys,imp\nif'%s' in sys.modules:\n imp.reload(%s)\nelse:\n import %s\n" m m m)))
                                  ;; (format "execfile(r'%s')\n" file)
                                  (py-which-execute-file-command file))))
      (py-execute-buffer))))

(defun py--qualified-module-name (file)
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
    (py--execute-buffer-base)))

(defun py-execute-buffer-switch ()
  "Send the contents of the buffer to a Python interpreter and switches to output. "
  (interactive)
  (let ((py-switch-buffers-on-execute-p t))
    (py--execute-buffer-base)))

(defalias 'py-execute-buffer-switch-dedicated 'py-execute-buffer-dedicated-switch)
(defun py-execute-buffer-dedicated-switch ()
  "Send the contents of the buffer to an unique Python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p'. "
  (interactive)
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py--execute-buffer-base)))

(defun py-execute-buffer ()
  "Send the contents of the buffer to a Python interpreter. "
  (interactive)
  ;; (when py-debug-p (message "run: %s" "py-execute-buffer"))
  (let ((origline (or (ignore-errors origline) 1)))
    (and py-prompt-on-changed-p (buffer-file-name) (interactive-p) (buffer-modified-p)
         (y-or-n-p "Buffer changed, save first? ")
         (write-file (buffer-file-name)))
    (py-execute-region (point-min) (point-max))))

(defun py--execute-buffer-base ()
  "Honor `py-master-file'. "
  (let* ((py-master-file (or py-master-file (py-fetch-py-master-file)))
         (file
          (if py-master-file
              (expand-file-name py-master-file)
            (buffer-file-name))))
    (if file
	(py-execute-file file)
      (py-execute-region (point-min) (point-max)))))

(defun py-execute-buffer-no-switch ()
  "Send the contents of the buffer to a Python interpreter but don't switch to output. "
  (interactive)
  (let (py-switch-buffers-on-execute-p)
    (py--execute-buffer-base)))

;; Fixme: Try to define the function or class within the relevant
;; module, not just at top level.
(defun py-execute-defun ()
  "Send the current defun (class or method) to the Python process."
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

(defun py-execute-file (filename)
  "When called interactively, user is prompted for filename. "
  (interactive "fFilename: ")
  (let (;; py--postprocess-intern might want origline
        (origline 1)
        (windows-config (window-configuration-to-register 313465889))
        (py-exception-buffer filename)
        erg)
    (if (file-readable-p filename)
        (if py-store-result-p
            (setq erg (py--execute-file-base nil (expand-file-name filename)))
          (py--execute-file-base nil (expand-file-name filename)))
      (message "%s not readable. %s" filename "Do you have write permissions?"))
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

(unless py-separator-char (setq py-separator-char (py-update-separator-char)))

(defun py--current-working-directory (&optional shell)
  "Return the directory of current `py-shell'."
  (replace-regexp-in-string "\n" "" (shell-command-to-string (concat (or shell py-shell-name) " -c \"import os; print(os.getcwd())\""))))

(defun py--update-execute-directory-intern (dir proc)
  (comint-send-string proc (concat "import os;os.chdir(\"" dir "\")\n")))

(defun py--update-execute-directory (proc procbuf execute-directory)
  (let ((py-exception-buffer (current-buffer))
        orig cwd)
    (set-buffer procbuf)
    (setq cwd (py--current-working-directory))
    (setq orig (point))
    (unless (string= execute-directory (concat cwd "/"))
      (py--update-execute-directory-intern (or py-execute-directory execute-directory) proc)
      (delete-region orig (point-max)))
    (set-buffer py-exception-buffer)))

(defun py--store-result-maybe (erg)
  "If no error occurred and `py-store-result-p' store result for yank. "
  (and (not py-error) erg (or py-debug-p py-store-result-p) (kill-new erg)))

(defun py--close-execution (tempbuf)
  "Delete temporary buffer and and run `py--store-result-maybe'"
  (unless py-debug-p
    (py-kill-buffer-unconditional tempbuf)
    (py-delete-temporary tempfile tempbuf)))

(defun py--fetch-result (orig)
  "Return buffer-substring from orig to point-max. "
  (buffer-substring-no-properties orig (point-max)))

;;; Pdb
;; Autoloaded.
(declare-function compilation-shell-minor-mode "compile" (&optional arg))

(defun py--pdbtrack-overlay-arrow (activation)
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

(defun py--pdbtrack-track-stack-file (text)
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
        (py--pdbtrack-overlay-arrow nil)

      (let* ((procmark (process-mark currproc))
             (block (buffer-substring (max comint-last-input-end
                                           (- procmark
                                              py-pdbtrack-track-range))
                                      procmark))
             target target_fname target_lineno target_buffer)

        (if (not (string-match (concat py-pdbtrack-input-prompt "$") block))
            (py--pdbtrack-overlay-arrow nil)

          (setq target (py--pdbtrack-get-source-buffer block))

          (if (stringp target)
              (message "pdbtrack: %s" target)

            (setq target_lineno (car target))
            (setq target_buffer (cadr target))
            (setq target_fname (buffer-file-name target_buffer))
            (switch-to-buffer-other-window target_buffer)
            (goto-char (point-min))
            (forward-line (1- target_lineno))
            (message "pdbtrack: line %s, file %s" target_lineno target_fname)
            (py--pdbtrack-overlay-arrow t)
            (pop-to-buffer origbuf t)))))))

(defun py--pdbtrack-map-filename (filename)

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

(defun py--pdbtrack-get-source-buffer (block)
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

            ((file-exists-p (py--pdbtrack-map-filename filename))
             (list lineno (find-file-noselect (py--pdbtrack-map-filename filename))))

            ((setq funcbuffer (py--pdbtrack-grub-for-buffer funcname lineno))
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

(defun py--pdbtrack-grub-for-buffer (funcname lineno)
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
  ;; (if (not (get-buffer-process (current-buffer)))
  ;; (error "No process associated with buffer '%s'" (current-buffer)))

  ;; missing or 0 is toggle, >0 turn on, <0 turn off
  (cond ((not arg)
         (setq py-pdbtrack-do-tracking-p (not py-pdbtrack-do-tracking-p)))
        ((zerop (prefix-numeric-value arg))
         (setq py-pdbtrack-do-tracking-p nil))
        ((> (prefix-numeric-value arg) 0)
         (setq py-pdbtrack-do-tracking-p t)))
  (if py-pdbtrack-do-tracking-p
      (progn
        (add-hook 'comint-output-filter-functions 'py--pdbtrack-track-stack-file t)
        (remove-hook 'comint-output-filter-functions 'python-pdbtrack-track-stack-file t))
    (remove-hook 'comint-output-filter-functions 'py--pdbtrack-track-stack-file t)
    (remove-hook 'comint-output-filter-functions 'python-pdbtrack-track-stack-file t))
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
  (let (imports erg)
    (save-excursion
      (if (eq major-mode 'comint-mode)
	  (progn
	    (re-search-backward comint-prompt-regexp nil t 1)
	    (goto-char (match-end 0))
	    (while (re-search-forward
		    "import *[A-Za-z_][A-Za-z_0-9].*\\|^from +[A-Za-z_][A-Za-z_0-9.]+ +import .*" nil t)
	      (setq imports
		    (concat
		     imports
		     (replace-regexp-in-string
		      "[\\]\r?\n?\s*" ""
		      (buffer-substring-no-properties (match-beginning 0) (point))) ";")))
	    (when (ignore-errors (string-match ";" imports))
	      (setq imports (split-string imports ";" t))
	      (dolist (ele imports)
		(and (string-match "import" ele)
		     (if erg
			 (setq erg (concat erg ";" ele))
		       (setq erg ele)))
		(setq imports erg))))
	(goto-char (point-min))
	(while (re-search-forward
		"^import *[A-Za-z_][A-Za-z_0-9].*\\|^from +[A-Za-z_][A-Za-z_0-9.]+ +import .*" nil t)
	  (unless (py--end-of-statement-p)
	    (py-end-of-statement))
	  (setq imports
		(concat
		 imports
		 (replace-regexp-in-string
		  "[\\]\r*\n*\s*" ""
		  (buffer-substring-no-properties (match-beginning 0) (point))) ";")))))
    ;; (and imports
    ;; (setq imports (replace-regexp-in-string ";$" "" imports)))
    (when (and py-verbose-p (interactive-p)) (message "%s" imports))
    imports))

(defalias 'py-describe-symbol 'py-help-at-point)
(defalias 'py-eldoc-function 'py-help-at-point)
(defun py-help-at-point (&optional debug)
  "Print help on symbol at point.

If symbol is defined in current buffer, jump to it's definition
Optional \\[universal-argument] used for debugging, will prevent deletion of temp file. "
  (interactive "P")
  (let* ((orig (point))
         (beg (progn (when (and (looking-back "(")(not (looking-at "\\sw"))) (forward-char -1)) (skip-chars-backward "a-zA-Z0-9_." (line-beginning-position))(point)))
         (end (progn (skip-chars-forward "a-zA-Z0-9_." (line-end-position))(point)))
         (sym (buffer-substring-no-properties beg end))
         (origfile (buffer-file-name))
         (temp (md5 (buffer-name)))
         (file (concat (py--normalize-directory py-temp-directory) temp "-py-help-at-point.py"))
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
      (if py-max-help-buffer-p
          (progn
            (set-buffer "*Python-Help*")
            (switch-to-buffer (current-buffer))
            (help-mode)
            (delete-other-windows))
        (message "%s" erg))
      (when (file-readable-p file)
        (unless py-debug-p (delete-file file))))))


(defun py-describe-mode ()
  "Dump long form of `python-mode' docs."
  (interactive)
  (py--dump-help-string "Major mode for editing Python files.
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

;;; Info
(defun py-info-lookup-symbol ()
  (interactive)
  "Calls `info-lookup-symbol'.

Sends help if stuff is missing. "
  (if (functionp 'pydoc-info-add-help)
      (call-interactively 'info-lookup-symbol)
    (message "pydoc-info-add-help not found. Please check INSTALL-INFO-FILES")))

;;;
(defun variables-state (&optional buffer directory-in directory-out)
  "Diplays state of python-mode variables in an org-mode buffer.

Reads variables from python-mode.el as current buffer.

Variables which would produce a large output are left out:
- syntax-tables
- python-mode-map

Maybe call M-x describe-variable RET to query its value. "
  (interactive)
  (variables-prepare "state"))

(defun variables-prepare (kind)
  "Used by variable-finds, variable-states. "
  (let* ((py-exception-buffer (buffer-name (or buffer (current-buffer))))
         ;; (file (buffer-file-name))
         (orgname (concat (substring py-exception-buffer 0 (string-match "\\." py-exception-buffer)) ".org"))
         (reSTname (concat (substring py-exception-buffer 0 (string-match "\\." py-exception-buffer)) ".rst"))
         (directory-in (or directory-in (and (not (string= "" py-devel-directory-in)) py-devel-directory-in) default-directory))
         (directory-out (or directory-out (expand-file-name finds-directory-out)))
	 (command (concat "variables-base-" kind)))
    (funcall (intern-soft command) py-exception-buffer orgname reSTname directory-in directory-out)))

(defun variables-base-state (py-exception-buffer orgname reSTname directory-in directory-out)
  (save-restriction
    (let ((suffix (file-name-nondirectory (buffer-file-name)))
          variableslist)
      ;; (widen)
      (goto-char (point-min))
      ;; (eval-buffer)
      (while (and (not (eobp))(re-search-forward "^(defvar [[:alpha:]]\\|^(defcustom [[:alpha:]]\\|^(defconst [[:alpha:]]" nil t 1))
        (let* ((name (symbol-at-point))
               (state
                (unless
                    (or (eq name 'py-menu)
                        (eq name 'python-mode-map)
                        (string-match "syntax-table" (prin1-to-string name)))

                  (prin1-to-string (symbol-value name)))))
          (if state
              (add-to-list 'variableslist (cons (prin1-to-string name) state))
            (message "don't see a state for %s" (prin1-to-string name))))
        (forward-line 1))
      (setq variableslist (nreverse variableslist))
      ;; (with-temp-buffer
      (set-buffer (get-buffer-create "State-of-Python-mode-variables.org"))
      (erase-buffer)
      ;; org
      (insert "State of python-mode variables\n\n")
      (switch-to-buffer (current-buffer))
      (dolist (ele variableslist)
        (if (string-match "^;;; " (car ele))
            (unless (or (string-match "^;;; Constants\\|^;;; Commentary\\|^;;; Code\\|^;;; Macro definitions\\|^;;; Customization" (car ele)))

              (insert (concat (replace-regexp-in-string "^;;; " "* " (car ele)) "\n")))
          (insert (concat "\n** "(car ele) "\n"))
          (insert (concat "   " (cdr ele) "\n\n")))
        (richten)
        (sit-for 0.01))
      (sit-for 0.01)
      (org-mode))))

;;;
(defun py-load-file (file-name)
  "Load a Python file FILE-NAME into the Python process.

If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names."
  (interactive "f")
  (py--execute-file-base (get-buffer-process (get-buffer (py-shell))) file-name))

(defalias 'py-find-function 'py-find-definition)
(defun py-find-definition (&optional symbol)
  "Find source of definition of SYMBOL.

Interactively, prompt for SYMBOL."
  (interactive)
  (set-register 98888888 (list (current-window-configuration) (point-marker)))
  (let* ((py-exception-buffer (current-buffer))
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
                 (py--until-found (concat "class " symbol) imenu--index-alist)
                 (py--until-found symbol imenu--index-alist)))
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
      (setq source (py--send-string-return-output (concat imports "import inspect;inspect.getmodule(" symbol ")")))
      (cond ((string-match "SyntaxError" source)
             (setq source (substring-no-properties source (match-beginning 0)))
             (jump-to-register 98888888)
             (message "Can't get source: %s" source))
            ((and source (string-match "builtin" source))
             (progn (jump-to-register 98888888)
                    (message "%s" source)))
            ((and source (setq path (replace-regexp-in-string "'" "" (py--send-string-return-output "import os;os.getcwd()")))
                  (setq sourcefile (replace-regexp-in-string "'" "" (py--send-string-return-output (concat "inspect.getsourcefile(" symbol ")"))))
                  (interactive-p) (message "sourcefile: %s" sourcefile)
                  (find-file (concat path py-separator-char sourcefile))
                  (goto-char (point-min))
                  (re-search-forward (concat py-def-or-class-re symbol) nil nil 1))
             (push-mark)
             (goto-char (match-beginning 0))
             (exchange-point-and-mark)
             (display-buffer py-exception-buffer)))
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

(defun py--empty-arglist-indent (nesting py-indent-offset indent-offset)
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

(defun py--line-backward-maybe ()
  (skip-chars-backward " \t\f" (line-beginning-position))
  (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
    (setq line t)))

(defun py-symbol-at-point ()
  "Return the current Python symbol."
  (interactive)
  (let ((erg (with-syntax-table
                 py-dotted-expression-syntax-table
               (current-word))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py--fetch-previous-indent (orig)
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

(defun py--after-empty-line ()
  "Return `t' if line before contains only whitespace characters. "
  (save-excursion
    (beginning-of-line)
    (forward-line -1)
    (beginning-of-line)
    (looking-at "\\s-*$")))

(defalias 'py-count-indentation 'py-compute-indentation)
(defun py-compute-indentation (&optional orig origline closing line nesting repeat indent-offset liep)
  "Compute Python indentation.

When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of dedenting.

Optional arguments are flags resp. values set and used by `py-compute-indentation' internally
"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      ;; in shell, narrow from previous prompt
      ;; needed by closing
      (unless orig (unless (bobp) (back-to-indentation)))
      (let* ((orig (or orig (point)))
             (origline (or origline (py-count-lines)))
             ;; closing indicates: when started, looked
             ;; at a single closing parenthesis
             ;; line: moved already a line backward
             (liep (or liep (line-end-position)))
             (line line)
             (pps (syntax-ppss))
             (closing
              (or closing
                  (and (nth 1 pps)
                       (looking-at ".*\\(\\s)\\)")(nth 0 pps)
                       ;; char doesn't matter for now, maybe drop
                       (string-to-char (match-string-no-properties 1)))))
             ;; in a recursive call already
             (repeat (if repeat
			 (setq repeat (1+ repeat))
		       0))
             ;; nesting: started nesting a list
             (nesting nesting)
             (indent-offset (or indent-offset py-indent-offset))
             (name (current-buffer))
             erg indent this-line)
        (if (and (< repeat 1)
                 (and (comint-check-proc (current-buffer))
                      (re-search-backward (concat py-shell-prompt-regexp "\\|" ipython-de-output-prompt-regexp "\\|" ipython-de-input-prompt-regexp) nil t 1)))
            ;; common recursion not suitable because of prompt
            (with-temp-buffer
              (insert-buffer-substring name (match-end 0) orig)
              (goto-char orig)
              (setq indent (py-compute-indentation)))
	  (if (< py-max-specpdl-size repeat)
	      (error "`py-compute-indentation' reached loops max.")
	    (setq nesting (nth 0 pps))
	    (setq indent
		  (cond
		   ((and (bobp) (eq liep (line-end-position)))
		    0)
		   ((and (bobp)(py--statement-opens-block-p py-extended-block-or-clause-re))
		    (+ (if py-smart-indentation (py-guess-indent-offset) indent-offset) (current-indentation)))
		   ((and (bobp)(not (py--statement-opens-block-p py-extended-block-or-clause-re)))
		    (current-indentation))
		   ;; in string
		   ((and (nth 3 pps)(nth 8 pps))
		    (if
			;; still at original line
			(eq origline (line-end-position))
			(progn
			  (forward-line -1)
			  (end-of-line)
			  (skip-chars-backward " \t\r\n\f")
			  (if (ignore-errors (< (nth 8 (syntax-ppss)) (line-beginning-position)))
			      (current-indentation)
			    (ignore-errors (goto-char (nth 8 pps)))
			    (py--line-backward-maybe)
			    (back-to-indentation)
			    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep)))
		      (goto-char (nth 8 pps))
		      (current-indentation)))
		   ((and (looking-at "\"\"\"\\|'''")(not (bobp)))
		    (py-beginning-of-statement)
		    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		   ;; comments
		   ((nth 8 pps)
		    (if (eq liep (line-end-position))
			(progn
			  (goto-char (nth 8 pps))
			  (py--line-backward-maybe)
			  (skip-chars-backward " \t")
			  (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		      (goto-char (nth 8 pps))
		      (if
			  line
			  (if py-indent-honors-inline-comment
			      (current-column)
			    (if py-indent-comments
				(progn
				  (py-beginning-of-commented-section)
				  (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			      0))
			(forward-char -1)
			(py-compute-indentation orig origline closing line nesting repeat indent-offset liep))))
		   ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*")(not line)
			 (eq liep (line-end-position)))
		    (if py-indent-comments
			(progn
			  (setq line t)
			  (skip-chars-backward " \t\r\n\f")
			  ;; as previous comment-line might
			  ;; be wrongly unindented, travel
			  ;; whole commented section
			  (py-beginning-of-commented-section)
			  (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		      0))
		   ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*")(not
									 (eq liep (line-end-position))))
		    (current-indentation))
		   ((and (eq ?\# (char-after)) line py-indent-honors-inline-comment)
		    (current-column))
		   ;; lists
		   ((nth 1 pps)
		    (if
		     ;; ((and nesting (not line))
		      nesting
		       ;; still at original line
		      (save-excursion
			(goto-char (nth 1 pps))
			(setq this-line (py-count-lines))
			(cond
			 ((< 0 (- origline this-line))
			  (if (< 1 (- origline this-line))
			      (cond
			       (closing
				(cond
				 (py-closing-list-dedents-bos
				  (goto-char (nth 1 pps))
				  (current-indentation))
				 ((looking-back "^[ \t]*")
				  (current-column))
				 ((and (looking-at "\\s([ \t]*$") py-closing-list-keeps-space)
				  (+ (current-column) py-closing-list-space))
				 ((looking-at "\\s([ \t]*$")
				  (py--empty-arglist-indent nesting py-indent-offset indent-offset))
				 (t (py--fetch-previous-indent orig))))
			       ;; already behind a dedented element in list
			       ((<= 2 (- origline this-line))
				(py--fetch-previous-indent orig))
			       ((< (current-indentation) (current-column))
				(+ (current-indentation) py-indent-offset))
			       (t (py--fetch-previous-indent orig)))
			    (cond ((looking-at "\\s([ \t]*$")
				   (py--empty-arglist-indent nesting py-indent-offset indent-offset))
				  ((looking-at "\\s([ \t]*\\([^ \t]+.*\\)$")
				   (goto-char (match-beginning 1))
				   (if py-indent-paren-spanned-multilines-p
				       (+ (current-column) py-indent-offset)
				     (current-column)))
				  (t (+ (current-column) (* (nth 0 pps)))))))
			 ((nth 1 (syntax-ppss))
			  (goto-char (nth 1 (syntax-ppss)))
			  (setq line
				;; should be faster
				(< (line-end-position) liep)
				;; (< (py-count-lines) origline)
				)
			  (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			 ((not (py--beginning-of-statement-p))
			  (py-beginning-of-statement)
			  (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			 (t (1+ (current-column)))))
		     (if line
			 (progn 
		      (py-beginning-of-statement)
		      (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			(goto-char (+ py-lhs-inbound-indent (nth 1 pps)))
			     (when (looking-at "[ \t]+")
			       (goto-char (match-end 0)))
			     (current-column))
		     ;; nesting or not nesting
		     ;; (t
		     ;; (goto-char (nth 1 pps))
		     ;; (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		     ))
		   ((and (eq (char-after) (or ?\( ?\{ ?\[)) line)
		    (1+ (current-column)))
		   ((py-preceding-line-backslashed-p)
		    (progn
		      (py-beginning-of-statement)
		      (setq this-line (py-count-lines))
		      (if (< 1 (- origline this-line))
			  (py--fetch-previous-indent orig)
			(if (looking-at "from +\\([^ \t\n]+\\) +import")
			    py-backslashed-lines-indent-offset
			  (+ (current-indentation) py-continuation-offset)))))
		   ((and (looking-at py-block-closing-keywords-re)
			 (eq liep (line-end-position)))
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
		    (if
			(eq liep (line-end-position))
			(progn
			  (back-to-indentation)
			  (py--line-backward-maybe)
			  (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		      (current-indentation)))
		   ((and (looking-at py-elif-re) (eq (py-count-lines) origline))
		    (py--line-backward-maybe)
		    (car (py--clause-lookup-keyword py-elif-re -1 nil orig origline)))
		   ((and (looking-at py-clause-re)(not line)
			 (eq liep (line-end-position)))
		    (cond ((looking-at py-finally-re)
			   (car (py--clause-lookup-keyword py-finally-re -1 nil orig origline)))
			  ((looking-at py-except-re)
			   (car (py--clause-lookup-keyword py-except-re -1 nil orig origline)))
			  ((looking-at py-else-re)
			   (car (py--clause-lookup-keyword py-else-re -1 nil orig origline)))
			  ((looking-at py-elif-re)
			   (car (py--clause-lookup-keyword py-elif-re -1 nil orig origline)))
			  ;; maybe at if, try, with
			  (t (car (py--clause-lookup-keyword py-block-or-clause-re -1 nil orig origline)))))
		   ((looking-at py-extended-block-or-clause-re)
		    (cond ((and (not line)
				(eq liep (line-end-position)))
			   (py--line-backward-maybe)
			   (setq line t)
			   (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			  (t (+
			      (cond (indent-offset)
				    (py-smart-indentation
				     (py-guess-indent-offset))
				    (t py-indent-offset))
			      (current-indentation)))))
		   ((and
		     (< (line-end-position) liep)
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
		    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		   ((and (< (current-indentation) (current-column))(not line))
		    (back-to-indentation)
		    (unless line
		      (setq nesting (nth 0 (syntax-ppss))))
		    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		   ((and (not (py--beginning-of-statement-p)) (not (and line (eq ?\# (char-after)))))
		    (if (bobp)
			(current-column)
		      (if (eq (point) orig)
			  (progn
			    (py--line-backward-maybe)
			    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			(py-beginning-of-statement)
			(py-compute-indentation orig origline closing line nesting repeat indent-offset liep))))
		   ((or (py--statement-opens-block-p py-extended-block-or-clause-re)(looking-at "@"))
		    (if (< (py-count-lines) origline)
			(+ (if py-smart-indentation (py-guess-indent-offset) indent-offset) (current-indentation))
		      (skip-chars-backward " \t\r\n\f")
		      (setq line t)
		      (back-to-indentation)
		      (py-compute-indentation orig origline closing line nesting repeat indent-offset liep)))
		   ((and py-empty-line-closes-p (py--after-empty-line))
		    (progn (py-beginning-of-statement)
			   (- (current-indentation) py-indent-offset)))
		   ;; still at orignial line
		   ((and (eq liep (line-end-position))
			 (save-excursion
			   (and (setq erg (py--go-to-keyword py-extended-block-or-clause-re))
				(if py-smart-indentation (setq indent-offset (py-guess-indent-offset)) t)
				(ignore-errors (< orig (or (py-end-of-block-or-clause)(point)))))))
		    (+ (car erg) (if py-smart-indentation
				     (or indent (py-guess-indent-offset))
				   indent-offset)))
		   ((and (not line)
			 (eq liep (line-end-position))
			 (py--beginning-of-statement-p))
		    (py-beginning-of-statement)
		    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		   (t (current-indentation))))
	    (when (and py-verbose-p (interactive-p)) (message "%s" indent))
	    indent))))))

(defalias 'pios 'py-indentation-of-statement)
(defalias 'ios 'py-indentation-of-statement)
(defun py-indentation-of-statement ()
  "Returns the indenation of the statement at point. "
  (interactive)
  (let ((erg (save-excursion
               (back-to-indentation)
               (or (py--beginning-of-statement-p)
                   (py-beginning-of-statement))
               (current-indentation))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

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

(defun py-set-ipython-completion-command-string (shell)
  "Set and return `ipython-completion-command-string'. "
  (interactive)
  (let* ((ipython-version (shell-command-to-string (concat shell " -V"))))
    (if (string-match "[0-9]" ipython-version)
        (setq ipython-completion-command-string
              (cond ((string-match "^[^0].+" ipython-version)
		     ipython0.11-completion-command-string)
                    ((string-match "^0.1[1-3]" ipython-version)
                     ipython0.11-completion-command-string)
                    ((string= "^0.10" ipython-version)
                     ipython0.10-completion-command-string)))
      (error ipython-version))))

(defun py-ipython--module-completion-import (proc)
  "Import module-completion "
  (interactive)
  (let ((ipython-version (shell-command-to-string (concat py-shell-name " -V"))))
    (when (and (string-match "^[0-9]" ipython-version)
               (string-match "^[^0].+" ipython-version))
      (process-send-string proc "from IPython.core.completerlib import module_completion")
      (process-send-string proc "\n")
      ;; (sit-for 0.1)
      )))

(defalias 'py-dedicated-shell 'py-shell-dedicated)
(defun py-shell-dedicated (&optional argprompt)
  "Start an interactive Python interpreter in another window.

With optional \\[universal-argument] user is prompted by
`py-choose-shell' for command and options to pass to the Python
interpreter."
  (interactive "P")
  (py-shell argprompt t))

(defun py--compose-buffer-name-initials (liste)
  (let (erg)
    (dolist (ele liste)
      (unless (string= "" ele)
	(setq erg (concat erg (char-to-string (aref ele 0))))))
    erg))

(defun py--remove-home-directory-from-list (liste)
  "Prepare for compose-buffer-name-initials. "
  (let ((case-fold-search t)
	(liste liste)
	erg)
    (if (listp (setq erg (split-string (expand-file-name "~") "\/")))
	erg
      (setq erg (split-string (expand-file-name "~") "\\\\")))
     (while erg
      (when (member (car erg) liste)
	(setq liste (cdr (member (car erg) liste))))
      (setq erg (cdr erg)))
    (butlast liste)))

(defun py--choose-buffer-name (&optional name dedicated fast-process)
  "Return an appropriate name to display in modeline.
SEPCHAR is the file-path separator of your system. "
  (let* ((name-first (or name py-shell-name))
	 (erg (when name-first (if (stringp name-first) name-first (prin1-to-string name-first))))
	 (fast-process (or fast-process py-fast-process-p))
	 prefix suffix liste)
    ;; remove suffix
    (when (string-match "[.]" erg)
      (setq erg (substring erg 0 (string-match "[.]" erg))))
    ;; remove prefix
    (when (string-match "^py-" erg)
      (setq erg (nth 1 (split-string erg "-"))))
    ;; remove home-directory from prefix to display
    (unless py-modeline-acronym-display-home-p
      (save-match-data
	(let ((case-fold-search t))
	  (when (string-match (concat ".*" (expand-file-name "~")) erg)
	    (setq erg (replace-regexp-in-string (concat "^" (expand-file-name "~")) "" erg))))))
    (if (or (and (setq prefix (split-string erg "\\\\"))
		 (< 1 (length prefix)))
	    (and (setq prefix (split-string erg "\/"))
		 (< 1 (length prefix))))
	(progn
	  ;; exect something like default py-shell-name
	  (setq erg (car (last prefix)))
	  (unless py-modeline-acronym-display-home-p
	    ;; home-directory may still inside
	    (setq prefix (py--remove-home-directory-from-list prefix))
	    (setq prefix (py--compose-buffer-name-initials prefix))))
      (setq erg (or name py-shell-name))
      (setq prefix nil))
    (when fast-process (setq erg (concat erg " Fast")))

    ;; (setq name (substring name (1+ (string-match "/[^/]+\\|\\\\[[:alnum:].]+$" name)))))
    (setq erg
          (cond ((string-match "^ipython" erg)
                 (replace-regexp-in-string "ipython" "IPython" erg))
                ((string-match "^jython" erg)
                 (replace-regexp-in-string "jython" "Jython" erg))
                ((string-match "^python" erg)
                 (replace-regexp-in-string "python" "Python" erg))
                ((string-match "^python2" erg)
                 (replace-regexp-in-string "python2" "Python2" erg))
                ((string-match "^python3" erg)
                 (replace-regexp-in-string "python3" "Python3" erg))
                (t erg)))
    (when (or dedicated py-dedicated-process-p)
      (setq erg (make-temp-name (concat erg "-"))))
    (cond ((and prefix (string-match "^\*" erg))
           (setq erg (replace-regexp-in-string "^\*" (concat "*" prefix " ") erg)))
          (prefix
           (setq erg (concat "*" prefix " " erg "*")))
          (t (unless (string-match "^\*" erg)(setq erg (concat "*" erg "*")))))
    erg))

(defalias 'py-toggle-split-window-on-execute-function 'py-toggle-split-window-function)
(defun py-toggle-split-window-function ()
  "If window is splitted vertically or horizontally.

When code is executed and `py-split-window-on-execute' is `t', the result is displays in an output-buffer, \"\*Python\*\" by default.

Customizable variable `py-split-windows-on-execute-function' tells how to split the screen."
  (interactive)
  (if (eq 'split-window-vertically py-split-windows-on-execute-function)
      (setq py-split-windows-on-execute-function'split-window-horizontally)
    (setq py-split-windows-on-execute-function 'split-window-vertically))
  (when (and py-verbose-p (interactive-p))
    (message "py-split-windows-on-execute-function set to: %s" py-split-windows-on-execute-function)))

(defun py--alternative-split-windows-on-execute-function ()
  "If `py--split-windows-on-execute-function' is `split-window-vertically' return `split-window-horizontally' and vice versa"
  (if (eq py-split-windows-on-execute-function 'split-window-vertically)
      'split-window-horizontally
    'split-window-vertically))

(defun py--get-splittable-window (output-buffer)
  "If selected window doesn't permit a further split, search window-list for a suitable one. "
  (let ((this-window (selected-window))
	erg)
    (or (and (window-left-child)(split-window (window-left-child)))
	(and (window-top-child)(split-window (window-top-child)))
	(and (window-parent)(ignore-errors (split-window (window-parent))))
	(and (window-atom-root)(split-window (window-atom-root))))))

(defun py--manage-windows-split (exception-buffer output-buffer)
  "If one window, split according to `py-split-windows-on-execute-function. "
  (interactive)
  (set-buffer exception-buffer)
  (when py-debug-p (message "Calling %s" "py--manage-windows-split"))
  (or
   (ignore-errors (funcall py-split-windows-on-execute-function))
   ;; If call didn't succeed according to settings of
   ;; `split-height-threshold', `split-width-threshold'
   ;; resp. `window-min-height', `window-min-width'
   ;; try alternative split
   (unless (ignore-errors (funcall (py--alternative-split-windows-on-execute-function)))
     ;; if alternative split fails, look for larger window
     (py--get-splittable-window output-buffer)
     (ignore-errors (funcall (py--alternative-split-windows-on-execute-function))))))

(defun py--manage-windows-set-and-switch (buffer)
  "Switch to output-buffer, go to point-max.

Internal use"
  (set-buffer buffer)
  (goto-char (process-mark (get-buffer-process (current-buffer)))))

(defun py--shell-manage-windows (output-buffer windows-config py-exception-buffer)
  "Adapt or restore window configuration. Return nil "
  ;; (message "py-exception-buffer: %s" py-exception-buffer)
  (let ((output-buffer (or output-buffer py-buffer-name))
	(py-exception-buffer (or py-exception-buffer (current-buffer))))
    (cond
     (py-keep-windows-configuration
      (py-restore-window-configuration)
      (set-buffer output-buffer)
      (goto-char (point-max)))
     ((and (eq py-split-window-on-execute 'always)
	   py-switch-buffers-on-execute-p)
      (if (member (get-buffer-window output-buffer)(window-list))
	  ;; (delete-window (get-buffer-window output-buffer))
	  (select-window (get-buffer-window output-buffer))
	(py--manage-windows-split py-exception-buffer output-buffer)
	;; otherwise new window appears above
	(save-excursion
	  (other-window 1)
	  (switch-to-buffer output-buffer))
	(display-buffer py-exception-buffer)))
     ((and
       (eq py-split-window-on-execute 'always)
       (not py-switch-buffers-on-execute-p))
      (if (member (get-buffer-window output-buffer)(window-list))
	  (select-window (get-buffer-window output-buffer))
	(py--manage-windows-split py-exception-buffer output-buffer)
	;; otherwise new window appears above
	;; (save-excursion
	;; (other-window 1)
	;; (display-buffer output-buffer)
	;;)
	(pop-to-buffer py-exception-buffer)))
     ((and
       (eq py-split-window-on-execute 'just-two)
       py-switch-buffers-on-execute-p)
      (set-buffer py-exception-buffer)
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      ;; (sit-for py-new-shell-delay)
      (py--manage-windows-split py-exception-buffer output-buffer)
      ;; otherwise new window appears above
      (other-window 1)
      (set-buffer output-buffer)
      (switch-to-buffer (current-buffer)))
     ((and
       (eq py-split-window-on-execute 'just-two)
       (not py-switch-buffers-on-execute-p))
      (set-buffer py-exception-buffer)
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (unless
	  (member (get-buffer-window output-buffer)(window-list))
	(py--manage-windows-split py-exception-buffer output-buffer))
      ;; Fixme: otherwise new window appears above
      (save-excursion
	(other-window 1)
	(pop-to-buffer output-buffer)
	(goto-char (point-max))
	(other-window 1)))
     ((and
       py-split-window-on-execute
       (not py-switch-buffers-on-execute-p))
      (set-buffer py-exception-buffer)
      (switch-to-buffer (current-buffer))
      (unless
	  (member (get-buffer-window output-buffer)(window-list))
	(py--manage-windows-split py-exception-buffer output-buffer))
      ;; Fixme: otherwise new window appears above
      (save-excursion
	(other-window 1)
	(pop-to-buffer output-buffer)
	(goto-char (point-max))
	(other-window 1)))
     ;; ((and
     ;;   py-switch-buffers-on-execute-p
     ;;   (not py-split-window-on-execute))
     ;;  (set-buffer output-buffer)
     ;;  (switch-to-buffer (current-buffer)))
     ;; no split, no switch
     ((not py-switch-buffers-on-execute-p)
      (let (pop-up-windows)
	(py-restore-window-configuration))))))

(defun py-kill-buffer-unconditional (&optional buffer)
  "Kill buffer unconditional, kill buffer-process if existing. "
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
        proc kill-buffer-query-functions)
    (ignore-errors
      (setq proc (get-buffer-process buffer))
      (and proc (kill-process proc))
      (set-buffer buffer)
      (set-buffer-modified-p 'nil)
      (kill-buffer (current-buffer)))))

(defun py-kill-shell-unconditional (&optional shell)
  "With optional argument SHELL.

Otherwise kill default (I)Python shell.
Kill buffer and its process.
Receives a buffer-name as argument"
  (interactive)
  (let ((shell (or shell (py-shell))))
    (py-kill-buffer-unconditional shell)))

(defun py-kill-default-shell-unconditional ()
  "Kill buffer \"\*Python\*\" and its process. "
  (interactive)
  (py-kill-buffer-unconditional "*Python*"))

(defun py--report-executable (py-buffer-name)
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

(defun py--delete-all-but-first-prompt ()
  "Don't let prompts from setup-codes sent clutter buffer. "
  (let (last erg)
    (when (re-search-backward py-fast-filter-re nil t 1)
      (setq erg (match-end 0))
      (while (and (re-search-backward py-fast-filter-re nil t 1) (setq erg (match-end 0))))
      (delete-region erg (point-max))))
  (goto-char (point-max)))

(defun py--shell-send-setup-code (process)
  "Send all setup code for shell.
This function takes the list of setup code to send from the
`py-setup-codes' list."
  (let ((erg (string-match "^i" (process-name process))))
    (dolist (code py-setup-codes)
      ;; (message "%s" code)
      ;; `py--fast-send-string' doesn't word with IPython for now
      ;; wants magic %paste %cpaste
      (if erg
	  (progn
	    (py--send-string-no-output
	     (py--fix-start (symbol-value code)) process)
	    (sit-for py-new-shell-delay)
	    (py--delete-all-but-first-prompt))
	(py--fast-send-string-no-output (py--fix-start (symbol-value code)) process (buffer-name (process-buffer process)))))))

(defun py--shell-simple-send (proc string)
  (let* ((strg (substring-no-properties string))
         (nln (string-match "\n$" strg)))
    ;; (or nln (setq strg (concat strg "\n")))
    ;; (comint-simple-send proc (substring-no-properties string))
    (process-send-string proc strg)
    (or nln (process-send-string proc "\n"))))

(defun py--shell-make-comint (executable py-buffer-name args)
  (let ((buffer (apply 'make-comint-in-buffer executable py-buffer-name executable nil args)))
    (with-current-buffer buffer
      (py-shell-mode)
      (sit-for 0.1 t))
    buffer))

(defun py--shell-setup (buffer proc)
  (py--shell-send-setup-code proc)
  (and (string-match "[iI][pP]ython[[:alnum:]*-]*$" py-buffer-name)
       (py-ipython--module-completion-import proc))
  (set-process-sentinel proc #'shell-write-history-on-exit))

(defun py--guess-buffer-name (argprompt)
  "Guess the buffer-name core string. "
  (and (not dedicated) argprompt
       (cond ((eq 4 (prefix-numeric-value argprompt))
	(prog1
	    (read-buffer "Py-Shell buffer: "
			 (generate-new-buffer-name (py--choose-buffer-name)))))
	     ((and (eq 2 (prefix-numeric-value argprompt))
		   (fboundp 'split-string))
	      (setq args (split-string
			  (read-string "Py-Shell arguments: "
				       (concat
					(mapconcat 'identity py-python-command-args " ") " "))))))))

(defun py--configured-shell (name)
  "Return the configured PATH/TO/STRING if any. "
  (if (string-match "//\\|\\\\" name)
      name
    (cond ((string-match "^[Ii]" name)
	   (or py-ipython-command name))
	  ((string-match "[Pp]ython3" name)
	   (or py-python3-command name))
	  ((string-match "[Pp]ython2" name)
	   (or py-python2-command name))
	  ((string-match "[Jj]ython" name)
	   (or py-jython-command name))
	  (t (or py-python-command name)))))

(defun py--unfontify-banner (&optional buffer)
  "Unfontify the shell banner-text.

Cancels `py--timer'
Expects being called by `py--run-unfontify-timer' "
  (interactive)
  (save-excursion
    (let ((buffer (or buffer (current-buffer))))
      (if (ignore-errors (buffer-live-p (get-buffer buffer)))
	  (with-current-buffer buffer
	    (goto-char (point-min))
	    (let ((erg (or (ignore-errors (car comint-last-prompt))
			   (and
			    (re-search-forward py-fast-filter-re nil t 1)
			    (match-beginning 0)))))
	      (sit-for 0.1 t)
	      (if erg
		  (progn
		    (font-lock-unfontify-region (point-min) erg)
		    (goto-char (point-max)))
		(progn (and py-debug-p (message "%s" (concat "py--unfontify-banner: Don't see a prompt in buffer " (buffer-name buffer)))))))
	    (and (timerp py--timer)(cancel-timer py--timer)))
	(and (timerp py--timer)(cancel-timer py--timer))))))

(defun py--start-fast-process (shell buffer)
  (let ((proc (start-process shell buffer shell)))
    (with-current-buffer buffer
      (erase-buffer))
    proc))

(defun py-shell (&optional argprompt dedicated shell buffer-name fast-process py-exception-buffer)
  "Start an interactive Python interpreter in another window.
  Interactively, \\[universal-argument] prompts for a new buffer-name.
  \\[universal-argument] 2 prompts for `py-python-command-args'.
  If `default-directory' is a remote file name, it is also prompted
  to change if called with a prefix arg.

  Returns py-shell's buffer-name.
  Optional string PYSHELLNAME overrides default `py-shell-name'.
  BUFFER allows specifying a name, the Python process is connected to
  "
  (interactive "P")
  ;; done by py-shell-mode
  (let* ((iact (or (interactive-p) (eq 1 argprompt))) ;; interactively?
	 (windows-config (window-configuration-to-register 313465889))
	 (fast-process (or fast-process py-fast-process-p))
	 ;; (newpath (when (eq 4 (prefix-numeric-value argprompt))
	 ;; (read-shell-command "PATH/TO/EXECUTABLE/[I]python[version]: ")))
	 (py-exception-buffer (or py-exception-buffer (and (or (eq 'major-mode 'python-mode)(eq 'major-mode 'py-shell-mode)) (current-buffer))))
	 (dedicated (or dedicated py-dedicated-process-p))
	 (path (getenv "PYTHONPATH"))
	 (py-shell-name (or shell
			    ;; (py--configured-shell (py-choose-shell))
			    (py-choose-shell)))
	 (args
	  (cond (fast-process nil)
		((string-match "^[Ii]" py-shell-name)
		 py-ipython-command-args)
		((string-match "^[^-]+3" py-shell-name)
		 py-python3-command-args)
		(t py-python-command-args)))
	 ;; unless Path is given with `py-shell-name'
	 ;; call configured command
	 ;; (py-shell-name (py--configured-shell py-shell-name-raw))

	 ;; If we use a pipe, Unicode characters are not printed
	 ;; correctly (Bug#5794) and IPython does not work at
	 ;; all (Bug#5390). python.el
	 ;; (process-connection-type t)
	 ;; already in py-choose-shell
	 (py-use-local-default
	  (if (not (string= "" py-shell-local-path))
	      (expand-file-name py-shell-local-path)
	    (when py-use-local-default
	      (error "Abort: `py-use-local-default' is set to `t' but `py-shell-local-path' is empty. Maybe call `py-toggle-local-default-use'"))))
	 (py-buffer-name (or buffer-name (py--guess-buffer-name argprompt)))
	 (py-buffer-name (or py-buffer-name (py--choose-buffer-name nil dedicated fast-process)))
	 (executable (cond (py-shell-name)
			   (py-buffer-name
			    (py--report-executable py-buffer-name))))
	 proc)
    ;; lp:1169687, if called from within an existing py-shell, open a new one
    (and (bufferp py-exception-buffer)(string= py-buffer-name (buffer-name py-exception-buffer))
	 (setq py-buffer-name (generate-new-buffer-name py-buffer-name)))
    (sit-for 0.1 t)
    (if fast-process
	;; user rather wants an interactive shell
	(unless (get-buffer-process (get-buffer py-buffer-name))
	  (py--start-fast-process py-shell-name py-buffer-name)
	  (setq py-output-buffer py-buffer-name))
      (unless (comint-check-proc py-buffer-name)
	;; buffer might exist but not being empty
	(when (buffer-live-p py-buffer-name)
	  (with-current-buffer py-buffer-name
	    (erase-buffer)))
	(py--shell-make-comint executable py-buffer-name args)
	;; if called from a program, banner needs some delay
	;; (sit-for 0.5 t)
	(setq py-output-buffer py-buffer-name)
	(if (comint-check-proc py-buffer-name)
	    (with-current-buffer py-buffer-name
	      ;; (when py-debug-p (switch-to-buffer (current-buffer)))
	      ;; (py--unfontify-banner py-buffer-name)
	      (setq proc (get-buffer-process py-buffer-name))
	      ;; (comint-send-string proc "\n")
	      (py--delay-process-dependent proc)
	      (py--shell-setup py-buffer-name proc)
	      ;; lp:1393882, occasionally input first time not processed
	      (when py-new-session-p (py-kill-buffer-unconditional py-buffer-name)
		    (setq py-new-session-p nil)
		    (py-shell argprompt dedicated shell buffer-name fast-process py-exception-buffer)))
		    
	  (error (concat "py-shell: No process in " py-buffer-name))))
      ;; (goto-char (point-max))
      (when (or (interactive-p)
		;; M-x python RET sends from interactive "p"
		argprompt
		py-switch-buffers-on-execute-p py-split-window-on-execute)
	(py--shell-manage-windows py-buffer-name windows-config py-exception-buffer)))
    ;; (sit-for py-new-shell-delay t)
    py-buffer-name))

(defun py-indent-forward-line (&optional arg)
  "Indent and move one line forward to next indentation.
Returns column of line reached.

If `py-kill-empty-line' is non-nil, delete an empty line.
When closing a form, use py-close-block et al, which will move and indent likewise.
With \\[universal argument] just indent."
  (interactive "*P")
  (let ((orig (point))
        erg)
    (unless (eobp)
      (if (and (py--in-comment-p)(not py-indent-comments))
          (forward-line 1)
        (py-indent-line-outmost)
        (unless (eq 4 (prefix-numeric-value arg))
          (if (eobp) (newline)
            (progn (forward-line 1))
            (when (and py-kill-empty-line (and (bolp)(eolp)) (not (looking-at "[ \t]*\n[[:alpha:]]")) (not (eobp)))
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

(defun py--close-intern (regexp)
  "Core function, internal used only. "
  (let ((cui (ignore-errors (car (py--go-to-keyword (symbol-value regexp))))))
    (py--end-base regexp (point))
    (forward-line 1)
    (if py-close-provides-newline
        (unless (and (bolp)(eolp)) (split-line))
      (fixup-whitespace))
    (indent-to-column cui)
    cui))

(defun py-close-def ()
  "Set indent level to that of beginning of function definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py--close-intern py-def-re)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-close-class ()
  "Set indent level to that of beginning of class definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py--close-intern py-class-re)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-close-clause ()
  "Set indent level to that of beginning of clause definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py--close-intern py-clause-re)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-close-block ()
  "Set indent level to that of beginning of block definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py--close-intern 'py-block-re)))
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
         ;; guess if doublequotes or parentheses are needed
         (numbered (and (string-match "^[0-9]" name) (string-match "^[ \t]*[0-9]" name)(string-match "[0-9][ \t]*$" name)))
         (form (cond ((or (eq major-mode 'python-mode)(eq major-mode 'py-shell-mode))
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
    (when (and (bolp)(eolp))
      (delete-region (line-beginning-position) (line-end-position)))
    (goto-char orig)
    (insert "pdb.set_trace()")))

(defun py-line-to-printform-python2 (&optional arg)
  "Transforms the item on current in a print statement. "
  (interactive "*")
  (let* ((name (thing-at-point 'word))
         (form (cond ((or (eq major-mode 'python-mode)(eq major-mode 'py-shell-mode))
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
  "Switch between series 5. index machine `py--imenu-create-index' and `py--imenu-create-index-new', which also lists modules variables "
  (interactive)
  (if (eq major-mode 'python-mode)
      (progn
        (if (eq py--imenu-create-index-function 'py--imenu-create-index-new)
            (set (make-local-variable 'py--imenu-create-index-function) 'py--imenu-create-index)
          (set (make-local-variable 'py--imenu-create-index-function) 'py--imenu-create-index-new))
        (when py-menu
          (easy-menu-add py-menu))
        (when py-verbose-p (message "imenu-create-index-function: %s" (prin1-to-string py--imenu-create-index-function)))
        (funcall imenu-create-index-function))
    (error "%s" "Only available in buffers set to python-mode")))

(defun py--imenu-create-index ()
  "Python interface function for the Imenu package.
Finds all Python classes and functions/methods. Calls function
\\[py--imenu-create-index-engine].  See that function for the details
of how this works."
  (setq py-imenu-generic-regexp (car py-imenu-generic-expression)
        py-imenu-generic-parens (if py-imenu-show-method-args-p
                                    py-imenu-method-arg-parens
                                  py-imenu-method-no-arg-parens))
  (goto-char (point-min))
  ;; Warning: When the buffer has no classes or functions, this will
  ;; return nil, which seems proper according to the Imenu API, but
  ;; causes an error in the XEmacs port of Imenu.  Sigh.
  (py--imenu-create-index-engine nil))

(defun py--imenu-create-index-engine (&optional start-indent)
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
the function \\[py--imenu-create-index-function].

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
       ((py--in-literal))
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
        (setq sub-method-alist (py--imenu-create-index-engine cur-indent))
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

(defun py--imenu-create-index-new-intern (&optional thisend end)
  (let* ((pos (match-beginning 0))
         (name (match-string-no-properties 2))
         (classname (concat "class " name))
         (thisend (or thisend (save-match-data (py--end-of-def-or-class-position))))
         sublist)
    (while (and (re-search-forward "^[ \t]*\\(?:\\(def\\|class\\)\\)[ \t]+\\(?:\\(\\sw+\\)\\)" (or thisend end) t 1)(not (nth 8 (syntax-ppss))))
      (let* ((pos (match-beginning 0))
             (name (match-string-no-properties 2))
             (classname (concat "class " name))
             (thisend (or thisend (save-match-data (py--end-of-def-or-class-position)))))
        (if (string= "class" (match-string-no-properties 1))
            (py--imenu-create-index-new-intern (save-match-data (py--end-of-def-or-class-position) end))
          (push (cons (concat " " name) pos) sublist))))
    (if classname
        (progn
          (setq sublist (nreverse sublist))
          (push (cons classname pos) sublist)
          (push (cons classname sublist) index-alist))
      (push sublist index-alist))))

(defun py--imenu-create-index-new (&optional beg end)
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
                  thisend (save-match-data (py--end-of-def-or-class-position))
                  sublist '())
            (while (and (re-search-forward "^[ \t]*\\(def\\|class\\)[ \t]+\\(\\sw+\\)" (or thisend end) t 1)(not (nth 8 (syntax-ppss))))
              (let* ((pos (match-beginning 0))
                     (name (match-string-no-properties 2))
                     (classname (concat "class " name))
                     (thisend (or thisend (save-match-data (py--end-of-def-or-class-position)))))
                (if (string= "class" (match-string-no-properties 1))
                    (py--imenu-create-index-new-intern (save-match-data (py--end-of-def-or-class-position)) end)
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

(defun py--choose-shell-by-import ()
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
                     \"MY-PATH-TO-SHELL\")"
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
                 py-which-bufname (py--choose-buffer-name)
                 msg "CPython"
                 mode-name (py--choose-buffer-name)))
          ((string-match "jython" name)
           (setq py-shell-name name
                 py-which-bufname (py--choose-buffer-name)
                 msg "Jython"
                 mode-name (py--choose-buffer-name)))
          ((string-match "python" name)
           (setq py-shell-name name
                 py-which-bufname (py--choose-buffer-name)
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

(defun py--cleanup-process-name (res)
  "Make res ready for use by `executable-find'

Returns RES or substring of RES"
  (if (string-match "<" res)
      (substring res 0 (match-beginning 0))
    res))

(defalias 'py-toggle-shells 'py-choose-shell)
(defalias 'py-which-shell 'py-choose-shell)
(defun py-choose-shell (&optional arg pyshell py-dedicated-process-p py-edit-only-p)
  "Return an appropriate executable as a string.

Returns nil, if no executable found.

This does the following:
 - look for an interpreter with `py-choose-shell-by-shebang'
 - examine imports using `py--choose-shell-by-import'
 - look if Path/To/File indicates a Python version
 - if not successful, return default value of `py-shell-name'

When interactivly called, messages the shell name, Emacs would in the given circtumstances.

With \\[universal-argument] 4 is called `py-switch-shell' see docu there."
  (interactive "P")
  (if (eq 4 (prefix-numeric-value arg))
      (py-switch-shell '(4))
    (let* (res done
	       (erg (cond (py-force-py-shell-name-p
			   (default-value 'py-shell-name))
			  (py-use-local-default
			   (if (not (string= "" py-shell-local-path))
			       (expand-file-name py-shell-local-path)
			     (message "Abort: `py-use-local-default' is set to `t' but `py-shell-local-path' is empty. Maybe call `py-toggle-local-default-use'")))
			  ((and py-fast-process-p
				(comint-check-proc (current-buffer))
				(string-match "ython" (process-name (get-buffer-process (current-buffer)))))
			   (progn
			     (setq res (process-name (get-buffer-process (current-buffer))))
			     (py--cleanup-process-name res)))
			  ((and (not py-fast-process-p)
				(comint-check-proc (current-buffer))
				(setq done t)
				(string-match "ython" (process-name (get-buffer-process (current-buffer)))))
			   (setq res (process-name (get-buffer-process (current-buffer))))
			   (py--cleanup-process-name res))
			  ((py-choose-shell-by-shebang))
			  ((py--choose-shell-by-import))
			  ((py-choose-shell-by-path))
			  (t (or
			      (default-value 'py-shell-name)
			      "python"))))
	       (cmd (if (or
			 ;; comint-check-proc was succesful
			 done
			 py-edit-only-p) erg
		      (executable-find erg))))
      (if cmd
          (when (interactive-p)
            (message "%s" cmd))
        (when (interactive-p) (message "%s" "Could not detect Python on your system. Maybe set `py-edit-only-p'?")))
      erg)))

;;;

(defun py--normalize-directory (directory)
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
  (let ((py-install-directory (py--normalize-directory py-install-directory)))
    (cond ((and (not (string= "" py-install-directory))(stringp py-install-directory))
           (add-to-list 'load-path (expand-file-name py-install-directory))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "completion"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "extensions"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "test"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "tools"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "autopair")))
          (py-guess-py-install-directory-p
	   (let ((guessed-py-install-directory (py-guess-py-install-directory)))
	     (when guessed-py-install-directory
	       (add-to-list 'load-path guessed-py-install-directory))))
          (t (error "Please set `py-install-directory', see INSTALL"))
          (when (interactive-p) (message "%s" load-path)))))

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
        (define-key map [(control c)(control e)] 'py-help-at-point)
        (define-key map [(control c)(-)] 'py-up-exception)
        (define-key map [(control c)(=)] 'py-down-exception)
        (define-key map [(control x) (n) (d)] 'py-narrow-to-defun)
        ;; information
        (define-key map [(control c)(control b)] 'py-submit-bug-report)
        (define-key map [(control c)(control v)] 'py-version)
        (define-key map [(control c)(control w)] 'py-pychecker-run)
        ;; (define-key map (kbd "TAB") 'py-indent-line)
        (define-key map (kbd "TAB") 'py-indent-or-complete)
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
to change if called with a prefix arg.

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

                   ["python3.3" python3.3
                    :help "`python3.3'
Start an Python3.3 interpreter.

Optional C-u prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'."]

                   ["python3.4" python3.4
                    :help "`python3.3'
Start an Python3.4 interpreter.

Optional C-u prompts for options to pass to the Python3.4 interpreter. See `py-python-command-args'."]

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

                   "-"
                   ("Ignoring defaults "
                    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-window-on-execute'"

                    ["Execute file python switch" py-execute-file-python-switch
                     :help " `py-execute-file-python-switch'
Send file to a Python interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python no-switch" py-execute-file-python-no-switch
                     :help " `py-execute-file-python-no-switch'
Send file to a Python interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python dedicated" py-execute-file-python-dedicated
                     :help " `py-execute-file-python-dedicated'
Send file to a Python interpreter.

Uses a dedicated shell. "]

                    ["Execute file python dedicated switch" py-execute-file-python-dedicated-switch
                     :help " `py-execute-file-python-dedicated-switch'
Send file to a Python interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file ipython switch" py-execute-file-ipython-switch
                     :help " `py-execute-file-ipython-switch'
Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file ipython no-switch" py-execute-file-ipython-no-switch
                     :help " `py-execute-file-ipython-no-switch'
Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file ipython dedicated" py-execute-file-ipython-dedicated
                     :help " `py-execute-file-ipython-dedicated'
Send file to a Ipython interpreter.

Uses a dedicated shell. "]

                    ["Execute file ipython dedicated switch" py-execute-file-ipython-dedicated-switch
                     :help " `py-execute-file-ipython-dedicated-switch'
Send file to a Ipython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3 switch" py-execute-file-python3-switch
                     :help " `py-execute-file-python3-switch'
Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3 no-switch" py-execute-file-python3-no-switch
                     :help " `py-execute-file-python3-no-switch'
Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python3 dedicated" py-execute-file-python3-dedicated
                     :help " `py-execute-file-python3-dedicated'
Send file to a Python3 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python3 dedicated switch" py-execute-file-python3-dedicated-switch
                     :help " `py-execute-file-python3-dedicated-switch'
Send file to a Python3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2 switch" py-execute-file-python2-switch
                     :help " `py-execute-file-python2-switch'
Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2 no-switch" py-execute-file-python2-no-switch
                     :help " `py-execute-file-python2-no-switch'
Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python2 dedicated" py-execute-file-python2-dedicated
                     :help " `py-execute-file-python2-dedicated'
Send file to a Python2 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python2 dedicated switch" py-execute-file-python2-dedicated-switch
                     :help " `py-execute-file-python2-dedicated-switch'
Send file to a Python2 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2.7 switch" py-execute-file-python2.7-switch
                     :help " `py-execute-file-python2.7-switch'
Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2.7 no-switch" py-execute-file-python2.7-no-switch
                     :help " `py-execute-file-python2.7-no-switch'
Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python2.7 dedicated" py-execute-file-python2.7-dedicated
                     :help " `py-execute-file-python2.7-dedicated'
Send file to a Python2.7 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python2.7 dedicated switch" py-execute-file-python2.7-dedicated-switch
                     :help " `py-execute-file-python2.7-dedicated-switch'
Send file to a Python2.7 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file jython switch" py-execute-file-jython-switch
                     :help " `py-execute-file-jython-switch'
Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file jython no-switch" py-execute-file-jython-no-switch
                     :help " `py-execute-file-jython-no-switch'
Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file jython dedicated" py-execute-file-jython-dedicated
                     :help " `py-execute-file-jython-dedicated'
Send file to a Jython interpreter.

Uses a dedicated shell. "]

                    ["Execute file jython dedicated switch" py-execute-file-jython-dedicated-switch
                     :help " `py-execute-file-jython-dedicated-switch'
Send file to a Jython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.3 switch" py-execute-file-python3.3-switch
                     :help " `py-execute-file-python3.3-switch'
Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.3 no-switch" py-execute-file-python3.3-no-switch
                     :help " `py-execute-file-python3.3-no-switch'
Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python3.3 dedicated" py-execute-file-python3.3-dedicated
                     :help " `py-execute-file-python3.3-dedicated'
Send file to a Python3.3 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python3.3 dedicated switch" py-execute-file-python3.3-dedicated-switch
                     :help " `py-execute-file-python3.3-dedicated-switch'
Send file to a Python3.3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file bpython switch" py-execute-file-bpython-switch
                     :help " `py-execute-file-bpython-switch'
Send file to a Bpython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file bpython no-switch" py-execute-file-bpython-no-switch
                     :help " `py-execute-file-bpython-no-switch'
Send file to a Bpython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file bpython dedicated" py-execute-file-bpython-dedicated
                     :help " `py-execute-file-bpython-dedicated'
Send file to a Bpython interpreter.

Uses a dedicated shell. "]

                    ["Execute file bpython dedicated switch" py-execute-file-bpython-dedicated-switch
                     :help " `py-execute-file-bpython-dedicated-switch'
Send file to a Bpython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]))
                  "-"

		  ["Toggle shell" py-toggle-shell
		   :help " `py-toggle-shell'

Toggles between the interpreter customized in `py-shell-toggle-1' resp\. `py-shell-toggle-2'\. Was hard-coded CPython and Jython in earlier versions, now starts with Python2 and Python3 by default\.

ARG might be a python-version string to set to\.

C-u `py-toggle-shell' prompts to specify a reachable Python command\.
C-u followed by numerical arg 2 or 3, `py-toggle-shell' opens a respective Python shell\.
C-u followed by numerical arg 5 opens a Jython shell\.

Should you need more shells to select, extend this command by adding inside the first cond:

                    ((eq NUMBER (prefix-numeric-value arg))
                     "MY-PATH-TO-SHELL")"]

                  ["Kill shell unconditional" py-kill-shell-unconditional
                   :help " `py-kill-shell-unconditional'

With optional argument SHELL\.

Otherwise kill default (I)Python shell\.
Kill buffer and its process.
Receives a buffer-name as argument "]

                  ["Kill default shell unconditional" py-kill-default-shell-unconditional
                   :help " `py-kill-default-shell-unconditional'

Kill buffer \"*Python*\" and its process\.  "])

                 "-"
                 ("Mark"

                  ["Mark block" py-mark-block
                   :help " `py-mark-block'

Mark block at point.

Returns beginning and end positions of marked area, a cons. "]

                  ["Mark minor block" py-mark-minor-block
                   :help " `py-mark-minor-block'

Mark minor-block at point.

A minor block is started by a `for', `if', `try' or `with'.
Returns beginning and end positions of marked area, a cons. "]

                  ["Mark def or class" py-mark-def-or-class
                   :help " `py-mark-def-or-class'

Mark def-or-class at point.

Returns beginning and end positions of marked area, a cons. "]

                  ["Mark statement" py-mark-statement
                   :help "`py-mark-statement'
Mark statement at point"]

                  ["Mark top level" py-mark-top-level
                   :help " `py-mark-top-level'

Mark top-level form at point.

Returns beginning and end positions of marked area, a cons. "]

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

                  ("BOL forms"

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

Mark minor-block at point reaching beginning-of-line.
A minor block is started by a `for', `if', `try' or `with'."]))

		 "-"

                 ["Shift region left" py-shift-region-left
                  :help " `py-shift-region-left'

Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. "]

                 ["Shift region right" py-shift-region-right
                  :help " `py-shift-region-right'

Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. "]

                 "-"

                 ("Comment"
                  ["Comment Region"   py-comment-region (point) (mark)
                   :help "Like `comment-region' but uses double hash (`#') comment starter." ]
                  ["Uncomment" py-uncomment
                   :help " `py-uncomment'

Uncomment commented lines at point.

If region is active, restrict uncommenting at region . "]

                  ["Uncomment Region"     (py-comment-region (point) (mark) '(4))
                   :help "(py-comment-region (point) (mark) '(4))" ]
                  "-"
                  ["Comment block" py-comment-block
                   :help " `py-comment-block'
Comments block at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment minor-block" py-comment-minor-block
                   :help " `py-comment-minor-block'
Comments minor-block at point.

A minor block is started by a `for', `if', `try' or `with'.
Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment top level" py-comment-top-level
                   :help " `py-comment-top-level'

Comments top-level form at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment clause" py-comment-clause
                   :help " `py-comment-clause'
Comments clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment block or clause" py-comment-block-or-clause
                   :help " `py-comment-block-or-clause'
Comments block-or-clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment def" py-comment-def
                   :help " `py-comment-def'
Comments def at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment class" py-comment-class
                   :help " `py-comment-class'
Comments class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment def or class" py-comment-def-or-class
                   :help " `py-comment-def-or-class'
Comments def-or-class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment statement" py-comment-statement
                   :help " `py-comment-statement'
Comments statement at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                 "-"

                 ("Move"

		  ["Backward same level" py-backward-same-level
		   :help " `py-backward-same-level'

Go form backward keeping indent level if possible\.

If inside a delimited form --string or list-- go to its beginning\.
If not at beginning of a statement or block, go to its beginning\.
If at beginning of a statement or block, go to previous beginning of compound statement or definition at point\.
If no further element at same level, go one level up. "]

                  ["Beginning of block" py-beginning-of-block
                   :help " `py-beginning-of-block'

Go to beginning block, skip whitespace at BOL. "]

                  ["Go to end of block" py-end-of-block]

                  "-"

                  ["Beginning of def or class" py-beginning-of-def-or-class
                   :help " `py-beginning-of-def-or-class'

Go to beginning def-or-class, skip whitespace at BOL.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. "]

                  ["End of def or class" py-end-of-def-or-class
                   :help " `py-end-of-def-or-class'

Go to end of def-or-class.

Returns end of def-or-class if successful, nil otherwise

With M-x universal argument or `py-mark-decorators' set to `t', decorators are marked too. "]

                  "-"

                  ["Beginning of statement" py-beginning-of-statement
                   :help " `py-beginning-of-statement'

Go to the initial line of a simple statement. "]

                  ["End of statement" py-end-of-statement
                   :help " `py-end-of-statement'

Go to the last char of current statement.

To go just beyond the final line of the current statement, use `py-down-statement-bol'. "]

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

                  ("BOL forms"
                   ("Beginning"

                    ["Beginning of block bol" py-beginning-of-block-bol
                     :help " `py-beginning-of-block-bol'

Go to beginning block, go to beginning-of-line\.

Returns beginning of block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of clause bol" py-beginning-of-clause-bol
                     :help " `py-beginning-of-clause-bol'

Go to beginning clause, go to beginning-of-line\.

Returns beginning of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of block or clause bol" py-beginning-of-block-or-clause-bol
                     :help " `py-beginning-of-block-or-clause-bol'

Go to beginning block-or-clause, go to beginning-of-line\.

Returns beginning of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of def bol" py-beginning-of-def-bol
                     :help " `py-beginning-of-def-bol'

Go to beginning def, go to beginning-of-line\.

Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too\.

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of class bol" py-beginning-of-class-bol
                     :help " `py-beginning-of-class-bol'

Go to beginning class, go to beginning-of-line\.

Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too\.

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of def or class bol" py-beginning-of-def-or-class-bol
                     :help " `py-beginning-of-def-or-class-bol'

Go to beginning def-or-class, go to beginning-of-line\.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too\.

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of if block bol" py-beginning-of-if-block-bol
                     :help " `py-beginning-of-if-block-bol'

Go to beginning if-block, go to beginning-of-line\.

Returns beginning of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of try block bol" py-beginning-of-try-block-bol
                     :help " `py-beginning-of-try-block-bol'

Go to beginning try-block, go to beginning-of-line\.

Returns beginning of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of minor block bol" py-beginning-of-minor-block-bol
                     :help " `py-beginning-of-minor-block-bol'

Go to beginning minor-block, go to beginning-of-line\.

Returns beginning of minor-block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of statement bol" py-beginning-of-statement-bol
                     :help " `py-beginning-of-statement-bol'

Goto beginning of line where statement starts\.
  Returns position reached, if successful, nil otherwise\.

See also `py-up-statement': up from current definition to next beginning of statement above\.  "])
                   ("End"

                    ["End of block bol" py-end-of-block-bol
                     :help " `py-end-of-block-bol'

Goto beginning of line following end of block\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-block': down from current definition to next beginning of block below\.  "]

                    ["End of clause bol" py-end-of-clause-bol
                     :help " `py-end-of-clause-bol'

Goto beginning of line following end of clause\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-clause': down from current definition to next beginning of clause below\.  "]

                    ["End of block or clause bol" py-end-of-block-or-clause-bol
                     :help " `py-end-of-block-or-clause-bol'

Goto beginning of line following end of block-or-clause\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-block-or-clause': down from current definition to next beginning of block-or-clause below\.  "]

                    ["End of def bol" py-end-of-def-bol
                     :help " `py-end-of-def-bol'

Goto beginning of line following end of def\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-def': down from current definition to next beginning of def below\.  "]

                    ["End of class bol" py-end-of-class-bol
                     :help " `py-end-of-class-bol'

Goto beginning of line following end of class\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-class': down from current definition to next beginning of class below\.  "]

                    ["End of def or class bol" py-end-of-def-or-class-bol
                     :help " `py-end-of-def-or-class-bol'

Goto beginning of line following end of def-or-class\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-def-or-class': down from current definition to next beginning of def-or-class below\.  "]

                    ["End of if block bol" py-end-of-if-block-bol
                     :help " `py-end-of-if-block-bol'

 "]

                    ["End of try block bol" py-end-of-try-block-bol
                     :help " `py-end-of-try-block-bol'

 "]

                    ["End of minor block bol" py-end-of-minor-block-bol
                     :help " `py-end-of-minor-block-bol'

 "]

                    ["End of statement bol" py-end-of-statement-bol
                     :help " `py-end-of-statement-bol'

Goto beginning of line following end of statement\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-statement': down from current definition to next beginning of statement below\.  "]))

                  "-"

                  ("More"

                   ["Up level" py-up
                    :help " `py-up'
Go to beginning one level above of compound statement or definition at point. "]

                   ["Down level" py-down
                    :help " `py-down'
Go to beginning one level below of compound statement or definition at point. "]

                   "-"

                   ["Beginning of top level" py-beginning-of-top-level
                    :help " `py-beginning-of-top-level'

Go to the very beginning of top-level form at point. "]

                   ["End of top level" py-end-of-top-level
                    :help " `py-end-of-top-level'

Go to end of top-level form at point. "]

                   "-"

                   ["Beginning of block current-column" py-beginning-of-block-current-column
                    :help " `py-beginning-of-block-current-column'

Reach next beginning of block upwards which starts at current column.

Return position. "]

                   "-"

                   ["Move to start of def" py-beginning-of-def t]

                   ["Move to end of def"   py-end-of-def t]

                   "-"

                   ["Beginning of clause" py-beginning-of-clause
                    :help " `py-beginning-of-clause'

Go to beginning clause, skip whitespace at BOL. "]

                   ["End of clause" py-end-of-clause
                    :help " `py-end-of-clause'

Go to end of clause. "]

                   "-"

                   ["Beginning of comment" py-beginning-of-comment
                    :help " `py-beginning-of-comment'
Go to beginning of comment at point. "]

                   ["End of comment" py-end-of-comment
                    :help " `py-end-of-comment'

Go to end of comment at point. "]

                   "-"

                   ["Go to start of expression" (py-beginning-of-expression t) t]
                   ["Move to end of expression" (py-end-of-expression t) t]

                   "-"

                   ["Go to start of minor-expression" (py-beginning-of-minor-expression t) t]

                   ["Move to end of minor-expression" (py-end-of-minor-expression t) t]
                   "-"

                   ["Beginning of minor block" py-beginning-of-minor-block
                    :help " `py-beginning-of-minor-block'

Go to beginning minor-block, skip whitespace at BOL.

Returns beginning of minor-block if successful, nil otherwise

A minor block is started by a `for', `if', `try' or `with'.

"]

                   ["End of minor block" py-end-of-minor-block
                    :help " `py-end-of-minor-block'

Go to end of minor-block.

Returns end of minor-block if successful, nil otherwise

A minor block is started by a `for', `if', `try' or `with'. "]))

                 "-"

                 ("Copy "
                  ["Copy statement" py-copy-statement
                   :help "`py-copy-statement'
Copy statement at point"]

                  ["Copy top level" py-copy-top-level
                   :help " `py-copy-top-level'

Copy top-level form at point. "]

                  ["Copy clause" py-copy-clause
                   :help "`py-copy-clause'
Copy innermost clause at point"]

                  ["Copy block" py-copy-block
                   :help "`py-copy-block'
Copy innermost block at point"]

                  ["Copy minor block" py-copy-minor-block
                   :help " `py-copy-minor-block'

Copy minor-block at point.

Store data in kill ring, so it might yanked back.
A minor block is started by a `for', `if', `try' or `with'. "]

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
Copy innermost definition at point"]

                  ("BOL forms"

                   ["Copy block bol" py-copy-block-bol
                    :help " `py-copy-block-bol'

Delete block, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy clause bol" py-copy-clause-bol
                    :help " `py-copy-clause-bol'

Delete clause, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy block or clause bol" py-copy-block-or-clause-bol
                    :help " `py-copy-block-or-clause-bol'

Delete block-or-clause, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy def bol" py-copy-def-bol
                    :help " `py-copy-def-bol'

Delete def, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy class bol" py-copy-class-bol
                    :help " `py-copy-class-bol'

Delete class, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy def or class bol" py-copy-def-or-class-bol
                    :help " `py-copy-def-or-class-bol'

Delete def-or-class, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy statement bol" py-copy-statement-bol
                    :help " `py-copy-statement-bol'

Delete statement, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy minor block bol" py-copy-minor-block-bol
                    :help " `py-copy-minor-block-bol'

Delete block, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.

See `py-minor-block-re' "]))

		 ("Hide-Show"

		  ["Hide region" py-hide-region
		   :help " `py-hide-region'

Hide active region\. "]

		  ["Hide statement" py-hide-statement
		   :help " `py-hide-statement'

Hide statement at point\. "]

		  ["Hide block" py-hide-block
		   :help " `py-hide-block'

Hide block at point\. "]

		  ["Hide clause" py-hide-clause
		   :help " `py-hide-clause'

Hide clause at point\. "]

		  ["Hide block or clause" py-hide-block-or-clause
		   :help " `py-hide-block-or-clause'

Hide block-or-clause at point\. "]

		  ["Hide def" py-hide-def
		   :help " `py-hide-def'

Hide def at point\. "]

		  ["Hide class" py-hide-class
		   :help " `py-hide-class'

Hide class at point\. "]

		  ["Hide expression" py-hide-expression
		   :help " `py-hide-expression'

Hide expression at point\. "]

		  ["Hide partial expression" py-hide-partial-expression
		   :help " `py-hide-partial-expression'

Hide partial-expression at point\. "]

		  ["Hide line" py-hide-line
		   :help " `py-hide-line'

Hide line at point\. "]

		  ["Hide top level" py-hide-top-level
		   :help " `py-hide-top-level'

Hide top-level at point\. "]

		  ("Show"

		   ["Show region" py-show-region
		    :help " `py-show-region'

Un-hide active region\. "]

		   ["Show statement" py-show-statement
		    :help " `py-show-statement'

Show statement at point\. "]

		   ["Show block" py-show-block
		    :help " `py-show-block'

Show block at point\. "]

		   ["Show clause" py-show-clause
		    :help " `py-show-clause'

Show clause at point\. "]

		   ["Show block or clause" py-show-block-or-clause
		    :help " `py-show-block-or-clause'

Show block-or-clause at point\. "]

		   ["Show def" py-show-def
		    :help " `py-show-def'

Show def at point\. "]

		   ["Show class" py-show-class
		    :help " `py-show-class'

Show class at point\. "]

		   ["Show expression" py-show-expression
		    :help " `py-show-expression'

Show expression at point\. "]

		   ["Show partial expression" py-show-partial-expression
		    :help " `py-show-partial-expression'

Show partial-expression at point\. "]

		   ["Show line" py-show-line
		    :help " `py-show-line'

Show line at point\. "]

		   ["Show top level" py-show-top-level
		    :help " `py-show-top-level'

Show top-level at point\. "]))

                 "-"

                 ["Execute region" py-execute-region
                  :help " `py-execute-region'

Send the region to a Python interpreter.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment. "]

                 ["Execute buffer" py-execute-buffer
                  :help " `py-execute-buffer'

Send the contents of the buffer to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file."]

                 ["Execute def or class" py-execute-def-or-class
                  :help " `py-execute-def-or-class'

Send def-or-class at point to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
See also `py-force-py-shell-name-p'.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment."]

                 ["Execute statement" py-execute-statement
                  :help " `py-execute-statement'

Send statement at point to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
See also `py-force-py-shell-name-p'.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment."]

                 ["Execute string" py-execute-string
                  :help " `py-execute-string'

Send the argument STRING to a Python interpreter.

See also `py-execute-region'. "]

                 ["Execute line" py-execute-line
                  :help " `py-execute-line'

Send current line from beginning of indent to Python interpreter\.  "]

                 ("More... "
                  :help "Python-specific features"

                  ["Execute top level" py-execute-top-level
                   :help " `py-execute-top-level'

Send top-level form at point to a Python interpreter. "]

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

                   ["py-execute-statement-python3.3" py-execute-statement-python3.3
                    :help "Execute statement through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-statement-bpython" py-execute-statement-bpython
                    :help "Execute statement through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ("Dedicated"

		    ["Execute statement dedicated" py-execute-statement-dedicated
		     :help " `py-execute-statement-dedicated'

Send statement to unique interpreter\. "]

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

		    ["py-execute-statement-python3.3-dedicated" py-execute-statement-python3.3-dedicated
		     :help "Execute statement through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-statement-bpython-dedicated" py-execute-statement-bpython-dedicated
		     :help "Execute statement through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]
		    )

		   ("Ignoring defaults "
		    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-window-on-execute'"            ;; switch

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

                   ["py-execute-block-python3.3" py-execute-block-python3.3
                    :help "Execute block through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-block-bpython" py-execute-block-bpython
                    :help "Execute block through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ("Dedicated"

		    ["Execute block dedicated" py-execute-block-dedicated
		     :help " `py-execute-block-dedicated'

Send block to unique interpreter\. "]

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

		    ["py-execute-block-python3.3-dedicated" py-execute-block-python3.3-dedicated
		     :help "Execute block through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-block-bpython-dedicated" py-execute-block-bpython-dedicated
		     :help "Execute block through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]
		    )

                   ("Ignoring defaults "
                    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-window-on-execute'"            ;; switch

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

                   ["py-execute-def-python3.3" py-execute-def-python3.3
                    :help "Execute def through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-def-bpython" py-execute-def-bpython
                    :help "Execute def through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ("Dedicated"

		    ["Execute def dedicated" py-execute-def-dedicated
		     :help " `py-execute-def-dedicated'

Send def to unique interpreter\. "]

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

		    ["py-execute-def-python3.3-dedicated" py-execute-def-python3.3-dedicated
		     :help "Execute def through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-def-bpython-dedicated" py-execute-def-bpython-dedicated
		     :help "Execute def through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]
		   )

                   ("Ignoring defaults "
                    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-window-on-execute'"            ;; switch

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

		   ["Execute class" py-execute-class
		    :help " `py-execute-class'

Send class at point to a Python interpreter\. "]

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

                   ["py-execute-class-python3.3" py-execute-class-python3.3
                    :help "Execute class through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-class-bpython" py-execute-class-bpython
                    :help "Execute class through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ("Dedicated"

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

		    ["py-execute-class-python3.3-dedicated" py-execute-class-python3.3-dedicated
		     :help "Execute class through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-class-bpython-dedicated" py-execute-class-bpython-dedicated
		     :help "Execute class through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]
		    )

		   ("Ignoring defaults "
                    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-window-on-execute'"            ;; switch

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

                   ["py-execute-region-python3.3" py-execute-region-python3.3
                    :help "Execute region through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-region-bpython" py-execute-region-bpython
                    :help "Execute region through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ("Dedicated"

		    ["Execute region dedicated" py-execute-region-dedicated
		     :help " `py-execute-region-dedicated'

Send region to unique interpreter\. "]

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

		    ["py-execute-region-python3.3-dedicated" py-execute-region-python3.3-dedicated
		     :help "Execute region through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-region-bpython-dedicated" py-execute-region-bpython-dedicated
		     :help "Execute region through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]
		    )

		   ("Ignoring defaults "
		    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-window-on-execute'"            ;; switch

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

                  ["Execute file" py-execute-file
                   :help "`py-execute-file'
       Send file at point to Python interpreter. "]

                   ["Execute file python" py-execute-file-python
                    :help " `py-execute-file-python'
Send file to a Python interpreter. "]

                   ["Execute file ipython" py-execute-file-ipython
                    :help " `py-execute-file-ipython'
Send file to a Ipython interpreter. "]

                   ["Execute file python3" py-execute-file-python3
                    :help " `py-execute-file-python3'
Send file to a Python3 interpreter. "]

                   ["Execute file python2" py-execute-file-python2
                    :help " `py-execute-file-python2'
Send file to a Python2 interpreter. "]

                   ["Execute file python2.7" py-execute-file-python2.7
                    :help " `py-execute-file-python2.7'
Send file to a Python2.7 interpreter. "]

                   ["Execute file jython" py-execute-file-jython
                    :help " `py-execute-file-jython'
Send file to a Jython interpreter. "]

                   ["Execute file python3.3" py-execute-file-python3.3
                    :help " `py-execute-file-python3.3'
Send file to a Python3.3 interpreter. "]

                   ["Execute file bpython" py-execute-file-bpython
                    :help " `py-execute-file-bpython'
Send file to a Bpython interpreter. "]

		   ("Dedicated"
		    ["Execute file dedicated" py-execute-file-dedicated
		     :help " `py-execute-file-dedicated'

"])

                   ("Ignoring defaults "
                    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-window-on-execute'"

                    ["Execute file python switch" py-execute-file-python-switch
                     :help " `py-execute-file-python-switch'
Send file to a Python interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python no-switch" py-execute-file-python-no-switch
                     :help " `py-execute-file-python-no-switch'
Send file to a Python interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python dedicated" py-execute-file-python-dedicated
                     :help " `py-execute-file-python-dedicated'
Send file to a Python interpreter.

Uses a dedicated shell. "]

                    ["Execute file python dedicated switch" py-execute-file-python-dedicated-switch
                     :help " `py-execute-file-python-dedicated-switch'
Send file to a Python interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file ipython switch" py-execute-file-ipython-switch
                     :help " `py-execute-file-ipython-switch'
Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file ipython no-switch" py-execute-file-ipython-no-switch
                     :help " `py-execute-file-ipython-no-switch'
Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file ipython dedicated" py-execute-file-ipython-dedicated
                     :help " `py-execute-file-ipython-dedicated'
Send file to a Ipython interpreter.

Uses a dedicated shell. "]

                    ["Execute file ipython dedicated switch" py-execute-file-ipython-dedicated-switch
                     :help " `py-execute-file-ipython-dedicated-switch'
Send file to a Ipython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3 switch" py-execute-file-python3-switch
                     :help " `py-execute-file-python3-switch'
Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3 no-switch" py-execute-file-python3-no-switch
                     :help " `py-execute-file-python3-no-switch'
Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python3 dedicated" py-execute-file-python3-dedicated
                     :help " `py-execute-file-python3-dedicated'
Send file to a Python3 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python3 dedicated switch" py-execute-file-python3-dedicated-switch
                     :help " `py-execute-file-python3-dedicated-switch'
Send file to a Python3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2 switch" py-execute-file-python2-switch
                     :help " `py-execute-file-python2-switch'
Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2 no-switch" py-execute-file-python2-no-switch
                     :help " `py-execute-file-python2-no-switch'
Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python2 dedicated" py-execute-file-python2-dedicated
                     :help " `py-execute-file-python2-dedicated'
Send file to a Python2 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python2 dedicated switch" py-execute-file-python2-dedicated-switch
                     :help " `py-execute-file-python2-dedicated-switch'
Send file to a Python2 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2.7 switch" py-execute-file-python2.7-switch
                     :help " `py-execute-file-python2.7-switch'
Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2.7 no-switch" py-execute-file-python2.7-no-switch
                     :help " `py-execute-file-python2.7-no-switch'
Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python2.7 dedicated" py-execute-file-python2.7-dedicated
                     :help " `py-execute-file-python2.7-dedicated'
Send file to a Python2.7 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python2.7 dedicated switch" py-execute-file-python2.7-dedicated-switch
                     :help " `py-execute-file-python2.7-dedicated-switch'
Send file to a Python2.7 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file jython switch" py-execute-file-jython-switch
                     :help " `py-execute-file-jython-switch'
Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file jython no-switch" py-execute-file-jython-no-switch
                     :help " `py-execute-file-jython-no-switch'
Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file jython dedicated" py-execute-file-jython-dedicated
                     :help " `py-execute-file-jython-dedicated'
Send file to a Jython interpreter.

Uses a dedicated shell. "]

                    ["Execute file jython dedicated switch" py-execute-file-jython-dedicated-switch
                     :help " `py-execute-file-jython-dedicated-switch'
Send file to a Jython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.3 switch" py-execute-file-python3.3-switch
                     :help " `py-execute-file-python3.3-switch'
Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.3 no-switch" py-execute-file-python3.3-no-switch
                     :help " `py-execute-file-python3.3-no-switch'
Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python3.3 dedicated" py-execute-file-python3.3-dedicated
                     :help " `py-execute-file-python3.3-dedicated'
Send file to a Python3.3 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python3.3 dedicated switch" py-execute-file-python3.3-dedicated-switch
                     :help " `py-execute-file-python3.3-dedicated-switch'
Send file to a Python3.3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file bpython switch" py-execute-file-bpython-switch
                     :help " `py-execute-file-bpython-switch'
Send file to a Bpython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file bpython no-switch" py-execute-file-bpython-no-switch
                     :help " `py-execute-file-bpython-no-switch'
Send file to a Bpython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file bpython dedicated" py-execute-file-bpython-dedicated
                     :help " `py-execute-file-bpython-dedicated'
Send file to a Bpython interpreter.

Uses a dedicated shell. "]

                    ["Execute file bpython dedicated switch" py-execute-file-bpython-dedicated-switch
                     :help " `py-execute-file-bpython-dedicated-switch'
Send file to a Bpython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]))

                  ["Execute minor block" py-execute-minor-block
                   :help " `py-execute-minor-block'

Send minor-block at point to a Python interpreter.

A minor block is started by a `for', `if', `try' or `with'.
. "]

                  ["Execute def" py-execute-def
                   :help "`py-execute-def'
       Send def at point to Python interpreter. "]

                  ["Execute class" py-execute-class
                   :help "`py-execute-class'
       Send class at point to Python interpreter. "]

		  )

                 ("Fast process..."

                  ["Fast send string" py--fast-send-string
                   :help " `py--fast-send-string'

Process Python strings, being prepared for large output\.

Output buffer displays \"Fast\" in name by default
See also `py-fast-shell'"]

                  ["Process region fast" py-process-region-fast
                   :help " `py-process-region-fast'

 "]

                  ["Execute statement fast" py-execute-statement-fast
                   :help " `py-execute-statement-fast'

Process statement at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute block fast" py-execute-block-fast
                   :help " `py-execute-block-fast'

Process block at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute block or clause fast" py-execute-block-or-clause-fast
                   :help " `py-execute-block-or-clause-fast'

Process block-or-clause at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute def fast" py-execute-def-fast
                   :help " `py-execute-def-fast'

Process def at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute class fast" py-execute-class-fast
                   :help " `py-execute-class-fast'

Process class at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute def or class fast" py-execute-def-or-class-fast
                   :help " `py-execute-def-or-class-fast'

Process def-or-class at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute expression fast" py-execute-expression-fast
                   :help " `py-execute-expression-fast'

Process expression at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute partial expression fast" py-execute-partial-expression-fast
                   :help " `py-execute-partial-expression-fast'

Process partial-expression at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute top level fast" py-execute-top-level-fast
                   :help " `py-execute-top-level-fast'

Process top-level at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute clause fast" py-execute-clause-fast
                   :help " `py-execute-clause-fast'

Process clause at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Result arrives in output-buffer, which is not in comint-mode "])

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

Check if a directory is a virtualenv. "])

                 ["Execute import or reload" py-execute-import-or-reload
                  :help " `py-execute-import-or-reload'

Import the current buffer's file in a Python interpreter.

If the file has already been imported, then do reload instead to get
the latest version.

If the file's name does not end in ".py", then do execfile instead.

If the current buffer is not visiting a file, do `py-execute-buffer'
instead.

If the file local variable `py-master-file' is non-nil, import or
reload the named file instead of the buffer's file.  The file may be
saved based on the value of `py-execute-import-or-reload-save-p'.

See also `M-x py-execute-region'.

This may be preferable to `M-x py-execute-buffer' because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE.NAME).

 - The Python debugger gets line number information about the functions. "]

                 ("Help"

		  ["Find definition" py-find-definition
		   :help " `py-find-definition'

Find source of definition of SYMBOL\.

Interactively, prompt for SYMBOL\."]

		  ["Imenu" imenu
		   :help " `imenu'

Jump to a INDEX-ITEM "]

                  ["Info lookup symbol" py-info-lookup-symbol
                   :help " `py-info-lookup-symbol'

Calls `info-lookup-symbol'.

Sends help if stuff is missing. "]

                  ["Symbol at point" py-symbol-at-point
                   :help " `py-symbol-at-point'

Return the current Python symbol\. "]

		  "-"

                  ["Describe mode"        py-describe-mode t]

                  ["Help on symbol" py-help-at-point
                   :help "`py-help-at-point'\n
Use pydoc on symbol at point"]

		  )

                 ("Debugger"

                  ["pdb" pdb
		   :help "`pdb' Run pdb under GUD"]

		  ["Execute statement pdb" py-execute-statement-pdb
		   :help " `py-execute-statement-pdb'

Execute statement running pdb\. . "])
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

                  ("Flake8 " :help
                   "code checker running "

                   ["Flake8 run" py-flake8-run
                    :help " `py-flake8-run'

        Flake8 is a wrapper around these tools:
        - PyFlakes
        - pep8
        - Ned Batchelder's McCabe script

        It also adds features:
        - files that contain this line are skipped::
            # flake8: noqa
        - lines that contain a ``# noqa`` comment at the end will not issue warnings.
        - a Git and a Mercurial hook.
        - a McCabe complexity checker.
        - extendable through ``flake8.extension`` entry points.

. "]

                   ["Flake8 help" py-flake8-help
                    :help " `py-flake8-help'

Display flake8 command line help messages. "])

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
Toggle flymake-mode running `pyflakespep8' "]))

                 ("Customize"

                  ["Python-mode customize group" (customize-group 'python-mode)
                   :help "Open the customization buffer for Python mode"]
                  ("Switches"
                   :help "Toggle useful modes like `highlight-indentation'"
                   ("Interpreter"

                    ["Shell prompt read only"
                     (setq py-shell-prompt-read-only
                           (not py-shell-prompt-read-only))
                     :help "If non-nil, the python prompt is read only.  Setting this variable will only effect new shells.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-shell-prompt-read-only]

                    ["Remove cwd from path"
                     (setq py-remove-cwd-from-path
                           (not py-remove-cwd-from-path))
                     :help "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
a Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion).Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-remove-cwd-from-path]

                    ["Honor IPYTHONDIR "
                     (setq py-honor-IPYTHONDIR-p
                           (not py-honor-IPYTHONDIR-p))
                     :help "When non-nil ipython-history file is constructed by \$IPYTHONDIR
followed by "/history". Default is nil.

Otherwise value of py-ipython-history is used. Use `M-x customize-variable' to set it permanently"
:style toggle :selected py-honor-IPYTHONDIR-p]

                    ["Honor PYTHONHISTORY "
                     (setq py-honor-PYTHONHISTORY-p
                           (not py-honor-PYTHONHISTORY-p))
                     :help "When non-nil python-history file is set by \$PYTHONHISTORY
Default is nil.

Otherwise value of py-python-history is used. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-honor-PYTHONHISTORY-p]

                    ["Enforce py-shell-name" force-py-shell-name-p-on
                     :help "Enforce customized default `py-shell-name' should upon execution. "]

                    ["Don't enforce default interpreter" force-py-shell-name-p-off
                     :help "Make execute commands guess interpreter from environment"]

                    ["Enforce local Python shell " py-force-local-shell-on
                     :help "Locally indicated Python being enforced upon sessions execute commands. "]

                    ["Remove local Python shell enforcement, restore default" py-force-local-shell-off
                     :help "Restore `py-shell-name' default value and `behaviour'. "])

                   ("Execute"

		    ["Fast process" py-fast-process-p
		     :help " `py-fast-process-p'

Use `py-fast-process'\.

Commands prefixed \"py-fast-...\" suitable for large output

See: large output makes Emacs freeze, lp:1253907

Output-buffer is not in comint-mode"
		     :style toggle :selected py-fast-process-p]

		    ["Python mode v5 behavior"
                     (setq python-mode-v5-behavior-p
                           (not python-mode-v5-behavior-p))
                     :help "Execute region through `shell-command-on-region' as
v5 did it - lp:990079. This might fail with certain chars - see UnicodeEncodeError lp:550661

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected python-mode-v5-behavior-p]

                    ["Force shell name "
                     (setq py-force-py-shell-name-p
                           (not py-force-py-shell-name-p))
                     :help "When `t', execution with kind of Python specified in `py-shell-name' is enforced, possibly shebang doesn't take precedence. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-force-py-shell-name-p]

                    ["Execute \"if name == main\" blocks p"
                     (setq py-if-name-main-permission-p
                           (not py-if-name-main-permission-p))
                     :help " `py-if-name-main-permission-p'

Allow execution of code inside blocks delimited by
if __name__ == '__main__'

Default is non-nil. "
                     :style toggle :selected py-if-name-main-permission-p]

                    ["Ask about save"
                     (setq py-ask-about-save
                           (not py-ask-about-save))
                     :help "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-ask-about-save]

                    ["Store result"
                     (setq py-store-result-p
                           (not py-store-result-p))
                     :help " `py-store-result-p'

When non-nil, put resulting string of `py-execute-...' into kill-ring, so it might be yanked. "
                     :style toggle :selected py-store-result-p]

                    ["Prompt on changed "
                     (setq py-prompt-on-changed-p
                           (not py-prompt-on-changed-p))
                     :help "When called interactively, ask for save before a changed buffer is sent to interpreter.

Default is `t'Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-prompt-on-changed-p]

                    ["Dedicated process "
                     (setq py-dedicated-process-p
                           (not py-dedicated-process-p))
                     :help "If commands executing code use a dedicated shell.

Default is nilUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-dedicated-process-p]

                    ["Execute without temporary file"
                     (setq py-execute-no-temp-p
                           (not py-execute-no-temp-p))
                     :help " `py-execute-no-temp-p'
Seems Emacs-24.3 provided a way executing stuff without temporary files.
In experimental state yet "
                     :style toggle :selected py-execute-no-temp-p]

                    ["Warn tmp files left "
                     (setq py--warn-tmp-files-left-p
                           (not py--warn-tmp-files-left-p))
                     :help "Messages a warning, when `py-temp-directory' contains files susceptible being left by previous Python-mode sessions. See also lp:987534 Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py--warn-tmp-files-left-p])

                   ("Edit"

                    ("Completion"

                     ["No completion calls dabbrev expand "
                      (setq py-no-completion-calls-dabbrev-expand-p
                            (not py-no-completion-calls-dabbrev-expand-p))
                      :help "If completion function should call dabbrev-expand when no completion found\. Default is `t'

See also `py-indent-no-completion-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-no-completion-calls-dabbrev-expand-p]

                     ["Set Pymacs-based complete keymap "
                      (setq py-set-complete-keymap-p
                            (not py-set-complete-keymap-p))
                      :help "If `py-complete-initialize', which sets up enviroment for Pymacs based py-complete, should load it's keys into `python-mode-map'

Default is nil.
See also resp. edit `py-complete-set-keymap' Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-set-complete-keymap-p]

                     ["No completion calls dabbrev expand "
                      (setq py-no-completion-calls-dabbrev-expand-p
                            (not py-no-completion-calls-dabbrev-expand-p))
                      :help "If completion function should call dabbrev-expand when no completion found. Default is `t'

See also `py-indent-no-completion-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-no-completion-calls-dabbrev-expand-p]

                     ["Indent no completion "
                      (setq py-indent-no-completion-p
                            (not py-indent-no-completion-p))
                      :help "If completion function should indent when no completion found. Default is `t'

See also `py-no-completion-calls-dabbrev-expand-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-no-completion-p]

                     ["Company pycomplete "
                      (setq py-company-pycomplete-p
                            (not py-company-pycomplete-p))
                      :help "Load company-pycomplete stuff. Default is nilUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-company-pycomplete-p])

                    ("Autopair mode"
                     :help "Toggle `autopair-mode'"

                     ["Toggle autopair mode" toggle-py-autopair-mode
                      :help " `toggle-autopair-mode'

If `autopair-mode' should be on or off.

  Returns value of `autopair-mode ' switched to. "]

                     ["Autopair mode on" py-autopair-mode-on
                      :help " `autopair-mode on'

Make sure, `autopair-mode' is on.

Returns value of `autopair-mode'. "]

                     ["Autopair mode off" py-autopair-mode-off
                      :help " `autopair-mode' off

Make sure, `autopair-mode' is off.

Returns value of `autopair-mode'. "])

                    ;; py-smart-operator-mode-p forms
                    ("Smart operator mode"
                     :help "Toggle `smart-operator-mode'"

                     ["Toggle smart operator mode" toggle-py-smart-operator-mode-p
                      :help " `toggle-smart-operator-mode'

If `smart-operator-mode' should be on or off.

  Returns value of `smart-operator-mode ' switched to. "]

                     ["Smart operator mode on" py-smart-operator-mode-p-on
                      :help " `smart-operator-mode -on'

Make sure, `smart-operator-mode' is on.

Returns value of `smart-operator-mode'. "]

                     ["Smart operator mode off" py-smart-operator-mode-p-off
                      :help " `smart-operator-mode' off

Make sure, `smart-operator-mode' is off.

Returns value of `smart-operator-mode'. "])

                    ("Filling"

                     ("Docstring styles"
                      :help "Switch docstring-style"

                      ["Nil" py-set-nil-docstring-style
                       :help " `py-set-nil-docstring-style'

Set py-docstring-style to nil, format string normally. "]

                      ["pep-257-nn" py-set-pep-257-nn-docstring-style
                       :help " `py-set-pep-257-nn-docstring-style'

Set py-docstring-style to 'pep-257-nn "]

                      ["pep-257" py-set-pep-257-docstring-style
                       :help " `py-set-pep-257-docstring-style'

Set py-docstring-style to 'pep-257 "]

                      ["django" py-set-django-docstring-style
                       :help " `py-set-django-docstring-style'

Set py-docstring-style to 'django "]

                      ["onetwo" py-set-onetwo-docstring-style
                       :help " `py-set-onetwo-docstring-style'

Set py-docstring-style to 'onetwo "]

                      ["symmetric" py-set-symmetric-docstring-style
                       :help " `py-set-symmetric-docstring-style'

Set py-docstring-style to 'symmetric "])

                     ["Auto-fill mode"
                      (setq py-auto-fill-mode
                            (not py-auto-fill-mode))
                      :help "Fill according to `py-docstring-fill-column' and `py-comment-fill-column'

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-auto-fill-mode])

                    ["Use current dir when execute"
                     (setq py-use-current-dir-when-execute-p
                           (not py-use-current-dir-when-execute-p))
                     :help " `toggle-py-use-current-dir-when-execute-p'

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-use-current-dir-when-execute-p]

                    ("Indent"
		     ("TAB related"

		      ["indent-tabs-mode"
		       (setq indent-tabs-mode
			     (not indent-tabs-mode))
		       :help "Indentation can insert tabs if this is non-nil.

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected indent-tabs-mode]

		      ["Tab indent"
		       (setq py-tab-indent
			     (not py-tab-indent))
		       :help "Non-nil means TAB in Python mode calls `py-indent-line'.Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-indent]

		      ["Tab shifts region "
		       (setq py-tab-shifts-region-p
			     (not py-tab-shifts-region-p))
		       :help "If `t', TAB will indent/cycle the region, not just the current line.

Default is nil
See also `py-tab-indents-region-p'

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-shifts-region-p]

		      ["Tab indents region "
		       (setq py-tab-indents-region-p
			     (not py-tab-indents-region-p))
		       :help "When `t' and first TAB doesn't shift, indent-region is called.

Default is nil
See also `py-tab-shifts-region-p'

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-indents-region-p])

                     ["Close at start column"
                      (setq py-closing-list-dedents-bos
                            (not py-closing-list-dedents-bos))
                      :help "When non-nil, indent list's closing delimiter like start-column.

It will be lined up under the first character of
 the line that starts the multi-line construct, as in:

my_list = \[
    1, 2, 3,
    4, 5, 6,
]

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-dedents-bos]

                     ["Closing list keeps space"
                      (setq py-closing-list-keeps-space
                            (not py-closing-list-keeps-space))
                      :help "If non-nil, closing parenthesis dedents onto column of opening plus `py-closing-list-space', default is nil Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-keeps-space]

                     ["Closing list space"
                      (setq py-closing-list-space
                            (not py-closing-list-space))
                      :help "Number of chars, closing parenthesis outdent from opening, default is 1 Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-space]

                     ["Tab shifts region "
                      (setq py-tab-shifts-region-p
                            (not py-tab-shifts-region-p))
                      :help "If `t', TAB will indent/cycle the region, not just the current line.

Default is nil
See also `py-tab-indents-region-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-tab-shifts-region-p]

                     ["Lhs inbound indent"
                      (setq py-lhs-inbound-indent
                            (not py-lhs-inbound-indent))
                      :help "When line starts a multiline-assignment: How many colums indent should be more than opening bracket, brace or parenthesis. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-lhs-inbound-indent]

                     ["Continuation offset"
                      (setq py-continuation-offset
                            (not py-continuation-offset))
                      :help "With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-continuation-offset]

                     ["Electric colon"
                      (setq py-electric-colon-active-p
                            (not py-electric-colon-active-p))
                      :help " `py-electric-colon-active-p'

`py-electric-colon' feature.  Default is `nil'. See lp:837065 for discussions. "
                      :style toggle :selected py-electric-colon-active-p]

                     ["Electric colon at beginning of block only"
                      (setq py-electric-colon-bobl-only
                            (not py-electric-colon-bobl-only))
                      :help "When inserting a colon, do not indent lines unless at beginning of block.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-colon-bobl-only]

                     ["Electric yank active "
                      (setq py-electric-yank-active-p
                            (not py-electric-yank-active-p))
                      :help " When non-nil, `yank' will be followed by an `indent-according-to-mode'.

Default is nilUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-yank-active-p]

                     ["Electric kill backward "
                      (setq py-electric-kill-backward-p
                            (not py-electric-kill-backward-p))
                      :help "Affects `py-electric-backspace'. Default is nil.

If behind a delimited form of braces, brackets or parentheses,
backspace will kill it's contents

With when cursor after
my_string\[0:1]
--------------^

==>

my_string\[]
----------^

In result cursor is insided emptied delimited form.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-kill-backward-p]

                     ["Trailing whitespace smart delete "
                      (setq py-trailing-whitespace-smart-delete-p
                            (not py-trailing-whitespace-smart-delete-p))
                      :help "Default is nil. When t, python-mode calls
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)

Also commands may delete trailing whitespace by the way.
When editing other peoples code, this may produce a larger diff than expected Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-trailing-whitespace-smart-delete-p]

                     ["Newline delete trailing whitespace "
                      (setq py-newline-delete-trailing-whitespace-p
                            (not py-newline-delete-trailing-whitespace-p))
                      :help "Delete trailing whitespace maybe left by `py-newline-and-indent'.

Default is `t'. See lp:1100892 Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-newline-delete-trailing-whitespace-p]

                     ["Dedent keep relative column"
                      (setq py-dedent-keep-relative-column
                            (not py-dedent-keep-relative-column))
                      :help "If point should follow dedent or kind of electric move to end of line. Default is t - keep relative position. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-dedent-keep-relative-column]

                     ["Indent paren spanned multilines "
                      (setq py-indent-paren-spanned-multilines-p
                            (not py-indent-paren-spanned-multilines-p))
                      :help "If non-nil, indents elements of list a value of `py-indent-offset' to first element:

def foo():
    if (foo &&
            baz):
        bar()

Default lines up with first element:

def foo():
    if (foo &&
        baz):
        bar()
Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-paren-spanned-multilines-p]

                     ["Indent honors multiline listing"
                      (setq py-indent-honors-multiline-listing
                            (not py-indent-honors-multiline-listing))
                      :help "If `t', indents to 1\+ column of opening delimiter. If `nil', indent adds one level to the beginning of statement. Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-honors-multiline-listing]

                     ["Indent comment "
                      (setq py-indent-comments
                            (not py-indent-comments))
                      :help "If comments should be indented like code. Default is `nil'.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-comments]

                     ["Uncomment indents "
                      (setq py-uncomment-indents-p
                            (not py-uncomment-indents-p))
                      :help "When non-nil, after uncomment indent lines. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-uncomment-indents-p]

                     ["Indent honors inline comment"
                      (setq py-indent-honors-inline-comment
                            (not py-indent-honors-inline-comment))
                      :help "If non-nil, indents to column of inlined comment start.
Default is nil. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-honors-inline-comment]

                     ["Kill empty line"
                      (setq py-kill-empty-line
                            (not py-kill-empty-line))
                      :help "If t, py-indent-forward-line kills empty lines. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-kill-empty-line]

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

Use `M-x customize-variable' to set it permanently"])

                     ["Beep if tab change"
                      (setq py-beep-if-tab-change
                            (not py-beep-if-tab-change))
                      :help "Ring the bell if `tab-width' is changed.
If a comment of the form

                           	# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) `tab-width' does not
equal <number>, `tab-width' is set to <number>, a message saying so is
displayed in the echo area, and if `py-beep-if-tab-change' is non-nil
the Emacs bell is also rung as a warning.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-beep-if-tab-change]

                     ["Highlight indentation" highlight-indentation
                      :help "Toggle highlight indentation.

Use `M-x customize-variable' to set it permanently

Make sure `highlight-indentation' is installed"

                      ]

                     ["Electric comment "
                      (setq py-electric-comment-p
                            (not py-electric-comment-p))
                      :help "If \"#\" should call `py-electric-comment'. Default is `nil'.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-comment-p]

                     ["Electric comment add space "
                      (setq py-electric-comment-add-space-p
                            (not py-electric-comment-add-space-p))
                      :help "If py-electric-comment should add a space.  Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-comment-add-space-p]

                     ["Empty line closes "
                      (setq py-empty-line-closes-p
                            (not py-empty-line-closes-p))
                      :help "When non-nil, dedent after empty line following block

if True:
    print(\"Part of the if-statement\")

print(\"Not part of the if-statement\")

Default is nil

If non-nil, a C-j from empty line dedents.
Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-empty-line-closes-p])
                    ["Defun use top level "
                     (setq py-defun-use-top-level-p
                           (not py-defun-use-top-level-p))
                     :help "When non-nil, keys C-M-a, C-M-e address top-level form.

Beginning- end-of-defun forms use
commands `py-beginning-of-top-level', `py-end-of-top-level'

mark-defun marks top-level form at point etc. "
                     :style toggle :selected py-defun-use-top-level-p]

                    ["Close provides newline"
                     (setq py-close-provides-newline
                           (not py-close-provides-newline))
                     :help "If a newline is inserted, when line after block isn't empty. Default is non-nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-close-provides-newline]

                    ["Block comment prefix "
                     (setq py-block-comment-prefix-p
                           (not py-block-comment-prefix-p))
                     :help "If py-comment inserts py-block-comment-prefix.

Default is tUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-block-comment-prefix-p])

                   ("Display"

                    ("Index"

                     ["Imenu create index "
                      (setq py--imenu-create-index-p
                            (not py--imenu-create-index-p))
                      :help "Non-nil means Python mode creates and displays an index menu of functions and global variables. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py--imenu-create-index-p]

                     ["Imenu show method args "
                      (setq py-imenu-show-method-args-p
                            (not py-imenu-show-method-args-p))
                      :help "Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-imenu-show-method-args-p]
                     ["Switch index-function" py-switch-imenu-index-function
                      :help "`py-switch-imenu-index-function'
Switch between `py--imenu-create-index' from 5.1 series and `py--imenu-create-index-new'."])

                    ("Fontification"

                     ["Mark decorators"
                      (setq py-mark-decorators
                            (not py-mark-decorators))
                      :help "If py-mark-def-or-class functions should mark decorators too. Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-mark-decorators]

                     ["Fontify shell buffer "
                      (setq py-fontify-shell-buffer-p
                            (not py-fontify-shell-buffer-p))
                      :help "If code in Python shell should be highlighted as in script buffer.

Default is nil.

If `t', related vars like `comment-start' will be set too.
Seems convenient when playing with stuff in IPython shell
Might not be TRT when a lot of output arrives Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-fontify-shell-buffer-p]

                     ["Use font lock doc face "
                      (setq py-use-font-lock-doc-face-p
                            (not py-use-font-lock-doc-face-p))
                      :help "If documention string inside of def or class get `font-lock-doc-face'.

`font-lock-doc-face' inherits `font-lock-string-face'.

Call M-x `customize-face' in order to have a visible effect. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-font-lock-doc-face-p])

                    ["Switch buffers on execute"
                     (setq py-switch-buffers-on-execute-p
                           (not py-switch-buffers-on-execute-p))
                     :help "When non-nil switch to the Python output buffer.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-switch-buffers-on-execute-p]

                    ["Split windows on execute"
                     (setq py-split-window-on-execute
                           (not py-split-window-on-execute))
                     :help "When non-nil split windows.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-split-window-on-execute]

                    ["Keep windows configuration"
                     (setq py-keep-windows-configuration
                           (not py-keep-windows-configuration))
                     :help "If a windows is splitted displaying results, this is directed by variable `py-split-window-on-execute'\. Also setting `py-switch-buffers-on-execute-p' affects window-configuration\. While commonly a screen splitted into source and Python-shell buffer is assumed, user may want to keep a different config\.

Setting `py-keep-windows-configuration' to `t' will restore windows-config regardless of settings mentioned above\. However, if an error occurs, it's displayed\.

To suppres window-changes due to error-signaling also: M-x customize-variable RET. Set `py-keep-4windows-configuration' onto 'force

Default is nil Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-keep-windows-configuration]

                    ["Which split windows on execute function"
                     (progn
                       (if (eq 'split-window-vertically py-split-windows-on-execute-function)
                           (setq py-split-windows-on-execute-function'split-window-horizontally)
                         (setq py-split-windows-on-execute-function 'split-window-vertically))
                       (message "py-split-windows-on-execute-function set to: %s" py-split-windows-on-execute-function))

                     :help "If `split-window-vertically' or `...-horizontally'. Use `M-x customize-variable' RET `py-split-windows-on-execute-function' RET to set it permanently"
                     :style toggle :selected py-split-windows-on-execute-function]

                    ["Modeline display full path "
                     (setq py-modeline-display-full-path-p
                           (not py-modeline-display-full-path-p))
                     :help "If the full PATH/TO/PYTHON should be displayed in shell modeline.

Default is nil. Note: when `py-shell-name' is specified with path, it's shown as an acronym in buffer-name already. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-modeline-display-full-path-p]

                    ["Modeline acronym display home "
                     (setq py-modeline-acronym-display-home-p
                           (not py-modeline-acronym-display-home-p))
                     :help "If the modeline acronym should contain chars indicating the home-directory.

Default is nil Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-modeline-acronym-display-home-p]

                    ["Hide show hide docstrings"
                     (setq py-hide-show-hide-docstrings
                           (not py-hide-show-hide-docstrings))
                     :help "Controls if doc strings can be hidden by hide-showUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-hide-show-hide-docstrings]

                    ["Hide comments when hiding all"
                     (setq py-hide-comments-when-hiding-all
                           (not py-hide-comments-when-hiding-all))
                     :help "Hide the comments too when you do `hs-hide-all'. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-hide-comments-when-hiding-all]

                    ["Max help buffer "
                     (setq py-max-help-buffer-p
                           (not py-max-help-buffer-p))
                     :help "If \"\*Python-Help\*\"-buffer should appear as the only visible.

Default is nil. In help-buffer, \"q\" will close it.  Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-max-help-buffer-p]

                    ["Current defun show"
                     (setq py-current-defun-show
                           (not py-current-defun-show))
                     :help "If `py-current-defun' should jump to the definition, highlight it while waiting PY-WHICH-FUNC-DELAY seconds, before returning to previous position.

Default is `t'.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-current-defun-show]

                    ["Match paren mode"
                     (setq py-match-paren-mode
                           (not py-match-paren-mode))
                     :help "Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets `py-match-paren-key' in python-mode-map.
Customize `py-match-paren-key' which key to use. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-match-paren-mode])

                   ("Debug"

		    ["py-debug-p"
		     (setq py-debug-p
			   (not py-debug-p))
		     :help "When non-nil, keep resp\. store information useful for debugging\.

Temporary files are not deleted\. Other functions might implement
some logging etc\. Use `M-x customize-variable' to set it permanently"
		     :style toggle :selected py-debug-p]

                    ["Pdbtrack do tracking "
                     (setq py-pdbtrack-do-tracking-p
                           (not py-pdbtrack-do-tracking-p))
                     :help "Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the \*Python\* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-pdbtrack-do-tracking-p]

                    ["Jump on exception"
                     (setq py-jump-on-exception
                           (not py-jump-on-exception))
                     :help "Jump to innermost exception frame in Python output buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-jump-on-exception]

                    ["Highlight error in source "
                     (setq py-highlight-error-source-p
                           (not py-highlight-error-source-p))
                     :help "Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-highlight-error-source-p])

                   ("Other"

                    ("Directory"

                     ["Guess install directory "
                      (setq py-guess-py-install-directory-p
                            (not py-guess-py-install-directory-p))
                      :help "If in cases, `py-install-directory' isn't set,  `py-set-load-path'should guess it from `buffer-file-name'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-guess-py-install-directory-p]

                     ["Use local default"
                      (setq py-use-local-default
                            (not py-use-local-default))
                      :help "If `t', py-shell will use `py-shell-local-path' instead
of default Python.

Making switch between several virtualenv's easier,
                               `python-mode' should deliver an installer, so named-shells pointing to virtualenv's will be available. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-local-default]

                     ["Use current dir when execute "
                      (setq py-use-current-dir-when-execute-p
                            (not py-use-current-dir-when-execute-p))
                      :help "When `t', current directory is used by Python-shell for output of `py-execute-buffer' and related commands.

See also `py-execute-directory'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-current-dir-when-execute-p]

                     ["Keep shell dir when execute "
                      (setq py-keep-shell-dir-when-execute-p
                            (not py-keep-shell-dir-when-execute-p))
                      :help "Don't change Python shell's current working directory when sending code.

See also `py-execute-directory'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-keep-shell-dir-when-execute-p]

                     ["Fileless buffer use default directory "
                      (setq py-fileless-buffer-use-default-directory-p
                            (not py-fileless-buffer-use-default-directory-p))
                      :help "When `py-use-current-dir-when-execute-p' is non-nil and no buffer-file exists, value of `default-directory' sets current working directory of Python output shellUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-fileless-buffer-use-default-directory-p])

                    ("Underscore word syntax"
                     :help "Toggle `py-underscore-word-syntax-p'"

                     ["Toggle underscore word syntax" toggle-py-underscore-word-syntax-p
                      :help " `toggle-py-underscore-word-syntax-p'

If `py-underscore-word-syntax-p' should be on or off.

  Returns value of `py-underscore-word-syntax-p' switched to. .

Use `M-x customize-variable' to set it permanently"]

                     ["Underscore word syntax on" py-underscore-word-syntax-p-on
                      :help " `py-underscore-word-syntax-p-on'

Make sure, py-underscore-word-syntax-p' is on.

Returns value of `py-underscore-word-syntax-p'. .

Use `M-x customize-variable' to set it permanently"]

                     ["Underscore word syntax off" py-underscore-word-syntax-p-off
                      :help " `py-underscore-word-syntax-p-off'

Make sure, `py-underscore-word-syntax-p' is off.

Returns value of `py-underscore-word-syntax-p'. .

Use `M-x customize-variable' to set it permanently"])

                    ["Load pymacs "
                     (setq py-load-pymacs-p
                           (not py-load-pymacs-p))
                     :help "If Pymacs related stuff should be loaded.

Default is nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.caUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-load-pymacs-p]

                    ["Verbose "
                     (setq py-verbose-p
                           (not py-verbose-p))
                     :help "If functions should report results.

Default is nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-verbose-p]

                    ["Empty comment line separates paragraph "
                     (setq empty-comment-line-separates-paragraph-p
                           (not empty-comment-line-separates-paragraph-p))
                     :help "Consider paragraph start/end lines with nothing inside but comment sign.

Default is non-nilUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected empty-comment-line-separates-paragraph-p]

                    ["Org cycle "
                     (setq py-org-cycle-p
                           (not py-org-cycle-p))
                     :help "When non-nil, command `org-cycle' is available at shift-TAB, <backtab>

Default is nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-org-cycle-p]

                    ["Set pager cat"
                     (setq py-set-pager-cat-p
                           (not py-set-pager-cat-p))
                     :help "If the shell environment variable \$PAGER should set to `cat'.

If `t', use `C-c C-r' to jump to beginning of output. Then scroll normally.

Avoids lp:783828, \"Terminal not fully functional\", for help('COMMAND') in python-shell

When non-nil, imports module `os' Use `M-x customize-variable' to
set it permanently"
                     :style toggle :selected py-set-pager-cat-p]

                    ["Edit only "
                     (setq py-edit-only-p
                           (not py-edit-only-p))
                     :help "When `t' `python-mode' will not take resort nor check for installed Python executables. Default is nil.

See bug report at launchpad, lp:944093. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-edit-only-p])))

                 ("More... "

                  ("Edit commands "

		   ("Hide"
		    ["Hide statement" py-hide-statement
		     :help " `py-hide-statement'

Hide statement at point\. "]

		    ["Hide block" py-hide-block
		     :help " `py-hide-block'

Hide block at point\. "]

		    ["Hide clause" py-hide-clause
		     :help " `py-hide-clause'

Hide clause at point\. "]

		    ["Hide block or clause" py-hide-block-or-clause
		     :help " `py-hide-block-or-clause'

Hide block-or-clause at point\. "]

		    ["Hide def" py-hide-def
		     :help " `py-hide-def'

Hide def at point\. "]

		    ["Hide class" py-hide-class
		     :help " `py-hide-class'

Hide class at point\. "]

		    ["Hide expression" py-hide-expression
		     :help " `py-hide-expression'

Hide expression at point\. "]

		    ["Hide partial expression" py-hide-partial-expression
		     :help " `py-hide-partial-expression'

Hide partial-expression at point\. "]

		    ["Hide line" py-hide-line
		     :help " `py-hide-line'

Hide line at point\. "]

		    ["Hide top level" py-hide-top-level
		     :help " `py-hide-top-level'

Hide top-level at point\. "])

		   ("Show"

		    ["Show statement" py-show-statement
		     :help " `py-show-statement'

Show statement at point\. "]

		    ["Show block" py-show-block
		     :help " `py-show-block'

Show block at point\. "]

		    ["Show clause" py-show-clause
		     :help " `py-show-clause'

Show clause at point\. "]

		    ["Show block or clause" py-show-block-or-clause
		     :help " `py-show-block-or-clause'

Show block-or-clause at point\. "]

		    ["Show def" py-show-def
		     :help " `py-show-def'

Show def at point\. "]

		    ["Show class" py-show-class
		     :help " `py-show-class'

Show class at point\. "]

		    ["Show expression" py-show-expression
		     :help " `py-show-expression'

Show expression at point\. "]

		    ["Show partial expression" py-show-partial-expression
		     :help " `py-show-partial-expression'

Show partial-expression at point\. "]

		    ["Show line" py-show-line
		     :help " `py-show-line'

Show line at point\. "]

		    ["Show top level" py-show-top-level
		     :help " `py-show-top-level'

Show top-level at point\. "])

                   ("Kill "

                    ["Kill statement" py-kill-statement
                     :help "`py-kill-statement'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill top level" py-kill-top-level
                     :help " `py-kill-top-level'

Delete top-level form at point.

Stores data in kill ring. Might be yanked back using `C-y'. "]

                    ["Kill clause" py-kill-clause
                     :help "`py-kill-clause'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill block" py-kill-block
                     :help "`py-kill-block'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill minor block" py-kill-minor-block
                     :help " `py-kill-minor-block'

Delete minor-block at point.

Stores data in kill ring. Might be yanked back using `C-y'. "]

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

                   ("Delete"
                    ["Delete statement " py-delete-statement
                     :help "`py-delete-statement'
Delete STATEMENT at point, don't store in kill-ring. "]

                    ["Delete top-level " py-delete-top-level
                     :help "`py-delete-top-level'
Delete TOP-LEVEL at point, don't store in kill-ring. "]

                    ["Delete block " py-delete-block
                     :help "`py-delete-block'
Delete BLOCK at point, don't store in kill-ring. "]

                    ["Delete block-or-clause " py-delete-block-or-clause
                     :help "`py-delete-block-or-clause'
Delete BLOCK-OR-CLAUSE at point, don't store in kill-ring. "]

                    ["Delete def " py-delete-def
                     :help "`py-delete-def'
Delete DEF at point, don't store in kill-ring. "]

                    ["Delete class " py-delete-class
                     :help "`py-delete-class'
Delete CLASS at point, don't store in kill-ring. "]

                    ["Delete def-or-class " py-delete-def-or-class
                     :help "`py-delete-def-or-class'
Delete DEF-OR-CLASS at point, don't store in kill-ring. "]

                    ["Delete expression " py-delete-expression
                     :help "`py-delete-expression'
Delete EXPRESSION at point, don't store in kill-ring. "]

                    ["Delete partial-expression " py-delete-partial-expression
                     :help "`py-delete-partial-expression'
Delete PARTIAL-EXPRESSION at point, don't store in kill-ring. "]

                    ["Delete minor-block " py-delete-minor-block
                     :help "`py-delete-minor-block'
Delete MINOR-BLOCK at point, don't store in kill-ring.

A minor block is started by a `for', `if', `try' or `with'. "])
                   "-"

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

                    ["Shift minor block right" py-shift-minor-block-right
                     :help " `py-shift-minor-block-right'

Indent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "]

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
Shift block-or-clause right. "]

                    ["Shift region left" py-shift-region-left
                     :help " `py-shift-region-left'

Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. "]

                    ["Shift region right" py-shift-region-right
                     :help " `py-shift-region-right'

Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. "])

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

                    ["Shift minor block left" py-shift-minor-block-left
                     :help " `py-shift-minor-block-left'

Dedent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "]

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
Shift block-or-clause left. "])
                   ("More"
                    :help "extended edit commands'"

                    ["Kill buffer unconditional" py-kill-buffer-unconditional
                     :help " `py-kill-buffer-unconditional'

Kill buffer unconditional, kill buffer-process if existing\. "]

                    ["Empty out list backward" py-empty-out-list-backward
                     :help " `py-empty-out-list-backward'
Deletes all elements from list before point. "]

                    ["Revert boolean assignent" py-boolswitch
                     :help " `py-boolswitch'
Edit the assigment of a boolean variable, rever them.

I.e. switch it from \"True\" to \"False\" and vice versa "]

                    ["Remove overlays at point" py-remove-overlays-at-point
                     :help " `py-remove-overlays-at-point'

Remove overlays as set when `py-highlight-error-source-p' is non-nil. "]))

                  "-"
                  ("Forms "
                   ("Comment"

                    ["Beginning of comment" py-beginning-of-comment
                     :help " `py-beginning-of-comment'
Go to beginning of comment at point. "]

                    ["End of comment" py-end-of-comment
                     :help " `py-end-of-comment'

Go to end of comment at point. "])
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

Comments block at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Top-level form"

                    ["Beginning of top-level form" py-beginning-of-top-level
                     :help "`py-beginning-of-top-level'
Go to start of top-level form form at point"]

                    ["End of top-level form" py-end-of-top-level
                     :help "`py-end-of-top-level'
Go to end of top-level form at point"]

                    ["Down top-level form" py-down-top-level
                     :help "`py-down-top-level'

Go to the beginning of top-level form below in buffer. "]

                    ["Up top-level form" py-up-top-level
                     :help "`py-up-top-level'

Go upwards to the beginning of next top-level form in buffer. "]

                    ["Copy top-level form" py-copy-top-level
                     :help "`py-copy-top-level'
Copy innermost top-level form at point"]

                    ["Kill top-level form" py-kill-top-level
                     :help "`py-kill-top-level'
Delete top-level form at point, store deleted string in kill-ring"]

                    ["Delete top-level form" py-delete-top-level
                     :help "`py-delete-top-level'
Delete top-level form at point, don't store deleted string in kill-ring"]

                    ["Comment top-level form" py-comment-top-level
                     :help " `py-comment-top-level'

Comments top-level form at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Minor-block"

                    ["Beginning of minor-block" py-beginning-of-minor-block
                     :help "`py-beginning-of-minor-block'
Go to start of innermost minor-block at point"]
                    ["End of minor-block" py-end-of-minor-block
                     :help "`py-end-of-minor-block'
Go to end of innermost minor-block at point"]

                    ["Down minor-block" py-down-minor-block
                     :help "`py-down-minor-block'

Go to the beginning of next minor-block below in buffer.

Returns indentation if minor-block found, nil otherwise. "]

                    ["Up minor-block" py-up-minor-block
                     :help "`py-up-minor-block'

Go upwards to the beginning of next minor-block below in buffer.

Returns indentation if minor-block found, nil otherwise. "]

                    ["Copy minor-block" py-copy-minor-block
                     :help "`py-copy-minor-block'
Copy innermost minor-block at point"]

                    ["Kill minor-block" py-kill-minor-block
                     :help "`py-kill-minor-block'
Delete innermost minor-block at point, store deleted string in kill-ring"]

                    ["Delete minor-block" py-delete-minor-block
                     :help "`py-delete-minor-block'
Delete innermost minor-block at point, don't store deleted string in kill-ring"]

                    ["Shift minor-block right" py-shift-minor-block-right
                     :help "`py-shift-minor-block-right'
Shift minor-block right. "]

                    ["Shift minor-block left" py-shift-minor-block-left
                     :help "`py-shift-minor-block-left'
Shift minor-block left. "]

                    ["Comment minor-block" py-comment-minor-block
                     :help " `py-comment-minor-block'

Comments minor-block at point.

Uses double hash (`#') comment starter when `py-minor-block-comment-prefix-p' is `t',
the default. "])

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

Comments def-or-class at point.

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

Comments clause at point.

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

Comments statement at point.

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

Comments class at point.

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

Comments def at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   "-"

                   ("Block bol "

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

                   ("Minor-block bol "

                    ["Beginning of minor-block bol" py-beginning-of-minor-block-bol
                     :help "`py-beginning-of-minor-block-bol'
Go to beginning of line at beginning of minor-block.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["End of minor-block bol" py-end-of-minor-block-bol
                     :help "`py-end-of-minor-block-bol'
Go to beginning of line following end of minor-block.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Up minor-block bol" py-up-minor-block-bol
                     :help "`py-up-minor-block-bol'
Go to next minor-block upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Down minor-block bol" py-down-minor-block-bol
                     :help "`py-down-minor-block-bol'
Go to next minor-block downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Mark minor-block bol" py-mark-minor-block-bol
                     :help "`py-mark-minor-block-bol'
Mark minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Copy minor-block bol" py-copy-minor-block-bol
                     :help "`py-copy-minor-block-bol'
Copy minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Kill minor-block bol" py-kill-minor-block-bol
                     :help "`py-kill-minor-block-bol'
Kill minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Delete minor-block bol" py-delete-minor-block-bol
                     :help "`py-delete-minor-block-bol'
Delete minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Shift minor-block right" py-shift-minor-block-right
                     :help "`py-shift-minor-block-right'
Shift minor-block right.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Shift minor-block left" py-shift-minor-block-left
                     :help "`py-shift-minor-block-left'
Shift minor-block left.

A minor block is started by a `for', `if', `try' or `with'. "])

                   ("Clause bol "

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

                   ("Block-Or-Clause bol "

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

                   ("Def bol "

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

                   ("Class bol "
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

                   ("Def-Or-Class bol "
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

                   ("Statement bol "
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
Shift statement left. "]))
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
back to the previous non-whitespace character.
See also C-c <delete>. "]

                   ["Hungry delete forward" py-hungry-delete-forward
                    :help " `py-hungry-delete-forward'

Delete the following character or all following whitespace
up to the next non-whitespace character.
See also C-c <C-backspace>. "]

                   ["Electric colon" py-electric-colon
                    :help " `py-electric-colon'
Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix C-u.

Switched by `py-electric-colon-active-p', default is nil
See also `py-electric-colon-greedy-p' "]

                   ["Electric colon greedy "
                    (setq py-electric-colon-greedy-p
                          (not py-electric-colon-greedy-p))
                    :help "If py-electric-colon should indent to the outmost reasonable level.

If nil, default, it will not move from at any reasonable level. Use `M-x customize-variable' to set it permanently"
                    :style toggle :selected py-electric-colon-greedy-p]

                   ["Electric colon newline and indent "
                    (setq py-electric-colon-newline-and-indent-p
                          (not py-electric-colon-newline-and-indent-p))
                    :help "If non-nil, `py-electric-colon' will call `newline-and-indent'.  Default is `nil'. Use `M-x customize-variable' to set it permanently"
                    :style toggle :selected py-electric-colon-newline-and-indent-p]

                   ["Electric delete" py-electric-delete
                    :help " `py-electric-delete'
Delete following character or levels of whitespace.

With ARG do that ARG times. "]

                   ["Electric backspace" py-electric-backspace
                    :help " `py-electric-backspace'
Delete preceding character or level of indentation.

With ARG do that ARG times.
Returns column reached. "]

                   ["Electric comment" py-electric-comment
                    :help " `py-electric-comment'
Insert a comment. If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many \"#\" are inserted
non-electrically.
With C-u \"#\" electric behavior is inhibited inside a string or comment. "]

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
                    :help "Inserts py-try/except-statement"])

                  ("Completion"
                   :help "Completion options"

		   ["Auto complete mode"
		    (setq py-auto-complete-p
			  (not py-auto-complete-p))
		    :help "Auto complete mode

Use `M-x customize-variable' to set it permanently"
		    :style toggle :selected py-auto-complete-p]

		   ["Indent or complete" py-indent-or-complete
		    :help " `py-indent-or-complete'

Complete or indent depending on the context\.

If cursor is at end of line, try to complete
Otherwise call `py-indent-line'

Use `C-q TAB' to insert a literally TAB-character "]

                   ["Complete symbol" py-shell-complete
                    :help "`py-shell-complete'
Complete (qualified) symbol before point"]

                   ["Complete" py-complete
                    :help " `py-complete'
Complete symbol before point using Pymacs . "])

                  ["Find function" py-find-function
                   :help "`py-find-function'
Try to find source definition of function at point"]))))
        map))

;; avoid errors from ipython.el - which isn't needed anymore
(defvaralias 'py-mode-map 'python-mode-map)

(defvar py-shell-mode-map)
(setq py-shell-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "RET") 'comint-send-input)
        (define-key map [(control c)(-)] 'py-up-exception)
        (define-key map [(control c)(=)] 'py-down-exception)
	(define-key map (kbd "TAB") 'py-indent-or-complete)
	(define-key map [(meta tab)] 'py-shell-complete)
	(define-key map [(control c)(!)] 'py-shell)
	(define-key map [(control c)(control t)] 'py-toggle-shell)
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
        (define-key map [(control j)] 'py-newline-and-indent)
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
        (define-key map [(control meta h)] 'py-mark-def-or-class)
        (define-key map [(control c)(control k)] 'py-mark-block-or-clause)
        (define-key map [(control c)(.)] 'py-expression)
        ;; Miscellaneous
        (define-key map [(super q)] 'py-copy-statement)
        (define-key map [(control c)(control d)] 'py-pdbtrack-toggle-stack-tracking)
        (define-key map [(control c)(\#)] 'py-comment-region)
        (define-key map [(control c)(\?)] 'py-describe-mode)
        (define-key map [(control c)(control e)] 'py-help-at-point)
        (define-key map [(control x) (n) (d)] 'py-narrow-to-defun)
        ;; information
        (define-key map [(control c)(control b)] 'py-submit-bug-report)
        (define-key map [(control c)(control v)] 'py-version)
        (define-key map [(control c)(control w)] 'py-pychecker-run)
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
               py-shell-menu map "Py-Shell menu"
               `("Py-Shell"
                 ("Interpreter"

		  ["Comint send input" comint-send-input
		   :help " `comint-send-input'

Send input to process\.
After the process output mark, sends all text from the process mark to
point as input to the process\.  Before the process output mark, calls
value of variable `comint-get-old-input' to retrieve old input, copies
it to the process mark, and sends it\.

This command also sends and inserts a final newline, unless
NO-NEWLINE is non-nil\.

Any history reference may be expanded depending on the value of the variable
`comint-input-autoexpand'\.  The list of function names contained in the value
of `comint-input-filter-functions' is called on the input before sending it\.
The input is entered into the input history ring, if the value of variable
`comint-input-filter' returns non-nil when called on the input\.

\(fn &optional NO-NEWLINE ARTIFICIAL)"]

		  ["Up exception" py-up-exception
		   :help " `py-up-exception'

Go to the previous line up in the traceback\.
With C-u (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack\."]

		  ["Down exception" py-down-exception
		   :help " `py-down-exception'

Go to the next line down in the traceback\.
With M-x univeral-argument (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack\."]

		  ["Default interpreter..." py-shell
                   :help " `py-shell'

Start an interactive Python interpreter.

Interactively, C-u 4 prompts for a buffer.
C-u 2 prompts for `py-python-command-args'.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

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

                   ["python3.3" python3.3
                    :help "`python3.3'
Start an Python3.3 interpreter.

Optional C-u prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'."]

                   ["python3.4" python3.4
                    :help "`python3.3'
Start an Python3.4 interpreter.

Optional C-u prompts for options to pass to the Python3.4 interpreter. See `py-python-command-args'."]

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

Optional C-u prompts for options to pass to the Jython interpreter. See `py-python-command-args'."])
                  "-"

                  ["Kill shell unconditional" py-kill-shell-unconditional
                   :help " `py-kill-shell-unconditional'

With optional argument SHELL\.

Otherwise kill default (I)Python shell\.
Kill buffer and its process.
Receives a buffer-name as argument "]

                  ["Kill default shell unconditional" py-kill-default-shell-unconditional
                   :help " `py-kill-default-shell-unconditional'

Kill buffer \"*Python*\" and its process\.  "])
		 ("Completion"
		  :help "Completion options"

		  ["Indent or complete" py-indent-or-complete
		   :help " `py-indent-or-complete'

Complete or indent depending on the context\.

If cursor is at end of line, try to complete
Otherwise call `py-indent-line'

Use `C-q TAB' to insert a literally TAB-character "]

		  ["Complete symbol" py-shell-complete
		   :help "`py-shell-complete'
Complete (qualified) symbol before point"]

		  ["Complete" py-complete
		   :help " `py-complete'
Complete symbol before point using Pymacs . "])

                 "-"
                 ("Mark"

                  ["Mark block" py-mark-block
                   :help " `py-mark-block'

Mark block at point.

Returns beginning and end positions of marked area, a cons. "]

                  ["Mark minor block" py-mark-minor-block
                   :help " `py-mark-minor-block'

Mark minor-block at point.

A minor block is started by a `for', `if', `try' or `with'.
Returns beginning and end positions of marked area, a cons. "]

                  ["Mark def or class" py-mark-def-or-class
                   :help " `py-mark-def-or-class'

Mark def-or-class at point.

Returns beginning and end positions of marked area, a cons. "]

                  ["Mark statement" py-mark-statement
                   :help "`py-mark-statement'
Mark statement at point"]

                  ["Mark top level" py-mark-top-level
                   :help " `py-mark-top-level'

Mark top-level form at point.

Returns beginning and end positions of marked area, a cons. "]

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

                  ("BOL forms"

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

Mark minor-block at point reaching beginning-of-line.
A minor block is started by a `for', `if', `try' or `with'."]))

		 "-"

                 ["Shift region left" py-shift-region-left
                  :help " `py-shift-region-left'

Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. "]

                 ["Shift region right" py-shift-region-right
                  :help " `py-shift-region-right'

Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. "]

                 "-"

                 ("Comment"
                  ["Comment Region"   py-comment-region (point) (mark)
                   :help "Like `comment-region' but uses double hash (`#') comment starter." ]
                  ["Uncomment" py-uncomment
                   :help " `py-uncomment'

Uncomment commented lines at point.

If region is active, restrict uncommenting at region . "]

                  ["Uncomment Region"     (py-comment-region (point) (mark) '(4))
                   :help "(py-comment-region (point) (mark) '(4))" ]
                  "-"
                  ["Comment block" py-comment-block
                   :help " `py-comment-block'
Comments block at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment minor-block" py-comment-minor-block
                   :help " `py-comment-minor-block'
Comments minor-block at point.

A minor block is started by a `for', `if', `try' or `with'.
Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment top level" py-comment-top-level
                   :help " `py-comment-top-level'

Comments top-level form at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment clause" py-comment-clause
                   :help " `py-comment-clause'
Comments clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment block or clause" py-comment-block-or-clause
                   :help " `py-comment-block-or-clause'
Comments block-or-clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment def" py-comment-def
                   :help " `py-comment-def'
Comments def at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment class" py-comment-class
                   :help " `py-comment-class'
Comments class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment def or class" py-comment-def-or-class
                   :help " `py-comment-def-or-class'
Comments def-or-class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment statement" py-comment-statement
                   :help " `py-comment-statement'
Comments statement at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                 "-"

                 ("Move"

		  ["Backward same level" py-backward-same-level
		   :help " `py-backward-same-level'

Go form backward keeping indent level if possible\.

If inside a delimited form --string or list-- go to its beginning\.
If not at beginning of a statement or block, go to its beginning\.
If at beginning of a statement or block, go to previous beginning of compound statement or definition at point\.
If no further element at same level, go one level up. "]

                  ["Beginning of block" py-beginning-of-block
                   :help " `py-beginning-of-block'

Go to beginning block, skip whitespace at BOL. "]

                  ["Go to end of block" py-end-of-block]

                  "-"

                  ["Beginning of def or class" py-beginning-of-def-or-class
                   :help " `py-beginning-of-def-or-class'

Go to beginning def-or-class, skip whitespace at BOL.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. "]

                  ["End of def or class" py-end-of-def-or-class
                   :help " `py-end-of-def-or-class'

Go to end of def-or-class.

Returns end of def-or-class if successful, nil otherwise

With M-x universal argument or `py-mark-decorators' set to `t', decorators are marked too. "]

                  "-"

                  ["Beginning of statement" py-beginning-of-statement
                   :help " `py-beginning-of-statement'

Go to the initial line of a simple statement. "]

                  ["End of statement" py-end-of-statement
                   :help " `py-end-of-statement'

Go to the last char of current statement.

To go just beyond the final line of the current statement, use `py-down-statement-bol'. "]

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

                  ("BOL forms"
                   ("Beginning"

                    ["Beginning of block bol" py-beginning-of-block-bol
                     :help " `py-beginning-of-block-bol'

Go to beginning block, go to beginning-of-line\.

Returns beginning of block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of clause bol" py-beginning-of-clause-bol
                     :help " `py-beginning-of-clause-bol'

Go to beginning clause, go to beginning-of-line\.

Returns beginning of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of block or clause bol" py-beginning-of-block-or-clause-bol
                     :help " `py-beginning-of-block-or-clause-bol'

Go to beginning block-or-clause, go to beginning-of-line\.

Returns beginning of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of def bol" py-beginning-of-def-bol
                     :help " `py-beginning-of-def-bol'

Go to beginning def, go to beginning-of-line\.

Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too\.

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of class bol" py-beginning-of-class-bol
                     :help " `py-beginning-of-class-bol'

Go to beginning class, go to beginning-of-line\.

Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too\.

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of def or class bol" py-beginning-of-def-or-class-bol
                     :help " `py-beginning-of-def-or-class-bol'

Go to beginning def-or-class, go to beginning-of-line\.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too\.

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of if block bol" py-beginning-of-if-block-bol
                     :help " `py-beginning-of-if-block-bol'

Go to beginning if-block, go to beginning-of-line\.

Returns beginning of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of try block bol" py-beginning-of-try-block-bol
                     :help " `py-beginning-of-try-block-bol'

Go to beginning try-block, go to beginning-of-line\.

Returns beginning of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of minor block bol" py-beginning-of-minor-block-bol
                     :help " `py-beginning-of-minor-block-bol'

Go to beginning minor-block, go to beginning-of-line\.

Returns beginning of minor-block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of statement bol" py-beginning-of-statement-bol
                     :help " `py-beginning-of-statement-bol'

Goto beginning of line where statement starts\.
  Returns position reached, if successful, nil otherwise\.

See also `py-up-statement': up from current definition to next beginning of statement above\.  "])
                   ("End"

                    ["End of block bol" py-end-of-block-bol
                     :help " `py-end-of-block-bol'

Goto beginning of line following end of block\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-block': down from current definition to next beginning of block below\.  "]

                    ["End of clause bol" py-end-of-clause-bol
                     :help " `py-end-of-clause-bol'

Goto beginning of line following end of clause\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-clause': down from current definition to next beginning of clause below\.  "]

                    ["End of block or clause bol" py-end-of-block-or-clause-bol
                     :help " `py-end-of-block-or-clause-bol'

Goto beginning of line following end of block-or-clause\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-block-or-clause': down from current definition to next beginning of block-or-clause below\.  "]

                    ["End of def bol" py-end-of-def-bol
                     :help " `py-end-of-def-bol'

Goto beginning of line following end of def\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-def': down from current definition to next beginning of def below\.  "]

                    ["End of class bol" py-end-of-class-bol
                     :help " `py-end-of-class-bol'

Goto beginning of line following end of class\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-class': down from current definition to next beginning of class below\.  "]

                    ["End of def or class bol" py-end-of-def-or-class-bol
                     :help " `py-end-of-def-or-class-bol'

Goto beginning of line following end of def-or-class\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-def-or-class': down from current definition to next beginning of def-or-class below\.  "]

                    ["End of if block bol" py-end-of-if-block-bol
                     :help " `py-end-of-if-block-bol'

 "]

                    ["End of try block bol" py-end-of-try-block-bol
                     :help " `py-end-of-try-block-bol'

 "]

                    ["End of minor block bol" py-end-of-minor-block-bol
                     :help " `py-end-of-minor-block-bol'

 "]

                    ["End of statement bol" py-end-of-statement-bol
                     :help " `py-end-of-statement-bol'

Goto beginning of line following end of statement\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-statement': down from current definition to next beginning of statement below\.  "]))

                  "-"

                  ("More"

                   ["Up level" py-up
                    :help " `py-up'
Go to beginning one level above of compound statement or definition at point. "]

                   ["Down level" py-down
                    :help " `py-down'
Go to beginning one level below of compound statement or definition at point. "]

                   "-"

                   ["Beginning of top level" py-beginning-of-top-level
                    :help " `py-beginning-of-top-level'

Go to the very beginning of top-level form at point. "]

                   ["End of top level" py-end-of-top-level
                    :help " `py-end-of-top-level'

Go to end of top-level form at point. "]

                   "-"

                   ["Beginning of block current-column" py-beginning-of-block-current-column
                    :help " `py-beginning-of-block-current-column'

Reach next beginning of block upwards which starts at current column.

Return position. "]

                   "-"

                   ["Move to start of def" py-beginning-of-def t]

                   ["Move to end of def"   py-end-of-def t]

                   "-"

                   ["Beginning of clause" py-beginning-of-clause
                    :help " `py-beginning-of-clause'

Go to beginning clause, skip whitespace at BOL. "]

                   ["End of clause" py-end-of-clause
                    :help " `py-end-of-clause'

Go to end of clause. "]

                   "-"

                   ["Beginning of comment" py-beginning-of-comment
                    :help " `py-beginning-of-comment'
Go to beginning of comment at point. "]

                   ["End of comment" py-end-of-comment
                    :help " `py-end-of-comment'

Go to end of comment at point. "]

                   "-"

                   ["Go to start of expression" (py-beginning-of-expression t) t]
                   ["Move to end of expression" (py-end-of-expression t) t]

                   "-"

                   ["Go to start of minor-expression" (py-beginning-of-minor-expression t) t]

                   ["Move to end of minor-expression" (py-end-of-minor-expression t) t]
                   "-"

                   ["Beginning of minor block" py-beginning-of-minor-block
                    :help " `py-beginning-of-minor-block'

Go to beginning minor-block, skip whitespace at BOL.

Returns beginning of minor-block if successful, nil otherwise

A minor block is started by a `for', `if', `try' or `with'.

"]

                   ["End of minor block" py-end-of-minor-block
                    :help " `py-end-of-minor-block'

Go to end of minor-block.

Returns end of minor-block if successful, nil otherwise

A minor block is started by a `for', `if', `try' or `with'. "]))

                 "-"

                 ("Copy "
                  ["Copy statement" py-copy-statement
                   :help "`py-copy-statement'
Copy statement at point"]

                  ["Copy top level" py-copy-top-level
                   :help " `py-copy-top-level'

Copy top-level form at point. "]

                  ["Copy clause" py-copy-clause
                   :help "`py-copy-clause'
Copy innermost clause at point"]

                  ["Copy block" py-copy-block
                   :help "`py-copy-block'
Copy innermost block at point"]

                  ["Copy minor block" py-copy-minor-block
                   :help " `py-copy-minor-block'

Copy minor-block at point.

Store data in kill ring, so it might yanked back.
A minor block is started by a `for', `if', `try' or `with'. "]

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
Copy innermost definition at point"]

                  ("BOL forms"

                   ["Copy block bol" py-copy-block-bol
                    :help " `py-copy-block-bol'

Delete block, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy clause bol" py-copy-clause-bol
                    :help " `py-copy-clause-bol'

Delete clause, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy block or clause bol" py-copy-block-or-clause-bol
                    :help " `py-copy-block-or-clause-bol'

Delete block-or-clause, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy def bol" py-copy-def-bol
                    :help " `py-copy-def-bol'

Delete def, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy class bol" py-copy-class-bol
                    :help " `py-copy-class-bol'

Delete class, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy def or class bol" py-copy-def-or-class-bol
                    :help " `py-copy-def-or-class-bol'

Delete def-or-class, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy statement bol" py-copy-statement-bol
                    :help " `py-copy-statement-bol'

Delete statement, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy minor block bol" py-copy-minor-block-bol
                    :help " `py-copy-minor-block-bol'

Delete block, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.

See `py-minor-block-re' "]))

		 ("Hide-Show"

		  ["Hide region" py-hide-region
		   :help " `py-hide-region'

Hide active region\. "]

		  ["Hide statement" py-hide-statement
		   :help " `py-hide-statement'

Hide statement at point\. "]

		  ["Hide block" py-hide-block
		   :help " `py-hide-block'

Hide block at point\. "]

		  ["Hide clause" py-hide-clause
		   :help " `py-hide-clause'

Hide clause at point\. "]

		  ["Hide block or clause" py-hide-block-or-clause
		   :help " `py-hide-block-or-clause'

Hide block-or-clause at point\. "]

		  ["Hide def" py-hide-def
		   :help " `py-hide-def'

Hide def at point\. "]

		  ["Hide class" py-hide-class
		   :help " `py-hide-class'

Hide class at point\. "]

		  ["Hide expression" py-hide-expression
		   :help " `py-hide-expression'

Hide expression at point\. "]

		  ["Hide partial expression" py-hide-partial-expression
		   :help " `py-hide-partial-expression'

Hide partial-expression at point\. "]

		  ["Hide line" py-hide-line
		   :help " `py-hide-line'

Hide line at point\. "]

		  ["Hide top level" py-hide-top-level
		   :help " `py-hide-top-level'

Hide top-level at point\. "]

		  ("Show"

		   ["Show region" py-show-region
		    :help " `py-show-region'

Un-hide active region\. "]

		   ["Show statement" py-show-statement
		    :help " `py-show-statement'

Show statement at point\. "]

		   ["Show block" py-show-block
		    :help " `py-show-block'

Show block at point\. "]

		   ["Show clause" py-show-clause
		    :help " `py-show-clause'

Show clause at point\. "]

		   ["Show block or clause" py-show-block-or-clause
		    :help " `py-show-block-or-clause'

Show block-or-clause at point\. "]

		   ["Show def" py-show-def
		    :help " `py-show-def'

Show def at point\. "]

		   ["Show class" py-show-class
		    :help " `py-show-class'

Show class at point\. "]

		   ["Show expression" py-show-expression
		    :help " `py-show-expression'

Show expression at point\. "]

		   ["Show partial expression" py-show-partial-expression
		    :help " `py-show-partial-expression'

Show partial-expression at point\. "]

		   ["Show line" py-show-line
		    :help " `py-show-line'

Show line at point\. "]

		   ["Show top level" py-show-top-level
		    :help " `py-show-top-level'

Show top-level at point\. "]))

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

Check if a directory is a virtualenv. "])

                 ("Help"

		  ["Find definition" py-find-definition
		   :help " `py-find-definition'

Find source of definition of SYMBOL\.

Interactively, prompt for SYMBOL\."]

		  ["Imenu" imenu
		   :help " `imenu'

Jump to a INDEX-ITEM "]

                  ["Info lookup symbol" py-info-lookup-symbol
                   :help " `py-info-lookup-symbol'

Calls `info-lookup-symbol'.

Sends help if stuff is missing. "]

                  ["Symbol at point" py-symbol-at-point
                   :help " `py-symbol-at-point'

Return the current Python symbol\. "]

		  "-"

                  ["Describe mode"        py-describe-mode t]

                  ["Help on symbol" py-help-at-point
                   :help "`py-help-at-point'\n
Use pydoc on symbol at point"])

                 ("Debugger"

                  ["pdb" pdb
		   :help "`pdb' Run pdb under GUD"])

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

                  ("Flake8 " :help
                   "code checker running "

                   ["Flake8 run" py-flake8-run
                    :help " `py-flake8-run'

        Flake8 is a wrapper around these tools:
        - PyFlakes
        - pep8
        - Ned Batchelder's McCabe script

        It also adds features:
        - files that contain this line are skipped::
            # flake8: noqa
        - lines that contain a ``# noqa`` comment at the end will not issue warnings.
        - a Git and a Mercurial hook.
        - a McCabe complexity checker.
        - extendable through ``flake8.extension`` entry points.

. "]

                   ["Flake8 help" py-flake8-help
                    :help " `py-flake8-help'

Display flake8 command line help messages. "])

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
Toggle flymake-mode running `pyflakespep8' "]))

                 ("Customize"

                  ["Python-mode customize group" (customize-group 'python-mode)
                   :help "Open the customization buffer for Python mode"]
                  ("Switches"
                   :help "Toggle useful modes like `highlight-indentation'"
                   ("Interpreter"

                    ["Shell prompt read only"
                     (setq py-shell-prompt-read-only
                           (not py-shell-prompt-read-only))
                     :help "If non-nil, the python prompt is read only.  Setting this variable will only effect new shells.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-shell-prompt-read-only]

                    ["Remove cwd from path"
                     (setq py-remove-cwd-from-path
                           (not py-remove-cwd-from-path))
                     :help "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
a Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion).Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-remove-cwd-from-path]

                    ["Honor IPYTHONDIR "
                     (setq py-honor-IPYTHONDIR-p
                           (not py-honor-IPYTHONDIR-p))
                     :help "When non-nil ipython-history file is constructed by \$IPYTHONDIR
followed by "/history". Default is nil.

Otherwise value of py-ipython-history is used. Use `M-x customize-variable' to set it permanently"
:style toggle :selected py-honor-IPYTHONDIR-p]

                    ["Honor PYTHONHISTORY "
                     (setq py-honor-PYTHONHISTORY-p
                           (not py-honor-PYTHONHISTORY-p))
                     :help "When non-nil python-history file is set by \$PYTHONHISTORY
Default is nil.

Otherwise value of py-python-history is used. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-honor-PYTHONHISTORY-p]

                    ["Enforce py-shell-name" force-py-shell-name-p-on
                     :help "Enforce customized default `py-shell-name' should upon execution. "]

                    ["Don't enforce default interpreter" force-py-shell-name-p-off
                     :help "Make execute commands guess interpreter from environment"]

                    ["Enforce local Python shell " py-force-local-shell-on
                     :help "Locally indicated Python being enforced upon sessions execute commands. "]

                    ["Remove local Python shell enforcement, restore default" py-force-local-shell-off
                     :help "Restore `py-shell-name' default value and `behaviour'. "])

                   ("Edit"

                    ("Completion"

                     ["No completion calls dabbrev expand "
                      (setq py-no-completion-calls-dabbrev-expand-p
                            (not py-no-completion-calls-dabbrev-expand-p))
                      :help "If completion function should call dabbrev-expand when no completion found\. Default is `t'

See also `py-indent-no-completion-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-no-completion-calls-dabbrev-expand-p]

                     ["Set Pymacs-based complete keymap "
                      (setq py-set-complete-keymap-p
                            (not py-set-complete-keymap-p))
                      :help "If `py-complete-initialize', which sets up enviroment for Pymacs based py-complete, should load it's keys into `py-shell-mode-map'

Default is nil.
See also resp. edit `py-complete-set-keymap' Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-set-complete-keymap-p]

                     ["No completion calls dabbrev expand "
                      (setq py-no-completion-calls-dabbrev-expand-p
                            (not py-no-completion-calls-dabbrev-expand-p))
                      :help "If completion function should call dabbrev-expand when no completion found. Default is `t'

See also `py-indent-no-completion-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-no-completion-calls-dabbrev-expand-p]

                     ["Indent no completion "
                      (setq py-indent-no-completion-p
                            (not py-indent-no-completion-p))
                      :help "If completion function should indent when no completion found. Default is `t'

See also `py-no-completion-calls-dabbrev-expand-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-no-completion-p]

                     ["Company pycomplete "
                      (setq py-company-pycomplete-p
                            (not py-company-pycomplete-p))
                      :help "Load company-pycomplete stuff. Default is nilUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-company-pycomplete-p])

                    ("Autopair mode"
                     :help "Toggle `autopair-mode'"

                     ["Toggle autopair mode" toggle-py-autopair-mode
                      :help " `toggle-autopair-mode'

If `autopair-mode' should be on or off.

  Returns value of `autopair-mode ' switched to. "]

                     ["Autopair mode on" py-autopair-mode-on
                      :help " `autopair-mode on'

Make sure, `autopair-mode' is on.

Returns value of `autopair-mode'. "]

                     ["Autopair mode off" py-autopair-mode-off
                      :help " `autopair-mode' off

Make sure, `autopair-mode' is off.

Returns value of `autopair-mode'. "])

                    ;; py-smart-operator-mode-p forms
                    ("Smart operator mode"
                     :help "Toggle `smart-operator-mode'"

                     ["Toggle smart operator mode" toggle-py-smart-operator-mode-p
                      :help " `toggle-smart-operator-mode'

If `smart-operator-mode' should be on or off.

  Returns value of `smart-operator-mode ' switched to. "]

                     ["Smart operator mode on" py-smart-operator-mode-p-on
                      :help " `smart-operator-mode -on'

Make sure, `smart-operator-mode' is on.

Returns value of `smart-operator-mode'. "]

                     ["Smart operator mode off" py-smart-operator-mode-p-off
                      :help " `smart-operator-mode' off

Make sure, `smart-operator-mode' is off.

Returns value of `smart-operator-mode'. "])

                    ("Filling"

                     ("Docstring styles"
                      :help "Switch docstring-style"

                      ["Nil" py-set-nil-docstring-style
                       :help " `py-set-nil-docstring-style'

Set py-docstring-style to nil, format string normally. "]

                      ["pep-257-nn" py-set-pep-257-nn-docstring-style
                       :help " `py-set-pep-257-nn-docstring-style'

Set py-docstring-style to 'pep-257-nn "]

                      ["pep-257" py-set-pep-257-docstring-style
                       :help " `py-set-pep-257-docstring-style'

Set py-docstring-style to 'pep-257 "]

                      ["django" py-set-django-docstring-style
                       :help " `py-set-django-docstring-style'

Set py-docstring-style to 'django "]

                      ["onetwo" py-set-onetwo-docstring-style
                       :help " `py-set-onetwo-docstring-style'

Set py-docstring-style to 'onetwo "]

                      ["symmetric" py-set-symmetric-docstring-style
                       :help " `py-set-symmetric-docstring-style'

Set py-docstring-style to 'symmetric "])

                     ["Auto-fill mode"
                      (setq py-auto-fill-mode
                            (not py-auto-fill-mode))
                      :help "Fill according to `py-docstring-fill-column' and `py-comment-fill-column'

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-auto-fill-mode])

                    ["Use current dir when execute"
                     (setq py-use-current-dir-when-execute-p
                           (not py-use-current-dir-when-execute-p))
                     :help " `toggle-py-use-current-dir-when-execute-p'

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-use-current-dir-when-execute-p]

                    ("Indent"
		     ("TAB related"

		      ["indent-tabs-mode"
		       (setq indent-tabs-mode
			     (not indent-tabs-mode))
		       :help "Indentation can insert tabs if this is non-nil.

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected indent-tabs-mode]

		      ["Tab indent"
		       (setq py-tab-indent
			     (not py-tab-indent))
		       :help "Non-nil means TAB in Python mode calls `py-indent-line'.Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-indent]

		      ["Tab shifts region "
		       (setq py-tab-shifts-region-p
			     (not py-tab-shifts-region-p))
		       :help "If `t', TAB will indent/cycle the region, not just the current line.

Default is nil
See also `py-tab-indents-region-p'

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-shifts-region-p]

		      ["Tab indents region "
		       (setq py-tab-indents-region-p
			     (not py-tab-indents-region-p))
		       :help "When `t' and first TAB doesn't shift, indent-region is called.

Default is nil
See also `py-tab-shifts-region-p'

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-indents-region-p])

                     ["Close at start column"
                      (setq py-closing-list-dedents-bos
                            (not py-closing-list-dedents-bos))
                      :help "When non-nil, indent list's closing delimiter like start-column.

It will be lined up under the first character of
 the line that starts the multi-line construct, as in:

my_list = \[
    1, 2, 3,
    4, 5, 6,
]

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-dedents-bos]

                     ["Closing list keeps space"
                      (setq py-closing-list-keeps-space
                            (not py-closing-list-keeps-space))
                      :help "If non-nil, closing parenthesis dedents onto column of opening plus `py-closing-list-space', default is nil Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-keeps-space]

                     ["Closing list space"
                      (setq py-closing-list-space
                            (not py-closing-list-space))
                      :help "Number of chars, closing parenthesis outdent from opening, default is 1 Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-space]

                     ["Tab shifts region "
                      (setq py-tab-shifts-region-p
                            (not py-tab-shifts-region-p))
                      :help "If `t', TAB will indent/cycle the region, not just the current line.

Default is nil
See also `py-tab-indents-region-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-tab-shifts-region-p]

                     ["Lhs inbound indent"
                      (setq py-lhs-inbound-indent
                            (not py-lhs-inbound-indent))
                      :help "When line starts a multiline-assignment: How many colums indent should be more than opening bracket, brace or parenthesis. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-lhs-inbound-indent]

                     ["Continuation offset"
                      (setq py-continuation-offset
                            (not py-continuation-offset))
                      :help "With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-continuation-offset]

                     ["Electric colon"
                      (setq py-electric-colon-active-p
                            (not py-electric-colon-active-p))
                      :help " `py-electric-colon-active-p'

`py-electric-colon' feature.  Default is `nil'. See lp:837065 for discussions. "
                      :style toggle :selected py-electric-colon-active-p]

                     ["Electric colon at beginning of block only"
                      (setq py-electric-colon-bobl-only
                            (not py-electric-colon-bobl-only))
                      :help "When inserting a colon, do not indent lines unless at beginning of block.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-colon-bobl-only]

                     ["Electric yank active "
                      (setq py-electric-yank-active-p
                            (not py-electric-yank-active-p))
                      :help " When non-nil, `yank' will be followed by an `indent-according-to-mode'.

Default is nilUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-yank-active-p]

                     ["Electric kill backward "
                      (setq py-electric-kill-backward-p
                            (not py-electric-kill-backward-p))
                      :help "Affects `py-electric-backspace'. Default is nil.

If behind a delimited form of braces, brackets or parentheses,
backspace will kill it's contents

With when cursor after
my_string\[0:1]
--------------^

==>

my_string\[]
----------^

In result cursor is insided emptied delimited form.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-kill-backward-p]

                     ["Trailing whitespace smart delete "
                      (setq py-trailing-whitespace-smart-delete-p
                            (not py-trailing-whitespace-smart-delete-p))
                      :help "Default is nil. When t, python-mode calls
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)

Also commands may delete trailing whitespace by the way.
When editing other peoples code, this may produce a larger diff than expected Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-trailing-whitespace-smart-delete-p]

                     ["Newline delete trailing whitespace "
                      (setq py-newline-delete-trailing-whitespace-p
                            (not py-newline-delete-trailing-whitespace-p))
                      :help "Delete trailing whitespace maybe left by `py-newline-and-indent'.

Default is `t'. See lp:1100892 Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-newline-delete-trailing-whitespace-p]

                     ["Dedent keep relative column"
                      (setq py-dedent-keep-relative-column
                            (not py-dedent-keep-relative-column))
                      :help "If point should follow dedent or kind of electric move to end of line. Default is t - keep relative position. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-dedent-keep-relative-column]

                     ["Indent paren spanned multilines "
                      (setq py-indent-paren-spanned-multilines-p
                            (not py-indent-paren-spanned-multilines-p))
                      :help "If non-nil, indents elements of list a value of `py-indent-offset' to first element:

def foo():
    if (foo &&
            baz):
        bar()

Default lines up with first element:

def foo():
    if (foo &&
        baz):
        bar()
Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-paren-spanned-multilines-p]

                     ["Indent honors multiline listing"
                      (setq py-indent-honors-multiline-listing
                            (not py-indent-honors-multiline-listing))
                      :help "If `t', indents to 1\+ column of opening delimiter. If `nil', indent adds one level to the beginning of statement. Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-honors-multiline-listing]

                     ["Indent comment "
                      (setq py-indent-comments
                            (not py-indent-comments))
                      :help "If comments should be indented like code. Default is `nil'.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-comments]

                     ["Uncomment indents "
                      (setq py-uncomment-indents-p
                            (not py-uncomment-indents-p))
                      :help "When non-nil, after uncomment indent lines. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-uncomment-indents-p]

                     ["Indent honors inline comment"
                      (setq py-indent-honors-inline-comment
                            (not py-indent-honors-inline-comment))
                      :help "If non-nil, indents to column of inlined comment start.
Default is nil. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-honors-inline-comment]

                     ["Kill empty line"
                      (setq py-kill-empty-line
                            (not py-kill-empty-line))
                      :help "If t, py-indent-forward-line kills empty lines. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-kill-empty-line]

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

Use `M-x customize-variable' to set it permanently"])

                     ["Beep if tab change"
                      (setq py-beep-if-tab-change
                            (not py-beep-if-tab-change))
                      :help "Ring the bell if `tab-width' is changed.
If a comment of the form

                           	# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) `tab-width' does not
equal <number>, `tab-width' is set to <number>, a message saying so is
displayed in the echo area, and if `py-beep-if-tab-change' is non-nil
the Emacs bell is also rung as a warning.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-beep-if-tab-change]

                     ["Highlight indentation" highlight-indentation
                      :help "Toggle highlight indentation.

Use `M-x customize-variable' to set it permanently

Make sure `highlight-indentation' is installed"

                      ]

                     ["Electric comment "
                      (setq py-electric-comment-p
                            (not py-electric-comment-p))
                      :help "If \"#\" should call `py-electric-comment'. Default is `nil'.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-comment-p]

                     ["Electric comment add space "
                      (setq py-electric-comment-add-space-p
                            (not py-electric-comment-add-space-p))
                      :help "If py-electric-comment should add a space.  Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-comment-add-space-p]

                     ["Empty line closes "
                      (setq py-empty-line-closes-p
                            (not py-empty-line-closes-p))
                      :help "When non-nil, dedent after empty line following block

if True:
    print(\"Part of the if-statement\")

print(\"Not part of the if-statement\")

Default is nil

If non-nil, a C-j from empty line dedents.
Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-empty-line-closes-p])
                    ["Defun use top level "
                     (setq py-defun-use-top-level-p
                           (not py-defun-use-top-level-p))
                     :help "When non-nil, keys C-M-a, C-M-e address top-level form.

Beginning- end-of-defun forms use
commands `py-beginning-of-top-level', `py-end-of-top-level'

mark-defun marks top-level form at point etc. "
                     :style toggle :selected py-defun-use-top-level-p]

                    ["Close provides newline"
                     (setq py-close-provides-newline
                           (not py-close-provides-newline))
                     :help "If a newline is inserted, when line after block isn't empty. Default is non-nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-close-provides-newline]

                    ["Block comment prefix "
                     (setq py-block-comment-prefix-p
                           (not py-block-comment-prefix-p))
                     :help "If py-comment inserts py-block-comment-prefix.

Default is tUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-block-comment-prefix-p])

                   ("Display"

                    ("Index"

                     ["Imenu create index "
                      (setq py--imenu-create-index-p
                            (not py--imenu-create-index-p))
                      :help "Non-nil means Python mode creates and displays an index menu of functions and global variables. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py--imenu-create-index-p]

                     ["Imenu show method args "
                      (setq py-imenu-show-method-args-p
                            (not py-imenu-show-method-args-p))
                      :help "Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-imenu-show-method-args-p]
                     ["Switch index-function" py-switch-imenu-index-function
                      :help "`py-switch-imenu-index-function'
Switch between `py--imenu-create-index' from 5.1 series and `py--imenu-create-index-new'."])

                    ("Fontification"

                     ["Mark decorators"
                      (setq py-mark-decorators
                            (not py-mark-decorators))
                      :help "If py-mark-def-or-class functions should mark decorators too. Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-mark-decorators]

                     ["Fontify shell buffer "
                      (setq py-fontify-shell-buffer-p
                            (not py-fontify-shell-buffer-p))
                      :help "If code in Python shell should be highlighted as in script buffer.

Default is nil.

If `t', related vars like `comment-start' will be set too.
Seems convenient when playing with stuff in IPython shell
Might not be TRT when a lot of output arrives Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-fontify-shell-buffer-p]

                     ["Use font lock doc face "
                      (setq py-use-font-lock-doc-face-p
                            (not py-use-font-lock-doc-face-p))
                      :help "If documention string inside of def or class get `font-lock-doc-face'.

`font-lock-doc-face' inherits `font-lock-string-face'.

Call M-x `customize-face' in order to have a visible effect. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-font-lock-doc-face-p])

                    ["Modeline display full path "
                     (setq py-modeline-display-full-path-p
                           (not py-modeline-display-full-path-p))
                     :help "If the full PATH/TO/PYTHON should be displayed in shell modeline.

Default is nil. Note: when `py-shell-name' is specified with path, it's shown as an acronym in buffer-name already. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-modeline-display-full-path-p]

                    ["Modeline acronym display home "
                     (setq py-modeline-acronym-display-home-p
                           (not py-modeline-acronym-display-home-p))
                     :help "If the modeline acronym should contain chars indicating the home-directory.

Default is nil Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-modeline-acronym-display-home-p]

                    ["Hide show hide docstrings"
                     (setq py-hide-show-hide-docstrings
                           (not py-hide-show-hide-docstrings))
                     :help "Controls if doc strings can be hidden by hide-showUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-hide-show-hide-docstrings]

                    ["Hide comments when hiding all"
                     (setq py-hide-comments-when-hiding-all
                           (not py-hide-comments-when-hiding-all))
                     :help "Hide the comments too when you do `hs-hide-all'. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-hide-comments-when-hiding-all]

                    ["Max help buffer "
                     (setq py-max-help-buffer-p
                           (not py-max-help-buffer-p))
                     :help "If \"\*Python-Help\*\"-buffer should appear as the only visible.

Default is nil. In help-buffer, \"q\" will close it.  Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-max-help-buffer-p]

                    ["Current defun show"
                     (setq py-current-defun-show
                           (not py-current-defun-show))
                     :help "If `py-current-defun' should jump to the definition, highlight it while waiting PY-WHICH-FUNC-DELAY seconds, before returning to previous position.

Default is `t'.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-current-defun-show]

                    ["Match paren mode"
                     (setq py-match-paren-mode
                           (not py-match-paren-mode))
                     :help "Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets `py-match-paren-key' in py-shell-mode-map.
Customize `py-match-paren-key' which key to use. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-match-paren-mode])

                   ("Debug"

		    ["py-debug-p"
		     (setq py-debug-p
			   (not py-debug-p))
		     :help "When non-nil, keep resp\. store information useful for debugging\.

Temporary files are not deleted\. Other functions might implement
some logging etc\. Use `M-x customize-variable' to set it permanently"
		     :style toggle :selected py-debug-p]

                    ["Pdbtrack do tracking "
                     (setq py-pdbtrack-do-tracking-p
                           (not py-pdbtrack-do-tracking-p))
                     :help "Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the \*Python\* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-pdbtrack-do-tracking-p]

                    ["Jump on exception"
                     (setq py-jump-on-exception
                           (not py-jump-on-exception))
                     :help "Jump to innermost exception frame in Python output buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-jump-on-exception]

                    ["Highlight error in source "
                     (setq py-highlight-error-source-p
                           (not py-highlight-error-source-p))
                     :help "Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-highlight-error-source-p])

                   ("Other"

                    ("Directory"

                     ["Guess install directory "
                      (setq py-guess-py-install-directory-p
                            (not py-guess-py-install-directory-p))
                      :help "If in cases, `py-install-directory' isn't set,  `py-set-load-path'should guess it from `buffer-file-name'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-guess-py-install-directory-p]

                     ["Use local default"
                      (setq py-use-local-default
                            (not py-use-local-default))
                      :help "If `t', py-shell will use `py-shell-local-path' instead
of default Python.

Making switch between several virtualenv's easier,
                               `python-mode' should deliver an installer, so named-shells pointing to virtualenv's will be available. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-local-default]

                     ["Use current dir when execute "
                      (setq py-use-current-dir-when-execute-p
                            (not py-use-current-dir-when-execute-p))
                      :help "When `t', current directory is used by Python-shell for output of `py-execute-buffer' and related commands.

See also `py-execute-directory'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-current-dir-when-execute-p]

                     ["Keep shell dir when execute "
                      (setq py-keep-shell-dir-when-execute-p
                            (not py-keep-shell-dir-when-execute-p))
                      :help "Don't change Python shell's current working directory when sending code.

See also `py-execute-directory'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-keep-shell-dir-when-execute-p]

                     ["Fileless buffer use default directory "
                      (setq py-fileless-buffer-use-default-directory-p
                            (not py-fileless-buffer-use-default-directory-p))
                      :help "When `py-use-current-dir-when-execute-p' is non-nil and no buffer-file exists, value of `default-directory' sets current working directory of Python output shellUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-fileless-buffer-use-default-directory-p])

                    ("Underscore word syntax"
                     :help "Toggle `py-underscore-word-syntax-p'"

                     ["Toggle underscore word syntax" toggle-py-underscore-word-syntax-p
                      :help " `toggle-py-underscore-word-syntax-p'

If `py-underscore-word-syntax-p' should be on or off.

  Returns value of `py-underscore-word-syntax-p' switched to. .

Use `M-x customize-variable' to set it permanently"]

                     ["Underscore word syntax on" py-underscore-word-syntax-p-on
                      :help " `py-underscore-word-syntax-p-on'

Make sure, py-underscore-word-syntax-p' is on.

Returns value of `py-underscore-word-syntax-p'. .

Use `M-x customize-variable' to set it permanently"]

                     ["Underscore word syntax off" py-underscore-word-syntax-p-off
                      :help " `py-underscore-word-syntax-p-off'

Make sure, `py-underscore-word-syntax-p' is off.

Returns value of `py-underscore-word-syntax-p'. .

Use `M-x customize-variable' to set it permanently"])

                    ["Load pymacs "
                     (setq py-load-pymacs-p
                           (not py-load-pymacs-p))
                     :help "If Pymacs related stuff should be loaded.

Default is nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.caUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-load-pymacs-p]

                    ["Verbose "
                     (setq py-verbose-p
                           (not py-verbose-p))
                     :help "If functions should report results.

Default is nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-verbose-p]

                    ["Empty comment line separates paragraph "
                     (setq empty-comment-line-separates-paragraph-p
                           (not empty-comment-line-separates-paragraph-p))
                     :help "Consider paragraph start/end lines with nothing inside but comment sign.

Default is non-nilUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected empty-comment-line-separates-paragraph-p]

                    ["Org cycle "
                     (setq py-org-cycle-p
                           (not py-org-cycle-p))
                     :help "When non-nil, command `org-cycle' is available at shift-TAB, <backtab>

Default is nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-org-cycle-p]

                    ["Set pager cat"
                     (setq py-set-pager-cat-p
                           (not py-set-pager-cat-p))
                     :help "If the shell environment variable \$PAGER should set to `cat'.

If `t', use `C-c C-r' to jump to beginning of output. Then scroll normally.

Avoids lp:783828, \"Terminal not fully functional\", for help('COMMAND') in python-shell

When non-nil, imports module `os' Use `M-x customize-variable' to
set it permanently"
                     :style toggle :selected py-set-pager-cat-p]

                    ["Edit only "
                     (setq py-edit-only-p
                           (not py-edit-only-p))
                     :help "When `t' `python-mode' will not take resort nor check for installed Python executables. Default is nil.

See bug report at launchpad, lp:944093. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-edit-only-p])))

                 ("More... "

                  ("Edit commands "

		   ("Hide"
		    ["Hide statement" py-hide-statement
		     :help " `py-hide-statement'

Hide statement at point\. "]

		    ["Hide block" py-hide-block
		     :help " `py-hide-block'

Hide block at point\. "]

		    ["Hide clause" py-hide-clause
		     :help " `py-hide-clause'

Hide clause at point\. "]

		    ["Hide block or clause" py-hide-block-or-clause
		     :help " `py-hide-block-or-clause'

Hide block-or-clause at point\. "]

		    ["Hide def" py-hide-def
		     :help " `py-hide-def'

Hide def at point\. "]

		    ["Hide class" py-hide-class
		     :help " `py-hide-class'

Hide class at point\. "]

		    ["Hide expression" py-hide-expression
		     :help " `py-hide-expression'

Hide expression at point\. "]

		    ["Hide partial expression" py-hide-partial-expression
		     :help " `py-hide-partial-expression'

Hide partial-expression at point\. "]

		    ["Hide line" py-hide-line
		     :help " `py-hide-line'

Hide line at point\. "]

		    ["Hide top level" py-hide-top-level
		     :help " `py-hide-top-level'

Hide top-level at point\. "])

		   ("Show"

		    ["Show statement" py-show-statement
		     :help " `py-show-statement'

Show statement at point\. "]

		    ["Show block" py-show-block
		     :help " `py-show-block'

Show block at point\. "]

		    ["Show clause" py-show-clause
		     :help " `py-show-clause'

Show clause at point\. "]

		    ["Show block or clause" py-show-block-or-clause
		     :help " `py-show-block-or-clause'

Show block-or-clause at point\. "]

		    ["Show def" py-show-def
		     :help " `py-show-def'

Show def at point\. "]

		    ["Show class" py-show-class
		     :help " `py-show-class'

Show class at point\. "]

		    ["Show expression" py-show-expression
		     :help " `py-show-expression'

Show expression at point\. "]

		    ["Show partial expression" py-show-partial-expression
		     :help " `py-show-partial-expression'

Show partial-expression at point\. "]

		    ["Show line" py-show-line
		     :help " `py-show-line'

Show line at point\. "]

		    ["Show top level" py-show-top-level
		     :help " `py-show-top-level'

Show top-level at point\. "])

                   ("Kill "

                    ["Kill statement" py-kill-statement
                     :help "`py-kill-statement'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill top level" py-kill-top-level
                     :help " `py-kill-top-level'

Delete top-level form at point.

Stores data in kill ring. Might be yanked back using `C-y'. "]

                    ["Kill clause" py-kill-clause
                     :help "`py-kill-clause'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill block" py-kill-block
                     :help "`py-kill-block'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill minor block" py-kill-minor-block
                     :help " `py-kill-minor-block'

Delete minor-block at point.

Stores data in kill ring. Might be yanked back using `C-y'. "]

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

                   ("Delete"
                    ["Delete statement " py-delete-statement
                     :help "`py-delete-statement'
Delete STATEMENT at point, don't store in kill-ring. "]

                    ["Delete top-level " py-delete-top-level
                     :help "`py-delete-top-level'
Delete TOP-LEVEL at point, don't store in kill-ring. "]

                    ["Delete block " py-delete-block
                     :help "`py-delete-block'
Delete BLOCK at point, don't store in kill-ring. "]

                    ["Delete block-or-clause " py-delete-block-or-clause
                     :help "`py-delete-block-or-clause'
Delete BLOCK-OR-CLAUSE at point, don't store in kill-ring. "]

                    ["Delete def " py-delete-def
                     :help "`py-delete-def'
Delete DEF at point, don't store in kill-ring. "]

                    ["Delete class " py-delete-class
                     :help "`py-delete-class'
Delete CLASS at point, don't store in kill-ring. "]

                    ["Delete def-or-class " py-delete-def-or-class
                     :help "`py-delete-def-or-class'
Delete DEF-OR-CLASS at point, don't store in kill-ring. "]

                    ["Delete expression " py-delete-expression
                     :help "`py-delete-expression'
Delete EXPRESSION at point, don't store in kill-ring. "]

                    ["Delete partial-expression " py-delete-partial-expression
                     :help "`py-delete-partial-expression'
Delete PARTIAL-EXPRESSION at point, don't store in kill-ring. "]

                    ["Delete minor-block " py-delete-minor-block
                     :help "`py-delete-minor-block'
Delete MINOR-BLOCK at point, don't store in kill-ring.

A minor block is started by a `for', `if', `try' or `with'. "])
                   "-"

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

                    ["Shift minor block right" py-shift-minor-block-right
                     :help " `py-shift-minor-block-right'

Indent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "]

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
Shift block-or-clause right. "]

                    ["Shift region left" py-shift-region-left
                     :help " `py-shift-region-left'

Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. "]

                    ["Shift region right" py-shift-region-right
                     :help " `py-shift-region-right'

Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. "])

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

                    ["Shift minor block left" py-shift-minor-block-left
                     :help " `py-shift-minor-block-left'

Dedent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "]

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
Shift block-or-clause left. "])
                   ("More"
                    :help "extended edit commands'"

                    ["Kill buffer unconditional" py-kill-buffer-unconditional
                     :help " `py-kill-buffer-unconditional'

Kill buffer unconditional, kill buffer-process if existing\. "]

                    ["Empty out list backward" py-empty-out-list-backward
                     :help " `py-empty-out-list-backward'
Deletes all elements from list before point. "]

                    ["Revert boolean assignent" py-boolswitch
                     :help " `py-boolswitch'
Edit the assigment of a boolean variable, rever them.

I.e. switch it from \"True\" to \"False\" and vice versa "]

                    ["Remove overlays at point" py-remove-overlays-at-point
                     :help " `py-remove-overlays-at-point'

Remove overlays as set when `py-highlight-error-source-p' is non-nil. "]))

                  "-"
                  ("Forms "
                   ("Comment"

                    ["Beginning of comment" py-beginning-of-comment
                     :help " `py-beginning-of-comment'
Go to beginning of comment at point. "]

                    ["End of comment" py-end-of-comment
                     :help " `py-end-of-comment'

Go to end of comment at point. "])
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

Comments block at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Top-level form"

                    ["Beginning of top-level form" py-beginning-of-top-level
                     :help "`py-beginning-of-top-level'
Go to start of top-level form form at point"]

                    ["End of top-level form" py-end-of-top-level
                     :help "`py-end-of-top-level'
Go to end of top-level form at point"]

                    ["Down top-level form" py-down-top-level
                     :help "`py-down-top-level'

Go to the beginning of top-level form below in buffer. "]

                    ["Up top-level form" py-up-top-level
                     :help "`py-up-top-level'

Go upwards to the beginning of next top-level form in buffer. "]

                    ["Copy top-level form" py-copy-top-level
                     :help "`py-copy-top-level'
Copy innermost top-level form at point"]

                    ["Kill top-level form" py-kill-top-level
                     :help "`py-kill-top-level'
Delete top-level form at point, store deleted string in kill-ring"]

                    ["Delete top-level form" py-delete-top-level
                     :help "`py-delete-top-level'
Delete top-level form at point, don't store deleted string in kill-ring"]

                    ["Comment top-level form" py-comment-top-level
                     :help " `py-comment-top-level'

Comments top-level form at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Minor-block"

                    ["Beginning of minor-block" py-beginning-of-minor-block
                     :help "`py-beginning-of-minor-block'
Go to start of innermost minor-block at point"]
                    ["End of minor-block" py-end-of-minor-block
                     :help "`py-end-of-minor-block'
Go to end of innermost minor-block at point"]

                    ["Down minor-block" py-down-minor-block
                     :help "`py-down-minor-block'

Go to the beginning of next minor-block below in buffer.

Returns indentation if minor-block found, nil otherwise. "]

                    ["Up minor-block" py-up-minor-block
                     :help "`py-up-minor-block'

Go upwards to the beginning of next minor-block below in buffer.

Returns indentation if minor-block found, nil otherwise. "]

                    ["Copy minor-block" py-copy-minor-block
                     :help "`py-copy-minor-block'
Copy innermost minor-block at point"]

                    ["Kill minor-block" py-kill-minor-block
                     :help "`py-kill-minor-block'
Delete innermost minor-block at point, store deleted string in kill-ring"]

                    ["Delete minor-block" py-delete-minor-block
                     :help "`py-delete-minor-block'
Delete innermost minor-block at point, don't store deleted string in kill-ring"]

                    ["Shift minor-block right" py-shift-minor-block-right
                     :help "`py-shift-minor-block-right'
Shift minor-block right. "]

                    ["Shift minor-block left" py-shift-minor-block-left
                     :help "`py-shift-minor-block-left'
Shift minor-block left. "]

                    ["Comment minor-block" py-comment-minor-block
                     :help " `py-comment-minor-block'

Comments minor-block at point.

Uses double hash (`#') comment starter when `py-minor-block-comment-prefix-p' is `t',
the default. "])

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

Comments def-or-class at point.

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

Comments clause at point.

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

Comments statement at point.

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

Comments class at point.

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

Comments def at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   "-"

                   ("Block bol "

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

                   ("Minor-block bol "

                    ["Beginning of minor-block bol" py-beginning-of-minor-block-bol
                     :help "`py-beginning-of-minor-block-bol'
Go to beginning of line at beginning of minor-block.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["End of minor-block bol" py-end-of-minor-block-bol
                     :help "`py-end-of-minor-block-bol'
Go to beginning of line following end of minor-block.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Up minor-block bol" py-up-minor-block-bol
                     :help "`py-up-minor-block-bol'
Go to next minor-block upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Down minor-block bol" py-down-minor-block-bol
                     :help "`py-down-minor-block-bol'
Go to next minor-block downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Mark minor-block bol" py-mark-minor-block-bol
                     :help "`py-mark-minor-block-bol'
Mark minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Copy minor-block bol" py-copy-minor-block-bol
                     :help "`py-copy-minor-block-bol'
Copy minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Kill minor-block bol" py-kill-minor-block-bol
                     :help "`py-kill-minor-block-bol'
Kill minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Delete minor-block bol" py-delete-minor-block-bol
                     :help "`py-delete-minor-block-bol'
Delete minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Shift minor-block right" py-shift-minor-block-right
                     :help "`py-shift-minor-block-right'
Shift minor-block right.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Shift minor-block left" py-shift-minor-block-left
                     :help "`py-shift-minor-block-left'
Shift minor-block left.

A minor block is started by a `for', `if', `try' or `with'. "])

                   ("Clause bol "

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

                   ("Block-Or-Clause bol "

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

                   ("Def bol "

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

                   ("Class bol "
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

                   ("Def-Or-Class bol "
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

                   ("Statement bol "
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
Shift statement left. "]))
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
back to the previous non-whitespace character.
See also C-c <delete>. "]

                   ["Hungry delete forward" py-hungry-delete-forward
                    :help " `py-hungry-delete-forward'

Delete the following character or all following whitespace
up to the next non-whitespace character.
See also C-c <C-backspace>. "]

                   ["Electric colon" py-electric-colon
                    :help " `py-electric-colon'
Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix C-u.

Switched by `py-electric-colon-active-p', default is nil
See also `py-electric-colon-greedy-p' "]

                   ["Electric colon greedy "
                    (setq py-electric-colon-greedy-p
                          (not py-electric-colon-greedy-p))
                    :help "If py-electric-colon should indent to the outmost reasonable level.

If nil, default, it will not move from at any reasonable level. Use `M-x customize-variable' to set it permanently"
                    :style toggle :selected py-electric-colon-greedy-p]

                   ["Electric colon newline and indent "
                    (setq py-electric-colon-newline-and-indent-p
                          (not py-electric-colon-newline-and-indent-p))
                    :help "If non-nil, `py-electric-colon' will call `newline-and-indent'.  Default is `nil'. Use `M-x customize-variable' to set it permanently"
                    :style toggle :selected py-electric-colon-newline-and-indent-p]

                   ["Electric delete" py-electric-delete
                    :help " `py-electric-delete'
Delete following character or levels of whitespace.

With ARG do that ARG times. "]

                   ["Electric backspace" py-electric-backspace
                    :help " `py-electric-backspace'
Delete preceding character or level of indentation.

With ARG do that ARG times.
Returns column reached. "]

                   ["Electric comment" py-electric-comment
                    :help " `py-electric-comment'
Insert a comment. If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many \"#\" are inserted
non-electrically.
With C-u \"#\" electric behavior is inhibited inside a string or comment. "]

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
                    :help "Inserts py-try/except-statement"])

                  ["Find function" py-find-function
                   :help "`py-find-function'
Try to find source definition of function at point"]))))
        map))

(and py-load-skeletons-p (require 'python-components-skeletons))
(and py-company-pycomplete-p (require 'company-pycomplete))

;; avoid errors from ipython.el - which isn't needed anymore
(defvaralias 'py-shell-map 'py-shell-mode-map)

(defun py-report-comint-variable-setting ()
  "Print values of comint-variables.

Used for debugging in \"*Python*\" shell buffer for example"
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (set-buffer "*Help*")
    (and (boundp 'comint-accum-marker)
	 (insert (concat "comint-accum-marker" " --> " (prin1-to-string comint-accum-marker) "\n")))
    (and (boundp 'comint-buffer-maximum-size)
	 (insert (concat "comint-buffer-maximum-size" " --> " (prin1-to-string comint-buffer-maximum-size) "\n")))
    (and (boundp 'comint-completion-addsuffix)
	 (insert (concat "comint-completion-addsuffix" " --> " (prin1-to-string comint-completion-addsuffix) "\n")))
    (and (boundp 'comint-completion-autolist)
	 (insert (concat "comint-completion-autolist" " --> " (prin1-to-string comint-completion-autolist) "\n")))
    (and (boundp 'comint-completion-fignore)
	 (insert (concat "comint-completion-fignore" " --> " (prin1-to-string comint-completion-fignore) "\n")))
    (and (boundp 'comint-completion-recexact)
	 (insert (concat "comint-completion-recexact" " --> " (prin1-to-string comint-completion-recexact) "\n")))
    (and (boundp 'comint-delimiter-argument-list)
	 (insert (concat "comint-delimiter-argument-list" " --> " (prin1-to-string comint-delimiter-argument-list) "\n")))
    (and (boundp 'comint-displayed-dynamic-completions)
	 (insert (concat "comint-displayed-dynamic-completions" " --> " (prin1-to-string comint-displayed-dynamic-completions) "\n")))
    (and (boundp 'comint-dynamic-complete-functions)
	 (insert (concat "comint-dynamic-complete-functions" " --> " (prin1-to-string comint-dynamic-complete-functions) "\n")))
    (and (boundp 'comint-dynamic-list-completions-config)
	 (insert (concat "comint-dynamic-list-completions-config" " --> " (prin1-to-string comint-dynamic-list-completions-config) "\n")))
    (and (boundp 'comint-eol-on-send)
	 (insert (concat "comint-eol-on-send" " --> " (prin1-to-string comint-eol-on-send) "\n")))
    (and (boundp 'comint-exec-hook)
	 (insert (concat "comint-exec-hook" " --> " (prin1-to-string comint-exec-hook) "\n")))
    (and (boundp 'comint-file-name-chars)
	 (insert (concat "comint-file-name-chars" " --> " (prin1-to-string comint-file-name-chars) "\n")))
    (and (boundp 'comint-file-name-prefix)
	 (insert (concat "comint-file-name-prefix" " --> " (prin1-to-string comint-file-name-prefix) "\n")))
    (and (boundp 'comint-file-name-quote-list)
	 (insert (concat "comint-file-name-quote-list" " --> " (prin1-to-string comint-file-name-quote-list) "\n")))
    (and (boundp 'comint-get-old-input)
	 (insert (concat "comint-get-old-input" " --> " (prin1-to-string comint-get-old-input) "\n")))
    (and (boundp 'comint-history-isearch)
	 (insert (concat "comint-history-isearch" " --> " (prin1-to-string comint-history-isearch) "\n")))
    (and (boundp 'comint-history-isearch-message-overlay)
	 (insert (concat "comint-history-isearch-message-overlay" " --> " (prin1-to-string comint-history-isearch-message-overlay) "\n")))
    (and (boundp 'comint-inhibit-carriage-motion)
	 (insert (concat "comint-inhibit-carriage-motion" " --> " (prin1-to-string comint-inhibit-carriage-motion) "\n")))
    (and (boundp 'comint-input-autoexpand)
	 (insert (concat "comint-input-autoexpand" " --> " (prin1-to-string comint-input-autoexpand) "\n")))
    (and (boundp 'comint-input-filter)
	 (insert (concat "comint-input-filter" " --> " (prin1-to-string comint-input-filter) "\n")))
    (and (boundp 'comint-input-filter-functions)
	 (insert (concat "comint-input-filter-functions" " --> " (prin1-to-string comint-input-filter-functions) "\n")))
    (and (boundp 'comint-input-history-ignore)
	 (insert (concat "comint-input-history-ignore" " --> " (prin1-to-string comint-input-history-ignore) "\n")))
    (and (boundp 'comint-input-ignoredups)
	 (insert (concat "comint-input-ignoredups" " --> " (prin1-to-string comint-input-ignoredups) "\n")))
    ;; (insert (concat "comint-input-ring" " --> " (prin1-to-string comint-input-ring) "\n")))
    (and (boundp 'comint-input-ring-file-name)
	 (insert (concat "comint-input-ring-file-name" " --> " (prin1-to-string comint-input-ring-file-name) "\n")))
    (and (boundp 'comint-input-ring-index)
	 (insert (concat "comint-input-ring-index" " --> " (prin1-to-string comint-input-ring-index) "\n")))
    (and (boundp 'comint-input-ring-separator)
	 (insert (concat "comint-input-ring-separator" " --> " (prin1-to-string comint-input-ring-separator) "\n")))
    (and (boundp 'comint-input-ring-size)
	 (insert (concat "comint-input-ring-size" " --> " (prin1-to-string comint-input-ring-size) "\n")))
    (and (boundp 'comint-input-sender)
	 (insert (concat "comint-input-sender" " --> " (prin1-to-string comint-input-sender) "\n")))
    (and (boundp 'comint-input-sender-no-newline)
	 (insert (concat "comint-input-sender-no-newline" " --> " (prin1-to-string comint-input-sender-no-newline) "\n")))
    (and (boundp 'comint-insert-previous-argument-last-index)
	 (insert (concat "comint-insert-previous-argument-last-index" " --> " (prin1-to-string comint-insert-previous-argument-last-index) "\n")))
    (and (boundp 'comint-insert-previous-argument-last-start-pos)
	 (insert (concat "comint-insert-previous-argument-last-start-pos" " --> " (prin1-to-string comint-insert-previous-argument-last-start-pos) "\n")))
    (and (boundp 'comint-last-input-end)
	 (insert (concat "comint-last-input-end" " --> " (prin1-to-string comint-last-input-end) "\n")))
    (and (boundp 'comint-last-input-start)
	 (insert (concat "comint-last-input-start" " --> " (prin1-to-string comint-last-input-start) "\n")))
    (and (boundp 'comint-last-output-start)
	 (insert (concat "comint-last-output-start" " --> " (prin1-to-string comint-last-output-start) "\n")))
    (and (boundp 'comint-last-prompt-overlay)
	 (insert (concat "comint-last-prompt-overlay" " --> " (prin1-to-string comint-last-prompt-overlay) "\n")))
    (and (boundp 'comint-matching-input-from-input-string)
	 (insert (concat "comint-matching-input-from-input-string" " --> " (prin1-to-string comint-matching-input-from-input-string) "\n")))
    ;; (insert (concat "comint-mode-abbrev-table" " --> " (prin1-to-string comint-mode-abbrev-table) "\n")))
    (and (boundp 'comint-mode-hook)
	 (insert (concat "comint-mode-hook" " --> " (prin1-to-string comint-mode-hook) "\n")))
    ;; (insert (concat "comint-mode-map" " --> " (prin1-to-string comint-mode-map) "\n")))
    ;; (insert (concat "comint-mode-syntax-table" " --> " (prin1-to-string comint-mode-syntax-table) "\n")))
    (and (boundp 'comint-move-point-for-output)
	 (insert (concat "comint-move-point-for-output" " --> " (prin1-to-string comint-move-point-for-output) "\n")))
    (and (boundp 'comint-output-filter-functions)
	 (insert (concat "comint-output-filter-functions" " --> " (prin1-to-string comint-output-filter-functions) "\n")))
    (and (boundp 'comint-password-prompt-regexp)
	 (insert (concat "comint-password-prompt-regexp" " --> " (prin1-to-string comint-password-prompt-regexp) "\n")))
    (and (boundp 'comint-preoutput-filter-functions)
	 (insert (concat "comint-preoutput-filter-functions" " --> " (prin1-to-string comint-preoutput-filter-functions) "\n")))
    (and (boundp 'comint-process-echoes)
	 (insert (concat "comint-process-echoes" " --> " (prin1-to-string comint-process-echoes) "\n")))
    (and (boundp 'comint-prompt-read-only)
	 (insert (concat "comint-prompt-read-only" " --> " (prin1-to-string comint-prompt-read-only) "\n")))
    (and (boundp 'comint-prompt-regexp)
	 (insert (concat "comint-prompt-regexp" " --> " (prin1-to-string comint-prompt-regexp) "\n")))
    (and (boundp 'comint-ptyp)
	 (insert (concat "comint-ptyp" " --> " (prin1-to-string comint-ptyp) "\n")))
    (and (boundp 'comint-redirect-completed)
	 (insert (concat "comint-redirect-completed" " --> " (prin1-to-string comint-redirect-completed) "\n")))
    (and (boundp 'comint-redirect-echo-input)
	 (insert (concat "comint-redirect-echo-input" " --> " (prin1-to-string comint-redirect-echo-input) "\n")))
    (and (boundp 'comint-redirect-filter-functions)
	 (insert (concat "comint-redirect-filter-functions" " --> " (prin1-to-string comint-redirect-filter-functions) "\n")))
    (and (boundp 'comint-redirect-finished-regexp)
	 (insert (concat "comint-redirect-finished-regexp" " --> " (prin1-to-string comint-redirect-finished-regexp) "\n")))
    (and (boundp 'comint-redirect-insert-matching-regexp)
	 (insert (concat "comint-redirect-insert-matching-regexp" " --> " (prin1-to-string comint-redirect-insert-matching-regexp) "\n")))
    (and (boundp 'comint-redirect-original-filter-function)
	 (insert (concat "comint-redirect-original-filter-function" " --> " (prin1-to-string comint-redirect-original-filter-function) "\n")))
    (and (boundp 'comint-redirect-original-mode-line-process)
	 (insert (concat "comint-redirect-original-mode-line-process" " --> " (prin1-to-string comint-redirect-original-mode-line-process) "\n")))
    (and (boundp 'comint-redirect-output-buffer)
	 (insert (concat "comint-redirect-output-buffer" " --> " (prin1-to-string comint-redirect-output-buffer) "\n")))
    (and (boundp 'comint-redirect-perform-sanity-check)
	 (insert (concat "comint-redirect-perform-sanity-check" " --> " (prin1-to-string comint-redirect-perform-sanity-check) "\n")))
    (and (boundp 'comint-redirect-subvert-readonly)
	 (insert (concat "comint-redirect-subvert-readonly" " --> " (prin1-to-string comint-redirect-subvert-readonly) "\n")))
    (and (boundp 'comint-redirect-verbose)
	 (insert (concat "comint-redirect-verbose" " --> " (prin1-to-string comint-redirect-verbose) "\n")))
    (and (boundp 'comint-save-input-ring-index)
	 (insert (concat "comint-save-input-ring-index" " --> " (prin1-to-string comint-save-input-ring-index) "\n")))
    (and (boundp 'comint-scroll-show-maximum-output)
	 (insert (concat "comint-scroll-show-maximum-output" " --> " (prin1-to-string comint-scroll-show-maximum-output) "\n")))
    (and (boundp 'comint-scroll-to-bottom-on-input)
	 (insert (concat "comint-scroll-to-bottom-on-input" " --> " (prin1-to-string comint-scroll-to-bottom-on-input) "\n")))
    (and (boundp 'comint-scroll-to-bottom-on-output)
	 (insert (concat "comint-scroll-to-bottom-on-output" " --> " (prin1-to-string comint-scroll-to-bottom-on-output) "\n")))
    (and (boundp 'comint-stored-incomplete-input)
	 (insert (concat "comint-stored-incomplete-input" " --> " (prin1-to-string comint-stored-incomplete-input) "\n")))
    (and (boundp 'comint-use-prompt-regexp)
	 (insert (concat "comint-use-prompt-regexp" " --> " (prin1-to-string comint-use-prompt-regexp) "\n")))
    (and (boundp 'comint-use-prompt-regexp-instead-of-fields)
	 (insert (concat "comint-use-prompt-regexp-instead-of-fields" " --> " (prin1-to-string comint-use-prompt-regexp-instead-of-fields) "\n")))))

(defun py--report-comint-variable-setting-intern ()
  (insert (concat "comint-accum-marker" " --> " (prin1-to-string comint-accum-marker) "\n"))
  (insert (concat "comint-buffer-maximum-size" " --> " (prin1-to-string comint-buffer-maximum-size) "\n"))
  (insert (concat "comint-completion-addsuffix" " --> " (prin1-to-string comint-completion-addsuffix) "\n"))
  (insert (concat "comint-completion-autolist" " --> " (prin1-to-string comint-completion-autolist) "\n"))
  (insert (concat "comint-completion-fignore" " --> " (prin1-to-string comint-completion-fignore) "\n"))
  (insert (concat "comint-completion-recexact" " --> " (prin1-to-string comint-completion-recexact) "\n"))
  (insert (concat "comint-delimiter-argument-list" " --> " (prin1-to-string comint-delimiter-argument-list) "\n"))
  (insert (concat "comint-displayed-dynamic-completions" " --> " (prin1-to-string comint-displayed-dynamic-completions) "\n"))
  (insert (concat "comint-dynamic-complete-functions" " --> " (prin1-to-string comint-dynamic-complete-functions) "\n"))
  (insert (concat "comint-dynamic-list-completions-config" " --> " (prin1-to-string comint-dynamic-list-completions-config) "\n"))
  (insert (concat "comint-eol-on-send" " --> " (prin1-to-string comint-eol-on-send) "\n"))
  (insert (concat "comint-exec-hook" " --> " (prin1-to-string comint-exec-hook) "\n"))
  (insert (concat "comint-file-name-chars" " --> " (prin1-to-string comint-file-name-chars) "\n"))
  (insert (concat "comint-file-name-prefix" " --> " (prin1-to-string comint-file-name-prefix) "\n"))
  (insert (concat "comint-file-name-quote-list" " --> " (prin1-to-string comint-file-name-quote-list) "\n"))
  (insert (concat "comint-get-old-input" " --> " (prin1-to-string comint-get-old-input) "\n"))
  (insert (concat "comint-history-isearch" " --> " (prin1-to-string comint-history-isearch) "\n"))
  (insert (concat "comint-history-isearch-message-overlay" " --> " (prin1-to-string comint-history-isearch-message-overlay) "\n"))
  (insert (concat "comint-inhibit-carriage-motion" " --> " (prin1-to-string comint-inhibit-carriage-motion) "\n"))
  (insert (concat "comint-input-autoexpand" " --> " (prin1-to-string comint-input-autoexpand) "\n"))
  (insert (concat "comint-input-filter" " --> " (prin1-to-string comint-input-filter) "\n"))
  (insert (concat "comint-input-filter-functions" " --> " (prin1-to-string comint-input-filter-functions) "\n"))
  (insert (concat "comint-input-history-ignore" " --> " (prin1-to-string comint-input-history-ignore) "\n"))
  (insert (concat "comint-input-ignoredups" " --> " (prin1-to-string comint-input-ignoredups) "\n"))
  ;; (insert (concat "comint-input-ring" " --> " (prin1-to-string comint-input-ring) "\n"))
  (insert (concat "comint-input-ring-file-name" " --> " (prin1-to-string comint-input-ring-file-name) "\n"))
  (insert (concat "comint-input-ring-index" " --> " (prin1-to-string comint-input-ring-index) "\n"))
  (insert (concat "comint-input-ring-separator" " --> " (prin1-to-string comint-input-ring-separator) "\n"))
  (insert (concat "comint-input-ring-size" " --> " (prin1-to-string comint-input-ring-size) "\n"))
  (insert (concat "comint-input-sender" " --> " (prin1-to-string comint-input-sender) "\n"))
  (insert (concat "comint-input-sender-no-newline" " --> " (prin1-to-string comint-input-sender-no-newline) "\n"))
  (insert (concat "comint-insert-previous-argument-last-index" " --> " (prin1-to-string comint-insert-previous-argument-last-index) "\n"))
  (insert (concat "comint-insert-previous-argument-last-start-pos" " --> " (prin1-to-string comint-insert-previous-argument-last-start-pos) "\n"))
  (insert (concat "comint-last-input-end" " --> " (prin1-to-string comint-last-input-end) "\n"))
  (insert (concat "comint-last-input-start" " --> " (prin1-to-string comint-last-input-start) "\n"))
  (insert (concat "comint-last-output-start" " --> " (prin1-to-string comint-last-output-start) "\n"))
  (insert (concat "comint-last-prompt-overlay" " --> " (prin1-to-string comint-last-prompt-overlay) "\n"))
  (insert (concat "comint-matching-input-from-input-string" " --> " (prin1-to-string comint-matching-input-from-input-string) "\n"))
  ;; (insert (concat "comint-mode-abbrev-table" " --> " (prin1-to-string comint-mode-abbrev-table) "\n"))
  (insert (concat "comint-mode-hook" " --> " (prin1-to-string comint-mode-hook) "\n"))
  ;; (insert (concat "comint-mode-map" " --> " (prin1-to-string comint-mode-map) "\n"))
  ;; (insert (concat "comint-mode-syntax-table" " --> " (prin1-to-string comint-mode-syntax-table) "\n"))
  (insert (concat "comint-move-point-for-output" " --> " (prin1-to-string comint-move-point-for-output) "\n"))
  (insert (concat "comint-output-filter-functions" " --> " (prin1-to-string comint-output-filter-functions) "\n"))
  (insert (concat "comint-password-prompt-regexp" " --> " (prin1-to-string comint-password-prompt-regexp) "\n"))
  (insert (concat "comint-preoutput-filter-functions" " --> " (prin1-to-string comint-preoutput-filter-functions) "\n"))
  (insert (concat "comint-process-echoes" " --> " (prin1-to-string comint-process-echoes) "\n"))
  (insert (concat "comint-prompt-read-only" " --> " (prin1-to-string comint-prompt-read-only) "\n"))
  (insert (concat "comint-prompt-regexp" " --> " (prin1-to-string comint-prompt-regexp) "\n"))
  (insert (concat "comint-ptyp" " --> " (prin1-to-string comint-ptyp) "\n"))
  (insert (concat "comint-redirect-completed" " --> " (prin1-to-string comint-redirect-completed) "\n"))
  (insert (concat "comint-redirect-echo-input" " --> " (prin1-to-string comint-redirect-echo-input) "\n"))
  (insert (concat "comint-redirect-filter-functions" " --> " (prin1-to-string comint-redirect-filter-functions) "\n"))
  (insert (concat "comint-redirect-finished-regexp" " --> " (prin1-to-string comint-redirect-finished-regexp) "\n"))
  (insert (concat "comint-redirect-insert-matching-regexp" " --> " (prin1-to-string comint-redirect-insert-matching-regexp) "\n"))
  (insert (concat "comint-redirect-original-filter-function" " --> " (prin1-to-string comint-redirect-original-filter-function) "\n"))
  (insert (concat "comint-redirect-original-mode-line-process" " --> " (prin1-to-string comint-redirect-original-mode-line-process) "\n"))
  (insert (concat "comint-redirect-output-buffer" " --> " (prin1-to-string comint-redirect-output-buffer) "\n"))
  (insert (concat "comint-redirect-perform-sanity-check" " --> " (prin1-to-string comint-redirect-perform-sanity-check) "\n"))
  (insert (concat "comint-redirect-subvert-readonly" " --> " (prin1-to-string comint-redirect-subvert-readonly) "\n"))
  (insert (concat "comint-redirect-verbose" " --> " (prin1-to-string comint-redirect-verbose) "\n"))
  (insert (concat "comint-save-input-ring-index" " --> " (prin1-to-string comint-save-input-ring-index) "\n"))
  (insert (concat "comint-scroll-show-maximum-output" " --> " (prin1-to-string comint-scroll-show-maximum-output) "\n"))
  (insert (concat "comint-scroll-to-bottom-on-input" " --> " (prin1-to-string comint-scroll-to-bottom-on-input) "\n"))
  (insert (concat "comint-scroll-to-bottom-on-output" " --> " (prin1-to-string comint-scroll-to-bottom-on-output) "\n"))
  (insert (concat "comint-stored-incomplete-input" " --> " (prin1-to-string comint-stored-incomplete-input) "\n"))
  (insert (concat "comint-use-prompt-regexp" " --> " (prin1-to-string comint-use-prompt-regexp) "\n"))
  (insert (concat "comint-use-prompt-regexp-instead-of-fields" " --> " (prin1-to-string comint-use-prompt-regexp-instead-of-fields) "\n")))

(defun py-report-comint-variable-setting ()
  "Display some comint-mode variables of interest for debugging.

Some vars like comint-mode maps and tables are not displayed here because of its amount.

Typing `q' will close the buffer displayed"
  (interactive)
  (let ((help-window-select t))
    (with-help-window "*Comint variables*"
      (set-buffer "*Comint variables*")
      (erase-buffer)
      (switch-to-buffer (current-buffer))
      (py--report-comint-variable-setting-intern))))

(defun py--dump-help-string (str)
  (with-output-to-temp-buffer "*Help*"
    (let ((locals (buffer-local-variables))
          (comint-vars-p (eq major-mode 'comint-mode))
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
          (error "Error in py--dump-help-string, tag `%s'" funckind)))
        (princ (format "\n-> %s:\t%s\t%s\n\n"
                       (if (equal funckind "c") "Command" "Variable")
                       funcname keys))
        (princ funcdoc)
        (terpri)
        (setq start end))
      (princ (substitute-command-keys (substring str start)))
      (and comint-vars-p (py-report-comint-variable-setting)))
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

(defun py--add-abbrev-propose (table type arg &optional dont-ask)
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
    (py--add-abbrev-propose
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

(defun py--kill-emacs-hook ()
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

(defun py--input-filter (str)
  "`comint-input-filter' function for Python.

Don't save anything for STR matching `py-input-filter-re' "
  (not (string-match py-input-filter-re str)))

;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-01/msg00076.html
(defalias
  'py-shell-redirect-send-command-to-process
  'comint-redirect-send-command-to-process)
(defalias
  'py-shell-dynamic-simple-complete
  'comint-dynamic-simple-complete)

(defun py-restore-window-configuration ()
  "Restore py-restore-window-configuration when completion is done resp. abandoned. "
  (let (val)
    (and (setq val (get-register py-windows-config-register))(and (consp val) (window-configuration-p (car val))(markerp (cadr val)))(marker-buffer (cadr val))
	 (jump-to-register py-windows-config-register))))

(defun py-shell-execute-string-now (string &optional shell buffer proc output-buffer)
  "Send to Python interpreter process PROC \"exec STRING in {}\".
and return collected output"
  (let* (wait
         (procbuf (or buffer (process-buffer proc) (progn (setq wait py-new-shell-delay) (py-shell nil nil shell))))
         (proc (or proc (get-buffer-process procbuf)))
	 (cmd (format "exec '''%s''' in {}"
		      (mapconcat 'identity (split-string string "\n") "\\n")))
	 ;; TBD remove redundant outbuf
         (outbuf procbuf))
    ;; wait is used only when a new py-shell buffer was connected
    (and wait (sit-for wait))
    (unwind-protect
        (condition-case nil
            (progn
              (with-current-buffer outbuf
                (delete-region (point-min) (point-max)))
              (with-current-buffer procbuf
                ;; (sit-for 3)
                (comint-redirect-send-command-to-process
                 cmd outbuf proc nil t)
                (accept-process-output proc 5))
              (with-current-buffer outbuf
                (buffer-substring (point-min) (point-max))))
          (quit (with-current-buffer procbuf
                  (interrupt-process proc comint-ptyp)
                  (while (not comint-redirect-completed) ; wait for output
                    (accept-process-output proc 1)))
                (signal 'quit nil))))))

;;; Completion

;; started from python.el
(defun py--complete-base (shell pos beg end word imports debug py-exception-buffer)
  (let* ((shell (or shell (py-choose-shell)))
         (proc (or
		;; completing inside a shell
		(get-buffer-process py-exception-buffer)
		   (and (comint-check-proc shell)
			(get-process shell))
	       (prog1
		   (get-buffer-process (py-shell nil nil shell))
		 (sit-for py-new-shell-delay))))
    (code (if (string-match "[Ii][Pp]ython*" shell)
	      (py-set-ipython-completion-command-string shell)
	    python-shell-module-completion-string-code)))
  (py--shell--do-completion-at-point proc imports word pos py-exception-buffer code)))

(defun py--complete-prepare (&optional shell debug beg end word fast-complete)
  (let* ((py-exception-buffer (current-buffer))
         (pos (copy-marker (point)))
	 (pps (syntax-ppss))
	 (in-string (when (nth 3 pps) (nth 8 pps)))
         (beg
	  (save-excursion
	    (or beg
		(and in-string
		     ;; possible completion of filenames
		     (progn
		       (goto-char in-string)
		       (and
			(save-excursion
			  (skip-chars-backward "^ \t\r\n\f")(looking-at "open")))

		       (skip-chars-forward "\"'")(point)))
		(progn (and (eq (char-before) ?\()(forward-char -1))
		       (skip-chars-backward "a-zA-Z0-9_.'") (point)))))
         (end (or end (point)))
	 ;;
         (word (or word (buffer-substring-no-properties beg end)))
	 (ausdruck (and (string-match "^/" word)(setq word (substring-no-properties word 1))(concat "\"" word "*\"")))
	 ;; when in string, assume looking for filename
	 (filenames (and in-string ausdruck
			 (list (replace-regexp-in-string "\n" "" (shell-command-to-string (concat "find / -maxdepth 1 -name " ausdruck))))))
         (imports (py-find-imports))
         py-fontify-shell-buffer-p completion-buffer erg)
    (cond (fast-complete (py--fast-complete-base shell pos beg end word imports debug py-exception-buffer))
	  ((and in-string filenames)
	   (when (setq erg (try-completion (concat "/" word) filenames))
	     (delete-region beg end)
	     (insert erg)))
	  (t (py--complete-base shell pos beg end word imports debug py-exception-buffer)))
    nil))

(defun py-shell-complete (&optional shell debug beg end word)
  "Complete word before point, if any. "
  (interactive)
  (save-excursion
    (and (buffer-live-p (get-buffer "*Python Completions*"))
	 (py-kill-buffer-unconditional "*Python Completions*")))
  (setq py-completion-last-window-configuration
        (current-window-configuration))
  (when debug (setq py-shell-complete-debug nil))
  (py--complete-prepare shell debug beg end word nil))

(defalias 'ipython-complete 'py-shell-complete)
(defun py-indent-or-complete ()
  "Complete or indent depending on the context.

If cursor is at end of line, try to complete
Otherwise call `py-indent-line'

If `(region-active-p)' returns `t', indent region.
Use `C-q TAB' to insert a literally TAB-character

In python-mode `py-complete-function' is called,
in py-shell-mode `py-shell-complete'"
  (interactive "*")
  (cond ((region-active-p)
	 (py-indent-region (region-beginning) (region-end)))
	((or (member (char-before)(list 9 10 12 13 32))
	     (bobp))
	 (py-indent-line))
	((eq major-mode 'python-mode)
	 (if (string-match "ipython" (py-choose-shell))
	     (py-shell-complete)
	   (funcall py-complete-function)))
	((eq major-mode 'py-shell-mode)
	 (if (string-match "[iI]?[Pp]ython" (buffer-name (current-buffer)))
	     (py-shell-complete)
	   (funcall py-complete-function)))
	(t
	 (funcall py-complete-function))))

(defun py--after-change-function (beg end len)
  "Restore window-confiuration after completion. "
  (when
      (and (or
            (eq this-command 'completion-at-point)
            (eq this-command 'choose-completion)
            (eq this-command 'choose-completion)
            (eq this-command 'py-shell-complete)
            (and (or
                  (eq last-command 'completion-at-point)
                  (eq last-command 'choose-completion)
                  (eq last-command 'choose-completion)
                  (eq last-command 'py-shell-complete))
                 (eq this-command 'self-insert-command))))
    (set-window-configuration
     py-completion-last-window-configuration))
  (goto-char end))

(defalias 'py-python2-shell-complete 'py-shell-complete)
(defalias 'py-python3-shell-complete 'py-shell-complete)
;;; Checker
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
Keegan Carruthers-Smith"
  (interactive)
  (if flymake-mode
      ;; switch off
      (flymake-mode)
    (py-toggle-flymake-intern "pyflakespep8" "pyflakespep8")
    (flymake-mode)))

;; pep8
(defun py-pep8-run (command)
  "*Run pep8, check formatting - default on the file currently visited."
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
  "*Run pylint (default on the file currently visited).

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
			      (or last default)
                              'py-pylint-history)
        (read-string "Run pylint like this: "
		     (or default last)
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

Extracted from http://manpages.ubuntu.com/manpages/natty/man1/pyflakes.1.html"))))

;; Pyflakes-pep8
(defalias 'pyflakespep8 'py-pyflakespep8-run)
(defun py-pyflakespep8-run (command)
  "Run pyflakespep8, check formatting - default on the file currently visited. "
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

;; flake8
(defalias 'flake8 'py-flake8-run)
(defun py-flake8-run (command)
  "Flake8 is a wrapper around these tools:
        - PyFlakes
        - pep8
        - Ned Batchelder's McCabe script

        It also adds features:
        - files that contain this line are skipped::
            # flake8: noqa
        - lines that contain a ``# noqa`` comment at the end will not issue warnings.
        - a Git and a Mercurial hook.
        - a McCabe complexity checker.
        - extendable through ``flake8.extension`` entry points. "
  (interactive
   (let* ((py-flake8-command
           (if (string= "" py-flake8-command)
               (or (executable-find "flake8")
                   (error "Don't see \"flake8\" on your system.
Consider \"pip install flake8\" resp. visit \"pypi.python.org\""))
             py-flake8-command))
          (default
            (if (buffer-file-name)
                (format "%s %s %s" py-flake8-command
                        (mapconcat 'identity py-flake8-command-args " ")
                        (buffer-file-name))
              (format "%s %s" py-flake8-command
                      (mapconcat 'identity py-flake8-command-args " "))))
          (last
           (when py-flake8-history
             (let* ((lastcmd (car py-flake8-history))
                    (cmd (cdr (reverse (split-string lastcmd))))
                    (newcmd (reverse (cons (buffer-file-name) cmd))))
               (mapconcat 'identity newcmd " ")))))
     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run flake8 like this: "
                              ;; (if last
                              ;; last
                              default
                              'py-flake8-history1)
        (read-string "Run flake8 like this: "
                     (if last
                         last
                       default)
                     'py-flake8-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defun py-flake8-help ()
  "Display flake8 command line help messages. "
  (interactive)
  (set-buffer (get-buffer-create "*flake8-Help*"))
  (erase-buffer)
  (shell-command "flake8 --help" "*flake8-Help*"))

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
  (virtualenv-add-to-path (concat (py--normalize-directory dir) "bin"))
  (add-to-list 'exec-path (concat (py--normalize-directory dir) "bin"))

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
      (virtualenv-activate (concat (py--normalize-directory (getenv "WORKON_HOME")) name))
    (virtualenv-activate (concat (py--normalize-directory virtualenv-workon-home) name))))

;;; Execute forms at point
(defun py-execute-statement ()
  "Send statement at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "statement"))

(defun py-execute-block ()
  "Send block at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "block"))

(defun py-execute-block-or-clause ()
  "Send block-or-clause at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "block-or-clause"))

(defun py-execute-def ()
  "Send def at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "def"))

(defun py-execute-class ()
  "Send class at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "class"))

(defun py-execute-def-or-class ()
  "Send def-or-class at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "def-or-class"))

(defun py-execute-expression ()
  "Send expression at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "expression"))

(defun py-execute-partial-expression ()
  "Send partial-expression at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "partial-expression"))

(defun py-execute-top-level ()
  "Send top-level at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "top-level"))

(defun py-execute-clause ()
  "Send clause at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "clause"))

;;; Process fast forms
(defun py-fast-process (&optional argprompt dedicated shell buffer-name)
  "Connect am (I)Python process suitable for large output.

Output buffer displays \"Fast\" in name by default
It is not in interactive, i.e. comint-mode, as its bookkeepings seem linked to the freeze reported by lp:1253907

Return the process"
  (interactive "P")
  (py-shell argprompt dedicated shell buffer-name t))

(defun py--fast-send-string (string &optional proc windows-config)
  "Process Python strings, being prepared for large output.

Output buffer displays \"Fast\" in name by default
See also `py-fast-shell'

"
  (let* ((proc (or proc (get-buffer-process (py-fast-process))))
	 (buffer (process-buffer proc)))
    (if (or py-store-result-p py-return-result-p)
	(py--fast-send-string-intern string proc buffer py-store-result-p py-return-result-p)
      (py--fast-send-string-no-output string proc buffer))))

(defun py--filter-result (string)
  "Set `py-result' according to `py-fast-filter-re'.

Remove trailing newline"
  (let* ((erg (ansi-color-filter-apply string)))
    (setq py-result (replace-regexp-in-string (format "[ \n]*%s[ \n]*" py-fast-filter-re) "" erg))
    py-result))

(defun py--fast-send-string-no-output (string proc output-buffer)
  (with-current-buffer output-buffer
    ;; in comint-mode, prompt might be read-only
    ;; delete-region would fail
    (let ((comint-prompt-read-only-old comint-prompt-read-only)
	  comint-prompt-read-only)
      (process-send-string proc "\n")
      (let ((orig (point)))
	(process-send-string proc string)
	(process-send-string proc "\n")
	(accept-process-output proc 5)
	(sit-for 0.1 t)
	;; (when py-verbose-p (message "py--fast-send-string-intern comint-prompt-read-only: %s" comint-prompt-read-only))
	(delete-region orig (point-max))
	(setq comint-prompt-read-only comint-prompt-read-only-old)))))

(defun py--fast-send-string-intern (string proc output-buffer store return)
  (with-current-buffer output-buffer
    (process-send-string proc "\n")
    (let ((orig (point)))
      (process-send-string proc string)
      (process-send-string proc "\n")
      ;; `py--fast-send-string-no-output' sets `py-store-result-p' to
      ;; nil
      (accept-process-output proc 5)
      (sit-for py-fast-completion-delay t)
      ;; sets py-result
      (setq py-result (py--filter-result (py--fetch-result orig)))
      (when return
	py-result))))

(defun py-fast-send-string (string)
  "Evaluate STRING in Python process which is not in comint-mode.

From a programm use `py--fast-send-string'"
  (interactive "sPython command: ")
  (py--fast-send-string string))

(defun py-process-region-fast (beg end)
  (interactive "r")
  (let ((py-fast-process-p t))
    (py-execute-region beg end)))

(defun py-execute-statement-fast ()
  "Process statement at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "statement")))

(defun py-execute-block-fast ()
  "Process block at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "block")))

(defun py-execute-block-or-clause-fast ()
  "Process block-or-clause at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "block-or-clause")))

(defun py-execute-def-fast ()
  "Process def at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "def")))

(defun py-execute-class-fast ()
  "Process class at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "class")))

(defun py-execute-def-or-class-fast ()
  "Process def-or-class at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "def-or-class")))

(defun py-execute-expression-fast ()
  "Process expression at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "expression")))

(defun py-execute-partial-expression-fast ()
  "Process partial-expression at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "partial-expression")))

(defun py-execute-top-level-fast ()
  "Process top-level at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "top-level")))

(defun py-execute-clause-fast ()
  "Process clause at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "clause")))

;;; py-fast-complete
(defun py--fast-completion-get-completions (input process completion-code)
  "Retrieve available completions for INPUT using PROCESS.
Argument COMPLETION-CODE is the python code used to get
completions on the current context."
  (let ((completions
	 (py--fast-send-string-intern
	  (format completion-code input) process py-buffer-name nil t)))
    (when (> (length completions) 2)
      (split-string completions "^'\\|^\"\\|;\\|'$\\|\"$" t))))

(defun py--fast--do-completion-at-point (process imports input orig py-exception-buffer code output-buffer)
  "Do completion at point for PROCESS."
  ;; send setup-code
  (let (py-return-result-p)
    (py--fast-send-string-no-output py-shell-completion-setup-code process output-buffer)
    (when imports
      ;; (message "%s" imports)
      (py--fast-send-string-no-output imports process output-buffer)))
  (let* ((completion
	  (py--fast-completion-get-completions input process code))
	 ;; (completion (when completions
	 ;; (try-completion input completions)))
	 newlist erg)
    ;; (message "%s" (current-buffer))
    (set-buffer py-exception-buffer)
    ;; (sit-for 1 t)
    (cond ((eq completion t)
	   (and py-verbose-p (message "py--fast--do-completion-at-point %s" "`t' is returned, not completion. Might be a bug."))
	   nil)
	  ((null completion)
	   (and py-verbose-p (message "py--fast--do-completion-at-point %s" "Don't see a completion"))
	   nil)
	  ((and completion
		(or (and (listp completion)
			 (string= input (car completion)))
		    (and (stringp completion)
			 (string= input completion))))
	   nil)
	  ((and completion (stringp completion)(not (string= input completion)))
	   (progn (delete-char (- (length input)))
		  (insert completion)
		  ;; (move-marker orig (point))
		  ;; minibuffer.el expects a list
		  nil))
	  (t (py--try-completion input completion)))

    nil))

(defun py--fast-complete-base (shell pos beg end word imports debug py-exception-buffer)
  (let* ((shell (or shell (py-choose-shell)))
	 (py-buffer-name (py-shell nil nil shell nil t))
	 (proc (get-buffer-process py-buffer-name))
	 (code (if (string-match "[Ii][Pp]ython*" shell)
		   (py-set-ipython-completion-command-string shell)
		 python-shell-module-completion-string-code)))
    (with-current-buffer py-buffer-name
      (erase-buffer))
    (py--fast--do-completion-at-point proc imports word pos py-exception-buffer code py-buffer-name)))

(defun py-fast-complete (&optional shell debug beg end word)
  "Complete word before point, if any.

Use `py-fast-process' "
  (interactive)
  (setq py-completion-last-window-configuration
        (current-window-configuration))
  (let ((py-fast-complete-p t))
    (py--complete-prepare shell debug beg end word t)))

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
  (py--execute-prepare filename "python" nil nil nil nil t))

(defun py-execute-file-python-switch (&optional filename)
  "Send file to a Python interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python" nil 'switch nil nil t))

(defun py-execute-file-python-no-switch (&optional filename)
  "Send file to a Python interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python" nil 'no-switch nil nil t))

(defun py-execute-file-python-dedicated (&optional filename)
  "Send file to a Python interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "python" 'dedicated nil nil nil t))

(defun py-execute-file-python-dedicated-switch (&optional filename)
  "Send file to a Python interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python" 'dedicated 'switch nil nil t))

(defun py-execute-file-ipython (&optional filename)
  "Send file to a Ipython interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "ipython" nil nil nil nil t))

(defun py-execute-file-ipython-switch (&optional filename)
  "Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "ipython" nil 'switch nil nil t))

(defun py-execute-file-ipython-no-switch (&optional filename)
  "Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "ipython" nil 'no-switch nil nil t))

(defun py-execute-file-ipython-dedicated (&optional filename)
  "Send file to a Ipython interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "ipython" 'dedicated nil nil nil t))

(defun py-execute-file-ipython-dedicated-switch (&optional filename)
  "Send file to a Ipython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "ipython" 'dedicated 'switch nil nil t))

(defun py-execute-file-python3 (&optional filename)
  "Send file to a Python3 interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "python3" nil nil nil nil t))

(defun py-execute-file-python3-switch (&optional filename)
  "Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3" nil 'switch nil nil t))

(defun py-execute-file-python3-no-switch (&optional filename)
  "Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3" nil 'no-switch nil nil t))

(defun py-execute-file-python3-dedicated (&optional filename)
  "Send file to a Python3 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "python3" 'dedicated nil nil nil t))

(defun py-execute-file-python3-dedicated-switch (&optional filename)
  "Send file to a Python3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3" 'dedicated 'switch nil nil t))

(defun py-execute-file-python2 (&optional filename)
  "Send file to a Python2 interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "python2" nil nil nil nil t))

(defun py-execute-file-python2-switch (&optional filename)
  "Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python2" nil 'switch nil nil t))

(defun py-execute-file-python2-no-switch (&optional filename)
  "Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python2" nil 'no-switch nil nil t))

(defun py-execute-file-python2-dedicated (&optional filename)
  "Send file to a Python2 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "python2" 'dedicated nil nil nil t))

(defun py-execute-file-python2-dedicated-switch (&optional filename)
  "Send file to a Python2 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python2" 'dedicated 'switch nil nil t))

(defun py-execute-file-python2.7 (&optional filename)
  "Send file to a Python2.7 interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "python2.7" nil nil nil nil t))

(defun py-execute-file-python2.7-switch (&optional filename)
  "Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python2.7" nil 'switch nil nil t))

(defun py-execute-file-python2.7-no-switch (&optional filename)
  "Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python2.7" nil 'no-switch nil nil t))

(defun py-execute-file-python2.7-dedicated (&optional filename)
  "Send file to a Python2.7 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "python2.7" 'dedicated nil nil nil t))

(defun py-execute-file-python2.7-dedicated-switch (&optional filename)
  "Send file to a Python2.7 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python2.7" 'dedicated 'switch nil nil t))

(defun py-execute-file-jython (&optional filename)
  "Send file to a Jython interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "jython" nil nil nil nil t))

(defun py-execute-file-jython-switch (&optional filename)
  "Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "jython" nil 'switch nil nil t))

(defun py-execute-file-jython-no-switch (&optional filename)
  "Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "jython" nil 'no-switch nil nil t))

(defun py-execute-file-jython-dedicated (&optional filename)
  "Send file to a Jython interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "jython" 'dedicated nil nil nil t))

(defun py-execute-file-jython-dedicated-switch (&optional filename)
  "Send file to a Jython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "jython" 'dedicated 'switch nil nil t))

(defun py-execute-file-python3.2 (&optional filename)
  "Send file to a Python3.2 interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.2" nil nil nil nil t))

(defun py-execute-file-python3.2-switch (&optional filename)
  "Send file to a Python3.2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.2" nil 'switch nil nil t))

(defun py-execute-file-python3.2-no-switch (&optional filename)
  "Send file to a Python3.2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.2" nil 'no-switch nil nil t))

(defun py-execute-file-python3.2-dedicated (&optional filename)
  "Send file to a Python3.2 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.2" 'dedicated nil nil nil t))

(defun py-execute-file-python3.2-dedicated-switch (&optional filename)
  "Send file to a Python3.2 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.2" 'dedicated 'switch nil nil t))

(defun py-execute-file-python3.3 (&optional filename)
  "Send file to a Python3.3 interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.3" nil nil nil nil t))

(defun py-execute-file-python3.3-switch (&optional filename)
  "Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.3" nil 'switch nil nil t))

(defun py-execute-file-python3.3-no-switch (&optional filename)
  "Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.3" nil 'no-switch nil nil t))

(defun py-execute-file-python3.3-dedicated (&optional filename)
  "Send file to a Python3.3 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.3" 'dedicated nil nil nil t))

(defun py-execute-file-python3.3-dedicated-switch (&optional filename)
  "Send file to a Python3.3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.3" 'dedicated 'switch nil nil t))

(defun py-execute-file-bpython (&optional filename)
  "Send file to a Bpython interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "bpython" nil nil nil nil t))

(defun py-execute-file-bpython-switch (&optional filename)
  "Send file to a Bpython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "bpython" nil 'switch nil nil t))

(defun py-execute-file-bpython-no-switch (&optional filename)
  "Send file to a Bpython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "bpython" nil 'no-switch nil nil t))

(defun py-execute-file-bpython-dedicated (&optional filename)
  "Send file to a Bpython interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "bpython" 'dedicated nil nil nil t))

(defun py-execute-file-bpython-dedicated-switch (&optional filename)
  "Send file to a Bpython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "bpython" 'dedicated 'switch nil nil t))

;;; Extended executes
;; created by `write-unified-extended-execute-forms'
(defun py--execute-prepare (form &optional shell dedicated switch beg end file)
  "Used by python-extended-executes ."
  (save-excursion
    (let* ((beg (unless file
		  (prog1
		      (or beg (funcall (intern-soft (concat "py--beginning-of-" form "-p")))

			  (funcall (intern-soft (concat "py-beginning-of-" form)))
			  (push-mark)))))
	   (end (unless file
		  (or end (funcall (intern-soft (concat "py-end-of-" form))))))
	   (py-dedicated-process-p dedicated)
	   (py-switch-buffers-on-execute-p (cond ((eq 'switch switch)
						  t)
						 ((eq 'no-switch switch)
						  nil)
						 (t py-switch-buffers-on-execute-p)))
	   filename)
      (setq py-buffer-name nil)
      (if file
          (progn
            (setq filename (expand-file-name form))
            (if (file-readable-p filename)
                (py--execute-file-base nil filename nil nil (or (and (boundp 'py-orig-buffer-or-file) py-orig-buffer-or-file) filename))
              (message "%s not readable. %s" file "Do you have write permissions?")))
        (py--execute-base beg end shell)))))

(defun py-execute-statement-dedicated (&optional shell switch)
  "Send statement to unique interpreter. "
  (interactive)
  (py--execute-prepare "statement" shell t switch))

(defun py-execute-statement-python ()
  "Send statement at point to default interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "statement" 'python nil nil))

(defun py-execute-statement-python-switch ()
  "Send statement at point to default interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "statement" 'python nil 'switch))

(defun py-execute-statement-python-no-switch ()
  "Send statement at point to default interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p'

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "statement" 'python nil 'no-switch))

(defun py-execute-statement-python-dedicated ()
  "Send statement at point to default unique interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "statement" 'python t nil))

(defun py-execute-statement-python-dedicated-switch ()
  "Send statement at point to default unique interpreter and switch to result.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "statement" 'python t 'switch))

(defun py-execute-statement-ipython ()
  "Send statement at point to IPython interpreter. "
  (interactive)
  (py--execute-prepare "statement" 'ipython nil nil))

(defun py-execute-statement-ipython-switch ()
  "Send statement at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "statement" 'ipython nil 'switch))

(defun py-execute-statement-ipython-no-switch ()
  "Send statement at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "statement" 'ipython nil 'no-switch))

(defun py-execute-statement-ipython-dedicated ()
  "Send statement at point to IPython unique interpreter. "
  (interactive)
  (py--execute-prepare "statement" 'ipython t nil))

(defun py-execute-statement-ipython-dedicated-switch ()
  "Send statement at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "statement" 'ipython t 'switch))

(defun py-execute-statement-python2 ()
  "Send statement at point to Python2 interpreter. "
  (interactive)
  (py--execute-prepare "statement" 'python2 nil nil))

(defun py-execute-statement-python2-switch ()
  "Send statement at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "statement" 'python2 nil 'switch))

(defun py-execute-statement-python2-no-switch ()
  "Send statement at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "statement" 'python2 nil 'no-switch))

(defun py-execute-statement-python2-dedicated ()
  "Send statement at point to Python2 unique interpreter. "
  (interactive)
  (py--execute-prepare "statement" 'python2 t nil))

(defun py-execute-statement-python2-dedicated-switch ()
  "Send statement at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "statement" 'python2 t 'switch))

(defun py-execute-statement-jython ()
  "Send statement at point to Jython interpreter. "
  (interactive)
  (py--execute-prepare "statement" 'jython nil nil))

(defun py-execute-statement-jython-switch ()
  "Send statement at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "statement" 'jython nil 'switch))

(defun py-execute-statement-jython-no-switch ()
  "Send statement at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "statement" 'jython nil 'no-switch))

(defun py-execute-statement-jython-dedicated ()
  "Send statement at point to Jython unique interpreter. "
  (interactive)
  (py--execute-prepare "statement" 'jython t nil))

(defun py-execute-statement-jython-dedicated-switch ()
  "Send statement at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "statement" 'jython t 'switch))

(defun py-execute-statement-python3 ()
  "Send statement at point to Python3 interpreter. "
  (interactive)
  (py--execute-prepare "statement" 'python3 nil nil))

(defun py-execute-statement-python3-switch ()
  "Send statement at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "statement" 'python3 nil 'switch))

(defun py-execute-statement-python3-no-switch ()
  "Send statement at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "statement" 'python3 nil 'no-switch))

(defun py-execute-statement-python3-dedicated ()
  "Send statement at point to Python3 unique interpreter. "
  (interactive)
  (py--execute-prepare "statement" 'python3 t nil))

(defun py-execute-statement-python3-dedicated-switch ()
  "Send statement at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "statement" 'python3 t 'switch))

(defun py-execute-statement-bpython ()
  "Send statement at point to Bpython interpreter. "
  (interactive)
  (py--execute-prepare "statement" 'bpython nil nil))

(defun py-execute-statement-bpython-switch ()
  "Send statement at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "statement" 'bpython nil 'switch))

(defun py-execute-statement-bpython-no-switch ()
  "Send statement at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "statement" 'bpython nil 'no-switch))

(defun py-execute-statement-bpython-dedicated ()
  "Send statement at point to Bpython unique interpreter. "
  (interactive)
  (py--execute-prepare "statement" 'bpython t nil))

(defun py-execute-statement-bpython-dedicated-switch ()
  "Send statement at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "statement" 'bpython t 'switch))

(defun py-execute-block-dedicated (&optional shell switch)
  "Send block to unique interpreter. "
  (interactive)
  (py--execute-prepare "block" shell t switch))

(defun py-execute-block-python ()
  "Send block at point to default interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "block" 'python nil nil))

(defun py-execute-block-python-switch ()
  "Send block at point to default interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "block" 'python nil 'switch))

(defun py-execute-block-python-no-switch ()
  "Send block at point to default interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p'

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "block" 'python nil 'no-switch))

(defun py-execute-block-python-dedicated ()
  "Send block at point to default unique interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "block" 'python t nil))

(defun py-execute-block-python-dedicated-switch ()
  "Send block at point to default unique interpreter and switch to result.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "block" 'python t 'switch))

(defun py-execute-block-ipython ()
  "Send block at point to IPython interpreter. "
  (interactive)
  (py--execute-prepare "block" 'ipython nil nil))

(defun py-execute-block-ipython-switch ()
  "Send block at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "block" 'ipython nil 'switch))

(defun py-execute-block-ipython-no-switch ()
  "Send block at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "block" 'ipython nil 'no-switch))

(defun py-execute-block-ipython-dedicated ()
  "Send block at point to IPython unique interpreter. "
  (interactive)
  (py--execute-prepare "block" 'ipython t nil))

(defun py-execute-block-ipython-dedicated-switch ()
  "Send block at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "block" 'ipython t 'switch))

(defun py-execute-block-python2 ()
  "Send block at point to Python2 interpreter. "
  (interactive)
  (py--execute-prepare "block" 'python2 nil nil))

(defun py-execute-block-python2-switch ()
  "Send block at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "block" 'python2 nil 'switch))

(defun py-execute-block-python2-no-switch ()
  "Send block at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "block" 'python2 nil 'no-switch))

(defun py-execute-block-python2-dedicated ()
  "Send block at point to Python2 unique interpreter. "
  (interactive)
  (py--execute-prepare "block" 'python2 t nil))

(defun py-execute-block-python2-dedicated-switch ()
  "Send block at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "block" 'python2 t 'switch))

(defun py-execute-block-jython ()
  "Send block at point to Jython interpreter. "
  (interactive)
  (py--execute-prepare "block" 'jython nil nil))

(defun py-execute-block-jython-switch ()
  "Send block at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "block" 'jython nil 'switch))

(defun py-execute-block-jython-no-switch ()
  "Send block at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "block" 'jython nil 'no-switch))

(defun py-execute-block-jython-dedicated ()
  "Send block at point to Jython unique interpreter. "
  (interactive)
  (py--execute-prepare "block" 'jython t nil))

(defun py-execute-block-jython-dedicated-switch ()
  "Send block at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "block" 'jython t 'switch))

(defun py-execute-block-python3 ()
  "Send block at point to Python3 interpreter. "
  (interactive)
  (py--execute-prepare "block" 'python3 nil nil))

(defun py-execute-block-python3-switch ()
  "Send block at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "block" 'python3 nil 'switch))

(defun py-execute-block-python3-no-switch ()
  "Send block at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "block" 'python3 nil 'no-switch))

(defun py-execute-block-python3-dedicated ()
  "Send block at point to Python3 unique interpreter. "
  (interactive)
  (py--execute-prepare "block" 'python3 t nil))

(defun py-execute-block-python3-dedicated-switch ()
  "Send block at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "block" 'python3 t 'switch))

(defun py-execute-block-bpython ()
  "Send block at point to Bpython interpreter. "
  (interactive)
  (py--execute-prepare "block" 'bpython nil nil))

(defun py-execute-block-bpython-switch ()
  "Send block at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "block" 'bpython nil 'switch))

(defun py-execute-block-bpython-no-switch ()
  "Send block at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "block" 'bpython nil 'no-switch))

(defun py-execute-block-bpython-dedicated ()
  "Send block at point to Bpython unique interpreter. "
  (interactive)
  (py--execute-prepare "block" 'bpython t nil))

(defun py-execute-block-bpython-dedicated-switch ()
  "Send block at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "block" 'bpython t 'switch))

(defun py-execute-clause-dedicated (&optional shell switch)
  "Send clause to unique interpreter. "
  (interactive)
  (py--execute-prepare "clause" shell t switch))

(defun py-execute-clause-python ()
  "Send clause at point to default interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "clause" 'python nil nil))

(defun py-execute-clause-python-switch ()
  "Send clause at point to default interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "clause" 'python nil 'switch))

(defun py-execute-clause-python-no-switch ()
  "Send clause at point to default interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p'

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "clause" 'python nil 'no-switch))

(defun py-execute-clause-python-dedicated ()
  "Send clause at point to default unique interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "clause" 'python t nil))

(defun py-execute-clause-python-dedicated-switch ()
  "Send clause at point to default unique interpreter and switch to result.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "clause" 'python t 'switch))

(defun py-execute-clause-ipython ()
  "Send clause at point to IPython interpreter. "
  (interactive)
  (py--execute-prepare "clause" 'ipython nil nil))

(defun py-execute-clause-ipython-switch ()
  "Send clause at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "clause" 'ipython nil 'switch))

(defun py-execute-clause-ipython-no-switch ()
  "Send clause at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "clause" 'ipython nil 'no-switch))

(defun py-execute-clause-ipython-dedicated ()
  "Send clause at point to IPython unique interpreter. "
  (interactive)
  (py--execute-prepare "clause" 'ipython t nil))

(defun py-execute-clause-ipython-dedicated-switch ()
  "Send clause at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "clause" 'ipython t 'switch))

(defun py-execute-clause-python2 ()
  "Send clause at point to Python2 interpreter. "
  (interactive)
  (py--execute-prepare "clause" 'python2 nil nil))

(defun py-execute-clause-python2-switch ()
  "Send clause at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "clause" 'python2 nil 'switch))

(defun py-execute-clause-python2-no-switch ()
  "Send clause at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "clause" 'python2 nil 'no-switch))

(defun py-execute-clause-python2-dedicated ()
  "Send clause at point to Python2 unique interpreter. "
  (interactive)
  (py--execute-prepare "clause" 'python2 t nil))

(defun py-execute-clause-python2-dedicated-switch ()
  "Send clause at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "clause" 'python2 t 'switch))

(defun py-execute-clause-jython ()
  "Send clause at point to Jython interpreter. "
  (interactive)
  (py--execute-prepare "clause" 'jython nil nil))

(defun py-execute-clause-jython-switch ()
  "Send clause at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "clause" 'jython nil 'switch))

(defun py-execute-clause-jython-no-switch ()
  "Send clause at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "clause" 'jython nil 'no-switch))

(defun py-execute-clause-jython-dedicated ()
  "Send clause at point to Jython unique interpreter. "
  (interactive)
  (py--execute-prepare "clause" 'jython t nil))

(defun py-execute-clause-jython-dedicated-switch ()
  "Send clause at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "clause" 'jython t 'switch))

(defun py-execute-clause-python3 ()
  "Send clause at point to Python3 interpreter. "
  (interactive)
  (py--execute-prepare "clause" 'python3 nil nil))

(defun py-execute-clause-python3-switch ()
  "Send clause at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "clause" 'python3 nil 'switch))

(defun py-execute-clause-python3-no-switch ()
  "Send clause at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "clause" 'python3 nil 'no-switch))

(defun py-execute-clause-python3-dedicated ()
  "Send clause at point to Python3 unique interpreter. "
  (interactive)
  (py--execute-prepare "clause" 'python3 t nil))

(defun py-execute-clause-python3-dedicated-switch ()
  "Send clause at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "clause" 'python3 t 'switch))

(defun py-execute-clause-bpython ()
  "Send clause at point to Bpython interpreter. "
  (interactive)
  (py--execute-prepare "clause" 'bpython nil nil))

(defun py-execute-clause-bpython-switch ()
  "Send clause at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "clause" 'bpython nil 'switch))

(defun py-execute-clause-bpython-no-switch ()
  "Send clause at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "clause" 'bpython nil 'no-switch))

(defun py-execute-clause-bpython-dedicated ()
  "Send clause at point to Bpython unique interpreter. "
  (interactive)
  (py--execute-prepare "clause" 'bpython t nil))

(defun py-execute-clause-bpython-dedicated-switch ()
  "Send clause at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "clause" 'bpython t 'switch))

(defun py-execute-block-or-clause-dedicated (&optional shell switch)
  "Send block-or-clause to unique interpreter. "
  (interactive)
  (py--execute-prepare "block-or-clause" shell t switch))

(defun py-execute-block-or-clause-python ()
  "Send block-or-clause at point to default interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "block-or-clause" 'python nil nil))

(defun py-execute-block-or-clause-python-switch ()
  "Send block-or-clause at point to default interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "block-or-clause" 'python nil 'switch))

(defun py-execute-block-or-clause-python-no-switch ()
  "Send block-or-clause at point to default interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p'

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "block-or-clause" 'python nil 'no-switch))

(defun py-execute-block-or-clause-python-dedicated ()
  "Send block-or-clause at point to default unique interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "block-or-clause" 'python t nil))

(defun py-execute-block-or-clause-python-dedicated-switch ()
  "Send block-or-clause at point to default unique interpreter and switch to result.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "block-or-clause" 'python t 'switch))

(defun py-execute-block-or-clause-ipython ()
  "Send block-or-clause at point to IPython interpreter. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'ipython nil nil))

(defun py-execute-block-or-clause-ipython-switch ()
  "Send block-or-clause at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'ipython nil 'switch))

(defun py-execute-block-or-clause-ipython-no-switch ()
  "Send block-or-clause at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "block-or-clause" 'ipython nil 'no-switch))

(defun py-execute-block-or-clause-ipython-dedicated ()
  "Send block-or-clause at point to IPython unique interpreter. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'ipython t nil))

(defun py-execute-block-or-clause-ipython-dedicated-switch ()
  "Send block-or-clause at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'ipython t 'switch))

(defun py-execute-block-or-clause-python2 ()
  "Send block-or-clause at point to Python2 interpreter. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'python2 nil nil))

(defun py-execute-block-or-clause-python2-switch ()
  "Send block-or-clause at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'python2 nil 'switch))

(defun py-execute-block-or-clause-python2-no-switch ()
  "Send block-or-clause at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "block-or-clause" 'python2 nil 'no-switch))

(defun py-execute-block-or-clause-python2-dedicated ()
  "Send block-or-clause at point to Python2 unique interpreter. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'python2 t nil))

(defun py-execute-block-or-clause-python2-dedicated-switch ()
  "Send block-or-clause at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'python2 t 'switch))

(defun py-execute-block-or-clause-jython ()
  "Send block-or-clause at point to Jython interpreter. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'jython nil nil))

(defun py-execute-block-or-clause-jython-switch ()
  "Send block-or-clause at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'jython nil 'switch))

(defun py-execute-block-or-clause-jython-no-switch ()
  "Send block-or-clause at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "block-or-clause" 'jython nil 'no-switch))

(defun py-execute-block-or-clause-jython-dedicated ()
  "Send block-or-clause at point to Jython unique interpreter. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'jython t nil))

(defun py-execute-block-or-clause-jython-dedicated-switch ()
  "Send block-or-clause at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'jython t 'switch))

(defun py-execute-block-or-clause-python3 ()
  "Send block-or-clause at point to Python3 interpreter. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'python3 nil nil))

(defun py-execute-block-or-clause-python3-switch ()
  "Send block-or-clause at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'python3 nil 'switch))

(defun py-execute-block-or-clause-python3-no-switch ()
  "Send block-or-clause at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "block-or-clause" 'python3 nil 'no-switch))

(defun py-execute-block-or-clause-python3-dedicated ()
  "Send block-or-clause at point to Python3 unique interpreter. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'python3 t nil))

(defun py-execute-block-or-clause-python3-dedicated-switch ()
  "Send block-or-clause at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'python3 t 'switch))

(defun py-execute-block-or-clause-bpython ()
  "Send block-or-clause at point to Bpython interpreter. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'bpython nil nil))

(defun py-execute-block-or-clause-bpython-switch ()
  "Send block-or-clause at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'bpython nil 'switch))

(defun py-execute-block-or-clause-bpython-no-switch ()
  "Send block-or-clause at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "block-or-clause" 'bpython nil 'no-switch))

(defun py-execute-block-or-clause-bpython-dedicated ()
  "Send block-or-clause at point to Bpython unique interpreter. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'bpython t nil))

(defun py-execute-block-or-clause-bpython-dedicated-switch ()
  "Send block-or-clause at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "block-or-clause" 'bpython t 'switch))

(defun py-execute-def-dedicated (&optional shell switch)
  "Send def to unique interpreter. "
  (interactive)
  (py--execute-prepare "def" shell t switch))

(defun py-execute-def-python ()
  "Send def at point to default interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "def" 'python nil nil))

(defun py-execute-def-python-switch ()
  "Send def at point to default interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "def" 'python nil 'switch))

(defun py-execute-def-python-no-switch ()
  "Send def at point to default interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p'

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "def" 'python nil 'no-switch))

(defun py-execute-def-python-dedicated ()
  "Send def at point to default unique interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "def" 'python t nil))

(defun py-execute-def-python-dedicated-switch ()
  "Send def at point to default unique interpreter and switch to result.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "def" 'python t 'switch))

(defun py-execute-def-ipython ()
  "Send def at point to IPython interpreter. "
  (interactive)
  (py--execute-prepare "def" 'ipython nil nil))

(defun py-execute-def-ipython-switch ()
  "Send def at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "def" 'ipython nil 'switch))

(defun py-execute-def-ipython-no-switch ()
  "Send def at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "def" 'ipython nil 'no-switch))

(defun py-execute-def-ipython-dedicated ()
  "Send def at point to IPython unique interpreter. "
  (interactive)
  (py--execute-prepare "def" 'ipython t nil))

(defun py-execute-def-ipython-dedicated-switch ()
  "Send def at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "def" 'ipython t 'switch))

(defun py-execute-def-python2 ()
  "Send def at point to Python2 interpreter. "
  (interactive)
  (py--execute-prepare "def" 'python2 nil nil))

(defun py-execute-def-python2-switch ()
  "Send def at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "def" 'python2 nil 'switch))

(defun py-execute-def-python2-no-switch ()
  "Send def at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "def" 'python2 nil 'no-switch))

(defun py-execute-def-python2-dedicated ()
  "Send def at point to Python2 unique interpreter. "
  (interactive)
  (py--execute-prepare "def" 'python2 t nil))

(defun py-execute-def-python2-dedicated-switch ()
  "Send def at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "def" 'python2 t 'switch))

(defun py-execute-def-jython ()
  "Send def at point to Jython interpreter. "
  (interactive)
  (py--execute-prepare "def" 'jython nil nil))

(defun py-execute-def-jython-switch ()
  "Send def at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "def" 'jython nil 'switch))

(defun py-execute-def-jython-no-switch ()
  "Send def at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "def" 'jython nil 'no-switch))

(defun py-execute-def-jython-dedicated ()
  "Send def at point to Jython unique interpreter. "
  (interactive)
  (py--execute-prepare "def" 'jython t nil))

(defun py-execute-def-jython-dedicated-switch ()
  "Send def at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "def" 'jython t 'switch))

(defun py-execute-def-python3 ()
  "Send def at point to Python3 interpreter. "
  (interactive)
  (py--execute-prepare "def" 'python3 nil nil))

(defun py-execute-def-python3-switch ()
  "Send def at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "def" 'python3 nil 'switch))

(defun py-execute-def-python3-no-switch ()
  "Send def at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "def" 'python3 nil 'no-switch))

(defun py-execute-def-python3-dedicated ()
  "Send def at point to Python3 unique interpreter. "
  (interactive)
  (py--execute-prepare "def" 'python3 t nil))

(defun py-execute-def-python3-dedicated-switch ()
  "Send def at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "def" 'python3 t 'switch))

(defun py-execute-def-bpython ()
  "Send def at point to Bpython interpreter. "
  (interactive)
  (py--execute-prepare "def" 'bpython nil nil))

(defun py-execute-def-bpython-switch ()
  "Send def at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "def" 'bpython nil 'switch))

(defun py-execute-def-bpython-no-switch ()
  "Send def at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "def" 'bpython nil 'no-switch))

(defun py-execute-def-bpython-dedicated ()
  "Send def at point to Bpython unique interpreter. "
  (interactive)
  (py--execute-prepare "def" 'bpython t nil))

(defun py-execute-def-bpython-dedicated-switch ()
  "Send def at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "def" 'bpython t 'switch))

(defun py-execute-class-dedicated (&optional shell switch)
  "Send class to unique interpreter. "
  (interactive)
  (py--execute-prepare "class" shell t switch))

(defun py-execute-class-python ()
  "Send class at point to default interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "class" 'python nil nil))

(defun py-execute-class-python-switch ()
  "Send class at point to default interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "class" 'python nil 'switch))

(defun py-execute-class-python-no-switch ()
  "Send class at point to default interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p'

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "class" 'python nil 'no-switch))

(defun py-execute-class-python-dedicated ()
  "Send class at point to default unique interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "class" 'python t nil))

(defun py-execute-class-python-dedicated-switch ()
  "Send class at point to default unique interpreter and switch to result.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "class" 'python t 'switch))

(defun py-execute-class-ipython ()
  "Send class at point to IPython interpreter. "
  (interactive)
  (py--execute-prepare "class" 'ipython nil nil))

(defun py-execute-class-ipython-switch ()
  "Send class at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "class" 'ipython nil 'switch))

(defun py-execute-class-ipython-no-switch ()
  "Send class at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "class" 'ipython nil 'no-switch))

(defun py-execute-class-ipython-dedicated ()
  "Send class at point to IPython unique interpreter. "
  (interactive)
  (py--execute-prepare "class" 'ipython t nil))

(defun py-execute-class-ipython-dedicated-switch ()
  "Send class at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "class" 'ipython t 'switch))

(defun py-execute-class-python2 ()
  "Send class at point to Python2 interpreter. "
  (interactive)
  (py--execute-prepare "class" 'python2 nil nil))

(defun py-execute-class-python2-switch ()
  "Send class at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "class" 'python2 nil 'switch))

(defun py-execute-class-python2-no-switch ()
  "Send class at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "class" 'python2 nil 'no-switch))

(defun py-execute-class-python2-dedicated ()
  "Send class at point to Python2 unique interpreter. "
  (interactive)
  (py--execute-prepare "class" 'python2 t nil))

(defun py-execute-class-python2-dedicated-switch ()
  "Send class at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "class" 'python2 t 'switch))

(defun py-execute-class-jython ()
  "Send class at point to Jython interpreter. "
  (interactive)
  (py--execute-prepare "class" 'jython nil nil))

(defun py-execute-class-jython-switch ()
  "Send class at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "class" 'jython nil 'switch))

(defun py-execute-class-jython-no-switch ()
  "Send class at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "class" 'jython nil 'no-switch))

(defun py-execute-class-jython-dedicated ()
  "Send class at point to Jython unique interpreter. "
  (interactive)
  (py--execute-prepare "class" 'jython t nil))

(defun py-execute-class-jython-dedicated-switch ()
  "Send class at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "class" 'jython t 'switch))

(defun py-execute-class-python3 ()
  "Send class at point to Python3 interpreter. "
  (interactive)
  (py--execute-prepare "class" 'python3 nil nil))

(defun py-execute-class-python3-switch ()
  "Send class at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "class" 'python3 nil 'switch))

(defun py-execute-class-python3-no-switch ()
  "Send class at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "class" 'python3 nil 'no-switch))

(defun py-execute-class-python3-dedicated ()
  "Send class at point to Python3 unique interpreter. "
  (interactive)
  (py--execute-prepare "class" 'python3 t nil))

(defun py-execute-class-python3-dedicated-switch ()
  "Send class at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "class" 'python3 t 'switch))

(defun py-execute-class-bpython ()
  "Send class at point to Bpython interpreter. "
  (interactive)
  (py--execute-prepare "class" 'bpython nil nil))

(defun py-execute-class-bpython-switch ()
  "Send class at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "class" 'bpython nil 'switch))

(defun py-execute-class-bpython-no-switch ()
  "Send class at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "class" 'bpython nil 'no-switch))

(defun py-execute-class-bpython-dedicated ()
  "Send class at point to Bpython unique interpreter. "
  (interactive)
  (py--execute-prepare "class" 'bpython t nil))

(defun py-execute-class-bpython-dedicated-switch ()
  "Send class at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "class" 'bpython t 'switch))

(defun py-execute-region-dedicated (&optional shell switch)
  "Send region to unique interpreter. "
  (interactive)
  (py--execute-prepare "region" shell t switch))

(defun py-execute-region-python (beg end)
  "Send region at point to default interpreter.

For `default' see value of `py-shell-name'"
  (interactive "r")
  (py--execute-prepare "region" 'python nil nil beg end))

(defun py-execute-region-python-switch (beg end)
  "Send region at point to default interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'.

For `default' see value of `py-shell-name'"
  (interactive "r")
  (py--execute-prepare "region" 'python nil 'switch beg end))

(defun py-execute-region-python-no-switch (beg end)
  "Send region at point to default interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p'

For `default' see value of `py-shell-name'"
  (interactive "r")
  (py--execute-prepare "region" 'python nil 'no-switch beg end))

(defun py-execute-region-python-dedicated (beg end)
  "Send region at point to default unique interpreter.

For `default' see value of `py-shell-name'"
  (interactive "r")
  (py--execute-prepare "region" 'python t nil beg end))

(defun py-execute-region-python-dedicated-switch (beg end)
  "Send region at point to default unique interpreter and switch to result.

For `default' see value of `py-shell-name'"
  (interactive "r")
  (py--execute-prepare "region" 'python t 'switch beg end))

(defun py-execute-region-ipython (beg end)
  "Send region at point to IPython interpreter. "
  (interactive "r")
  (py--execute-prepare "region" 'ipython nil nil beg end))

(defun py-execute-region-ipython-switch (beg end)
  "Send region at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py--execute-prepare "region" 'ipython nil 'switch beg end))

(defun py-execute-region-ipython-no-switch (beg end)
  "Send region at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py--execute-prepare "region" 'ipython nil 'no-switch beg end))

(defun py-execute-region-ipython-dedicated (beg end)
  "Send region at point to IPython unique interpreter. "
  (interactive "r")
  (py--execute-prepare "region" 'ipython t nil beg end))

(defun py-execute-region-ipython-dedicated-switch (beg end)
  "Send region at point to IPython unique interpreter and switch to result. "
  (interactive "r")
  (py--execute-prepare "region" 'ipython t 'switch beg end))

(defun py-execute-region-python2 (beg end)
  "Send region at point to Python2 interpreter. "
  (interactive "r")
  (py--execute-prepare "region" 'python2 nil nil beg end))

(defun py-execute-region-python2-switch (beg end)
  "Send region at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py--execute-prepare "region" 'python2 nil 'switch beg end))

(defun py-execute-region-python2-no-switch (beg end)
  "Send region at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py--execute-prepare "region" 'python2 nil 'no-switch beg end))

(defun py-execute-region-python2-dedicated (beg end)
  "Send region at point to Python2 unique interpreter. "
  (interactive "r")
  (py--execute-prepare "region" 'python2 t nil beg end))

(defun py-execute-region-python2-dedicated-switch (beg end)
  "Send region at point to Python2 unique interpreter and switch to result. "
  (interactive "r")
  (py--execute-prepare "region" 'python2 t 'switch beg end))

(defun py-execute-region-jython (beg end)
  "Send region at point to Jython interpreter. "
  (interactive "r")
  (py--execute-prepare "region" 'jython nil nil beg end))

(defun py-execute-region-jython-switch (beg end)
  "Send region at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py--execute-prepare "region" 'jython nil 'switch beg end))

(defun py-execute-region-jython-no-switch (beg end)
  "Send region at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py--execute-prepare "region" 'jython nil 'no-switch beg end))

(defun py-execute-region-jython-dedicated (beg end)
  "Send region at point to Jython unique interpreter. "
  (interactive "r")
  (py--execute-prepare "region" 'jython t nil beg end))

(defun py-execute-region-jython-dedicated-switch (beg end)
  "Send region at point to Jython unique interpreter and switch to result. "
  (interactive "r")
  (py--execute-prepare "region" 'jython t 'switch beg end))

(defun py-execute-region-python3 (beg end)
  "Send region at point to Python3 interpreter. "
  (interactive "r")
  (py--execute-prepare "region" 'python3 nil nil beg end))

(defun py-execute-region-python3-switch (beg end)
  "Send region at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py--execute-prepare "region" 'python3 nil 'switch beg end))

(defun py-execute-region-python3-no-switch (beg end)
  "Send region at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py--execute-prepare "region" 'python3 nil 'no-switch beg end))

(defun py-execute-region-python3-dedicated (beg end)
  "Send region at point to Python3 unique interpreter. "
  (interactive "r")
  (py--execute-prepare "region" 'python3 t nil beg end))

(defun py-execute-region-python3-dedicated-switch (beg end)
  "Send region at point to Python3 unique interpreter and switch to result. "
  (interactive "r")
  (py--execute-prepare "region" 'python3 t 'switch beg end))

(defun py-execute-region-bpython (beg end)
  "Send region at point to Bpython interpreter. "
  (interactive "r")
  (py--execute-prepare "region" 'bpython nil nil beg end))

(defun py-execute-region-bpython-switch (beg end)
  "Send region at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py--execute-prepare "region" 'bpython nil 'switch beg end))

(defun py-execute-region-bpython-no-switch (beg end)
  "Send region at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py--execute-prepare "region" 'bpython nil 'no-switch beg end))

(defun py-execute-region-bpython-dedicated (beg end)
  "Send region at point to Bpython unique interpreter. "
  (interactive "r")
  (py--execute-prepare "region" 'bpython t nil beg end))

(defun py-execute-region-bpython-dedicated-switch (beg end)
  "Send region at point to Bpython unique interpreter and switch to result. "
  (interactive "r")
  (py--execute-prepare "region" 'bpython t 'switch beg end))

(defun py-execute-buffer-dedicated (&optional shell switch)
  "Send buffer to unique interpreter. "
  (interactive)
  (py--execute-prepare "buffer" shell t switch))

(defun py-execute-buffer-python ()
  "Send buffer at point to default interpreter.

For `default' see value of `py-shell-name'"
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
      (py--execute-prepare "buffer" 'python nil nil (point-min) (point-max)))))

(defun py-execute-buffer-python-switch ()
  "Send buffer at point to default interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'.

For `default' see value of `py-shell-name'"
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
      (py--execute-prepare "buffer" 'python nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python-no-switch ()
  "Send buffer at point to default interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p'

For `default' see value of `py-shell-name'"
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
      (py--execute-prepare "buffer" 'python nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-python-dedicated ()
  "Send buffer at point to default unique interpreter.

For `default' see value of `py-shell-name'"
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
      (py--execute-prepare "buffer" 'python t nil (point-min) (point-max)))))

(defun py-execute-buffer-python-dedicated-switch ()
  "Send buffer at point to default unique interpreter and switch to result.

For `default' see value of `py-shell-name'"
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
      (py--execute-prepare "buffer" 'python t 'switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'ipython nil nil (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'ipython nil 'switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'ipython nil 'no-switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'ipython t nil (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'ipython t 'switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'python2 nil nil (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'python2 nil 'switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'python2 nil 'no-switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'python2 t nil (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'python2 t 'switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'jython nil nil (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'jython nil 'switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'jython nil 'no-switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'jython t nil (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'jython t 'switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'python3 nil nil (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'python3 nil 'switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'python3 nil 'no-switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'python3 t nil (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'python3 t 'switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'bpython nil nil (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'bpython nil 'switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'bpython nil 'no-switch (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'bpython t nil (point-min) (point-max)))))

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
      (py--execute-prepare "buffer" 'bpython t 'switch (point-min) (point-max)))))

(defun py-execute-expression-dedicated (&optional shell switch)
  "Send expression to unique interpreter. "
  (interactive)
  (py--execute-prepare "expression" shell t switch))

(defun py-execute-expression-python ()
  "Send expression at point to default interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "expression" 'python nil nil))

(defun py-execute-expression-python-switch ()
  "Send expression at point to default interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "expression" 'python nil 'switch))

(defun py-execute-expression-python-no-switch ()
  "Send expression at point to default interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p'

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "expression" 'python nil 'no-switch))

(defun py-execute-expression-python-dedicated ()
  "Send expression at point to default unique interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "expression" 'python t nil))

(defun py-execute-expression-python-dedicated-switch ()
  "Send expression at point to default unique interpreter and switch to result.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "expression" 'python t 'switch))

(defun py-execute-expression-ipython ()
  "Send expression at point to IPython interpreter. "
  (interactive)
  (py--execute-prepare "expression" 'ipython nil nil))

(defun py-execute-expression-ipython-switch ()
  "Send expression at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "expression" 'ipython nil 'switch))

(defun py-execute-expression-ipython-no-switch ()
  "Send expression at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "expression" 'ipython nil 'no-switch))

(defun py-execute-expression-ipython-dedicated ()
  "Send expression at point to IPython unique interpreter. "
  (interactive)
  (py--execute-prepare "expression" 'ipython t nil))

(defun py-execute-expression-ipython-dedicated-switch ()
  "Send expression at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "expression" 'ipython t 'switch))

(defun py-execute-expression-python2 ()
  "Send expression at point to Python2 interpreter. "
  (interactive)
  (py--execute-prepare "expression" 'python2 nil nil))

(defun py-execute-expression-python2-switch ()
  "Send expression at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "expression" 'python2 nil 'switch))

(defun py-execute-expression-python2-no-switch ()
  "Send expression at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "expression" 'python2 nil 'no-switch))

(defun py-execute-expression-python2-dedicated ()
  "Send expression at point to Python2 unique interpreter. "
  (interactive)
  (py--execute-prepare "expression" 'python2 t nil))

(defun py-execute-expression-python2-dedicated-switch ()
  "Send expression at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "expression" 'python2 t 'switch))

(defun py-execute-expression-jython ()
  "Send expression at point to Jython interpreter. "
  (interactive)
  (py--execute-prepare "expression" 'jython nil nil))

(defun py-execute-expression-jython-switch ()
  "Send expression at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "expression" 'jython nil 'switch))

(defun py-execute-expression-jython-no-switch ()
  "Send expression at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "expression" 'jython nil 'no-switch))

(defun py-execute-expression-jython-dedicated ()
  "Send expression at point to Jython unique interpreter. "
  (interactive)
  (py--execute-prepare "expression" 'jython t nil))

(defun py-execute-expression-jython-dedicated-switch ()
  "Send expression at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "expression" 'jython t 'switch))

(defun py-execute-expression-python3 ()
  "Send expression at point to Python3 interpreter. "
  (interactive)
  (py--execute-prepare "expression" 'python3 nil nil))

(defun py-execute-expression-python3-switch ()
  "Send expression at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "expression" 'python3 nil 'switch))

(defun py-execute-expression-python3-no-switch ()
  "Send expression at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "expression" 'python3 nil 'no-switch))

(defun py-execute-expression-python3-dedicated ()
  "Send expression at point to Python3 unique interpreter. "
  (interactive)
  (py--execute-prepare "expression" 'python3 t nil))

(defun py-execute-expression-python3-dedicated-switch ()
  "Send expression at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "expression" 'python3 t 'switch))

(defun py-execute-expression-bpython ()
  "Send expression at point to Bpython interpreter. "
  (interactive)
  (py--execute-prepare "expression" 'bpython nil nil))

(defun py-execute-expression-bpython-switch ()
  "Send expression at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "expression" 'bpython nil 'switch))

(defun py-execute-expression-bpython-no-switch ()
  "Send expression at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "expression" 'bpython nil 'no-switch))

(defun py-execute-expression-bpython-dedicated ()
  "Send expression at point to Bpython unique interpreter. "
  (interactive)
  (py--execute-prepare "expression" 'bpython t nil))

(defun py-execute-expression-bpython-dedicated-switch ()
  "Send expression at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "expression" 'bpython t 'switch))

(defun py-execute-partial-expression-dedicated (&optional shell switch)
  "Send partial-expression to unique interpreter. "
  (interactive)
  (py--execute-prepare "partial-expression" shell t switch))

(defun py-execute-partial-expression-python ()
  "Send partial-expression at point to default interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "partial-expression" 'python nil nil))

(defun py-execute-partial-expression-python-switch ()
  "Send partial-expression at point to default interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "partial-expression" 'python nil 'switch))

(defun py-execute-partial-expression-python-no-switch ()
  "Send partial-expression at point to default interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p'

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "partial-expression" 'python nil 'no-switch))

(defun py-execute-partial-expression-python-dedicated ()
  "Send partial-expression at point to default unique interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "partial-expression" 'python t nil))

(defun py-execute-partial-expression-python-dedicated-switch ()
  "Send partial-expression at point to default unique interpreter and switch to result.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "partial-expression" 'python t 'switch))

(defun py-execute-partial-expression-ipython ()
  "Send partial-expression at point to IPython interpreter. "
  (interactive)
  (py--execute-prepare "partial-expression" 'ipython nil nil))

(defun py-execute-partial-expression-ipython-switch ()
  "Send partial-expression at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "partial-expression" 'ipython nil 'switch))

(defun py-execute-partial-expression-ipython-no-switch ()
  "Send partial-expression at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "partial-expression" 'ipython nil 'no-switch))

(defun py-execute-partial-expression-ipython-dedicated ()
  "Send partial-expression at point to IPython unique interpreter. "
  (interactive)
  (py--execute-prepare "partial-expression" 'ipython t nil))

(defun py-execute-partial-expression-ipython-dedicated-switch ()
  "Send partial-expression at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "partial-expression" 'ipython t 'switch))

(defun py-execute-partial-expression-python2 ()
  "Send partial-expression at point to Python2 interpreter. "
  (interactive)
  (py--execute-prepare "partial-expression" 'python2 nil nil))

(defun py-execute-partial-expression-python2-switch ()
  "Send partial-expression at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "partial-expression" 'python2 nil 'switch))

(defun py-execute-partial-expression-python2-no-switch ()
  "Send partial-expression at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "partial-expression" 'python2 nil 'no-switch))

(defun py-execute-partial-expression-python2-dedicated ()
  "Send partial-expression at point to Python2 unique interpreter. "
  (interactive)
  (py--execute-prepare "partial-expression" 'python2 t nil))

(defun py-execute-partial-expression-python2-dedicated-switch ()
  "Send partial-expression at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "partial-expression" 'python2 t 'switch))

(defun py-execute-partial-expression-jython ()
  "Send partial-expression at point to Jython interpreter. "
  (interactive)
  (py--execute-prepare "partial-expression" 'jython nil nil))

(defun py-execute-partial-expression-jython-switch ()
  "Send partial-expression at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "partial-expression" 'jython nil 'switch))

(defun py-execute-partial-expression-jython-no-switch ()
  "Send partial-expression at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "partial-expression" 'jython nil 'no-switch))

(defun py-execute-partial-expression-jython-dedicated ()
  "Send partial-expression at point to Jython unique interpreter. "
  (interactive)
  (py--execute-prepare "partial-expression" 'jython t nil))

(defun py-execute-partial-expression-jython-dedicated-switch ()
  "Send partial-expression at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "partial-expression" 'jython t 'switch))

(defun py-execute-partial-expression-python3 ()
  "Send partial-expression at point to Python3 interpreter. "
  (interactive)
  (py--execute-prepare "partial-expression" 'python3 nil nil))

(defun py-execute-partial-expression-python3-switch ()
  "Send partial-expression at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "partial-expression" 'python3 nil 'switch))

(defun py-execute-partial-expression-python3-no-switch ()
  "Send partial-expression at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "partial-expression" 'python3 nil 'no-switch))

(defun py-execute-partial-expression-python3-dedicated ()
  "Send partial-expression at point to Python3 unique interpreter. "
  (interactive)
  (py--execute-prepare "partial-expression" 'python3 t nil))

(defun py-execute-partial-expression-python3-dedicated-switch ()
  "Send partial-expression at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "partial-expression" 'python3 t 'switch))

(defun py-execute-partial-expression-bpython ()
  "Send partial-expression at point to Bpython interpreter. "
  (interactive)
  (py--execute-prepare "partial-expression" 'bpython nil nil))

(defun py-execute-partial-expression-bpython-switch ()
  "Send partial-expression at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "partial-expression" 'bpython nil 'switch))

(defun py-execute-partial-expression-bpython-no-switch ()
  "Send partial-expression at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "partial-expression" 'bpython nil 'no-switch))

(defun py-execute-partial-expression-bpython-dedicated ()
  "Send partial-expression at point to Bpython unique interpreter. "
  (interactive)
  (py--execute-prepare "partial-expression" 'bpython t nil))

(defun py-execute-partial-expression-bpython-dedicated-switch ()
  "Send partial-expression at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "partial-expression" 'bpython t 'switch))

(defun py-execute-line-dedicated (&optional shell switch)
  "Send line to unique interpreter. "
  (interactive)
  (py--execute-prepare "line" shell t switch))

(defun py-execute-line-python ()
  "Send line at point to default interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "line" 'python nil nil))

(defun py-execute-line-python-switch ()
  "Send line at point to default interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "line" 'python nil 'switch))

(defun py-execute-line-python-no-switch ()
  "Send line at point to default interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p'

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "line" 'python nil 'no-switch))

(defun py-execute-line-python-dedicated ()
  "Send line at point to default unique interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "line" 'python t nil))

(defun py-execute-line-python-dedicated-switch ()
  "Send line at point to default unique interpreter and switch to result.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "line" 'python t 'switch))

(defun py-execute-line-ipython ()
  "Send line at point to IPython interpreter. "
  (interactive)
  (py--execute-prepare "line" 'ipython nil nil))

(defun py-execute-line-ipython-switch ()
  "Send line at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "line" 'ipython nil 'switch))

(defun py-execute-line-ipython-no-switch ()
  "Send line at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "line" 'ipython nil 'no-switch))

(defun py-execute-line-ipython-dedicated ()
  "Send line at point to IPython unique interpreter. "
  (interactive)
  (py--execute-prepare "line" 'ipython t nil))

(defun py-execute-line-ipython-dedicated-switch ()
  "Send line at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "line" 'ipython t 'switch))

(defun py-execute-line-python2 ()
  "Send line at point to Python2 interpreter. "
  (interactive)
  (py--execute-prepare "line" 'python2 nil nil))

(defun py-execute-line-python2-switch ()
  "Send line at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "line" 'python2 nil 'switch))

(defun py-execute-line-python2-no-switch ()
  "Send line at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "line" 'python2 nil 'no-switch))

(defun py-execute-line-python2-dedicated ()
  "Send line at point to Python2 unique interpreter. "
  (interactive)
  (py--execute-prepare "line" 'python2 t nil))

(defun py-execute-line-python2-dedicated-switch ()
  "Send line at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "line" 'python2 t 'switch))

(defun py-execute-line-jython ()
  "Send line at point to Jython interpreter. "
  (interactive)
  (py--execute-prepare "line" 'jython nil nil))

(defun py-execute-line-jython-switch ()
  "Send line at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "line" 'jython nil 'switch))

(defun py-execute-line-jython-no-switch ()
  "Send line at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "line" 'jython nil 'no-switch))

(defun py-execute-line-jython-dedicated ()
  "Send line at point to Jython unique interpreter. "
  (interactive)
  (py--execute-prepare "line" 'jython t nil))

(defun py-execute-line-jython-dedicated-switch ()
  "Send line at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "line" 'jython t 'switch))

(defun py-execute-line-python3 ()
  "Send line at point to Python3 interpreter. "
  (interactive)
  (py--execute-prepare "line" 'python3 nil nil))

(defun py-execute-line-python3-switch ()
  "Send line at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "line" 'python3 nil 'switch))

(defun py-execute-line-python3-no-switch ()
  "Send line at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "line" 'python3 nil 'no-switch))

(defun py-execute-line-python3-dedicated ()
  "Send line at point to Python3 unique interpreter. "
  (interactive)
  (py--execute-prepare "line" 'python3 t nil))

(defun py-execute-line-python3-dedicated-switch ()
  "Send line at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "line" 'python3 t 'switch))

(defun py-execute-line-bpython ()
  "Send line at point to Bpython interpreter. "
  (interactive)
  (py--execute-prepare "line" 'bpython nil nil))

(defun py-execute-line-bpython-switch ()
  "Send line at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "line" 'bpython nil 'switch))

(defun py-execute-line-bpython-no-switch ()
  "Send line at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "line" 'bpython nil 'no-switch))

(defun py-execute-line-bpython-dedicated ()
  "Send line at point to Bpython unique interpreter. "
  (interactive)
  (py--execute-prepare "line" 'bpython t nil))

(defun py-execute-line-bpython-dedicated-switch ()
  "Send line at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "line" 'bpython t 'switch))

(defun py-execute-top-level-dedicated (&optional shell switch)
  "Send top-level to unique interpreter. "
  (interactive)
  (py--execute-prepare "top-level" shell t switch))

(defun py-execute-top-level-python ()
  "Send top-level at point to default interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "top-level" 'python nil nil))

(defun py-execute-top-level-python-switch ()
  "Send top-level at point to default interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "top-level" 'python nil 'switch))

(defun py-execute-top-level-python-no-switch ()
  "Send top-level at point to default interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p'

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "top-level" 'python nil 'no-switch))

(defun py-execute-top-level-python-dedicated ()
  "Send top-level at point to default unique interpreter.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "top-level" 'python t nil))

(defun py-execute-top-level-python-dedicated-switch ()
  "Send top-level at point to default unique interpreter and switch to result.

For `default' see value of `py-shell-name'"
  (interactive)
  (py--execute-prepare "top-level" 'python t 'switch))

(defun py-execute-top-level-ipython ()
  "Send top-level at point to IPython interpreter. "
  (interactive)
  (py--execute-prepare "top-level" 'ipython nil nil))

(defun py-execute-top-level-ipython-switch ()
  "Send top-level at point to IPython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "top-level" 'ipython nil 'switch))

(defun py-execute-top-level-ipython-no-switch ()
  "Send top-level at point to IPython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "top-level" 'ipython nil 'no-switch))

(defun py-execute-top-level-ipython-dedicated ()
  "Send top-level at point to IPython unique interpreter. "
  (interactive)
  (py--execute-prepare "top-level" 'ipython t nil))

(defun py-execute-top-level-ipython-dedicated-switch ()
  "Send top-level at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "top-level" 'ipython t 'switch))

(defun py-execute-top-level-python2 ()
  "Send top-level at point to Python2 interpreter. "
  (interactive)
  (py--execute-prepare "top-level" 'python2 nil nil))

(defun py-execute-top-level-python2-switch ()
  "Send top-level at point to Python2 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "top-level" 'python2 nil 'switch))

(defun py-execute-top-level-python2-no-switch ()
  "Send top-level at point to Python2 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "top-level" 'python2 nil 'no-switch))

(defun py-execute-top-level-python2-dedicated ()
  "Send top-level at point to Python2 unique interpreter. "
  (interactive)
  (py--execute-prepare "top-level" 'python2 t nil))

(defun py-execute-top-level-python2-dedicated-switch ()
  "Send top-level at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "top-level" 'python2 t 'switch))

(defun py-execute-top-level-jython ()
  "Send top-level at point to Jython interpreter. "
  (interactive)
  (py--execute-prepare "top-level" 'jython nil nil))

(defun py-execute-top-level-jython-switch ()
  "Send top-level at point to Jython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "top-level" 'jython nil 'switch))

(defun py-execute-top-level-jython-no-switch ()
  "Send top-level at point to Jython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "top-level" 'jython nil 'no-switch))

(defun py-execute-top-level-jython-dedicated ()
  "Send top-level at point to Jython unique interpreter. "
  (interactive)
  (py--execute-prepare "top-level" 'jython t nil))

(defun py-execute-top-level-jython-dedicated-switch ()
  "Send top-level at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "top-level" 'jython t 'switch))

(defun py-execute-top-level-python3 ()
  "Send top-level at point to Python3 interpreter. "
  (interactive)
  (py--execute-prepare "top-level" 'python3 nil nil))

(defun py-execute-top-level-python3-switch ()
  "Send top-level at point to Python3 interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "top-level" 'python3 nil 'switch))

(defun py-execute-top-level-python3-no-switch ()
  "Send top-level at point to Python3 interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "top-level" 'python3 nil 'no-switch))

(defun py-execute-top-level-python3-dedicated ()
  "Send top-level at point to Python3 unique interpreter. "
  (interactive)
  (py--execute-prepare "top-level" 'python3 t nil))

(defun py-execute-top-level-python3-dedicated-switch ()
  "Send top-level at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "top-level" 'python3 t 'switch))

(defun py-execute-top-level-bpython ()
  "Send top-level at point to Bpython interpreter. "
  (interactive)
  (py--execute-prepare "top-level" 'bpython nil nil))

(defun py-execute-top-level-bpython-switch ()
  "Send top-level at point to Bpython interpreter.

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py--execute-prepare "top-level" 'bpython nil 'switch))

(defun py-execute-top-level-bpython-no-switch ()
  "Send top-level at point to Bpython interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py--execute-prepare "top-level" 'bpython nil 'no-switch))

(defun py-execute-top-level-bpython-dedicated ()
  "Send top-level at point to Bpython unique interpreter. "
  (interactive)
  (py--execute-prepare "top-level" 'bpython t nil))

(defun py-execute-top-level-bpython-dedicated-switch ()
  "Send top-level at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py--execute-prepare "top-level" 'bpython t 'switch))

;;; Subprocess utilities and filters
(defun py--postprocess-intern (buf &optional origline)
  "Highlight exceptions found in BUF.
If an exception occurred return error-string, otherwise return nil.  BUF must exist.

Indicate LINE if code wasn't run from a file, thus remember line of source buffer "
  (let* ((pmx (copy-marker (point-max)))
	 file bol estring ecode limit erg)
    ;; (switch-to-buffer (current-buffer))
    (goto-char pmx)
    (sit-for 0.1 t)
    (save-excursion
      (unless (looking-back py-pdbtrack-input-prompt)
        (forward-line -1)
        (end-of-line)
        (when (or (re-search-backward py-shell-prompt-regexp nil t 1)
                  (re-search-backward (concat ipython-de-input-prompt-regexp "\\|" ipython-de-output-prompt-regexp) nil t 1))
          (save-excursion
            (when (re-search-forward "File \"\\(.+\\)\", line \\([0-9]+\\)\\(.*\\)$" nil t)
              (setq erg (copy-marker (point)))
              (delete-region (progn (beginning-of-line)
				    (save-match-data
				      (when (looking-at
					     ;; all prompt-regexp known
					     py-fast-filter-re)
					(goto-char (match-end 0))))

				    (skip-chars-forward " \t\r\n\f")(point)) (line-end-position))
	      (insert (concat "    File " (buffer-name py-exception-buffer) ", line "
			      (prin1-to-string origline)))))
	  ;; Delete links at temporary files created by py--execute-buffer-finally
	  ;; these are let-bound as `tempbuf'
	  (and (boundp 'tempbuf)
	       ;; (message "%s" tempbuf)
	       (search-forward (buffer-name tempbuf) nil t)
	       (delete-region (line-beginning-position) (1+ (line-end-position))))
          ;; if no buffer-file exists, signal "Buffer", not "File(when
          (when erg
            (goto-char erg)
            ;; (forward-char -1)
            ;; (skip-chars-backward "^\t\r\n\f")
            ;; (skip-chars-forward " \t")
            (save-match-data
              (and (not (buffer-file-name
                         (or
                          (get-buffer py-exception-buffer)
                          (get-buffer (file-name-nondirectory py-exception-buffer)))))
		   (string-match "^[ \t]*File" (buffer-substring-no-properties (point) (line-end-position)))
		   (looking-at "[ \t]*File")
		   (replace-match " Buffer")))
            (add-to-list 'py-error origline)
            (add-to-list 'py-error (buffer-name py-exception-buffer))

	    ;; (put-text-property (line-beginning-position) (line-end-position) 'font-lock-face 'comint-error)
            ;; (put-text-property (line-beginning-position) (line-end-position) 'font-lock-face 'comint-highlight-prompt)
	    ;; (overlay-put (make-overlay (line-beginning-position)
	    ;; (1- (line-end-position)))
	    ;; 'face 'highlight)

            ;; If not file exists, just a buffer, correct message
            (forward-line 1)
            (when (looking-at "[ \t]*\\([^\t\n\r\f]+\\)[ \t]*$")
              (setq estring (match-string-no-properties 1))
              ;; (setq ecode (buffer-substring-no-properties (line-end-position)
              ;; (progn (re-search-forward comint-prompt-regexp nil t 1)(match-beginning 0))))
              (setq ecode (replace-regexp-in-string "[ \n\t\f\r^]+" " " estring))
              (add-to-list 'py-error ecode t))))))
    ;;))
    py-error))

(defun py-remove-overlays-at-point ()
  "Remove overlays as set when `py-highlight-error-source-p' is non-nil. "
  (interactive "*")
  (delete-overlay (car (overlays-at (point)))))

(defun py--jump-to-exception-intern (action exception-buffer origline)
  (let (erg)
    (set-buffer exception-buffer)
    (goto-char (point-min))
    (forward-line (1- origline))
    (push-mark)
    (and (search-forward action (line-end-position) t)
         (and py-verbose-p (message "Exception in file %s on line %d" py-exception-buffer origline))
         (and py-highlight-error-source-p
              (setq erg (make-overlay (match-beginning 0) (match-end 0)))
              (overlay-put erg
                           'face 'highlight)))))

(defun py--jump-to-exception (py-error origline &optional file)
  "Jump to the Python code in FILE at LINE."
  (let (
        ;; (inhibit-point-motion-hooks t)
        (file (or file (car py-error)))
        (line (cadr py-error))
        (action (nth 2 py-error))
        (errm (nth 3 py-error)))
    (cond ((and py-exception-buffer
                (buffer-live-p py-exception-buffer))
           ;; (pop-to-buffer procbuf)
           (py--jump-to-exception-intern action py-exception-buffer origline))
          ((ignore-errors (file-readable-p file))
           (find-file file)
           (py--jump-to-exception-intern action (get-buffer (file-name-nondirectory file origline))))
          ((buffer-live-p (get-buffer file))
           (set-buffer file)
           (py--jump-to-exception-intern action file origline))
          (t (setq file (find-file (read-file-name "Exception file: "
                                                   nil
                                                   file t)))
             (py--jump-to-exception-intern action file origline)))))

(defun py-down-exception (&optional bottom)
  "Go to the next line down in the traceback.

With \\[universal-argument] (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack."
  (interactive "P")
  (py--find-next-exception-prepare 'down (when (eq 4 (prefix-numeric-value bottom)) "BOTTOM")))

(defun py-up-exception (&optional top)
  "Go to the previous line up in the traceback.

With \\[universal-argument] (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack."
  (interactive "P")
  (unless py-last-exeption-buffer (setq py-last-exeption-buffer (current-buffer)))
  (py--find-next-exception-prepare 'up (when (eq 4 (prefix-numeric-value top)) "TOP")))

(defun py--find-next-exception-prepare (direction start)
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
            (py--find-next-exception 'bob buffer 're-search-forward "Top")
          (py--find-next-exception 'bol buffer 're-search-backward "Top"))
      (if (string= start "BOTTOM")
          (py--find-next-exception 'eob buffer 're-search-backward "Bottom")
        (py--find-next-exception 'eol buffer 're-search-forward "Bottom")))))

(defun py--find-next-exception (start buffer searchdir errwhere)
  "Find the next Python exception and jump to the code that caused it.
START is the buffer position in BUFFER from which to begin searching
for an exception.  SEARCHDIR is a function, either
`re-search-backward' or `re-search-forward' indicating the direction
to search.  ERRWHERE is used in an error message if the limit (top or
bottom) of the trackback stack is encountered."
  (let (file line)
    (save-excursion
      (set-buffer buffer)
      (goto-char (py--point start))
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
           (py--jump-to-exception (car info) origline (cdr info)))))))

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

(defun py--fetch-error (buf &optional origline)
  "Highlight exceptions found in BUF.
If an exception occurred return error-string, otherwise return nil.  BUF must exist.

Indicate LINE if code wasn't run from a file, thus remember line of source buffer "
  (let* ((pmx (copy-marker (point-max)))
	 file bol estring ecode limit erg)
    ;; (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    (when (re-search-forward "File \"\\(.+\\)\", line \\([0-9]+\\)\\(.*\\)$" nil t)
      (setq erg (copy-marker (point)))
      (delete-region (progn (beginning-of-line)
			    (save-match-data
			      (when (looking-at
				     ;; all prompt-regexp known
				     py-fast-filter-re)
				(goto-char (match-end 0))))

			    (skip-chars-forward " \t\r\n\f")(point)) (line-end-position))
      (insert (concat "    File " (buffer-name py-exception-buffer) ", line "
		      (prin1-to-string origline))))
    (when erg
      (goto-char erg)
      ;; (forward-char -1)
      ;; (skip-chars-backward "^\t\r\n\f")
      ;; (skip-chars-forward " \t")
      (save-match-data
	(and (not (buffer-file-name
		   (or
		    (get-buffer py-exception-buffer)
		    (get-buffer (file-name-nondirectory py-exception-buffer)))))
	     (string-match "^[ \t]*File" (buffer-substring-no-properties (point) (line-end-position)))
	     (looking-at "[ \t]*File")
	     (replace-match " Buffer")))
      ;; (add-to-list 'py-error origline)
      ;; (add-to-list 'py-error (concat "Buffer " (buffer-name py-exception-buffer) ", line " (prin1-to-string origline)))
      (setq py-error (buffer-substring-no-properties (point-min) (point-max)))
      py-error)))

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
  "Evaluate STRING in Python process."
  (interactive "sPython command: ")
  (let* ((proc (or process (get-buffer-process (py-shell))))
	 (buffer (process-buffer proc)))
    ;; (comint-send-string proc "\n")
    (goto-char (point-max))
    (comint-send-string proc string)
    (goto-char (point-max))
    (unless (string-match "\n\\'" string)
      ;; Make sure the text is properly LF-terminated.
      (comint-send-string proc "\n"))
    (when py-debug-p (message "%s" (current-buffer)))
    (goto-char (point-max))))

(defun py-send-file (file-name &optional process temp-file-name)
  "Send FILE-NAME to Python PROCESS.
If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME."
  (interactive "fFile to send: ")
  (let* ((process (or process (get-buffer-process (py-shell))))
         (temp-file-name (when temp-file-name
                           (expand-file-name temp-file-name)))
         (file-name (or (expand-file-name file-name) temp-file-name)))
    (when (not file-name)
      (error "If FILE-NAME is nil then TEMP-FILE-NAME must be non-nil"))
    (py-send-string
     (format
      (concat "__pyfile = open('''%s''');"
              "exec(compile(__pyfile.read(), '''%s''', 'exec'));"
              "__pyfile.close()")
      file-name file-name)
     process)))
;;;

(when (or py-load-pymacs-p (featurep 'pymacs))
  (defun py-load-pycomplete ()
    "Load Pymacs based pycomplete."
    (interactive)
    (let* ((path (getenv "PYTHONPATH"))
           (py-install-directory (cond ((string= "" py-install-directory)
                                        (py-guess-py-install-directory))
                                       (t (py--normalize-directory py-install-directory))))
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

;;; Hooks
;; arrange to kill temp files when Emacs exists
(add-hook 'kill-emacs-hook 'py--kill-emacs-hook)
(add-hook 'comint-output-filter-functions 'py--pdbtrack-track-stack-file)

;; (add-hook 'py-shell-mode-hook 'py-send-shell-setup-code)

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

(when py--warn-tmp-files-left-p
  (add-hook 'python-mode-hook 'py--warn-tmp-files-left))

;; FixMe: for unknown reasons this is not done by mode
(if (file-readable-p abbrev-file-name)
    (add-hook 'python-mode-hook
              (lambda ()
                (setq py-this-abbrevs-changed abbrevs-changed)
                (load abbrev-file-name nil t)
                (setq abbrevs-changed py-this-abbrevs-changed)))
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
;;                 (load (concat (py--normalize-directory py-install-directory) "extensions" py-separator-char "highlight-indentation.el"))))))

(add-to-list 'same-window-buffer-names (purecopy "*Python*"))
(add-to-list 'same-window-buffer-names (purecopy "*IPython*"))

(add-to-list 'auto-mode-alist (cons (purecopy "\\.py\\'")  'python-mode))

;; (add-to-list 'interpreter-mode-alist
;; (cons (purecopy "[bi]*python[0-9.]*") 'python-mode))

;; (add-to-list 'interpreter-mode-alist
;; (cons (purecopy "jython[0-9.]*") 'jython-mode))

(add-to-list 'magic-mode-alist
	     '("!#[ \t]*/.*[jp]ython[0-9.]*" . python-mode))

(defun py--set-auto-fill-values ()
  "Internal use by `py--run-auto-fill-timer'"
  (let ((pps (syntax-ppss)))
    (cond ((and (nth 4 pps)(numberp py-comment-fill-column))
           (setq fill-column py-comment-fill-column))
          ((and (nth 3 pps)(numberp py-docstring-fill-column))
           (set (make-local-variable 'fill-column) py-docstring-fill-column))
          (t (setq fill-column py-fill-column-orig)))))

(defun py--run-auto-fill-timer ()
  "Set fill-column to values of `py-docstring-fill-column' resp. to `py-comment-fill-column' according to environment. "
  (when py-auto-fill-mode
    (unless py-autofill-timer
      (setq py-autofill-timer
            (run-with-idle-timer
             py-autofill-timer-delay t
             'py--set-auto-fill-values)))))

(defun py--run-unfontify-timer (&optional buffer)
  "Unfontify the shell banner-text "
  (when py--shell-unfontify
    (let ((buffer (or buffer (current-buffer)))
	  done)
      (if (and (buffer-live-p buffer)(eq major-mode 'py-shell-mode))
	  (unless py--timer
	    (setq py--timer
		  (run-with-idle-timer
		   (if py--timer-delay (setq py--timer-delay 3)
		     (setq py--timer-delay 0.1))
		   t
		   #'py--unfontify-banner buffer)))
	(cancel-timer py--timer)))))

(defun py-unload-python-el ()
  "Unloads python-mode delivered by shipped python.el

Removes python-skeleton forms from abbrevs.
These would interfere when inserting forms heading a block"
  (interactive)
  (let (done)
    (when (featurep 'python) (unload-feature 'python t))
    (when (file-readable-p abbrev-file-name)
      (find-file abbrev-file-name)
      (goto-char (point-min))
      (while (re-search-forward "^.+python-skeleton.+$" nil t 1)
	(setq done t)
	(delete-region (match-beginning 0) (1+ (match-end 0))))
      (when done (write-file abbrev-file-name)
	    ;; now reload
	    (read-abbrev-file abbrev-file-name))
      (kill-buffer (file-name-nondirectory abbrev-file-name)))))
;;;
(defun py-complete-auto ()
  "Auto-complete function using py-complete. "
  ;; disable company
  ;; (when company-mode (company-mode))
  (let ((modified (buffer-chars-modified-tick)))
    ;; don't try completion if buffer wasn't modified
    (unless (eq modified py-complete-last-modified)
      (if py-auto-completion-mode-p
	  (if (string= "*PythonCompletions*" (buffer-name (current-buffer)))
	      (sit-for 0.1 t)
	    (if
		(eq py-auto-completion-buffer (current-buffer))
		;; not after whitespace, TAB or newline
		(unless (member (char-before) (list 32 9 10))
		  (py-complete)
		  (setq py-complete-last-modified (buffer-chars-modified-tick)))
	      (setq py-auto-completion-mode-p nil
		    py-auto-completion-buffer nil)
	      (cancel-timer py--auto-complete-timer)))))))

(define-derived-mode py-auto-completion-mode python-mode "Pac"
  "Run auto-completion"
  ;; disable company
  ;; (when company-mode (company-mode))
  (if py-auto-completion-mode-p
      (progn
	(setq py-auto-completion-mode-p nil
	      py-auto-completion-buffer nil)
	(when (timerp py--auto-complete-timer)(cancel-timer py--auto-complete-timer)))
    (setq py-auto-completion-mode-p t
	  py-auto-completion-buffer (current-buffer))
    (setq py--auto-complete-timer
	  (run-with-idle-timer
	   py--auto-complete-timer-delay
	   ;; 1
	   t
	   #'py-complete-auto))))

;;;
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
`py-split-window-on-execute'		When non-nil split windows
`py-switch-buffers-on-execute-p'	When non-nil switch to the Python output buffer

See available customizations listed in files variables-python-mode at directory doc

\\{python-mode-map}"
  :group 'python-mode
  ;; Local vars
  (set (make-local-variable 'electric-indent-inhibit) nil)
  (set (make-local-variable 'outline-regexp)
       (concat (mapconcat 'identity
                          (mapcar #'(lambda (x) (concat "^\\s-*" x "\\_>"))
                                  py-outline-mode-keywords)
                          "\\|")))
  (if py-use-font-lock-doc-face-p
      (set (make-local-variable 'font-lock-defaults)
           '(python-font-lock-keywords nil nil nil nil
				       (font-lock-syntactic-keywords
					. py-font-lock-syntactic-keywords)
				       (font-lock-syntactic-face-function
					. py--font-lock-syntactic-face-function)))
    (set (make-local-variable 'font-lock-defaults)
         '(python-font-lock-keywords nil nil nil nil
				     (font-lock-syntactic-keywords
				      . py-font-lock-syntactic-keywords))))
  ;; avoid to run py-choose-shell again from `py--fix-start'
  (cond ((and (boundp 'py-buffer-name) py-buffer-name)
	 (if (string-match "python3" py-buffer-name)
	     (font-lock-add-keywords 'python-mode
				     '(("\\<print\\>" . 'py-builtins-face)))
	   '(("\\<print\\>" . 'font-lock-keyword-face))))
	((string-match "python3" (py-choose-shell))
	 (font-lock-add-keywords 'python-mode
				 '(("\\<print\\>" . 'py-builtins-face))))
	(t (font-lock-add-keywords 'python-mode
				   '(("\\<print\\>" . 'font-lock-keyword-face)))))

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
  (set (make-local-variable 'comment-indent-function) #'py--comment-indent-function)
  (set (make-local-variable 'indent-region-function) 'py-indent-region)
  (set (make-local-variable 'indent-line-function) 'py-indent-line)
  (set (make-local-variable 'hs-hide-comments-when-hiding-all) 'py-hide-comments-when-hiding-all)
  (set (make-local-variable 'outline-heading-end-regexp) ":[^\n]*\n")
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
  ;; (set (make-local-variable 'imenu-create-index-function) 'py--imenu-create-index-function)
  (setq imenu-create-index-function 'py--imenu-create-index-function)
  (add-hook 'python-mode-hook (lambda ()(setq imenu-create-index-function py--imenu-create-index-function)))
  (and py-guess-py-install-directory-p (py-set-load-path))
  ;;  (unless gud-pdb-history (when (buffer-file-name) (add-to-list 'gud-pdb-history (buffer-file-name))))
  (and py-autopair-mode
       (load-library "autopair")
       (add-hook 'python-mode-hook
                 #'(lambda ()
                     (setq autopair-handle-action-fns
                           (list #'autopair-default-handle-action
                                 #'autopair-python-triple-quote-action))))
       (py-autopair-mode-on))
  (when py-trailing-whitespace-smart-delete-p
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local))
  (when py-pdbtrack-do-tracking-p
    (add-hook 'comint-output-filter-functions 'py--pdbtrack-track-stack-file t))
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
  (if py-auto-fill-mode
      (add-hook 'python-mode-hook 'py--run-auto-fill-timer)
    (remove-hook 'python-mode-hook 'py--run-auto-fill-timer))
  ;; caused insert-file-contents error lp:1293172
  ;;  (add-hook 'after-change-functions 'py--after-change-function nil t)
  (if py-defun-use-top-level-p
      (progn
        (set (make-local-variable 'beginning-of-defun-function) 'py-beginning-of-top-level)
        (set (make-local-variable 'end-of-defun-function) 'py-end-of-top-level)
        (define-key python-mode-map [(control meta a)] 'py-beginning-of-top-level)
        (define-key python-mode-map [(control meta e)] 'py-end-of-top-level))
    (set (make-local-variable 'beginning-of-defun-function) 'py-beginning-of-def-or-class)
    (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class)
    (define-key python-mode-map [(control meta a)] 'py-beginning-of-def-or-class)
    (define-key python-mode-map [(control meta e)] 'py-end-of-def-or-class))
  (when (and py--imenu-create-index-p
             (fboundp 'imenu-add-to-menubar)
             (ignore-errors (require 'imenu)))
    (setq imenu--index-alist (funcall py--imenu-create-index-function))
    ;; (setq imenu--index-alist (py--imenu-create-index-new))
    ;; (message "imenu--index-alist: %s" imenu--index-alist)
    (imenu-add-to-menubar "PyIndex"))
  ;; add the menu
  (when py-menu
    (easy-menu-add py-menu))
  (when py-hide-show-minor-mode-p (hs-minor-mode 1))
  (when py-outline-minor-mode-p (outline-minor-mode 1))
  (when (interactive-p) (message "python-mode loaded from: %s" python-mode-message-string)))

;; backward compatibility
;; some third party stuff relying on v5 serie might use this
(defalias 'py-goto-beyond-block 'py-end-of-block-bol)
(defalias 'py-goto-beyond-final-line 'py-end-of-statement-bol)

(define-derived-mode py-shell-mode comint-mode "Py"
  "Major mode for interacting with a Python process.
A Python process can be started with \\[py-shell].

You can send text to the Python process from other buffers
containing Python source.
 * \\[py-execute-region] sends the current region to the Python process.

Sets basic comint variables, see also versions-related stuff in `py-shell'.
\\{py-shell-mode-map}"
  :group 'python-mode
  ;; (require 'ansi-color) ; for ipython
  (setq mode-line-process '(":%s"))
  (when py-fontify-shell-buffer-p
    (set (make-local-variable 'font-lock-defaults)
	 '(python-font-lock-keywords nil nil nil nil
				     (font-lock-syntactic-keywords
				      . py-font-lock-syntactic-keywords))))
  (setenv "PAGER" "cat")
  (setenv "TERM" "dumb")
  (set-syntax-table python-mode-syntax-table)
  (set (make-local-variable 'py--shell-unfontify) 'py-shell-unfontify-p)

  (if py-shell-unfontify-p
      (add-hook 'py-shell-mode-hook #'py--run-unfontify-timer (current-buffer))
    (remove-hook 'py-shell-mode-hook 'py--run-unfontify-timer))

  ;; comint settings
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
  (remove-hook 'comint-output-filter-functions 'font-lock-extend-jit-lock-region-after-change t)

  (make-local-variable 'comint-output-filter-functions)
  (set (make-local-variable 'comint-input-filter) 'py--input-filter)
  (set (make-local-variable 'compilation-error-regexp-alist)
       py-compilation-regexp-alist)
  (set (make-local-variable 'comint-input-filter) 'py-history-input-filter)
  (set (make-local-variable 'comint-prompt-read-only) py-shell-prompt-read-only)
  ;; It might be useful having a different setting of `comint-use-prompt-regexp' in py-shell - please report when a use-case shows up
  ;; (set (make-local-variable 'comint-use-prompt-regexp) nil)
  (set (make-local-variable 'compilation-error-regexp-alist)
       py-compilation-regexp-alist)
  ;; (setq completion-at-point-functions nil)

  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "^[ \t]*#+ *")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-indent-function) #'py--comment-indent-function)
  (set (make-local-variable 'indent-region-function) 'py-indent-region)
  (set (make-local-variable 'indent-line-function) 'py-indent-line)
  (set (make-local-variable 'inhibit-point-motion-hooks) t)
  (set (make-local-variable 'comint-input-sender) 'py--shell-simple-send)
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
                       (concat (getenv "PYTHONHISTORY") "/" (py--report-executable py-buffer-name) "_history")
                     py-ipython-history)
                 py-ipython-history))))
  (comint-read-input-ring t)
  (compilation-shell-minor-mode 1)
  ;;
  (if py-complete-function
      (progn
  	(add-hook 'completion-at-point-functions
  		  py-complete-function nil 'local)
  	(add-to-list (make-local-variable 'comint-dynamic-complete-functions)
  		     py-complete-function))
    (add-hook 'completion-at-point-functions
              'py-shell-complete nil 'local)
    (add-to-list (make-local-variable 'comint-dynamic-complete-functions)
  		 'py-shell-complete))
  (when py-shell-menu
    (easy-menu-add py-menu)))

(provide 'python-mode)
;; python-mode.el ends here
