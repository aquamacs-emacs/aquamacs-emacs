;;; matlab-load.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (matlab-cedet-setup) "cedet-matlab" "cedet-matlab.el"
;;;;;;  (19026 21759))
;;; Generated autoloads from cedet-matlab.el

(autoload 'matlab-cedet-setup "cedet-matlab" "\
Update various paths to get SRecode to identify our macros.

\(fn)" t nil)

;;;***

;;;### (autoloads (company-matlab-shell) "company-matlab-shell" "company-matlab-shell.el"
;;;;;;  (19026 24400))
;;; Generated autoloads from company-matlab-shell.el

(autoload 'company-matlab-shell "company-matlab-shell" "\
A `company-mode' completion back-end for Matlab-Shell.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (matlab-shell matlab-mode) "matlab" "matlab.el"
;;;;;;  (19086 58944))
;;; Generated autoloads from matlab.el

(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))

(autoload 'matlab-mode "matlab" "\
MATLAB(R) mode is a major mode for editing MATLAB dot-m files.
\\<matlab-mode-map>
Convenient editing commands are:
 \\[matlab-comment-region]   - Comment/Uncomment out a region of code.
 \\[matlab-fill-comment-line] - Fill the current comment line.
 \\[matlab-fill-region] - Fill code and comments in region.
 \\[matlab-fill-paragraph]     - Refill the current command or comment.
 \\[matlab-complete-symbol]   - Symbol completion of matlab symbolsbased on the local syntax.
 \\[matlab-indent-sexp] - Indent syntactic block of code.

Convenient navigation commands are:
 \\[matlab-beginning-of-command]   - Move to the beginning of a command.
 \\[matlab-end-of-command]   - Move to the end of a command.
 \\[matlab-beginning-of-defun] - Move to the beginning of a function.
 \\[matlab-end-of-defun] - Move do the end of a function.
 \\[matlab-forward-sexp] - Move forward over a syntactic block of code.
 \\[matlab-backward-sexp] - Move backwards over a syntactic block of code.

Convenient template insertion commands:
 \\[tempo-template-matlab-function] - Insert a function definition.
 \\[tempo-template-matlab-if] - Insert an IF END block.
 \\[tempo-template-matlab-for] - Insert a FOR END block.
 \\[tempo-template-matlab-switch] - Insert a SWITCH END statement.
 \\[matlab-insert-next-case] - Insert the next CASE condition in a SWITCH.
 \\[matlab-insert-end-block] - Insert a matched END statement.  With optional ARG, reindent.
 \\[matlab-stringify-region] - Convert plaintext in region to a string with correctly quoted chars.

Variables:
  `matlab-indent-level'		Level to indent blocks.
  `matlab-cont-level'		Level to indent continuation lines.
  `matlab-cont-requires-ellipsis' Does your MATLAB support implied elipsis.
  `matlab-case-level'		Level to unindent case statements.
  `matlab-indent-past-arg1-functions'
                                Regexp of functions to indent past the first
                                  argument on continuation lines.
  `matlab-maximum-indents'      List of maximum indents during lineups.
  `matlab-comment-column'       Goal column for on-line comments.
  `fill-column'			Column used in auto-fill.
  `matlab-indent-function-body' If non-nil, indents body of MATLAB functions.
  `matlab-functions-have-end'	If non-nil, MATLAB functions terminate with end.
  `matlab-return-function'	Customize RET handling with this function.
  `matlab-auto-fill'            Non-nil, do auto-fill at startup.
  `matlab-fill-code'            Non-nil, auto-fill code.
  `matlab-fill-strings'         Non-nil, auto-fill strings.
  `matlab-verify-on-save-flag'  Non-nil, enable code checks on save.
  `matlab-highlight-block-match-flag'
                                Enable matching block begin/end keywords.
  `matlab-vers-on-startup'	If t, show version on start-up.
  `matlab-handle-simulink'      If t, enable simulink keyword highlighting.

All Key Bindings:
\\{matlab-mode-map}

\(fn)" t nil)

(autoload 'matlab-shell "matlab" "\
Create a buffer with MATLAB running as a subprocess.

MATLAB shell cannot work on the MS Windows platform because MATLAB is not
a console application.

\(fn)" t nil)

;;;***

;;;### (autoloads (mlint-minor-mode) "mlint" "mlint.el" (19086 58971))
;;; Generated autoloads from mlint.el

(autoload 'mlint-minor-mode "mlint" "\
Toggle mlint minor mode, a mode for showing mlint errors.
With prefix ARG, turn mlint minor mode on iff ARG is positive.
\\{mlint-minor-mode-map\\}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (semantic-default-matlab-setup) "semantic-matlab"
;;;;;;  "semantic-matlab.el" (19026 21813))
;;; Generated autoloads from semantic-matlab.el

(autoload 'semantic-default-matlab-setup "semantic-matlab" "\
Set up a buffer for parsing of MATLAB files.

\(fn)" nil nil)

;;;***

;;;### (autoloads (tlc-mode) "tlc" "tlc.el" (17295 18676))
;;; Generated autoloads from tlc.el

(autoload 'tlc-mode "tlc" "\
Major mode for editing Tlc files, or files found in tlc directories.

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("\\.tlc$" .tlc-mode))

;;;***

;;;### (autoloads nil nil ("hg2.el" "matlab-publish.el" "semanticdb-matlab.el")
;;;;;;  (19086 58974 113326))

;;;***

(provide 'matlab-load)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; matlab-load.el ends here
