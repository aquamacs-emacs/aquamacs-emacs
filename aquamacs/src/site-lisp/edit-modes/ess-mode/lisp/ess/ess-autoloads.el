;;; ess-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "ess" "../../../../tmp/ess-18.10.2/lisp/ess.el"
;;;;;;  "c87efa59b8ffce65358f0a774f132598")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess.el

(autoload 'ess-mode "ess" "\
Major mode for editing ESS source.
Optional arg ALIST describes how to customize the editing mode.
Optional arg PROC-NAME is name of associated inferior process.

\\{ess-mode-map}

Extra binding to note:  'ESC C-\\' indent-region.

Entry to this mode runs the hooks in ess-mode-hook.

You can send text to the inferior ESS process from other buffers containing
ESS source.
    `ess-eval-region' sends the current region to the ESS process.
    `ess-eval-buffer' sends the current buffer to the ESS process.
    `ess-eval-function' sends the current function to the ESS process.
    `ess-eval-line' sends the current line to the ESS process.
    `ess-beginning-of-function' and `ess-end-of-function' move the point to
        the beginning and end of the current ESS function.
    `ess-switch-to-ESS' switches the current buffer to the ESS process buffer.
    `ess-switch-to-end-of-ESS' switches the current buffer to the ESS process
        buffer and puts point at the end of it.

    `ess-eval-region-and-go', `ess-eval-buffer-and-go',
        `ess-eval-function-and-go', and `ess-eval-line-and-go' switch to the S
        process buffer after sending their text.

    `ess-load-file' sources a file of commands to the ESS process.

\\[ess-indent-command] indents for ESS code.
\\[backward-delete-char-untabify] converts tabs to spaces as it moves back.
Comments are indented in a similar way to Emacs-lisp mode:
       `###'     beginning of line
       `##'      the same level of indentation as the code
       `#'       the same column on the right, or to the right of such a
                 column if that is not possible.(default value 40).
                 \\[indent-for-comment] command automatically inserts such a
                 `#' in the right place, or aligns such a comment if it is
                 already inserted.
\\[ess-indent-exp] command indents each line of the syntactic unit following point.

Variables controlling indentation style:
 `ess-tab-always-indent'
    Non-nil means TAB in ESS mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `ess-auto-newline'
    Non-nil means automatically newline before and after braces inserted in S
    code.
 `ess-indent-offset'
    Indentation of ESS statements within surrounding block.
    The surrounding block's indentation is the indentation of the line on
    which the open-brace appears.
 `ess-offset-block'
    Indentation of blocks opened with curly braces or anonymous parentheses.
 `ess-offset-arguments'
    Indentation of function arguments or bracket indices.
 `ess-offset-arguments-newline'
    Indentation of function arguments or bracket indices when the opening
    delimiter is immediately followed by a newline.
 `ess-offset-continued'
    Indentation style for continued statements.
 `ess-align-nested-calls'
    Functions whose nested calls should be aligned.
 `ess-align-arguments-in-calls'
    Calls in which arguments should be aligned.
 `ess-align-continuations-in-calls'
    Whether ignore indentation after an operator in calls
 `ess-align-blocks'
    Blocks that should always be aligned vertically.
 `ess-indent-from-lhs'
    Whether function calls given as argument should be indented from the
    parameter name.
 `ess-indent-from-chain-start'
    Whether to indent arguments from the first of several consecutive calls.
 `ess-indent-with-fancy-comments'
    Non-nil means distinguish between #, ##, and ### for indentation.

Furthermore, \\[ess-set-style] command enables you to set up predefined ess-mode
indentation style. At present, predefined style are `BSD', `GNU', `K&R', `C++',
`CLB' (quoted from C language style).

\(fn &optional ALIST PROC-NAME IS-DERIVED)" nil nil)

(autoload 'ess-dump-object-into-edit-buffer "ess" "\
Edit an ESS OBJECT in its own buffer.
Without a prefix argument, this simply finds the file pointed to by
`ess-source-directory'.  If this file does not exist, or if a
prefix argument is given, a dump() command is sent to the ESS process to
generate the source buffer.

\(fn OBJECT)" t nil)

(autoload 'ess-version "ess" "\
Return a string with ESS version information.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ess-bugs-d" "../../../../tmp/ess-18.10.2/lisp/ess-bugs-d.el"
;;;;;;  "264e1d17e1fccae31e5c23798cc64871")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-bugs-d.el

(autoload 'ess-bugs-mode "ess-bugs-d" "\
ESS[BUGS]: Major mode for BUGS.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode))

(add-to-list 'auto-mode-alist '("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode))

(add-to-list 'auto-mode-alist '("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode))

;;;***

;;;### (autoloads nil "ess-generics" "../../../../tmp/ess-18.10.2/lisp/ess-generics.el"
;;;;;;  "598ab07ddbcaeb07e8129bcde3d76b6c")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-generics.el

(autoload 'ess-defgeneric "ess-generics" "\
Define a new function, as with `defun', which can be overloaded.
NAME is the name of the function to create. ARGS are the
arguments to the function. DOCSTRING is a documentation string to
describe the function.  The docstring will automatically have
details about its overload symbol appended to the end. BODY is
code that would be run when there is no override defined.  The
default is to signal error if {name}-function is not defined.

\(fn NAME ARGS DOCSTRING &rest BODY)" nil t)

(function-put 'ess-defgeneric 'doc-string-elt '3)

(function-put 'ess-defgeneric 'lisp-indent-function 'defun)

;;;***

;;;### (autoloads nil "ess-gretl" "../../../../tmp/ess-18.10.2/lisp/ess-gretl.el"
;;;;;;  "f21cdd79d9add8db8067cc988aee4a69")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-gretl.el

(autoload 'gretl-mode "ess-gretl" "\
Major mode for editing gretl source.  See `ess-mode' for more help.

\(fn &optional PROC-NAME)" t nil)

(autoload 'gretl "ess-gretl" "\
Call 'gretl',
Optional prefix (C-u) allows to set command line arguments, such as
--vsize.  This should be OS agnostic.
If you have certain command line arguments that should always be passed
to gretl, put them in the variable `inferior-gretl-args'.

\(fn &optional START-ARGS)" t nil)

;;;***

;;;### (autoloads nil "ess-help" "../../../../tmp/ess-18.10.2/lisp/ess-help.el"
;;;;;;  "b8e5042299366152df5444cd47422de6")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-help.el

(autoload 'ess-display-help-on-object "ess-help" "\
Display documentation for OBJECT in another window.
If prefix arg is given, force an update of the cached help topics
and query the ESS process for the help file instead of reusing an
existing buffer if it exists. Uses the variable
`inferior-ess-help-command' for the actual help command. Prompts
for the object name based on the cursor location for all cases
except the S-Plus GUI. With S-Plus on Windows (both GUI and in an
inferior Emacs buffer) the GUI help window is used. If COMMAND is
suplied, it is used instead of `inferior-ess-help-command'.

\(fn OBJECT &optional COMMAND)" t nil)

(defalias 'ess-help 'ess-display-help-on-object)

(autoload 'ess-helpobjs-at-point "ess-help" "\
Return a list (def obj fun).
Obj is a name at point, fun is the name of the function call
point is in, and def is either obj or fun (in that order) which
has a a help file, i.e. it is a member of slist (string-list).
nil otherwise.

\(fn SLIST)" nil nil)

(autoload 'ess-goto-info "ess-help" "\
Display node NODE from `ess-mode' info.

\(fn NODE)" nil nil)

(autoload 'ess-submit-bug-report "ess-help" "\
Submit a bug report to the ESS maintainers.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ess-inf" "../../../../tmp/ess-18.10.2/lisp/ess-inf.el"
;;;;;;  "49d14ca28697eb923fa3d4c0e9bbaf2c")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-inf.el

(autoload 'ess-proc-name "ess-inf" "\
Return name of process N, as a string, with NAME prepended.
If `ess-plain-first-buffername', then initial process is number-free.

\(fn N NAME)" nil nil)

(autoload 'inferior-ess "ess-inf" "\
Start inferior ESS process.

Without a prefix argument, starts a new ESS process, or switches
to the ESS process associated with the current buffer.  With
ESS-START-ARGS (perhaps specified via \\[universal-argument]),
starts the process with those args.  The current buffer is used
if it is an `inferior-ess-mode' or `ess-transcript-mode' buffer.

If `ess-ask-about-transfile' is non-nil, you will be asked for a
transcript file to use.  If there is no transcript file, the
buffer name will be like *R* or *R2*, determined by
`ess-gen-proc-buffer-name-function'.

Takes the program name from the variable `inferior-ess-program'.
An initialization file (dumped into the process) is specified by
`inferior-ess-start-file', and `inferior-ess-start-args' is used
to accompany the call for `inferior-ess-program'.

When creating a new process, the process buffer replaces the
current window if `inferior-ess-same-window' is non-nil.
Alternatively, it can appear in its own frame if
`inferior-ess-own-frame' is non-nil.

\(Type \\[describe-mode] in the process buffer for a list of
commands.)

CUSTOMIZE-ALIST is the list of dialect-specific variables.  When
non-nil, NO-WAIT tells ESS not to wait for the process to finish.
This may be useful for debugging.

\(fn &optional ESS-START-ARGS CUSTOMIZE-ALIST NO-WAIT)" t nil)

(ess-defgeneric ess-load-file (&optional filename) "Load a source file into an inferior ESS process.\nThis handles Tramp when working on a remote." (interactive (list (or (and (memq major-mode '(ess-mode ess-julia-mode)) (buffer-file-name)) (expand-file-name (read-file-name "Load source file: " nil nil t))))) (ess-load-file--normalise-buffer filename) (save-selected-window (ess-switch-to-ESS t)) (:override (let ((file (ess-load-file--normalise-file filename))) (let ((command (ess-build-load-command file nil t))) (ess-send-string (ess-get-process) command t)))))

(autoload 'inferior-ess-mode "ess-inf" "\
Major mode for interacting with an inferior ESS process.
Runs an S interactive job as a subprocess of Emacs, with I/O through an
Emacs buffer.  Variable `inferior-ess-program' controls which S
is run.

Commands are sent to the ESS process by typing them, and pressing
\\[inferior-ess-send-input].  Pressing \\[complete-dynamic-complete]
completes known object names or filenames, as appropriate.  Other
keybindings for this mode are:

\\{inferior-ess-mode-map}

When editing S objects, the use of \\[ess-load-file] is advocated.
`ess-load-file' keeps source files (if `ess-keep-dump-files' is non-nil) in
the directory specified by `ess-source-directory', with the
filename chosen according to `ess-dump-filename-template'. When a file is
loaded, `ess-mode' parses error messages and jumps to the appropriate file
if errors occur. The ess-eval- commands do not do this.

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-ess-mode-hook' (in that order).

You can send text to the inferior ESS process from other buffers
containing source code. The key bindings of these commands can be
found by typing \\[describe-mode].

    `ess-eval-region' sends the current region to the ESS process.
    `ess-eval-buffer' sends the current buffer to the ESS process.
    `ess-eval-function' sends the current function to the ESS process.
    `ess-eval-line' sends the current line to the ESS process.
    `ess-beginning-of-function' and `ess-end-of-function' move the point to
        the beginning and end of the current S function.
    `ess-switch-to-ESS' switches the current buffer to the ESS process buffer.
    `ess-switch-to-end-of-ESS' switches the current buffer to the ESS process
        buffer and puts point at the end of it.

    `ess-eval-region-and-go', `ess-eval-buffer-and-go',
        `ess-eval-function-and-go', and `ess-eval-line-and-go' switch to the S
        process buffer after sending their text.
    `ess-dump-object-into-edit-buffer' moves an S object into a temporary file
        and buffer for editing
    `ess-load-file' sources a file of commands to the ESS process.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Crosshatches start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ess-jags-d" "../../../../tmp/ess-18.10.2/lisp/ess-jags-d.el"
;;;;;;  "81e11b6f7f1815e0411219636e19c9ec")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-jags-d.el

(autoload 'ess-jags-mode "ess-jags-d" "\
ESS[JAGS]: Major mode for JAGS.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode))

;;;***

;;;### (autoloads nil "ess-julia" "../../../../tmp/ess-18.10.2/lisp/ess-julia.el"
;;;;;;  "a44017a881af5bd126fcffa710dcf45c")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-julia.el

(autoload 'ess-julia-mode "ess-julia" "\
Major mode for editing julia source.  See `ess-mode' for more help.

\(fn)" t nil)

(autoload 'julia "ess-julia" "\
Call 'julia'.
Optional prefix (C-u) allows to set command line arguments, such as
--load=<file>.  This should be OS agnostic.
If you have certain command line arguments that should always be passed
to julia, put them in the variable `inferior-julia-args'.

\(fn &optional START-ARGS)" t nil)

;;;***

;;;### (autoloads nil "ess-noweb-font-lock-mode" "../../../../tmp/ess-18.10.2/lisp/ess-noweb-font-lock-mode.el"
;;;;;;  "f90c0e350963927bc8d3b9d4f8fb01dd")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-noweb-font-lock-mode.el

(autoload 'ess-noweb-font-lock-mode "ess-noweb-font-lock-mode" "\
Minor mode for syntax highlighting when using `ess-noweb-mode' to edit noweb files.
Each chunk is fontified in accordance with its own mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "ess-noweb-mode" "../../../../tmp/ess-18.10.2/lisp/ess-noweb-mode.el"
;;;;;;  "37071a6ac93d1d7386064586950a27d1")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-noweb-mode.el

(autoload 'ess-noweb-mode "ess-noweb-mode" "\
Minor meta mode for editing noweb files.
`Meta' refers to the fact that this minor mode is switching major
modes depending on the location of point.

The following special keystrokes are available in noweb mode:

Movement:
\\[ess-noweb-next-chunk] 	goto the next chunk
\\[ess-noweb-previous-chunk] 	goto the previous chunk
\\[ess-noweb-goto-previous] 	goto the previous chunk of the same name
\\[ess-noweb-goto-next] 	goto the next chunk of the same name
\\[ess-noweb-goto-chunk] 		goto a chunk
\\[ess-noweb-next-code-chunk] 		goto the next code chunk
\\[ess-noweb-previous-code-chunk] 		goto the previous code chunk
\\[ess-noweb-next-doc-chunk] 		goto the next documentation chunk
\\[ess-noweb-previous-doc-chunk] 		goto the previous documentation chunk

Copying/Killing/Marking/Narrowing:
\\[ess-noweb-copy-chunk-as-kill] 		copy the chunk the point is in into the kill ring
\\[ess-noweb-copy-chunk-pair-as-kill] 		copy the pair of doc/code chunks the point is in
\\[ess-noweb-kill-chunk] 		kill the chunk the point is in
\\[ess-noweb-kill-chunk-pair] 		kill the pair of doc/code chunks the point is in
\\[ess-noweb-mark-chunk] 		mark the chunk the point is in
\\[ess-noweb-mark-chunk-pair] 		mark the pair of doc/code chunks the point is in
\\[ess-noweb-narrow-to-chunk] 		narrow to the chunk the point is in
\\[ess-noweb-narrow-to-chunk-pair] 		narrow to the pair of doc/code chunks the point is in
\\[widen] 	widen
\\[ess-noweb-toggle-narrowing] 		toggle auto narrowing

Filling and Indenting:
\\[ess-noweb-fill-chunk] 	fill (or indent) the chunk at point according to mode
\\[ess-noweb-fill-paragraph-chunk] 	fill the paragraph at point, restricted to chunk
\\[ess-noweb-indent-line] 	indent the line at point according to mode

Insertion:
\\[ess-noweb-insert-default-mode-line] 	insert a line to set this file's code mode
\\[ess-noweb-new-chunk] 		insert a new chunk at point
\\[ess-noweb-complete-chunk] 	complete the chunk name before point
\\[ess-noweb-electric-@] 		insert a `@' or start a new doc chunk
\\[ess-noweb-electric-<] 		insert a `<' or start a new code chunk

Modes:
\\[ess-noweb-set-doc-mode] 		set the major mode for editing doc chunks
\\[ess-noweb-set-code-mode] 	set the major mode for editing code chunks
\\[ess-noweb-set-this-code-mode] 	set the major mode for editing this code chunk

Misc:
\\[ess-noweb-occur] 		find all occurrences of the current chunk
\\[ess-noweb-update-chunk-vector] 	update the markers for chunks
\\[ess-noweb-describe-mode] 	describe ess-noweb-mode

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "ess-omg-d" "../../../../tmp/ess-18.10.2/lisp/ess-omg-d.el"
;;;;;;  "10d7f1ebd3f585232baa1fadb3f83878")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-omg-d.el

(autoload 'OMG-mode "ess-omg-d" "\
Major mode for editing Omegahat source.  NOT EVEN STARTED.

\(fn &optional PROC-NAME)" t nil)

(add-to-list 'auto-mode-alist '("\\.omg\\'" . omegahat-mode))

(add-to-list 'auto-mode-alist '("\\.hat\\'" . omegahat-mode))

;;;***

;;;### (autoloads nil "ess-r-mode" "../../../../tmp/ess-18.10.2/lisp/ess-r-mode.el"
;;;;;;  "c76808f4be6bb2bae5eeafde86cdcd12")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-r-mode.el

(defvar ess-dev-map (let (ess-dev-map) (define-prefix-command 'ess-dev-map) (define-key ess-dev-map "" 'ess-r-set-evaluation-env) (define-key ess-dev-map "s" 'ess-r-set-evaluation-env) (define-key ess-dev-map "T" 'ess-toggle-tracebug) (define-key ess-dev-map "\f" 'ess-r-devtools-load-package) (define-key ess-dev-map "l" 'ess-r-devtools-load-package) (define-key ess-dev-map "`" 'ess-show-traceback) (define-key ess-dev-map "~" 'ess-show-call-stack) (define-key ess-dev-map "" 'ess-watch) (define-key ess-dev-map "w" 'ess-watch) (define-key ess-dev-map "" 'ess-debug-flag-for-debugging) (define-key ess-dev-map "d" 'ess-debug-flag-for-debugging) (define-key ess-dev-map "" 'ess-debug-unflag-for-debugging) (define-key ess-dev-map "u" 'ess-debug-unflag-for-debugging) (define-key ess-dev-map [(control 68)] 'ess-debug-unflag-for-debugging) (define-key ess-dev-map "" 'ess-bp-set) (define-key ess-dev-map "b" 'ess-bp-set) (define-key ess-dev-map [(control 66)] 'ess-bp-set-conditional) (define-key ess-dev-map "B" 'ess-bp-set-conditional) (define-key ess-dev-map "\f" 'ess-bp-set-logger) (define-key ess-dev-map "L" 'ess-bp-set-logger) (define-key ess-dev-map "" 'ess-bp-toggle-state) (define-key ess-dev-map "o" 'ess-bp-toggle-state) (define-key ess-dev-map "" 'ess-bp-kill) (define-key ess-dev-map "k" 'ess-bp-kill) (define-key ess-dev-map "" 'ess-bp-kill-all) (define-key ess-dev-map "K" 'ess-bp-kill-all) (define-key ess-dev-map "" 'ess-bp-next) (define-key ess-dev-map "n" 'ess-bp-next) (define-key ess-dev-map "i" 'ess-debug-goto-input-event-marker) (define-key ess-dev-map "I" 'ess-debug-goto-input-event-marker) (define-key ess-dev-map "" 'ess-bp-previous) (define-key ess-dev-map "p" 'ess-bp-previous) (define-key ess-dev-map "" 'ess-debug-toggle-error-action) (define-key ess-dev-map "e" 'ess-debug-toggle-error-action) (define-key ess-dev-map "0" 'ess-electric-selection) (define-key ess-dev-map "1" 'ess-electric-selection) (define-key ess-dev-map "2" 'ess-electric-selection) (define-key ess-dev-map "3" 'ess-electric-selection) (define-key ess-dev-map "4" 'ess-electric-selection) (define-key ess-dev-map "5" 'ess-electric-selection) (define-key ess-dev-map "6" 'ess-electric-selection) (define-key ess-dev-map "7" 'ess-electric-selection) (define-key ess-dev-map "8" 'ess-electric-selection) (define-key ess-dev-map "9" 'ess-electric-selection) (define-key ess-dev-map "?" 'ess-tracebug-show-help) ess-dev-map) "\
Keymap for commands related to development and debugging.")

(autoload 'run-ess-r "ess-r-mode" "\
Call 'R', the 'GNU S' system from the R Foundation.
Optional prefix (\\[universal-argument]) allows to set command line arguments, such as
--vsize.  This should be OS agnostic.
If you have certain command line arguments that should always be passed
to R, put them in the variable `inferior-R-args'.

START-ARGS can be a string representing an argument, a list of
such strings, or any other non-nil value.  In the latter case, you
will be prompted to enter arguments interactively.

\(fn &optional START-ARGS)" t nil)

(defalias 'R #'run-ess-r)

(autoload 'R-mode "ess-r-mode" "\
Major mode for editing R source.  See `ess-mode' for more help.

\(fn &optional PROC-NAME)" t nil)

(defalias 'r-mode #'R-mode)

(defalias 'ess-r-mode #'R-mode)

(add-to-list 'auto-mode-alist '("/R/.*\\.q\\'" . R-mode))

(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))

(add-to-list 'auto-mode-alist '("\\.[rR]profile\\'" . R-mode))

(add-to-list 'auto-mode-alist '("NAMESPACE\\'" . R-mode))

(add-to-list 'auto-mode-alist '("CITATION\\'" . R-mode))

(autoload 'Rnw-mode "ess-r-mode" "\
Major mode for editing Sweave(R) source.
See `ess-noweb-mode' and `R-mode' for more help.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[rR]nw\\'" . Rnw-mode))

(add-to-list 'auto-mode-alist '("\\.[sS]nw\\'" . Snw-mode))

(autoload 'R-transcript-mode "ess-r-mode" "\
Does the right thing.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[Rr]out" . R-transcript-mode))

(add-to-list 'interpreter-mode-alist '("Rscript" . R-mode))

(add-to-list 'interpreter-mode-alist '("r" . R-mode))

(add-to-list 'auto-mode-alist '("/Makevars\\(\\.win\\)?$" . makefile-mode))

;;;***

;;;### (autoloads nil "ess-rd" "../../../../tmp/ess-18.10.2/lisp/ess-rd.el"
;;;;;;  "c9f970c8858c9affeedd16d7c971af9c")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-rd.el

(autoload 'Rd-mode "ess-rd" "\
Major mode for editing R documentation source files.

This mode makes it easier to write R documentation by helping with
indentation, doing some of the typing for you (with Abbrev mode) and by
showing keywords, strings, etc. in different faces (with Font Lock mode
on terminals that support it).

Type \\[list-abbrevs] to display the built-in abbrevs for Rd keywords.

Keybindings
===========

\\{Rd-mode-map}

Variables you can use to customize Rd mode
==========================================

`Rd-indent-level'
  Indentation of Rd code with respect to containing blocks.
  Default is 2.

Turning on Rd mode runs the hook `Rd-mode-hook'.

To automatically turn on the abbrev(iate) features, add the
following lines to your `.emacs' file:

  (add-hook 'Rd-mode-hook
            (lambda ()
              (abbrev-mode 1)))

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.Rd\\'" . Rd-mode))

(autoload 'Rd-preview-help "ess-rd" "\
Preview the current Rd buffer contents as help.
If optional VIA-SHELL is set, using `Rd-to-help-command'.
If the current buffer is not associated with a file, create a
temporary one in `temporary-file-directory'.

\(fn &optional VIA-SHELL)" t nil)

;;;***

;;;### (autoloads nil "ess-rdired" "../../../../tmp/ess-18.10.2/lisp/ess-rdired.el"
;;;;;;  "78b5c91b72cd5fe45f17e27b276f7b61")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-rdired.el

(autoload 'ess-rdired "ess-rdired" "\
Run dired-like mode on R objects.
This is the main function.  See documentation for `ess-rdired-mode' though
for more information!

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ess-s-lang" "../../../../tmp/ess-18.10.2/lisp/ess-s-lang.el"
;;;;;;  "74c6408b67019b7b82e43e18614e3a00")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-s-lang.el

(add-to-list 'auto-mode-alist '("\\.[Ss]t\\'" . S-transcript-mode))

(add-to-list 'auto-mode-alist '("\\.Sout" . S-transcript-mode))

;;;***

;;;### (autoloads nil "ess-sas-d" "../../../../tmp/ess-18.10.2/lisp/ess-sas-d.el"
;;;;;;  "e6a74050a0764489bbc52cdb2bbf9cc2")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-sas-d.el

(autoload 'SAS-mode "ess-sas-d" "\
Major mode for editing SAS source.  See ess-mode for more help.

\(fn &optional PROC-NAME)" t nil)

(add-to-list 'auto-mode-alist '("\\.[Ss][Aa][Ss]\\'" . SAS-mode))

(autoload 'SAS-menu "ess-sas-d" "\
Start SAS from the menu.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ess-sp6-d" "../../../../tmp/ess-18.10.2/lisp/ess-sp6-d.el"
;;;;;;  "b25e421c0de61a99d1ceed1bcacd7ef9")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-sp6-d.el

(autoload 'S+-mode "ess-sp6-d" "\
Major mode for editing S+ source.  See `ess-mode' for more help.

\(fn &optional PROC-NAME)" t nil)

;;;***

;;;### (autoloads nil "ess-sp6w-d" "../../../../tmp/ess-18.10.2/lisp/ess-sp6w-d.el"
;;;;;;  "3b3183311aedf9a4b618e24e0f183f13")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-sp6w-d.el

(add-to-list 'auto-mode-alist '("\\.sp\\'" . S-mode))

(add-to-list 'auto-mode-alist '("\\.[qsS]\\'" . S-mode))

(add-to-list 'auto-mode-alist '("\\.ssc\\'" . S-mode))

(add-to-list 'auto-mode-alist '("\\.SSC\\'" . S-mode))

;;;***

;;;### (autoloads nil "ess-stata-mode" "../../../../tmp/ess-18.10.2/lisp/ess-stata-mode.el"
;;;;;;  "967f862ea0be819d3d23449f105359ac")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-stata-mode.el

(autoload 'STA-mode "ess-stata-mode" "\
Major mode for editing Stata source.  See `ess-mode' for more help.

\(fn &optional PROC-NAME)" t nil)

(add-to-list 'auto-mode-alist '("\\.do\\'" . STA-mode))

(add-to-list 'auto-mode-alist '("\\.ado\\'" . STA-mode))

;;;***

;;;### (autoloads nil "ess-trns" "../../../../tmp/ess-18.10.2/lisp/ess-trns.el"
;;;;;;  "41955dddb0f83c8396ff6286af6188ea")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-trns.el

(autoload 'ess-transcript-mode "ess-trns" "\
Major mode for manipulating {ESS} transcript files.

Type \\[ess-transcript-send-command] to send a command in the
transcript to the current S process. \\[ess-transcript-copy-command]
copies the command but does not execute it, allowing you to edit it in
the process buffer first.

Type \\[ess-transcript-clean-region] to delete all outputs and prompts
in the region, leaving only the S commands.  Other keybindings are:

\\{ess-transcript-mode-map}

\(fn ALIST &optional PROC)" nil nil)

(autoload 'ess-transcript-clean-region "ess-trns" "\
Strip the transcript in the region, leaving only (R/S/Lsp/..) commands.
Deletes any lines not beginning with a prompt, and then removes the
prompt from those lines that remain.  Prefix argument means to
clean even if the buffer is \\[read-only].

\(fn BEG END EVEN-IF-READ-ONLY)" t nil)

;;;***

;;;### (autoloads nil "ess-xls-d" "../../../../tmp/ess-18.10.2/lisp/ess-xls-d.el"
;;;;;;  "5cf0cc78a57f070e7fca1f7d62b18ca9")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/ess-xls-d.el

(autoload 'XLS-mode "ess-xls-d" "\
Major mode for editing XLispStat source.  NOT EVEN STARTED.

\(fn &optional PROC-NAME)" t nil)

(add-to-list 'auto-mode-alist '("\\.lsp\\'" . XLS-mode))

;;;***

;;;### (autoloads nil "essd-els" "../../../../tmp/ess-18.10.2/lisp/essd-els.el"
;;;;;;  "6fd7d752aa2ffb5c712aae57bd753d05")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/essd-els.el

(autoload 'ess-remote "essd-els" "\
Execute this command from within a buffer running a process.  It
runs `ess-add-ess-process' to add the process to
`ess-process-name-alist' and to make it the
`ess-current-process-name'.  It then prompts the user for an ESS
language and sets the editing characteristics appropriately.

To use this command, first start a process on a remote computer by
manual use of telnet, rlogin, ssh, or some other protocol.  Start the
relevant program (\"S\" or \"R\" or \"sas -stdio\") in that buffer.  Once
you are talking to S or R or SAS, then execute `ess-remote' to make
the current buffer an inferior-ess buffer with the right behavior for
the language you are currently working with.  With S and R, use C-c
C-n to send lines over.  With SAS, use C-c i
`ess-eval-line-and-step-invisibly' to send lines over invisibly.

DIALECT is the desired ess-dialect. If nil, ask for dialect

\(fn &optional PROC-NAME DIALECT)" t nil)

;;;***

;;;### (autoloads nil "julia-mode" "../../../../tmp/ess-18.10.2/lisp/julia-mode.el"
;;;;;;  "d2343ca79862d65c9f0b4cda1c98475b")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/julia-mode.el

(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))

(autoload 'julia-mode "julia-mode" "\
Major mode for editing julia code.

\(fn)" t nil)

(autoload 'inferior-julia "julia-mode" "\
Run an inferior instance of `julia' inside Emacs.

\(fn)" t nil)

(defalias 'run-julia #'inferior-julia "\
Run an inferior instance of `julia' inside Emacs.")

;;;***

;;;### (autoloads nil "mouseme" "../../../../tmp/ess-18.10.2/lisp/mouseme.el"
;;;;;;  "1dd41c8191881b3b1a92ed806ad3dcba")
;;; Generated autoloads from ../../../../tmp/ess-18.10.2/lisp/mouseme.el

(autoload 'mouse-me "mouseme" "\
Popup a menu of functions to run on selected string or region.

\(fn EVENT)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../tmp/ess-18.10.2/lisp/ess-arc-d.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-bugs-d.el" "../../../../tmp/ess-18.10.2/lisp/ess-bugs-l.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-custom.el" "../../../../tmp/ess-18.10.2/lisp/ess-dde.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-font-lock.el" "../../../../tmp/ess-18.10.2/lisp/ess-generics.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-gretl.el" "../../../../tmp/ess-18.10.2/lisp/ess-help.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-inf.el" "../../../../tmp/ess-18.10.2/lisp/ess-jags-d.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-julia.el" "../../../../tmp/ess-18.10.2/lisp/ess-lsp-l.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-mode.el" "../../../../tmp/ess-18.10.2/lisp/ess-mouse.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-noweb-font-lock-mode.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-noweb-mode.el" "../../../../tmp/ess-18.10.2/lisp/ess-noweb.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-omg-d.el" "../../../../tmp/ess-18.10.2/lisp/ess-omg-l.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-r-a.el" "../../../../tmp/ess-18.10.2/lisp/ess-r-completion.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-r-flymake.el" "../../../../tmp/ess-18.10.2/lisp/ess-r-gui.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-r-mode.el" "../../../../tmp/ess-18.10.2/lisp/ess-r-package.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-r-syntax.el" "../../../../tmp/ess-18.10.2/lisp/ess-r-xref.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-rd.el" "../../../../tmp/ess-18.10.2/lisp/ess-rdired.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-roxy.el" "../../../../tmp/ess-18.10.2/lisp/ess-rutils.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-s-lang.el" "../../../../tmp/ess-18.10.2/lisp/ess-s3-d.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-s4-d.el" "../../../../tmp/ess-18.10.2/lisp/ess-sas-a.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-sas-d.el" "../../../../tmp/ess-18.10.2/lisp/ess-sas-l.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-site.el" "../../../../tmp/ess-18.10.2/lisp/ess-sp3-d.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-sp4-d.el" "../../../../tmp/ess-18.10.2/lisp/ess-sp5-d.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-sp6-d.el" "../../../../tmp/ess-18.10.2/lisp/ess-sp6w-d.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-stata-lang.el" "../../../../tmp/ess-18.10.2/lisp/ess-stata-mode.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-swv.el" "../../../../tmp/ess-18.10.2/lisp/ess-toolbar.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-tracebug.el" "../../../../tmp/ess-18.10.2/lisp/ess-trns.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-utils.el" "../../../../tmp/ess-18.10.2/lisp/ess-vst-d.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/ess-xls-d.el" "../../../../tmp/ess-18.10.2/lisp/ess.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/essd-els.el" "../../../../tmp/ess-18.10.2/lisp/julia-mode.el"
;;;;;;  "../../../../tmp/ess-18.10.2/lisp/make-regexp.el" "../../../../tmp/ess-18.10.2/lisp/mouseme.el")
;;;;;;  (23865 122 421300 45000))

;;;***

(provide 'ess-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ess-autoloads.el ends here
