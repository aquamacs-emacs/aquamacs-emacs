;;; dart-mode.el --- Major mode for editing Dart files

;; Author: Nathan Weizenbaum
;; URL: http://code.google.com/p/dart-mode
;; Version: 0.9
;; Keywords: language

;; Copyright (C) 2011 Google Inc.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To install, save this on your load path and add the following to
;; your .emacs file:
;;
;; (require 'dart-mode)
;;
;; Known bugs:
;;
;; * Multiline strings using """ and ''' are not recognized. They fontify
;;   correctly, but only because they look like three strings in a row.
;; * In a map with identifier keys, the first key is fontified like a label.
;; * Return values for operator methods aren't fontified correctly.
;; * Untyped parameters aren't fontified correctly.
;; * Quotes immediately after string interpolation are marked as unclosed.
;; * Sexp movement doesn't properly ignore quotes in interpolation.
;; * Methods using "=>" can cause indentation woes.
;; * C and C++ modes seem to be hosed.

;;; Code:

(require 'cc-mode)
(require 'cc-langs)
(eval-when-compile (require 'cl))

(eval-and-compile (c-add-language 'dart-mode 'java-mode))


;;; CC configuration

(c-lang-defconst c-symbol-start
  dart (concat "[" c-alpha "_]"))

(c-lang-defconst c-identifier-ops
  dart nil)

(c-lang-defconst c-after-id-concat-ops
  dart nil)

(c-lang-defconst c-multiline-string-start-char
  dart ?@)

(c-lang-defconst c-opt-cpp-prefix
  dart "\\s *#\\s *")

(c-lang-defconst c-cpp-message-directives
  dart nil)

(c-lang-defconst c-cpp-include-directives
  dart nil)

(c-lang-defconst c-opt-cpp-macro-define
  dart nil)

(c-lang-defconst c-cpp-expr-directives
  dart '("import" "source" "library" "resource"))

(c-lang-defconst c-cpp-expr-functions
  dart nil)

(c-lang-defconst c-operators
  dart `((prefix "#")
         (postfix-if-paren "<" ">")
         (prefix "super")
         (left-assoc ".")
         (postfix "++" "--" "[" "]" "(" ")")
         (unary "++" "--" "+" "-" "!" "~" "negate" "new" "const")
         (left-assoc "*" "/" "%")
         (left-assoc "+" "-")
         (left-assoc "<<" ">>" ">>>")
         (left-assoc "<" ">" "<=" ">=")
         (left-assoc "==" "!=" "===" "!==" "is" "is!")
         (left-assoc "&")
         (left-assoc "^")
         (left-assoc "|")
         (left-assoc "&&")
         (left-assoc "||")
         (right-assoc-sequence "?" ":")
         (left-assoc "=>")
         (right-assoc ,@(c-lang-const c-assignment-operators))
         (left-assoc ",")))

(c-lang-defconst c-overloadable-operators
  dart '("==" "<" ">" "<=" ">=" "-" "+" "*" "/" "%" "|" "^" "&"
         "<<" ">>" ">>>" "[]=" "[]" "~" "negate"))

(c-lang-defconst c-opt-op-identifier-prefix
  dart (c-make-keywords-re t '("operator")))

(c-lang-defconst c-doc-comment-start-regexp
  dart nil)

(c-lang-defconst c-paragraph-start
  dart "$")

(c-lang-defconst c-primitive-type-kwds
  dart '("Dynamic" "void" "num" "int" "double" "bool"))

(c-lang-defconst c-class-decl-kwds
  dart '("class" "interface"))

;; Don't put these in c-modifier-kwds because they can be used without a type
;; following them.
(c-lang-defconst c-typeless-decl-kwds
  dart '("abstract" "const" "factory" "final" "operator" "static" "typedef" "var"))

(c-lang-defconst c-modifier-kwds
  dart nil)

(c-lang-defconst c-other-decl-kwds
  dart nil)

(c-lang-defconst c-decl-hangon-kwds
  dart '("get" "set" "native"))

(c-lang-defconst c-postfix-decl-spec-kwds
  dart '("extends" "implements" "factory"))

(c-lang-defconst c-type-list-kwds
  dart '("new" "const" "is" "is!" "extends" "implements" "factory"))

(c-lang-defconst c-ref-list-kwds
  dart nil)

(c-lang-defconst c-block-stmt-2-kwds
  dart '("for" "if" "switch" "while" "catch"))

(c-lang-defconst c-simple-stmt-kwds
  dart '("break" "continue" "return" "throw"))

(c-lang-defconst c-before-label-kwds
  dart '("break" "continue"))

(c-lang-defconst c-nonlabel-token-key
  dart (concat (concat "\\s\(\\|" (c-lang-const c-nonlabel-token-key))))

(c-lang-defconst c-inexpr-class-kwds
  dart nil)

(c-lang-defconst c-inexpr-brace-list-kwds
  dart nil)

(c-lang-defconst c-other-kwds
  dart '("in"))

(c-lang-defconst c-decl-prefix-re
  dart "\\([\{\}\([;,<]+\\)")

(c-lang-defconst c-cast-parens
  dart nil)

(c-lang-defconst c-block-prefix-disallowed-chars
  dart (set-difference (c-lang-const c-block-prefix-disallowed-chars)
                       '(?\" ?')))

(c-lang-defconst c-type-decl-prefix-key
  dart "\\(\(\\)\\([^=]\\|$\\)")

(c-lang-defconst c-after-suffixed-type-decl-key
  dart (concat (c-lang-const c-after-suffixed-type-decl-key) "\\|:"))

(c-lang-defconst c-opt-type-suffix-key
  dart nil)

(c-lang-defconst c-recognize-typeless-decls
  dart t)

(c-lang-defconst c-recognize-<>-arglists
  dart t)

(c-lang-defconst c-opt-postfix-decl-spec-kwds
  dart '("native"))

(c-lang-defconst c-opt-postfix-decl-spec-kwds
  dart '("native"))

(push '(dart-brace-list-cont-nonempty . 0)
      (get 'c-offsets-alist 'c-stylevar-fallback))

(defconst dart-c-style
  '("java"
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)
    (fill-column . 80)
    (c-offsets-alist . ((arglist-intro . ++)
                        (arglist-cont-nonempty . ++)
                        (statement-block-intro . dart-block-offset)
                        (block-close . dart-block-offset)
                        (dart-brace-list-cont-nonempty .
                         dart-brace-list-cont-nonempty-offset)
                        (case-label . +))))
  "The default Dart styles.")

(c-add-style "dart" dart-c-style)

(defvar dart-mode-map (c-make-inherited-keymap)
  "Keymap used in dart-mode buffers.")


;;; CC indentation support

(defun dart-block-offset (info)
  "Calculate the correct indentation for inline functions.

When indenting inline functions, we want to pretend that
functions taking them as parameters essentially don't exist."
  (destructuring-bind (syntax . anchor) info
    (let ((arglist-count
           (loop for (symbol . _) in c-syntactic-context
                 count (eq symbol 'arglist-cont-nonempty))))
      (if (> arglist-count 0)
          (- (* -1 c-basic-offset arglist-count)
             (if (eq syntax 'block-close) c-basic-offset 0))
        (if (eq syntax 'block-close) 0 '+)))))

(defun dart-brace-list-cont-nonempty-offset (info)
  "Indent a brace-list line in the same style as arglist-cont-nonempty.
This could be either an actual brace-list or an optional parameter."
  (destructuring-bind (syntax . anchor) info
    ;; If we're in a function definition with optional arguments, indent as if
    ;; the brace wasn't there. Currently this misses the in-function function
    ;; definition, but that's probably acceptable.
    (if (and
         (save-excursion (backward-up-list) (eq (char-after) ?\[))
         (assq 'topmost-intro
               (save-excursion (goto-char anchor) (c-guess-basic-syntax))))
        '++
      ;; Otherwise, we're in an actual brace list, in which case only indent
      ;; once.
      '+)))

(defun dart-in-block-p (syntax-guess)
  "Return whether or not the immediately enclosing {} block is a code block.
The other option, of course, is a map literal.

SYNTAX-GUESS is the output of `c-guess-basic-syntax'."
  (save-excursion
    (c-safe
      ;; If we're in a continued statement within a class, we want to know we're
      ;; in a class so we can return true.
      (when (eq 'statement-cont (caar syntax-guess))
        (save-excursion
          (c-beginning-of-statement-1 nil t t)
          (setq syntax-guess (c-guess-basic-syntax))))

      (backward-up-list)
      (when (= (char-after) ?\{)
        (c-backward-comments)
        (or
         ;; Both anonymous and named functions have a ")" immediately before the
         ;; code block.
         (= (char-before) ?\))
         ;; "else" and "try" are the only keywords that come immediately before
         ;; a block.
         (looking-back "\\<\\(else\\|try\\)\\>")
         ;; CC is good at figuring out if we're in a class.
         (assq 'inclass syntax-guess))))))

(defadvice c-guess-basic-syntax (after dart-guess-basic-syntax activate)
  (when (c-major-mode-is 'dart-mode)
    (let* ((syntax (car (last ad-return-value)))
           (type (car syntax)))
      (save-excursion
        (back-to-indentation)

        (or
         ;; Handle indentation in a constructor with an initializer on a
         ;; separate line.
         (when (memq type '(defun-block-intro inline-close))
           (save-excursion
             (c-safe
               (goto-char (cadr syntax))
               (when (= (char-after) ?:)
                 (c-beginning-of-statement-1)
                 (setq ad-return-value `((,type ,(point))))
                 t))))

         ;; Handle array literal indentation
         (when (memq type
                     '(arglist-intro
                       arglist-cont
                       arglist-cont-nonempty
                       arglist-close))
           (save-excursion
             (c-safe
               (backward-up-list)
               (when (= (char-after) ?\[)
                 (setq ad-return-value
                       `((,(case type
                             (arglist-intro 'brace-list-intro)
                             (arglist-cont 'brace-list-entry)
                             (arglist-cont-nonempty 'dart-brace-list-cont-nonempty)
                             (arglist-close 'brace-list-close))
                          ,(cadr syntax)))))
               t)))

         ;; Handle map literal indentation
         (when (and (memq type '(label statement-block-intro statement-cont statement
                                 block-close defun-block-intro defun-close))
                    (not (dart-in-block-p ad-return-value)))
           (save-excursion
             (c-safe
               (if (= (char-after) ?\})
                   (progn
                     (backward-up-list)
                     (when (= (char-after) ?\{)
                       (back-to-indentation)
                       (setq ad-return-value `((brace-list-close ,(point))))))
                 (c-backward-comments)
                 ;; Completely reset ad-return-value here because otherwise it
                 ;; gets super-screwy.
                 (if (= (char-before) ?\{)
                     (progn
                       (back-to-indentation)
                       (setq ad-return-value `((brace-list-intro ,(point))))
                       t)
                   (backward-up-list)
                   (when (= (char-after) ?\{)
                     (forward-char)
                     (let ((contp (not (looking-at "\\s-*$"))))
                       (c-forward-comments)
                       (back-to-indentation)
                       (setq ad-return-value
                             `((,(if contp 'dart-brace-list-cont-nonempty
                                   'brace-list-entry)
                                ,(point))))
                       t))))))))))))

(defadvice c-inside-bracelist-p (after dart-inside-bracelist-p activate)
  ;; This function is only called within c-guess-basic-syntax. Since we do all
  ;; out brace-list detection in our advice, we just never report being in a
  ;; bracelist there.
  (when (c-major-mode-is 'dart-mode)
    (setq ad-return-value nil)))

(defadvice c-search-decl-header-end (around dart-search-decl-header-end activate)
  (if (not (c-major-mode-is 'dart-mode)) ad-do-it
    (let ((base (point)))
      (while (and
              (c-syntactic-re-search-forward "[;{=:]" nil 'move t t)
              (c-end-of-current-token base))
        (setq base (point)))
      ;; If we hit :, we're in a member initialization list and we want to
      ;; ignore = signs.
      (when (= (char-before) ?:)
        (while (and
                (c-syntactic-re-search-forward "[;{]" nil 'move t t)
                (c-end-of-current-token base))
        (setq base (point)))))))

(defadvice c-parse-state (around dart-c-parse-state activate)
  (if (not (c-major-mode-is 'dart-mode)) ad-do-it
    ;; c-parse-state is a wrapper around c-parse-state-1 that does some tricks
    ;; to ensure that dangling brackets in preprocessor commands don't screw up
    ;; parse information for the real world. In Dart, all "preprocessor"
    ;; directives have matched braces, so we don't need to worry about that. The
    ;; wrapper was also screwing up indentation in weird ways, so we just ignore
    ;; it.
    (setq ad-return-value (c-parse-state-1))))


;;; Additional fontification support

(defun dart-fontify-region (beg end)
  "Use fontify the region between BEG and END as Dart.

This will overwrite fontification due to strings and comments."
  (save-excursion
    (save-match-data
      (save-restriction
        (let ((font-lock-dont-widen t))
          (narrow-to-region (- beg 1) end)
          ;; font-lock-fontify-region apparently isn't inclusive,
          ;; so we have to move the beginning back one char
          (font-lock-fontify-region (point-min) (point-max)))))))

(defun dart-limited-forward-sexp (limit &optional arg)
  "Move forward using `forward-sexp' or to limit,
whichever comes first."
  (let (forward-sexp-function)
    (condition-case err
        (save-restriction
          (narrow-to-region (point) limit)
          (forward-sexp arg))
      (scan-error
       (unless (equal (nth 1 err) "Unbalanced parentheses")
         (signal 'scan-error (cdr err)))
       (goto-char limit)))))

(defun dart-highlight-interpolation (limit)
  "Highlight interpolation (${foo})."
  (let ((start (point)))
    (when (re-search-forward "\\(\\${\\)" limit t)
      (if (elt (parse-partial-sexp start (point)) 3) ; in a string
          (save-match-data
            (forward-char -1)
            (let ((beg (point)))
              (dart-limited-forward-sexp limit)
              (dart-fontify-region (+ 1 beg) (point)))

            ;; Highlight the end of the interpolation.
            (when (eq (char-before) ?})
              (put-text-property (- (point) 1) (point) 'face font-lock-variable-name-face))
            t)
        (looking-at "\\<\\>")))))


;;; Boilerplate font-lock piping

(defcustom dart-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in DART mode.
Each list item should be a regexp matching a single identifier.")

(defconst dart-font-lock-keywords-1 (c-lang-const c-matchers-1 dart)
  "Minimal highlighting for Dart mode.")

(defconst dart-font-lock-keywords-2 (c-lang-const c-matchers-2 dart)
  "Fast normal highlighting for Dart mode.")

(defconst dart-font-lock-keywords-3
  (cons
   '(dart-highlight-interpolation 1 font-lock-variable-name-face prepend)
   (c-lang-const c-matchers-3 dart))
  "Accurate normal highlighting for Dart mode.")

(defvar dart-font-lock-keywords dart-font-lock-keywords-3
  "Default expressions to highlight in Dart mode.")

(defvar dart-mode-syntax-table nil
  "Syntax table used in dart-mode buffers.")
(unless dart-mode-syntax-table
  (setq dart-mode-syntax-table
        (funcall (c-lang-const c-make-mode-syntax-table dart))))


;;; Flymake Support

(defun flymake-dart-init ()
  "Return the dart_analyzer command to invoke for flymake."
  (let* ((temp-file  (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
	 (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name)))
         ;; Work around Dart issue 7497
         (work-dir (expand-file-name
                    "flymake-dart-work"
                    (flymake-get-temp-dir))))
    (list "dart_analyzer" (list "--error_format" "machine" local-file
                                "--work" work-dir))))

(defun flymake-dart-cleanup ()
  "Clean up after running the Dart analyzer."
  (flymake-simple-cleanup)
  (let ((dir-name (expand-file-name
                   "flymake-dart-work"
                   (flymake-get-temp-dir))))
    (condition-case nil
        (delete-dir dir-name t)
      (error
       (flymake-log 1 "Failed to delete dir %s, error ignored" dir-name)))))

(eval-after-load 'flymake
  '(progn
     (when (boundp 'flymake-warn-line-regexp)
       (add-hook 'dart-mode-hook
                 (lambda ()
                   (setq (make-variable-buffer-local 'flymake-warn-line-regexp)
                         "^WARNING|"))))

     (defadvice flymake-post-syntax-check (before flymake-post-syntax-check-dart activate)
       "Sets the exit code of the dart_analyzer process to 0.

dart_analyzer can report errors for files other than the current
file. flymake dies horribly if the process emits a non-zero exit
code without any warnings for the current file. These two
properties interact poorly."
       (when (eq major-mode 'dart-mode)
         (ad-set-arg 0 0)))

     (push '("\\.dart\\'" flymake-dart-init flymake-dart-cleanup)
           flymake-allowed-file-name-masks)
     ;; Accept negative numbers to work around Dart issue 7495
     (push '("^[^|]+|[^|]+|[^|]+|file:\\([^|]+\\)|\\([0-9]+\\)|\\([0-9]+\\)|[0-9]+|\\(.*\\)$"
             1 2 3 4)
           flymake-err-line-patterns)))


;;; Initialization

;;;###autoload (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

;;;###autoload
(defun dart-mode ()
  "Major mode for editing Dart files.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `dart-mode-hook'.

Key bindings:
\\{dart-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table dart-mode-syntax-table)
  (setq major-mode 'dart-mode
        mode-name "Dart")
  (use-local-map dart-mode-map)
  (c-init-language-vars dart-mode)
  (c-common-init 'dart-mode)
  (c-set-style "dart")
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'dart-mode-hook)
  (c-update-modeline))

(provide 'dart-mode)

;;; dart-mode.el ends here
