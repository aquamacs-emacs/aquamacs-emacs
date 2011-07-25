;;; py-bug-numbered-tests.el --- run single tests according to bug number

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages
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
;;
;;; Code:

(add-to-list 'load-path default-directory)
(require 'python-mode)
(require 'python-mode-test)
(defvar bug-numbered-tests nil
  "Tests following reports at https://bugs.launchpad.net/python-mode")

(setq bug-numbered-tests
      (if (featurep 'xemacs)
          (list
           'bullet-lists-in-comments-lp:328782-test
           'fill-paragraph-problems-lp:710373-test
           'nested-indents-lp:328775-test
           'previous-statement-lp:637955-test)
        (list
         'mark-block-region-lp:328806-test
         'mark-decorators-lp:328851-test
         'nested-dictionaries-indent-lp:328791-test
         'triple-quoted-string-dq-lp:302834-test
         'fore-00007F-breaks-indentation-lp:328788-test
         'dq-in-tqs-string-lp:328813-test
         'flexible-indentation-lp:328842-test
         'py-current-defun-lp:328846-test
         'cls-pseudo-keyword-lp:328849-test
         'hungry-delete-backwards-lp:328853-test
         'hungry-delete-forward-lp:328853-test
         'beg-end-of-defun-lp:303622-test
         'bullet-lists-in-comments-lp:328782-test
         'imenu-newline-arglist-lp:328783-test
         'imenu-matches-in-docstring-lp:436285-test
         'exceptions-not-highlighted-lp:473525-test
         'UnicodeEncodeError-lp:550661-test
         'fill-paragraph-problems-lp:710373-test
         'nested-indents-lp:328775-test
         'previous-statement-lp:637955-test
         'inbound-indentation-multiline-assignement-lp:629916-test
         'indentation-of-continuation-lines-lp:691185-test
         ;; test passes only when run from edebug
         ;; assistance appreciated
         ;; 'syntaxerror-on-py-execute-region-lp:691542-test
         'goto-beginning-of-tqs-lp:735328-test
         'class-treated-as-keyword-lp:709478-test
         'py-decorators-face-lp:744335-test
         'indent-after-return-lp:745208-test
         'keep-assignements-column-lp:748198-test
         'indent-triplequoted-to-itself-lp:752252-test
         'multiline-listings-indent-lp:761946-test
         'new-page-char-causes-loop-lp:762498-test
         'nested-dicts-indent-lp:763756-test
         'bad-indent-after-except-lp:771289-test
         'indent-open-paren-not-last-lp:771291-test
         'wrong-indent-after-else-lp:772610-test
         'except-indents-wrong-lp:784432-test
         'indent-explicitly-set-in-multiline-tqs-lp:784225-test
         'unbalanced-parentheses-lp:784645-test
         'explicitly-indent-in-list-lp:785018-test
         'explicit-backslashed-continuation-line-indent-lp:785091-test
         'indentation-error-lp:795773-test
         'indent-function-arglist-lp:800088-test
         'python-mode-hangs-lp:801780-test
         'stops-backslashed-line-lp:802504-test
         'stops-backslashed-line-lp:802504-test2
         'python-mode-slow-lp:803275-test
         'py-master-file-not-honored-lp:794850-test

         )))

(defun py-run-bug-numbered-tests (&optional arg)
  "With ARG greater 1 keep test buffers open. "
  (interactive "p")
  (dolist (ele bug-numbered-tests)
    (funcall ele arg)))

(defun py-bug-tests-intern (testname &optional arg teststring)
  (if arg
      (progn
        (set-buffer (get-buffer-create (replace-regexp-in-string "-base$" "-test" (prin1-to-string testname))))
        (switch-to-buffer (current-buffer))
        (erase-buffer)
        (insert teststring)
        (fundamental-mode)
        (python-mode)
        (funcall testname)
        (message "%s" (concat (replace-regexp-in-string "-base$" "-test" (prin1-to-string testname)) " passed"))
        (unless (< 1 arg)
          (set-buffer-modified-p 'nil)
          (cond ((processp (get-process "Python3")) (kill-process "Python3"))
                ((processp (get-process "Python2")) (kill-process "Python2"))
                ((processp (get-process "Python")) (kill-process "Python")))
          (kill-buffer (current-buffer))))
    (with-temp-buffer
      (let ((font-lock-verbose nil))
        (insert teststring)
        (funcall testname)))))

(defun sexp-commands-lp:328778-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked.

Reported by Montanaro on 2003-08-05
\[ ... ]
 You can kill balanced expressions on a
 particular line but it's not possible to remove the
 whole of an 'if' or 'while' block."
  (interactive "p")
  (let ((teststring "# Examples from http://diveintopython.org/

def main(argv):
    grammar = \"kant.xml\"
    try:
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    for opt, arg in opts:
        if opt in (\"-h\", \"--help\"):
            usage()
            sys.exit()
        elif opt == '-d':
            global _debug
            _debug = 1
        elif opt in (\"-g\", \"--grammar\"):
            grammar = arg
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'sexp-commands-lp:328778 arg teststring)))

(defun sexp-commands-lp:328778 ()
  (let ((size (buffer-size)))
    (goto-char (point-min))
    (forward-line 15)
    (py-kill-clause)
    (assert (< (buffer-size) size) nil "sexp-commands-lp:328778 test failed")
    (assert (eq (buffer-size) 526) nil "sexp-commands-lp:328778 test failed")
    (kill-line 1)
    (indent-according-to-mode)
    (forward-line -4)
    (py-kill-block)
    (assert (eq (buffer-size) 324) nil "sexp-commands-lp:328778 test failed")
    ))

(defun nested-dictionaries-indent-lp:328791-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.

If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked. "
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
    d = {'a':{'b':3,
              'c':4}}
"))
    (py-bug-tests-intern 'nested-dictionaries-indent-lp:328791 arg teststring)))

(defun nested-dictionaries-indent-lp:328791 ()
  (let ((py-indent-honors-multiline-listing t))
    (goto-char (point-min))
    (forward-line 2)
    (assert (eq 14 (py-compute-indentation)))))

(defun mark-block-region-lp:328806-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "def f():
    \"\"\"
    class blah blah
    \"\"\"
    if a:
        ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
                'python-partial-expression',
                'python-statement',
                ])
"))
    (py-bug-tests-intern 'mark-block-region-lp:328806-base arg teststring)))

(defun mark-block-region-lp:328806-base ()
  (forward-line -2)
  (py-mark-block)
  (assert (< (region-beginning) (region-end)) nil "mark-block-region-lp:328806 test failed!"))

(defun flexible-indentation-lp:328842-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "
\(long, sequence, of_items,
    that, needs, to_be, wrapped) = input_list

packed_entry = (long, sequence, of_items,
that, needs, to_be, wrapped)

\( whitespaced, long, sequence, of_items,
    that, needs, to_be, wrapped) = input_list
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'flexible-indentation-lp:328842 arg teststring)))

(defun flexible-indentation-lp:328842 ()
  (let ((py-indent-honors-multiline-listing t))
    (goto-char (point-min))
    (forward-line 2)
    (indent-according-to-mode)
    (assert (eq 1 (current-indentation)) nil "flexible-indentation-lp:328842 test failed")
    (forward-line 3)
    (indent-according-to-mode)
    (assert (eq 16 (current-indentation)) nil "flexible-indentation-lp:328842 test failed")
    (forward-line 3)
    (indent-according-to-mode)
    (assert (eq 2 (current-indentation)) nil "flexible-indentation-lp:328842 test failed")))

(defun py-current-defun-lp:328846-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-current-defun-lp:328846-base arg teststring)))

(defun py-current-defun-lp:328846-base ()
  (goto-char 331)
  (assert (string= "f" (py-current-defun)) nil "py-current-defun-lp:328846 test failed"))

(defun cls-pseudo-keyword-lp:328849-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class Foo(object):
    def summat(cls, x):
          .....
    summat = classmethod(summat)
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'cls-pseudo-keyword-lp:328849-base arg teststring)))

(defun cls-pseudo-keyword-lp:328849-base ()
  (let ((font-lock-verbose nil))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (goto-char 36)
    (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'py-pseudo-keyword-face) nil "cls-pseudo-keyword-lp:328849 test failed ")))

(defun mark-decorators-lp:328851-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "@foo.bar
def baz():
    pass
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'mark-decorators-lp:328851-base arg teststring)))

(defun mark-decorators-lp:328851-base ()
  (goto-char 10)
  (py-mark-def t)
  (assert (eq 28 (- (region-end)(region-beginning))) nil "mark-decorators-lp:328851 test failed"))

(defun beg-end-of-defun-lp:303622-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
class f():
    \"\"\"
    class blah blah
    \"\"\"
    if a:
        ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
                'python-partial-expression',
                'python-statement',
                ])
"))
    (py-bug-tests-intern 'beg-end-of-defun-lp:303622 arg teststring)))

(defun beg-end-of-defun-lp:303622 ()
  (goto-char (point-min))
  (forward-line 2)
  (end-of-defun)
  (assert (eq 292 (point)) nil "beg-end-of-defun-lp:303622 test failed!")
  (beginning-of-defun)
  (assert (eq 2 (point)) nil "beg-end-of-defun-lp:303622 test failed!"))

(defun dq-in-tqs-string-lp:328813-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
# Bug #328813 (sf1775975)
print \"\"\" \"Hi!\" I'm a doc string\"\"\"
print ''' 'Hi!' I'm a doc string'''
print \"\"\" ''' \"Hi!\" I'm a doc string ''' \"\"\"
print ''' \"\"\" \"Hi!\" I'm a doc string \"\"\" '''
"))
    (py-bug-tests-intern 'dq-in-tqs-string-lp:328813 arg teststring)))

(defun dq-in-tqs-string-lp:328813 ()
  (let ((font-lock-verbose nil))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (goto-char 78)
    (let ((erg (get-char-property (point) 'face)))
      (message "%s" erg)
      (insert "\"")
      (font-lock-fontify-buffer)
      (message "%s" erg)
      (message "%s" (get-char-property (point) 'face))
      (assert (eq erg (get-char-property (point) 'face)) nil "dq-in-tqs-string-lp:328813 test failed ")
      (goto-char 122))))

(defun imenu-matches-in-docstring-lp:436285-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "
class foo():
    \"\"\"
    class hello(object):
        def __init__(self):
        ...
    \"\"\"
    pass
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'imenu-matches-in-docstring-lp:436285-base arg teststring)))

(defun imenu-matches-in-docstring-lp:436285-base ()
  (goto-char 40)
  (assert (eq (py-beginning-of-def-or-class) 2) nil "imenu-matches-in-docstring-lp:436285 test failed"))

(defun fill-paragraph-problems-lp:710373-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
    \"\"\"
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    \"\"\"
"))
    (fill-paragraph-problems-lp:710373-test-intern arg teststring)))

(defun fill-paragraph-problems-lp:710373-test-intern (arg teststring)
  (let ((tmp-dir "/tmp/")
        (fpp-exec-buffer "fill-paragraph-problems-lp:710373")
        (diff-buffer "fpp-lp:710373-old"))
    (set-buffer (get-buffer-create diff-buffer))
    (erase-buffer)
    (fundamental-mode)
    (insert teststring)
    (write-file (concat tmp-dir diff-buffer))
    (if arg
        (progn
          (set-buffer (get-buffer-create fpp-exec-buffer))
          (switch-to-buffer (current-buffer))
          (erase-buffer)
          (insert teststring)
          (fundamental-mode)
          (fill-paragraph-problems-lp:710373-test-base arg tmp-dir fpp-exec-buffer diff-buffer))
      (with-temp-buffer
        (insert teststring)
        (fill-paragraph-problems-lp:710373-test-base arg tmp-dir fpp-exec-buffer diff-buffer)))))

(defun fill-paragraph-problems-lp:710373-test-base (arg tmp-dir fpp-exec-buffer diff-buffer)
  (goto-char 48)
  (py-fill-paragraph)
  (write-file (concat tmp-dir fpp-exec-buffer))
  (diff (concat tmp-dir fpp-exec-buffer) (concat tmp-dir diff-buffer) "-u")
  (if (featurep 'xemacs)
      (progn
        (set-buffer "*Diff Output*")
        (switch-to-buffer (current-buffer)))
    (set-buffer "*Diff*")
    (sit-for 1)
    (assert (numberp (progn (goto-char (point-min))(search-forward "no differences" nil t 1))) t)
    (message "%s" "fill-paragraph-problems-lp:710373 passed"))
  (set-buffer "fill-paragraph-problems-lp:710373")
  (unless (< 1 arg)
    (set-buffer-modified-p 'nil)
    (kill-buffer (current-buffer))))

(defun triple-quoted-string-dq-lp:302834-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\""))
    (py-bug-tests-intern 'triple-quoted-string-dq-lp:302834 arg teststring)))

(defun triple-quoted-string-dq-lp:302834 ()
  (let ((font-lock-verbose nil))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (goto-char 78)
    (let ((erg (get-char-property (point) 'face)))
      (insert "\"")
      (font-lock-fontify-buffer)
      (assert (eq erg (get-char-property (point) 'face)) "Being stuck inside triple-quoted-string. Did not reach beginning of class."))))

(defun inbound-indentation-multiline-assignement-lp:629916-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "foo_long_long_long_long = (
    bar_long_long_long_long[
        (x_long_long_long_long == X) &
        (y_long_long_long_long == Y)])
"))
    (py-bug-tests-intern 'inbound-indentation-multiline-assignement-lp:629916 arg teststring)))

(defun inbound-indentation-multiline-assignement-lp:629916 ()
  (let ((py-indent-honors-multiline-listing t))
    (goto-char (point-min))
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq 27 (current-indentation)) nil "inbound-indentation-multiline-assignement-lp:629916 test failed")
    (end-of-line)
    (search-backward "[")
    (newline)
    (indent-according-to-mode)
    (assert (eq 27 (current-indentation)) nil "inbound-indentation-multiline-assignement-lp:629916 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq 28 (current-indentation)) nil "inbound-indentation-multiline-assignement-lp:629916 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq 28 (current-indentation)) nil "inbound-indentation-multiline-assignement-lp:629916 test failed")))

(defun previous-statement-lp:637955-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\""))
    (py-bug-tests-intern 'previous-statement-lp:637955 arg teststring)))

(defun previous-statement-lp:637955 ()
  (beginning-of-line)
  (py-previous-statement)
  (assert (eq 31 (point)) nil "previous-statement-lp:637955-test failed."))

(defun nested-indents-lp:328775-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
if x > 0:
    for i in range(100):
        print i
    else:
    print \"All done\"
elif x < 0:
    print \"x is negative\"
"))
    (py-bug-tests-intern 'nested-indents-lp:328775 arg teststring)))

(defun nested-indents-lp:328775 ()
  (let ((font-lock-verbose nil))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (assert (eq 4 (py-compute-indentation)) nil "nested-indents-lp:328775 test failed!")
    (goto-char 41)
    (assert (eq 8 (py-compute-indentation)) nil "nested-indents-lp:328775 test failed!")
    (forward-line 1)
    (assert (eq 4 (py-compute-indentation)) nil "nested-indents-lp:328775 test failed!")))

(defun bullet-lists-in-comments-lp:328782-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring))
    (bullet-lists-in-comments-lp:328782-test-intern arg teststring)))

(defun bullet-lists-in-comments-lp:328782-test-intern (&optional arg teststring)
  (let ((font-lock-verbose nil))
    (set-buffer (get-buffer-create "bullet-lists-in-comments-lp:328782-test"))
    (erase-buffer)
    (with-temp-buffer
      (insert "
## * If the filename is a directory and not a Maildir nor
##   an MH Mailbox, it will be processed as a Mailbox --this bug named here: bullet-lists-in-comments-lp:328782.htm--
##   directory consisting of just .txt and .lorien files.
")
      (when arg (switch-to-buffer (current-buffer)))
      (python-mode)
      (font-lock-mode 1)
      (font-lock-fontify-buffer)
      (goto-char 100)
      (py-fill-paragraph))
    (set-buffer "bullet-lists-in-comments-lp:328782-test")
    (unless (< 1 arg)
      (set-buffer-modified-p 'nil)
      (kill-buffer (current-buffer)))))

(defun imenu-newline-arglist-lp:328783-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "def editor(db, db_name, table_name,
    #api
    dbapi,dbapi_exceptions):
        pass"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'imenu-newline-arglist-lp:328783-base arg teststring)))

(defun imenu-newline-arglist-lp:328783-base ()
  (goto-char 60)
  (py-beginning-of-def-or-class)
  (assert (eq (point) 1) nil "imenu-newline-arglist-lp:328783 test failed"))

(defun hungry-delete-backwards-lp:328853-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'hungry-delete-backwards-lp:328853 arg teststring)))

(defun hungry-delete-backwards-lp:328853 ()
  (goto-char 421)
  (py-hungry-delete-backwards)
  (assert (eq 416 (point)) nil "hungry-delete-backwards test failed"))

(defun hungry-delete-forward-lp:328853-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'hungry-delete-forward-lp:328853 arg teststring)))

(defun hungry-delete-forward-lp:328853 ()
  (goto-char 409)
  (py-hungry-delete-forward)
  (assert (looking-at "#") nil "hungry-delete-backwards test failed"))

(defun UnicodeEncodeError-lp:550661-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -\*- coding: utf-8 -\*-
print u'\\xA9'
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'UnicodeEncodeError-lp:550661-base arg teststring)))

(defun UnicodeEncodeError-lp:550661-base ()
  (python-mode)
  (goto-char 48)
  (push-mark)
  (end-of-line)
  (py-execute-region (line-beginning-position) (point))
  (when (looking-back comint-prompt-regexp)
    (goto-char (1- (match-beginning 0))))
  (sit-for 0.1)
  (assert (looking-back "©") nil "UnicodeEncodeError-lp:550661 test failed"))

(defun indentation-of-continuation-lines-lp:691185-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "    def f(val):
        # current behavior - indent to just after the first space
        a_verry_loonng_variable_nammmee = \\
                                        val
"))
    (py-bug-tests-intern 'indentation-of-continuation-lines-lp:691185 arg teststring)))

(defun indentation-of-continuation-lines-lp:691185 ()
  (let ((py-continuation-offset 2))
    (goto-char (point-min))
    (forward-line 3)
    (indent-according-to-mode)
    (assert (eq 10 (current-indentation)) nil "indentation-of-continuation-lines-lp:691185-test failed!")))

(defun goto-beginning-of-tqs-lp:735328-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "class Foo(object):
\"\"\"
This docstring isn't indented, test should pass anyway.
\"\"\"
"))
    (py-bug-tests-intern 'goto-beginning-of-tqs-lp:735328 arg teststring)))

(defun goto-beginning-of-tqs-lp:735328 ()
  (goto-char (point-min))
  (forward-line 4)
  (indent-according-to-mode)
  (assert (eq 4 (current-column)) nil "goto-beginning-of-tqs-lp:735328-test failed")
  )

(defun class-treated-as-keyword-lp:709478-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "foo = [
    T.div(
        T.tabl(*trows),

        CLASS='blok',)
]
"))
    (py-bug-tests-intern 'class-treated-as-keyword-lp:709478 arg teststring)))

(defun class-treated-as-keyword-lp:709478 ()
  (let ((font-lock-verbose nil))
    (font-lock-fontify-buffer)
    (goto-char 63)
    (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'font-lock-string-face) nil "class-treated-as-keyword-lp:709478d 1th test failed")
    (goto-char 57)
    (assert (not (get-char-property (point) 'face)) nil "class-treated-as-keyword-lp:709478-test 2th failed")))

(defun fore-00007F-breaks-indentation-lp:328788-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "class a:
    def __init__(self):
        self.StyleSetSpec(self.STYLE_FIELD,
\"fore:#00007F\" )
            self.StyleSetSpec(self.STYLE_FIELD,
\"fore:#00007F\" )
                self.StyleSetSpec(self.STYLE_FIELD,
\"fore:#00007F\" )
                    self.StyleSetSpec(self.STYLE_FIELD,
\"fore:#00007F\" )
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'fore-00007F-breaks-indentation-lp:328788 arg teststring)))

(defun fore-00007F-breaks-indentation-lp:328788 ()
  (goto-char (point-min))
  (forward-line 1)
  (indent-according-to-mode)
  (forward-line 1)
  (assert (eq 8 (py-compute-indentation)) nil "fore-00007F-breaks-indentation test failed")
  (indent-according-to-mode)
  (forward-line 1)
  (indent-according-to-mode)
  (forward-line 1)
  (assert (eq 8 (py-compute-indentation)) nil "fore-00007F-breaks-indentation test failed")
  (indent-according-to-mode)
  (forward-line 1)
  (indent-according-to-mode)
  (forward-line 1)
  (assert (eq 8 (py-compute-indentation)) nil "fore-00007F-breaks-indentation test failed")
  (indent-according-to-mode)
  (forward-line 1)
  (indent-according-to-mode)
  (forward-line 1)
  (assert (eq 8 (py-compute-indentation)) nil "fore-00007F-breaks-indentation test failed")
  (indent-according-to-mode)
  (forward-line 1)
  (indent-according-to-mode)
  )

(defun exceptions-not-highlighted-lp:473525-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "excs = (SystemExit, Exception, KeyboardInterrupt)"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'exceptions-not-highlighted-lp:473525 arg teststring)))

(defun exceptions-not-highlighted-lp:473525 ()
  (let ((font-lock-verbose nil))
    (goto-char 39)
    (font-lock-fontify-buffer)
    (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'py-exception-name-face) nil "exceptions-not-highlighted-lp:473525 test failed")))

(defun syntaxerror-on-py-execute-region-lp:691542-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "# -*- coding: utf-8 -*-
print \"Poet Friedrich Hölderlin\""))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'syntaxerror-on-py-execute-region-lp:691542-base arg teststring)))

(defun syntaxerror-on-py-execute-region-lp:691542-base ()
  (let ((oldbuf (current-buffer))
        erg kill-buffer-query-functions py-switch-to-python)
    (when (buffer-live-p (get-buffer (concat "*" py-which-bufname "*")))
      (when
          (processp (get-process py-which-bufname))

        (set-process-query-on-exit-flag (get-process py-which-bufname) nil))
      (kill-buffer (concat "*" py-which-bufname "*")))
    (py-execute-region (line-beginning-position) (line-end-position))
    (when (interactive-p) (switch-to-buffer (current-buffer)))
    (set-buffer (get-buffer (concat "*" py-which-bufname "*")))
    (assert (or (search-forward "Hölderlin" nil t 1)
                (search-backward "Hölderlin" nil t 1)) nil "syntaxerror-on-py-execute-region-lp:691542 test failed")))

(defun backslashed-continuation-line-indent-lp:742993-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "
self.last_abc_attr = \
self.last_xyz_attr = \
self.last_abc_other = \
self.last_xyz_other = None

self.last_abc_attr = \\
self.last_xyz_attr = \\
self.last_abc_other = \\
self.last_xyz_other = None

self.last_abc_attr = \\
self.last_xyz_attr = \\
self.last_abc_other = \\
self.last_xyz_other = None

self.last_abc_attr = \\
self.last_xyz_attr = \\
self.last_abc_other = \\
self.last_xyz_other = None
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'backslashed-continuation-line-indent-lp:742993 arg teststring)))

(defun backslashed-continuation-line-indent-lp:742993 ()
  (let ((py-continuation-offset 2))
    (goto-char (point-min))
    (forward-line 2)
    (insert (concat "\n# py-continuation-offset: " (number-to-string py-continuation-offset)))
    (forward-line 2)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")

    (setq py-continuation-offset 4)
    (forward-line 1)
    (insert (concat "\n# py-continuation-offset: " (number-to-string py-continuation-offset)))
    (forward-line 2)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")

    (setq py-continuation-offset 6)
    (forward-line 1)
    (insert (concat "\n# py-continuation-offset: " (number-to-string py-continuation-offset)))
    (forward-line 2)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    ))

(defun py-decorators-face-lp:744335-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "@foo.bar
def baz():
    pass
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-decorators-face-lp:744335 arg teststring)))

(defun py-decorators-face-lp:744335 ()
  (let ((font-lock-verbose nil))
    (goto-char 7)
    (font-lock-fontify-buffer)
    (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'py-decorators-face) nil "py-decorators-face-lp:744335 test failed")))

(defun indent-after-return-lp:745208-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "class FOO\():
    if len(sys.argv)==1:
        usage\()
        sys.exit\()

    def build_extension\(self, ext):

        if ext.name == '_ctypes':
            if not self.configure_ctypes\(ext):
                return

        try:
            build_ext.build_extension\(self, ext)
        except \(CCompilerError, DistutilsError) as why:
            self.announce\('WARNING: building of extension \"%s\"
failed: %s' %
                          \(ext.name, sys.exc_info()\[1]))
            self.failed.append(ext.name)
            return
        # Workaround for Mac OS X: The Carbon-based modules cannot be
        # reliably imported into a command-line Python
        if 'Carbon' in ext.extra_link_args:
            self.announce\(
                'WARNING: skipping import check for Carbon-based
\"%s\"' %
                ext.name)
            return
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-after-return-lp:745208 arg teststring)))

(defun indent-after-return-lp:745208 ()
  (goto-char (point-max))
  (assert (eq 8 (py-compute-indentation)) nil "indent-after-return-lp:745208 test failed"))

(defun keep-assignements-column-lp:748198-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "bar = foo(a=1,
          b=2,
          c=3)
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'keep-assignements-column-lp:748198 arg teststring)))

(defun keep-assignements-column-lp:748198 ()
  (goto-char 45)
  (py-newline-and-indent)
  (assert (eq 0 (current-column)) nil "py-vor test failed"))

(defun indent-triplequoted-to-itself-lp:752252-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "def foo():
    \"\"\"The real foo thing.\n"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-triplequoted-to-itself-lp:752252-base arg teststring)))

(defun indent-triplequoted-to-itself-lp:752252-base ()
  (assert (eq 4 (py-compute-indentation)) nil "indent-triplequoted-to-itself-lp:752252 test failed"))

(defun multiline-listings-indent-lp:761946-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
    do_something_first(
        a=1,
                       b=2,
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'multiline-listings-indent-lp:761946-base arg teststring)))

(defun multiline-listings-indent-lp:761946-base ()
  (goto-char (point-min))
  (forward-line 3)
  (back-to-indentation)
  (assert (eq 8 (py-compute-indentation)) nil "multiline-listings-indent-lp:761946 test failed"))

(defun new-page-char-causes-loop-lp:762498-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class Foo:
    def baz(self):


"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'new-page-char-causes-loop-lp:762498-base arg teststring)))

(defun new-page-char-causes-loop-lp:762498-base ()
  (goto-char (point-min))
  (forward-line 2)
  (assert (eq 8 (py-compute-indentation)) "new-page-char-causes-loop-lp:762498 test failed"))

(defun nested-dicts-indent-lp:763756-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }
))
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'nested-dicts-indent-lp:763756-base arg teststring)))

(defun nested-dicts-indent-lp:763756-base ()
  (let ((py-indent-honors-multiline-listing nil))
    (goto-char (point-min))
    (forward-line 1)
    (assert (eq 4 (py-compute-indentation)) nil "nested-dicts-indent-lp:763756 test failed")
    (forward-line 1)
    (assert (eq 8 (py-compute-indentation)) nil "nested-dicts-indent-lp:763756 test failed")
    (forward-line 1)
    (assert (eq 12 (py-compute-indentation)) nil "nested-dicts-indent-lp:763756 test failed")))

(defun bad-indent-after-except-lp:771289-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
    try:
        baz()
    except ValueError:
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-tests-intern 'bad-indent-after-except-lp:771289-base arg teststring)))

(defun bad-indent-after-except-lp:771289-base ()
  (assert (eq 8 (py-compute-indentation)) "bad-indent-after-except-lp:771289 test failed"))

(defun indent-open-paren-not-last-lp:771291-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "

# Put point after the comma on the last line and hit return. You
# end up in column 8 (i.e. under the 'g' in 'thing') when you
# should end up in column 20 (under the 'w' in 'with_something').
# Note that this is a different case than previously reported,
# where the open paren was the last thing on the line. When the
# open paren is *not* the last thing on the line, the next line's
# indentation should line up under the first non-whitespace
# character following the open paren.

def foo():
    thing = call_it(with_something,"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-open-paren-not-last-lp:771291-base arg teststring)))

(defun indent-open-paren-not-last-lp:771291-base ()
  (assert (eq 20 (py-compute-indentation)) nil "indent-open-paren-not-last-lp:771291 test failed"))

(defun wrong-indent-after-else-lp:772610-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True:
    pass
else:
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-tests-intern 'wrong-indent-after-else-lp:772610-base arg teststring)))

(defun wrong-indent-after-else-lp:772610-base ()
    (assert (eq 4 (py-compute-indentation)) nil "wrong-indent-after-else-lp:772610 test failed"))

(defun except-indents-wrong-lp:784432-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "try:
    block1
except:
    block2"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'except-indents-wrong-lp:784432-base arg teststring)))

(defun except-indents-wrong-lp:784432-base ()
  (goto-char 17)
  (assert (eq 0 (py-compute-indentation)) nil "except-indents-wrong-lp:784432.txt #1 test failed")
  (goto-char 25)
  (assert (eq 4 (py-compute-indentation)) nil "except-indents-wrong-lp:784432.txt #2 test failed"))

(defun indent-explicitly-set-in-multiline-tqs-lp:784225-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
    with bar('x', \"\"\"
        [hello]
"
))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-tests-intern 'indent-explicitly-set-in-multiline-tqs-lp:784225-base arg teststring)))

(defun indent-explicitly-set-in-multiline-tqs-lp:784225-base ()
    (assert (eq 8 (py-compute-indentation)) nil "explicitly-dedented-in-list-lp:784225 test failed"))

(defun unbalanced-parentheses-lp:784645-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
    something()
    another(
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-tests-intern 'unbalanced-parentheses-lp:784645-base arg teststring)))

(defun unbalanced-parentheses-lp:784645-base ()
    (goto-char 27)
    (newline-and-indent)
    (assert (eq 4 (py-compute-indentation)) nil "unbalanced-parentheses-lp:784645 test failed"))

(defun explicitly-indent-in-list-lp:785018-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
    with bar('x',
        [hello]
"
))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-tests-intern 'explicitly-indent-in-list-lp:785018-base arg teststring)))

(defun explicitly-indent-in-list-lp:785018-base ()
    (assert (eq 8 (py-compute-indentation)) nil "explicitly-dedented-in-list-lp:784225 test failed"))

(defun explicit-backslashed-continuation-line-indent-lp:785091-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "        a_verry_loonng_variable_nammmee = \\
                                        val \\
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-tests-intern 'explicit-backslashed-continuation-line-indent-lp:785091-base arg teststring)))

(defun explicit-backslashed-continuation-line-indent-lp:785091-base ()
    (assert (eq 40 (py-compute-indentation)) nil "explicit-backslashed-continuation-line-indent-lp:785091  test failed"))

(defun indentation-error-lp:795773-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class MailTransportAgentAliases:
    \"\"\"Utility for generating all the aliases of a mailing
list.\"\"\"

    implements(IMailTransportAgentAliases)

    def aliases(self, mlist):
        \"\"\"See `IMailTransportAgentAliases`.\"\"\"
        # Always return
        yield mlist.posting_address
        for destination in SUBDESTINATIONS:
            yield '{0}-{1}@{2}'.format(mlist.list_name,
                                             destination,
                                             mlist.host_name)
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-tests-intern 'indentation-error-lp:795773-base arg teststring)))

(defun indentation-error-lp:795773-base ()
  (goto-char 385)
  (assert (eq 39 (py-compute-indentation)) nil "indentation-error-lp:795773 test failed"))

(defun class-highlighted-as-keywords-lp:798287-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class X:
    pass

# Everything is highlighted as a keyword.
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-tests-intern 'class-highlighted-as-keywords-lp:798287-base arg teststring)))

(defun class-highlighted-as-keywords-lp:798287-base ()
  (let ((font-lock-verbose nil))
    (goto-char 7)
    (font-lock-fontify-buffer)
    (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'py-class-name-face) nil "class-highlighted-as-keywords-lp:798287 test failed")))

(defun indent-function-arglist-lp:800088-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def long_function_name(
        var_one, var_two, var_three,
        var_four):
     print(var_one)
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-tests-intern 'indent-function-arglist-lp:800088-base arg teststring)))

(defun indent-function-arglist-lp:800088-base ()
  (goto-char 25)
  (let ((py-indent-offset 4))
    (assert (eq 8 (py-compute-indentation)) nil "indent-function-arglist-lp:800088 test failed")))

(defun python-mode-hangs-lp:801780-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "@jit.unroll_safe
def pushrevvalues(self, n, values_w): # n should be len(values_w)
    make_sure_not_resized(values_w)
    while True:
        n -= 1
        if n < 0:
            break
        self.pushvalue(values_w[n])
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-tests-intern 'python-mode-hangs-lp:801780-base arg teststring)))

(defun python-mode-hangs-lp:801780-base ()
    (assert (eq 18 (py-beginning-of-def-or-class)) nil "python-mode-hangs-lp:801780 test failed"))

(defun stops-backslashed-line-lp:802504-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

if bar == 1 or bar == 2 or bar == 3 or bar == 4 or bar == 5 or bar == 6 or bar == 7 \\
  or bar == 8 or bar == 9 or bar == 10 or bar == 11 or bar == 12 or bar == 13 \\
  or bar == 14 or bar == 15 or bar == 16 or bar == 17 or bar == 18:
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-tests-intern 'stops-backslashed-line-lp:802504-base arg teststring)))

(defun stops-backslashed-line-lp:802504-base ()
    (goto-char 49)
    (assert (eq 282 (py-end-of-statement)) nil "stops-backslashed-line-lp:802504 test failed"))

(defun stops-backslashed-line-lp:802504-test2 (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

if x>1 and x<100 and y>1 and y<200:
  if bar == 1 or bar == 2 or bar == 3 or bar == 4 or bar == 5 or bar == 6 or bar == 7 \\
  or bar == 8 or bar == 9 or bar == 10 or bar == 11 or bar == 12 or bar == 13 or \\
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-tests-intern 'stops-backslashed-line2-lp:802504-base arg teststring)))

(defun stops-backslashed-line2-lp:802504-base ()
    (assert (eq 87 (py-beginning-of-statement)) nil "stops-backslashed-line-lp:802504 test failed"))

(defun python-mode-slow-lp:803275-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "# commands.py - command processing for mercurial
#
# Copyright 2005-2007 Matt Mackall <mpm@selenic.com>
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.

from node import hex, bin, nullid, nullrev, short
from lock import release
from i18n import _, gettext
import os, re, difflib, time, tempfile, errno
import hg, scmutil, util, revlog, extensions, copies, error, bookmarks
import patch, help, url, encoding, templatekw, discovery
import archival, changegroup, cmdutil, hbisect
import sshserver, hgweb, hgweb.server, commandserver
import merge as mergemod
import minirst, revset, fileset
import dagparser, context, simplemerge
import random, setdiscovery, treediscovery, dagutil

table = {}

command = cmdutil.command(table)

# common command options

globalopts = [
    ('R', 'repository', '',
     _('repository root directory or name of overlay bundle file'),
     _('REPO')),
    ('', 'cwd', '',
     _('change working directory'), _('DIR')),
    ('y', 'noninteractive', None,
     _('do not prompt, assume \\'yes\\' for any required answers')),
    ('q', 'quiet', None, _('suppress output')),
    ('v', 'verbose', None, _('enable additional output')),
    ('', 'config', [],
     _('set/override config option (use \\'section.name=value\\')'),
     _('CONFIG')),
    ('', 'debug', None, _('enable debugging output')),
    ('', 'debugger', None, _('start debugger')),
    ('', 'encoding', encoding.encoding, _('set the charset encoding'),
     _('ENCODE')),
    ('', 'encodingmode', encoding.encodingmode,
     _('set the charset encoding mode'), _('MODE')),
    ('', 'traceback', None, _('always print a traceback on exception')),
    ('', 'time', None, _('time how long the command takes')),
    ('', 'profile', None, _('print command execution profile')),
    ('', 'version', None, _('output version information and exit')),
    ('h', 'help', None, _('display help and exit')),
]

dryrunopts = [('n', 'dry-run', None,
               _('do not perform actions, just print output'))]

remoteopts = [
    ('e', 'ssh', '',
     _('specify ssh command to use'), _('CMD')),
    ('', 'remotecmd', '',
     _('specify hg command to run on the remote side'), _('CMD')),
    ('', 'insecure', None,
     _('do not verify server certificate (ignoring web.cacerts config)')),
]

walkopts = [
    ('I', 'include', [],
     _('include names matching the given patterns'), _('PATTERN')),
    ('X', 'exclude', [],
     _('exclude names matching the given patterns'), _('PATTERN')),
]

commitopts = [
    ('m', 'message', '',
     _('use text as commit message'), _('TEXT')),
    ('l', 'logfile', '',
     _('read commit message from file'), _('FILE')),
]

commitopts2 = [
    ('d', 'date', '',
     _('record the specified date as commit date'), _('DATE')),
    ('u', 'user', '',
     _('record the specified user as committer'), _('USER')),
]

templateopts = [
    ('', 'style', '',
     _('display using template map file'), _('STYLE')),
    ('', 'template', '',
     _('display with template'), _('TEMPLATE')),
]

logopts = [
    ('p', 'patch', None, _('show patch')),
    ('g', 'git', None, _('use git extended diff format')),
    ('l', 'limit', '',
     _('limit number of changes displayed'), _('NUM')),
    ('M', 'no-merges', None, _('do not show merges')),
    ('', 'stat', None, _('output diffstat-style summary of changes')),
] + templateopts

diffopts = [
    ('a', 'text', None, _('treat all files as text')),
    ('g', 'git', None, _('use git extended diff format')),
    ('', 'nodates', None, _('omit dates from diff headers'))
]

diffopts2 = [
    ('p', 'show-function', None, _('show which function each change is in')),
    ('', 'reverse', None, _('produce a diff that undoes the changes')),
    ('w', 'ignore-all-space', None,
     _('ignore white space when comparing lines')),
    ('b', 'ignore-space-change', None,
     _('ignore changes in the amount of white space')),
    ('B', 'ignore-blank-lines', None,
     _('ignore changes whose lines are all blank')),
    ('U', 'unified', '',
     _('number of lines of context to show'), _('NUM')),
    ('', 'stat', None, _('output diffstat-style summary of changes')),
]

similarityopts = [
    ('s', 'similarity', '',
     _('guess renamed files by similarity (0<=s<=100)'), _('SIMILARITY'))
]

subrepoopts = [
    ('S', 'subrepos', None,
     _('recurse into subrepositories'))
]

# Commands start here, listed alphabetically

@command('^add',
    walkopts + subrepoopts + dryrunopts,
    _('[OPTION]... [FILE]...'))
def add(ui, repo, \*pats, \*\*opts):
    \"\"\"add the specified files on the next commit

    Schedule files to be version controlled and added to the
    repository.

    The files will be added to the repository at the next commit. To
    undo an add before that, see :hg:`forget`.

    If no names are given, add all files to the repository.

    .. container:: verbose

       An example showing how new (unknown) files are added
       automatically by :hg:`add`::

         \$ ls
         foo.c
         \$ hg status
         ? foo.c
         \$ hg add
         adding foo.c
         \$ hg status
         A foo.c

    Returns 0 if all files are successfully added.
    \"\"\"

    m = scmutil.match(repo[None], pats, opts)
    rejected = cmdutil.add(ui, repo, m, opts.get('dry_run'),
                           opts.get('subrepos'), prefix=\"\")
    return rejected and 1 or 0
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-tests-intern 'python-mode-slow-lp:803275-base arg teststring)))

(defun python-mode-slow-lp:803275-base ()
    (goto-char (point-min))
    (assert (eq 5430 (py-end-of-def-or-class)) nil "python-mode-slow-lp:803275 test failed"))

(defun py-master-file-not-honored-lp:794850-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
 # -*- coding: utf-8 -*-

# Local Variables:
# py-master-file: \"/usr/tmp/my-master.py\"
# End:

print u'\xA9'
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-master-file-not-honored-lp:794850-base arg teststring)))

(defun py-master-file-not-honored-lp:794850-base ()
  (save-excursion 
    (set-buffer (get-buffer-create "lp:794850-test-master.py"))
    (erase-buffer)
    (insert "#! /usr/bin/env python
 # -*- coding: utf-8 -*-

print \"Hello, I'm your master!\"
")
    (write-file "/var/tmp/my-master.py"))
  (py-execute-buffer))

;;    (assert nil "py-master-file-not-honored-lp:794850 test failed"))

(provide 'py-bug-numbered-tests)
;;; py-bug-numbered-tests.el ends here




