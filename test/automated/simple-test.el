;;; simple-test.el --- Tests for simple.el           -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

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

;;; Code:

(require 'ert)
(require 'python)

(defmacro simple-test--dummy-buffer (&rest body)
  (declare (indent 0)
           (debug t))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (setq indent-tabs-mode nil)
     (insert "(a b")
     (save-excursion (insert " c d)"))
     ,@body
     (cons (buffer-substring (point-min) (point))
           (buffer-substring (point) (point-max)))))


(defmacro simple-test--transpositions (&rest body)
  (declare (indent 0)
           (debug t))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert "(s1) (s2) (s3) (s4) (s5)")
     (backward-sexp 1)
     ,@body
     (cons (buffer-substring (point-min) (point))
           (buffer-substring (point) (point-max)))))


;;; `newline'
(ert-deftest newline ()
  (should-error (newline -1))
  (should (equal (simple-test--dummy-buffer (newline 1))
                 '("(a b\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-mode -1)
                   (call-interactively #'newline))
                 '("(a b\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer
                   (let ((current-prefix-arg 5))
                     (call-interactively #'newline)))
                 '("(a b\n\n\n\n\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer (newline 5))
                 '("(a b\n\n\n\n\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-char 1)
                   (newline 1))
                 '("(a b \n" . "c d)"))))

(ert-deftest newline-indent ()
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (newline 1))
                 '("(a b\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (newline 1 'interactive))
                 '("(a b\n   " . "c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (let ((current-prefix-arg nil))
                     (call-interactively #'newline)
                     (call-interactively #'newline)))
                 '("(a b\n\n   " . "c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (newline 5 'interactive))
                 '("(a b\n\n\n\n\n   " . "c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (let ((current-prefix-arg 5))
                     (call-interactively #'newline)))
                 '("(a b\n\n\n\n\n   " . "c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-char 1)
                   (electric-indent-local-mode 1)
                   (newline 1 'interactive))
                 '("(a b\n   " . "c d)"))))


;;; `open-line'
(ert-deftest open-line ()
  (should-error (open-line -1))
  (should-error (open-line))
  (should (equal (simple-test--dummy-buffer (open-line 1))
                 '("(a b" . "\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-mode -1)
                   (call-interactively #'open-line))
                 '("(a b" . "\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (let ((current-prefix-arg 5))
                     (call-interactively #'open-line)))
                 '("(a b" . "\n\n\n\n\n c d)")))
  (should (equal (simple-test--dummy-buffer (open-line 5))
                 '("(a b" . "\n\n\n\n\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-char 1)
                   (open-line 1))
                 '("(a b " . "\nc d)"))))

(ert-deftest open-line-margin-and-prefix ()
  (should (equal (simple-test--dummy-buffer
                   (let ((left-margin 10))
                     (open-line 3)))
                 '("(a b" . "\n\n\n          c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-line 0)
                   (let ((left-margin 2))
                     (open-line 1)))
                 '("  " . "\n  (a b c d)")))
  (should (equal (simple-test--dummy-buffer
                   (let ((fill-prefix "- - "))
                     (open-line 1)))
                 '("(a b" . "\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-line 0)
                   (let ((fill-prefix "- - "))
                     (open-line 1)))
                 '("- - " . "\n(a b c d)"))))

;; For a while, from 24 Oct - 21 Nov 2015, `open-line' in the Emacs
;; development tree became sensitive to `electric-indent-mode', which
;; it had not been before.  This sensitivity was reverted for the
;; Emacs 25 release, so it could be discussed further (see thread
;; "Questioning the new behavior of `open-line'." on the Emacs Devel
;; mailing list, and bug #21884).
(ert-deftest open-line-indent ()
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (open-line 1))
                 '("(a b" . "\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (open-line 1))
                 '("(a b" . "\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (let ((current-prefix-arg nil))
                     (call-interactively #'open-line)
                     (call-interactively #'open-line)))
                 '("(a b" . "\n\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (open-line 5))
                 '("(a b" . "\n\n\n\n\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (let ((current-prefix-arg 5))
                     (call-interactively #'open-line)))
                 '("(a b" . "\n\n\n\n\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-char 1)
                   (electric-indent-local-mode 1)
                   (open-line 1))
                 '("(a b " . "\nc d)"))))

;; From 24 Oct - 21 Nov 2015, `open-line' took a second argument
;; INTERACTIVE and ran `post-self-insert-hook' if the argument was
;; true.  This test tested that.  Currently, however, `open-line'
;; does not run run `post-self-insert-hook' at all, so for now
;; this test just makes sure that it doesn't.
(ert-deftest open-line-hook ()
  (let* ((x 0)
         (inc (lambda () (setq x (1+ x)))))
    (simple-test--dummy-buffer
      (add-hook 'post-self-insert-hook inc nil 'local)
      (open-line 1))
    (should (= x 0))
    (simple-test--dummy-buffer
      (add-hook 'post-self-insert-hook inc nil 'local)
      (open-line 1))
    (should (= x 0))

    (unwind-protect
        (progn
          (add-hook 'post-self-insert-hook inc)
          (simple-test--dummy-buffer
            (open-line 1))
          (should (= x 0))
          (simple-test--dummy-buffer
            (open-line 10))
          (should (= x 0)))
      (remove-hook 'post-self-insert-hook inc))))


;;; `delete-trailing-whitespace'
(ert-deftest simple-delete-trailing-whitespace ()
  "Test bug#21766: delete-whitespace sometimes deletes non-whitespace."
  (defvar python-indent-guess-indent-offset)  ; to avoid a warning
  (let ((python (featurep 'python))
        (python-indent-guess-indent-offset nil)
        (delete-trailing-lines t))
    (unwind-protect
        (with-temp-buffer
          (python-mode)
          (insert (concat "query = \"\"\"WITH filtered AS \n"
                          "WHERE      \n"
                          "\"\"\".format(fv_)\n"
                          "\n"
                          "\n"))
          (delete-trailing-whitespace)
          (should (equal (count-lines (point-min) (point-max)) 3)))
      ;; Let's clean up if running interactive
      (unless (or noninteractive python)
        (unload-feature 'python)))))


;;; auto-boundary tests
(ert-deftest undo-auto-boundary-timer ()
  (should
   undo-auto-current-boundary-timer))

(ert-deftest undo-auto--boundaries-added ()
  ;; The change in the buffer should have caused addition
  ;; to undo-auto--undoably-changed-buffers.
  (should
   (with-temp-buffer
     (setq buffer-undo-list nil)
     (insert "hello")
     (member (current-buffer) undo-auto--undoably-changed-buffers)))
  ;; The head of buffer-undo-list should be the insertion event, and
  ;; therefore not nil
  (should
   (with-temp-buffer
     (setq buffer-undo-list nil)
     (insert "hello")
     (car buffer-undo-list)))
  ;; Now the head of the buffer-undo-list should be a boundary and so
  ;; nil. We have to call auto-boundary explicitly because we are out
  ;; of the command loop
  (should-not
   (with-temp-buffer
     (setq buffer-undo-list nil)
     (insert "hello")
     (car buffer-undo-list)
     (undo-auto--boundaries 'test))))

;;; Transposition with negative args (bug#20698, bug#21885)
(ert-deftest simple-transpose-subr ()
  (should (equal (simple-test--transpositions (transpose-sexps -1))
                 '("(s1) (s2) (s4)" . " (s3) (s5)")))
  (should (equal (simple-test--transpositions (transpose-sexps -2))
                 '("(s1) (s4)" . " (s2) (s3) (s5)"))))


;; Test for a regression introduced by undo-auto--boundaries changes.
;; https://lists.gnu.org/archive/html/emacs-devel/2015-11/msg01652.html
(defun undo-test-kill-c-a-then-undo ()
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (setq buffer-undo-list nil)
    (insert "a\nb\n\c\n")
    (goto-char (point-max))
    ;; We use a keyboard macro because it adds undo events in the same
    ;; way as if a user were involved.
    (kmacro-call-macro nil nil nil
                       [left
                        ;; Delete "c"
                        backspace
                        left left left
                        ;; Delete "a"
                        backspace
                        ;; C-/ or undo
                        67108911
                        ])
    (point)))

(defun undo-test-point-after-forward-kill ()
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (setq buffer-undo-list nil)
    (insert "kill word forward")
    ;; Move to word "word".
    (goto-char 6)
    (kmacro-call-macro nil nil nil
                       [
                        ;; kill-word
                        C-delete
                        ;; undo
                        67108911
                        ])
    (point)))

(ert-deftest undo-point-in-wrong-place ()
  (should
   ;; returns 5 with the bug
   (= 2
      (undo-test-kill-c-a-then-undo)))
  (should
   (= 6
      (undo-test-point-after-forward-kill))))

(defmacro simple-test-undo-with-switched-buffer (buffer &rest body)
  (declare (indent 1) (debug t))
  (let ((before-buffer (make-symbol "before-buffer")))
    `(let ((,before-buffer (current-buffer)))
       (unwind-protect
           (progn
             (switch-to-buffer ,buffer)
             ,@body)
         (switch-to-buffer ,before-buffer)))))

;; This tests for a regression in emacs 25.0 see bug #23632
(ert-deftest simple-test-undo-extra-boundary-in-tex ()
  (should
   (string=
    ""
    (simple-test-undo-with-switched-buffer
        "temp.tex"
      (latex-mode)
      ;; This macro calls `latex-insert-block'
      (execute-kbd-macro
       (read-kbd-macro
        "
C-c C-o			;; latex-insert-block
RET			;; newline
C-/                     ;; undo
"
        ))
      (buffer-substring-no-properties
       (point-min)
       (point-max))))))

(ert-deftest missing-record-point-in-undo ()
  "Check point is being restored correctly.

See Bug#21722."
  (should
   (= 5
      (with-temp-buffer
       (generate-new-buffer " *temp*")
       (emacs-lisp-mode)
       (setq buffer-undo-list nil)
       (insert "(progn (end-of-line) (insert \"hello\"))")
       (beginning-of-line)
       (forward-char 4)
       (undo-boundary)
       (eval-defun nil)
       (undo-boundary)
       (undo)
       (point)))))

(provide 'simple-test)
;;; simple-test.el ends here
