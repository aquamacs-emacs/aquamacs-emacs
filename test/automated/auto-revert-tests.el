;;; auto-revert-tests.el --- Tests of auto-revert

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; A whole test run can be performed calling the command `auto-revert-test-all'.

;;; Code:

(require 'ert)
(require 'autorevert)
(setq auto-revert-notify-exclude-dir-regexp "nothing-to-be-excluded"
      auto-revert-stop-on-user-input nil)

(when (string= system-type "darwin")
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp))

(defconst auto-revert--timeout 10
  "Time to wait until a message appears in the *Messages* buffer.")

(defun auto-revert--wait-for-revert (buffer)
  "Wait until the *Messages* buffer reports reversion of BUFFER."
  (with-timeout (auto-revert--timeout nil)
    (with-current-buffer "*Messages*"
      (while
          (null (string-match
                 (format-message "Reverting buffer `%s'." (buffer-name buffer))
                 (buffer-string)))
	(if (with-current-buffer buffer auto-revert-use-notify)
	    (read-event nil nil 0.1)
	  (sleep-for 0.1))))))

(ert-deftest auto-revert-test00-auto-revert-mode ()
  "Check autorevert for a file."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (let ((tmpfile (make-temp-file "auto-revert-test"))
        buf)
    (unwind-protect
	(progn
          (with-current-buffer (get-buffer-create "*Messages*")
            (narrow-to-region (point-max) (point-max)))
	  (write-region "any text" nil tmpfile nil 'no-message)
	  (setq buf (find-file-noselect tmpfile))
	  (with-current-buffer buf
	    (should (string-equal (buffer-string) "any text"))
            ;; `buffer-stale--default-function' checks for
            ;; `verify-visited-file-modtime'.  We must ensure that it
            ;; returns nil.
            (sleep-for 1)
	    (auto-revert-mode 1)
	    (should auto-revert-mode)

	    ;; Modify file.  We wait for a second, in order to have
	    ;; another timestamp.
	    (sleep-for 1)
            (write-region "another text" nil tmpfile nil 'no-message)

	    ;; Check, that the buffer has been reverted.
            (auto-revert--wait-for-revert buf)
            (should (string-match "another text" (buffer-string)))

            ;; When the buffer is modified, it shall not be reverted.
            (with-current-buffer (get-buffer-create "*Messages*")
              (narrow-to-region (point-max) (point-max)))
            (set-buffer-modified-p t)
	    (sleep-for 1)
            (write-region "any text" nil tmpfile nil 'no-message)

	    ;; Check, that the buffer hasn't been reverted.
            (auto-revert--wait-for-revert buf)
            (should-not (string-match "any text" (buffer-string)))))

      ;; Exit.
      (with-current-buffer "*Messages*" (widen))
      (ignore-errors
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (ignore-errors (delete-file tmpfile)))))

;; This is inspired by Bug#21841.
(ert-deftest auto-revert-test01-auto-revert-several-files ()
  "Check autorevert for several files at once."
  :tags '(:expensive-test)
  (skip-unless (executable-find "cp"))

  (let* ((cp (executable-find "cp"))
         (tmpdir1 (make-temp-file "auto-revert-test" 'dir))
         (tmpdir2 (make-temp-file "auto-revert-test" 'dir))
         (tmpfile1
          (make-temp-file (expand-file-name "auto-revert-test" tmpdir1)))
         (tmpfile2
          (make-temp-file (expand-file-name "auto-revert-test" tmpdir1)))
         buf1 buf2)
    (unwind-protect
	(progn
          (with-current-buffer (get-buffer-create "*Messages*")
            (narrow-to-region (point-max) (point-max)))
	  (write-region "any text" nil tmpfile1 nil 'no-message)
	  (setq buf1 (find-file-noselect tmpfile1))
	  (write-region "any text" nil tmpfile2 nil 'no-message)
	  (setq buf2 (find-file-noselect tmpfile2))

          (dolist (buf (list buf1 buf2))
            (with-current-buffer buf
              (should (string-equal (buffer-string) "any text"))
              ;; `buffer-stale--default-function' checks for
              ;; `verify-visited-file-modtime'.  We must ensure that
              ;; it returns nil.
              (sleep-for 1)
              (auto-revert-mode 1)
              (should auto-revert-mode)))

          ;; Modify files.  We wait for a second, in order to have
          ;; another timestamp.
          (sleep-for 1)
          (write-region
           "another text" nil
           (expand-file-name (file-name-nondirectory tmpfile1) tmpdir2)
           nil 'no-message)
          (write-region
           "another text" nil
           (expand-file-name (file-name-nondirectory tmpfile2) tmpdir2)
           nil 'no-message)
          ;;(copy-directory tmpdir2 tmpdir1 nil 'copy-contents)
          ;; Strange, that `copy-directory' does not work as expected.
          ;; The following shell command is not portable on all
          ;; platforms, unfortunately.
          (shell-command (format "%s -f %s/* %s" cp tmpdir2 tmpdir1))

          ;; Check, that the buffers have been reverted.
          (dolist (buf (list buf1 buf2))
            (with-current-buffer buf
              (auto-revert--wait-for-revert buf)
              (should (string-match "another text" (buffer-string))))))

      ;; Exit.
      (with-current-buffer "*Messages*" (widen))
      (ignore-errors
        (dolist (buf (list buf1 buf2))
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf)))
      (ignore-errors (delete-directory tmpdir1 'recursive))
      (ignore-errors (delete-directory tmpdir2 'recursive)))))

(ert-deftest auto-revert-test02-auto-revert-tail-mode ()
  "Check autorevert tail mode."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (let ((tmpfile (make-temp-file "auto-revert-test"))
        buf)
    (unwind-protect
	(progn
          (with-current-buffer (get-buffer-create "*Messages*")
            (narrow-to-region (point-max) (point-max)))
          (write-region "any text" nil tmpfile nil 'no-message)
	  (setq buf (find-file-noselect tmpfile))
	  (with-current-buffer buf
            ;; `buffer-stale--default-function' checks for
            ;; `verify-visited-file-modtime'.  We must ensure that it
            ;; returns nil.
            (sleep-for 1)
	    (auto-revert-tail-mode 1)
	    (should auto-revert-tail-mode)
            (erase-buffer)
            (insert "modified text\n")
            (set-buffer-modified-p nil)

	    ;; Modify file.  We wait for a second, in order to have
	    ;; another timestamp.
	    (sleep-for 1)
            (write-region "another text" nil tmpfile 'append 'no-message)

	    ;; Check, that the buffer has been reverted.
            (auto-revert--wait-for-revert buf)
            (should
             (string-match "modified text\nanother text" (buffer-string)))))

      ;; Exit.
      (with-current-buffer "*Messages*" (widen))
      (ignore-errors (kill-buffer buf))
      (ignore-errors (delete-file tmpfile)))))

(ert-deftest auto-revert-test03-auto-revert-mode-dired ()
  "Check autorevert for dired."
  ;; `auto-revert-buffers' runs every 5".  And we must wait, until the
  ;; file has been reverted.
  (let* ((tmpfile (make-temp-file "auto-revert-test"))
         (name (file-name-nondirectory tmpfile))
         buf)
    (unwind-protect
	(progn
	  (setq buf (dired-noselect temporary-file-directory))
	  (with-current-buffer buf
            ;; `buffer-stale--default-function' checks for
            ;; `verify-visited-file-modtime'.  We must ensure that it
            ;; returns nil.
            (sleep-for 1)
            (auto-revert-mode 1)
            (should auto-revert-mode)
	    (should
             (string-match name (substring-no-properties (buffer-string))))

	    ;; Delete file.  We wait for a second, in order to have
	    ;; another timestamp.
            (with-current-buffer (get-buffer-create "*Messages*")
              (narrow-to-region (point-max) (point-max)))
	    (sleep-for 1)
            (delete-file tmpfile)

	    ;; Check, that the buffer has been reverted.
            (auto-revert--wait-for-revert buf)
            (should-not
             (string-match name (substring-no-properties (buffer-string))))

            ;; Make dired buffer modified.  Check, that the buffer has
            ;; been still reverted.
            (with-current-buffer (get-buffer-create "*Messages*")
              (narrow-to-region (point-max) (point-max)))
            (set-buffer-modified-p t)
	    (sleep-for 1)
            (write-region "any text" nil tmpfile nil 'no-message)

	    ;; Check, that the buffer has been reverted.
            (auto-revert--wait-for-revert buf)
            (should
             (string-match name (substring-no-properties (buffer-string))))))

      ;; Exit.
      (with-current-buffer "*Messages*" (widen))
      (ignore-errors
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (ignore-errors (delete-file tmpfile)))))

(defun auto-revert-test-all (&optional interactive)
  "Run all tests for \\[auto-revert]."
  (interactive "p")
  (if interactive
      (ert-run-tests-interactively "^auto-revert-")
    (ert-run-tests-batch "^auto-revert-")))

(provide 'auto-revert-tests)
;;; auto-revert-tests.el ends here
