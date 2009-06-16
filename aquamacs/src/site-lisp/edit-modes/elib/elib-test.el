;;;; $Id: elib-test.el,v 1.1 2006/12/01 23:31:41 davidswelt Exp $
;;;; This file contains functions for testing all packages in ELIB.

;; Copyright (C) 1991-1995 Free Software Foundation

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;;	Inge Wallin <inge@lysator.liu.se>
;; Maintainer: elib-maintainers@lysator.liu.se
;; Created: 3 Aug 1992

;;;; This file is part of the GNU Emacs lisp library, Elib.
;;;;
;;;; GNU Elib is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; GNU Elib is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Elib; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA 02111-1307, USA
;;;;

;;; Code:

(require 'backquote)

;;; ================================================================
;;; Routines common to all the ELIB packages.


(defun test-package (package-test-suite)
  "Run through a list of test cases.
Each test case is a list of one of the following types:
 (test-case error expected-error)
 (test-case result expected-result)
 (test-case dont-care)"

  (interactive "Xtest suite: ")
  (let ((testno 0)
	(all-is-ok t))
    (mapcar (function 
	     (lambda (ele)
	       (if (not (test-a-case ele testno))
		   (setq all-is-ok nil))
	       (setq testno (1+ testno))))
	    package-test-suite)
    (if all-is-ok
	(princ "None of the errors have been found.\n"))))


(defun test-a-case (test-case testno)
  ;; Return t if ok and nil if error.

  (let* ((test-data (car test-case))
	 (result-type (car (cdr test-case)))
	 (expected-result (if (eq result-type 'dont-care)
			      nil
			    (car (cdr (cdr test-case)))))
	 result)
    (cond
     ((eq result-type 'error)

      ;; An error expected:
      (condition-case error
	  (progn
	    (setq result (eval test-data))
	    (princ (format "%3d: An error should have occured here: %s\n"
			   testno test-case))
	    (princ (format "     The result was: %s\n" result))
	    nil)
	(error (if (equal error expected-result)
		   t
		 (princ (format "%3d: Wrong error.\n" testno))
		 (princ (format "     %s\n" error))
		 (princ (format "     The error should have been:\n     %s\n"
				expected-result))
		 nil))))

     ((or (eq result-type 'result)
	  (eq result-type 'dont-care))

      ;; No error expected:
      (condition-case error
	  (let ((result (eval test-data)))
	    (if (or (eq result-type 'dont-care)
		    (equal result expected-result))
		t
	      (princ
	       (format
		"%3d: Wrong result while evaluating\n%s\nThe result: %s\n"
		testno test-case result))
	      (princ (format "     The result should have been: %s\n" 
			     expected-result))
	      nil))
	(error  
	 (princ
	  (format
	   "%3d: An error occured while evaluating\n%s\nThe error:\n     %s\n"
	   testno test-case error))
	 (if (eq result-type 'result)
	     (princ (format "     The result should have been:\n     %s\n"
			    expected-result)))
	 nil)))
     (t
      (princ (format "%3d: Misformed test case:\n     %s\n"
		     testno test-case))))))


;;; ================================================================
;;;         Tests for the function variety of the stack.


(setq stack-f-tests
      '(((setq stk (stack-create)) result (STACK))
	((setq xyz 'xyz) result xyz)

	((stack-p stk) result t)
	((stack-empty stk) result t)
	((stack-pop stk) result nil)
	((stack-top stk) result nil)
	((stack-length stk) result 0)
	((stack-all stk) result nil)

	((stack-p xyz) result nil)
	((stack-empty xyz) error (wrong-type-argument listp xyz))
	((stack-pop xyz) error (wrong-type-argument listp xyz))
	((stack-top xyz) error (wrong-type-argument listp xyz))
	((stack-length xyz) error (wrong-type-argument listp xyz))
	((stack-all xyz) error (wrong-type-argument listp xyz))

	((setq stk2 (stack-copy stk)) result (STACK))
	((eq stk stk2) result nil)
	((equal stk stk2) result t)

	((stack-push stk 'a) result (a))
	((stack-top stk) result a)
	((stack-all stk) result (a))
	((stack-empty stk) result nil)
	((stack-length stk) result 1)

	((stack-push stk 'b) result (b a))
	((stack-all stk) result (b a))
	((stack-length stk) result 2)
	((stack-nth stk -1) result b)
	((stack-nth stk 0) result b)
	((stack-nth stk 1) result a)
	((stack-nth stk 2) result nil)
	((stack-nth stk 3) result nil)

	((setq stk2 (stack-copy stk)) result (STACK b a))
	((eq stk stk2) result nil)
	((equal stk stk2) result t)

	((stack-pop stk) result b)
	((stack-all stk) result (a))
	((stack-pop stk) result a)
	((stack-all stk) result nil)
	((stack-empty stk) result t)

	((stack-push stk 'a) result (a))
	((stack-push stk 'b) result (b a))
	((stack-clear stk) result nil) 
	((stack-p stk) result t)		
	((stack-empty stk) result t)		
	((stack-pop stk) result nil)	     
	((stack-top stk) result nil)
	((stack-length stk) result 0)
	((stack-all stk) result nil)
	))
(setq stack-m-tests stack-f-tests)


(defun test-stack-f ()
    (interactive)
    (let ((load-path '(".")))
      (require 'stack-f)
      (with-output-to-temp-buffer "*Elib-test*"
	(princ "stack-f:\n")
	(test-package stack-f-tests))))


;;
;; Use the same tests for stack-m as for stack-f
;;
(defun test-stack-m ()
    (interactive)
    (let ((load-path '(".")))
      (require 'stack-m)
      (with-output-to-temp-buffer "*Elib-test*"
	(princ "stack-m:\n")
	(test-package stack-m-tests))))




;;; ================================================================
;;;         Tests for the function variety of the queue.


(setq queue-f-tests
      '(((setq que (queue-create)) result (QUEUE nil))
	((setq xyz 'xyz) result xyz)

	((queue-p que) result t)
	((queue-empty que) result t)
	((queue-dequeue que) result nil)
	((queue-first que) result nil)
	((queue-length que) result 0)
	((queue-all que) result nil)

	((queue-p xyz) result nil)
	((queue-empty xyz) error (wrong-type-argument listp xyz))
	((queue-dequeue xyz) error (wrong-type-argument listp xyz))
	((queue-first xyz) error (wrong-type-argument listp xyz))
	((queue-length xyz) error (wrong-type-argument listp xyz))
	((queue-all xyz) error (wrong-type-argument listp xyz))

	((setq que2 (queue-copy que)) result (QUEUE nil))
	((eq que que2) result nil)
	((equal que que2) result t)

	((queue-enqueue que 'a) result (a))
	((queue-first que) result a)
	((queue-all que) result (a))
	((queue-empty que) result nil)
	((queue-length que) result 1)

	((queue-enqueue que 'b) result (b))
	((queue-all que) result (a b))
	((queue-length que) result 2)
	((queue-nth que -1) result a)
	((queue-nth que 0) result a)
	((queue-nth que 1) result b)
	((queue-nth que 2) result nil)
	((queue-nth que 3) result nil)

	((setq que2 (queue-copy que)) result (QUEUE (a b) b))
	((eq que que2) result nil)
	((equal que que2) result t)

	((queue-dequeue que) result a)
	((queue-all que) result (b))
	((queue-dequeue que) result b)
	((queue-all que) result nil)
	((queue-empty que) result t)

	((queue-enqueue que 'a) dont-care)
	((queue-enqueue que 'b) dont-care)
	((queue-clear que) dont-care) 
	((queue-p que) result t)		
	((queue-empty que) result t)		
	((queue-dequeue que) result nil)	     
	((queue-first que) result nil)
	((queue-length que) result 0)
	((queue-all que) result nil)
	))
(setq queue-m-tests queue-f-tests)


(defun test-queue-f ()
    (interactive)
    (let ((load-path '(".")))
      (require 'queue-f)
      (with-output-to-temp-buffer "*Elib-test*"
	(princ "queue-f:\n")
	(test-package queue-f-tests))))

;;
;; Use the same tests for queue-m as for queue-f
;;
(defun test-queue-m ()
    (interactive)
    (let ((load-path '(".")))
      (require 'queue-m)
      (with-output-to-temp-buffer "*Elib-test*"
	(princ "queue-m:\n")
	(test-package queue-m-tests))))


;;; ================================================================
;;;         Tests for the string package.

(setq string-tests
      '(((string-replace-match "foo" "kallefoopelle" "bar")
	 result "kallebarpelle")
	((string-replace-match "[0-9]" "le127" "bar\\\\\\&\\\\")
	 result "lebar\\1\\27")
	((string-replace-match "f\\([^f]*\\)f\\([^f]*\\)f"
			       "abcdefghijfdddfojsan" "\\2\\1")
	 result "abcdedddghijojsan")
	((string-split "," "foo,bar,baz") result ("foo" "bar" "baz"))
	((string-split "," "foo,bar,baz" 2) result ("foo" "bar"))))
	
	 
(defun test-string ()
    (interactive)
    (let ((load-path '(".")))
      (require 'string)
      (with-output-to-temp-buffer "*Elib-test*"
	(princ "string:\n")
	(test-package string-tests))))

;;; ================================================================
;;;         Tests for the doubly linked list.

(setq dll-tests
      '(
	((setq a (dll-create)) dont-care)
	((dll-p a) result t)
	((dll-p nil) result nil)
	((setq b (dll-create-from-list (list 'a 'b 'c))) dont-care)
	((setq c (dll-copy b)) dont-care)
	((dll-enter-first c 'f) dont-care)
	((dll-enter-last c 'l) dont-care)
	((setq n (dll-nth c 2)) dont-care)
	((dll-element c n) result b)
	((dll-enter-after c n 'after) dont-care)
	((dll-element c n) result b)
	((dll-enter-before c n 'before) dont-care)
	((dll-element c n) result b)
	((dll-first a) result nil)
	((dll-first b) result a)
	((dll-first c) result f)
	((dll-empty b) result nil)
	((dll-last a) result nil)
	((dll-last b) result c)
	((dll-last c) result l)
	((dll-element c n) result b)
	((dll-all c) result (f a before b after c l))
	((dll-element c (dll-next c n)) result after)
	((dll-element c (dll-previous c n)) result before)
	((dll-next b (dll-nth b 2)) result nil)
	((dll-next b (dll-nth b -1)) result nil)
	((dll-previous c (dll-nth c 0)) result nil)
	((dll-all a) result nil)
	((dll-all b) result (a b c))
	((dll-all c) result (f a before b after c l))
	((dll-delete c n) result b)
	((dll-all c) result (f a before after c l))
	((setq d (dll-copy c)) dont-care)
	((dll-delete-first b) result a)
	((dll-all b) result (b c))
	((dll-delete-last b) result c)
	((dll-all b) result (b))
	((dll-delete-last b) result b)
	((dll-all b) result nil)
	((dll-clear c) dont-care)
	((dll-all c) result nil)
	((dll-clear c) dont-care)
	((dll-all c) result nil)
	((dll-p n) result nil)
	((dll-p a) result t)
	((dll-p b) result t)
	((dll-empty a) result t)
	((dll-empty b) result t)
	((dll-all d) result (f a before after c l))
	((dll-previous d (dll-nth d 0)) result nil)
	((let (xxx)
	   (dll-map-reverse (function (lambda (x) (setq xxx (cons x xxx)))) d)
	   xxx) result (f a before after c l))
	((let (xxx)
	   (dll-map (function (lambda (x) (setq xxx (cons x xxx)))) d)
	   xxx) result (l c after before a f))
	((dll-filter d (function (lambda (x)
				   (or (equal x 'after)
				       (equal x 'before))))) dont-care)
	((dll-all d) result (before after))
	((dll-length a) result 0)
	((dll-length d) result 2)
	((setq a (dll-create-from-list (list 7 2 843 2 8 19 7 289))) dont-care)
	((dll-sort a (function <)) dont-care)
	((dll-all a) result (2 2 7 7 8 19 289 843))
	))

(setq dll-debug-tests dll-tests)

(defun test-dll ()
    (interactive)
    (let ((load-path '(".")))
      (load-library "dll")
      (with-output-to-temp-buffer "*Elib-test*"
	(princ "dll:\n")
	(test-package dll-tests))))

(defun test-dll-debug ()
    (interactive)
    (let ((load-path '(".")))
      (load-library "dll-debug")
      (with-output-to-temp-buffer "*Elib-test*"
	(princ "dll-debug:\n")
	(test-package dll-debug-tests))))

;;; ================================================================
;;;         Tests for the cookie mode.

(setq cookie-tests
      '(
	((kill-buffer (get-buffer-create "*Cookie-buffer*")) dont-care)
	((setq a (collection-create "*Cookie-buffer*"
				    (function insert)
				    "Header" "Footer"))
	 dont-care)
	((collection-empty a) result t)
	((collection-length a) result 0)
	((collection-list-cookies a) result nil)
	((cookie-enter-first a "1") dont-care)
	((cookie-enter-last a "2") dont-care)
	((collection-append-cookies a '("3" "4")) dont-care)
	((collection-empty a) result nil)
	((collection-length a) result 4)
	((collection-list-cookies a) result ("1" "2" "3" "4"))
	((eq (collection-buffer a) (get-buffer "*Cookie-buffer*"))
	 result t)
	((save-window-excursion
	   (set-buffer (collection-buffer a))
	   (buffer-string))
	 result "Header\n1\n2\n3\n4\nFooter\n")
      ((eq (tin-locate a 1) (tin-nth a 0)) result t)
      ((eq (tin-locate a 2) (tin-nth a 0)) result t)
      ((eq (tin-locate a 7) (tin-nth a 0)) result t)
      ((eq (tin-locate a 8) (tin-nth a 0)) result t)
      ((eq (tin-locate a 9) (tin-nth a 0)) result t)
      ((eq (tin-locate a 9) (tin-nth a 1)) result nil)
      ((eq (tin-locate a 10) (tin-nth a 0)) result nil)
      ((eq (tin-locate a 10) (tin-nth a 1)) result t)
      ((eq (tin-locate a 12) (tin-nth a 2)) result t)
      ((eq (tin-locate a 14) (tin-nth a 3)) result t)
      ((eq (tin-locate a 16) (tin-nth a 3)) result t)
      ((eq (tin-locate a 18) (tin-nth a 3)) result t)

      ;; Test that last-tin is reset when collection-filter-cookies is called.
      ((tin-goto a (tin-locate a 12)) dont-care)
      ((collection-filter-cookies
	a (function (lambda (x) (not (string= x "3"))))) dont-care)
      ((collection-list-cookies a) result ("1" "2" "4"))
      ((tin-goto-previous a 13 1) dont-care)
      
))

(defun test-cookie ()
    (interactive)
    (save-some-buffers)
    (let ((load-path '(".")))
      (load-library "cookie")
      (with-output-to-temp-buffer "*Elib-test*"
	(princ "cookie:\n")
	(test-package cookie-tests))))


;;; ================================================================
;;;            Functions for testing a binary tree.
;;;            (not yet working)


(require 'bintree)


(defun bintree-print (tree)
  "Print the binary tree TREE on stdout."
  (princ "\n")
  (do-bintree-print (elib-bintree-root tree) 0)
  nil)


(defun do-bintree-print (root level)
  "Print the binary tre starting with the root node ROOT at level LEVEL."
  (if (null root)
      (princ "empty tree")
    (if (elib-node-left root)
	(do-bintree-print (elib-node-left root) (1+ level)))
    (let ((lvl level))
      (while (> lvl 0)
	(princ "   ")
	(setq lvl (1- lvl))))
    (princ (elib-node-data root))
    (princ "\n")
    (if (elib-node-right root)
	(do-bintree-print (elib-node-right root) (1+ level)))))


(defun bintree-enter-list (tree list)
  "Enter into TREE all elements of LIST."
  (mapcar (function (lambda (x) (elib-bintree-enter tree x)))
	  list))


;;; ================================================================
;;;            Functions for testing an AVL tree.
;;;            (not yet working)


(require 'avltree)


(defun avltree-print (tree)
  "Print the binary tree TREE on stdout."
  (princ "\n")
  (do-avltree-print (elib-avl-root tree) 0)
  nil)


(defun do-avltree-print (root level)
  "Print the binary tre starting with the root node ROOT at level LEVEL."
  (if (null root)
      (princ "empty tree")
    (if (elib-node-left root)
	(do-avltree-print (elib-node-left root) (1+ level)))
    (let ((lvl level))
      (while (> lvl 0)
	(princ "   ")
	(setq lvl (1- lvl))))
    (princ (elib-node-data root))
    (princ " ")
    (princ (elib-avl-node-balance root))
    (princ "\n")
    (if (elib-node-right root)
	(do-avltree-print (elib-node-right root) (1+ level)))))


(defun avltree-enter-list (tree list)
  "Enter into TREE all elements of LIST."
  (mapcar (function (lambda (x) (elib-avltree-enter tree x)))
	  list))


;;; ================================================================
;;;                      Test the entire Elib.


;; Note that some tests are commented away.  That is because they
;; are not yet written.


(setq elib-packages
      '("stack-f"
	"stack-m"
	"queue-f"
	"queue-m"
	"dll-debug"
	;; Check cookie immediately after dll-debug, so that we can
	;; see the output.
	"cookie"
	"dll"
	;; Check cookie again, in case dll-debug behaves differently.
	"cookie"
;;;	"elib-node"
;;;	"bintree"
;;;	"avltree"
	"string"
;;;	"read"
	))
	

(defun elib-test-all ()
  "Test all packages within ELIB for errors."
  (interactive)
  (let ((load-path '(".")))
    (with-output-to-temp-buffer "*Elib-test*"
      (mapcar (function
	       (lambda (package-name)
		 (load-library package-name)
		 (princ (format "%s:\n" package-name))
		 (test-package (eval (intern (concat package-name 
						     "-tests"))))))
	      elib-packages))))

;;; elib-test.el ends here
