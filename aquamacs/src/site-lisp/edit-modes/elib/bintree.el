;;;; $Id: bintree.el,v 1.1 2006/12/01 23:31:41 davidswelt Exp $
;;; This file implements binary trees.

;; Copyright (C) 1991-1995 Free Software Foundation

;; Author: Inge Wallin <inge@lysator.liu.se>
;; Maintainer: elib-maintainers@lysator.liu.se
;; Created: 21 May 1991
;; Keywords: extensions, lisp

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
;;;; Author:  Inge Wallin
;;;;

;;; Commentary:

;;;
;;; A binary tree consists of two cons cells, the first one holding
;;; the tag 'BINTREE in the car cell and the second one having
;;; the tree in the car and the compare function in the cdr cell. The
;;; tree has a dummy node as its root with the real tree in the left
;;; pointer.  The compare function must take two arguments of the type
;;; which is to be stored in the tree and must return non-nil if
;;; the first argument is "less than" the second argument and nil 
;;; otherwise.
;;;
;;; For example, use
;;;    (bintree-create '<)
;;; if the tree is going to store integers.
;;; 
;;;
;;; This package uses the macros in the file elib-node.el and
;;; a stack from stack.el.
;;;

;;; Code:

(require 'elib-node)
(require 'stack-m)

(provide 'bintree)


;;; ================================================================
;;;      Internal functions for use in the binary tree package


(defmacro elib-bintree-root (tree)

  ;; Return the root node for a binary tree.  INTERNAL USE ONLY.
  (` (elib-node-left (car (cdr (, tree))))))


(defmacro elib-bintree-dummyroot (tree)

  ;; Return the dummy node of a binary tree.  INTERNAL USE ONLY.
  (` (car (cdr (, tree)))))


(defmacro elib-bintree-cmpfun (tree)

  ;; Return the compare function of binary tree TREE.  INTERNAL USE ONLY."
  (` (cdr (cdr (, tree)))))



(defun elib-bintree-mapc (map-function root)

  ;; Apply MAP-FUNCTION to all nodes in the tree starting with ROOT.
  ;; The function is applied in-order.
  ;;
  ;; Note: MAP-FUNCTION is applied to the node and not to the data itself.
  ;;
  ;; INTERNAL USE ONLY."

  (let ((node root)
	(stack (elib-stack-create))
	(go-left t))
    (elib-stack-push stack nil)
    (while node
      (if (and go-left
	       (elib-node-left node))
	  (progn				   ; Do the left subtree first.
	    (elib-stack-push stack node)
	    (setq node (elib-node-left node)))
	(funcall map-function node)		   ; Apply the function...
	(if (elib-node-right node)		   ; and do the right subtree.
	    (setq node (elib-node-right node)
		  go-left t)
	  (setq node (elib-stack-pop stack)
		go-left nil))))))


(defun elib-bintree-do-copy (root)

  ;; Copy the tree with ROOT as root.  Highly recursive. INTERNAL USE ONLY.
  (if (null root) 
      nil
    (elib-node-create (elib-bintree-do-copy (elib-node-left root))
		      (elib-bintree-do-copy (elib-node-right root))
		      (elib-node-data root))))


;;; ================================================================
;;;       The public functions which operate on binary trees.


(defun bintree-create (compare-function)
  "Create an empty binary tree using COMPARE-FUNCTION as the compare function.
COMPARE-FUNCTION is a function which takes two arguments, A and B, and 
returns non-nil if A is less than B, and nil otherwise."
  
  (cons 'BINTREE
	(cons (elib-node-create nil nil nil)
	      compare-function)))



(defun bintree-p (obj)
  "return t if OBJ is a binary tree, nil otherwise."
  (eq (car-safe obj) 'BINTREE))



(defun bintree-compare-function (tree)
  "Return the comparision function for the binary tree TREE."
  (elib-bintree-cmpfun tree))



(defun bintree-empty (tree)
  "Return t if the binary tree TREE is empty, otherwise return nil."
  (null (elib-bintree-root tree)))



(defun bintree-enter (tree data)
  "In the binary tree TREE, insert DATA."

  (let ((cmpfun (elib-bintree-cmpfun tree))
	(node (elib-bintree-dummyroot tree))
	(new-node (elib-node-create nil nil data)))
    (if (null (elib-node-left node))
	(elib-node-set-left node new-node)
      (setq node (elib-node-left node))
      (while node
	(cond
	 ((funcall cmpfun data (elib-node-data node))
	  (if (elib-node-left node)
	      (setq node (elib-node-left node))
	    (elib-node-set-left node new-node)
	    (setq node nil)))

	 ((funcall cmpfun (elib-node-data node) data)
	  (if (elib-node-right node)
	      (setq node (elib-node-right node))
	    (elib-node-set-right node new-node)
	    (setq node nil)))

	 (t
	  (elib-node-set-data node data)
	  (setq node nil)))))))



(defun bintree-delete (tree data)
  "From the binary tree TREE, delete DATA.
Return the element in TREE which matched DATA, or nil if no element matched."

  (let* ((cmpfun (elib-bintree-cmpfun tree))
	 (upper-node (elib-bintree-dummyroot tree)) ; Start with the dummy node
	 (branch 0)				   ; Left branch
	 (branch-node (elib-node-left upper-node))
	 node-data
	 right-node)				   ; Only used while deleting,
						   ; not while searching
    (if (null branch-node)
	nil
      (while upper-node
	(setq node-data (elib-node-data branch-node))
	(cond 
	 ((funcall cmpfun data node-data)	   ; data<node-data => go left
	  (setq upper-node branch-node
		branch-node (elib-node-left upper-node)
		branch 0))
	 
	 ((funcall cmpfun node-data data)	   ; data>node-data => go right
	  (setq upper-node branch-node
		branch-node (elib-node-right upper-node)
		branch 1))
	 
	 (t					   ; This is the node we want 
						   ; to delete.
	  (cond
	   ((null (elib-node-left branch-node))	   ; Empty left node?
	    (elib-node-set-branch upper-node branch
				  (elib-node-right branch-node)))
	   
	   ((null (elib-node-right branch-node))   ; Empty right node?
	    (elib-node-set-branch upper-node branch
				  (elib-node-left branch-node)))
	   
	   (t					   ; Both branches occupied.

	    ;; At this point `branch-node' points at the node we want
	    ;; to delete.  Both the right and the left branches are
	    ;; non-nil, so we will take the data of the rightmost node
	    ;; of the left subtree and put into `branch-node'.
	    (setq right-node branch-node
		  branch 0)
	    (while (elib-node-right (elib-node-branch right-node branch))
	      (setq right-node (elib-node-branch right-node branch)
		    branch 1))
	    (elib-node-set-data branch-node 
				(elib-node-data (elib-node-branch right-node
								  branch)))
	    (elib-node-set-branch right-node branch
				  (elib-node-left
				   (elib-node-branch right-node branch)))))
	  (setq upper-node nil)))))))



(defun bintree-member (tree data)
  "Return the element in the binary tree TREE which matches DATA.
Matching uses the compare function previously specified in `bintree-create'
when TREE was created.

If there is no such element in the tree, the value is nil."
  
  (let ((node (elib-bintree-root tree))
	(compare-function (elib-bintree-cmpfun tree))
	found)
    (while (and node 
		(not found))
      (cond
       ((funcall compare-function data (elib-node-data node))
	(setq node (elib-node-left node)))
       ((funcall compare-function (elib-node-data node) data)
	(setq node (elib-node-right node)))
       (t 
	(setq found t))))

    (if node
	(elib-node-data node)
      nil)))



(defun bintree-map (__map-function__ tree)
  "Apply MAP-FUNCTION to all elements in the binary tree TREE."

  (elib-bintree-mapc
   (function (lambda (node)
	       (elib-node-set-data node
				   (funcall __map-function__
					    (elib-node-data node)))))
   (elib-bintree-root tree)))



(defun bintree-first (tree)
  "Return the first element in the binary tree TREE, or nil if TREE is empty."

  (let ((node (elib-bintree-root tree)))
    (if node
	(progn
	  (while (elib-node-left node)
	    (setq node (elib-node-left node)))
	  (elib-node-data node))
      nil)))



(defun bintree-last (tree)
  "Return the last element in the binary tree TREE, or nil if TREE is empty."

  (let ((node (elib-bintree-root tree)))
    (if node
	(progn
	  (while (elib-node-right node)
	    (setq node (elib-node-right node)))
	  (elib-node-data node))
      nil)))



(defun bintree-copy (tree)
  "Return a copy of the binary tree TREE.

Note: This function is recursive and might result in an 
      `max eval depth exceeded' error."

  (let ((new-tree (bintree-create 
		   (elib-bintree-cmpfun tree))))
    (elib-node-set-left (elib-bintree-dummyroot new-tree)
			(elib-bintree-do-copy (elib-bintree-root tree)))
    new-tree))

  

;;
;; Not the fastest way to do this.
;;
(defun bintree-flatten (tree)
  "Return a sorted list containing all elements of the binary tree TREE."

  (nreverse 
   (let ((treelist nil))
     (elib-bintree-mapc (function (lambda (node)
				    (setq treelist (cons (elib-node-data node)
							 treelist))))
			(elib-bintree-root tree))
     treelist)))



;;
;; Not the fastest way to do this:
;;
(defun bintree-size (tree)
  "Return the number of elements in the binary tree TREE."

  (let ((treesize 0))
    (elib-bintree-mapc (function (lambda (data)
				   (setq treesize (1+ treesize))))
		       (elib-bintree-root tree))
    treesize))



(defun bintree-clear (tree)
  "Clear the binary tree TREE."

  (elib-node-set-left (elib-bintree-dummyroot tree) nil))

;;; bintree.el ends here
