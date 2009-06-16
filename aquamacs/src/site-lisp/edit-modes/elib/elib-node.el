;;;; $Id: elib-node.el,v 1.1 2006/12/01 23:31:41 davidswelt Exp $
;;;; Nodes used in binary trees and doubly linked lists.

;; Copyright (C) 1991-1995 Free Software Foundation

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;;	Inge Wallin <inge@lysator.liu.se>
;; Maintainer: elib-maintainers@lysator.liu.se
;; Created: 20 May 1991
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
;;;; Author: Inge Wallin
;;;; 

;;; Commentary:

;;; A node is implemented as an array with three elements, using
;;; (elt node 0) as the left pointer
;;; (elt node 1) as the right pointer
;;; (elt node 2) as the data
;;;
;;; Some types of trees, e.g. AVL trees, need bigger nodes, but 
;;; as long as the first three parts are the left pointer, the 
;;; right pointer and the data field, these macros can be used.
;;;

;;; Code:

(provide 'elib-node)


(defmacro elib-node-create (left right data)

  ;; Create a tree node from LEFT, RIGHT and DATA.
  (` (vector (, left) (, right) (, data))))


(defmacro elib-node-left (node)

  ;; Return the left pointer of NODE.
  (` (aref (, node) 0)))


(defmacro elib-node-right (node)

  ;; Return the right pointer of NODE.
  (` (aref (, node) 1)))


(defmacro elib-node-data (node)

  ;; Return the data of NODE.
  (` (aref (, node) 2)))


(defmacro elib-node-set-left (node newleft)

  ;; Set the left pointer of NODE to NEWLEFT.
  (` (aset (, node) 0 (, newleft))))


(defmacro elib-node-set-right (node newright)

  ;; Set the right pointer of NODE to NEWRIGHT.
  (` (aset (, node) 1 (, newright))))


(defmacro elib-node-set-data (node newdata)
  ;; Set the data of NODE to NEWDATA.
  (` (aset (, node) 2 (, newdata))))



(defmacro elib-node-branch (node branch)

  ;; Get value of a branch of a node.
  ;; 
  ;; NODE is the node, and BRANCH is the branch.
  ;; 0 for left pointer, 1 for right pointer and 2 for the data."
  (` (aref (, node) (, branch))))


(defmacro elib-node-set-branch (node branch newval)

  ;; Set value of a branch of a node.
  ;;
  ;; NODE is the node, and BRANCH is the branch.
  ;; 0 for left pointer, 1 for the right pointer and 2 for the data.
  ;; NEWVAL is new value of the branch."
  (` (aset (, node) (, branch) (, newval))))

;;; elib-node.el ends here.
