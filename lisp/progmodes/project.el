;;; project.el --- Operations on the current project  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains generic infrastructure for dealing with
;; projects, and a number of public functions: finding the current
;; root, source directories, related directories, etc.

;;; Code:

(require 'cl-generic)

(defvar project-find-functions (list #'project-try-vc
                                     #'project-try-ede
                                     #'project-ask-user)
  "Special hook to find the project containing a given directory.
Each functions on this hook is called in turn with one
argument (the directory) and should return either nil to mean
that it is not applicable, or a project instance.")

;;;###autoload
(defun project-current (&optional dir)
  "Return the project instance in DIR or `default-directory'."
  (unless dir (setq dir default-directory))
  (run-hook-with-args-until-success 'project-find-functions dir))

(cl-defgeneric project-root (project)
  "Return the root directory of the current project.
The directory name should be absolute.")

(cl-defgeneric project-source-directories (project)
  "Return the list of source directories.
Including any where source (or header, etc) files used by the
current project may be found.  Including those outside of the
project tree.  The directory names should be absolute."
  (project-directories project))

(cl-defgeneric project-directories (project)
  "Return the list of directories related to the current project.
It should include the current project root, then possibly the
roots of any currently open related projects (if they're meant to
be edited together).  The directory names should be absolute."
  (list (project-root project)))

(defun project-try-vc (dir)
  (let* ((backend (vc-responsible-backend dir))
         (root (and backend (ignore-errors
                              (vc-call-backend backend 'root dir)))))
    (and root (cons 'vc root))))

(cl-defmethod project-root ((project (head vc)))
  (cdr project))

(declare-function ede-directory-get-open-project "ede")
(declare-function ede-project-root "ede")

(defun project-try-ede (dir)
  (when (featurep 'ede)
    (let ((project-dir
           (locate-dominating-file
            dir
            (lambda (dir)
              (ede-directory-get-open-project dir 'ROOT)))))
      (when project-dir
        (cons 'ede
              (ede-directory-get-open-project project-dir 'ROOT))))))

(defun project-ask-user (dir)
  (cons 'user (read-directory-name "Project root: " dir nil t)))

(cl-defmethod project-root ((project (head ede)))
  (ede-project-root-directory (cdr project)))

(provide 'project)
;;; project.el ends here
