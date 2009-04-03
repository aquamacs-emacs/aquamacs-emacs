;;; files+.el --- Redefinitions of some functions from `files.el'.
;;
;; Filename: files+.el
;; Description: Redefinitions of some functions from `files.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2005, Drew Adams, all rights reserved.
;; Created: Fri Aug 11 14:24:13 1995
;; Version: 21.0
;; Last-Updated: Wed Dec 07 09:32:56 2005 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 173
;; Keywords: internal, extensions, local
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Redefinitions of some functions from `files.el'.  The new
;;    definitions use `read-buffer' (not "B...") in interactive calls.
;;
;;
;;  ***** NOTE: The following functions defined in `files.el' have been
;;              REDEFINED HERE:
;;
;;  `switch-to-buffer-other-window' -
;;     1. Uses `read-buffer' (interactive).  2. defun -> defsubst.
;;  `switch-to-buffer-other-frame' -
;;     1. Uses `read-buffer' (interactive).  2. defun -> defsubst.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `files.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "files" '(require 'files+))
;;
;;  Suggested binding:
;;
;;   (global-set-key [M-f1] 'switch-to-buffer-other-window)
;;
;;  Library `files+' requires these libraries:
;;
;;    `misc-fns', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2005/05/28 dadams
;;     switch-to-buffer-other-*: Provide second arg to read-buffer.
;; 2004/09/21 dadams
;;     Updated signatures of switch-to-buffer-other-*
;;     switch-to-buffer-other-frame: Removed call to raise-frame.
;; 1999/03/17 dadams
;;     switch-to-buffer-other-frame: Removed call to raise-frame.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/01/12 dadams
;;     switch-to-buffer-other-frame: No longer raise-frame,
;;       (see my pop-to-buffer).
;; 1995/10/24 dadams
;;     Added corrected (?) version of set-auto-mode, but commented it out
;;     since original (bugged?) version is depended on in other places.
;; 1995/08/11  12:40:30  dadams
;;     interactive "B..." -> use read-buffer instead.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

 ;; Cannot do (require 'files), because `files.el' does no `provide'.
 ;; Don't want to do a (load-library "files") either, because it wouldn't
 ;; allow doing (eval-after-load "files" '(progn (require 'files+)))

(require 'strings nil t) ;; (no error if not found) read-buffer
(require 'misc-fns nil t) ;; (no error if not found) another-buffer

;;;;;;;;;;;;;;;;;;;;;


;; REPLACES ORIGINAL in `files.el':
;; 1. Uses `read-buffer' (not "B...") in the interactive spec.
;; 2. defun -> defsubst.
(defsubst switch-to-buffer-other-window (buffer &optional norecord)
  "Select buffer BUFFER in another window.
Optional second arg NORECORD non-nil means
do not put this buffer at the front of the list of recently selected ones."
  (interactive
   (list (read-buffer "Switch to buffer in other window: "
                      (if (fboundp 'another-buffer) ; Defined in `misc-fns.el'.
                          (another-buffer nil t)
                        (other-buffer (current-buffer))))
         nil))
  (let ((pop-up-windows t)) (pop-to-buffer buffer t norecord)))


;; REPLACES ORIGINAL in `files.el':
;; 1. Uses `read-buffer' (not "B...") in the interactive spec.
;; 2. defun -> defsubst.
(defsubst switch-to-buffer-other-frame (buffer &optional norecord)
  "Switch to buffer BUFFER in another frame.
Optional second arg NORECORD non-nil means
do not put this buffer at the front of the list of recently selected ones."
  (interactive (list (read-buffer "Switch to buffer in other frame: "
                                  (if (fboundp 'another-buffer)
                                      (another-buffer nil t)
                                    (other-buffer (current-buffer))))))
  (let ((pop-up-frames t)) (pop-to-buffer buffer t norecord)))


;;; `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
;; 1995 -
;; D. Adams: It seems to me that the original `set-auto-mode' is bugged.
;; It doesn't seem to correspond to the doc string of
;; `auto-mode-alist': One should be able to use (REGEXP FN), but it
;; appears that (REGEXP FN FN) is needed instead.  I would correct it
;; as follows, but there appear to be other programs (e.g.
;; `jka-compr.el') that now require the form (REGEXP FN FN), or
;; (REGEXP nil FN).

;; Anyway, here is my correction (commented out for now):
;; The original has (nth 2 alist), below, instead of (nth 1 alist).

;;(defun set-auto-mode ()
;;  "Select major mode appropriate for current buffer.
;;This checks for a -*- mode tag in the buffer's text, compares the filename
;;against the entries in `auto-mode-alist', or checks the interpreter that
;;runs this file against `interpreter-mode-alist'.

;;It does not check for the `mode:' local variable in the
;;Local Variables section of the file; for that, use `hack-local-variables'.

;;If `enable-local-variables' is nil, this function does not check for a
;;-*- mode tag."
;;  ;; Look for -*-MODENAME-*- or -*- ... mode: MODENAME; ... -*-
;;  (let (beg end done)
;;    (save-excursion
;;      (goto-char (point-min))
;;      (skip-chars-forward " \t\n")
;;      (and enable-local-variables
;;         ;; Don't look for -*- if this file name matches any
;;         ;; of the regexps in inhibit-first-line-modes-regexps.
;;         (let ((temp inhibit-first-line-modes-regexps))
;;           (while (and temp
;;                       (not (string-match (car temp)
;;                                          buffer-file-name)))
;;             (setq temp (cdr temp)))
;;           (not temp))
;;         (search-forward "-*-" (save-excursion
;;                                 ;; If the file begins with "#!"
;;                                 ;; (exec interpreter magic), look
;;                                 ;; for mode frobs in the first two
;;                                 ;; lines.  You cannot necessarily
;;                                 ;; put them in the first line of
;;                                 ;; such a file without screwing up
;;                                 ;; the interpreter invocation.
;;                                 (end-of-line (and (looking-at "^#!") 2))
;;                                 (point)) t)
;;         (progn
;;           (skip-chars-forward " \t")
;;           (setq beg (point))
;;           (search-forward "-*-"
;;                           (save-excursion (end-of-line) (point))
;;                           t))
;;         (progn
;;           (forward-char -3)
;;           (skip-chars-backward " \t")
;;           (setq end (point))
;;           (goto-char beg)
;;           (if (save-excursion (search-forward ":" end t))
;;               ;; Find all specifications for the `mode:' variable
;;               ;; and execute them left to right.
;;               (while (let ((case-fold-search t))
;;                        (search-forward "mode:" end t))
;;                 (skip-chars-forward " \t")
;;                 (setq beg (point))
;;                 (if (search-forward ";" end t)
;;                     (forward-char -1)
;;                   (goto-char end))
;;                 (skip-chars-backward " \t")
;;                 (funcall (intern (concat (downcase
;;                                           (buffer-substring beg (point)))
;;                                          "-mode"))))
;;             ;; Simple -*-MODE-*- case.
;;             (funcall (intern (concat (downcase (buffer-substring beg end))
;;                                      "-mode"))))
;;           (setq done t)))
;;      ;; If we didn't find a mode from a -*- line, try using the file name.
;;      (if (and (not done) buffer-file-name)
;;        (let ((name buffer-file-name)
;;              (keep-going t))
;;          ;; Remove backup-suffixes from file name.
;;          (setq name (file-name-sans-versions name))
;;          (while keep-going
;;            (setq keep-going nil)
;;            (let ((alist auto-mode-alist)
;;                  (mode nil))
;;              ;; Find first matching alist entry.
;;              (let ((case-fold-search (eq system-type 'vax-vms)))
;;                (while (and (not mode) alist)
;;                  (if (string-match (car (car alist)) name)
;;                      (if (and (consp (cdr (car alist)))
;;                               (nth 1 (car alist)))
;;                          (progn
;;                            (setq mode (car (cdr (car alist)))
;;                                  name (substring name 0 (match-beginning 0))
;;                                  keep-going t))
;;                        (setq mode (cdr (car alist))
;;                              keep-going nil)))
;;                  (setq alist (cdr alist))))
;;              (if mode
;;                  (funcall mode)
;;                ;; If we can't deduce a mode from the file name,
;;                ;; look for an interpreter specified in the first line.
;;                (let ((interpreter
;;                       (save-excursion
;;                         (goto-char (point-min))
;;                         (if (looking-at "#! *\\([^ \t\n]+\\)")
;;                             (buffer-substring (match-beginning 1)
;;                                               (match-end 1))
;;                           "")))
;;                      elt)
;;                  ;; Map interpreter name to a mode.
;;                  (setq elt (assoc (file-name-nondirectory interpreter)
;;                                   interpreter-mode-alist))
;;                  (if elt
;;                      (funcall (cdr elt))))))))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;

(provide 'files+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; files+.el ends here
;;; Aquamacs-Update: http://www.emacswiki.org/cgi-bin/wiki/files+.el/download/files+.el

;;; files+.el --- Redefinitions of some functions from `files.el'.
;;
;; Filename: files+.el
;; Description: Redefinitions of some functions from `files.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2005, Drew Adams, all rights reserved.
;; Created: Fri Aug 11 14:24:13 1995
;; Version: 21.0
;; Last-Updated: Wed Dec 07 09:32:56 2005 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 173
;; Keywords: internal, extensions, local
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Redefinitions of some functions from `files.el'.  The new
;;    definitions use `read-buffer' (not "B...") in interactive calls.
;;
;;
;;  ***** NOTE: The following functions defined in `files.el' have been
;;              REDEFINED HERE:
;;
;;  `switch-to-buffer-other-window' -
;;     1. Uses `read-buffer' (interactive).  2. defun -> defsubst.
;;  `switch-to-buffer-other-frame' -
;;     1. Uses `read-buffer' (interactive).  2. defun -> defsubst.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `files.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "files" '(require 'files+))
;;
;;  Suggested binding:
;;
;;   (global-set-key [M-f1] 'switch-to-buffer-other-window)
;;
;;  Library `files+' requires these libraries:
;;
;;    `misc-fns', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2005/05/28 dadams
;;     switch-to-buffer-other-*: Provide second arg to read-buffer.
;; 2004/09/21 dadams
;;     Updated signatures of switch-to-buffer-other-*
;;     switch-to-buffer-other-frame: Removed call to raise-frame.
;; 1999/03/17 dadams
;;     switch-to-buffer-other-frame: Removed call to raise-frame.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/01/12 dadams
;;     switch-to-buffer-other-frame: No longer raise-frame,
;;       (see my pop-to-buffer).
;; 1995/10/24 dadams
;;     Added corrected (?) version of set-auto-mode, but commented it out
;;     since original (bugged?) version is depended on in other places.
;; 1995/08/11  12:40:30  dadams
;;     interactive "B..." -> use read-buffer instead.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

 ;; Cannot do (require 'files), because `files.el' does no `provide'.
 ;; Don't want to do a (load-library "files") either, because it wouldn't
 ;; allow doing (eval-after-load "files" '(progn (require 'files+)))

(require 'strings nil t) ;; (no error if not found) read-buffer
(require 'misc-fns nil t) ;; (no error if not found) another-buffer

;;;;;;;;;;;;;;;;;;;;;


;; REPLACES ORIGINAL in `files.el':
;; 1. Uses `read-buffer' (not "B...") in the interactive spec.
;; 2. defun -> defsubst.
(defsubst switch-to-buffer-other-window (buffer &optional norecord)
  "Select buffer BUFFER in another window.
Optional second arg NORECORD non-nil means
do not put this buffer at the front of the list of recently selected ones."
  (interactive
   (list (read-buffer "Switch to buffer in other window: "
                      (if (fboundp 'another-buffer) ; Defined in `misc-fns.el'.
                          (another-buffer nil t)
                        (other-buffer (current-buffer))))
         nil))
  (let ((pop-up-windows t)) (pop-to-buffer buffer t norecord)))


;; REPLACES ORIGINAL in `files.el':
;; 1. Uses `read-buffer' (not "B...") in the interactive spec.
;; 2. defun -> defsubst.
(defsubst switch-to-buffer-other-frame (buffer &optional norecord)
  "Switch to buffer BUFFER in another frame.
Optional second arg NORECORD non-nil means
do not put this buffer at the front of the list of recently selected ones."
  (interactive (list (read-buffer "Switch to buffer in other frame: "
                                  (if (fboundp 'another-buffer)
                                      (another-buffer nil t)
                                    (other-buffer (current-buffer))))))
  (let ((pop-up-frames t)) (pop-to-buffer buffer t norecord)))


;;; `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
;; 1995 -
;; D. Adams: It seems to me that the original `set-auto-mode' is bugged.
;; It doesn't seem to correspond to the doc string of
;; `auto-mode-alist': One should be able to use (REGEXP FN), but it
;; appears that (REGEXP FN FN) is needed instead.  I would correct it
;; as follows, but there appear to be other programs (e.g.
;; `jka-compr.el') that now require the form (REGEXP FN FN), or
;; (REGEXP nil FN).

;; Anyway, here is my correction (commented out for now):
;; The original has (nth 2 alist), below, instead of (nth 1 alist).

;;(defun set-auto-mode ()
;;  "Select major mode appropriate for current buffer.
;;This checks for a -*- mode tag in the buffer's text, compares the filename
;;against the entries in `auto-mode-alist', or checks the interpreter that
;;runs this file against `interpreter-mode-alist'.

;;It does not check for the `mode:' local variable in the
;;Local Variables section of the file; for that, use `hack-local-variables'.

;;If `enable-local-variables' is nil, this function does not check for a
;;-*- mode tag."
;;  ;; Look for -*-MODENAME-*- or -*- ... mode: MODENAME; ... -*-
;;  (let (beg end done)
;;    (save-excursion
;;      (goto-char (point-min))
;;      (skip-chars-forward " \t\n")
;;      (and enable-local-variables
;;         ;; Don't look for -*- if this file name matches any
;;         ;; of the regexps in inhibit-first-line-modes-regexps.
;;         (let ((temp inhibit-first-line-modes-regexps))
;;           (while (and temp
;;                       (not (string-match (car temp)
;;                                          buffer-file-name)))
;;             (setq temp (cdr temp)))
;;           (not temp))
;;         (search-forward "-*-" (save-excursion
;;                                 ;; If the file begins with "#!"
;;                                 ;; (exec interpreter magic), look
;;                                 ;; for mode frobs in the first two
;;                                 ;; lines.  You cannot necessarily
;;                                 ;; put them in the first line of
;;                                 ;; such a file without screwing up
;;                                 ;; the interpreter invocation.
;;                                 (end-of-line (and (looking-at "^#!") 2))
;;                                 (point)) t)
;;         (progn
;;           (skip-chars-forward " \t")
;;           (setq beg (point))
;;           (search-forward "-*-"
;;                           (save-excursion (end-of-line) (point))
;;                           t))
;;         (progn
;;           (forward-char -3)
;;           (skip-chars-backward " \t")
;;           (setq end (point))
;;           (goto-char beg)
;;           (if (save-excursion (search-forward ":" end t))
;;               ;; Find all specifications for the `mode:' variable
;;               ;; and execute them left to right.
;;               (while (let ((case-fold-search t))
;;                        (search-forward "mode:" end t))
;;                 (skip-chars-forward " \t")
;;                 (setq beg (point))
;;                 (if (search-forward ";" end t)
;;                     (forward-char -1)
;;                   (goto-char end))
;;                 (skip-chars-backward " \t")
;;                 (funcall (intern (concat (downcase
;;                                           (buffer-substring beg (point)))
;;                                          "-mode"))))
;;             ;; Simple -*-MODE-*- case.
;;             (funcall (intern (concat (downcase (buffer-substring beg end))
;;                                      "-mode"))))
;;           (setq done t)))
;;      ;; If we didn't find a mode from a -*- line, try using the file name.
;;      (if (and (not done) buffer-file-name)
;;        (let ((name buffer-file-name)
;;              (keep-going t))
;;          ;; Remove backup-suffixes from file name.
;;          (setq name (file-name-sans-versions name))
;;          (while keep-going
;;            (setq keep-going nil)
;;            (let ((alist auto-mode-alist)
;;                  (mode nil))
;;              ;; Find first matching alist entry.
;;              (let ((case-fold-search (eq system-type 'vax-vms)))
;;                (while (and (not mode) alist)
;;                  (if (string-match (car (car alist)) name)
;;                      (if (and (consp (cdr (car alist)))
;;                               (nth 1 (car alist)))
;;                          (progn
;;                            (setq mode (car (cdr (car alist)))
;;                                  name (substring name 0 (match-beginning 0))
;;                                  keep-going t))
;;                        (setq mode (cdr (car alist))
;;                              keep-going nil)))
;;                  (setq alist (cdr alist))))
;;              (if mode
;;                  (funcall mode)
;;                ;; If we can't deduce a mode from the file name,
;;                ;; look for an interpreter specified in the first line.
;;                (let ((interpreter
;;                       (save-excursion
;;                         (goto-char (point-min))
;;                         (if (looking-at "#! *\\([^ \t\n]+\\)")
;;                             (buffer-substring (match-beginning 1)
;;                                               (match-end 1))
;;                           "")))
;;                      elt)
;;                  ;; Map interpreter name to a mode.
;;                  (setq elt (assoc (file-name-nondirectory interpreter)
;;                                   interpreter-mode-alist))
;;                  (if elt
;;                      (funcall (cdr elt))))))))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;

(provide 'files+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; files+.el ends here
