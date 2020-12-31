;;; algorithm.el --- AUCTeX style for the (LaTeX) algorithm package

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Uwe Brauer <oub@mat.ucm.es>
;; Created: 2020-01-26
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:
;; This file adds support for the algorithm package.

;;; Code:

(defvar LaTeX-algorithm-package-options
  '("section")
  "Package options for the algorithm package.")


(defun LaTeX-env-algorithm (environment)
  "Insert a algorithm-like ENVIRONMENT with caption and label."
  (let* ((pos (completing-read (TeX-argument-prompt t nil "Position")
			       '(("H"))))
	 (caption (TeX-read-string "Caption: "))
	 (short-caption (when (>= (length caption) LaTeX-short-caption-prompt-length)
			  (TeX-read-string "(Optional) Short caption: "))))
    (LaTeX-insert-environment environment
			      (concat
			       (unless (zerop (length pos))
				 (concat LaTeX-optop pos LaTeX-optcl))))
    ;; top caption -- do nothing if user skips caption
    (unless (zerop (length caption))
      ;; insert `\caption[short-caption]{caption':
      (insert TeX-esc "caption")
      (when (and short-caption (not (string= short-caption "")))
	(insert LaTeX-optop short-caption LaTeX-optcl))
      (insert TeX-grop caption)
      ;; ask for a label and insert it
;      (LaTeX-label environment 'environment)
      ;; the longtable `\caption' is equivalent to a
      ;; `\multicolumn', so it needs a `\\' at the
      ;; end of the line.  Prior to that, add } to
      ;; close `\caption{'
      (insert TeX-grcl "")
      ;; fill the caption
      (LaTeX-fill-paragraph)
      ;; Insert a new line and indent
      (LaTeX-newline)
      (LaTeX-label environment 'environment)
      (LaTeX-newline)
      (indent-according-to-mode))))


(TeX-add-style-hook
 "algorithm"
 (lambda ()
   (LaTeX-add-environments
    '("algorithm"  LaTeX-env-algorithm ))
   (TeX-add-symbols
    '("listofalgorithms" 0))
      LaTeX-dialect))


;;; algorithm.el ends here
