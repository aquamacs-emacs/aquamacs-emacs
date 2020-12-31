;;; overpic.el --- AUCTeX style for `overpic.sty' (v1.3)

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2020-02-23
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

;; This file adds support for `overpic.sty' (v1.3) from 2020/02/22.
;; `overpic.sty' is part of TeXLive.

;;; Code:

;; Silence the compiler
(declare-function font-latex-add-keywords
		  "font-latex"
		  (keywords class))
(defvar LaTeX-graphicx-key-val-options)
(defvar LaTeX-graphicx-package-options)

(defvar LaTeX-overpic-key-val-options
  '(("abs"     ("true" "false"))
    ("percent" ("true" "false"))
    ("permil"  ("true" "false"))
    ("rel")
    ("grid"    ("true" "false"))
    ("tics")
    ("unit"))
  "Key=value options for overpic macro and environments.")

(defun LaTeX-arg-overpic-key-val (optional)
  "Insert key-val for optional argument of overpic environments.
If OPTIONAL is non-nil, insert argument in square brackets.

This function is an variation of
`LaTeX-arg-graphicx-includegraphics-key-val' where the key-val's
in `LaTeX-overpic-key-val-options' are offered in addition to the
ones provided by `LaTeX-graphicx-key-val-options'."
  (let ((crm-local-completion-map
	 (remove (assoc 32 crm-local-completion-map)
		 crm-local-completion-map))
	(minibuffer-local-completion-map
	 (remove (assoc 32 minibuffer-local-completion-map)
		 minibuffer-local-completion-map)))
    (TeX-argument-insert
     (TeX-read-key-val optional
		       (if (and (or (and (eq TeX-engine 'default)
					 (not (TeX-PDF-from-DVI)))
				    (eq TeX-engine 'luatex))
				TeX-PDF-mode)
			   (append '(("page")
				     ("pagebox" ("mediabox"
						 "cropbox"
						 "bleedbox"
						 "trimbox"
						 "artbox")))
				   LaTeX-overpic-key-val-options
				   LaTeX-graphicx-key-val-options)
			 (append
			  LaTeX-overpic-key-val-options
			  LaTeX-graphicx-key-val-options)))
     optional)))


(TeX-add-style-hook
 "overpic"
 (lambda ()

   ;; overpic.sty loads graphicx.sty
   (TeX-run-style-hooks "graphicx")

   (TeX-add-symbols
    '("setOverpic" (TeX-arg-key-val LaTeX-overpic-key-val-options)))

   (LaTeX-add-environments
    '("overpic" LaTeX-env-args
      [ LaTeX-arg-overpic-key-val ]
      LaTeX-arg-includegraphics)

    '("Overpic" LaTeX-env-args
      [ LaTeX-arg-overpic-key-val ]
      (TeX-arg-literal "{" "}")))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("setOverpic" "{"))
			      'function)))
 LaTeX-dialect)

(defvar LaTeX-overpic-package-options
  (progn
    (TeX-load-style "graphicx")
    (append
     LaTeX-graphicx-package-options
     '("abs"
       "percent"
       "permil")))
  "Package options for the overpic package.")

;;; overpic.el ends here
