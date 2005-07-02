;;; mac-drag-N-drop.el --- an alternative to mac-drag-n-drop

;; Copyright (C) 2003-2005  Seiji Zenitani <zenitani@mac.com>

;; Author: Seiji Zenitani <zenitani@mac.com>
;; Based on: mac-win.el by Andrew Choi <akochoi@mac.com>
;; Version: v20050106
;; Keywords: tools
;; Created: 2003-04-27
;; Compatibility: Emacs 21, Mac OS X
;; URL(en): http://home.att.ne.jp/alpha/z123/elisp-e.html
;; URL(jp): http://macwiki.sourceforge.jp/cgi-bin/wiki.cgi?mac-drag-N-drop

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; This package provides `mac-drag-N-drop' function,
;; a modified version of `mac-drag-n-drop' in mac-win.el for Mac OS X.
;; By editing `mac-drag-N-drop-string-alist',
;; the user can insert pre-defined strings at point when a file is dropped.

;;; Usage:

;; (require 'mac-drag-N-drop)
;; (local-set-key [drag-n-drop] 'mac-drag-N-drop)

;; Due to improvement of `x-dnd' feature,
;; global-set-key is no-longer recommended.


;;; Code:

(defvar mac-drag-N-drop-string-alist '(
  (c++-mode . (
     ("\\.h\\'" . "#include <%f>")
     ))
  ("\\.tex\\'" . (
     ("\\.tex\\'" . "\\input{%r}\n")
     ("\\.cls\\'" . "\\documentclass{%f}\n")
     ("\\.sty\\'" . "\\usepackage{%f}\n")
     ("\\.eps\\'" . "\\includegraphics[]{%r}\n")
     ("\\.ps\\'"  . "\\includegraphics[]{%r}\n")
     ("\\.pdf\\'" . (mac-drag-N-drop-read-LEE-pdf file)) ; LaTeX Equation Editor
     ("\\.pdf\\'" . "\\includegraphics[]{%r}\n")
     ("\\.jpg\\'" . "\\includegraphics[]{%r}\n")
     ("\\.png\\'" . "\\includegraphics[]{%r}\n")
     ))
  ("\\.html?\\'" . (
     ("\\.gif\\'" . "<img src=\"%r\">\n")
     ("\\.jpg\\'" . "<img src=\"%r\">\n")
     ("\\.png\\'" . "<img src=\"%r\">\n")
     ("\\.css\\'" . "<link rel=\"stylesheet\" type=\"text/css\" href=\"%r\">\n" )
     ("\\.js\\'"  . "<script type=\"text/javascript\" src=\"%r\"></script>\n" )
     (".*" . "<a href=\"%r\">%f</a>\n")
     ))
  (shell-mode . (
     (".*" . "%F")
     ))
  (ome-smail-mode . (          ; for OME project <http://mac-ome.jp/>
     (".*" . "Attachment: %F\n")
     ))
  ))

(defvar mac-drag-N-drop-replace-alist '(
  ("%F" . file)
  ("%f" . (file-name-nondirectory file))
  ("%r" . (file-relative-name file (file-name-directory (buffer-file-name))))
  ("%n" . (file-name-sans-extension (file-name-nondirectory file)))
  ("%e" . (file-name-extension file))
  ))

(defvar mac-drag-N-drop-local-string-alist nil)
(make-variable-buffer-local 'mac-drag-N-drop-local-string-alist)


(defun mac-drag-N-drop-setup ()
  "Document forthcoming..."
  (interactive)
  (let ((alist mac-drag-N-drop-string-alist))
    (while alist
      (let ((condition (caar alist))
            (strlist (cdar alist)))
        (if (or (and (symbolp condition)
                     (equal condition major-mode) )
                (and (stringp condition)
                     (stringp (buffer-file-name))
                     (string-match condition (buffer-file-name))) )
            (progn
              (setq mac-drag-N-drop-local-string-alist strlist)
              (setq alist nil)
              ))
        )
      (setq alist (cdr alist))
      )
    ))

;; originally from `mac-drag-n-drop' in mac-win.el
(defun mac-drag-N-drop (event)
  "Document forthcoming..."
  (interactive "e")
;  (save-excursion
    ;; Make sure the drop target has positive co-ords
    ;; before setting the selected frame - otherwise it
    ;; won't work.  <skx@tardis.ed.ac.uk>
  (let* ((window (posn-window (event-start event)))
         (coords (posn-x-y (event-start event)))
         (x (car coords))
         (y (cdr coords)))
    (if (and (> x 0) (> y 0))
        (progn
          (set-frame-selected-window nil window)
          (goto-char (posn-point (event-start event)))
          (mapcar
           '(lambda (file)
              (mac-drag-N-drop-execute
               (decode-coding-string
                file
                (or file-name-coding-system
                    default-file-name-coding-system))))
           (car (cdr (cdr event)))))
      (mapcar
       '(lambda (file)
          (find-file
           (decode-coding-string
            file
            (or file-name-coding-system
                default-file-name-coding-system))))
       (car (cdr (cdr event))))
      )
    (raise-frame)
    (recenter)
    ))
  ;)


(defun mac-drag-N-drop-execute (file)
  "Document forthcoming..."
  (interactive "f")
  (mac-drag-N-drop-setup)
  (let( (alist mac-drag-N-drop-local-string-alist)
        (case-fold-search nil)
        (my-string nil)
        (not-yet t) )
    (while (and alist not-yet)
      (if (string-match (caar alist) file)
          (progn
            (setq my-string (cdar alist))
            (if (not (stringp my-string))
                (setq my-string (eval (cdar alist))))
            (if (stringp my-string)
		(insert (mac-drag-N-drop-string my-string))
	      )
            (when my-string
              (setq alist nil)
              (setq not-yet nil)
              )
            ))
      (setq alist (cdr alist))
      )
    (if not-yet (find-file file))
    ))

(defun mac-drag-N-drop-read-LEE-pdf (file)
  "read source lines from LaTeX Equation Editor's PDF file."
  (interactive "P")
  (with-temp-buffer
    (let ((case-fold-search nil)
          (fsize (elt (file-attributes file) 7)) )
      (if (> fsize (* 1024 1024)) nil
        (progn
          (insert-file-contents file)
          (replace-string "ESslash" "\\")
          (goto-char (point-min))
          (replace-string "ESleftbrack" "{")
          (goto-char (point-min))
          (replace-string "ESrightbrack" "}")
          (goto-char (point-min))
          (replace-string "ESdollar" "$")
          (goto-char (point-min))
          (and (re-search-forward "ESannot\\(.*\\)ESannotend" (point-max) t)
               (match-string-no-properties 1))
          )))))

(defun mac-drag-N-drop-string (arg)
  "Document forthcoming..."
  (interactive)
  (let ((rlist mac-drag-N-drop-replace-alist))
    (while rlist
      (while (string-match (caar rlist) arg)
        (setq arg
              (replace-match
               (eval (cdar rlist)) t nil arg)))
      (setq rlist (cdr rlist))
      )
    )
  arg)


(defun mac-drag-N-drop-read-vcf (file)
  "This code is experimental."
  (interactive "P")
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((case-fold-search nil)
	   (name
	    (and (re-search-forward "FN:\\(.*\\)" (point-max) 
				    t)
		 (match-string-no-properties 1)))
	   (mail
	    (and (re-search-forward "EMAIL;.*type=pref:\\(.*\\)" (point-max) 
				    t)
		 (match-string-no-properties 1))) )
      
      (concat name " <" mail ">")
      )))

(provide 'mac-drag-N-drop)

;; mac-drag-N-drop.el ends here