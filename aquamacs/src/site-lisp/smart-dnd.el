;;; smart-dnd.el --- user-configurable drag-n-drop feature

;; Copyright (C) 2003-2006  Seiji Zenitani <zenitani@mac.com>

;; Author: Seiji Zenitani <zenitani@mac.com>
;; Based on: mac-drag-N-drop.el by Seiji Zenitani
;; Version: v20060108
;; Keywords: tools
;; Created: 2003-04-27
;; Compatibility: Emacs 22
;; URL(en): http://homepage.mac.com/zenitani/comp-e.html
;; URL(jp): http://homepage.mac.com/zenitani/elisp-j.html#smart-dnd

;; Contributors: David Reitter

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

;; This package provides user-configurable drag-n-drop feature to Emacs 22.
;;
;; Usage:
;;
;; First, evaluate `smart-dnd-setup' function in the buffer
;; by using the following alist argument.
;; The code modifies drag-n-drop behaviour in the local buffer and then
;; a string "image file: file.png" will be inserted when *.png file is dropped.
;;
;; (require 'smart-dnd)
;; (smart-dnd-setup
;;  '(
;;    ("\\.png\\'" . "image file: %f\n")
;;    ("\\.jpg\\'" . "image file: %f\n")
;;    (".exe\\'"   . (message (concat "executable: " f)))
;;    (".*"        . "any filename: %f\n")
;;    ))
;;
;; String elements will be formatted by `smart-dnd-string'.
;; You can also put elisp expression into the alist.
;; A local variable 'f' will be replaced by the dropped filename.
;;
;; Major-mode-hook would be good place to install your own configuration.
;; For example,
;;
;; html-mode:
;;
;; (add-hook
;;  'html-mode-hook
;;  (lambda ()
;;    (smart-dnd-setup
;;     '(
;;       ("\\.gif\\'" . "<img src=\"%R\">\n")
;;       ("\\.jpg\\'" . "<img src=\"%R\">\n")
;;       ("\\.png\\'" . "<img src=\"%R\">\n")
;;       ("\\.css\\'" . "<link rel=\"stylesheet\" type=\"text/css\" href=\"%R\">\n" )
;;       ("\\.js\\'"  . "<script type=\"text/javascript\" src=\"%R\"></script>\n" )
;;       (".*" . "<a href=\"%R\">%f</a>\n")
;;       ))))
;;
;; LaTeX mode:
;;
;; (add-hook
;;  'latex-mode-hook
;;  (lambda ()
;;    (smart-dnd-setup
;;     '(
;;       ("\\.tex\\'" . "\\input{%r}\n")
;;       ("\\.cls\\'" . "\\documentclass{%f}\n")
;;       ("\\.sty\\'" . "\\usepackage{%f}\n")
;;       ("\\.eps\\'" . "\\includegraphics[]{%r}\n")
;;       ("\\.ps\\'"  . "\\includegraphics[]{%r}\n")
;;       ("\\.pdf\\'" . "\\includegraphics[]{%r}\n")
;;       ("\\.jpg\\'" . "\\includegraphics[]{%r}\n")
;;       ("\\.png\\'" . "\\includegraphics[]{%r}\n")
;;       ))))
;;
;; C/C++ mode:
;; 
;; (add-hook 'c-mode-common-hook
;;           (lambda () (smart-dnd-setup '(("\\.h\\'" . "#include <%f>")))))
;;


;;; Code:

(require 'dnd)

(defvar smart-dnd-protocol-alist
  '(("^file:///" . smart-dnd-handle-local-file)
    ("^file://"  . smart-dnd-handle-file)
    ("^file:"    . smart-dnd-handle-local-file))
  "The functions to call when a file is dropped to the buffer.
See `dnd-protocol-alist' for more information."
  )

(defun smart-dnd-handle-local-file (uri action)
  "Open a local file. See also `dnd-open-local-file'."

  (let* ((f (dnd-get-local-file-name uri t)))
    (if (and f (file-readable-p f))
        (progn
          (or (smart-dnd-execute f)
              (dnd-open-local-file uri action))
          'private)
      (error "Can not read %s" uri))))

(defun smart-dnd-handle-file (uri action)
  "Handle a local or remote file."
  (let ((local-file (dnd-get-local-file-uri uri)))
    (if local-file (smart-dnd-handle-local-file local-file action)
      (error "Remote files not supported"))))

(defun smart-dnd-execute (f)
  "Execute a Drag'n'Drop action with filename F
depending on `smart-dnd-string-alist'."
  (interactive "f")
  (save-excursion
    (if (eq (car-safe last-nonmenu-event) 'drag-n-drop)
        (goto-char (posn-point (car (cdr-safe last-nonmenu-event)))))
    (let( (alist smart-dnd-string-alist)
          (case-fold-search nil)
          (my-string nil)
          (succeed nil) )
      (while alist
        (when (string-match (caar alist) f)
          (setq my-string (cdar alist))
          (when (stringp my-string)
            (insert (smart-dnd-string my-string f))
            (setq alist nil)
            (setq succeed t)
            )
          (when (not (stringp my-string))
            (eval (cdar alist))
            (setq alist nil)
            (setq succeed t)
            )
          )
        (setq alist (cdr alist))
        )
      succeed)))

(defun smart-dnd-setup (alist)
  "Install smart-dnd feature to the local buffer."
  (interactive)
  (set (make-local-variable 'dnd-protocol-alist)
       (append smart-dnd-protocol-alist dnd-protocol-alist))
  (set (make-local-variable 'smart-dnd-string-alist) alist)
  )

(defun smart-dnd-string (string filename)
  "Generate a string, based on a format STRING and the FILENAME.
You can use the following keywords in the format control STRING.
%F means absolute pathname.           [ /home/zenitani/public_html/index.html ]
%f means file name without directory. [ index.html ]
%r and %R means relative path to the FILENAME from a file in the current buffer.
                                      [ public_html/index.html ]
When the target buffer hasn't been assigned a file name yet,
%r returns the absolute pathname      [ /home/zenitani/public_html/index.html ]
while %R returns the URL.             [ file:///home/zenitani/ .. /index.html ]
%n means file name without extention. [ index ]
%e means extention of file name.      [ html ]
"
  (interactive)
  (let ((rlist
         '(
           ("%F" . f)
           ("%f" . (file-name-nondirectory f))
           ("%r" . (if buffer-file-name
                       (file-relative-name
                        f (file-name-directory buffer-file-name))
                     f))
           ("%R" . (if buffer-file-name
                       (file-relative-name
                        f (file-name-directory buffer-file-name))
                     (concat "file://" f)))
           ("%n" . (file-name-sans-extension (file-name-nondirectory f)))
           ("%e" . (or (file-name-extension f) ""))
           ))
        (f filename))
    (while rlist
      (while (string-match (caar rlist) string)
        (setq string
              (replace-match
               (eval (cdar rlist)) t nil string)))
    (setq rlist (cdr rlist))
    ))
  string)


(provide 'smart-dnd)

;; smart-dnd.el ends here

