;;; highlight-indentation.el --- Function for highlighting indentation
;; Original Author: Anton Johansson <anton.johansson@gmail.com> - http://antonj.se
;; Created: Dec 15 23:42:04 2010
;; URL: https://github.com/antonj/Highlight-Indentation-for-Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;;; Commentary:
;; Customize `highlight-indent-face' to suit your theme.

;;; Code:

(defcustom highlight-indentation  nil
 "If level of indentation should be displayed at start.
Toggle buffer local status via `M-x highlight-indentation' during session. "

:type 'boolean
:group 'python)
(make-variable-buffer-local 'highlight-indentation)

(defvar highlight-indent-active nil)
(make-variable-buffer-local 'highlight-indent-active)

(defface highlight-indent-face
  '((((class color) (background dark))
     (:background "grey33"))
    (((class color) (background light))
     (:background "grey")))
  "Basic face for highlighting indentation guides.")

(setq-default highlight-indent-offset 4)

(defvar ruby-indent-level nil)
;; lp:1067928
;; (defvar nxml-child-indent nil)

(defun highlight-indentation-on ()
  "Make sure `highlight-indentation' is on. "
  (interactive)
  (set (make-local-variable 'highlight-indent-active) nil)
  (highlight-indentation)
  (when (called-interactively-p 'any)
    (message "highlight-indentation ON")))

(defun highlight-indentation-off ()
  "Make sure `highlight-indentation' is off. "
  (interactive)
  (set (make-local-variable 'highlight-indent-active) t)
  (highlight-indentation)
  (when (called-interactively-p 'any)
    (message "highlight-indentation OFF")))

(defun highlight-indentation (&optional indent-width)
  "Toggle highlight indentation.
Optional argument INDENT-WIDTH specifies which indentation
level (spaces only) should be highlighted, if omitted
indent-width will be guessed from current major-mode"
  (interactive "P")
  (let ((re (format "\\( \\) \\{%s\\}" (- highlight-indent-offset 1))))
    (if (not highlight-indent-active)
        (progn ;; Toggle on
          (set (make-local-variable 'highlight-indent-offset)
               (if indent-width
                   indent-width
                 ;; Set indentation offset according to major mode
                 (cond ((and (eq major-mode 'python-mode)(boundp 'py-indent-offset)) 
                        py-indent-offset)
                       ;; support both python.el
                       ((or (eq major-mode 'python-mode)(eq major-mode 'python))
                        python-indent)
                       ((eq major-mode 'ruby-mode)
                        ruby-indent-level)
                       ((eq major-mode 'nxml-mode)
                        nxml-child-indent)
                       ((local-variable-p 'c-basic-offset)
                        c-basic-offset)
                       (t
                        (default-value 'highlight-indent-offset)))))
          (set (make-local-variable 'highlight-indent-active) t)
          (if (featurep 'xemacs)
              (font-lock-add-keywords nil `((,re (1 'paren-face-match))))
            (font-lock-add-keywords nil `((,re (1 'highlight-indent-face)))))
          (message (format "highlight-indentation with indent-width %s"
                           highlight-indent-offset)))
      ;; Toggle off
      (set (make-local-variable 'highlight-indent-active) nil)
      (if (featurep 'xemacs)
          (font-lock-remove-keywords nil `((,re (1 'paren-face-match))))
        (font-lock-remove-keywords nil `((,re (1 'highlight-indent-face)))))
      (message "highlight-indentation OFF"))
    (font-lock-fontify-buffer)))

(provide 'highlight-indentation)

;;; highlight-indentation.el ends here
