;;; tab.el --- commands for tabs management

;; Copyright (C) 2010  Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@jurta.org>
;; Maintainer: FSF
;; Keywords: frames internal mouse

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

;; Tab functions.

;;; Code:

(defgroup tabs nil
  "Tabs."
  :group 'frames)

(defcustom tab-initial-buffer nil
  "Buffer to show in a new tab.
If the value is nil, show the current buffer from the old tab.
If the value is a string, visit the buffer with the specified buffer name.
If the value is a function, call it and switch to the buffer it returns."
  :type '(choice
	  (string   :tag "Buffer name" :value "*scratch*")
	  (function :tag "Function name")
	  (other    :tag "Current buffer" nil))
  :group 'tabs
  :version "24.1")

(defcustom tab-name nil
  "The name of the current tab to display in the tab bar."
  :type '(choice
	  (const    :tag "Buffer names of all windows" window-list)
	  (function :tag "Function name")
	  (other    :tag "Current buffer" nil))
  :group 'tabs
  :version "24.1")

(defun tab-name (&optional frame)
  (cond
   ((eq tab-name 'window-list)
    (mapconcat
     (lambda (w) (buffer-name (window-buffer w)))
     (window-list frame)
     ", "))
   ((functionp tab-name)
    (funcall tab-name frame))
   (frame
    (buffer-name (window-buffer (frame-selected-window frame))))
   (t
    (buffer-name))))

(defun tab-initial-buffer ()
  (cond
   ((stringp tab-initial-buffer)
    (switch-to-buffer (get-buffer-create tab-initial-buffer)))
   ((functionp tab-initial-buffer)
    (switch-to-buffer (funcall tab-initial-buffer)))
   (t
    )))

(defun make-tab (&optional frame parameters after)
  "Create tab with PARAMETERS, right after AFTER's existing tab.
FRAME nil or omitted means use the selected frame.
Optional argument PARAMETERS is an alist of parameters for the new frame.
Each element of PARAMETERS should have the form (NAME . VALUE), for example:

 (name . STRING)	The frame should be named STRING.

If AFTER is t or omitted, the new tab goes at the end of the tab list.

Return a newly created frame displaying the current buffer."
  (unless tab-bar-mode
    (tab-bar-mode 1))
  (let* ((tab-list (tab-list frame))
         (tab-parameters (if (assoc 'name parameters)
                             parameters
                           (append parameters `((name . ,(tab-name frame))))))
         (tab-new
          (list (tab-gensym)
                tab-parameters
                (current-window-configuration frame)
                ;; set-window-configuration does not restore the value
                ;; of point in the current buffer, so record that separately.
                (point-marker)
                nil ;; tab-history-back
                nil ;; tab-history-forward
                (delq nil (mapcar
                           (lambda (b) (and (buffer-live-p b) b))
                           (frame-parameter frame 'buffer-list)))
                (delq nil (mapcar
                           (lambda (b) (and (buffer-live-p b) b))
                           (frame-parameter frame 'buried-buffer-list)))
                )))
    ;; FIXME: use `AFTER'.  When nil, use default of custom `tab-after'.
    (modify-frame-parameters
     frame
     (list (cons 'tab-list (append tab-list (list tab-new)))))
    (tab-initial-buffer)
    (tab-bar-setup frame)
    (car tab-new)))

;;;###autoload
(defun make-tab-command (&optional frame)
  "Make a new tab, on the same frame as the selected tab.
If the terminal is a text-only terminal, this also selects the
new tab."
  (interactive)
  (select-tab (make-tab frame) frame))

(defun tab-list (&optional frame)
  "Return a list of all tabs on FRAME.
FRAME nil or omitted means use the selected frame.
Return the tab list from FRAME's `tab-list' frame parameter."
  (cdr (assoc 'tab-list (frame-parameters frame))))

(defun selected-tab (&optional frame)
  "Return the tab that is now selected on FRAME.
FRAME nil or omitted means use the selected frame."
  (cdr (assoc 'selected-tab (frame-parameters frame))))

;;;###autoload
(defun select-tab (tab &optional frame norecord)
  "Select TAB on FRAME.
FRAME nil or omitted means use the selected frame.
Subsequent editing commands apply to its selected window.
Optional argument NORECORD means to neither change the order of
recently selected windows nor the buffer list.

The selection of TAB lasts until the next time the user does
something to select a different tab, or until the next time
this function is called.

This function returns TAB, or nil if TAB has been deleted."
  ;; Save current win conf
  (let* ((selected-tab (selected-tab frame))
         (tab-list (tab-list frame))
         (tab-param (assq selected-tab tab-list))
         (tab-name (assq 'name (nth 1 tab-param)))
         (tab-new-param (assq tab tab-list)))
    (when tab-param
      (setcar (nthcdr 2 tab-param) (current-window-configuration frame))
      (setcar (nthcdr 3 tab-param) (point-marker))
      (setcar (nthcdr 4 tab-param) tab-history-back)
      (setcar (nthcdr 5 tab-param) tab-history-forward)
      (setcar (nthcdr 6 tab-param)
              (delq nil (mapcar (lambda (b) (and (buffer-live-p b) b))
                                (frame-parameter frame 'buffer-list))))
      (setcar (nthcdr 7 tab-param)
              (delq nil (mapcar (lambda (b) (and (buffer-live-p b) b))
                                (frame-parameter frame 'buried-buffer-list))))
      (if tab-name (setcdr tab-name (tab-name frame))))
    (modify-frame-parameters frame (list (cons 'selected-tab tab)))
    (set-window-configuration (nth 2 tab-new-param))
    ;; set-window-configuration does not restore the value
    ;; of point in the current buffer, so restore that separately.
    (when (and (markerp (nth 3 tab-new-param))
               (marker-buffer (nth 3 tab-new-param)))
      (goto-char (nth 3 tab-new-param)))
    (setq tab-history-back (nth 4 tab-new-param))
    (setq tab-history-forward (nth 5 tab-new-param))
    (modify-frame-parameters
     frame (list (cons 'buffer-list
                       (delq nil (mapcar (lambda (b) (and (buffer-live-p b) b))
                                         (nth 6 tab-new-param))))))
    (modify-frame-parameters
     frame (list (cons 'buried-buffer-list
                       (delq nil (mapcar (lambda (b) (and (buffer-live-p b) b))
                                         (nth 7 tab-new-param))))))
    (tab-bar-setup frame)))

(defun delete-tab (&optional tab frame)
  "Remove TAB from its FRAME.
TAB defaults to the selected tab.  Return nil.
FRAME nil or omitted means use the selected frame.
Signal an error when TAB is the only tab on its frame."
  (interactive)
  (let* ((selected-tab (selected-tab frame))
         (tab (or tab selected-tab))
         (tab-list (tab-list frame))
         (tab-param (assq tab tab-list))
         (tab-select (or (next-tab tab) (previous-tab tab))))
    (modify-frame-parameters
     frame
     (list (cons 'tab-list (assq-delete-all tab tab-list))))
    (if (null (tab-list frame))
        (tab-bar-mode 0)
      (when tab-select (select-tab tab-select frame))
      (tab-bar-setup frame))))

(defun delete-other-tabs (&optional tab frame)
  "Delete all tabs except TAB.
TAB defaults to the selected tab.
FRAME nil or omitted means use the selected frame."
  (interactive)
  (when tab (select-tab tab frame))
  (let* ((tab (or tab (selected-tab frame)))
         (tab-list (tab-list frame))
         (tab-param (assq tab tab-list)))
    (modify-frame-parameters
     frame
     (list (cons 'tab-list (list tab-param))))
    (if (null (tab-list frame))
        (tab-bar-mode 0)
      (tab-bar-setup frame))))

(defun next-tab (&optional tab frame wrap)
  "Return the next tab in the tab list after TAB.
FRAME nil or omitted means use the selected frame.
When WRAP is non-nil, warp to the first tab."
  (let* ((tab (or tab (selected-tab frame)))
         (tab-list (tab-list frame)))
    (caar (or (cdr (member (assq tab tab-list) tab-list))
              (and wrap tab-list)))))

(defun previous-tab (&optional tab frame wrap)
  "Return the previous tab in the tab list before TAB.
FRAME nil or omitted means use the selected frame.
When WRAP is non-nil, warp to the last tab."
  (let* ((tab (or tab (selected-tab frame)))
         (tab-list (tab-list frame)))
    (caar (or (cdr (member (assq tab tab-list) (reverse tab-list)))
              (and wrap (last tab-list))))))

(defun other-tab (count &optional tab frame)
  "Select another tab in cyclic ordering of tabs.
COUNT specifies the number of tabs to skip, starting with the
selected tab, before making the selection.  If COUNT is
positive, skip COUNT tabs forwards.  If COUNT is negative,
skip -COUNT tabs backwards.  COUNT zero means do not skip any
tab, so select the selected tab.  In an interactive call,
COUNT is the numeric prefix argument.  Return nil."
  (interactive "p")
  (let ((tab (or tab (selected-tab frame))))
    (while (> count 0)
      (setq tab (next-tab tab frame t))
      (setq count (1- count)))
    (while (< count 0)
      (setq tab (previous-tab tab frame t))
      (setq count (1+ count)))
    (select-tab tab frame)))

(defun select-next-tab (&optional count)
  "Select the next tab in cyclic order.
COUNT has the same meaning as in the command `other-tab'."
  (interactive "p")
  (other-tab (or count 1)))

(defun select-previous-tab (&optional count)
  "Select the previous tab in cyclic order.
COUNT has the same meaning as in the command `other-tab'."
  (interactive "p")
  (other-tab (- (or count 1))))


;;; Tab identity (until it's first-class object).

;; Adapted from `gensym' in lisp/emacs-lisp/cl-macs.el.
(defvar tab-gensym-index 1)

(defun tab-gensym (&optional prefix)
  "Generate a new interned symbol.
The name is made by appending a number to PREFIX, default \"tab-\"."
  (let ((pfix (if (stringp prefix) prefix "tab-"))
	(num (if (integerp prefix) prefix
	       (prog1 tab-gensym-index
		 (setq tab-gensym-index (1+ tab-gensym-index))))))
    (intern (format "%s%d" pfix num))))


;;; Tab history.

;; FIXME: add `tab-history-mode'.

(defvar tab-history-back nil
  "Stack of window configurations user has visited.
Each element of the stack is a window configuration.")

(defvar tab-history-forward nil
  "Stack of window configurations user has visited with `tab-history-back' command.
Each element of the stack is a window configuration.")

(defun tab-history-back ()
  (interactive)
  (let ((win-conf (cadr tab-history-back)))
    (when win-conf
      (push (pop tab-history-back) tab-history-forward)
      (set-window-configuration win-conf))))

(defun tab-history-forward ()
  (interactive)
  (let ((win-conf (car tab-history-forward)))
    (when win-conf
      (push (pop tab-history-forward) tab-history-back)
      (set-window-configuration win-conf))))

(defun tab-history-update ()
  (push (current-window-configuration) tab-history-back))

(defun tab-name-update (&optional frame)
  (let* ((selected-tab (selected-tab frame))
         (tab-list (tab-list frame))
         (tab-param (assq selected-tab tab-list))
         (tab-name (assq 'name (nth 1 tab-param))))
    (if tab-name (setcdr tab-name (tab-name frame)))
    (tab-bar-setup frame)))

(defvar tab-frames nil)

(defun tab-window-configuration-change ()
  (when (or (memq (selected-frame) tab-frames)
            (/= 0 (minibuffer-depth)))
    (tab-name-update)
    (tab-history-update)))


;;; List tabs.

(defun list-tabs (&optional frame)
  "Display a list of names of existing tabs.
The list is displayed in a tab named `*Tab List*'.
FRAME nil or omitted means use the selected frame.
For more information, see the function `tab-menu'."
  (interactive "P")
  (switch-to-buffer (list-tabs-noselect frame)))

(defvar list-tabs-column 3)

(defun list-tabs-noselect (&optional frame)
  "Create and return a buffer with a list of names of existing tabs.
The buffer is named `*Tab List*'.
FRAME nil or omitted means use the selected frame.
For more information, see the function `tab-menu'."
  (let ((tab-list (tab-list)))
    (with-current-buffer (get-buffer-create "*Tab List*")
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; Vertical alignment to the center of the frame
      (insert-char ?\n (/ (- (frame-height) (length tab-list) 1) 2))
      ;; Horizontal alignment to the center of the frame
      (setq list-tabs-column (- (/ (frame-width) 2) 15))
      (dolist (tab tab-list)
        (insert (propertize
                 (format "%s %s\n"
                         (make-string list-tabs-column ?\040)
                         (propertize
                          (cdr (assq 'name (nth 1 tab)))
                          'mouse-face 'highlight
                          'help-echo "mouse-2: select this tab"))
                 'tab tab)))
      ;; (tab-menu-mode)
      (goto-char (point-min))
      (goto-char (or (next-single-property-change (point) 'tab) (point-min)))
      ;; (when (> (length tab-list) 1)
      ;;   (tab-menu-next-line))
      (move-to-column list-tabs-column)
      (set-buffer-modified-p nil)
      (delete-other-windows)
      (current-buffer))))

(define-key ctl-x-7-map "\C-b" 'list-tabs)


;;;; Key bindings

(define-key ctl-x-7-map "2" 'make-tab-command)
(define-key ctl-x-7-map "1" 'delete-other-tabs)
(define-key ctl-x-7-map "0" 'delete-tab)
(define-key ctl-x-7-map "o" 'other-tab)

(global-set-key [(control tab)]               'select-next-tab)
(global-set-key [(control shift iso-lefttab)] 'select-previous-tab)

(provide 'tab)

;;; tab.el ends here
