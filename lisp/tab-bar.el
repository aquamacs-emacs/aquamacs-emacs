;;; tab-bar.el --- setting up the tab bar

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

;; Provides `tab-bar-mode' to control display of the tab-bar and
;; bindings for the global tab bar with convenience function
;; `tab-bar-add-item'.

;; The normal global binding for [tab-bar] (below) uses the value of
;; `tab-bar-map' as the actual keymap to define the tab bar.  Modes
;; may either bind items under the [tab-bar] prefix key of the local
;; map to add to the global bar or may set `tab-bar-map'
;; buffer-locally to override it.

;;; Code:

(defgroup tab-bar nil
  "Tab bar."
  :group 'frames)

(defface tab-bar
  '((default
      :box (:line-width 1 :style pressed-button)
      :foreground "black"
      :background "gray78")
    (((type x w32 ns) (class color))
     :background "gray78")
    (((type x) (class mono))
     :background "grey"))
  "Basic tab-bar face."
  :group 'tab-bar
  :version "24.1")

(defface tab-selected
  '((default
      :box (:line-width 1 :style released-button)
      :foreground "black"
      :background "gray92")
    (((type x w32 ns) (class color))
     :background "gray92")
    (((type x) (class mono))
     :background "grey"))
  "Selected tab face."
  :group 'tab-bar
  :version "24.1")

(defface tab
  '((default
      :box (:line-width 1 :style pressed-button)
      :foreground "black"
      :background "gray78")
    (((type x w32 ns) (class color))
     :background "gray78")
    (((type x) (class mono))
     :background "grey"))
  "Inactive tab face."
  :group 'tab-bar
  :version "24.1")


(define-minor-mode tab-bar-mode
  "Toggle use of the tab bar.
With numeric ARG, display the tab bar if and only if ARG is positive.

See `tab-bar-add-item' for conveniently adding tab bar items."
  :init-value nil
  :global t
  :group 'mouse
  :group 'frames
  (if tab-bar-mode
      (progn
	(modify-all-frames-parameters (list (cons 'tab-bar-lines 1)))
	(when (<= 1 (length (default-value 'tab-bar-map))) ; not yet setup
	  ;;(make-tab-command)
	  (tab-bar-setup))
	(setq tab-frames (frame-list))
	(add-hook 'window-configuration-change-hook 'tab-window-configuration-change))
    (modify-all-frames-parameters (list (cons 'tab-bar-lines 0)))
    (remove-hook 'window-configuration-change-hook 'tab-window-configuration-change)))

;;;###autoload
;; Used in the Show/Hide menu, to have the toggle reflect the current frame.
(defun toggle-tab-bar-mode-from-frame (&optional arg)
  "Toggle tab bar on or off, based on the status of the current frame.
See `tab-bar-mode' for more information."
  (interactive (list (or current-prefix-arg 'toggle)))
  (if (eq arg 'toggle)
      (tab-bar-mode (if (> (frame-parameter nil 'tab-bar-lines) 0) 0 1))
    (tab-bar-mode arg)))

;;;###autoload
(put 'tab-bar-mode 'standard-value '(t))

(defvar tab-bar-map (make-sparse-keymap)
  "Keymap for the tab bar.
Define this locally to override the global tab bar.")

(global-set-key [tab-bar]
		`(menu-item ,(purecopy "tab bar") ignore
			    :filter tab-bar-make-keymap))

(declare-function image-mask-p "image.c" (spec &optional frame))

(defconst tab-bar-keymap-cache (make-hash-table :weakness t :test 'equal))

(defun tab-bar-make-keymap (&optional ignore)
  "Generate an actual keymap from `tab-bar-map'.
Its main job is to figure out which images to use based on the display's
color capability and based on the available image libraries."
  (let ((key (cons (frame-terminal) tab-bar-map)))
    (or ;; FIXME: commented out: (gethash key tab-bar-keymap-cache)
	(puthash key (tab-bar-make-keymap-1) tab-bar-keymap-cache))))

(defun tab-bar-make-keymap-1 ()
  "Generate an actual keymap from `tab-bar-map', without caching."
  (mapcar (lambda (bind)
            (let (image-exp plist)
              (when (and (eq (car-safe (cdr-safe bind)) 'menu-item)
			 ;; For the format of menu-items, see node
			 ;; `Extended Menu Items' in the Elisp manual.
			 (setq plist (nthcdr (if (consp (nth 4 bind)) 5 4)
					     bind))
			 (setq image-exp (plist-get plist :image))
			 (consp image-exp)
			 (not (eq (car image-exp) 'image))
			 (fboundp (car image-exp)))
		(if (not (display-images-p))
		    (setq bind nil)
		  (let ((image (eval image-exp)))
		    (unless (and image (image-mask-p image))
		      (setq image (append image '(:mask heuristic))))
		    (setq bind (copy-sequence bind)
			  plist (nthcdr (if (consp (nth 4 bind)) 5 4)
					bind))
		    (plist-put plist :image image))))
	      bind))
	  tab-bar-map))

;;;###autoload
(defun tab-bar-add-item (icon name def key selected &rest props)
  "Add an item to the tab bar.
ICON names the image, DEF is the key definition and KEY is a symbol
for the fake function key in the menu keymap.  Remaining arguments
PROPS are additional items to add to the menu item specification.  See
Info node `(elisp)Tab Bar'.  Items are added from left to right.

ICON is the base name of a file containing the image to use.  The
function will first try to use low-color/ICON.xpm if `display-color-cells'
is less or equal to 256, then ICON.xpm, then ICON.pbm, and finally
ICON.xbm, using `find-image'.

Use this function only to make bindings in the global value of `tab-bar-map'.
To define items in any other map, use `tab-bar-local-item'."
  (apply 'tab-bar-local-item icon name def key
	 (default-value 'tab-bar-map) selected props))

;;;###autoload
(defun tab-bar-local-item (icon name def key map selected &rest props)
  "Add an item to the tab bar in map MAP.
ICON names the image, DEF is the key definition and KEY is a symbol
for the fake function key in the menu keymap.  Remaining arguments
PROPS are additional items to add to the menu item specification.  See
Info node `(elisp)Tab Bar'.  Items are added from left to right.

ICON is the base name of a file containing the image to use.  The
function will first try to use low-color/ICON.xpm if `display-color-cells'
is less or equal to 256, then ICON.xpm, then ICON.pbm, and finally
ICON.xbm, using `find-image'."
  (if (null icon)
      (define-key-after map (vector key)
	`(menu-item ,(propertize name
				 'face (if selected 'tab-selected 'tab)
				 'mouse-face 'highlight)
		    ,def ,@props))
    (let* ((fg (face-attribute 'tab-bar :foreground))
	   (bg (face-attribute 'tab-bar :background))
	   (colors (nconc (if (eq fg 'unspecified) nil (list :foreground fg))
			  (if (eq bg 'unspecified) nil (list :background bg))))
	   (xpm-spec (list :type 'xpm :file (concat icon ".xpm")))
	   (xpm-lo-spec (list :type 'xpm :file
			      (concat "low-color/" icon ".xpm")))
	   (pbm-spec (append (list :type 'pbm :file
				   (concat icon ".pbm")) colors))
	   (xbm-spec (append (list :type 'xbm :file
				   (concat icon ".xbm")) colors))
	   (image-exp `(find-image
			(cond ((not (display-color-p))
			       ',(list pbm-spec xbm-spec xpm-lo-spec xpm-spec))
			      ((< (display-color-cells) 256)
			       ',(list xpm-lo-spec xpm-spec pbm-spec xbm-spec))
			      (t
			       ',(list xpm-spec pbm-spec xbm-spec))))))
      (define-key-after map (vector key)
	`(menu-item ,(if name
			 (propertize name
				     'face (if selected 'tab-selected 'tab)
				     'mouse-face 'highlight))
		    ,def :image ,image-exp ,@props)))))

;;; Set up some global items.  Additions/deletions up for grabs.

(defun tab-bar-setup ()
  (setq tab-bar-map (make-sparse-keymap))
  (tab-bar-add-item "tab-left"
		    ""
		    'tab-history-back
		    'tab-history-back
		    nil
		    :enable 'tab-history-back
		    :help "Go back in history")
  (tab-bar-add-item "tab-right"
		    ""
		    'tab-history-forward
		    'tab-history-forward
		    nil
		    :enable 'tab-history-forward
		    :help "Go forward in history")
  (let ((selected-tab (selected-tab)))
    (dolist (tab (tab-list))
      (let ((tab-id (car tab))
	    (tab-name (or (cdr (assoc 'name (nth 1 tab))) (tab-name))))
	(when tab-id
	  (tab-bar-add-item nil
			    tab-name
			    `(lambda ()
			       (interactive)
			       (select-tab ',tab-id))
			    tab-id
			    (eq selected-tab tab-id)
			    :enable `(not (eq (selected-tab) ',tab-id))
			    :help "Select this tab")
	  (tab-bar-add-item "tab-delete"
			    ""
			    `(lambda ()
			       (interactive)
			       (delete-tab ',tab-id))
			    (intern (concat "delete-" (symbol-name tab-id)))
			    nil
			    :help "Delete this tab")))))
  ;; (redraw-frame (selected-frame))
  )

(provide 'tab-bar)

;;; tab-bar.el ends here
