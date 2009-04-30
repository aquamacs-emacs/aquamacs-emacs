;;; ns-platform-support.el 

;; Copyright (C) 1993, 1994, 2005, 2006, 2007, 2008, 2009
;;   Adrian Robert

;; This file is not part of GNU Emacs.
;; This file is not part of Aquamacs.

;; This is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;; These functions have been spun out from Emacs.app, since they
;; are not part of Emacs 23 due to the addition of specific functions
;; not available in compatible manner on free platforms.

;; To install and use:
;; Put this file into the Emacs load path.
;; Add to your initialization file (e.g., ~/.emacs):
;; (require 'ns-platform-support)
;; (ns-extended-platform-support-mode 1)
;;
;; The minor mode can be turned on and off using
;; M-x ns-extended-platform-support-mode RET.


;; Code by Adrian Robert and others.


;; ns-arrange functions contributed
;; by Eberhard Mandler <mandler@dbag.ulm.DaimlerBenz.COM>
(defun ns-arrange-all-frames ()
  "Arranges all frames according to topline"
  (interactive)
  (ns-arrange-frames t))

(defun ns-arrange-visible-frames ()
  "Arranges all visible frames according to topline"
  (interactive)
  (ns-arrange-frames nil))

(defun ns-arrange-frames (vis)
  (let ((frame (next-frame))
	(end-frame (selected-frame))
	(inc-x 20)                      ;relative position of frames
	(inc-y 22)
	(x-pos 100)                     ;start position
	(y-pos 40)
	(done nil))
    (while (not done)                   ;cycle through all frames
      (if (not (or vis (eq (frame-visible-p frame) t)))
          (setq x-pos x-pos); do nothing; true case
	(set-frame-position frame x-pos y-pos)
	(setq x-pos (+ x-pos inc-x))
	(setq y-pos (+ y-pos inc-y))
	(raise-frame frame))
      (select-frame frame)
      (setq frame (next-frame))
      (setq done (equal frame end-frame)))
    (set-frame-position end-frame x-pos y-pos)
    (raise-frame frame)
    (select-frame frame)))


;;;; File menu, replaces standard under ns-extended-platform-support
(defvar menu-bar-ns-file-menu (make-sparse-keymap "File"))
(define-key menu-bar-ns-file-menu [one-window]
  '("Remove Splits" . delete-other-windows))
(define-key menu-bar-ns-file-menu [split-window]
  '("Split Window" . split-window-vertically))

(define-key menu-bar-ns-file-menu [separator-print] '("--"))

(defvar ns-ps-print-menu-map (make-sparse-keymap "Postscript Print"))
(define-key ns-ps-print-menu-map [ps-print-region]
  '("Region (B+W)" . ps-print-region))
(define-key ns-ps-print-menu-map [ps-print-buffer]
  '("Buffer (B+W)" . ps-print-buffer))
(define-key ns-ps-print-menu-map [ps-print-region-faces]
  '("Region" . ps-print-region-with-faces))
(define-key ns-ps-print-menu-map [ps-print-buffer-faces]
  '("Buffer" . ps-print-buffer-with-faces))
(define-key menu-bar-ns-file-menu [postscript-print]
  (cons "Postscript Print" ns-ps-print-menu-map))

(define-key menu-bar-ns-file-menu [print-region]
  '("Print Region" . print-region))
(define-key menu-bar-ns-file-menu [print-buffer]
  '("Print Buffer" . ns-print-buffer))

(define-key menu-bar-ns-file-menu [separator-save] '("--"))

(define-key menu-bar-ns-file-menu [recover-session]
  '("Recover Crashed Session" . recover-session))
(define-key menu-bar-ns-file-menu [revert-buffer]
  '("Revert Buffer" . revert-buffer))
(define-key menu-bar-ns-file-menu [write-file]
  '("Save Buffer As..." . ns-write-file-using-panel))
(define-key menu-bar-ns-file-menu [save-buffer] '("Save Buffer" . save-buffer))

(define-key menu-bar-ns-file-menu [kill-buffer]
  '("Kill Current Buffer" . kill-this-buffer))
(define-key menu-bar-ns-file-menu [delete-this-frame]
  '("Close Frame" . delete-frame))

(define-key menu-bar-ns-file-menu [separator-open] '("--"))

(define-key menu-bar-ns-file-menu [insert-file]
  '("Insert File..." . insert-file))
(define-key menu-bar-ns-file-menu [dired]
  '("Open Directory..." . ns-open-file-using-panel))
(define-key menu-bar-ns-file-menu [open-file]
  '("Open File..." . ns-open-file-using-panel))
(define-key menu-bar-ns-file-menu [make-frame]
  '("New Frame" . make-frame))


(defun menu-bar-update-frames ()
  ;; If user discards the Windows item, play along.
  (when (lookup-key (current-global-map) [menu-bar windows])
    (let ((frames (frame-list))
          (frames-menu (make-sparse-keymap "Select Frame")))
      (setcdr frames-menu
              (nconc
               (mapcar (lambda (frame)
			 (nconc (list
			  (frame-parameter frame 'window-id)
			  (frame-parameter frame 'name))
			  `(lambda ()
			     (interactive) (menu-bar-select-frame ,frame))))
                       frames)
               (cdr frames-menu)))
      (define-key frames-menu [separator-frames] '("--"))
      (define-key frames-menu [popup-color-panel]
        '("Colors..." . ns-popup-color-panel))
      (define-key frames-menu [popup-font-panel]
        '("Font Panel..." . ns-popup-font-panel))
      (define-key frames-menu [separator-arrange] '("--"))
      (define-key frames-menu [arrange-all-frames]
        '("Arrange All Frames" . ns-arrange-all-frames))
      (define-key frames-menu [arrange-visible-frames]
        '("Arrange Visible Frames" . ns-arrange-visible-frames))
      ;; Don't use delete-frame as event name
      ;; because that is a special event.
      (define-key (current-global-map) [menu-bar windows]
        (cons "Window" frames-menu)))))

(defun force-menu-bar-update-buffers ()
  ;; This is a hack to get around fact that we already checked
  ;; frame-or-buffer-changed-p and reset it, so menu-bar-update-buffers
  ;; does not pick up any change.
  (menu-bar-update-buffers t))

(add-hook 'menu-bar-update-fab-hook 'menu-bar-update-frames)
(add-hook 'menu-bar-update-fab-hook 'force-menu-bar-update-buffers)

(defun menu-bar-update-frames-and-buffers ()
  (if (frame-or-buffer-changed-p)
      (run-hooks 'menu-bar-update-fab-hook)))



;; Toggle some additi7onal Nextstep-like features that may interfere
;; with users' expectations coming from emacs on other platforms.
(define-minor-mode ns-extended-platform-support-mode
  "Toggle Nextstep extended platform support features.
   When this mode is active (no modeline indicator):
   - File menu is altered slightly in keeping with conventions.
   - Screen position is preserved in scrolling.
   - Transient mark mode is activated"
  :init-value nil
  :global t
  :group 'ns
  (if ns-extended-platform-support-mode
      (progn
	(defun ns-show-manual () "Show Emacs.app section in the Emacs manual"
          (interactive)
          (info "(emacs) Mac OS / GNUstep"))
	(setq where-is-preferred-modifier 'super)
        (setq scroll-preserve-screen-position t)
        (transient-mark-mode 1)

        ;; Change file menu to simplify and add a couple of
        ;; Nextstep-specific items
        (easy-menu-remove-item global-map '("menu-bar") 'file)
        (easy-menu-add-item global-map '(menu-bar)
                            (cons "File" menu-bar-ns-file-menu) 'edit)
	(define-key menu-bar-help-menu [ns-manual]
	  '(menu-item "Read the Emacs.app Manual Chapter" ns-show-manual))

	(define-key global-map [menu-bar windows] (make-sparse-keymap "Window"))

	(setq menu-bar-update-hook
	      (delq 'menu-bar-update-buffers menu-bar-update-hook))
	(add-hook 'menu-bar-update-hook 'menu-bar-update-frames-and-buffers)
	(menu-bar-update-frames) (force-menu-bar-update-buffers))
    (progn
      ;; Undo everything above.
      (fmakunbound 'ns-show-manual)
      (setq where-is-preferred-modifier 'nil)
      (setq scroll-preserve-screen-position nil)
      (transient-mark-mode 0)
      (easy-menu-remove-item global-map '("menu-bar") 'file)
      (easy-menu-add-item global-map '(menu-bar)
                          (cons "File" menu-bar-file-menu) 'edit)
      (easy-menu-remove-item global-map '("menu-bar" "help-menu") 'ns-manual)
      (setq menu-bar-update-hook
	    (delq 'menu-bar-update-frames-and-buffers menu-bar-update-hook))
      (add-hook 'menu-bar-update-hook 'menu-bar-update-buffers)

      (define-key global-map [menu-bar windows] nil))))



;; Functions to set environment variables by running a subshell.
;;; Idea based on Nextstep 4.2 distribution, this version of code
;;; based on mac-read-environment-vars-from-shell () by David Reitter.
;;; Mostly used only under ns-extended-platform-support-mode.

(defun ns-make-command-string (cmdlist)
  (mapconcat 'identity cmdlist " ; "))

;;;###autoload
(defun ns-grabenv (&optional shell-path startup)
  "Set the Emacs environment using the output of a shell command.
This runs a shell subprocess, and interpret its output as a
series of environment variables to insert into the emacs
environment.
SHELL-PATH gives the path to the shell; if nil, this defaults to
the current setting of `shell-file-name'.
STARTUP is a list of commands for the shell to execute; if nil,
this defaults to \"printenv\"."
  (interactive)
  (with-temp-buffer
    (let ((shell-file-name (if shell-path shell-path shell-file-name))
	  (cmd (ns-make-command-string (if startup startup '("printenv")))))
      (shell-command cmd t)
      (while (search-forward-regexp "^\\([A-Za-z_0-9]+\\)=\\(.*\\)$" nil t)
	(setenv (match-string 1)
		(if (equal (match-string 1) "PATH")
		    (concat (getenv "PATH") ":" (match-string 2))
		  (match-string 2)))))))

(provide 'ns-platform-support)