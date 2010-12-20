;; Aquamacs startup file

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs
 
;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/


;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;; Copyright (C) 2006, 2007, 2008, 2009, 2010: David Reitter
 


; NONE
;; one at the end of this file

(unless noninteractive
;; do not initialize Aquamacs
;; in order to provide a clean build environment during
;; bootstrapping

;; Version information

(defvar aquamacs-version "2.2"
"A string with Aquamacs' version number.
The format of the string is undefined. 
For a reliable numerical representation, use `aquamacs-version-id'.")

(defvar aquamacs-version-id 211
"A float indicating Aquamacs' version number.
Full integers correspond to the third position of the public
version number, e.g. version 0.9.7 is represented as `97.x'.
Minor version numbers are reflected in the decimals. 
It is guaranteed that for any two Aquamacs releases A and B,
if aquamacs-version-id for B is higher than aquamacs-version-id 
for A, then B is newer than A.")

(defvar aquamacs-minor-version "dev"
"Version code for minor maintenance releases.
Changes in this code are ignored during the online version check.")

;; compatibility for Carbon Emacs
(unless (boundp 'initial-window-system)
  (defvaralias 'initial-window-system 'window-system))

;; only for Emacs.app
(when (fboundp 'ns-find-file) ;; running Cocoa?
  (setq unicode-emacs 0)
  (require 'cocoa-compatibility))

(when ;; do not load this twice 
    (not (memq 'aquamacs-site-start features))
 
  (provide 'aquamacs-site-start)

  (if (or init-file-user user-init-file)
      (require 'load-emacs-pre-plugins))

  ;; aquamacs-reload-preloaded-files should be set by a
  ;; plugin (preloaded) --> for Aquamacs developer(s) only.
  (when (and (boundp 'aquamacs-reload-preloaded-files)
	   aquamacs-reload-preloaded-files
	   (boundp 'aq-preloaded))
      (message "Ignoring preloaded files.")
      (mapc (lambda (p)
	      (setq features (delete p features)))
	    aq-preloaded))

  ;; load path  
  ;;(normal-top-level-add-subdirs-to-load-path)  ; no need to do this is in site-start.

  (require 'aquamacs)
  (require 'aquamacs-aux) ;; calls (aquamacs-setup)
  ;; fix function associations in load-history
  (aquamacs-cleanup-load-history)



  (require 'aquamacs-mode-defaults)
 
  ;; init-file-user is set at this time;
  ;; user-init-file is normally not set.
  ;; see comment in startup.el
  (if (or init-file-user user-init-file)
      (require 'load-emacs-plugins))

  ;; workaround - Emacs doesn't do it  (0.9.9b)
  (defun display-startup-echo-area-message-2 ()
    "Like `display-startup-echo-area-message', but without logging."
    (let ((message-log-max))
      (display-startup-echo-area-message)))

  (add-hook 'after-init-hook 'display-startup-echo-area-message-2 'append)

  )
)