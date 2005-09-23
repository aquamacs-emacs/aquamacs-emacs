;; aquamcs-mule -- Language specific settings for Aquamacs Emacs.

;; Author: Mahn-Soo Choi (mahn-soo.choi@unibas.ch),
;;         David Reitter
;; $Date: 2005/09/23 08:03:39 $
;; $Revision: 1.1 $

;;; Commentary

;; The built-in `set-language-environment' function already sets
;; default values of many parameters for each international language.
;; However, the peculiarity of Mac OS X interface and Unicode
;; handling, one has to adjust a few parameters to this particular
;; system.  Here I collected some common settings, which have been known
;; reasonable.  As indicated from the name, I only provide settings for
;; East Asian languages (Chinese, Japanese, and Korean); and of
;; course, English (Latin-1), which is almost trivial.
;;
;; I welcome any corrections from the native speakers of Chinese,
;; Japanese, and Korean.  In particular, I myself am a Korean, and it
;; is mostly likely that Chinese and Japanese settings provided here
;; might be quite poor.

;;; Changes

;; 2005-09-23 by Dave
;; - renamed to aquamacs-mule, fixed some typos
;; - added license
;; 2005-09-22 by Mahn-Soo
;; - bug in aquamacs-set-language-environment fixed
;; 2005-09-22 by Dave
;; - generalized aquamacs-set-language-environment
;; - changed names of functions to aquamacs-language-config-*
;;   with upper case spelling to match name of language
;; - added menu

;; license

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


;;; code

;; This package is crutial, at least, for Asian languages.
(require 'utf-8m)                   ; a file written by Seiji Zenitani

;;;###autoload
(defun aquamacs-set-language-environment (language-name)
  "Like `set-language-environment', but sets additional
parameters for Mac OS X users."
  (interactive (list
                (read-language-name
                 nil
                 "Set language environment (default, English): ")))
  (if language-name
      (if (symbolp language-name)
	  (setq language-name (symbol-name language-name)))
    (setq language-name "English"))
  (set-language-environment language-name)
  (funcall (intern (concat "aquamacs-language-config-" language-name)))
  )

;;; Language specific

;; Korean
(defun aquamacs-language-config-Korean ()
  "Korean specific extra settings for Aquamacs Emacs."
  (set-default-coding-systems 'euc-kr-unix)
  (set-keyboard-coding-system 'euc-kr-mac)
  (set-terminal-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8m)
  (set-clipboard-coding-system 'euc-kr-mac)
  (message "Korean"))

;; Japanese 
(defun aquamacs-language-config-Japanese ()
  "Japanese specific extra settings for Aquamacs Emacs."
  ;; Copied from Seiji Zenitani's carbon-emacs-japanese-init.el
  (set-default-coding-systems 'euc-jp-unix)
  (set-keyboard-coding-system 'sjis-mac)
  (set-clipboard-coding-system 'sjis-mac)
  (set-terminal-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8m)
  (message "Japanese")
  )

;; Traditional Chinese
(defun aquamacs-language-config-Chinese-BIG5 ()
  "Traditional Chinese specific extra settings for Aquamacs Emacs."
  (set-default-coding-systems 'chinese-big5)
  (set-keyboard-coding-system 'chinese-big5)
  (set-clipboard-coding-system 'chinese-big5-mac)
  (set-terminal-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8m)
  (message "Chinese (traditional)"))

;; Simplified Chinese
(defun aquamacs-language-config-Chinese-GB ()
  "Simplified Chinese specific extra settings for Aquamacs Emacs."
  (set-default-coding-systems 'chinese-iso-8bit)
  (set-keyboard-coding-system 'chinese-iso-8bit)
  (set-clipboard-coding-system 'chinese-iso-8bit-mac)
  (set-terminal-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8m)
  (message "Chinese (simplified)"))

;; English
(defun aquamacs-language-config-English ()
  "English specific extra settings for Aquamacs Emacs."
  (set-default-coding-systems 'mac-roman-mac)
  (set-keyboard-coding-system 'mac-roman-mac)
  (set-terminal-coding-system 'iso-8859-1)
  (set-file-name-coding-system 'iso-8859-1)
  (set-clipboard-coding-system 'mac-roman-mac)
  (message "English"))


;;; insert menu
 
(defvar aquamacs-setup-language-environment-map
  (make-sparse-keymap "Set Mac Language Environment"))

(let ((prefix "aquamacs-language-config-"))
  (mapc
   (lambda (func) 
     (define-key aquamacs-setup-language-environment-map
       (vector (make-symbol 
		(concat "aq-lang-config-" (symbol-name func))))
       (let ((le (substring (symbol-name func) (length prefix))))
         `(menu-item 
           ,le
           ,(eval 
             (list 'lambda '() '(interactive)
                   (list 'aquamacs-set-language-environment  `(quote ,le))
                   ))
           :help "Set language environment including Mac specific settings."
           ))
       )
     )
   (apropos-internal "aquamacs-language-config-.*" 'functionp)
   )
  )

(define-key mule-menu-keymap [aquamacs-set-language-environment]
  (list 'menu-item  "Set Mac Language Environment" 
	aquamacs-setup-language-environment-map
	:help "Multilingual environment suitable for a specific language, 
including Mac-specific settings."))

(define-key-after mule-menu-keymap [separator-aquamacs-mule]
  '("--")
  'aquamacs-set-language-environment)

(provide 'aquamacs-mule)
;; end
