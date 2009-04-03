;; aquamcs-mule -- Language specific settings for Aquamacs Emacs.

;; Author: Mahn-Soo Choi (mahn-soo.choi@unibas.ch),
;;         David Reitter
;; $Date: 2005/09/28 14:12:58 $
;; $Revision: 1.2 $

;;; Commentary

;; The built-in `set-language-environment' function already sets
;; default values of many parameters for each international language.
;; Because of the peculiar ways of user interface and Unicode handling
;; of Mac OS X, one needs to set additional parameter, especially, the
;; lanauge specific coding systems.  This file modifies the
;; `language-info-alist' so that these addional parameters can be set
;; whenever a user switches to a specific language environment.
;;
;; Currently, we modify on the settings of Chinese, Japanese, and
;; Korean.  Changes for other languages will also be added if
;; requested.
;;
;; We welcome any corrections from the native speakers of Chinese,
;; Japanese, and Korean.  In particular, I myself am a Korean, and it
;; is mostly likely that Chinese and Japanese settings provided here
;; might be quite poor.
;;
;; To use this file, put the following line in your init file.
;;    (if (eq system-type 'darwin) (require 'aquamacs-mule))

;;; Changes

;; 2005-09-24 by Dave
;; - added menu-bar change
;; - changed final message
;; 2005-09-23 by Mahn-Soo
;; - rewritten from the scratch
;; - directly modify the language-info-alist
;; - no need to worry about saving options
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

;;; License

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

(defvar aq-save-default-coding-system nil)
(defvar aq-save-keyboard-coding-system nil)
(defvar aq-save-selection-coding-system nil)
(defvar aq-save-terminal-coding-system nil)
(defvar aq-save-file-name-coding-system nil)

(defun aquamacs-mule-save-coding-systems ()
  "Save the current values of coding system parameters,
`buffer-file-coding-system', `default-process-coding-system',
`keyboard-coding-system', `selection-coding-system',
`default-terminal-coding-system', `file-name-coding-system'."
  (setq aq-save-default-coding-system   buffer-file-coding-system
        aq-save-keyboard-coding-system  keyboard-coding-system   
        aq-save-selection-coding-system selection-coding-system  
        aq-save-terminal-coding-system  default-terminal-coding-system   
        aq-save-file-name-coding-system file-name-coding-system))

(defun aquamacs-mule-restore-coding-systems ()
  "Restore the values of coding system parameters,
`buffer-file-coding-system', `default-process-coding-system',
`keyboard-coding-system', `selection-coding-system',
`default-terminal-coding-system', `file-name-coding-system'."
  (set-default-coding-systems aq-save-default-coding-system)
  (setq keyboard-coding-system         aq-save-keyboard-coding-system  
        selection-coding-system        aq-save-selection-coding-system 
        default-terminal-coding-system aq-save-terminal-coding-system    
        file-name-coding-system        aq-save-file-name-coding-system))

(defvar aquamacs-mule-language-help
  "Aqumacs Emacs have changed this language environment slightly.
See the documentation in aquamacs-mule.el for details."
  "Warning message that the language environment has been changed
  by Aquamacs.")

(defun aquamacs-mule-add-language-help (lang-env &optional text)
  "Add the remark that the language environment LANG-ENV is
slightly different from the one in the standard distribution of
GNU Emacs."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (let ((doc (get-language-info lang-env 'documentation))
        (help (if (and text (stringp text))
                  text
                aquamacs-mule-language-help)))
    (set-language-info
     lang-env 'documentation
     (if (and doc (stringp doc))
         (concat doc "\n" help)
       help))
    )
  )

;; Korean
(set-language-info "Korean"
                   'setup-function
                   'setup-korean-environment-mac)
(set-language-info "Korean"
                   'exit-function
                   'exit-korean-environment-mac)
(aquamacs-mule-add-language-help "Korean")

(defun setup-korean-environment-mac ()
  "Aquamacs version of `setup-korean-environment-internal'."
  (aquamacs-mule-save-coding-systems)
  (setup-korean-environment-internal)
  (set-default-coding-systems 'euc-kr-unix)
  (set-keyboard-coding-system 'euc-kr-mac)
  (set-selection-coding-system 'euc-kr-mac)
  (set-terminal-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8m)
  )

(defun exit-korean-environment-mac ()
  "Exit Korean language environment."
  (exit-korean-environment)
  (aquamacs-mule-restore-coding-systems)
  )

;; Japanese
(set-language-info "Japanese"
                   'setup-function
                   'setup-japanese-environment-mac)
(set-language-info "Japanese"
                   'exit-function
                   'aquamacs-mule-restore-coding-systems)
(aquamacs-mule-add-language-help "Japanese")

(defun setup-japanese-environment-mac ()
  "Aquamacs version of `setup-japanese-environment-internal'."
  (aquamacs-mule-save-coding-systems)
  (setup-japanese-environment-internal)
  (set-default-coding-systems 'euc-jp-unix)
  (set-keyboard-coding-system 'sjis-mac)
  (set-selection-coding-system 'sjis-mac)
  (set-terminal-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8m)
  )

;; Chinese (traditional)
(set-language-info "Chinese-BIG5"
                   'setup-function
                   'setup-chinese-big5-environment-mac)
(set-language-info "Chinese-BIG5"
                   'exit-function
                   'aquamacs-mule-restore-coding-systems)
(aquamacs-mule-add-language-help "Chinese-BIG5")

(defun setup-chinese-big5-environment-mac ()
  "Aquamacs version of `setup-chinese-environment-internal'."
  (aquamacs-mule-save-coding-systems)
  (set-default-coding-systems 'chinese-big5)
  (set-keyboard-coding-system 'chinese-big5)
  (set-selection-coding-system 'chinese-big5-mac)
  (set-terminal-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8m)
  )

;; Chinese (simplified)
(set-language-info "Chinese-GB"
                   'setup-function
                   'setup-chinese-gb-environment-mac)
(set-language-info "Chinese-GB"
                   'exit-function
                   'aquamacs-mule-restore-coding-systems)
(aquamacs-mule-add-language-help "Chinese-GB")

(defun setup-chinese-gb-environment-mac ()
  "Aquamacs version of `setup-chinese-environment-internal'."
  (aquamacs-mule-save-coding-systems)
  (set-default-coding-systems 'chinese-iso-8bit)
  (set-keyboard-coding-system 'chinese-iso-8bit)
  (set-selection-coding-system 'chinese-iso-8bit-mac)
  (set-terminal-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8m)
  )

;; Ensure that the changes in `language-info-alist' take effects.
(set-language-environment current-language-environment)
(message "Aquamacs Mule installed.")


(provide 'aquamacs-mule)
;; end
