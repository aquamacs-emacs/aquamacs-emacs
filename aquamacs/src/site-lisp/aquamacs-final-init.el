;; aquamacs-final-init -- Final Aquamacs-specific initialization

;; Author: Win Treese, treese@acm.org
;; Maintainer: Win Treese
;; Keywords: aquamacs

;; This file is part of Aquamacs Emacs
;; http://www.aquamacs.org/


;; Aquamacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; Aquamacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Aquamacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Copyright (C) 2020 Win Treese

;;; Commentary:
;; Final initialization for Aquamacs
;; This is called from startup.el just before after-init-hook is run
;; and after package-initialize is called.

;; The main purpose of this file is to load libraries that are (1)
;; bundled with Aquamacs, (2) can't be handled with autoloads, and (3)
;; might be updated as ELPA/MELPA packages.

;; The main example of this is AUCTeX.  Loading auctex.el sets up the
;; modes to work properly instead of loading the default Emacs
;; TeX/LaTeX modes.  This is an uncommon situation.

;; Load auctex-config if available.  One of the purposes is so that
;; Aquamcs can run in batch for updating AUCTeX.

(condition-case nil
    (require 'auctex-config nil t)
  (error nil))

(provide 'aquamacs-final-init)
;;; aquamacs-final-init.el ends here
