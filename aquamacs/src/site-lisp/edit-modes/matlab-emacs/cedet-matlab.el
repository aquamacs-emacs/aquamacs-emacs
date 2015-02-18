;;; cedet-matlab.el --- CEDET Setup support
;;
;; Copyright (C) 2009 Eric Ludlam
;;
;; Author: Eric Ludlam <eludlam@mathworks.com>
;; X-RCS: $Id: cedet-matlab.el,v 1.1 2009/07/06 19:48:15 zappo Exp $
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Setup miscelaneous CEDET tools to work with MATLAB.

;;; Code:
;;;###autoload
(defun matlab-cedet-setup ()
  "Update various paths to get SRecode to identify our macros."
  (interactive)

  ;; Setup Semantic parser:
  (add-hook 'matlab-mode-hook 'semantic-default-matlab-setup)

  ;; Setup semanticdb for MATLAB support:
  (require 'semanticdb-matlab)

  ;; Setup Semantic Recoder (Template support for MATLAB and TLC.):
  (let* ((lib (locate-library "matlab.el" t))
	 (ededir (file-name-directory lib))
	 (tmpdir (file-name-as-directory
		  (expand-file-name "templates" ededir))))
    (when (not tmpdir)
      (error "Unable to locate MATLAB Templates directory"))

    ;; Rig up the map.
    (require 'srecode-map)
    (add-to-list 'srecode-map-load-path tmpdir)
    (srecode-map-update-map t)
    )

  
  )


(provide 'cedet-matlab)
;;; cedet-matlab.el ends here
