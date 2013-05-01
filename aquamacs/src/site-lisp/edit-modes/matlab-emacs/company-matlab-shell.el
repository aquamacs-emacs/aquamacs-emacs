;;; company-matlab-shell.el --- a matlab-shell-mode completion back-end for AUCTeX
;;
;; Copyright (C) 2009 David Engster
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(condition-case nil
    (require 'company)
  (error nil))
(eval-when-compile (require 'cl))
(require 'matlab)

;; the following code is mostly taken from matlab.el, (C) Eric M. Ludlam
(defun company-matlab-shell-tab ()
   "Send [TAB] to the currently running matlab process and retrieve completion."
   (goto-char (point-max))
   (let ((inhibit-field-text-motion t))
     (beginning-of-line))
   (re-search-forward comint-prompt-regexp)
   (let* ((lastcmd (buffer-substring (point) (matlab-point-at-eol)))
	  (tempcmd lastcmd)
	  (completions nil)
	  (limitpos nil))
     ;; search for character which limits completion, and limit command to it
     (setq limitpos
	   (if (string-match ".*\\([( /[,;=']\\)" lastcmd)
	       (1+ (match-beginning 1))
	     0))
     (setq lastcmd (substring lastcmd limitpos))
     ;; Whack the old command so we can insert it back later.
     (delete-region (+ (point) limitpos) (matlab-point-at-eol))
     ;; double every single quote
     (while (string-match "[^']\\('\\)\\($\\|[^']\\)" tempcmd)
       (setq tempcmd (replace-match "''" t t tempcmd 1)))
     ;; collect the list
     (setq completions (matlab-shell-completion-list tempcmd))
     (goto-char (point-max))
     (insert lastcmd)
     completions))

(defun company-matlab-shell-grab-symbol ()
  (when (string= (buffer-name (current-buffer)) (concat "*" matlab-shell-buffer-name "*"))
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-field-text-motion t))
	(beginning-of-line))
      (re-search-forward comint-prompt-regexp)
      (let* ((lastcmd (buffer-substring (point) (matlab-point-at-eol)))
	     limitpos)
	(setq limitpos
	      (if (string-match ".*\\([( /[,;=']\\)" lastcmd)
		  (1+ (match-beginning 1))
		0))
	(substring-no-properties lastcmd limitpos)))))


(defun company-matlab-shell-get-completions ()
  (when (string= (buffer-name (current-buffer)) (concat "*" matlab-shell-buffer-name "*"))
    (mapcar 'car (company-matlab-shell-tab))))

;;;###autoload
(defun company-matlab-shell (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for Matlab-Shell."
  (interactive (list 'interactive))
  (case command
        ('interactive (company-begin-backend 'company-matlab-shell))
        ('prefix (company-matlab-shell-grab-symbol))
        ('candidates (company-matlab-shell-get-completions))
	('sorted t)))

(provide 'company-matlab-shell)
;;; company-matlab-shell.el ends here
