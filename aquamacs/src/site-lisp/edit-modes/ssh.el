;;; ssh.el --- remote login interface

;; Copyright (C) 1996, 97, 98, 2001 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: unix, comm
;; Created: 1996-07-03

;; $Id: ssh.el,v 1.11 2012/07/09 22:15:45 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Support for remote logins using `ssh'.
;; This program is layered on top of shell.el; the code here only accounts
;; for the variations needed to handle a remote process, e.g. directory
;; tracking and the sending of some special characters.

;; If you wish for ssh mode to prompt you in the minibuffer for
;; passwords when a password prompt appears, just enter m-x send-invisible
;; and type in your line, or add `comint-watch-for-password-prompt' to
;; `comint-output-filter-functions'.

;;; Code:

(require 'comint)
(require 'shell)

(defgroup ssh nil
  "Secure remote login interface"
  :group 'processes
  :group 'unix)

(defcustom ssh-program "ssh"
  "*Name of program to invoke ssh"
  :type 'string
  :group 'ssh)

(defcustom ssh-explicit-args '()
  "*List of arguments to pass to ssh on the command line."
  :type '(repeat (string :tag "Argument"))
  :group 'ssh)

(defcustom ssh-mode-hook nil
  "*Hooks to run after setting current buffer to ssh-mode."
  :type 'hook
  :group 'ssh)

(defcustom ssh-process-connection-type t
  "*If non-`nil', use a pty for the local ssh process.
If `nil', use a pipe (if pipes are supported on the local system).

Generally it is better not to waste ptys on systems which have a static
number of them.  However, ssh won't allocate a pty on the remote host
unless one is used locally as well."
  :type '(choice (const :tag "ptys" t)
		 (const :tag "pipes" nil))
  :group 'ssh)

(defcustom ssh-directory-tracking-mode 'local
  "*Control whether and how to do directory tracking in an ssh buffer.

nil means don't do directory tracking.

t means do so using an ftp remote file name.

Any other value means do directory tracking using local file names.
This works only if the remote machine and the local one
share the same directories (through NFS).  This is the default.

This variable becomes local to a buffer when set in any fashion for it.

It is better to use the function of the same name to change the behavior of
directory tracking in an ssh session once it has begun, rather than
simply setting this variable, since the function does the necessary
re-synching of directories."
  :type '(choice (const :tag "off" nil)
		 (const :tag "ftp" t)
		 (const :tag "local" local))
  :group 'ssh)

(make-variable-buffer-local 'ssh-directory-tracking-mode)

(defcustom ssh-x-display-follow-current-frame t
  "*Control X display used by ssh for X tunneling.
If non-nil and ssh is configured to enable remote X display forwarding,
the display of the current emacs frame will be used rather than the display
to which the emacs process was originally launched.  \(These may be
different if currently using a remote frame.\)"
  :type 'boolean
  :group 'ssh)

(defcustom ssh-host nil
  "*The name of the remote host.  This variable is buffer-local."
  :type '(choice (const nil) string)
  :group 'ssh)

(defcustom ssh-remote-user nil
  "*The username used on the remote host.
This variable is buffer-local and defaults to your local user name.
If ssh is invoked with the `-l' option to specify the remote username,
this variable is set from that."
  :type '(choice (const nil) string)
  :group 'ssh)

;; Initialize ssh mode map.
(defvar ssh-mode-map '())
(cond
 ((null ssh-mode-map)
  (setq ssh-mode-map (if (consp shell-mode-map)
                            (cons 'keymap shell-mode-map)
                          (copy-keymap shell-mode-map)))
  (define-key ssh-mode-map "\C-c\C-c" 'ssh-send-Ctrl-C)
  (define-key ssh-mode-map "\C-c\C-d" 'ssh-send-Ctrl-D)
  (define-key ssh-mode-map "\C-c\C-z" 'ssh-send-Ctrl-Z)
  (define-key ssh-mode-map "\C-c\C-\\" 'ssh-send-Ctrl-backslash)
  (define-key ssh-mode-map "\C-d" 'ssh-delchar-or-send-Ctrl-D)
  (define-key ssh-mode-map "\C-i" 'ssh-tab-or-complete)))


;;;###autoload (add-hook 'same-window-regexps "^\\*ssh-.*\\*\\(\\|<[0-9]+>\\)")

(defvar ssh-history nil)

;;;###autoload
(defun ssh (input-args &optional buffer)
  "Open a network login connection via `ssh' with args INPUT-ARGS.
INPUT-ARGS should start with a host name; it may also contain
other arguments for `ssh'.

Input is sent line-at-a-time to the remote connection.

Communication with the remote host is recorded in a buffer `*ssh-HOST*'
\(or `*ssh-USER@HOST*' if the remote username differs\).
If a prefix argument is given and the buffer `*ssh-HOST*' already exists,
a new buffer with a different connection will be made.

When called from a program, if the optional second argument BUFFER is
a string or buffer, it specifies the buffer to use.

The variable `ssh-program' contains the name of the actual program to
run.  It can be a relative or absolute path.

The variable `ssh-explicit-args' is a list of arguments to give to
the ssh when starting.  They are prepended to any arguments given in
INPUT-ARGS.

If the default value of `ssh-directory-tracking-mode' is t, then the
default directory in that buffer is set to a remote (FTP) file name to
access your home directory on the remote machine.  Occasionally this causes
an error, if you cannot access the home directory on that machine.  This
error is harmless as long as you don't try to use that default directory.

If `ssh-directory-tracking-mode' is neither t nor nil, then the default
directory is initially set up to your (local) home directory.
This is useful if the remote machine and your local machine
share the same files via NFS.  This is the default.

If you wish to change directory tracking styles during a session, use the
function `ssh-directory-tracking-mode' rather than simply setting the
variable.

The variable `ssh-x-display-follow-current-frame' can be used to specify
how ssh X display tunelling interacts with frames on remote displays."
  (interactive (list
		(read-from-minibuffer "ssh arguments (hostname first): "
				      nil nil nil 'ssh-history)
		current-prefix-arg))

  (let* ((process-connection-type ssh-process-connection-type)
         (args (ssh-parse-words input-args))
	 (host (car args))
	 (user (or (car (cdr (member "-l" args)))
                   (user-login-name)))
         (buffer-name (if (string= user (user-login-name))
                          (format "*ssh-%s*" host)
                        (format "*ssh-%s@%s*" user host)))
	 proc)

    (and ssh-explicit-args
         (setq args (append ssh-explicit-args args)))

    (cond ((null buffer))
	  ((stringp buffer)
	   (setq buffer-name buffer))
          ((bufferp buffer)
           (setq buffer-name (buffer-name buffer)))
          ((numberp buffer)
           (setq buffer-name (format "%s<%d>" buffer-name buffer)))
          (t
           (setq buffer-name (generate-new-buffer-name buffer-name))))

    (setq buffer (get-buffer-create buffer-name))
    (pop-to-buffer buffer-name)

    (cond
     ((comint-check-proc buffer-name))
     (t
      (ssh-with-check-display-override
       (lambda ()
         (comint-exec buffer buffer-name ssh-program nil args)))
      (setq proc (get-buffer-process buffer))
      ;; Set process-mark to point-max in case there is text in the
      ;; buffer from a previous exited process.
      (set-marker (process-mark proc) (point-max))

      (ssh-mode)
      (make-local-variable 'ssh-host)
      (setq ssh-host host)
      (make-local-variable 'ssh-remote-user)
      (setq ssh-remote-user user)

      (condition-case ()
          (cond ((eq ssh-directory-tracking-mode t)
                 ;; Do this here, rather than calling the tracking mode
                 ;; function, to avoid a gratuitous resync check; the default
                 ;; should be the user's home directory, be it local or remote.
                 (setq comint-file-name-prefix
                       (concat "/" ssh-remote-user "@" ssh-host ":"))
                 (cd-absolute comint-file-name-prefix))
                ((null ssh-directory-tracking-mode))
                (t
                 (cd-absolute (concat comint-file-name-prefix "~/"))))
        (error nil))))))

(put 'ssh-mode 'mode-class 'special)

(defun ssh-mode ()
  "Set major-mode for ssh sessions.
If `ssh-mode-hook' is set, run it."
  (interactive)
  (kill-all-local-variables)
  (shell-mode)
  (setq major-mode 'ssh-mode)
  (setq mode-name "ssh")
  (use-local-map ssh-mode-map)
  (setq shell-dirtrackp ssh-directory-tracking-mode)
  (make-local-variable 'comint-file-name-prefix)
  (run-hooks 'ssh-mode-hook))

(defun ssh-directory-tracking-mode (&optional prefix)
  "Do remote or local directory tracking, or disable entirely.

If called with no prefix argument or a unspecified prefix argument (just
``\\[universal-argument]'' with no number) do remote directory tracking via
ange-ftp.  If called as a function, give it no argument.

If called with a negative prefix argument, disable directory tracking
entirely.

If called with a positive, numeric prefix argument, e.g.
``\\[universal-argument] 1 M-x ssh-directory-tracking-mode\'',
then do directory tracking but assume the remote filesystem is the same as
the local system.  This only works in general if the remote machine and the
local one share the same directories (through NFS)."
  (interactive "P")
  (cond
   ((or (null prefix)
        (consp prefix))
    (setq ssh-directory-tracking-mode t)
    (setq shell-dirtrackp t)
    (setq comint-file-name-prefix
          (concat "/" ssh-remote-user "@" ssh-host ":")))
   ((< prefix 0)
    (setq ssh-directory-tracking-mode nil)
    (setq shell-dirtrackp nil))
   (t
    (setq ssh-directory-tracking-mode 'local)
    (setq comint-file-name-prefix "")
    (setq shell-dirtrackp t)))
  (cond
   (shell-dirtrackp
    (let* ((proc (get-buffer-process (current-buffer)))
           (proc-mark (process-mark proc))
           (current-input (buffer-substring proc-mark (point-max)))
           (orig-point (point))
           (offset (and (>= orig-point proc-mark)
                        (- (point-max) orig-point))))
      (unwind-protect
          (progn
            (delete-region proc-mark (point-max))
            (goto-char (point-max))
            (shell-resync-dirs))
        (goto-char proc-mark)
        (insert current-input)
        (if offset
            (goto-char (- (point-max) offset))
          (goto-char orig-point)))))))

;; Check to see if we should override the X display name that the ssh
;; process will inherit from the environment, which could affect where
;; remote clients will appear when using X forwarding.
;;
;; If ssh-x-display-follow-current-frame is non-nil, this function
;; overrides the process-environment display for the called function.
(defun ssh-with-check-display-override (fn)
  (let (frame-disp emacs-disp)
    (cond ((and ssh-x-display-follow-current-frame
                (eq window-system 'x)
                (setq frame-disp (cdr (assq 'display (frame-parameters))))
                (setq emacs-disp (getenv "DISPLAY"))
                ;; setenv is expensive, so don't do all that work if
                ;; there's no point.
                (not (string= frame-disp emacs-disp)))
           ;; Don't shadow process-environment completely because the
           ;; called function might legitimately want to modify other
           ;; environment variables permanently; just save and restore
           ;; original global display value.
           (unwind-protect
               (progn
                 (setenv "DISPLAY" frame-disp)
                 (funcall fn))
             (setenv "DISPLAY" emacs-disp)))
          (t
           (funcall fn)))))


;; rudimentary parser to split text into tokens.  Text in single or double
;; quotes is considered one token, though nested quoting may not work.
;;
;; e.g. (ssh-parse-words "host -o 'Compression no' -X")
;;      => ("host" "-o" "Compression no" "-X")
;;
;; Use a temporary buffer to do work because regexps against strings are
;; not powerful enough to check boundaries without excessive substring
;; consing (\` only matches at start of string, not at start of search).
(defun ssh-parse-words (line)
  (let ((list nil)
        (text nil)
        buf)
    (unwind-protect
        (save-match-data
          (save-excursion
            (setq buf (generate-new-buffer " *ssh-parse-words*"))
            (set-buffer buf)
            (insert line)
            (goto-char (point-min))
            (while (not (eobp))
              (setq text nil)
              (and (looking-at "\\`[ \t]+")
                   (narrow-to-region (match-end 0) (point-max)))
              (cond ((looking-at "\\`\\(['\"]\\)\\([^\\1]+\\)\\1")
                     (setq text (buffer-substring (match-beginning 2)
                                                  (match-end 2))))
                    ((looking-at "\\`[^ \t]+")
                     (setq text (buffer-substring (point-min) (match-end 0)))))
              (narrow-to-region (match-end 0) (point-max))
              (and text (setq list (cons text list))))))
      (kill-buffer buf))
    (nreverse list)))

(defun ssh-send-Ctrl-C ()
  (interactive)
  (process-send-string nil "\C-c"))

(defun ssh-send-Ctrl-D ()
  (interactive)
  (process-send-string nil "\C-d"))

(defun ssh-send-Ctrl-Z ()
  (interactive)
  (process-send-string nil "\C-z"))

(defun ssh-send-Ctrl-backslash ()
  (interactive)
  (process-send-string nil "\C-\\"))

(defun ssh-delchar-or-send-Ctrl-D (arg)
  "\
Delete ARG characters forward, or send a C-d to process if at end of buffer."
  (interactive "p")
  (if (eobp)
      (ssh-send-Ctrl-D)
    (delete-char arg)))

(defun ssh-tab-or-complete ()
  "Complete file name if doing directory tracking, or just insert TAB."
  (interactive)
  (if ssh-directory-tracking-mode
      (comint-dynamic-complete)
    (insert "\C-i")))

(provide 'ssh)

;;; ssh.el ends here
