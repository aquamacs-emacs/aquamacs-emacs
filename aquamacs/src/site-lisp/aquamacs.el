;; Aquamacs Emacs startup

;; these defaults attempt to turn Emacs into a nice application for
;; operating systems with a graphical user interface.

;; This is the central file containing miscellaneous configuration
;; settings and commands not suitable for any other files.

;; General utility functions are found in aquamacs-tools.
;; Functions specific to the Mac are found in packages in macosx/

;; This file is loaded by site-start.el.
;;
;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs

;; This file is part of Aquamacs Emacs
;; http://aquamacs.org/

;; Aquamacs Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; Aquamacs Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Copyright (C) 2005-2019: David Reitter


(require 'aquamacs-tools)
(require 'aquamacs-macros)

;; various functions

(defun aquamacs-find-file (&optional filename)
  "Find an existing file or create a new buffer for it.
If `one-buffer-one-frame' is non-nil, a new frame is created to
contain the new buffer.
Interactively, the default if you just type RET is the current directory,
but the visited file name is available through the minibuffer history:
type M-n to pull it into the minibuffer.

You can visit files on remote machines by specifying something
like /ssh:SOME_REMOTE_MACHINE:FILE for the file name.  You can
also visit local files as a different user by specifying
/sudo::FILE for the file name.
See the Info node `(tramp)Filename Syntax' in the Tramp Info
manual, for more about this.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files.  You can
suppress wildcard expansion by setting `find-file-wildcards' to nil.

To visit a file without any kind of conversion and without
automatically choosing a major mode, use \\[find-file-literally]."
  (interactive)
  (if (or (not one-buffer-one-frame)
          filename
          (< (buffer-size (window-buffer (frame-first-window))) 2))
      (if filename
          (aquamacs-find-file-2 filename)
        (call-interactively 'aquamacs-find-file-2))
    ;; open new frame with empty buffer
    (new-empty-buffer nil 'fundamental-mode) ;;  'fundamental-mode
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn
            ;; the following will open the file in the given
            ;; frame, because the buffer shown is empty.
            (call-interactively 'aquamacs-find-file-2)
            (unless (eq (current-buffer) buf) ; get rid of old buffer
              (with-current-buffer (current-buffer)
                (with-selected-window (selected-window)
                  (kill-buffer buf)))))
            ;;(setq one-buffer-one-frame t))
        (progn
           (when (eq major-mode 'dired-mode)
             ;; do not obof variable itself buffer-local
             ;; other packaes will bind it temporarily with let
             ;; and then change the current buffer (e.g., by
             ;; creating a tab).
             ;; the behavior in that case is undefined.
             (set (make-local-variable 'one-buffer-one-frame-inhibit) t))
        (when (and (buffer-live-p buf)
                 (< (buffer-size) 2))		; for safety
            (with-current-buffer (current-buffer)
                (with-selected-window (selected-window)
                  (kill-buffer buf)))))))))

(defun aquamacs-find-file-2 (filename &optional wildcards)
  "Edit file FILENAME."

  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))

  (let ((value (find-file-noselect filename nil nil wildcards)))

    (if tabbar-mode
        (if (listp value)
            (mapcar 'switch-to-buffer-in-tab (nreverse value))
          (switch-to-buffer-in-tab value))
      (if (listp value)
          (mapcar 'switch-to-buffer (nreverse value))
        (switch-to-buffer value)))))


(defun aquamacs-recentf-show-basenames (l &optional no-dir)
    "Filter the list of menu elements L to show filenames sans directory.
When a filename is duplicated, it is appended a sequence number if
optional argument NO-DIR is non-nil, or its directory otherwise.
Separate paths from file names with --."
    (let (filtered-names filtered-list full name counters sufx)
      (dolist (elt l (nreverse filtered-list))
        (setq full (recentf-menu-element-value elt)
              name (file-name-nondirectory full))
        (if (not (member name filtered-names))
            (push name filtered-names)
          (if no-dir
              (if (setq sufx (assoc name counters))
                  (setcdr sufx (1+ (cdr sufx)))
                (setq sufx 1)
                (push (cons name sufx) counters))
            (setq sufx (file-name-directory full)))
          (setq name (format "%s -- %s" name sufx)))
        (push (recentf-make-menu-element name full) filtered-list))))

  ;; define a single command to be included in the recentf menu
  (defun recentf-clearlist ()
    "Remove all files from the recent list."
    (interactive)
    (setq recentf-list ())
    (setq file-name-history ()))


  ;; make sure there are no old customizations around
  ;; N.B.: if no customization file is present,
  ;; aquamacs-customization-version-id is 0 or nil
  ;; activated by aquamacs-load-preferences

  (defun aquamacs-activate-features-new-in-this-version ()
    ;; aquamacs-customization-version-id contains the version id
    ;; of aquamacs when the customization file was written
    ;; not present if Aquamacs was loaded with --no-site-file

    (when (and (not (equal init-file-user nil)) ;; no .emacs was read (-q option)
               (boundp 'aquamacs-customization-version-id)
               aquamacs-customization-version-id
               (> aquamacs-customization-version-id 0))

    (if (< aquamacs-customization-version-id 092.5)

        ;; make sure we fit frames
        (assq-set 'user-position nil 'default-frame-alist)

      )

    (if (< aquamacs-customization-version-id 094.1)
        (progn
          ;; change the customizations.el file to not contain the setq statement any more
          ;; old versions of Aquamacs (unclear which ones) wrote it to custom-file this way.
          (if (file-exists-p custom-file)
              (with-temp-file custom-file
                (insert-file-contents custom-file)
                (replace-regexp "(setq aquamacs-customization-version-id [0-9\\.]+)"
                                "" nil (point-min) (point-max))))
          ;; in the mode-spec styles, this is taken care of
          ;; anyways
          (setq default-frame-alist
                (assq-delete-all 'scroll-bar-width default-frame-alist))
          (setq special-display-frame-alist
                (assq-delete-all 'scroll-bar-width special-display-frame-alist))

          ))

    (when (< aquamacs-customization-version-id 131)
      ;; turn on tool bar only once to show the nice new tool bar
      (add-hook 'after-init-functions
                (lambda ()
                  (mapc (lambda (frame)
                          (modify-frame-parameters frame (list (cons 'tool-bar-lines 1))))
                        (frame-list)))))
    (when (< aquamacs-customization-version-id 140)
      (condition-case nil
          (unless (boundp 'aquamacs-140-custom-file-upgraded)
            (with-temp-buffer
              (princ "
;; for compatibility with older Aquamacs versions
 (defvar aquamacs-140-custom-file-upgraded t)
 (unless (fboundp 'auto-detect-longlines) (defun auto-detect-longlines () t))"
                     (current-buffer))
              (append-to-file (point-min) (point-max) custom-file)))
        (error nil)))

    (when (< aquamacs-customization-version-id 146)
      (condition-case nil
          (aq-replace-in-list text-mode-hook 'turn-on-longlines 'turn-on-word-wrap)
        (error nil)))

    (when (< aquamacs-customization-version-id 160)
      ;; did the user not explicitly set obof or tabbar?
      (when (eq tabbar-mode 'default)
        (custom-set-variables '(tabbar-mode nil)))
      (when (eq one-buffer-one-frame-mode 'default)
        (custom-set-variables '(one-buffer-one-frame-mode t))))

    ;; This function is no longer available.
    ;; (when (< aquamacs-customization-version-id 162)
    ;;   (aquamacs-import-frame-parameters-to-auto-faces))
    (when (< aquamacs-customization-version-id 208)
      (setq aquamacs-tool-bar-user-customization
            (mapcar (lambda (x)
                      (cons (logand 67108863 (car x)) (cdr x))) ;; ?\x3FFFFFF
                    aquamacs-tool-bar-user-customization))
      (setcar (or (member 'turn-on-word-wrap text-mode-hook) (cons nil nil)) 'set-word-wrap)
      (setcar (or (member 'turn-on-auto-fill text-mode-hook) (cons nil nil)) 'set-auto-fill))
    ;; (when (< aquamacs-customization-version-id 307)

    ;;   (when global-linum-mode
    ;;  (global-linum-mode 0)
    ;;  (global-nlinum-mode 1)))

;; Emacs 23 transition

;; add to default-frame-alist:  (internal-border-width . 0)

  ;; create fontsets where needed
    (mapc
     (lambda (font-string)
       (when (and font-string (string-match "^fontset-\\([a-z]+\\)\\([0-9]+\\)$" font-string))
         (let ((font (intern (match-string 1 font-string)))
               (size (list (string-to-number (match-string 2 font-string)))))

           (let ((font-alist  '((monaco "apple" "Monaco*" "medium" "r" "normal")
                                (lucida  "apple" "Lucida Grande*" "medium" "r" "normal")
                                (lucida_typewriter "apple" "Lucida sans typewrite*" "medium" "r" "normal")
                                (lucida_console  "apple" "Lucida console*" "medium" "r")
                                (courier "courier*" "medium" "r" nil)
                                (vera_mono "bitstream vera sans mono" "medium" "r" "normal"))))
             (require 'aquamacs-mac-fontsets)
             (apply #'create-aquamacs-fontset (append (cdr (assq font font-alist)) (list size) (list (symbol-name font))))))))
     (list (cdr-safe (assq 'font default-frame-alist)) (cdr-safe (assq 'font special-display-frame-alist))))


    (if (boundp 'mac-reverse-ctrl-meta)
        (message "Warning: `mac-reverse-ctrl-meta' is not used any more from
Aquamacs 0.9.7 on. This variable had been deprecated for several versions.
Use `mac-{control|command|option|function}-modifier' instead."))
    (if (boundp 'mac-command-key-is-meta)
        (message "Warning: `mac-command-key-is-meta' is not used any more from
Aquamacs 0.9.7 on. This variable had been deprecated for several versions.
Use `mac-command-modifier' instead."))


    (when (and (boundp 'mac-pass-option-to-system)
               (not (eq mac-pass-option-to-system 'deprecated)))
      (when mac-pass-option-to-system
           (setq mac-option-modifier-enabled-value mac-option-modifier)
           (setq mac-option-modifier nil))
      (if (> aquamacs-customization-version-id 096.0)
        (message "Warning: `mac-pass-option-to-system' is deprecated from
Aquamacs 0.9.7 on. `mac-option-modifier' has been set for you.")))

    ;; Do not use custom themes for Aquamacs
    ;; They cannot be undone anyway
    ;; (if (< aquamacs-customization-version-id 310.0)
    ;;     ;; make sure that the new aquamacs themes are part of custom-enabled-themes
    ;;     (when (not (memq 'aquamacs-frame-look custom-enabled-themes))
    ;;       (let ((cet custom-enabled-themes))
    ;;         (enable-theme 'aquamacs-frame-look)
    ;;         (mapc (lambda (x) (enable-theme x))
    ;;               (reverse cet))
    ;;       )))
    )
    )

(defun aquamacs-cua-warning ()
    (and (not cua-mode)
         (message
          "Warning: You have turned off `cua-mode' in your customizations
or init file. Without this mode, Aquamacs will behave in an
un-Mac-like way when you select text and copy&paste it.")))


(defun aquamacs-notice-user-settings ()
  "React to various user settings."

  (protect

   (unless noninteractive
     (unless (equal init-file-user nil) ;; no .emacs was read (-q option)
       (aquamacs-load-scratch-file))
     (aquamacs-cua-warning)
     (and (fboundp 'osx-key-mode-command-key-warning) (osx-key-mode-command-key-warning)))

   ;;   (if global-smart-spacing-mode
   ;;       (global-smart-spacing-mode 1))
   ;; turn on smart spacing in all text mode buffers
   (toggle-text-mode-smart-spacing
    (if (memq 'smart-spacing-mode text-mode-hook) 1 0))

   (if global-hl-line-mode
       (global-hl-line-mode 1))

   (if global-show-newlines-mode
       (global-show-newlines-mode 1))

   (if global-flyspell-mode
       (global-flyspell-mode 1))

   (if (eq tabbar-mode 'default)
       (customize-set-variable 'tabbar-mode t))
   (if (eq one-buffer-one-frame-mode 'default)
       (customize-set-variable 'one-buffer-one-frame-mode nil))

   ;; have fringe-mode reflect user settings
   (setq fringe-mode
         (cons (cdr-safe (assq 'left-fringe default-frame-alist))
               (cdr-safe (assq 'right-fringe default-frame-alist))))
   (if (equal fringe-mode '(nil)) (setq fringe-mode nil))

   ;; run this after the frames have been established
   ;; via default-frame-alist
   (run-with-idle-timer
    0.1 nil
    (lambda ()
      (mapc
       (lambda (frame)
         (let ((fs (frame-parameter frame 'fullscreen)))
           (when (memq fs '(fullboth fullheight fullwidth))
             (modify-frame-parameters
              frame (list (cons 'fullscreen nil)))
             (modify-frame-parameters
              frame (list (cons 'fullscreen fs)))
             (message
              (substitute-command-keys
               "Press \\[aquamacs-toggle-full-frame] to exit full screen editing.")))))
       (frame-list))))))

; (aquamacs-notice-user-settings)

;; redefine this
;; can be redefined at dump time
  (defun startup-echo-area-message ()
    (concat
     (propertize
       "Aquamacs is based on GNU Emacs, a part of the GNU/Linux system."
       'face (list :family "Lucida Grande" :height 140))
     ;;The GPL stipulates that the following message is shown.
     (propertize
      (substitute-command-keys " It is Free Software: you can improve and redistribute it under the GNU General Public License, version 3 or later. (C) 2019
Free Software Foundation, and D. Reitter. No Warranty.")
      'face (list :family "Lucida Grande" :height 110))))

;; (progn (message "%s" (startup-echo-area-message)) (sit-for 4))
;;
(defvar aquamacs-backup-custom-file nil)
; (setq aquamacs-backup-custom-file nil)
(defun aquamacs-backup-custom-file ()
  "Makes a backup of the customization file upon version upgrades."
  ;; Version upgrade?
  (when (and custom-file
             (file-exists-p custom-file)
             aquamacs-customization-version-id
             (numberp aquamacs-customization-version-id)
             (> aquamacs-customization-version-id 0) ; actually read?
             (> (floor (/ aquamacs-version-id 10.0))
                (floor (/ aquamacs-customization-version-id 10.0))))
    ;; do not delete a-b-c-f - repeated Save Options shouldn't delete it!
    (setq aquamacs-backup-custom-file
          (concat (file-name-directory custom-file)
                  (format "customizations.%.1f.el"
                          (/ (/ aquamacs-customization-version-id 10) 10.0))))
    (condition-case nil
        (progn
          (copy-file custom-file aquamacs-backup-custom-file
                     'overwrite 'keep-time 'preserve)
          (message "Previous customization file backed up to %s" aquamacs-backup-custom-file))
      (error (setq aquamacs-backup-custom-file nil)))))

(defadvice custom-save-variables (after backwards-compatibility activate)
  "Ensure that generated custom-files print a warning if loaded with older versions."
 ;; we expect to be

  ;; remove existing code if any
  (goto-char (point-min))
  (if (search-forward-regexp "\n;; Check custom-file compatibility[^!]*?;; End compatibility check\n" nil t)
      (replace-match ""))
  (custom-save-delete 'dummy-sym) ;; move to end of file

  (when (and aquamacs-backup-custom-file  ; do not insert cruft
             (file-exists-p aquamacs-backup-custom-file))
    (insert (format "
;; Check custom-file compatibility
\(when (and (boundp 'aquamacs-version-id)
           (< (floor (/ aquamacs-version-id 10))
           (floor (/ aquamacs-customization-version-id 10))))
  (defadvice frame-notice-user-settings (before show-version-warning activate)
    (defvar aquamacs-backup-custom-file nil \"Backup of `custom-file', if any.\")
    (setq aquamacs-backup-custom-file %S)
    (let ((msg \"Aquamacs options were saved by a more recent program version.
Errors may occur.  Save Options to overwrite the customization file. %s\"))
      (if window-system
          (x-popup-dialog t (list msg '(\"OK\" . nil) 'no-cancel) \"Warning\")
        (message msg)))))
;; End compatibility check
" aquamacs-backup-custom-file
    (if aquamacs-backup-custom-file
        (format "The original, older customization file was backed up to %s."
                aquamacs-backup-custom-file)
      "")))))

(defvar aquamacs-faces-changed)
(defun aquamacs-menu-bar-options-save (&optional maybe-save)
    "Save current values of Options menu items using Custom.
Return non-nil if options where saved.
MAYBE-SAVE t means: only save if needed"
    (interactive)
    (aquamacs-backup-custom-file)  ; call before updating version ID
    (setq aquamacs-customization-version-id aquamacs-version-id)
    (let ((need-save nil))
      ;; These are set with menu-bar-make-mm-toggle, which does not
      ;; put on a customized-value property.
      (dolist (elt aquamacs-menu-bar-options-to-save)
        (and (customize-mark-to-save elt)
             (setq need-save (cons elt need-save))))
      ;; These are set with `customize-set-variable'.
      (dolist (elt aquamacs-menu-bar-customize-options-to-save)
        (and (get elt 'customized-value)
             (customize-mark-to-save elt)
             (setq need-save (cons elt need-save))))
      ;; Save if we changed anything.
      (if (or aquamacs-faces-changed need-save (not maybe-save))
          (progn (custom-save-all)
                 (setq aquamacs-faces-changed nil)
                 (message "Options saved."))
        (message "There's no need to save your options."))
      need-save))
;; (aquamacs-menu-bar-changed-options)
;; aquamacs-customization-version-id
  (defun aquamacs-menu-bar-changed-options ()
    (let ((need-save nil))
      (dolist (elt aquamacs-menu-bar-options-to-save)
        (and (not (eq elt 'default-frame-alist)) ;; work around a bug: saved-value of this is incorrect for tool-bar-lines
             (aquamacs-variable-customized-p elt)
             (setq need-save (cons elt need-save))))
      (dolist (elt aquamacs-menu-bar-customize-options-to-save)
        (and (get elt 'customized-value)
             (aquamacs-variable-customized-p elt)
             (setq need-save (cons elt need-save))))
      need-save))

;; make sure the old variant isn't called, overwriting the
;; Aquamacs settings
(fset 'menu-bar-options-save 'aquamacs-menu-bar-options-save)

  (defcustom aquamacs-save-options-on-quit 'ask
    "If t, always save the options (from the options menu) when quitting.
If set to `ask' (default), the user is asked in case the options
have changed."
    :group 'Aquamacs
    :type '(choice (const nil)  (const ask) (const t)))

;; (aquamacs-variable-customized-p 'case-fold-search)
;; (aquamacs-variable-customized-p 'ns-alternate-modifier)
;; (aquamacs-variable-customized-p 'mac-option-modifier)
;; (aquamacs-variable-customized-p 'default-frame-alist)
;; (print (get 'default-frame-alist 'saved-value))
;; (aquamacs-variable-customized-p 'global-smart-spacing-mode)
;; (print (get 'global-smart-spacing-mode 'saved-value))

(defun aquamacs-variable-customized-p (symbol)
    "Returns t if variable SYMBOL has a different value from what was saved."
    (custom-load-symbol symbol)
    (let* ((get (or (get symbol 'custom-get) 'default-value))
           (value (funcall get symbol))
           (customized-value  (car-safe (get symbol 'customized-value)))
           (saved (or (get symbol 'saved-value)
                      ;; variable alias?  (saved value may be incorrect)
                      (if (indirect-variable symbol)
                          (get (indirect-variable symbol) 'saved-value))))

           (standard (get symbol 'standard-value))
           (standard2 (get symbol 'alternative-standard-value))
           (comment (get symbol 'customized-variable-comment)))

      (if (or (eq customized-value value) ;; otherwise it's rogue
              (and (eq customized-value nil) value)
              (eq (condition-case nil (eval customized-value) (error nil)) value))
          (let ((cmp (or saved
                         (condition-case nil
                             (eval (car standard))
                           (error nil))))
                (cmp2 (condition-case nil
                          (eval (car standard2))
                        (error nil))))
            (not (or (equal cmp (list (custom-quote value)))
                     (equal cmp2 (list (custom-quote value)))
                     ;; not quite clear why this is doubled
                    (equal (custom-quote cmp) (custom-quote value))
                    (equal (custom-quote cmp2) (custom-quote value))
                    (and (listp value) (string-match "-alist$" (symbol-name symbol)) ;; heuristic...
                         (condition-case nil
                             (equal (sort (copy-alist (eval (car cmp))) (lambda (x y) (string< (car x) (car y))))
                                    (sort (copy-alist value) (lambda (x y) (string< (car x) (car y)))))
                           (error nil)))))))))

;;  (filter-list (aquamacs-menu-bar-changed-options)
;;                        (list 'aquamacs-customization-version-id
;;				'smart-frame-prior-positions
;;				'aquamacs-additional-fontsets
;;				'initial-frame-alist
;;				'transient-mark-mode))
(defun aquamacs-ask-to-save-options ()
  "Checks if options need saving and allows to do that.
Returns t."
  (interactive)
  (condition-case nil
      (let* ((changed (aquamacs-menu-bar-changed-options)))
    (if (and (or aquamacs-faces-changed
                 (filter-list changed
                          (list 'aquamacs-customization-version-id
                                'smart-frame-prior-positions
                                'aquamacs-additional-fontsets
                                'initial-frame-alist
                                'transient-mark-mode)))
             ;; depends on return value of `aquamacs-menu-bar-options-save'
             ;; NOT implemented for the standard menu-bar-options-save!
             ;; ask user whether to accept these saved changes
             (if (eq aquamacs-save-options-on-quit 'ask)
                 (progn
                   ;;              (print changed)
                   (aquamacs-ask-for-confirmation "Options have changed - save them? \nYour customizations will be lost if you don't save them." nil "Save" "Don't Save"))
               aquamacs-save-options-on-quit))
        (aquamacs-menu-bar-options-save)))
    (error nil)) ;; in case of quit
  t)


(defun aquamacs-add-warning-to-dotemacs ()
  "Insert a warning into .emacs to use Preferences.el if custom-file overrides settings"
  ;; modeled after custom-save-all
  (let* ((filename "~/.emacs")  ;; support ~/emacs.d/init.el ??
         (old-buffer (find-buffer-visiting filename))
         old-buffer-name)
    (when (file-readable-p filename)
      (with-current-buffer (let ((find-file-visit-truename t))
                             (or old-buffer
                                 (let ((delay-mode-hooks t))
                                   (find-file-noselect filename))))
        ;; We'll save using file-precious-flag, so avoid destroying
        ;; symlinks.  (If we're not already visiting the buffer, this is
        ;; handled by find-file-visit-truename, above.)
        (when old-buffer
          (setq old-buffer-name (buffer-file-name))
          (set-visited-file-name (file-chase-links filename)))

        (unless (eq major-mode 'emacs-lisp-mode)
          (delay-mode-hooks (emacs-lisp-mode)))
        (let ((inhibit-read-only t)
              (print-length nil)
              (print-level nil))
          (let ((marker "Aquamacs custom-file warning:"))
            (beginning-of-buffer)
            (unless (search-forward marker nil t)
              ;; not found
              (beginning-of-buffer)
              (end-of-line)
              (while (nth 4 (syntax-ppss))
                (forward-line)
                (end-of-line))
              (beginning-of-line)
              (insert (aquamacs-get-custom-file-dotemacs-warning marker))
              )))
        (let ((file-precious-flag t))
          (save-buffer))
        (if old-buffer
            (progn
              (set-visited-file-name old-buffer-name)
              (set-buffer-modified-p nil))
          (kill-buffer (current-buffer)))))))


(defun aquamacs-get-custom-file-dotemacs-warning (marker)
  (let ((warning (format
";; ____________________________________________________________________________
;; %s
;; Warning: After loading this .emacs file, Aquamacs will also load
;; customizations from `custom-file' (customizations.el). Any settings there
;; will override those made here.
;; Consider moving your startup settings to the Preferences.el file, which
;; is loaded after `custom-file':
;; %s
;; _____________________________________________________________________________
" marker (car (last aquamacs-preference-files)))))
      warning))

;; (defun aquamacs-vars-changed-by-theme (theme)
;;   (mapcar
;;    (lambda (c)
;;      (when (eq 'theme-value (car c))
;;        (car (cdr c))))
;;    (get theme 'theme-settings)))

(defun aquamacs-save-buffers-kill-emacs (&optional arg)
    "Offer to save each buffer, then kill this Emacs process.
With prefix arg, silently save all file-visiting buffers, then kill.
Like `save-buffers-kill-emacs', except that it doesn't ask again
if modified buffers exist."
    (interactive "P")
    (aquamacs-create-preferences-dirs)
    (let ((saved-timer-idle-list timer-idle-list))
      (unwind-protect
          (progn
            ;; deactivate all idle timers so that
            ;; our prompt is not being overwritten by obnoxious
            ;; echo area messages
            ;; Caveat: this may impede useful functionality in "view"
            ;; when reviewing stuff.
            (setq timer-idle-list nil)
            (save-some-buffers arg t)
            (and (or (not (fboundp 'process-list))
                     ;; process-list is not defined on VMS.
                     (let ((processes (process-list))
                           active)
                       (while processes
                         (and (memq (process-status (car processes))
                                    '(run stop open listen))
                              (process-query-on-exit-flag (car processes))
                              (setq active t))
                         (setq processes (cdr processes)))
                       (or (not active)
                           (list-processes t)
                           (yes-or-no-p
                            "Active processes exist; kill them and exit anyway? "))))
                 ;; Query the user for other things, perhaps.
                 (run-hook-with-args-until-failure 'kill-emacs-query-functions)
               (or (null confirm-kill-emacs)
                   (funcall confirm-kill-emacs "Really exit Aquamacs? "))
               (kill-emacs)))
        (setq timer-idle-list saved-timer-idle-list))))

;; MOUSE --------------

;; workaround for people who still call this in their .emacs
  (defun mwheel-install ()
    (message "mwheel-install ignored in Aquamacs. Mouse wheel support is present by default.")
    t)

(defcustom mouse-wheel-progressive-decelerator 4
  "Decelerate progressive mouse wheel scrolling by this factor.
Must be 1 or larger."
  :group 'mouse
  :group 'Aquamacs
  :type 'float)

(defun aquamacs-wheel-scroll-down (&optional amt)
  "Scroll down (/ AMT mouse-wheel-progressive-decelerator) units"
  (if amt
      (scroll-down (/ amt mouse-wheel-progressive-decelerator))
    (scroll-down)))

(defun aquamacs-wheel-scroll-up (&optional amt)
  "Scroll up (/ AMT mouse-wheel-progressive-decelerator) units"
  (if amt
      (scroll-up (/ amt mouse-wheel-progressive-decelerator))
    (scroll-up)))

(aquamacs-set-defaults
 '((mouse-wheel-progressive-speed t)
   (mouse-wheel-scroll-amount (1 ((shift) . 0.5) ((control) . 0.2) ))
   (mwheel-scroll-up-function aquamacs-wheel-scroll-up)
   (mwheel-scroll-down-function aquamacs-wheel-scroll-down)))

;; -------------


;; restore *scratch*

(defcustom aquamacs-scratch-file
  "~/Library/Application Support/Aquamacs Emacs/scratch buffer"
  "File name to save the scratch file. Set to nil to not save it."
  :group 'Aquamacs
  :version "22.0"
  :type 'file)

;; read scratch file
;; (aquamacs-load-scratch-file)

;; this will prevent Aquamacs from automatically saving the buffer in
;; case the user saves it elsewhere (under a different name)
;; this method is not perfect: it is not called if the buffer is
;; manually associated with a different file.
(defun aquamacs-do-not-save-without-query-if-saved-elsewhere ()
  (unless (equal aquamacs-scratch-file buffer-file-name)
    (setq buffer-save-without-query nil)))

(defun aquamacs-load-scratch-file ()
  "Load the scratch buffer.
The *scratch* buffer is loaded from `aquamacs-scratch-file'.
No errors are signaled."
  (when (and aquamacs-scratch-file (get-buffer "*scratch*"))
    (aquamacs-set-defaults
     '((desktop-buffers-not-to-save
        "\\(^nn\\.a[0-9]+\\|\\.log\\|\\*scratch\\*\\)$")))
    (with-current-buffer "*scratch*"
      (condition-case nil
          (progn
            (let (;(coding-system-for-read 'utf-8)
                  (buffer-undo-list t))
              (if (file-exists-p aquamacs-scratch-file)
                  ;; if file unreadable, this will trip the condition-case
                  (insert-file-contents aquamacs-scratch-file
                                        nil nil nil 'replace))
              (set-buffer-modified-p nil))
            (setq buffer-undo-list nil)
            (setq buffer-file-name (expand-file-name aquamacs-scratch-file))
            (setq buffer-offer-save nil)
            ;; Buffer auto save caused severe problems on occasion:
            ;; Aquamacs would ask about the file being changed upon exit,
            ;; answering "no" would cancel exiting emacs,
            ;; answer "yes" would delete the file!
            (aquamacs-set-defaults
             `((recentf-exclude ,(append (list
                                          (expand-file-name aquamacs-scratch-file)
                                          ;; this is for LaTeX preview
                                          "\\.*/_region_.tex") recentf-exclude))))
            (setq buffer-save-without-query t)
            (put 'buffer-save-without-query 'permanent-local t)
;           (setq buffer-file-coding-system 'utf-8)
            (add-hook 'before-save-hook
                      'aquamacs-do-not-save-without-query-if-saved-elsewhere
                      nil 'local)
            (funcall initial-major-mode)) ; ensure mode hooks are run
      ;; we aso need to avoid asking whether to save this
      ;; do this here so that we never save the scratch file
      ;; if it hasn't been successfully loaded initially
      ;; (or if the file simply doesn't exist yet)
        (error (insert (format "Scratch file %s could not be read.\nThis buffer will not be saved automatically." aquamacs-scratch-file)) nil)))))


(defun toggle-text-mode-smart-spacing (&optional on)
  "Toggle `smart-spacing-mode' in `text-mode-hook'"
  (interactive)
  (let ((enable (cond ((eq on 1) t)
                      ((eq on 0) nil)
                      (t (not (memq 'smart-spacing-mode text-mode-hook))))))
   (if enable
        (add-hook 'text-mode-hook 'smart-spacing-mode)
      (remove-hook 'text-mode-hook 'smart-spacing-mode))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (if (or (derived-mode-p 'text-mode) text-mode-variant)
            (smart-spacing-mode (if enable 1 0)))))))

;; preload these

  (defcustom aquamacs-quick-yes-or-no-prompt-flag t
    "If non-nil, the user does not have to type in yes or no at
yes-or-no prompts - y or n will do."
    :group 'Aquamacs
    :version "22.0"
    :type 'boolean
    )
  (defvaralias 'aquamacs-quick-yes-or-no-prompt
    'aquamacs-quick-yes-or-no-prompt-flag)

  ; (yes-or-no-p "asda")
  ; (aquamacs-ask-for-confirmation "asd" t)

  (defun aquamacs-repl-yes-or-no-p (&rest args)
    "Like `old-yes-or-no-p' - use that function instead."
    (aquamacs-ask-for-confirmation (if (cdr args)
                                       (apply #'format args)
                                     (car args))
                                     t nil nil t))
  (defun aquamacs-y-or-n-p (&rest args)
    "Like `old-y-or-n-p' - use that function instead."
    (aquamacs-ask-for-confirmation (if (cdr args)
                                       (apply #'format args)
                                     (car args))
                                   nil nil nil t))

  (unless (fboundp 'old-yes-or-no-p)
    (fset 'old-yes-or-no-p (symbol-function 'yes-or-no-p)))
  (unless (fboundp 'old-y-or-n-p)
    (fset 'old-y-or-n-p (symbol-function 'y-or-n-p)))


  (fset 'y-or-n-p 'aquamacs-y-or-n-p)
  (fset 'yes-or-no-p 'aquamacs-repl-yes-or-no-p)

  ;; No more annoying bells all the time

  (defun aquamacs-bell ()
    (let ((ring-bell-function nil))
      (ding)))

  (aquamacs-set-defaults
   '((ring-bell-function (lambda () (message "")))
     )
   )

  ;; this can be turned off in .emacs via
  ;; (setq ring-bell-function nil)

  (defcustom aquamacs-ring-bell-on-error-flag nil
    "If non-nil, Aquamacs gives an audio signal in cases of error, regardless of ``ring-bell-function''."
    :group 'Aquamacs
    :version "22.0"
    :type 'boolean
    )
  (defvaralias  'aquamacs-ring-bell-on-error 'aquamacs-ring-bell-on-error-flag)



  ;; Find-file may open a new frame
  (if (running-on-a-mac-p)
      (global-set-key [remap find-file] 'aquamacs-find-file))


  (defun font-exists-p (fontorfontset)
    (condition-case nil
        (or
         (font-info fontorfontset)
         (fontset-info fontorfontset)
         )
      (error nil)
      )
    )

  ;; Mode-Line Faces
  ;; face (defined and) applied using pretty-modeline.patch

  (defun aquamacs-set-modeline-faces (&optional theme)
    (set-face-attribute 'mode-line nil
                        :inherit 'aquamacs-variable-width
                        :weight 'normal
                        :width 'normal
                        :slant 'normal
                        :underline nil
                        :strike-through nil)
    (set-face-attribute 'mode-line-inactive nil
                        :inherit 'aquamacs-variable-width
                        :weight 'normal
                        :width 'normal
                        :slant 'normal
                        :underline nil
                        :strike-through nil)
    (set-face-attribute 'mode-line-flags nil
                        :family "Monaco"))
  (defvar color-theme-install-hook nil)
  ;; some themes override mode-line faces;
  ;; while they should be able to set colors, they should not remove :family or :inherit attribues
  (add-hook 'color-theme-install-hook 'aquamacs-set-modeline-faces)
  (aquamacs-set-modeline-faces)

  ;; Give the Echo Area(s) a face
  (defun aquamacs-setup-echo-areas (&optional frame)
    (mapc (lambda (bname)
            (with-current-buffer (get-buffer-create bname)
                (set (make-local-variable 'face-remapping-alist)
                     (cons '(default . echo-area)
                           (default-value 'face-remapping-alist)))))
          '(" *Echo Area 0*" " *Echo Area 1*" " *Echo Area 2*")))
  (defun aquamacs-set-minibuffer-face ()
    (set (make-local-variable 'face-remapping-alist)
         (cons '(default . minibuffer)
               face-remapping-alist)))

  (add-hook 'after-make-frame-functions 'aquamacs-setup-echo-areas)
  (add-hook 'after-init-hook 'aquamacs-setup-echo-areas)
  (add-hook 'minibuffer-setup-hook 'aquamacs-set-minibuffer-face)
  (aquamacs-setup-echo-areas)


;; the following code is not preloaded
(defun aquamacs-setup ()


  (require 'aquamacs-tools)
  (ats "aquamacs-tools done")
  ;; Stop Emacs from asking for "y-e-s", when a "y" will do.

  (require 'mac-extra-functions)
  (aquamacs-mac-initialize) ;; call at runtime only
  (defvar aquamacs-mac-application-bundle-directory
    "This is actually defined in mac-extra-functions.el")

  (ats "aquamacs-mac-initialize done")

  ;; please ring the bell when there is a real error
  (defadvice error (around ring-bell (&rest args) activate protect)

    (if aquamacs-ring-bell-on-error-flag
        (progn
          (aquamacs-bell)
          ad-do-it)
      ;; else
      ad-do-it))


  ;; tabbar needs to be defined before osxkeys
  (if (running-on-a-mac-p)
      ;; force loading of our own toolbar
      (let ((load-path (cons (expand-file-name "../lisp/aquamacs/tabbar" data-directory) load-path)))
        (require 'aquamacs-tabbar))
    ;; aquamacs-tabbar doesn't work without windows
    (require 'tabbar))

  (aquamacs-set-defaults `((tabbar-mode ,(if (running-on-a-mac-p) t nil))))
  (setq tabbar-mode 'default) ;; will be set later on

  ;; Mac OS X specific stuff


  (ats "osx_defaults ...")
  (require 'osx_defaults) ;; always load this to define various things

  (when (running-on-a-mac-p)
    (ats "setup...")
    (aquamacs-osx-defaults-setup))

  (ats "osx_defaults done")


;; POST-LOAD-PATH adjustment
;; from here on, the load path has been altered to include the user's
;; own libraries (before our own).  Users may replace libraries
;; that we load using "require" and "load".

;; Page scrolling


  (ats "aquamacs-editing")
  (require 'aquamacs-editing)

  ;; always move visually left/right with left-char/right-char
  (aquamacs-set-defaults '((visual-order-cursor-movement t)))

  ;; overwrites CUA stuff
  (global-set-key [remap scroll-up]           'aquamacs-page-down)
  (global-set-key [remap cua-scroll-up]       'aquamacs-page-down)
  (global-set-key [remap scroll-up-mark]      'aquamacs-page-down-extend-region)
  (global-set-key [next]              'aquamacs-page-down)
  (global-set-key [\S-next]           'aquamacs-page-down-extend-region)
  (global-set-key [\M-up]             'aquamacs-page-up)
  (global-set-key [remap scroll-down]         'aquamacs-page-up)
  (global-set-key [remap cua-scroll-down]             'aquamacs-page-up)
  (global-set-key [remap scroll-down-mark]      'aquamacs-page-up-extend-region)
  (global-set-key [prior]             'aquamacs-page-up)
  (global-set-key [\S-prior]          'aquamacs-page-up-extend-region)

  ;; was here in 0.9.5, taken out
  ;;(global-set-key [C-up]        'pager-row-up)
  ;;(global-set-key [C-down]      'pager-row-down)

  (ats "aquamacs-menu ...")
  (require 'aquamacs-menu) ; before osx_defaults

  (require 'aquamacs-autoface-mode)

  (menu-bar-update-buffers) ;; update Buffers menu now
  (aquamacs-update-menu t) ;; initial setup of the menu

  (ats "aquamacs-menu done")
  (require 'aquamacs-bug) ;; successfully send bug reports on the Mac
  (ats "aquamacs-bug done")

  ;; visual line navigation
  (require 'visual-line)

  (require 'saveplace)
(aquamacs-set-defaults
   `((line-move-visual arrow-keys-only)
     (text-mode-hook (smart-spacing-mode auto-detect-wrap))
     (save-place-limit 500) ;; speed on quit
     (save-place-forget-unreadable-files nil) ;; too slow
     (mail-setup-with-from nil)
     (send-mail-function mailclient-send-it)
                                        ; Colorized fonts
                                        ; Turn on font-lock in all modes that support it
     (global-font-lock-mode t)

     (font-lock-maximum-decoration t)

                                        ; Make Text mode the default mode for new buffers
                                        ; turn on Auto Fill mode automatically in Text mode
     (initial-major-mode text-mode)

     (frame-resize-pixelwise t)
     (window-resize-pixelwise t)
                                        ; scroll just one line when hitting the bottom of the window
     (scroll-step 1)
     (scroll-conservatively 99)
     ;; Start scrolling when 2 lines from top/bottom
     (scroll-margin 0)
     (visual-scroll-margin 2)

     (tramp-verbose 3)                  ;; don't annoy us  (3 is the default normally anyway)

     ;; workaround for a bug in viper cursor color saving mechanism
     (viper-replace-overlay-cursor-color "red4")

     ;; no flash instead of that annoying bell
     (visible-bell nil)

                                        ; Display the column number of the point in the mode line
     (column-number-mode t)

     ;; Isearch
     (search-default-mode char-fold-to-regexp)  ; flexible character matching.

     ;; Do not ask to save abbrevs table - just do it

     (save-abbrevs silently)

     ;; Do not automatically add newlines on (page) down
     (next-line-add-newlines nil)

     ;; Show directories in buffer names when needed
     (buffers-menu-show-directories t)

     ;; Do not complain when a minibuffer is still open somewhere

     (enable-recursive-minibuffers t)

     ;; menu strings
     (buffer-menu-modified-string "\u25CF")
     (buffer-menu-read-only-string "(read-only)")

     ;; do not allow user to mess with minibuffer prompt

     (minibuffer-prompt-properties
      ,(plist-put minibuffer-prompt-properties
                  'cursor-intangible t))))

;; on by default
(if (and (fboundp 'mac-inline-input-method-mode)
         (not (boundp 'mac-inline-input-method-missing))
         (running-on-a-mac-p))
    (progn
      (aquamacs-set-defaults '((mac-inline-input-method-mode t))))
  ;; otherwise, redefine the mode function
  ;; so that it won't be called when loading custom-file.
  (defvar mac-inline-input-method-missing t)
  (defvar mac-inline-input-method-mode nil)
  (defun mac-inline-input-method-mode ( &optional onoff)
      (interactive)
      (message
       "mac-input-method-mode not available without window system.")))


  ;; set a nntp server if there's none
  (if (getenv "NNTPSERVER") ;; (gnus-getenv-nntpserver)
      nil
    (aquamacs-set-defaults '((setq gnus-select-method
                                   '(nntp "news.readfreenews.net")))))

;; activate the modes now
  (save-place-mode 1)
  (global-font-lock-mode 1)
  (column-number-mode 1)

;; ------- Frames (OSX Windows) ----------

  (ats "before view")
  (require 'view)
  ;; redefine view-buffer
  (defun view-buffer (buffer &optional exit-action)
    "View BUFFER in View mode, returning to previous buffer when done.
Emacs commands editing the buffer contents are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type H or h while viewing.

This command runs the normal hook `view-mode-hook'.

Optional argument EXIT-ACTION is either nil or a function with buffer as
argument.  This function is called when finished viewing buffer.
Use this argument instead of explicitly setting `view-exit-action'."

    (interactive "bView buffer: ")
    (let ((undo-window (list (window-buffer) (window-start) (window-point)))
          (obof one-buffer-one-frame) ;;may be buffer-local!
          )
      (switch-to-buffer buffer)
      (view-mode-enter (cons (selected-window) (cons (cons nil undo-window) obof))
                       exit-action)))


(require 'color-theme-autoloads)


;; add aquamacs themes to load path

;; Do not load any themes for now.  Future development might use them.

;; (add-to-list 'custom-theme-load-path
;;              (concat (mac-resources-path)
;;                      "lisp/aquamacs/themes"))
;; (load-theme 'aquamacs-frame-look 'no-confirm)
;; (enable-theme 'aquamacs-frame-look)

(unless (equal init-file-user nil)
  (aquamacs-add-warning-to-dotemacs))


;; follow mouse autoload
(autoload 'turn-on-follow-mouse "follow-mouse.el"   "Moving the mouse will automatically select the window under it" 'interactive nil)
(autoload 'turn-off-follow-mouse "follow-mouse.el"   "Moving the mouse will not automatically select the window under it" 'interactive nil)
(autoload 'toggle-follow-mouse "follow-mouse.el"   "Toggle whether moving the mouse automatically selects the window under it" 'interactive nil)

;; ensure calc is loaded from the right place (and not from calc.el)
(autoload 'calc (concat (file-name-directory (locate-library "select")) "calc/calc")  "The Emacs Calculator." t)


(provide 'drews_init)	; migration from 0.9.1 (require in customizations)

;; default frame positioning  (without smart-frame-positioning)
;; for normal windows
;; for special windows, the user can set and save things
;; also, we don't want the initial frame to move around
(setq default-frame-alist (assq-delete-all 'top default-frame-alist))
(setq default-frame-alist (assq-delete-all 'left default-frame-alist))
(setq default-frame-alist (assq-delete-all 'width default-frame-alist))
(setq default-frame-alist (assq-delete-all 'height default-frame-alist))
;; sensible defaults for the position of the special windows
;; (in case user turns them off)
(assq-set 'top 30 'special-display-frame-alist)  ;; TO DO: replace with display-buffer-alist
(assq-set 'left '(- 0) 'special-display-frame-alist)
(assq-set 'height 30 'special-display-frame-alist)
(assq-set 'width 75 'special-display-frame-alist)
(assq-set 'user-position nil 'special-display-frame-alist)

(assq-set 'cursor-color "Red" 'default-frame-alist)


;; set some defaults which will be set by other functions anyways
;; just so we save them to standard-value

;; default-frame-alist settings are defaults in Aquamacs
(aquamacs-set-defaults `((default-frame-alist ,default-frame-alist)
                           (special-display-frame-alist ,special-display-frame-alist)))

(ats "fontsets done")

;; here would be the place to turn on mode-spec styles AFTER setting default-frame-alist
;; so everything is copied over to the 'default style as appropriate
;; mode-specific font settings
;;  if turned on, default-frame-alist should be empty now

;; local toolbars
(defun tool-bar-enabled-p (&optional frame)
  "Evaluates to non-nil if the tool-bar is present
in frame FRAME. If FRAME is nil, the function applies
to the selected frame."
  (> (or (frame-parameter frame 'tool-bar-lines) 0) 0))

(defun toggle-tool-bar-here ()
  (interactive)
  (modify-frame-parameters
   nil
   (list (cons 'tool-bar-lines
               (if (tool-bar-enabled-p)
                   0 1)))))


;; ----------- SMART FRAME POSITIONING ----------------

(when (running-on-a-mac-p)

  (require 'smart-frame-positioning)
  (ats "sfp loaded")

  (aquamacs-set-defaults
   '((smart-frame-positioning-mode t)
     ( smart-frame-positioning-enforce t))) ;; and enforce it

  (smart-frame-positioning-mode t) ;; and turn on!
  (ats "smart-frame-positioning-mode done"))

;; ----------- ONE BUFFER ONE FRAME ----------------

(ats "loading obof")
(require 'one-buffer-one-frame)
;; necessary to ensure the value is saved with the Options
;; (setting the default)
(aquamacs-set-defaults '((one-buffer-one-frame-mode nil)))
;; so we can detect changes to the variable by the user.
(setq one-buffer-one-frame-mode 'default)
;; will be set later on
(ats "obof done.")

;; ----------- MISC STUFF ----------------

(put 'upcase-region 'disabled nil)


;; -------- MOUSE BEHAVIOR / SELECTION -------------

;; we activate CUA mode again but disable the keys
;; thanks to Lawrence Akka for hints on the following section
;; ;; not needed


(aquamacs-set-defaults '((mouse-drag-copy-region t) ;; needed in Emacs 24
                         (cua-use-hyper-key only) ;;this avoids shift-return
                         (cua-enable-cua-keys nil)))

;; enable cua-keep-region-after-copy only for the mac like commands
(defadvice cua-copy-region (around keep-region activate)
  (if (eq this-original-command 'clipboard-kill-ring-save)
      (let ((cua-keep-region-after-copy t))
        ad-do-it)
    ;; respect user's setting of cua-keep-region-after-copy for M-w etc.
    ad-do-it))

(cua-mode 1) ;; this goes first (so we can overwrite the settings)


(add-hook 'after-init-hook 'aquamacs-notice-user-settings)

;; give context menus on right click
;; if mac-wh is nil, we need the following
;;  (define-key mode-line-major-mode-keymap [mode-line down-mouse-1]
;;     'mouse-major-mode-menu)
;;   (define-key mode-line-major-mode-keymap [mode-line mouse-2]
;;     'mode-line-mode-menu-1)
;;   (define-key mode-line-major-mode-keymap [mode-line down-mouse-3]
;;     'describe-mode)
;;   (define-key mode-line-minor-mode-keymap [header-line down-mouse-3]
;;     'mode-line-minor-mode-help)
;;   (define-key mode-line-minor-mode-keymap [mode-line down-mouse-3]
;;     'mode-line-minor-mode-help)
;;   (define-key mode-line-minor-mode-keymap [header-line mouse-2]
;;     'mode-line-mode-menu-1)
;;   (define-key mode-line-minor-mode-keymap [mode-line mouse-2]
;;     'mode-line-mode-menu-1)



;; do not use [ ... ] notation - pure space allocation!

;; enable flyspell's corrections on mouse-3
(setq flyspell-mouse-map
      (let ((map (make-sparse-keymap)))
        (define-key map (if (featurep 'xemacs) [button3] [down-mouse-3])
          #'flyspell-correct-word)
        map))

(defun mouse-save (click)
  "Set the region according to CLICK.
Same as `mouse-save-then-kill', except that the region will not
be killed."
  (interactive "e")
  (let ((last-command)) ;; do not kill
    (mouse-save-then-kill click)))

(defun mouse-save-secondary (click)
  "Set the secondary selection according to CLICK.
Same as `mouse-secondary-save-then-kill', except that the region will not
be killed."
  (interactive "e")
  (let ((last-command)) ;; do not kill
    (mouse-secondary-save-then-kill click)))


(global-set-key (vector '(shift down-mouse-1)) 'mouse-save)
;; does not work:
;; (global-set-key (vector '(shift drag-mouse-1)) 'mouse-save)
(global-set-key (vector `(meta shift down-mouse-1)) 'mouse-save-secondary)

;; applications on OS X don't display a splash screen
(aquamacs-set-defaults '((inhibit-startup-message t)))

(if (running-on-a-mac-p)
      (defun use-fancy-splash-screens-p () t))

;; overwrite
(defun first-that (predicate list)
  "Returns first element from LIST that satisfies PREDICATE"
  (while (and list (not (funcall predicate (car list))))
    (setq list (cdr list)))
  (car list))


(defun fancy-splash-frame ()
  "Return the frame to use for the fancy splash screen.
Returning non-nil does not mean we should necessarily
use the fancy splash screen, but if we do use it,
we put it on this frame."
  (make-frame
   `((name . "About Aquamacs Emacs")
     (font . ,(first-that 'font-exists-p
                '("-apple-lucida grande-medium-r-normal--0-0-0-0-m-0-mac-roman"
                "-*-Lucida Grande-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1")))
     (width . 75) (height . 40) (minibuffer . t)
     (background-color . "White")
     (foreground-color . "Black") (tool-bar-lines . 0)
     (vertical-scroll-bars . auto)
     (horizontal-scroll-bars . nil)
     (left-fringe . 5) (right-fringe . 0)
     (internal-border-width . 0) (unsplittable . t))))


;; -------- UI DEFAULTS -------------

;; the following causes not-so-good things to happen.
;; (defun fancy-splash-default-action () nil)

(require 'savehist) ;; because we configure and activate it

(aquamacs-set-defaults
 '((fancy-splash-image "aquamacs-logo.tiff")
   (fancy-splash-max-time 3000)))

(aquamacs-set-defaults
 '(
   ;; scratch buffer should be empty
   ;; the philosophy is: don't give users any text to read to get started!
   (initial-scratch-message nil)
   (focus-follows-mouse nil) ;; do not mess with user's mouse!
   (resize-mini-windows t)
   (show-paren-mode t)
   (blink-cursor-mode t)
   (cursor-type (bar . 2))
   ;; on modern systems, loading files doesn't take so long any more.
   (large-file-warning-threshold 20000000)
   ;; show unfinished key inputs early
   (echo-keystrokes 0.1)
   ;; save minibuffer history
   (savehist-mode 1)
   ;; do not create backups
   (make-backup-files nil)
   ;; higher undo limit
   (undo-outer-limit 12000000)
   (undo-limit 80000)
   ))

;; do not skip redisplays - tabbar (header line) and other things
;; are slower in the NS port.
(when (eq initial-window-system 'ns)
  (aquamacs-set-defaults
   '((redisplay-dont-pause t))))

;; -------- PACKAGES -------------

(aquamacs-set-defaults
 '((package-archives ;; is part of `package' (not loaded)
    (("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")))))

;; -------- EDITING MODES AND UI -------------

;; activate some editing modes
(show-paren-mode 1)
(blink-cursor-mode 1)
(savehist-mode 1)

;; should not be needed - done by a-s-d above
;; (set-default 'cursor-type '(bar . 2))


;;; for initial buffer
(defun aquamacs-turn-on-buffer-offer-save-in-scratch ()
    (protect
     (if (get-buffer "*scratch*")
         (with-current-buffer "*scratch*"
           (setq buffer-offer-save t)))))
(add-hook 'after-init-hook 'aquamacs-turn-on-buffer-offer-save-in-scratch)

;; -------- CUSTOMIZATION -------------


;; Define customization group
;; add items that aren't Aquamacs defcustoms
(defgroup Aquamacs
  '(
    (smart-frame-positioning-enforce custom-variable)
    (smart-frame-positioning-mode custom-variable)
    (ns-alternate-modifier  custom-variable)
    (ns-right-alternate-modifier  custom-variable)
    (ns-control-modifier  custom-variable)
    (ns-right-control-modifier  custom-variable)
    (ns-function-modifier  custom-variable)
    ;;      (mac-pass-control-to-system  custom-variable)
    (ns-command-modifier  custom-variable)
    (ns-right-command-modifier  custom-variable)
    ;; (mac-pass-command-to-system  custom-variable)
    (ns-emulate-three-button-mouse  custom-variable)
    (ns-antialias-text  custom-variable)
    (select-enable-clipboard  custom-variable)
    (special-display-regexps custom-variable)
    )
  "Options specific to Aquamacs Emacs.
Some of these customizations values exist in GNU Emacs as well,
but have default values different from those in GNU
Emacs. Customize them to achieve the GNU Emacs behavior.  Note
that not all customization variables with differing defaults are
listed here."
  :group 'emacs)

(defvar aquamacs-menu-bar-options-to-save
  (append '(global-linum-mode
            column-number-mode
            size-indication-mode
            tool-bar-mode
            global-hl-line-mode
            global-show-newlines-mode
            global-flyspell-mode
            show-paren-mode
            transient-mark-mode
            global-font-lock-mode
            display-time-mode
            display-battery-mode
            one-buffer-one-frame-mode
            visual-line-mode ; set by line wrapping menu functions
            aquamacs-autoface-mode
            aquamacs-tool-bar-user-customization
            ns-tool-bar-display-mode ;; can be set through GUI by user
            ns-tool-bar-size-mode ;; can be set through GUI by user
            ;; default-frame-alist should not be saved in customizations
            ;;   it interferes with themes.
;;;          do not save initial-frame-alist - it is stored by smart-frame-positions
;;;  to do: frame-notice-user-settings should use default-frame-alist in addition to
;;; initial-frame-alist, so "adopt frame parameters as default" should work.
            ns-alternate-modifier
            ns-right-alternate-modifier
            ns-right-command-modifier)
          (and (boundp 'aquamacs-additional-fontsets)
               '(aquamacs-additional-fontsets))
          ;; retain for backwards compatibility
          (mapcar (lambda (x)
                    (emkm-name (car x)))
                  (and (boundp 'emulate-mac-keyboard-mode-maps)
                       emulate-mac-keyboard-mode-maps))))
(put 'ns-tool-bar-display-mode 'alternative-standard-value '((quote both)))   ; default is nil, but that means C code sets it to system's default.
(put 'ns-tool-bar-size-mode 'alternative-standard-value '((quote regular)))   ; default is nil, but that means C code sets it to system's default.

(defvar aquamacs-menu-bar-customize-options-to-save
  '(scroll-bar-mode
    debug-on-quit debug-on-error
    tooltip-mode
    fringe-mode
    uniquify-buffer-name-style
    ;; fringe-mode and tool-bar-mode saved in default-frame-alist
    indicate-empty-lines indicate-buffer-boundaries
    case-fold-search
    current-language-environment default-input-method
    ;; Saving `text-mode-hook' is somewhat questionable,
    ;; as we might get more than we bargain for, if
    ;; other code may has added hooks as well.
    ;; Nonetheless, not saving it would like be confuse
    ;; more often.
    ;; -- Per Abrahamsen <abraham@dina.kvl.dk> 2002-02-11.
    text-mode-hook
    word-wrap truncate-lines line-move-visual visual-line-mode auto-fill-function fringe-indicator-alist
    blink-cursor-mode
    aquamacs-customization-version-id
    mac-print-monochrome-mode
    make-backup-files
    tabbar-mode))


(add-hook 'kill-emacs-query-functions 'aquamacs-ask-to-save-options)

;; (defun raise-frame-and-make-visible ()
;;   (interactive)
;;   ;; (if (not (eq t (frame-visible-p (selected-frame))))
;;   (make-frame-visible)
;;   (raise-frame))


(when (eq initial-window-system 'ns)
  (global-set-key [ns-application-open-untitled] 'new-empty-buffer-other-frame)
  (global-set-key [ns-application-activated] 'ignore)
  (global-set-key [ns-power-off] 'aquamacs-save-buffers-kill-emacs))

(global-set-key [remap save-buffers-kill-emacs]
                'aquamacs-save-buffers-kill-emacs)


;; this is initialized to the current version
;; it'll be overwritten by whatever is in the customization file
(defvar aquamacs-customization-version-id 0 "Version number of loaded `custom-file'.
Corresponds to the version number of Aquamacs (`aquamacs-version-id') used
to write the `custom-file'.")
;; the following ensures that it gets saved
;; as customized variable.
(customize-set-variable 'aquamacs-customization-version-id
                        aquamacs-customization-version-id)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Temporary stuff for releases according to admin/FOR-RELEASE

(setq undo-ask-before-discard nil)
;; http://sourceforge.net/tracker/index.php?func=detail&aid=1295333&group_id=138078&atid=740475


;; workarounds for current bugs
;; can't get rid of the menu bar on a Mac
(easy-menu-remove-item global-map
                       '("menu-bar" "options" "showhide") 'menu-bar-mode)

;; can't show a frame on a different display
(easy-menu-remove-item global-map
                       '("menu-bar" "file") 'make-frame-on-display)

;; the check for crashes must be done BEFORE checking for updates
;; as the latter updates the .id file.
(add-hook 'after-init-hook 'check-for-aquamacs-crashes 'append)

(require 'check-for-updates)
;; via hook so it can be turned off
(add-hook 'after-init-hook 'aquamacs-check-for-updates-if-necessary 'append)

;; tool-bar-setup was here


(ats "aquamacs-tool-bar-setup ...")
(when (running-on-a-mac-p)
  (require 'aquamacs-tool-bar)
  (aquamacs-tool-bar-setup))
(ats "aquamacs-tool-bar-setup done")


;; Session restoration

(require 'revive) ;; is usually preloaded
(revive:setup)
(unless (boundp 'revive:app-restore-path)
  (message "Inadequate version of Revive package loaded: %s.
The one that comes with Aquamacs will restore tabs and all frames and supports post-OSX 10.7 application resume."
           (locate-library "revive")))
;; this will bind all necessary


;; finish reading environment vars

(when (running-on-a-mac-p)
  (unless (mac-read-environment-vars-from-shell-2)
    (message "Warning - environment variable reading delayed.")
                                        ; wait one second
    ;; we should not delay this via run-with-timer, because
    ;; some code may depend on the PATH (exec-path!) being set correctly,
    ;; for example the (autoloaded!) ispell package.
    (sit-for 1)
    (mac-read-environment-vars-from-shell-2)))

) ;; aquamacs-setup

(provide 'aquamacs)
