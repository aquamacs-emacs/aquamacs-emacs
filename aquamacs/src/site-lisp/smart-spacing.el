;;; smart-spacing.el --- word-wise kill and yank  -*- lexical-binding: t -*-

;; Author: David Reitter, david.reitter@gmail.com
;; Maintainer: David Reitter
;; Keywords: aquamacs

;; This code is part of Aquamacs Emacs
;; http://aquamacs.org/
;; Copyright (C) 2009, 2014: David Reitter

;; (defcustom smart-spacing-when-killing-words nil
;;   "Delete extra spaces when killing words.
;; Affects commands `aquamacs-kill-word' and `aquamacs-backwards-kill-word'."
;;   :group 'convenience
;;   :group 'Aquamacs
;;   :type '(choice (const nil) (const t)))

(defcustom smart-spacing-excluded-modes '(nxml-mode)
  "Major modes in which `smart-spacing-mode' is never enabled.
Modes derived from these are excluded as well.  This matters for
modes derived from `text-mode', such as `nxml-mode', where smart
spacing would otherwise be turned on by `text-mode-hook'."
  :group 'convenience
  :type '(repeat symbol))

(define-minor-mode smart-spacing-mode
  "Smart spacing: word-wise kill&yank.
When this mode is enabled, kill and yank operations support
word-wise editing.  Afer killing (copying) a word or several
words, the text will be inserted as a full phrase when
yanking.That means that spaces around the word may be inserted
during yanking, and spaces and other word delimiters are removed
during killing as necessary to leave only one space between
words.

During killing, `smart-spacing-mode' behaves conservatively.  It
will never delete more than one extra space at a time.

The mode is not enabled in buffers whose major mode is in
`smart-spacing-excluded-modes'.

This feature is part of Aquamacs."
  :group 'convenience
  :lighter " Spc"

  (when (and smart-spacing-mode
             (apply #'derived-mode-p smart-spacing-excluded-modes))
    (setq smart-spacing-mode nil))
  ;; Use advice on the buffer local value of
  ;; filter-buffer-substring-function so that smart-spacing-mode
  ;; doesn't interfere with other functions that advise it (e.g.,
  ;; nxml-mode).
  (if smart-spacing-mode
      (add-function :around (local 'filter-buffer-substring-function)
		    #'smart-spacing-filter-buffer-substring)
    (remove-function (local 'filter-buffer-substring-function)
		     #'smart-spacing-filter-buffer-substring)))

(defun turn-on-smart-spacing-mode ()
  (interactive)
  (smart-spacing-mode 1))

(defun turn-off-smart-spacing-mode ()
  (interactive)
  (smart-spacing-mode 0))

(custom-add-option 'text-mode-hook 'turn-on-smart-spacing-mode)

(define-globalized-minor-mode
  global-smart-spacing-mode smart-spacing-mode
  turn-on-smart-spacing-mode
  :group 'convenience)

(defvar smart-spacing-rules
  '(("  " . (bidi . 1))
    ("--" . 1)
    (" ." . -1)
    (" )" . -1)
    ("( " . 1)
    (" :" . -1)
    (" ," . -1)
    (" ;" . -1)
    (" \"" . -1)
    ("\" " . 1)
    (" '" . -1)
    ("\n " . 1)
    (" " . 1) ; buffer boundary
    ;; ("\n\n" . "\n")
    )
  "Assoc list for smart spacing.
If key is at point after killing text, delete |value| chars to
the left or the right.  Negative value indicates deletion to the
left.  If value is a cons (xxx . num), then num characters will
be deleted either to the left or to the right, depending on where
the point is when the command is called.")

(defsubst smart-spacing-end-of-line (pos)
  (and (<= pos (point-max)) ;; buffer may be narrowed
       (> pos (point-min))
       (equal "\n" (buffer-substring-no-properties  (1- pos) pos))))

(defun smart-spacing-filter-buffer-substring (orig-fun beg end &optional delete)
  "Like `filter-buffer-substring', but add spaces around content if region is a phrase.
Used as `:around' advice on `filter-buffer-substring-function';
ORIG-FUN is the function being advised."
  (if smart-spacing-mode
      (let* ((from (min beg end)) (to (max beg end))
	     (point-at-end (eq (point) end))
	     (end-of-line (smart-spacing-end-of-line to))
	     (use-smart-string
	      (and
	       (not (string-prefix-p " " (buffer-name))) ; not an internal buffer
	       (smart-spacing-char-is-word-boundary (1- from) from)
	       (smart-spacing-char-is-word-boundary to (1+ to))))
	     ;; the following is destructive (side-effect).
	     ;; do after checking for word boundaries.
	     (string (funcall orig-fun beg end delete)))
	(when use-smart-string
	  (put-text-property 0 (length string)
			     'yank-handler
			     '(smart-spacing-yank-handler nil nil nil)
			     string)
	  (when (and (not end-of-line) delete)
	    (smart-spacing-remove-remaining-spaces from point-at-end)))
	string)
    ;; smart-spacing is off, but this function is called anyway:
    (funcall orig-fun beg end delete)))

(defun smart-spacing-delete-region (from to)
  (if (and smart-spacing-mode
	   (memq this-command '(cua-delete-region mouse-save-then-kill)))
      (let ((end-of-line (smart-spacing-end-of-line to)))
	(delete-region from to)
	(unless end-of-line
	  (smart-spacing-remove-remaining-spaces (min from to)
						 (eq (point) (max from to)))))
    (delete-region from to)))

(define-obsolete-function-alias 'smart-delete-region
  #'smart-spacing-delete-region "Aquamacs 4")

(defun smart-spacing-remove-remaining-spaces (pos point-at-end)
  "Remove remaining spaces.
Adheres to `smart-spacing-rules'.
If POINT-AT-END, behaves as if point was at then end of
a previously deleted region (now at POS).
Returns non-nil if spaces were deleted - 1 for forwards, -1 for backwards
or a cons with a combination."
  (unless (eq (point-min) (point-max))
    (let ((del (assoc (buffer-substring-no-properties
		       (max (point-min) (- pos 1))
		       (max (point-min) (min (1- (point-max)) (1+ pos))))
		      smart-spacing-rules)))
      (when del
	(setq del (cdr del))
	;; in some cases we want point to end up
	;; further to the left or to the right,
	;; depending on whether it was on the left or the right
	;; edge of the region
	(when (consp del)
	  (if point-at-end
	      (setq del (cdr del))
	    (setq del (- (cdr del)))))
	;; delete either to the left or to the right
	;; this deletion will keep point in the right place.
	(delete-region pos (+ del pos))
	del))))

(defun smart-spacing-char-is-word-boundary (pos &optional side)
  (or (< pos (point-min))
      (>= pos (point-max))
      (not (let ((str (buffer-substring-no-properties pos (1+ pos))))
	     (smart-spacing-string-is-word-boundary str side)))))

(defun smart-spacing-string-is-word-boundary (str side)
  (or (string-match "\\w" str)
      (if (eq side 'left)
          ;; punctuation typically requiring space to the right
          (member str '("." "," ":" "!" "?" ";" ")")))
      ;; to do -- this is language specific
      (if (eq side 'right) (equal str "("))))

(memq "." '("," "." ";" ")"))


(defun smart-spacing-yank-handler (string)
  (let ((opoint (point))) ;; experimental - is this right? DR
      (when  (and smart-spacing-mode
		  major-mode ; paranoia
		  (not (string-prefix-p " " (buffer-name)))) ; not an internal buffer
	(or (smart-spacing-char-is-word-boundary opoint 'right) ; to the right
	     (setq string (concat string " ")))
	(or (smart-spacing-char-is-word-boundary (1- opoint) 'left) ; to the left
	    (setq string (concat " " string))
	     ))
      (insert string)))

;; currently not advising backward-delete-char-untabity
;; or delete-char


(provide 'smart-spacing)

;;; smart-spacing.el ends here
