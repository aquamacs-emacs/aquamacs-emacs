;;; -*- Emacs-Lisp -*-
;;; <plaintext>
;;; revive.el: Resume Emacs and Aquamacs.
;;; (c) 1994-2003 by HIROSE Yuuji [yuuji@gentei.org]
;;; (c) 2011 by David Reitter [david.reitter@gmail.com]
;;; revive.el 2.20aquamacs 2011/11/17

;;;[[[   NOTICE ���� NOTICE ���� NOTICE ���� NOTICE ���� NOTICE ����   ]]]
;;;--------------------------------------------------------------------------
;;;	If you are using `windows.el', you can omit the settings of
;;;	define-key and autoload.
;;;	windows.el�����i�g�������������� revive.el ���������L�[��������
;;;	����autoload�������������K�v�������������B
;;;--------------------------------------------------------------------------
;;;
;;;		Resume Emacs:		revive.el
;;;
;;;[What is `revive'?]
;;;
;;;	  Revive.el  saves current editing  status including  the window
;;;	splitting   configuration,   which   can't   be   recovered   by
;;;	`desktop.el' nor by `saveconf.el', into a file  and reconstructs
;;;	that status correctly.
;;;
;;;     This version of `revive' can use `desktop' to save and
;;;	restore global variables and buffers.  It saves all frames.
;;;     It also integrated with Aquamacs' `tabbar' package.
;;;
;;;[Installation]
;;;
;;;	  Put revive.el into your elisp  directory included in load-path
;;;	and write the next expressions.
;;;
;;;	  (autoload 'revive-save-desktop "revive" "Save status" t)
;;;	  (autoload 'revive-desktop "revive" "Resume Emacs" t)
;;;	  (autoload 'wipe "revive" "Wipe Emacs" t)
;;;
;;;	And define favorite keys to those functions.  Here is a sample.
;;;
;;;	  (define-key ctl-x-map "S" 'revive-save-desktop)
;;;	  (define-key ctl-x-map "F" 'revive-desktop)
;;;	  (define-key ctl-x-map "K" 'wipe)
;;;
;;;[How to use]
;;;
;;;     Call `revive-desktop' to load the configuration;
;;;     call `revive-save-desktop' to save it to the file.
;;;     This will use the `desktop' package that comes with Emacs.
;;; 
;;;	Alternatively, you can sidestep the `desktop' package and
;;;     call `save-current-configuration' (`C-x S' if you define key as
;;;	above) when  you want to   save current editing status  and call
;;;	`resume' to restore it.  Numerical prefix  arg to them specifies
;;;	the buffer number in which the editing status will be saved.
;;;
;;;		[Sample Operations]
;;;		C-u 2 C-x S		;save status into the buffer #2
;;;		C-u 3 C-x F		;load status from the buffer #3
;;;
;;;     This variant will only store and restore one frame.
;;;
;;;[Save Variables]
;;;
;;;     NOTE: use `desktop' and related configuration variables unless
;;;     using the resume/save-current-configuration mechanism.
;;;
;;;	  Revive.el can save global or local variables.  The default
;;;	variables to be saved are in revive:save-variables-global-default
;;;	and revive:save-variables-local-default.  If you want to save other
;;;	global/local variables, define them in
;;;	revive:save-variables-global-private or
;;;	revive:save-variables-local-private in a form of a list.  If you
;;;	are using `gmhist', the next expression in ~/.emacs is useful.
;;;
;;;		(setq revive:save-variables-global-private
;;;		      '(file-history buffer-history minibuffer-history))
;;;
;;;[Restoring abnormal buffers]
;;;
;;;     If using `save-current-configuration' and `resume', the following
;;;     mechanisms for non-file buffers are available.
;;;	  The variable revive:major-mode-command-alist-default holds the
;;;	list of  major-mode  name  vs.   command  name.   For  example,
;;;	mh-rmail  command sees the directory ~/Mail/inbox/ and sets  the
;;;	major-mode  to  'mh-folder-mode.   And  gnus  command  sets  the
;;;	major-mode to  'gnus-Group-mode.   If  you want to define  other
;;;	relations between major-mode and command,  set the user variable
;;;	revive:major-mode-command-alist-private as follows:
;;;
;;;		(setq revive:major-mode-command-alist-private
;;;		  '((your-own-mode	. your-own)
;;;		    (foo-mode		. foo)
;;;		    ("*Hanoi*"		. hanoi)
;;;
;;;	it tells revive.el to execute the corresponding command when the
;;;	saved configuration has the buffer with that major-mode. To know
;;;	the major-mode of a certain buffer, type `M-ESC' and `major-mode
;;;	CR'.   And as  you  see above, buffer-name  string  can be  used
;;;	instead of major-mode symbol.
;;;
;;;	  For  other  special modes that  cannot be resumed by executing
;;;	certain function,  define a function to resume and declare it in
;;;	the  variable revive:major-mode-command-alist-private.  In  that
;;;	function, the following values are available.
;;;
;;;		(revive:prop-file-name x)	;file name
;;;		(revive:prop-buffer-name x)	;buffer name
;;;		(revive:prop-major-mode x)	;major-mode
;;;		(revive:prop-point x)		;(point)
;;;		(revive:prop-mark x)		;(mark)
;;;		(revive:prop-varlist x)		;alist of pairs of (var . val)
;;;
;;;[For programmers]
;;;
;;;	  This program provides two powerful functions:
;;;
;;;		* current-window-configuration-printable
;;;		* restore-window-configuration
;;;
;;;	When you want to save a screen your program manages into a file,
;;;	and  restore  it,  save  the  return  value  of  current-window-
;;;	configuration-printable, and read and give it to restore-window-
;;;	configuration.
;;;
;;;	*Sample*
;;;	;;To save
;;;	(insert (prin1-to-string (current-window-configuration-printable)))
;;;	;;To restore
;;;	(restore-window-configuration (read (current-buffer)))
;;;
;;;	Since set-window-configuration cannot set the configuration of
;;;	other frame, the program as below should be useful.
;;;
;;;	*Sample*
;;;	(defun frame-copy-configuration (nth)
;;;	  "Copy confinguration of current frame to NTH next frame."
;;;	  (let ((config (current-window-configuration-printable)))
;;;	    (other-frame nth)
;;;	    (restore-window-configuration config)))
;;;
;;;[Copying]
;;;
;;;	  This program is distributed as a free  software. The author is
;;;	not responsible  for  any  possible   defects   caused  by  this
;;;	software.
;;;
;;;	  Comments  and bug   reports  are welcome. Don't  hesitate   to
;;;	report them.  My e-mail address is following.
;;;
;;;							yuuji@gentei.org
;;;

(defconst revive:version
  "2.20"
  "Version of revive.el")

(defconst revive:version-prefix ";;;")

(defvar revive:emacs-19 (string< "18" (substring emacs-version 0 2)))
(defvar revive:xemacs-p (featurep 'xemacs))
(defun revive:window-edges (&optional window)
  "Borrowed from tapestry.el"
  (if (and (fboundp 'window-pixel-edges) revive:xemacs-p)
      (let ((edges (window-pixel-edges window))
            tmp)
        (setq tmp edges)
        (setcar tmp (/ (car tmp) (face-width 'default)))
        (setq tmp (cdr tmp))
        (setcar tmp (/ (car tmp) (face-height 'default)))
        (setq tmp (cdr tmp))
        (setcar tmp (/ (car tmp) (face-width 'default)))
        (setq tmp (cdr tmp))
        (setcar tmp (/ (car tmp) (face-height 'default)))
        edges)
    (window-edges window)))

(cond
 ((fboundp 'screen-height)
  (fset 'revive:screen-height 'screen-height)
  (fset 'revive:screen-width 'screen-width))
 ((fboundp 'frame-height)
  (fset 'revive:screen-height 'frame-height)
  (fset 'revive:screen-width 'frame-width))
 (t (error "I don't know how to run revive.el on this Emacs...")))

(defun revive:minx () 0)
(defun revive:miny ()
  (if revive:emacs-19
      (car (cdr (revive:window-edges (frame-first-window nil))))
    0))

(defun revive:window-list ()
  "Return the all window list in sorted order."
  (let*((curwin (selected-window)) (win curwin) wlist)
    (if (null
	 (catch 'found
	   (while t
	     (if (and (= (revive:minx) (car (revive:window-edges win)))
		      (= (revive:miny) (car (cdr (revive:window-edges win)))))
		 (throw 'found t))
	     (if (eq (setq win (next-window win)) curwin)
		 (throw 'found nil)))))
	(error "Unexpected window configuration."))
    (setq curwin win wlist (list win))
    (while (not (eq curwin (setq win (next-window win "w/o mini"))))
      (setq wlist (append wlist (list win)))) ;use append to preserve order
    wlist))

(defun revive:window-buffer-list ()
  "Return the all shown buffer list.
Each element consists of '(buffer-file-name window-start point)"
  (let ((curw (selected-window))(wlist (revive:window-list)) wblist)
    (save-excursion
      (while wlist
	(select-window (car wlist))
	(set-buffer (window-buffer (car wlist))) ;for Emacs 19
	(setq wblist
	      (append wblist
		      (list (list
			     (if (and (fboundp 'abbreviate-file-name)
				      (buffer-file-name))
				 (abbreviate-file-name (buffer-file-name))
			       (buffer-file-name))
			     (window-start)
			     (point))))
	      wlist (cdr wlist)))
      (select-window curw)
      wblist)))

(defun revive:all-window-edges ()
  "Return the all windows edges by list."
  (let ((wlist (revive:window-list)) edges)
    (while wlist
      (setq edges (append edges (list (revive:window-edges (car wlist))))
	    wlist (cdr wlist)))
    edges))

(defun revive:select-window-by-edge (x y)
  "Select window whose north west corner is (X, Y).
If the matching window is not found, select the nearest window."
  (let*((curwin (selected-window)) (win (next-window curwin)) edges
	s2 (min 99999) minwin)
    (or
     (catch 'found
       (while t
	 (setq edges (revive:window-edges win)
	       s2 (+ (* (- (car edges) x) (- (car edges) x))
		     (* (- (nth 1 edges) y) (- (nth 1 edges) y))))
	 (cond
	  ((= s2 0)
	   (select-window win)
	   (throw 'found t))
	  ((< s2 min)
	   (setq min s2 minwin win)))
	 (if (eq win curwin) (throw 'found nil)) ;select the nearest window
	 (setq win (next-window win))))
     (select-window minwin))))

(defun revive:split-window-safe (window size &optional hor-flag)
  "Same as split-window but avoids error."
  (split-window
   window
   (min (max (if hor-flag window-min-width window-min-height) size)
	(if hor-flag (- (revive:screen-width) window-min-width 1)
	  (- (revive:screen-height) window-min-height 1)))
   hor-flag))

(defun revive:restore-winconf (x1 y1 x2 y2 edges)
  "Restore partial window configuration.
Assume (X1, Y1), (X2, Y2) as diagonal corners of partial window.
EDGES is a list of sub-windows' edges."
  (let*((topwin (car edges)) (width (- x2 x1)) (height (- y2 y1))
	right lower)
    (cond
     ((= (length edges) 1) nil)		;nothing to do.

     ;;if the top window has the same width as whole frame.
     ;; +---------+
     ;; |top      |
     ;; +-----+---+
     ;; |2    |3  |
     ;; +-----+---+
     ((= width (- (nth 2 topwin) (car topwin)))
      (setq lower (cdr edges))
      (revive:select-window-by-edge x1 y1)
      (revive:split-window-safe nil (- (nth 3 topwin) (nth 1 topwin)))
      (revive:restore-winconf
       (car (car lower)) (nth 1 (car lower)) x2 y2 lower))

     ;;if the top window has the same height as whole frame.
     ;; +-----+---+
     ;; |top  |2  |
     ;; |     +---+
     ;; |     |3  |
     ;; +-----+---+
     ((= height (- (nth 3 topwin) (nth 1 topwin)))
      (setq right (cdr edges))
      (revive:select-window-by-edge x1 y1)
      (revive:split-window-safe nil (- (nth 2 topwin) (car topwin)) t)
      (revive:restore-winconf
       (car (car right)) (nth 1 (car right)) x2 y2 right))

     ;;These two cases above are specialized solution of below for speed.

     ;;general cases.
     ;; +------+--+  Detect whether window is mainly divided vertically or
     ;; |top   |2 |  horizontally.  And call this function recursively on
     ;; +---+--+--+  former (that is, upper half in vertical division or
     ;; |3  |4..  |  left half in horizontal) and latter configuration.
     ;; +---+-----+  
     (t
      (let ((flist (list topwin))
	    (elist (cdr edges)) divwin div-x div-y former latter)
	(while elist
	  (if (or (and (= x1 (car (car elist)))
		       (not (eq (car divwin) x1)))
		  (and (= y1 (nth 1 (car elist)))
		       (not (eq (nth 1 divwin) y1))))
	      (setq divwin (car elist)
		    former flist
		    latter elist))
	  (setq flist (append flist (list (car elist))))
	  (setq elist (cdr elist)))
	(setq div-x (car divwin) div-y (nth 1 divwin))
	(cond
	 ((= x1 (car divwin))	;Mainly divided vertically
	  (revive:select-window-by-edge x1 y1)
	  (revive:split-window-safe nil (- div-y y1))
	  (revive:restore-winconf x1 y1 x2 div-y former)
	  (revive:restore-winconf x1 div-y x2 y2 latter))
	 ((= y1 (nth 1 divwin))
	  (revive:select-window-by-edge x1 y1)
	  (revive:split-window-safe nil (- div-x x1) t)
	  (revive:restore-winconf x1 y1 div-x y2 former)
	  (revive:restore-winconf div-x y1 x2 y2 latter))))))))

(defvar revive:major-mode-command-alist-default
  '((gnus-Group-mode	. gnus)
    (gnus-group-mode	. gnus)		;version 4.x or later
    (mh-folder-mode	. revive:mh)
    (mew-summary-mode	. revive:mew)
    (shell-mode		. revive:shell)
    (sokoban-mode	. sokoban)
    (dired-mode		. revive:dired)
    (view-mode		. revive:view)
    (Info-mode		. info)
    (mpg123-mode	. revive:mpg123)
    ;;(c-mode		. revive:c-set-style)
    ;;(cc-mode		. revive:c-set-style)
    ;;(java-mode		. revive:c-set-style)
    )
  "Default alist of major-mode vs. command name.")
(defvar revive:major-mode-command-alist-private nil
  "*User defined revive:major-mode-command-alist")
(defvar revive:major-mode-command-alist nil
  "*Alist of major-mode vs. commandname.")
(setq revive:major-mode-command-alist
      (append revive:major-mode-command-alist-private
	      revive:major-mode-command-alist-default))

(defvar revive:save-variables-global-default
  '(truncate-partial-width-windows
    make-backup-files version-control
    visible-bell file-name-history buffer-name-history minibuffer-history)
  "Default list of global variables to save.")
(defvar revive:save-variables-global-private nil
  "*User defined list of the global variables to save.")
(defvar revive:save-variables-global nil
  "*List of global variables to save.")
(setq revive:save-variables-global
      (append revive:save-variables-global-private
	      revive:save-variables-global-default))

(defvar revive:save-variables-local-default
  '(buffer-read-only
    truncate-lines fill-column fill-prefix case-fold-search default-directory)
  "Default list of the local variables to save.")
(defvar revive:save-variables-local-private nil
  "*User defined list of the local variables to save.")

(defvar revive:save-variables-mode-local-default
  '((dired-mode dired-actual-switches)
    (yatex-mode YaTeX-math-mode YaTeX-modify-mode YaTeX-parent-file)
    (yahtml-mode YaTeX-parent-file)
    (c-mode c-indentation-style c-basic-offset)
    (cc-mode c-indentation-style c-basic-offset)
    (java-mode c-indentation-style c-basic-offset))
  "Default list of the mode specific local variables to save.
Actually, revive doesn't make the buffer local variables assuming
those variable have already localized by their major mode.")
(defvar revive:save-variables-mode-local-private
  nil
  "*User defined list of the mode specific local variables to save.")

(defvar revive:configuration-file
  (cond ((eq system-type 'ms-dos) "~/_revive.el")
	((boundp 'aquamacs-version) "~/Library/Preferences/Aquamacs Emacs/Revive.el")
	(t "~/.revive.el"))
  "*File to save window configuration")

(defvar revive:ignore-buffer-pattern "^ \\*"
  "*Regexp of buffer names revive should ignore.")

(defun revive:normalize-edges (width height edgelist)
  "Normalize all coordinates for current screen size.
'(WIDTH, HEIGHT) is old screen size and EDGELIST is a list of
window-edges."
  (let (normalized (curw (revive:screen-width))
		   (curh (revive:screen-height)) e n)
    (if (and (equal curw width) (equal curh height))
	edgelist
      (while edgelist
	(setq e (car edgelist)
	      n (list (/ (+ (* curw (nth 0 e)) (/ width 2)) width)
		      (/ (+ (* curh (nth 1 e)) (/ height 2)) height)
		      (/ (+ (* curw (nth 2 e)) (/ width 2)) width)
		      (/ (+ (* curh (nth 3 e)) (/ height 2)) height))
	      normalized (append normalized (list n))
	      edgelist (cdr edgelist)))
      normalized)))

(defun construct-window-configuration (edgelist)
  "Restore window configuration by EDGELIST.  EDGELIST should be sorted."
  (delete-other-windows)
  (revive:restore-winconf (revive:minx) (revive:miny)
			  (revive:screen-width)
			  (1- (revive:screen-height)) edgelist))

;;;###autoload
(defun current-window-configuration-printable ()
  "Return the printable current-window-configuration.
This configuration will be stored by restore-window-configuration.
Returned configurations are list of:
'(Screen-Width Screen-Height Edge-List Buffer-List)

Edge-List is a return value of revive:all-window-edges, list of all
window-edges whose first member is always of north west window.

Buffer-List is a list of buffer property list of all windows.  This
property lists are stored in order corresponding to Edge-List.  Buffer
property list is formed as
'((buffer-file-name) (buffer-name) (point) (window-start) tabs).
"
  (let ((curwin (selected-window))
	(wlist (revive:window-list)) (edges (revive:all-window-edges)) buflist)
    (save-excursion
      (while wlist
	(select-window (car wlist))
	;should set buffer on Emacs 19
	(set-buffer (window-buffer (car wlist)))
	(setq buflist
	      (append buflist (list (list
				     (if (and
					  (buffer-file-name)
					  (fboundp 'abbreviate-file-name))
					 (abbreviate-file-name
					  (buffer-file-name))
				       (buffer-file-name))
				     (buffer-name)
				     (point)
				     (window-start)
				     (car (if (fboundp 'tabbar-window-list-tabsets-to-save)
					      (tabbar-window-list-tabsets-to-save t))))))
	      wlist (cdr wlist)))
      (select-window curwin)
      (list (revive:screen-width) (revive:screen-height) edges buflist))))

(defmacro revive:get-file (x)
  (list 'car x))
(defmacro revive:get-buffer (x)
  (list 'nth 1 x))
(defmacro revive:get-point (x)
  (list 'nth 2 x))
(defmacro revive:get-window-start (x)
  (list 'nth 3 x))
(defmacro revive:get-tabs (x)
  (list 'nth 4 x))

(defun revive:find-file (file)
  "Make the best effort to find-file FILE."
  (cond
   ((or (null file) (not (stringp file))) nil)
   ((file-exists-p file) (find-file file) (current-buffer))
   ((string-match ":" file)		;maybe ange-ftp's external file
    (if (progn (load "ange-ftp" t) (featurep 'ange-ftp))
	(progn (condition-case err
		   (find-file file)
		 (ftp-error
		  (message "Can't remote file `%s'" file)
		  (condition-case err2	;give a user one more chance.
		      (find-file file)
		    (ftp-error (error "Maybe you made mistake twice.")))))
	       (current-buffer))))
   (t nil)))

(defun revive:find-buffer (name file)
  (or (and file 
	   (find-buffer-visiting file))
      (get-buffer name)))

;;;###autoload
(defun restore-window-configuration (config)
  "Restore the window configuration.
Configuration CONFIG should be created by
current-window-configuration-printable."
  (let ((one-buffer-one-frame-inhibit t) (one-buffer-one-frame-mode nil)
	(width (car config)) (height (nth 1 config))
	(edges (nth 2 config)) (winlist (nth 3 config)) winspec (num 1)
	(windows-to-delete))
    (set-buffer (get-buffer-create "*scratch*"))
    (setq edges (revive:normalize-edges width height edges))
    (construct-window-configuration edges)
    (revive:select-window-by-edge (revive:minx) (revive:miny))
    (while winlist
      (setq winspec (car winlist))
      (if (fboundp 'tabbar-window-restore-tabs-in-window)
	  (unless (tabbar-window-restore-tabs-in-window (revive:get-tabs winspec))
	    (setq winspec nil)
	    (add-to-list 'windows-to-delete (selected-window))))
      (if winspec
	  (let ((target-buffer (revive:find-buffer (revive:get-buffer winspec) (revive:get-file winspec))))
	(cond
	 (target-buffer
	  (switch-to-buffer target-buffer)
	  (goto-char (revive:get-window-start winspec)) ;to prevent high-bit missing
	  (set-window-start nil (point))
	  (goto-char (revive:get-point winspec)))
	 ((and (stringp (revive:get-file winspec))
	       (not (file-directory-p (revive:get-file winspec)))
		 (revive:find-file (revive:get-file winspec)))
	  (set-window-start nil (revive:get-window-start winspec))
	  (goto-char (revive:get-point winspec))))))
      (setq winlist (cdr winlist))
      (other-window 1))
    (mapc (lambda (w) (if (window-live-p w) (delete-window w))) windows-to-delete)
    ))

;;;	
(defun revive:buffer-list ()
  (delq nil
	(mapcar (function
		 (lambda (b)
		   (if (string-match
			revive:ignore-buffer-pattern
			(buffer-name b)) nil
		     b)))
		(buffer-list))))

;;;###autoload
(defun wipe ()
  "Wipe Emacs."
  (interactive)
  (save-some-buffers)
  (mapcar (function kill-buffer) (revive:buffer-list)))

(defun revive:varlist (var2save)
  "Return the (variable . value) list of variables in VAR2SAVE."
  (delq nil (mapcar 
	     (function (lambda (s)
			 (if (and s (boundp s)) (cons s (symbol-value s)))))
	     var2save)))

(defun revive:map-string-match (string alist)
  "Return sub-list if a car component of list in ALIST matches STRING.
That car component is regexp."
  (let (regex)
    (catch 'match
      (while alist
	(setq regex (car (car alist)))
	(if (and regex
		 (stringp regex)
		 (string-match (car (car alist)) string))
	    (throw 'match t))
	(setq alist (cdr alist))))))

(defvar revive:buffer-property-list-hook nil
  "*Evaluated at the beginning of revive:buffer-property-list.")
(defun revive:buffer-property-list ()
  "Return buffers property list.
Returned list is a form of: '(Buffer-List Variable-List).
Buffer-List is a list as
'((buffer-file-name) (buffer-name) major-mode (point) (mark)).
Variable-List is a return value of revive:varlist."
  (let ((buflist (revive:buffer-list)) plist
	(local-var (append revive:save-variables-local-default
			   revive:save-variables-local-private))
	(mode-local-var (append revive:save-variables-mode-local-default
				revive:save-variables-mode-local-private))) 
    (save-excursion
      (run-hooks 'revive:buffer-property-list-hook)
      (while buflist
	(set-buffer (car buflist))
	(if (or (assq major-mode revive:major-mode-command-alist)
		(revive:map-string-match
		 (buffer-name) revive:major-mode-command-alist)
		(if (and (fboundp 'abbreviate-file-name) (buffer-file-name))
		    (abbreviate-file-name (buffer-file-name))
		  (buffer-file-name)))
	    (setq plist
		  (append
		   plist
		   (list
		    (list
		     (if (and (fboundp 'abbreviate-file-name)
			      (buffer-file-name))
			 (abbreviate-file-name (buffer-file-name))
		       (buffer-file-name))
		     (buffer-name)
		     major-mode
		     (point)
		     (if revive:emacs-19 (mark t) (mark))
		     (revive:varlist
		      (append local-var (assq major-mode mode-local-var)))
		     )))))
	(setq buflist (cdr buflist))))
    (list plist (revive:varlist revive:save-variables-global))))

(defmacro revive:prop-file-name (x)
  (list 'car x))
(defmacro revive:prop-buffer-name (x)
  (list 'nth 1 x))
(defmacro revive:prop-major-mode (x)
  (list 'nth 2 x))
(defmacro revive:prop-point (x)
  (list 'nth 3 x))
(defmacro revive:prop-mark (x)
  (list 'nth 4 x))
(defmacro revive:prop-varlist (x)
  (list 'nth 5 x))
(defmacro revive:prop-get-value (x y)
  (list 'cdr (list 'assq y (list 'nth 5 x))))

;;;###autoload
(defun save-current-configuration (&optional num)
  "Save current window/buffer configuration into configuration file."
  (interactive "p")
  (let ((bufs (revive:buffer-property-list))
	(config (current-window-configuration-printable)))
    (set-buffer
     (find-file-noselect (expand-file-name revive:configuration-file)))
    (emacs-lisp-mode)
    (widen)
    (goto-char (point-min))
    (and (search-forward revive:version-prefix nil t)
	 (goto-char (match-beginning 0)) (kill-line 1))
    (insert (format "%s%s\n" revive:version-prefix revive:version))
    (setq num (or num 1))
    (if (re-search-forward (format "^(%d" num) nil t)
	(progn (goto-char (match-beginning 0))
	       (kill-sexp 1) (delete-char 1))
      (goto-char (point-max)))
    (delete-blank-lines)
    (if (not (bolp)) (newline 1))
    (insert (format "%s\n" (prin1-to-string (list num bufs config))))
    (basic-save-buffer)
    (kill-buffer (current-buffer))))

(defun revive:restore-value (vlist)
  "Restore variables in VLIST which is a return value of revive:varlist."
  (while vlist
    (if (and (car vlist) (listp (car vlist)))
	(set (car (car vlist)) (cdr (car vlist))))
    (setq vlist (cdr vlist))))

(defvar revive:restore-buffers-hook nil
  "*Run at the end of revive:restore-buffers.")
(defun revive:restore-buffers (buflist)
  "Restore all buffers in BUFLIST which is from revive:buffer-property-list."
  (let ((blist (car buflist)) x command success
	(mmc-alist revive:major-mode-command-alist))
    (revive:restore-value (car (cdr buflist)))
    (while blist
      (setq x (car blist) success nil)
      (if (setq command (or (assq (revive:prop-major-mode x) mmc-alist)
			    (assoc (revive:prop-buffer-name x) mmc-alist)))
	  (condition-case err
	      (let ((noninteractive nil))
		(if (fboundp (cdr command))
		    (progn
		      (call-interactively (cdr command))
		      (setq success t))))
	    ;;(funcall (cdr command))
	    (error (message "%s: %s." (cdr command) err) (sit-for 1)))
	(if (revive:find-file (revive:prop-file-name x))
	    (progn
	      (if (and (not (eq (revive:prop-major-mode x) major-mode))
		       (fboundp (revive:prop-major-mode x)))
		  (if (commandp (revive:prop-major-mode x))
		      (call-interactively (revive:prop-major-mode x))
		    (funcall (revive:prop-major-mode x))))
	      (setq success t)
	      )))
      (cond
       (success
	(if (not (string= (revive:prop-buffer-name x) (buffer-name)))
	    (rename-buffer (revive:prop-buffer-name x)))
	(set-mark (revive:prop-mark x))
	(goto-char (revive:prop-point x))
	(revive:restore-value (revive:prop-varlist x))))
      (setq blist (cdr blist)))
    (run-hooks 'revive:restore-buffers-hook)))

(defvar resume-hook nil "*Run at the end of resume.")
;;;###autoload
(defun resume (&optional num)
  "Resume window/buffer configuration.
Configuration should be saved by save-current-configuration."
  (interactive "p")
  (setq num (or num 1))
  (let (sexp bufs x config)
    (find-file (expand-file-name revive:configuration-file))
    (goto-char (point-min))
    (emacs-lisp-mode)
    (if (null (search-forward revive:version-prefix nil t))
	(error "Configuration file collapsed."))
    (if (and (not (string= revive:version
			   (buffer-substring
			    (point)
			    (prog2 (end-of-line) (point)))))
	     (y-or-n-p
	      "Configuration file's version conflicts. Continue?"))
	(error "Configuration file is old.  Please update."))
    (if (null (re-search-forward (format "^(%d" num) nil t))
	(error "Configuration empty."))
    (goto-char (match-beginning 0))
    (setq sexp (read (current-buffer))
	  bufs (nth 1 sexp)
	  config (nth 2 sexp))
    (kill-buffer (current-buffer))
    (revive:restore-buffers bufs)
    (restore-window-configuration config)
    (run-hooks 'resume-hook)))


;;;
;; Special functions (Each functions should be an interactive one)
;;;
(defun revive:dired ()
  "Restore dired buffer referring the variable x of revive:restore-buffers."
  (interactive)
  (let ((find-file-run-dired t)
	(dir (cdr (assq 'default-directory (revive:prop-varlist x)))))
    (if (and (stringp dir) (file-directory-p dir))
	(progn
	  (dired dir
		 (cdr (assq 'dired-actual-switches (revive:prop-varlist x))))
	  (setq default-directory dir)
	  (cd dir))
      (error "Directory [%s] not found" dir))))

(defun revive:view ()
  "Restore view-mode's buffer."
  (interactive)
  (require 'view)
  (if (fboundp 'view-exit) (view-file (revive:prop-file-name x))
    (if (get 'revive:view 'warning)
	nil
      (switch-to-buffer
       (create-file-buffer
	(concat "** out of use - " (revive:prop-file-name x) " **")))
      (insert
       (message
	"Skip view-modes' buffer since your view.el may use recursive-edit."))
      (kill-buffer nil)
      (put 'revive:view 'warning t)
      (sit-for 5))))

(defun revive:mh ()
  "Restore mh-e's buffer."
  (interactive)
  (require 'mh-e)
  (require 'mh-utils)
  (mh-find-path)
  (get-buffer-create " *mh-temp*")
  (mh-visit-folder (revive:prop-buffer-name x) "all")
  (setq buffer-file-name (revive:prop-file-name x)
	default-directory
	(or (cdr (assq 'default-directory (revive:prop-varlist x)))
	    "~/"))
  (cd default-directory))

(defun revive:mew ()
  "Restore mew's summary buffer."
  (interactive)
  (require 'mew)
  (set-buffer (get-buffer-create (revive:prop-buffer-name x)))
  (setq default-directory
	(or (cdr (assq 'default-directory (revive:prop-varlist x)))
	    "~/"))
  (or (and (boundp 'mew-path) mew-path)
      (and (fboundp 'mew-init) (let (mew-demo) (mew-init))))
  (mew-cache-flush)			;Mew should take more care of 
  (get-buffer-create " *mew tmp*")	;unexisting buffer...
  (let ((b (revive:prop-buffer-name x)))
    (cond
     ((fboundp 'mew-summary-folder-create)
      (mew-summary-folder-create b))
     ((fboundp 'mew-summary-switch-to-folder)
      (kill-buffer b)
      (mew-summary-switch-to-folder b))
     (t nil))))

(defun revive:shell ()
  "Restore shell-mode's buffer."
  (interactive)
  (set-buffer (get-buffer-create " *tmp*"))
  (let ((default-directory
	  (or (cdr (assq 'default-directory (revive:prop-varlist x)))
	      ".")))
    (shell)
    (cd default-directory)
    (kill-buffer " *tmp*")))

(defun revive:mpg123 ()
  "Restore mpg123-mode's buffer."
  (interactive)
  (let ((d (cdr (assq 'default-directory (revive:prop-varlist x)))))
    (if (and d (file-directory-p d))
	(progn
	  (require 'mpg123)
	  (mpg123 d)))))

(defun revive:c-set-style ()
  "Restore c-style of C/C++/Java(by cc-mode)."
  (interactive)
  (require 'cc-mode)
  (revive:find-file (revive:prop-file-name x))
  (funcall (revive:prop-major-mode x))
  (c-set-style (or (revive:prop-get-value x 'c-indentation-style) "gnu")))


(defvar revive:app-restore-path "~/Library/Application Support/Aquamacs Emacs/")
(defvar revive:desktop-base-file-name "SessionDesktop.el")
(defvar revive:frame-configuration-to-restore nil "This variable is typically set by the desktop file.
Its value is evaluated after loading the file in `revive:restore-application-state'.")

;;;###autoload
(defun revive-desktop (&optional file auto)
  "Restores the application state.
Similar to `resume', though using `desktop' to restore buffers."
  (interactive)
  (require 'desktop)
  (when (and (called-interactively-p) (not auto))
    (setq file
	  (expand-file-name
	  (if (or  
	       (and last-nonmenu-event 
		    (not (consp last-nonmenu-event))) 
	       ;;(not (eq (car-safe last-nonmenu-event)  
	       ;;	  'mac-apple-event)))
	       (not use-dialog-box)
	       (not window-system))
	      (read-file-name "Session file to load: " nil revive:desktop-base-file-name 'mustmatch)
	    (ns-read-file-name "Session file to load" nil 'mustmatch revive:desktop-base-file-name)))))

  (message "Restoring application state.")
  (interactive)
  (let ((one-buffer-one-frame-mode nil)
	(desktop-base-file-name (if (and file (not (file-directory-p file)))
				    (file-name-nondirectory file)
				  revive:desktop-base-file-name))
	(desktop-missing-file-warning nil)
	(desktop-load-locked-desktop t))
    (flet ((y-or-n-p (&rest args) t)
	   (yes-or-no-p (&rest args) t)
		 (desktop-clear nil)) 
      (desktop-read (if file (if (file-directory-p file) file (file-name-directory file)) revive:app-restore-path))
      ;; restore window config, if any
      (eval revive:frame-configuration-to-restore)
      (setq revive:frame-configuration-to-restore nil))))

(defun revive:restore-frame (fp fc)
  "Restores configuration of a single frame.
Uses frame parameters FP and window configuration FC."
  (let ((f (make-frame (append '((visibility . nil)) fp)))) ; needed in Emacs 23 (iconified)
    (select-frame f)
    (sit-for 0) ; needed in Emacs 23 for various frames
    (set-frame-parameter f 'visibility t) ; needed in Emacs 23 (iconified frames)
    (sit-for 0) ; needed in Emacs 23 for various frames
    (set-frame-parameter f 'visibility (or (cdr (assq 'visibility fp)) t)) ; Emacs 23
    (restore-window-configuration fc)
    ;; (if (eq 'icon (cdr (assq 'visibility fp)))
    ;; 	(iconify-frame f)))
  t))

(defun revive:print-frame-states ()
  "Prints commands to set frame / window configuration.
Suitable for use in `desktop-save-hook'"
  (insert "(setq revive:frame-configuration-to-restore\n  '(progn ")
  (condition-case err
      (mapc (lambda (f)
	      (and (frame-visible-p f) ; do not store invisible frames
		   (let ((p (frame-parameters f)))
		     (insert 
		      (format "\n     \(revive:restore-frame '%S '%S\)"
			      (mapcon (lambda (x)
					(unless (memq (caar x) 
						      '(buried-buffer-list 
							buffer-list minibuffer 
							display display-type window-system 
							parent-id window-id font-backend
							name))
					  (list (car x))))
				      p)
			      (with-selected-frame f
				;; this function from revive.el
				(current-window-configuration-printable)))))))
	    (frame-list))
    (error (message "Error while saving application state: %s" err)))
  (insert "))\n"))


;;;###autoload
(defun revive-save-desktop (&optional file auto)
  "Save application state with `desktop' and `revive'.
Similar to `save-current-configuration', 
though uses `desktop' to restore buffers."
  (interactive)
  (require 'desktop)
  (when (and (called-interactively-p) (not auto) (not file))
    (setq file
	  (if (or  
	       (and last-nonmenu-event 
		    (not (consp last-nonmenu-event))) 
	       ;;(not (eq (car-safe last-nonmenu-event)  
	       ;;	  'mac-apple-event)))
	       (not use-dialog-box)
	       (not window-system))
	      (read-file-name "Session file for saving: ")
	    (ns-read-file-name "Session file for saving" nil nil revive:desktop-base-file-name))))
  (let ((desktop-base-file-name (if (and file (not (file-directory-p file)))
				    (file-name-nondirectory file)
				  revive:desktop-base-file-name))
	(desktop-save-hook desktop-save-hook))
    (add-hook 'desktop-save-hook #'revive:print-frame-states)
    (flet ((y-or-n-p (&rest args) t)
	   (yes-or-no-p (&rest args) t)) 
      (desktop-save (if file (if (file-directory-p file)
				 file
			       (file-name-directory file)) revive:app-restore-path) 
		    'release))))


(defcustom revive-desktop-after-launching 'never
  "Control whether to restore the desktop after application launch.
If set to nil (default), the system control this behavior, and the
desktop state is saved when required by the system and upon exiting
Emacs.  If set to `never', the desktop is never stored nor restored.
If set to t, desktop is always saved and restored.

Under Mac OS X 10.7 `Lion' or later, sessions are stored regularly and retrieved
upon application start in case the system reboots or the application exists
unexpectedly.

As application state, Aquamacs stores all buffers showing a file, all tabs,
windows and frames and their exact locations, and some customization variables.
Process buffers are not stored."
  :group 'Aquamacs
  :version 23.3
  :type '(choice :tag "Restore desktop..."
		 (const :tag "always" t)
		 (const :tag "automatically" nil)
		 (const :tag "never" 'never)))

(defun revive:revive-desktop (&optional delete-frames)
  "Automatically restores desktop.
Does nothing if `revive-desktop-after-launching' is set to `never'.
If DELETE-FRAMES is non-nil, delete all pre-existing frames."
  (interactive)
  (unless (eq 'never revive-desktop-after-launching)
    (if (boundp 'ns-session-restore-request)
	(setq ns-session-restore-request 'restored))
    (condition-case err
	(let ((frames (visible-frame-list)))
	  (revive-desktop nil 'auto)
	  (mapcar #'delete-frame frames))
      (error (message "Error while restoring application state: %s" err)))))

(defun revive:auto-save-desktop ()
  "Automatically save desktop.
Does nothing if `revive-desktop-after-launching' is set to `never'."
  (interactive)
  (unless (or (eq 'never revive-desktop-after-launching)
	      ;; do not save before restoring, if requested
	      (and (boundp 'ns-session-restore-request)
		   (eq 'requested ns-session-restore-request)))
    (condition-case err
	(revive-save-desktop nil 'auto)
      (error (message "Error while saving application state: %s" err)))))

(defun revive:after-application-start ()
  "Restore desktop after application start, if so requested.
See also `revive-desktop-after-launching'."
  (unless (or (eq 'never revive-desktop-after-launching) noninteractive)
    (when (or revive-desktop-after-launching
	      (and (boundp 'ns-session-restore-request)
		   (eq 'requested ns-session-restore-request)))
      (revive:revive-desktop 'delete-frames))))

(defun revive:setup ()
  (when (not noninteractive)
    (add-hook 'kill-emacs-hook #'revive:auto-save-desktop)
    (add-hook 'after-init-hook #'revive:after-application-start 'append)
    (when (featurep 'ns)
      (define-key global-map [ns-application-restore] 'revive:revive-desktop)
      (define-key global-map [ns-application-store-state] 'revive:auto-save-desktop))))


;;(provide 'resume)
(provide 'revive)


;; revive.el,v 2.20aquamacs

;; Revision 2.20aquamacs  2011/11/17 davidswelt
;; Store/restore full application state.  Use Revive for windows, tabbar for tabs.
;;
;; Revision 2.19  2008/05/13 01:19:16  yuuji
;; Add below to revive:save-variables-global-default.
;; * file-name-history buffer-name-history minibuffer-history
;;
;; Revision 2.18  2003/06/22 05:11:46  yuuji
;; revive:c-set-style disabled.
;;
;; Revision 2.17  2003/06/13 09:27:56  yuuji
;; Use window-pixel-edges on XEmacs only (Thanks to sasaki@fcc.ad.jp)
;;
;; Revision 2.17  2003/06/13 09:25:02  yuuji
;; Use window-pixel-edges on XEmacs only (Thanks to sasaki@fcc.ad.jp)
;;
;; Revision 2.16  2003/05/16 16:47:52  yuuji
;; revive:c-set-style fixed
;;
;; Revision 2.15  2001/11/21 13:51:24  yuuji
;; Support mew2
;;
;; Revision 2.14  1999/08/26 07:27:52  yuuji
;; mpg123.el now revivable.
;;
;; Revision 2.13  1998/04/01 03:52:08  yuuji
;; FSF's Address fixed
;;
;; Revision 2.12  1997/11/29 13:19:33  yuuji
;; XEmacs support
;;
;; Revision 2.11  1997/10/16 16:13:25  yuuji
;; for Emacs-20
;;
;; Revision 2.10  1997/01/27 04:05:17  yuuji
;; revive:split-window-safe fixed
;; mew support fixed again
;;
;; Revision 2.9  1997/01/17 05:33:59  yuuji
;; fix for mew (" *mew tmp*")
;;
; Revision 2.8  1997/01/17  03:06:10  yuuji
; Support mew
;
; Revision 2.7  1995/07/13  01:36:10  yuuji
; Correct document.
; Abbriviate file (directory) name of revive file if possible.
; GNUS v4.x revivable.
; Revive functions for 'mh and 'shell are available.
;
; Revision 2.6  1995/01/23  11:12:08  yuuji
; revive:ignore-buffer-pattern added
;
; Revision 2.5  1994/12/18  09:18:00  yuuji
; Fixed the bug of revive:split-window-safe.
;
; Revision 2.4  1994/11/19  17:50:06  yuuji
; Add revive:save-variables-mode-local-*.
;
; Revision 2.3  1994/09/26  17:17:15  yuuji
; Fix the bug in counting menu-bar-lines.
;
; Revision 2.2  1994/06/06  07:55:05  yuuji
; Support remote file opened with ange-ftp.
;
; Revision 2.1  1994/03/07  08:24:31  yuuji
; Fix the recovery of dired-mode buffer.
;
; Revision 2.0  1994/02/21  07:08:15  yuuji
; Initial version as `revive.el'.
;
; Revision 1.5  1994/02/17  08:48:41  yuuji
; Add `wipe'.
;
; Revision 1.4  1994/02/09  12:41:40  yuuji
; Check nil for file-exists-p.
;
; Revision 1.3  1994/02/09  10:57:49  yuuji
; Resume dired.
;
; Revision 1.2  1994/02/07  12:40:23  yuuji
; Add saving of global variables.
;
; Revision 1.1  1994/02/05  07:55:11  yuuji
; Initial revision
;

; Local variables:
; fill-prefix: ";;;	"
; paragraph-start: "^$\\|\\|;;;$"
; paragraph-separate: "^$\\|\\|;;;$"
; End:
