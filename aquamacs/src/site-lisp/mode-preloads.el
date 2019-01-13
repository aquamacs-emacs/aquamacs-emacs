;; cache (relative) load path for edit-modes

(defun aquamacs-preload-add-load-path (dir max-depth)
  "Recursively find all subdirectories of current directory.
More precisely, this uses only the subdirectories whose names
start with letters or digits; it excludes any subdirectory named `RCS'
or `CVS', and any subdirectory that contains a file named `.nosearch'."
  (let (dirs
	attrs
	(pending (list (cons default-directory 0))))
    ;; This loop does a breadth-first tree walk on DIR's subtree,
    ;; putting each subdir into DIRS as its contents are examined.
    (while pending
      
      (let* ((this-entry (pop pending))
	     (this-dir (car this-entry))
	     (depth (cdr this-entry))
	     (contents (directory-files this-dir))
	     (default-directory this-dir)
	     (canonicalized (if (fboundp 'untranslated-canonical-name)
				(untranslated-canonical-name this-dir))))
	(push (car this-entry) dirs)
	;; The Windows version doesn't report meaningful inode
	;; numbers, so use the canonicalized absolute file name of the
	;; directory instead.
	(setq attrs (or canonicalized
			(nthcdr 10 (file-attributes this-dir))))
	(unless (member attrs normal-top-level-add-subdirs-inode-list)
	  (push attrs normal-top-level-add-subdirs-inode-list)
	  (dolist (file contents)
	    ;; The lower-case variants of RCS and CVS are for DOS/Windows.
	    (unless (member file '("." ".." "RCS" "CVS" "rcs" "cvs"))
	      (when (and (string-match "\\`[[:alnum:]]" file)
			 ;; Avoid doing a `stat' when it isn't necessary
			 ;; because that can cause trouble when an NFS server
			 ;; is down.
			 (not (string-match "\\.elc?\\'" file))
			 (file-directory-p file))
		(let ((expanded (expand-file-name file)))
		  (if (and (or (null max-depth) (<= depth max-depth))
			       ;; (file-exists-p (expand-file-name ".nosearch"
			       ;; 					expanded))
			       )
		    (setq pending (nconc pending (list (cons expanded (1+ depth)))))))))))))

    dirs))


;; we define this during preloading
(defvar aquamacs-preloaded-load-path nil)
(setq aquamacs-preloaded-load-path
      (let ((ddir (file-name-directory (or (locate-file "aquamacs/mode-preloads.el" load-path)
						 (locate-file "mode-preloads.el" load-path))))
	    (normal-top-level-add-subdirs-inode-list))
	(message "Caching directory %sedit-modes" ddir)
	(let ((default-directory ddir))
	  (append (mapcar (lambda (f) (file-relative-name f "../../"))
			  (aquamacs-preload-add-load-path "edit-modes" 1))
		  (mapcar (lambda (f) (file-relative-name f "../../"))
			  (aquamacs-preload-add-load-path "util" nil))))
	))
(message "... caching: %s" aquamacs-preloaded-load-path)

(load "aquamacs/smart-dnd") ;; Smart Drag&Drop

;; NXML

;; (unless (or (boundp 'nxml-version) (>= emacs-major-version 23))
;;   (load "rng-auto"))
 
(assq-set-equal "\\.\\(xml\\|xsl\\|rng\\|xhtml\\)" 
		'nxml-mode 'auto-mode-alist)
 
(assq-set-equal "<\\?xml " 'nxml-mode 'magic-mode-alist)


;; The following takes 100msec, so it's here
;; (let* ((nxhtml-install-dir (concat default-directory "../lisp/aquamacs/edit-modes/nxhtml/"))
;;       (load-path (cons nxhtml-install-dir load-path)))
;;   ;; does not work
;;   (load "nxhtml-loaddefs"))
(autoload 'nxhtml-mode "nxhtml/autostart.el" "Major mode for editing XHTML documents." 'interactive nil)
(autoload 'nxhtml-menu-mode "nxhtml/autostart.el" "Minor mode providing web project management and more." 'interactive nil)


;; JDEE
;; in mode-preloads

(defun aquamacs-announce-jdee ()
  "Notify users of the JDEE plugin."
  (unless (boundp 'jde-jdk)
    (message "For the best Java editing environment, get the JDEE plugin at http://aquamacs.org."))
  (remove-hook 'jde-mode-hook 'aquamacs-announce-jdee))

(add-hook 'jde-mode-hook 'aquamacs-announce-jdee)




;; SLIME
;; in mode-preloads

(defun aquamacs-announce-slime ()
  "Notify users of the SLIME plugin."
  (unless (fboundp 'slime-setup)
    (message "For superior Lisp interaction, get the SLIME plugin at http://aquamacs.org."))
  (remove-hook 'lisp-mode-hook 'aquamacs-announce-slime))

(add-hook 'lisp-mode-hook 'aquamacs-announce-slime)



;;; HEADER LINES


;; we don't want header lines (tab uses them)
(aquamacs-set-defaults 
 '((Info-use-header-line nil)
   (slime-header-line-p nil)
   (erc-mode-line-format "%s %a. %n on %t (%m,%l) %o")
   (erc-header-line-format nil)))

(assq-set-equal "\\.wiki$" 'wikipedia-mode 'auto-mode-alist) 
(autoload 'wikipedia-mode "wikipedia-mode.el" "Major mode for editing Wikipedia articles." 'interactive nil)

;; ESS
;; Need this to prevent the tramp problem.
(setq ess-r-versions nil)

(autoload 'ess-mode "ess-mode/lisp/ess/ess-site" "Emacs Speaks Statistics" t)
(autoload 'R-mode "ess-mode/lisp/ess/ess-site" "Emacs Speaks Statistics" t)
(autoload 'R "ess-mode/lisp/ess/ess-site" "Emacs Speaks Statistics" t)
(autoload 'S-mode "ess-mode/lisp/ess/ess-site" "Emacs Speaks Statistics" t)
(autoload 'Rnw-mode "ess-mode/lisp/ess/ess-site" "Emacs Speaks Statistics" t)
(autoload 'omegahat-mode "ess-mode/lisp/ess/ess-site" "Emacs Speaks Statistics" t)
(autoload 'XLS-mode "ess-mode/lisp/ess/ess-site" "Emacs Speaks Statistics" t)
(autoload 'STA-mode "ess-mode/lisp/ess/ess-site" "Emacs Speaks Statistics" t)
(autoload 'SAS-mode "ess-mode/lisp/ess/ess-site" "Emacs Speaks Statistics" t)
(autoload 'S-transcript-mode "ess-mode/lisp/ess/ess-site" "Emacs Speaks Statistics" t)
(autoload 'R-transcript-mode "ess-mode/lisp/ess/ess-site" "Emacs Speaks Statistics" t)



;; 
(autoload 'actr-mode "actr-mode" "ACT-R mode" 'interactive nil)
(add-to-list (quote auto-mode-alist) (quote ("\\.actr\\'" . actr-mode)))

(load "haskell-mode/haskell-site-file") ;; autoloads

(setq auto-mode-alist
	(append
	 '(("\\.sp\\'"		. S-mode) ;; re: Don MacQueen <macq@llnl.gov>
	   ("\\.[qsS]\\'"	. S-mode) ;; q,s,S [see ess-restore-asm-extns above!]
	   ("\\.ssc\\'"		. S-mode) ;; Splus 4.x script files.
	   ("\\.[rR]\\'"	. R-mode)
	   ("\\.[rR]nw\\'"	. Rnw-mode)
	   ("\\.[rR]profile\\'" . R-mode)
	   ("NAMESPACE\\'"	. R-mode)
	   ("\\.omg\\'"         . omegahat-mode)
	   ("\\.hat\\'"         . omegahat-mode) ;; Duncan's pref'd...
	   ("\\.lsp\\'"		. XLS-mode)
	   ("\\.do\\'"		. STA-mode)
	   ("\\.ado\\'"		. STA-mode)
	   ("\\.[Ss][Aa][Ss]\\'"	. SAS-mode)
	   ;; Many .log/.lst files, not just SAS
	   ;;("\\.log\\'"	. SAS-log-mode)
	   ;;("\\.lst\\'"	. SAS-listing-mode)
	   ("\\.[Ss]t\\'"	. S-transcript-mode)
	   ("\\.[Ss]out"	. S-transcript-mode)
	   ("\\.[Rr]t\\'"	. R-transcript-mode)
	   ("\\.[Rr]out"	. R-transcript-mode) 
          )
	 auto-mode-alist))




(aquamacs-set-defaults 
 '((html-helper-mode-uses-JDE nil)))
(autoload 'html-helper-mode "html-helper-mode" 
  "major mode for editing HTML source." t)
(assq-set-equal "\\.html$" 'html-helper-mode 'auto-mode-alist)
(assq-set-equal "\\.shtml$" 'html-helper-mode 'auto-mode-alist)
(assq-set-equal "\\(?:<\\?xml\\s +[^>]*>\\)?\\s *<\\(?:!--\\(?:[^-]\\|-[^-]\\)*-->\\s *<\\)*\\(?:!DOCTYPE\\s +[^>]*>\\s *<\\s *\\(?:!--\\(?:[^-]\\|-[^-]\\)*-->\\s *<\\)*\\)?[Hh][Tt][Mm][Ll]" 'html-helper-mode 'magic-mode-alist)
(defun smart-dnd-html ()
   (smart-dnd-setup
    '(
      ("\\.gif\\'" . "<img src=\"%r\">\n")
      ("\\.jpg\\'" . "<img src=\"%r\">\n")
      ("\\.png\\'" . "<img src=\"%r\">\n")
      ("\\.css\\'" . "<link rel=\"stylesheet\" type=\"text/css\" href=\"%r\">\n" )
      ("\\.js\\'"  . "<script type=\"text/javascript\" src=\"%r\"></script>\n" )
      (".*" . "<a href=\"%r\">%f</a>\n")
      )))
(add-hook 'html-mode-hook 'smart-dnd-html)
 
(autoload 'javascript-mode "javascript-mode" "JavaScript mode" t)
(assq-set-equal "\\.js$" 'javascript-mode 'auto-mode-alist)

(autoload 'applescript-mode "applescript-mode" 
  "major mode for editing AppleScript source." t)
(assq-set-equal "\\.applescript$" 'applescript-mode 'auto-mode-alist)

(autoload 'php-mode "php-mode" "major mode for editing PHP source." t)
(assq-set-equal "\\.php$" 'php-mode 'auto-mode-alist)

;; do we need to distinguish?
(autoload 'rails-minor-mode "rails.el" "Enter Ruby on Rails mode" 'interactive nil)

;; Matlab
;; (autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
;; (autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
;; (assq-set-equal "\\.m$" 'matlab-mode 'auto-mode-alist) 
(load "matlab-emacs/matlab-load.el")

;; Objective C

(defun objc-mode-buffer-check ()
  (and buffer-file-name
       (string-match "\\.m$" buffer-file-name)
       (save-restriction
	 (narrow-to-region (point-min)
			   (min (point-max)
				(+ (point-min) magic-mode-regexp-match-limit)))
	 (looking-at "\\(.\\|\n\\)*#\\(include\\|import\\|define\\)"))))

(setq magic-mode-alist
     (append '(("\\(.\\|\n\\)*\n@\\(implementation\\|interface\\|protocol\\)" . objc-mode)
	       (objc-mode-buffer-check . objc-mode))
	   magic-mode-alist))

(assq-set-equal "\\.org\\'" 'org-mode 'auto-mode-alist) 
(aquamacs-set-defaults 
 '((org-support-shift-select t)))

;; ---------------------------------------------------------
;; PERL EDITING  

(autoload 'cperl-mode "cperl-mode" "major mode for editing Perl source." t)
(assq-set-equal "\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'"  
		'cperl-mode 'auto-mode-alist)
(assq-set-equal "perl" 'cperl-mode 'interpreter-mode-alist)
(assq-set-equal "perl5" 'cperl-mode 'interpreter-mode-alist)
(assq-set-equal "miniperl" 'cperl-mode 'interpreter-mode-alist)

(aquamacs-set-defaults 
 '((cperl-invalid-face nil)
   (cperl-highlight-variables-indiscriminately t)))

;; C-Mode
(defun smart-dnd-c () (smart-dnd-setup '(("\\.h\\'" . "#include <%f>"))))
(add-hook 'c-mode-common-hook 'smart-dnd-c)
(setq auto-mode-alist
	(append
	 '(("\\.cp\\'"		. c++-mode)    ;; old Mac c++ code
          )
	 auto-mode-alist))

(add-to-list (quote interpreter-mode-alist) (quote ("jython" . jython-mode)))

(add-to-list (quote interpreter-mode-alist) (quote ("python" . python-mode)))

(add-to-list (quote auto-mode-alist) (quote ("\\.py\\'" . python-mode)))

(autoload 'prolog-mode "prolog.el" "Major mode for editing Prolog code." 'interactive nil)


(autoload (quote py-shell) "python-mode" 
  "Start an interactive Python interpreter in another window.")
(defalias 'python-shell 'py-shell)
; what about run-python - we'll leave it for now

(autoload (quote python-mode) "python-mode" 
  "Major mode for editing Python files." t)

(autoload (quote jython-mode) "python-mode" 
  "Major mode for editing Jython/Jython files." t)

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(assq-set-equal "\\.\\(frm\\|bas\\|cls\\)$" 
		'visual-basic-mode 'auto-mode-alist)

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code.")
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code.")

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files." t)
(assq-set-equal "\\.\\(md\\)$"
		'markdown-mode 'auto-mode-alist)

(autoload 'dart-mode "dart-mode"
  "Major mode for editing Dart files." t)
(assq-set-equal "\\.dart$"
		'dart-mode 'auto-mode-alist)




;; ---------------------------------------------------------
;; ---------------------------------------------------------
;; Autoloads extracted semi-manually with update-file-autoloads


;;;### (autoloads nil "swift-mode" "swift-mode.el" (22114 983 0 0))
;;; Generated autoloads from swift-mode.el

(autoload 'swift-mode-run-repl "swift-mode" "\
Run a REPL process, input and output via buffer `*swift-repl*'.
If there is a process already running in `*swift-repl*', switch to that buffer.
With argument CMD allows you to edit the command line (default is value
of `swift-repl-executable').
With DONT-SWITCH-P cursor will stay in current buffer.
Runs the hook `swift-repl-mode-hook' (after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn CMD &optional DONT-SWITCH-P)" t nil)

(autoload 'swift-mode "swift-mode" "\
Major mode for Apple's Swift programming language.

\\<swift-mode-map>

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))
