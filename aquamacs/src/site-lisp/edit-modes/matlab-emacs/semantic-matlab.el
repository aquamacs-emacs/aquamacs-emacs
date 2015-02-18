;;; semantic-matlab.el --- Semantic details for MATLAB files

;;; Copyright (C) 2004, 2005, 2008 Eric M. Ludlam: The Mathworks, Inc

;; Author: Eric M. Ludlam <eludlam@mathworks.com>
;; X-RCS: $Id: semantic-matlab.el,v 1.14 2009/07/06 19:49:09 zappo Exp $

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Parse a MATLAB M file for use w/ CEDET/Semantic
;;
;; The MATLAB language is pretty simple from a functional standpoint in that
;; you can only declare functions.  In addition, the language itself is not
;; expressable in a yacc style grammar.  It is therefore more expedient
;; to scan for regular expressions.
;;
;; Caveat: MCOS classes have property declarations. @todo - support them

(require 'mode-local)
(require 'semantic)
(require 'semantic-format)
(require 'matlab)
(require 'semanticdb-matlab)

;;; Code:
(defvar semantic-matlab-system-paths-include '("toolbox/matlab/funfun" "toolbox/matlab/general")
  "List of include paths under `semantic-matlab-root-directory'.
These paths will be parsed recursively by semantic.  Class and
private directories will be omitted here.")

(defvar semantic-matlab-root-directory
  (let* ((mlab (locate-file "matlab" exec-path))
	 (mlint (and (boundp 'mlint-program)
		     mlint-program))
	 (exe (or mlab mlint)))
    (if exe
	(let ((dir
	       (or (file-symlink-p exe)
		   exe)))
	  ;; If we have a dir, take everything until /bin as root dir.
	  (string-match "\\(.*\\)/bin.*" dir)
	  (match-string 1 dir))
      (message "semantic-matlab: Could not find MATLAB executable in path.")
      nil))
  "Root directory of MATLAB installation.
Will be automatically determined by MATLAB or mlint executable.
Use `semantic-matlab-system-paths-include' to let semantic know
which system directories you would like to include when doing
completions.")

(defun semantic-matlab-root-directory ()
  "Calculate the current MATLAB root directory."
  (if (matlab-shell-active-p)
      (matlab-shell-matlabroot)
    semantic-matlab-root-directory))

;; The version of this variable in MATLAB.el is not condusive to extracting
;; the information we need.
(defvar semantic-matlab-match-function-re
  "\\(^\\s-*function\\b[ \t\n.]*\\)\\(\\[[^]]+\\]\\s-*=\\|\\w+\\s-*=\\|\\)\\s-*\\(\\(\\sw\\|\\s_\\)+\\)\\>"
  "Expression to match a function start line.")

;; This function may someday be a part of matlab.el.
;; It does the raw scan and split for function tags.
(defun semantic-matlab-function-tags (&optional buffer)
  "Find all MATLAB function tags in BUFFER.
Return argument is:
  (START END RETURNVARS NAME ARGUMENTS DOCSTRING).
Note that builtin functions from MATLAB will always return
START=END=0 and no arguments or return values."
  (save-excursion
    (if buffer (set-buffer buffer))
    (let ((re semantic-matlab-match-function-re)
	  start ret fn arg end doc
	  (taglist nil)
	  )
      (goto-char (point-min))
      (if (and (string-match (format "^%s" (semantic-matlab-root-directory))
			     (buffer-file-name))
	       (looking-at "%\\([A-Z0-9_]+\\)\\s-+\\(.*\\)\\s-*$"))
	  ;; This is a builtin function, ie there's no function line.
	  ;; Hence we must use function name from the doc string.
	  ;; FIXME
	  ;; How can we get function arguments/return vals for builtin func's?
	  (setq taglist
		(cons (list 0 0 nil (downcase (match-string-no-properties 1))
			    nil (match-string-no-properties 2) t
			    )
		      taglist))
	;; this is a either not builtin or a user function
	(while (re-search-forward re nil t)
	  (setq start (match-beginning 0)
		ret (buffer-substring-no-properties
		     (match-beginning 2) (match-end 2))
		fn (buffer-substring-no-properties
		    (match-beginning 3) (match-end 3))
		arg (buffer-substring-no-properties
		     (match-end 3) (save-excursion
				     (matlab-end-of-command)
				     (point)))
		doc (save-excursion
		      (forward-line)
		      (beginning-of-line)
		      ;; snarf doc string
		      (cond
		       ;; Mathworks standard
		       ((looking-at "%[A-Z0-9_]+\\s-+\\(.*\\)\\s-*$")
			(match-string-no-properties 1))
		       ;; lookfor string
		       ((looking-at "%\\s-+\\(.*\\)\\s-*$")
			(match-string-no-properties 1))
		       ;; otherwise simply snarf first line of
		       ;; comments under function declaration
		       (t
			(re-search-forward "[^[:blank:][:cntrl:]]" nil t)
			(backward-char)
			(if (looking-at "%\\s-+\\(.*\\)")
			    (match-string-no-properties 1)
			  nil))))
		end (save-excursion
		      (goto-char start)
		      (if matlab-functions-have-end
			  (condition-case nil
			      ;; If we get a failure, we should at least
			      ;; return whatever we got so far.
			      (matlab-forward-sexp)
			    (error (point-max)))
			(matlab-end-of-defun))
		      (point)))
	  (setq taglist
		(cons (list start end
			    (split-string ret "[][,=. \t\n]+" t)
			    fn
			    (split-string arg "[(), \n\t.]+" t)
			    doc
			    nil
			    )
		      taglist))))
	(nreverse taglist))))

(defun semantic-matlab-parse-oldstyle-class (tags &optional buffer)
  "Check if BUFFER with current TAGS is the constructor of a class.
If this is the case, retrieve attributes from the buffer and scan
the whole directory for methods.  The function returns a single tag
describing the class.  This means that in semantic-matlab, the
old-style MATLAB classes are linked to the constructor file."
  (let* ((name (buffer-file-name buffer))
	 class methods retval attributes)
    (when (string-match ".*/@\\(.*?\\)/\\(.*?\\)\\.m" name)
      ;; this buffer is part of a class - check
      (setq class (match-string 1 name))
      (setq method (match-string 2 name))
      (when (string= class method)	; is this the constructor?
	;; get attributes of the class
	;; TODO - we blindly assume the constructor is correctly defined
	(setq retval (semantic-tag-get-attribute (car tags) :return))
	(goto-char (point-min))
	;; search for attributes
	(while (re-search-forward
		(concat "^\\s-*" (car retval)
			"\\.\\([A-Za-z0-9_]+\\)\\s-*=\\s-*\\(.+\\);")
		nil t)
	  (push (list (match-string-no-properties 1) ; name
		      (match-string-no-properties 2)) ; default value
		attributes))
	;; now scan the methods
	(dolist (cur (delete class
			     (nthcdr 2
				     (assoc class semanticdb-matlab-user-class-cache))))
	  (push
	   (semantic-tag-put-attribute
	    (car (semanticdb-file-stream
		  (concat
		   (file-name-directory name)
		   cur ".m")))
	    :typemodifiers '("public"))
	   methods))
	;; generate tag
	(semantic-tag-new-type
	 class
	 "class"
	 (append
	  (mapcar (lambda (cur)
		    (semantic-tag-new-variable
		      (car cur) nil (cdr cur)
		      :typemodifiers '("public")))
		  attributes)
	  methods)
	 nil
	 :typemodifiers '("public"))))))

(defun semantic-matlab-find-oldstyle-classes (files)
  "Scan FILES for old-style Matlab class system.
Returns an alist with elements (CLASSNAME LOCATION METHODS)."
  (let (classes temp tags)
    (dolist (cur files)
      ;; scan file path for @-directory
      (when (string-match "\\(.*\\)/@\\(.*?\\)/\\(.*?\\)\\.m" cur)
	(if (setq temp
		  (assoc (match-string 2 cur) classes))
	    (nconc temp `(,(match-string 3 cur)))
	  (push `( ,(match-string 2 cur) ,(match-string 1 cur)
		   ,(match-string 3 cur)) classes))))
    classes))

;;; BEGIN PARSER
;;
(defun semantic-matlab-parse-region (&rest ignore)
  "Parse the current MATLAB buffer for function definitions.
IGNORE any arguments which specify a subregion to parse.
Each tag returned is a semantic FUNCTION tag.  See
`semantic-tag-new-function'."
  (semanticdb-matlab-cache-files)
  (let ((raw (condition-case nil
		 ;; Errors from here ought not to be propagated.
		 (semantic-matlab-parse-functions)
	       (error nil)))
	tags ctags)
    (setq tags (mapcar 'semantic-matlab-expand-tag raw))
    ;; check if this is a class constructor
    (setq ctags (list (semantic-matlab-parse-oldstyle-class tags)))
    (if (car ctags)
	ctags
      tags)))

(defun semantic-matlab-parse-changes ()
  "Parse all changes for the current MATLAB buffer."
  ;; NOTE: For now, just schedule a full reparse.
  ;;       To be implemented later.
  (semantic-parse-tree-set-needs-rebuild))

(defun semantic-matlab-expand-tag (tag)
  "Expand the MATLAB function tag TAG."
  (let ((chil (semantic-tag-components-with-overlays tag)))
    (if chil
        (semantic-tag-put-attribute
         tag :members (mapcar 'semantic-matlab-expand-tag chil)))
    (car (semantic--tag-expand tag))))

(defun semantic-matlab-parse-functions ()
  "Parse all functions from the current MATLAB buffer."
  (car
   (semantic-matlab-sort-raw-tags (semantic-matlab-function-tags)
				  (point-max))
   ))

(defun semantic-matlab-sort-raw-tags (tag-list &optional end)
  "Return a split list of tags from TAG-LIST before END.
Return list is:
  (TAGS-BEFORE-END REMAINING-TAGS)"
  (let ((newlist nil)
	(rest tag-list))
    ;; Loop until there are no more tags, or no tags before END.
    (while (and tag-list (> end (car (car tag-list))))
      (let* ((tag (car tag-list))
	     (start (car tag))
	     (end (nth 1 tag))
	     (ret (nth 2 tag))
	     (name (nth 3 tag))
	     (args (nth 4 tag))
	     (doc (nth 5 tag))
	     (builtin (nth 6 tag))
	     (parts (semantic-matlab-sort-raw-tags (cdr tag-list) end))
	     (chil (car parts)))
	(setq rest (car (cdr parts)))
	(setq newlist
	      (cons (append
		     (semantic-tag-new-function name nil args
						:return ret
						:subfunctions chil
						:documentation doc
						:builtin builtin)
		     (list start end))
		    newlist))
	(setq tag-list rest)))
    (list (nreverse newlist) tag-list)))

;; The following function tries to parse MATLAB variable
;; assignments. There are only three categories of types: doubles,
;; structs, and classes. It returns a list with elements
;; (NAME TYPE ATTRIBUTES), for example:
;; ("astruct" "struct" "field1" "field2" "field3")
;; ("aclass" "class" "exampleclass")
;; ("anumber" "double" "1356")
;; Of course we can't parse things we don't know, e.g.
;; if (a==5) variable=aclass; else variable=anotherclass;
;; In these cases, the latter assignment counts.
;; Also, we don't know return types of functions (yet...) - the parser
;; will always think it's "double".  You can override the
;; parser with a special comment, like: %type% avariable = aclass

;; One day we might use a grammar for this...?

(defconst semantic-matlab-type-hint-string "%type%"
  "Comment string which prefixes a type hint for the parser.")

(defun semantic-matlab-parse-assignments ()
  "Parse assignments in current buffer.
This function starts at current point and goes backwards, until
it reaches a function declaration or the beginning of the buffer.
It returns a list of variable assignments (NAME TYPE ATTRIBUTES),
where NAME is unique."
  (let ((limit (or (save-excursion
		     (if (re-search-backward semantic-matlab-match-function-re nil t)
			 (progn
			   (forward-line 1)
			   (point))
		       nil))
		   (point-min)))
	vars)
    ;; don't parse current line
    (beginning-of-line)
    (while (re-search-backward (concat "^\\(" (regexp-quote semantic-matlab-type-hint-string)
				       "\\)?\\([^%]*[^=><~]\\)=\\([^=].*\\)$") limit t)
      (let ((left (match-string-no-properties 2))
	    (right (match-string-no-properties 3))
	    temp)
	;; first we have to deal with elipsis...
	(save-excursion
	  (while (string-match
		(concat "\\(.*\\)"
			(regexp-quote matlab-elipsis-string) "\\s-*$")
		right)
	    (forward-line 1)
	    (setq right
		  (concat
		   (match-string 1 right)
		   (progn (looking-at "^.*$")
			  (match-string-no-properties 0))))))
	(save-excursion
	  (while (and (not (bobp))
		      (progn
			(forward-line -1)
			(looking-at
			 (concat "\\(.*\\)"
				 (regexp-quote matlab-elipsis-string) "\\s-*$"))))
	    (setq left
		  (concat (match-string-no-properties 1) left))))
	;; remove bracket expressions and beginning/trailing whitespaces on left-hand side
	(while (or (string-match "\\((.*)\\|{.*}\\)" left)
		   (string-match "^\\(\\s-+\\)" left)
		   (string-match "\\(\\s-+\\)$" left))
	  (setq left (replace-match "" t t left)))
	;; deal with right-hand side
	(cond
	 ;; special case: a = set(class,attribute,value)
	 ((string-match "\\s-*set(\\s-*\\([A-Za-z_0-9 ]+\\)\\s-*," right)
	  (setq right (match-string 1 right)))
	 ;; method call which returns same class: class=method(class [,args])
	 ((and (string-match "\\s-*[A-Za-z_0-9 ]+\\s-*(\\s-*\\([A-Za-z_0-9 ]+\\)\\s-*\\(,\\|)\\)" right)
	       (string= left (match-string 1 right)))
	  (setq right (match-string 1 right)))
	;; otherwise reduce right-hand side to first symbol
	(t 
	 (string-match "[[({ ]*\\([A-Za-z_0-9]*\\)" right)
	 (setq right (match-string 1 right))))
	(cond
	 ;; multiple assignment, e.g. [a,b]=size(A);
	 ((string-match "\\[\\(.*\\)\\]" left)
	  (dolist (cur (split-string (match-string 1 left) ","))
	    (string-match "\\s-*\\([A-Za-z_0-9]+\\)\\s-*" cur)
	    (setq cur (match-string 1 cur))
	    (unless (assoc cur vars)
	      ;; since we don't know any return types, we just say it's double
	      (push (list cur "double" "") vars))))
	 ;; (nested) structure
	 ((string-match "\\([A-Za-z_0-9.]+\\)\\.\\([A-Za-z_0-9]+\\)" left)
	  (while (string-match "\\([A-Za-z_0-9.]+\\)\\.\\([A-Za-z_0-9]+\\)" left)
	    (let ((name (match-string 1 left))
		  (field (match-string 2 left)))
	      (if (setq temp (assoc name vars))
		  (unless (member field temp)
		    (nconc temp (list field)))
		(push (list name "struct" field)
		      vars))
	      (setq left name))))
	 ;; class
	 ((assoc right semanticdb-matlab-user-class-cache)
	  (string-match "\\([A-Za-z_0-9]+\\)\\s-*$" left)
	  (setq left (match-string 1 left))
	  (if (and (setq temp (assoc left vars))
		   (string= (nth 1 temp) "struct"))
	      ;; we first thought it's a structure, but it's probably a
	      ;; new-style class
	      (setcdr temp `("class" ,right))
	    (unless temp
	      (push `(,left "class" ,right) vars))))
	 (t
	  ;; default is double
	  (string-match "\\([A-Za-z_0-9]+\\)\\s-*$" left)
	  (setq left (match-string 1 left))
	  (unless (or (assoc left vars)
		      (string= left right)) ; self assignment
	    (push `(,left "double" ,right) vars)))
	 )))
    vars))



(define-mode-local-override semantic-get-local-variables
  matlab-mode (&optional point)
  "Return a list of local variables for POINT."
  (semanticdb-matlab-cache-files)
  (save-excursion
    (let ((vars (semantic-matlab-parse-assignments))
	  knowntypes tags)
      (dolist (cur vars)
	;; check right-hand side for known types which might be
	;; assigned this variable
	(if (string= (nth 1 cur) "double")
	    (when (member (nth 2 cur) knowntypes)
	      (setcdr cur (cdr (assoc (nth 2 cur) vars))))
	  (push (nth 0 cur) knowntypes))
	;; generate the tag
	(push
	 (semantic-tag-new-variable
	  (car cur)
	  (cond
	   ;; structures
	   ((string= (cadr cur) "struct")
	    (semantic-tag-new-type
	     ;; this is just a placeholder
	     (concat (car cur) "_struct")
	     "struct"
	     (mapcar
	      (lambda (x)
		(semantic-tag-new-variable
		 x
		 nil nil :typemodifiers '("public") ))
	      (nthcdr 2 cur))
	     nil))
	   ;; classes
	   ((string= (cadr cur) "class")
	    ;; include tags from class constructor
	    ;; (contains whole class, including methods)
	    (car (semanticdb-file-stream
		  (concat
		   (nth 1 (assoc (nth 2 cur) semanticdb-matlab-user-class-cache))
		   "/@" (nth 2 cur) "/" (nth 2 cur) ".m"))))
	   (t
	    nil)))
	 tags))
      tags)))


(define-mode-local-override semantic-tag-components-with-overlays
  matlab-mode (tag)
  "Return the list of subfunctions in TAG."
  (semantic-tag-get-attribute tag :subfunctions))

(define-mode-local-override semantic-format-tag-prototype matlab-mode
  (tag &optional parent color)
  "Return a prototype string describing tag.
For MATLAB, we have to mark builtin functions, since we currently
cannot derive an argument list for them."
  (let ((class (semantic-tag-class tag))
	(name (semantic-format-tag-name tag parent color))
	str)
    (if (eq class 'function)
	(let* ((args  (semantic-tag-function-arguments tag))
	       (argstr (semantic--format-tag-arguments args
						       #'identity
						       color))
	       (builtin (semantic-tag-get-attribute tag :builtin))
	       (doc (semantic-tag-docstring tag)))
	  (if builtin
	      (if color
		  (setq builtin (semantic--format-colorize-text " [builtin] " 'keyword)
			argstr (semantic--format-colorize-text " arguments unavailable" 'label))
		(setq builtin " [builtin] "
		      argstr " arguments unavailable"))
	    (setq builtin ""))
	  (concat name builtin "(" (if args " " "")
		  argstr
		  " )"))
      (semantic-format-tag-prototype-default tag parent color))))

(defun semantic-idle-summary-format-matlab-mode (tag &optional parent color)
  "Describe TAG and display corresponding MATLAB 'lookfor' doc-string."
  (let* ((proto (semantic-format-tag-prototype-matlab-mode tag nil color))
	 (doc (semantic-tag-docstring tag)))
    (concat proto " (" doc ")")))

(defcustom-mode-local-semantic-dependency-system-include-path
  matlab-mode semantic-matlab-dependency-system-include-path
  (if semantic-matlab-root-directory
      (mapcar (lambda (cur)
		(concat (file-name-as-directory semantic-matlab-root-directory)
			cur))
	      semantic-matlab-system-paths-include)
    nil)
  "The system include paths from MATLAB.")

(defvar-mode-local matlab-mode semantic-idle-summary-function
  'semantic-idle-summary-format-matlab-mode
  "Function to use when displaying tag information during idle time.")

(defvar semantic-matlab-display-docstring t
  "Flag if function documentation should be displayed after completion.")

(define-mode-local-override semantic-ia-insert-tag
  matlab-mode (tag)
  "Insert TAG into the current buffer based on completion."
  (insert (semantic-tag-name tag))
  (let ((name (semantic-tag-name tag))
	(tt (semantic-tag-class tag))
	(args (semantic-tag-function-arguments tag))
	(doc (semantic-tag-docstring tag)))
    (when (and (eq tt 'function)
	       args
	       (not (looking-at "\\s-*(")))
      (insert "("))
    ;; delete trailing whitespaces when completing class methods
    (when (looking-at "\\(\\s-+\\)(")
      (delete-char (length (match-string 1))))
    (when semantic-matlab-display-docstring
      (fame-message-nolog
       (semantic-idle-summary-format-matlab-mode tag nil t)))))

(define-mode-local-override semantic-ctxt-current-symbol
  matlab-mode (&optional point)
  "Return the current symbol the cursor is on at point in a list.
This will include a list of type/field names when applicable."
  (let*  ((case-fold-search semantic-case-fold)
	  sym)
    (with-syntax-table semantic-lex-syntax-table
      (save-excursion
	(when point (goto-char point))
	;; go to beginning of symbol
	(skip-syntax-backward "w_")
	(setq sym (if (looking-at "[a-zA-Z_0-9]+")
		      (match-string-no-properties 0)
		    nil))
	(if sym
	    (cond
	     ;; method call: var = method(class,args)
	     ((progn
		(and (looking-back "[^=><~]=\\s-*")
		     (looking-at "[a-zA-Z_0-9]*\\s-*(\\([a-zA-Z_0-9]+\\),?")))
	      (list (match-string-no-properties 1) sym))
	     ;; class properties: var = get(class,'attribute')
	     ((looking-back "\\(get\\|set\\)(\\s-*\\([a-zA-Z_0-9]+\\),'")
	      (list (match-string-no-properties 2) sym))
	     ;; (nested) structures or new-style classes
	     ((looking-back "[^A-Za-z_0-9.]\\([A-Za-z_0-9.]+\\)\\.")
	      (list (match-string-no-properties 1) sym))
	     (t
	      (list sym)))
	  nil)))))

 (define-mode-local-override semantic-ctxt-current-symbol-and-bounds
   matlab-mode (&optional point)
   "Return the current symbol and bounds the cursor is on at POINT.
 Uses `semantic-ctxt-current-symbol' to calculate the symbol.
 Return (PREFIX ENDSYM BOUNDS)."
   (let ((sym (semantic-ctxt-current-symbol-matlab-mode point))
	 bounds endsym)
     (save-excursion
       (when point (goto-char point))
       (when sym
	 ;; go to beginning of symbol
	 (skip-syntax-backward "w_")
	 (setq endsym (progn (looking-at "[a-zA-Z_0-9]+")
			     (match-string-no-properties 0)))
	 (setq bounds (cons
		       (match-beginning 0)
		       (match-end 0)))
	 (list sym endsym bounds)))))


;;;###autoload
(defun semantic-default-matlab-setup ()
  "Set up a buffer for parsing of MATLAB files."
  ;; This will use our parser.
  (semantic-install-function-overrides
   '((parse-region . semantic-matlab-parse-region)
     (parse-changes . semantic-matlab-parse-changes)))
  (setq semantic-parser-name "MATLAB"
        ;; Setup a dummy parser table to enable parsing!
        semantic--parse-table t
        imenu-create-index-function 'semantic-create-imenu-index
	;; semantic-command-separation-character "."
	semantic-type-relation-separator-character '(".")
	semantic-symbol->name-assoc-list '((function . "Function")
					   )
	semantic-imenu-expandable-tag-classes '(function)
	semantic-imenu-bucketize-file nil
	semantic-imenu-bucketize-type-members nil
	senator-step-at-start-end-tag-classes '(function)
	semantic-stickyfunc-sticky-classes '(function)
	)
  )

(provide 'semantic-matlab)

;;; semantic-matlab.el ends here
