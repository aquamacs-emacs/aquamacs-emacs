Index: recentf.el
===================================================================
RCS file: /sources/emacs/emacs/lisp/recentf.el,v
retrieving revision 1.55
diff -c -r1.55 recentf.el
*** lisp/recentf.el	21 Jan 2007 03:53:11 -0000	1.55
--- lisp/recentf.el	23 Mar 2007 08:26:50 -0000
***************
*** 363,373 ****
  (defsubst recentf-push (filename)
    "Push FILENAME into the recent list, if it isn't there yet.
  If it is there yet, move it at the beginning of the list.
! If `recentf-case-fold-search' is non-nil, ignore case when comparing
! filenames."
    (let ((m (recentf-string-member filename recentf-list)))
      (and m (setq recentf-list (delq (car m) recentf-list)))
!     (push filename recentf-list)))
  
  (defun recentf-apply-filename-handlers (name)
    "Apply `recentf-filename-handlers' to file NAME.
--- 363,384 ----
  (defsubst recentf-push (filename)
    "Push FILENAME into the recent list, if it isn't there yet.
  If it is there yet, move it at the beginning of the list.
! If `recentf-initialize-file-name-history' is non-nil, update 
! the file name history in the same way. If `recentf-case-fold-search' 
! is non-nil, ignore case when comparing filenames."
    (let ((m (recentf-string-member filename recentf-list)))
      (and m (setq recentf-list (delq (car m) recentf-list)))
!     (push filename recentf-list)) 
!   (when (and recentf-initialize-file-name-history
! 	     ;; prevent adding files opened via minibuffer interaction
! 	     ;; a second time.
! 	     (not (and file-name-history
! 		       (recentf-string-equal 
! 			(recentf-expand-file-name (car file-name-history))
! 			filename))))
!     (let ((m (recentf-string-member filename file-name-history)))
!       (and m (setq file-name-history (delq (car m) file-name-history)))
!       (push filename file-name-history))))
  
  (defun recentf-apply-filename-handlers (name)
    "Apply `recentf-filename-handlers' to file NAME.
***************
*** 427,435 ****
    "Add or move FILENAME at the beginning of the recent list.
  Does nothing if the name satisfies any of the `recentf-exclude'
  regexps or predicates."
!   (setq filename (recentf-expand-file-name filename))
!   (when (recentf-include-p filename)
!     (recentf-push filename)))
  
  (defsubst recentf-remove-if-non-kept (filename)
    "Remove FILENAME from the recent list, if file is not kept.
--- 438,446 ----
    "Add or move FILENAME at the beginning of the recent list.
  Does nothing if the name satisfies any of the `recentf-exclude'
  regexps or predicates."
!   (let ((exp-filename (recentf-expand-file-name filename)))
!     (when (recentf-include-p exp-filename)
!       (recentf-push exp-filename))))
  
  (defsubst recentf-remove-if-non-kept (filename)
    "Remove FILENAME from the recent list, if file is not kept.
***************
*** 1268,1274 ****
          (insert "\n\n;;; Local Variables:\n"
                  (format ";;; coding: %s\n" recentf-save-file-coding-system)
                  ";;; End:\n")
!         (write-file (expand-file-name recentf-save-file))
          (when recentf-save-file-modes
            (set-file-modes recentf-save-file recentf-save-file-modes))
          nil)
--- 1279,1286 ----
          (insert "\n\n;;; Local Variables:\n"
                  (format ";;; coding: %s\n" recentf-save-file-coding-system)
                  ";;; End:\n")
! 	(let (write-file-functions) ; don't add this file to the history
! 	  (write-file (expand-file-name recentf-save-file)))
          (when recentf-save-file-modes
            (set-file-modes recentf-save-file recentf-save-file-modes))
          nil)
