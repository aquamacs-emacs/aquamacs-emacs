*** lisp/recentf.el	09 Nov 2006 16:48:46 +0000	1.54
--- lisp/recentf.el	12 Dec 2006 17:02:40 +0000	
***************
*** 359,373 ****
               recentf-auto-cleanup nil 'recentf-cleanup))))))
  
  ;;; File functions
! ;;
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
--- 359,382 ----
               recentf-auto-cleanup nil 'recentf-cleanup))))))
  
  ;;; File functions
! ;; (setq file-name-history nil)
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
! 	     (not (equal (recentf-expand-file-name (car file-name-history))
! 			filename)))
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
--- 436,444 ----
    "Add or move FILENAME at the beginning of the recent list.
  Does nothing if the name satisfies any of the `recentf-exclude'
  regexps or predicates."
!   (let ((exp-filename (recentf-expand-file-name filename)))
!     (when (recentf-include-p exp-filename)
!       (recentf-push exp-filename))))
  
  (defsubst recentf-remove-if-non-kept (filename)
    "Remove FILENAME from the recent list, if file is not kept.
