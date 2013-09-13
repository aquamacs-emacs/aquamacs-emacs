(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	  (normal-top-level-add-subdirs-to-load-path))
;; This will not load edit-modes, which are cached at preload time.
