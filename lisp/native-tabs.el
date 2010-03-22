;; -*- lisp-interaction-mode -*-

(defun handle-tab-event (event)
  "Handle tab-changed-event to change tabs on the frame in EVENT."
  (interactive "e")
  (let* ((keys (nth 1 event))
	 (new-tab (car keys))
	 (old-tab (cdr keys))
	 (frame (nth 2 event))
	 (configs (frame-parameter frame 'tab-config))
	 (new-config (assoc new-tab configs))
	 (old-config (assoc old-tab configs)))
    (if old-config
	(setcdr old-config (current-window-configuration))
      (setq configs (append configs 
			    (list (cons old-tab 
					(current-window-configuration))))))
    (set-frame-parameter frame 'tab-config configs)
    (if new-config
	(set-window-configuration (cdr new-config)))))

(if (featurep 'tabs)
    (progn
      (define-key special-event-map [tab-changed-event]
	'handle-tab-event)
      (global-set-key "\C-x70" 'tab-delete)
      (global-set-key "\C-x71" 'tab-delete-other)
      (global-set-key "\C-x72" 'tab-new)
      (global-set-key "\C-x7n" 'tab-next)
      (global-set-key "\C-x7p" 'tab-previous)))

