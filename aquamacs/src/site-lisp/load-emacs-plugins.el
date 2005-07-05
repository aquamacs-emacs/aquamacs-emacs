;; load-emacs-plugins
;; this realized as separate package
;; so it the plugins won't be loaded twice


(provide 'load-emacs-plugins)
(defun load-sitestart-files ()
  (mapcar 
    (lambda (p) (load (concat p "/site-start") 'noerror))
    load-path
    )
  t
)

(load-sitestart-files)
