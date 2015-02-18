;; load-emacs-plugins
;; this realized as separate package
;; so it the plugins won't be loaded twice


(provide 'load-emacs-plugins)
(require 'aquamacs-tools) ; for load-post-sitestart-files
(require 'mac-extra-functions) ; for mac-add-standard-directories


(message "Loading plugins ...")
(mac-add-standard-directories) ;; make sure standard dirs are loaded
(load-post-sitestart-files)
(message "... done.")
