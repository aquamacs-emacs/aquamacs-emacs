;; load-emacs-pre-plugins
;; this realized as separate package
;; so it the plugins won't be loaded twice


(provide 'load-emacs-pre-plugins)
(require 'aquamacs-tools) ; for load-pre-sitestart-files
(require 'mac-extra-functions) ; for mac-add-standard-directories


(message "Loading prestart plugin files ...")
(mac-add-standard-directories) ;; make sure standard dirs are loaded
(load-pre-sitestart-files)
(message "... done.")