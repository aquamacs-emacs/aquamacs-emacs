;; create directory for semantic.cache

(make-directory (concat temporary-file-directory "semantic.cache"))

;; require cedet - but only on demand

(require 'cedet)

(load "jde/jde.el") ;; this will provide `jde'