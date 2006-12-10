;; create directory for semantic.cache

(make-directory semanticdb-default-save-directory 'dont-complain)

;; require cedet - but only on demand

(require 'cedet)

(load "jde/lisp/jde.el") ;; this will provide `jde'