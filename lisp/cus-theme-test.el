;; Do eval-buffer to run the test code.

(setq debug-on-error t)
(switch-to-buffer (get-buffer-create "*custom-theme test*"))
(erase-buffer)
(load-file "custom.el")
(load-file "cus-face.el")
(load-file "cus-edit.el")

;; Declare a theme
(deftheme test
  "The test-theme."
  :set-variable-settings
    "This variable was set by the test theme."
  :set-face-settings
    "This face was set by the test theme.")

(if (memq 'test custom-known-themes)
    (insert "memq 'test custom-known-themes\n")
  (error "TEST FAILED: memq 'test custom-known-themes"))

(provide-theme 'test)

(if (memq 'test custom-loaded-themes)
    (insert "memq 'test custom-loaded-themes\n")
  (error "TEST FAILED: memq 'test custom-loaded-themes"))

(if (condition-case var
	(custom-theme-set-variables
	 'unknown
	 '(test-1 "unknown one")
	 '(test-2 "unknown two"))
      (error
       (and (string= "Unknown theme `unknown'" (cadr var)))))
    (insert "custom-theme-set-variable 'unknown\n")
  (error "TEST FAILED: custom-theme-set-variable 'unknown"))

(insert "custom-theme-set-variables\n")

(custom-theme-set-variables
 'test
 '(test-1 "test one")
 '(test-2 "test two" t)
 '(test-3 (+ 1 2) t))

;; check theme variable properties

;; maybe use unintern at the start to run this test repeatedly?
(if (not (boundp 'test-1))
    (insert "boundp test-1\n")
  (error "TEST FAILED: boundp test-1"))

(if (equal (get 'test-1 'saved-value) '("test one"))
    (insert "get 'test-1 'saved-value\n")
  (error "TEST FAILED: get 'test-1 'saved-value"))

(if (member '(test set "test one") (get 'test-1 'theme-value))
    (insert "get 'test-1 'theme-value\n")
  (error "TEST FAILED: get 'test-1 'theme-value"))

(if (eq nil (get 'test-1 'force-value))
    (insert "get 'test-1 'force-value\n")
  (error "TEST FAILED: get 'test-1 'force-value"))

(if (boundp 'test-2)
    (insert "boundp test-2\n")
  (error "TEST FAILED: boundp test-2"))

(if (string= test-2 "test two")
    (insert "string= test-2 ...\n")
  (error "TEST FAILED: string= test-2 ..."))

(if (equal (get 'test-2 'saved-value) '("test two"))
    (insert "get 'test-2 'saved-value\n")
  (error "TEST FAILED: get 'test-2 'saved-value"))

(if (member '(test set "test two") (get 'test-2 'theme-value))
    (insert "get 'test-2 'theme-value\n")
  (error "TEST FAILED: get 'test-2 'theme-value"))

(if (eq 'rogue (get 'test-2 'force-value))
    (insert "get 'test-2 'force-value\n")
  (error "TEST FAILED: get 'test-2 'force-value"))

(if (boundp 'test-3)
    (insert "boundp test-3\n")
  (error "TEST FAILED: boundp test-3"))

(if (= test-3 3)
    (insert "= test-3 3\n")
  (error "TEST FAILED: = test-3 3"))

(if (equal (get 'test-3 'saved-value) '((+ 1 2)))
    (insert "get 'test-3 'saved-value\n")
  (error "TEST FAILED: get 'test-3 'saved-value"))

;; faces for test theme
(if (condition-case var
	(custom-theme-set-faces
	 'unknown
	 '(modeline ((t (:foreground "black" :background "gold")))))
      (error
       (and (string= "Unknown theme `unknown'" (cadr var)))))
    (insert "custom-theme-set-faces 'unknown\n")
  (error "TEST FAILED: custom-theme-set-faces 'unknown"))

(insert "custom-theme-set-faces\n")

(custom-theme-set-faces
 'test
 '(modeline ((t (:foreground "black" :background "gold"))))
 '(fancy ((t (:foreground "white" :background "black"))))
 '(weird ((t (:foreground "yellow" :background "red"))) t))

(if (facep 'modeline)
    (insert "facep 'modeline\n")
  (error "TEST FAILED: facep 'modeline"))

(if (equal (get 'modeline 'saved-face) 
	   '((t (:foreground "black" :background "gold"))))
    (insert "get 'modeline 'saved-face\n")
  (error "TEST FAILED: get 'modeline 'saved-value"))

(if (member '(test set ((t (:foreground "black" :background "gold")))) 
	    (get 'modeline 'theme-face))
    (insert "get 'modeline 'theme-face\n")
  (error "TEST FAILED: get 'modeline 'theme-face"))

(if (eq nil (get 'modeline 'force-face))
    (insert "get 'modeline 'force-face\n")
  (error "TEST FAILED: get 'modeline 'force-face"))

;; (unintern 'fancy)
(if (not (facep 'fancy))
    (insert "facep 'fancy\n")
  (error "TEST FAILED: facep 'fancy"))

(if (equal (get 'fancy 'saved-face)
	   '((t (:foreground "white" :background "black"))))
    (insert "get 'fancy 'saved-face\n")
  (error "TEST FAILED: get 'fancy 'saved-value"))

(if (member '(test set ((t (:foreground "white" :background "black"))))
	    (get 'fancy 'theme-face))
    (insert "get 'fancy 'theme-face\n")
  (error "TEST FAILED: get 'fancy 'theme-face"))

(if (eq nil (get 'fancy 'force-face))
    (insert "get 'fancy 'force-face\n")
  (error "TEST FAILED: get 'fancy 'force-face"))

(if (facep 'weird)
    (insert "facep 'weird\n")
  (error "TEST FAILED: facep 'weird"))

(if (equal (get 'weird 'saved-face)
	   '((t (:foreground "yellow" :background "red"))))
    (insert "get 'weird 'saved-face\n")
  (error "TEST FAILED: get 'weird 'saved-value"))

(if (member '(test set ((t (:foreground "yellow" :background "red"))))
	    (get 'weird 'theme-face))
    (insert "get 'weird 'theme-face\n")
  (error "TEST FAILED: get 'weird 'theme-face"))

(if (eq 'rogue (get 'weird 'force-face))
    (insert "get 'weird 'force-face\n")
  (error "TEST FAILED: get 'weird 'force-face"))
