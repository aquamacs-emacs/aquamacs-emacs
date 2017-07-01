
(deftheme aquamacs-frame-look
  "The Aquamacs default appearance.")

(custom-theme-set-variables 'aquamacs-frame-look
                            
 '(cursor-type t)
 '(cursor-color "Red")
 '(default-frame-alist
    '((tool-bar-lines . 1)
      (menu-bar-lines . 1)
      (foreground-color . "Black")
      (background-color . "White")
      (vertical-scroll-bars . right)
      (internal-border-width . 0)
      (left-fringe . 1)
      (right-fringe)
      (fringe))))

(provide-theme 'aquamacs-frame-look)

;; (enable-theme 'aquamacs-frame-look)
