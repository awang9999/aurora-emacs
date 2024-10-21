(require 'aurora-base-colors)
(require 'aurora-colors)

(defun aurora-theme-set-light ()
  "Apply light Aurora theme base."
  ;; Colors from Material design at https://material.io/
  (setq frame-background-mode    'light)
  (setq aurora-color-foreground (material-color "blue-grey-8"))   ;; Blue Grey   / L800
  (setq aurora-color-background (material-color "blue-grey-0"))   ;; Blue Grey   / L50
  (setq aurora-color-highlight  (material-color "amber-1"))       ;; Amber       / L100
  (setq aurora-color-critical   (material-color "amber-9"))       ;; Amber       / L900
  (setq aurora-color-salient    (material-color "deep-purple-5")) ;; Deep Purple / L500
  (setq aurora-color-strong     (basic-color "black"))            ;; Black
  (setq aurora-color-popout     (material-color "deep-orange-2")) ;; Deep Orange / L200
  (setq aurora-color-subtle     (material-color "blue-grey-1"))   ;; Blue Grey   / L100
  (setq aurora-color-faded      (material-color "blue-grey-3"))   ;; Blue Grey   / L300
  ;; to allow for toggling of the themes.
  (setq aurora-theme-var "light")
  )

(provide 'aurora-theme-light)
