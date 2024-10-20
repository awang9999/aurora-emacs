(require 'aurora-base-colors)

(defun aurora-theme-set-dark ()
  "Apply dark Aurora theme base."
  ;; Colors from Nord theme at https://www.nordtheme.com
  (setq frame-background-mode     'dark)
  (setq aurora-color-foreground "#ECEFF4") ;; Snow Storm 3  / nord  6
  (setq aurora-color-background "#2E3440") ;; Polar Night 0 / nord  0
  (setq aurora-color-highlight  "#3B4252") ;; Polar Night 1 / nord  1
  (setq aurora-color-critical   "#EBCB8B") ;; Aurora        / nord 11
  (setq aurora-color-salient    "#81A1C1") ;; Frost         / nord  9
  (setq aurora-color-strong     "#ECEFF4") ;; Snow Storm 3  / nord  6
  (setq aurora-color-popout     "#D08770") ;; Aurora        / nord 12
  (setq aurora-color-subtle     "#434C5E") ;; Polar Night 2 / nord  2
  (setq aurora-color-faded      "#677691") ;;
  ;; to allow for toggling of the themes.
  (setq aurora-theme-var "dark")
  )

(provide 'aurora-theme-dark)
