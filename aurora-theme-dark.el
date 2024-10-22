(require 'aurora-base-colors)
(require 'aurora-colors)

(defun aurora-theme-set-dark ()
  "Apply dark Aurora theme base."
  ;; Colors from Nord theme at https://www.nordtheme.com
  (setq frame-background-mode     'dark)
  (setq aurora-color-foreground (nord-color "snow-storm-0"))   ;; Snow Storm 0 
  (setq aurora-color-background (nord-color "polar-night-0"))  ;; Polar Night 0
  (setq aurora-color-highlight  (nord-color "polar-night-2"))  ;; Polar Night 2
  (setq aurora-color-critical   (nord-color "aurora-0"))       ;; Aurora 0
  (setq aurora-color-salient    (nord-color "frost-1"))        ;; Frost 1
  (setq aurora-color-strong     (nord-color "aurora-4"))       ;; Aurora 4
  (setq aurora-color-popout     (nord-color "aurora-2"))       ;; Aurora 2
  (setq aurora-color-subtle     (nord-color "polar-night-3"))  ;; Polar Night 3
  (setq aurora-color-faded      (nord-color "polar-night-4"))  ;; Polar Night 4
  ;; to allow for toggling of the themes.
  (setq aurora-theme-var "dark")
  )

(provide 'aurora-theme-dark)
