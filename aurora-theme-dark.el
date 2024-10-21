(require 'aurora-base-colors)

(defun aurora-theme-set-dark ()
  "Apply dark Aurora theme base."
  ;; Colors from Nord theme at https://www.nordtheme.com
  (setq frame-background-mode     'dark)
  (setq aurora-color-foreground (nord-color "nord-6"))  ;; Snow Storm 3  / nord  6
  (setq aurora-color-background (nord-color "nord-0"))  ;; Polar Night 0 / nord  0
  (setq aurora-color-highlight  (nord-color "nord-1"))  ;; Polar Night 1 / nord  1
  (setq aurora-color-critical   (nord-color "nord-11")) ;; Aurora 0      / nord 11
  (setq aurora-color-salient    (nord-color "nord-9"))  ;; Frost 2       / nord  9
  (setq aurora-color-strong     (nord-color "nord-6"))  ;; Snow Storm 2  / nord  6
  (setq aurora-color-popout     (nord-color "nord-12")) ;; Aurora 1      / nord 12
  (setq aurora-color-subtle     (nord-color "nord-2"))  ;; Polar Night 2 / nord  2
  (setq aurora-color-faded      (nord-color "nord-15")) ;; Polar Night 4 / nord 16
  ;; to allow for toggling of the themes.
  (setq aurora-theme-var "dark")
  )

(provide 'aurora-theme-dark)
