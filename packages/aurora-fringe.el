(use-package git-gutter
  :hook ((prog-mode org-mode) . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [#b11111000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [#b11111000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted
  [#b11111111
   #b11111111
   #b11111111
   #b11111111] nil nil 'bottom)
  (set-face-foreground 'git-gutter-fr:added "pale green")
  (set-face-foreground 'git-gutter-fr:modified "goldenrod")
  (set-face-foreground 'git-gutter-fr:deleted "slate gray"))

(provide 'aurora-fringe)
