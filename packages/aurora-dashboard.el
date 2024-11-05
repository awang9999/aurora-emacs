(use-package dashboard
  :ensure t
  :config
  ;; Set the banner text
  (setq dashboard-banner-logo-title (format "Version %d" emacs-major-version))
  ;; Set the banner
  (setq dashboard-startup-banner 'logo)
  ;; Set the icon type
  (setq dashboard-icon-type 'nerd-icons)
  ;; Enable icons in widget headers and entries
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  ;; Set icon mappings
  (dashboard-modify-heading-icons '((recents   . "nf-oct-issue_reopened")
                                  (projects . "nf-oct-rocket")))
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; vertically center content
  (setq dashboard-vertically-center-content t)
  ;; set what widgets show up
  (setq dashboard-items '((recents   . 5)
                          (projects  . 5)))
  (dashboard-setup-startup-hook))


(provide 'aurora-dashboard)
