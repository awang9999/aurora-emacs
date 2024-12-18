(use-package nerd-icons)
(use-package all-the-icons)

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  ;; (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  ;; (kind-icon-blend-frac 0.08)

  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

  ;; Reduce icon size to make corfu completion window not be cut off
  ;; (custom-set-variables
  ;;  '(kind-icon-default-style
  ;;    '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.4 :scale 1.0))
  ;;  '(package-selected-packages '(kind-icon corfu)))

(provide 'aurora-icons)
