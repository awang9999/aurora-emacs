;; aurora-custom provides a place for you to call all the packages and package configurations
;; unrelated to faces and theming. You may use all of my configuration, pick and choose
;; which packages/configurations you want, or use your own entirely.
;; These custom packages are loaded after the theme so for theme-related
;; configurations, please load them at the right level.

;; Org mode configuration
(require 'aurora-org)

;; Org-roam configuration
(require 'aurora-org-roam)

;; Consult provides search and navigation commands     | https://github.com/minad/consult
(require 'aurora-consult)
;; Vertico minibuffer completion                       | https://github.com/minad/vertico
(require 'aurora-vertico)
;; Orderless completion style                          | https://github.com/oantolin/orderless
(require 'aurora-orderless)
;; marginalia provides annotations                     | https://github.com/minad/marginalia
(require 'aurora-marginalia)
;; embark provides context-aware actions               | https://github.com/oantolin/embark
(require 'aurora-embark)
;; Corfu in-buffer completion                          | https://github.com/minad/corfu
(require 'aurora-corfu)

;; Magit - it's magitcal!                              | https://github.com/magit/magit
(require 'aurora-magit)

;; Fringe configuration
(require 'aurora-fringe)

;; Undo tree
(require 'aurora-undo)

;; language major modes and eglot
(require 'aurora-languages)

;; Snippets configuration
(require 'aurora-snippets)

;; Nerd/all-the icons and Kind Icon                    | look it up
(require 'aurora-icons)

;; Centaur Tabs - better tab functionality             | https://github.com/ema2159/centaur-tabs
(require 'aurora-tabs)

;; Vterm
(require 'aurora-terminal)

;; emacs-dashboard
(require 'aurora-dashboard)

;; doom-modeline
(require 'aurora-modeline)

;; Treemacs!
(require 'aurora-treemacs)

(provide 'aurora-custom)
