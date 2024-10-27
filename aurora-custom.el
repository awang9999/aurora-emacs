;; aurora-custom provides a place for you to call all the packages and package configurations
;; unrelated to faces and theming. You may use all of my configuration, pick and choose
;; which packages/configurations you want, or use your own entirely.
;; These custom packages are loaded after the theme so for theme-related
;; configurations, please load them at the right level.

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


(provide 'aurora-custom)
