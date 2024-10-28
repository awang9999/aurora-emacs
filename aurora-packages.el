(require 'package)

(setq package-archive-priorities
      '(("gnu" . 10)
	    ("melpa" . 5)
	    ("org" . 4))
      )

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	    ("elpa" . "https://elpa.gnu.org/packages/")
	    ("org" . "https://orgmode.org/elpa/"))
      )

(package-initialize)

;; Delay loading package until it is used (improves startup time)
;; (setq use-package-always-defer t)

;; Download packages when they are not available on the system
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Automatically update packages and delete old versions
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
)

(provide 'aurora-packages)

