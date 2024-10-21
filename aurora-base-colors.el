;; Defines the 9 basic colors of the aurora theme.
;; The default values are loaded from well-known faces of Emacs.
;;
;; To change aurora's appearance you therefore may do one of the following:
;; - Load any Emacs theme before loading aurora to change the appearance
;; - Load one of the few aurora themes after this file. This will result in
;;   the best experience.
;; - Set your own colors by customizing aurora group
;;
;; ---------------------------------------------------------------------

(defgroup aurora '()
  "Faces and colors for the aurora emacs theme")

;; Derive our default color set from classic Emacs faces.
;; This allows dropping aurora components into already themed Emacs configurations.
;;
;; We store the default colorset in this variable to not confuse
;; customize: the STANDARD argument of defcustom gets re-evaluated by customize
;; to determine if the current value is default or not.
(defvar aurora-base-colors--defaults
  `((foreground . ,(face-foreground 'default nil t))
    (background . ,(face-background 'default nil t))
    (highlight . ,(face-background 'fringe nil t))
    (critical . ,(face-foreground 'error nil t))
    (salient . ,(face-foreground 'font-lock-keyword-face nil t))
    (strong . ,(face-foreground 'default nil t))
    (popout . ,(face-foreground 'font-lock-string-face nil t))
    (subtle . ,(face-background 'mode-line-inactive nil t))
    (faded . ,(face-foreground 'shadow nil t))))

(defun aurora-base-colors--get (name)
  "Get default color associated with symbol NAME."
  (cdr (assoc name aurora-base-colors--defaults)))

(defcustom aurora-color-foreground (aurora-base-colors--get 'foreground)
  ""
  :type 'color
  :group 'aurora)

(defcustom aurora-color-background (aurora-base-colors--get 'background)
  ""
  :type 'color
  :group 'aurora)

(defcustom aurora-color-highlight (aurora-base-colors--get 'highlight)
  ""
  :type 'color
  :group 'aurora)

(defcustom aurora-color-critical (aurora-base-colors--get 'critical)
  ""
  :type 'color
  :group 'aurora)

(defcustom aurora-color-salient (aurora-base-colors--get 'salient)
  ""
  :type 'color
  :group 'aurora)

(defcustom aurora-color-strong (aurora-base-colors--get 'strong)
  ""
  :type 'color
  :group 'aurora)

(defcustom aurora-color-popout (aurora-base-colors--get 'popout)
  ""
  :type 'color
  :group 'aurora)

(defcustom aurora-color-subtle (aurora-base-colors--get 'subtle)
  ""
  :type 'color
  :group 'aurora)

(defcustom aurora-color-faded (aurora-base-colors--get 'faded)
  ""
  :type 'color
  :group 'aurora)

(provide 'aurora-base-colors)
