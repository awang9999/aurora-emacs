;; This file derives all faces in various modes from the fundamental faces
;; defined in aurora-faces.
;;
;; Notable exceptions:
;; - The highlight-line face uses a very light color.
;; - The terminal faces use a palette derived from material theme.
;; that uses a very light color,
;;
;; ---------------------------------------------------------------------
;;; Code:
(require 'aurora-faces)

(defcustom aurora-theme-var nil
  "Variable which sets the default startup theme as light or dark.
Also allows for toggling of the themes. Is set to 'light' by
'aurora-theme-light' and 'dark' by 'aurora-theme-dark'.
Defaults to nil."
  :group 'aurora
  :type 'string)

;; When we set a face, we take care of removing any previous settings
(defun set-face (face style)
  "Reset FACE and make it inherit STYLE."
  (if (facep face)
      (set-face-attribute face nil
                          :foreground 'unspecified :background 'unspecified
                          :family     'unspecified :slant      'unspecified
                          :weight     'unspecified :height     'unspecified
                          :underline  'unspecified :overline   'unspecified
                          :box        'unspecified :inherit    style)
    (message "AURORA Warning: Face %s could not be set. It may not be defined."
             face)))
  


(defun aurora-theme--basics ()
  "Derive basic Emacs faces from aurora-faces and aurora-color-theme."

  (set-foreground-color aurora-color-foreground)
  (set-background-color aurora-color-background)

  ;; XXX the following seems to be a no-op, should it be removed?
  (set-face-attribute 'default nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)
                      :weight     'light
                      :family     (face-attribute 'aurora-face-default :family)
                      :height     (face-attribute 'aurora-face-default :height))

  (if (display-graphic-p)
      (set-face-attribute 'bold nil :weight 'regular)
    (set-face-attribute 'bold nil :weight 'bold))

  ;; Structural
  (set-face 'bold                                     'aurora-face-strong)
  (set-face 'italic                                    'aurora-face-faded)
  (set-face 'bold-italic                              'aurora-face-strong)
  (set-face 'region                                   'aurora-face-subtle)
  (set-face 'highlight                                'aurora-face-subtle)
  ;;(set-face 'fixed-pitch                                     'default)
  (set-face 'fixed-pitch-serif                       'aurora-face-default)
  (set-face 'cursor                                  'aurora-face-default)
  (if 'aurora-font-family-proportional
      (set-face-attribute 'variable-pitch nil ;; to work with mixed-pitch
                :foreground (face-foreground 'default)
                :background (face-background 'default)
                :family     (face-attribute 'aurora-face-variable-pitch :family)
                :height     (face-attribute 'aurora-face-variable-pitch :height))
      (set-face 'variable-pitch                     'aurora-face-default))

  (set-face-attribute 'cursor nil
                      :background (face-foreground 'aurora-face-default))
  (set-face-attribute 'window-divider nil
                      :foreground (face-background 'aurora-face-default))
  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground aurora-color-background)
  ;;                  :foreground (face-background 'aurora-face-subtle))
  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground aurora-color-background)
  ;;                  :foreground (face-background 'aurora-face-subtle)))
  (set-face-foreground 'vertical-border aurora-color-subtle)

  ;; Semantic
  (set-face 'shadow                                    'aurora-face-faded)
  (set-face 'success                                 'aurora-face-salient)
  (set-face 'warning                                  'aurora-face-popout)
  (set-face 'error                                  'aurora-face-critical)
  (set-face 'match                                    'aurora-face-popout)

  ;; General
  (set-face 'buffer-menu-buffer                       'aurora-face-strong)
  (set-face 'minibuffer-prompt                        'aurora-face-strong)
  (set-face 'link                                    'aurora-face-salient)
  (set-face 'fringe                                    'aurora-face-faded)
  (set-face-attribute 'fringe nil
                      :foreground (face-background 'aurora-face-subtle)
                      :background (face-background 'default))
  (set-face 'isearch                                  'aurora-face-strong)
  (set-face 'isearch-fail                              'aurora-face-faded)
  (set-face 'lazy-highlight                           'aurora-face-subtle)
  (set-face 'trailing-whitespace                      'aurora-face-subtle)
  (set-face 'show-paren-match                         'aurora-face-popout)
  (set-face 'show-paren-mismatch                           'face-normal)
  (set-face-attribute 'tooltip nil                         :height 0.85)
  (set-face 'secondary-selection                      'aurora-face-subtle)
  (set-face 'completions-common-part                   'aurora-face-faded)
  (set-face 'completions-first-difference            'aurora-face-default))
(defun aurora-theme--font-lock ()
  "Derive font-lock faces from aurora-faces."
  (set-face 'font-lock-comment-face                    'aurora-face-faded)
  (set-face 'font-lock-doc-face                        'aurora-face-faded)
  (set-face 'font-lock-string-face                    'aurora-face-popout)
  (set-face 'font-lock-constant-face                 'aurora-face-salient)
  (set-face 'font-lock-warning-face                   'aurora-face-popout)
  (set-face 'font-lock-function-name-face             'aurora-face-strong)
  (set-face 'font-lock-variable-name-face             'aurora-face-strong)
  (set-face 'font-lock-builtin-face                  'aurora-face-salient)
  (set-face 'font-lock-type-face                     'aurora-face-salient)
  (set-face 'font-lock-keyword-face                  'aurora-face-salient))


(defun aurora-theme--mode-line ()
  "Derive mode-line and header-line faces from aurora-faces."
  (set-face-attribute 'mode-line nil
                      :height 0.1
                      :foreground (if (display-graphic-p)
                                      (face-background 'aurora-face-default)
                                    (face-foreground 'aurora-face-default))
                      :background (face-background 'aurora-face-default)
                      :underline  (if (display-graphic-p)
                                      (face-background 'aurora-face-subtle)
                                    t)
                      :overline nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :height 0.1
                      :foreground (if (display-graphic-p)
                                      (face-background 'aurora-face-default)
                                    (face-foreground 'aurora-face-default))
                      :background (face-background 'aurora-face-default)
                      :underline (if (display-graphic-p)
                                     (face-background 'aurora-face-subtle)
                                   t)
                      :overline nil
                      :inherit nil
                      :box nil)
  
  ;;(when (display-graphic-p)
  (set-face-attribute 'header-line nil
                       :weight 'light
                       :foreground (face-foreground 'aurora-face-default)
                       :background (face-background 'aurora-face-default)

                       :overline nil
                       :underline nil
                       :box nil
                       :box `(:line-width 1
                                          :color ,(face-background 'aurora-face-default)
                                          :style nil)
                       :inherit nil)

  ;; (when (not (display-graphic-p))
  ;;   (set-face-attribute 'header-line nil
  ;;                    :weight 'light
  ;;                       :foreground (face-foreground 'aurora-face-default)
  ;;                       :background (face-background 'aurora-face-subtle)
  ;;                       :inverse-video t
  ;;                       :overline nil
  ;;                       :underline nil
  ;;                       :box nil
  ;;                            :inherit nil))

  ;; (set-face-attribute 'internal-border nil
  ;;                     :background (face-foreground 'aurora-face-default))

  (set-face-attribute 'internal-border nil
                       :background (face-background 'aurora-face-default)))


(defun aurora-theme--minibuffer ()
  "Derive minibuffer / echo area faces from aurora faces."
  ;; Minibuffer / echo area
  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'aurora-face-faded)))))


(defun aurora-theme--hl-line ()
  "Derive hl-line faces from aurora faces."
  (with-eval-after-load 'hl-line
    (set-face-attribute 'hl-line nil
                         :background aurora-color-highlight)))



(defun aurora-theme--buttons ()
  "Derive button faces from aurora faces."
  ;; Buttons
  (with-eval-after-load 'cus-edit
    (set-face-attribute 'custom-button nil
                         :foreground (face-foreground 'aurora-face-faded)
                         :background (face-background 'aurora-face-default)
                         :box `(:line-width 1
                                :color ,(face-foreground 'aurora-face-faded)
                                :style nil))
    (set-face-attribute 'custom-button-mouse nil
                         ;;                      :inherit 'custom-button
                         :foreground (face-foreground 'aurora-face-faded)
                         :background (face-background 'aurora-face-subtle)
                         :box `(:line-width 1
                                            :color ,(face-foreground 'aurora-face-faded)
                                            :style nil))
    (set-face-attribute 'custom-button-pressed nil
                         :foreground (face-background 'default)
                         :background (face-foreground 'aurora-face-salient)
                         :inherit 'aurora-face-salient
                         :box `(:line-width 1
                                            :color ,(face-foreground 'aurora-face-salient)
                                            :style nil)
                         :inverse-video nil)))


(defun aurora-theme--info ()
  "Derive info faces from aurora faces."
  (with-eval-after-load 'info
    (set-face 'info-menu-header                       'aurora-face-strong)
    (set-face 'info-header-node                      'aurora-face-default)
    (set-face 'info-index-match                      'aurora-face-salient)
    (set-face 'Info-quoted                             'aurora-face-faded)
    (set-face 'info-title-1                           'aurora-face-strong)
    (set-face 'info-title-2                           'aurora-face-strong)
    (set-face 'info-title-3                           'aurora-face-strong)
    (set-face 'info-title-4                           'aurora-face-strong)))


(defun aurora-theme--speedbar ()
  "Derive speedbar faces from aurora faces "
  (with-eval-after-load 'speedbar
    (set-face 'speedbar-button-face                    'aurora-face-faded)
    (set-face 'speedbar-directory-face                'aurora-face-strong)
    (set-face 'speedbar-file-face                    'aurora-face-default)
    (set-face 'speedbar-highlight-face             'aurora-face-highlight)
    (set-face 'speedbar-selected-face                 'aurora-face-subtle)
    (set-face 'speedbar-separator-face                 'aurora-face-faded)
    (set-face 'speedbar-tag-face                       'aurora-face-faded)))


(defun aurora-theme--bookmark ()
  "Derive bookmark faces from aurora faces."
  (with-eval-after-load 'bookmark
    (set-face 'bookmark-menu-heading                  'aurora-face-strong)
    (set-face 'bookmark-menu-bookmark                'aurora-face-salient)))


(defun aurora-theme--message ()
  "Derive message faces from aurora faces."
  (with-eval-after-load 'message
    (unless (version< emacs-version "27.0")
      (set-face 'message-cited-text-1                  'aurora-face-faded)
      (set-face 'message-cited-text-2                  'aurora-face-faded)
      (set-face 'message-cited-text-3                  'aurora-face-faded)
      (set-face 'message-cited-text-4                 'aurora-face-faded))
    (set-face 'message-cited-text                      'aurora-face-faded)
    (set-face 'message-header-cc                     'aurora-face-default)
    (set-face 'message-header-name                    'aurora-face-strong)
    (set-face 'message-header-newsgroups             'aurora-face-default)
    (set-face 'message-header-other                  'aurora-face-default)
    (set-face 'message-header-subject                'aurora-face-salient)
    (set-face 'message-header-to                     'aurora-face-salient)
    (set-face 'message-header-xheader                'aurora-face-default)
    (set-face 'message-mml                            'aurora-face-popout)
    (set-face 'message-separator                       'aurora-face-faded)))


(defun aurora-theme--outline ()
  "Derive outline faces from aurora faces."
  (with-eval-after-load 'outline
    (set-face 'outline-1                              'aurora-face-strong)
    (set-face 'outline-2                              'aurora-face-strong)
    (set-face 'outline-3                              'aurora-face-strong)
    (set-face 'outline-4                              'aurora-face-strong)
    (set-face 'outline-5                              'aurora-face-strong)
    (set-face 'outline-6                              'aurora-face-strong)
    (set-face 'outline-7                              'aurora-face-strong)
    (set-face 'outline-8                              'aurora-face-strong)))


(defun aurora-theme--customize ()
  "Derive customize faces from aurora faces."
  (with-eval-after-load 'cus-edit
    (set-face 'widget-field                           'aurora-face-subtle)
    (set-face 'widget-button                          'aurora-face-strong)
    (set-face 'widget-single-line-field               'aurora-face-subtle)
    (set-face 'custom-group-subtitle                  'aurora-face-strong)
    (set-face 'custom-group-tag                       'aurora-face-strong)
    (set-face 'custom-group-tag-1                     'aurora-face-strong)
    (set-face 'custom-comment                          'aurora-face-faded)
    (set-face 'custom-comment-tag                      'aurora-face-faded)
    (set-face 'custom-changed                        'aurora-face-salient)
    (set-face 'custom-modified                       'aurora-face-salient)
    (set-face 'custom-face-tag                        'aurora-face-strong)
    (set-face 'custom-variable-tag                    'aurora-face-strong)
    (set-face 'custom-invalid                         'aurora-face-popout)
    (set-face 'custom-visibility                     'aurora-face-salient)
    (set-face 'custom-state                          'aurora-face-salient)
    (set-face 'custom-link                           'aurora-face-salient)))

(defun aurora-theme--package ()
  "Derive package faces from aurora faces."
  (with-eval-after-load 'package
    (set-face 'package-description                   'aurora-face-default)
    (set-face 'package-help-section-name             'aurora-face-default)
    (set-face 'package-name                          'aurora-face-salient)
    (set-face 'package-status-avail-obso               'aurora-face-faded)
    (set-face 'package-status-available              'aurora-face-default)
    (set-face 'package-status-built-in               'aurora-face-salient)
    (set-face 'package-status-dependency             'aurora-face-salient)
    (set-face 'package-status-disabled                 'aurora-face-faded)
    (set-face 'package-status-external               'aurora-face-default)
    (set-face 'package-status-held                   'aurora-face-default)
    (set-face 'package-status-incompat                 'aurora-face-faded)
    (set-face 'package-status-installed              'aurora-face-salient)
    (set-face 'package-status-new                    'aurora-face-default)
    (set-face 'package-status-unsigned               'aurora-face-default))

  ;; Button face is hardcoded, we have to redefine the relevant
  ;; function
  (defun package-make-button (text &rest properties)
    "Insert button labeled TEXT with button PROPERTIES at point.
PROPERTIES are passed to `insert-text-button', for which this
function is a convenience wrapper used by `describe-package-1'."
    (let ((button-text (if (display-graphic-p)
                           text (concat "[" text "]")))
          (button-face (if (display-graphic-p)
                           `(:box `(:line-width 1
                                    :color ,aurora-color-subtle
                                    :style nil)
                                  :foreground ,aurora-color-faded
                                  :background ,aurora-color-subtle)
                         'link)))
      (apply #'insert-text-button button-text
               'face button-face 'follow-link t properties))))


(defun aurora-theme--flyspell ()
  "Derive flyspell faces from aurora faces."
  (with-eval-after-load 'flyspell
    (set-face 'flyspell-duplicate                     'aurora-face-popout)
    (set-face 'flyspell-incorrect                     'aurora-face-popout)))


(defun aurora-theme--ido ()
  "Derive ido faces from aurora faces."
  (with-eval-after-load 'ido
    (set-face 'ido-first-match                       'aurora-face-salient)
    (set-face 'ido-only-match                          'aurora-face-faded)
    (set-face 'ido-subdir                             'aurora-face-strong)))


(defun aurora-theme--diff ()
  "Derive diff faces from aurora faces."
  (with-eval-after-load 'diff-mode
    (set-face 'diff-header                             'aurora-face-faded)
    (set-face 'diff-file-header                       'aurora-face-strong)
    (set-face 'diff-context                          'aurora-face-default)
    (set-face 'diff-removed                            'aurora-face-faded)
    (set-face 'diff-changed                           'aurora-face-popout)
    (set-face 'diff-added                            'aurora-face-salient)
    (set-face 'diff-refine-added                    '(aurora-face-salient
                                                      aurora-face-strong))
    (set-face 'diff-refine-changed                    'aurora-face-popout)
    (set-face 'diff-refine-removed                    'aurora-face-faded)
    (set-face-attribute     'diff-refine-removed nil :strike-through t)))


(defun aurora-theme--term ()
  "Derive term faces from aurora faces, and material theme colors."
  (with-eval-after-load 'term
    ;; (setq eterm-256color-disable-bold nil)
    (set-face 'term-bold                                   'aurora-face-strong)
    (set-face-attribute 'term-color-black nil
                         :foreground (face-foreground 'aurora-face-default)
                         :background (face-foreground 'aurora-face-default))
    (set-face-attribute 'term-color-white nil
                         :foreground (face-background 'aurora-face-default)
                         :background (face-background 'aurora-face-default))
    (set-face-attribute 'term-color-blue nil
                         :foreground "#42A5F5"   ;; material color blue L400
                         :background "#BBDEFB")  ;; material color blue L100
    (set-face-attribute 'term-color-cyan nil
                         :foreground "#26C6DA"   ;; material color cyan L400
                         :background "#B2EBF2")  ;; material color cyan L100
    (set-face-attribute 'term-color-green nil
                         :foreground "#66BB6A"   ;; material color green L400
                         :background "#C8E6C9")  ;; material color green L100
    (set-face-attribute 'term-color-magenta nil
                         :foreground "#AB47BC"   ;; material color purple L400
                         :background "#E1BEE7")  ;; material color purple L100
    (set-face-attribute 'term-color-red nil
                         :foreground "#EF5350"   ;; material color red L400
                         :background "#FFCDD2")  ;; material color red L100
    (set-face-attribute 'term-color-yellow nil
                         :foreground "#FFEE58"    ;; material color yellow L400
                         :background "#FFF9C4"))) ;; material color yellow L100


(defun aurora-theme--calendar ()
  "Derive calendar faces from aurora faces."
  (with-eval-after-load 'calendar
    (set-face 'calendar-today                         'aurora-face-strong)))


(defun aurora-theme--agenda ()
  "Derive agenda faces from aurora faces."
  (with-eval-after-load 'org-agenda
    (set-face 'org-agenda-calendar-event             'aurora-face-default)
    (set-face 'org-agenda-calendar-sexp              'aurora-face-salient)
    (set-face 'org-agenda-clocking                     'aurora-face-faded)
    (set-face 'org-agenda-column-dateline              'aurora-face-faded)
    (set-face 'org-agenda-current-time                'aurora-face-strong)
    (set-face 'org-agenda-date                       'aurora-face-salient)
    (set-face 'org-agenda-date-today                '(aurora-face-salient
                                                       aurora-face-strong))
    (set-face 'org-agenda-date-weekend                 'aurora-face-faded)
    (set-face 'org-agenda-diary                        'aurora-face-faded)
    (set-face 'org-agenda-dimmed-todo-face             'aurora-face-faded)
    (set-face 'org-agenda-done                         'aurora-face-faded)
    (set-face 'org-agenda-filter-category              'aurora-face-faded)
    (set-face 'org-agenda-filter-effort                'aurora-face-faded)
    (set-face 'org-agenda-filter-regexp                'aurora-face-faded)
    (set-face 'org-agenda-filter-tags                  'aurora-face-faded)
    ;;  (set-face 'org-agenda-property-face                'aurora-face-faded)
    (set-face 'org-agenda-restriction-lock             'aurora-face-faded)
    (set-face 'org-agenda-structure                   'aurora-face-strong)))


(defun aurora-theme--org ()
  "Derive org faces from aurora faces."
  (with-eval-after-load 'org
    (set-face 'org-archived                            'aurora-face-faded)

    (set-face 'org-block                                       'hl-line)
    (set-face 'org-block-begin-line                    'aurora-face-faded)
    (set-face 'org-block-end-line                      'aurora-face-faded)
    (unless (version< emacs-version "27.0")
      (set-face-attribute 'org-block nil                      :extend t)
      (set-face-attribute 'org-block-begin-line nil           :extend t)
      (set-face-attribute 'org-block-end-line nil             :extend t))

    (set-face 'org-checkbox                            'aurora-face-faded)
    (set-face 'org-checkbox-statistics-done            'aurora-face-faded)
    (set-face 'org-checkbox-statistics-todo            'aurora-face-faded)
    (set-face 'org-clock-overlay                       'aurora-face-faded)
    (set-face 'org-code                                'aurora-face-faded)
    (set-face 'org-column                              'aurora-face-faded)
    (set-face 'org-column-title                        'aurora-face-faded)
    (set-face 'org-date                                'aurora-face-faded)
    (set-face 'org-date-selected                       'aurora-face-faded)
    (set-face 'org-default                             'aurora-face-faded)
    (set-face 'org-document-info                       'aurora-face-faded)
    (set-face 'org-document-info-keyword               'aurora-face-faded)
    (set-face 'org-document-title                      'aurora-face-faded)
    (set-face 'org-done                              'aurora-face-default)
    (set-face 'org-drawer                              'aurora-face-faded)
    (set-face 'org-ellipsis                            'aurora-face-faded)
    (set-face 'org-footnote                            'aurora-face-faded)
    (set-face 'org-formula                             'aurora-face-faded)
    (set-face 'org-headline-done                       'aurora-face-faded)
    ;; (set-face 'org-hide                             'aurora-face-faded)
    ;; (set-face 'org-indent                           'aurora-face-faded)
    (set-face 'org-latex-and-related                   'aurora-face-faded)
    (set-face 'org-level-1                            'aurora-face-strong)
    (set-face 'org-level-2                            'aurora-face-strong)
    (set-face 'org-level-3                            'aurora-face-strong)
    (set-face 'org-level-4                            'aurora-face-strong)
    (set-face 'org-level-5                            'aurora-face-strong)
    (set-face 'org-level-6                            'aurora-face-strong)
    (set-face 'org-level-7                            'aurora-face-strong)
    (set-face 'org-level-8                            'aurora-face-strong)
    (set-face 'org-link                              'aurora-face-salient)
    (set-face 'org-list-dt                             'aurora-face-faded)
    (set-face 'org-macro                               'aurora-face-faded)
    (set-face 'org-meta-line                           'aurora-face-faded)
    (set-face 'org-mode-line-clock                     'aurora-face-faded)
    (set-face 'org-mode-line-clock-overrun             'aurora-face-faded)
    (set-face 'org-priority                            'aurora-face-faded)
    (set-face 'org-property-value                      'aurora-face-faded)
    (set-face 'org-quote                               'aurora-face-faded)
    (set-face 'org-scheduled                           'aurora-face-faded)
    (set-face 'org-scheduled-previously                'aurora-face-faded)
    (set-face 'org-scheduled-today                   '(aurora-face-salient
+                                                      aurora-face-strong))
    (set-face 'org-sexp-date                           'aurora-face-faded)
    (set-face 'org-special-keyword                     'aurora-face-faded)
    (set-face 'org-table                               'aurora-face-faded)
    (set-face 'org-tag                                'aurora-face-popout)
    (set-face 'org-tag-group                           'aurora-face-faded)
    (set-face 'org-target                              'aurora-face-faded)
    (set-face 'org-time-grid                           'aurora-face-faded)
    (set-face 'org-todo                              'aurora-face-salient)
    (set-face 'org-upcoming-deadline                 'aurora-face-default)
    (set-face 'org-verbatim                           'aurora-face-popout)
    (set-face 'org-verse                               'aurora-face-faded)
    (set-face 'org-warning                            'aurora-face-popout)))


(defun aurora-theme--mu4e ()
  "Derive mu4e faces from aurora faces."
  (with-eval-after-load 'mu4e
    (set-face 'mu4e-attach-number-face                'aurora-face-strong)
    (set-face 'mu4e-cited-1-face                       'aurora-face-faded)
    (set-face 'mu4e-cited-2-face                       'aurora-face-faded)
    (set-face 'mu4e-cited-3-face                       'aurora-face-faded)
    (set-face 'mu4e-cited-4-face                       'aurora-face-faded)
    (set-face 'mu4e-cited-5-face                       'aurora-face-faded)
    (set-face 'mu4e-cited-6-face                       'aurora-face-faded)
    (set-face 'mu4e-cited-7-face                       'aurora-face-faded)
    (set-face 'mu4e-compose-header-face                'aurora-face-faded)
    (set-face 'mu4e-compose-separator-face             'aurora-face-faded)
    (set-face 'mu4e-contact-face                     'aurora-face-salient)
    (set-face 'mu4e-context-face                       'aurora-face-faded)
    (set-face 'mu4e-draft-face                         'aurora-face-faded)
    (set-face 'mu4e-flagged-face                      'aurora-face-popout)
    (set-face 'mu4e-footer-face                        'aurora-face-faded)
    (set-face 'mu4e-forwarded-face                   'aurora-face-default)
    (set-face 'mu4e-header-face                      'aurora-face-default)
    (set-face 'mu4e-header-highlight-face                      'hl-line)
    (set-face 'mu4e-header-key-face                   'aurora-face-strong)
    (set-face 'mu4e-header-marks-face                  'aurora-face-faded)
    (set-face 'mu4e-header-title-face                 'aurora-face-strong)
    (set-face 'mu4e-header-value-face                'aurora-face-default)
    (set-face 'mu4e-highlight-face                    'aurora-face-popout)
    (set-face 'mu4e-link-face                        'aurora-face-salient)
    (set-face 'mu4e-modeline-face                      'aurora-face-faded)
    (set-face 'mu4e-moved-face                         'aurora-face-faded)
    (set-face 'mu4e-ok-face                            'aurora-face-faded)
    (set-face 'mu4e-region-code                        'aurora-face-faded)
    (set-face 'mu4e-replied-face                     'aurora-face-default)
    (set-face 'mu4e-special-header-value-face        'aurora-face-default)
    (set-face 'mu4e-system-face                        'aurora-face-faded)
    (set-face 'mu4e-title-face                        'aurora-face-strong)
    (set-face 'mu4e-trashed-face                       'aurora-face-faded)
    (set-face 'mu4e-unread-face                       'aurora-face-strong)
    ;;(set-face-attribute 'mu4e-unread-face nil :weight 'regular)
    (set-face 'mu4e-url-number-face                    'aurora-face-faded)
    (set-face 'mu4e-view-body-face                   'aurora-face-default)
    (set-face 'mu4e-warning-face                      'aurora-face-popout)))


(defun aurora-theme--elfeed ()
  "Derive elfeed faces from aurora faces."
  (with-eval-after-load 'elfeed
    (set-face 'elfeed-log-date-face                    'aurora-face-faded)
    (set-face 'elfeed-log-info-level-face            'aurora-face-default)
    (set-face 'elfeed-log-debug-level-face           'aurora-face-default)
    (set-face 'elfeed-log-warn-level-face             'aurora-face-popout)
    (set-face 'elfeed-log-error-level-face            'aurora-face-popout)
    (set-face 'elfeed-search-tag-face                  'aurora-face-faded)
    (set-face 'elfeed-search-date-face                 'aurora-face-faded)
    (set-face 'elfeed-search-feed-face               'aurora-face-salient)
    (set-face 'elfeed-search-filter-face               'aurora-face-faded)
    (set-face 'elfeed-search-last-update-face        'aurora-face-salient)
    (set-face 'elfeed-search-title-face              'aurora-face-default)
    (set-face 'elfeed-search-tag-face                  'aurora-face-faded)
    (set-face 'elfeed-search-unread-count-face        'aurora-face-strong)
    (set-face 'elfeed-search-unread-title-face        'aurora-face-strong)))

(defun aurora-theme--deft ()
  "Derive deft faces from aurora faces."
  (with-eval-after-load 'deft
    (set-face 'deft-filter-string-error-face         'aurora-face-popout)
    (set-face 'deft-filter-string-face              'aurora-face-default)
    (set-face 'deft-header-face                     'aurora-face-salient)
    (set-face 'deft-separator-face                    'aurora-face-faded)
    (set-face 'deft-summary-face                      'aurora-face-faded)
    (set-face 'deft-time-face                       'aurora-face-salient)
    (set-face 'deft-title-face                       'aurora-face-strong)))

(defun aurora-theme--rst ()
  "Derive rst faces from aurora faces."
  (with-eval-after-load 'rst
    (set-face 'rst-adornment                           'aurora-face-faded)
    (set-face 'rst-block                             'aurora-face-default)
    (set-face 'rst-comment                             'aurora-face-faded)
    (set-face 'rst-definition                        'aurora-face-salient)
    (set-face 'rst-directive                         'aurora-face-salient)
    (set-face 'rst-emphasis1                           'aurora-face-faded)
    (set-face 'rst-emphasis2                          'aurora-face-strong)
    (set-face 'rst-external                          'aurora-face-salient)
    (set-face 'rst-level-1                            'aurora-face-strong)
    (set-face 'rst-level-2                            'aurora-face-strong)
    (set-face 'rst-level-3                            'aurora-face-strong)
    (set-face 'rst-level-4                            'aurora-face-strong)
    (set-face 'rst-level-5                            'aurora-face-strong)
    (set-face 'rst-level-6                            'aurora-face-strong)
    (set-face 'rst-literal                           'aurora-face-salient)
    (set-face 'rst-reference                         'aurora-face-salient)
    (set-face 'rst-transition                        'aurora-face-default)))


(defun aurora-theme--markdown ()
  "Derive markdown faces from aurora faces."
  (with-eval-after-load 'markdown-mode
    (set-face 'markdown-blockquote-face              'aurora-face-default)
    (set-face 'markdown-bold-face                     'aurora-face-strong)
    (set-face 'markdown-code-face                    'aurora-face-default)
    (set-face 'markdown-comment-face                   'aurora-face-faded)
    (set-face 'markdown-footnote-marker-face         'aurora-face-default)
    (set-face 'markdown-footnote-text-face           'aurora-face-default)
    (set-face 'markdown-gfm-checkbox-face            'aurora-face-default)
    (set-face 'markdown-header-delimiter-face          'aurora-face-faded)
    (set-face 'markdown-header-face                   'aurora-face-strong)
    (set-face 'markdown-header-face-1                 'aurora-face-strong)
    (set-face 'markdown-header-face-2                 'aurora-face-strong)
    (set-face 'markdown-header-face-3                 'aurora-face-strong)
    (set-face 'markdown-header-face-4                 'aurora-face-strong)
    (set-face 'markdown-header-face-5                 'aurora-face-strong)
    (set-face 'markdown-header-face-6                'aurora-face-strong)
    (set-face 'markdown-header-rule-face             'aurora-face-default)
    (set-face 'markdown-highlight-face               'aurora-face-default)
    (set-face 'markdown-hr-face                      'aurora-face-default)
    (set-face 'markdown-html-attr-name-face          'aurora-face-default)
    (set-face 'markdown-html-attr-value-face         'aurora-face-default)
    (set-face 'markdown-html-entity-face             'aurora-face-default)
    (set-face 'markdown-html-tag-delimiter-face      'aurora-face-default)
    (set-face 'markdown-html-tag-name-face           'aurora-face-default)
    (set-face 'markdown-inline-code-face              'aurora-face-popout)
    (set-face 'markdown-italic-face                    'aurora-face-faded)
    (set-face 'markdown-language-info-face           'aurora-face-default)
    (set-face 'markdown-language-keyword-face        'aurora-face-default)
    (set-face 'markdown-line-break-face              'aurora-face-default)
    (set-face 'markdown-link-face                    'aurora-face-salient)
    (set-face 'markdown-link-title-face              'aurora-face-default)
    (set-face 'markdown-list-face                      'aurora-face-faded)
    (set-face 'markdown-markup-face                    'aurora-face-faded)
    (set-face 'markdown-math-face                    'aurora-face-default)
    (set-face 'markdown-metadata-key-face              'aurora-face-faded)
    (set-face 'markdown-metadata-value-face            'aurora-face-faded)
    (set-face 'markdown-missing-link-face            'aurora-face-default)
    (set-face 'markdown-plain-url-face               'aurora-face-default)
    (set-face 'markdown-pre-face                     'aurora-face-default)
    (set-face 'markdown-reference-face               'aurora-face-salient)
    (set-face 'markdown-strike-through-face            'aurora-face-faded)
    (set-face 'markdown-table-face                   'aurora-face-default)
    (set-face 'markdown-url-face                     'aurora-face-salient)))


(defun aurora-theme--ivy ()
  "Derive ivy faces from aurora faces."
  (with-eval-after-load 'ivy
    (set-face 'ivy-action                              'aurora-face-faded)
    (set-face 'ivy-completions-annotations             'aurora-face-faded)
    (set-face 'ivy-confirm-face                        'aurora-face-faded)
    (set-face 'ivy-current-match    '(aurora-face-strong aurora-face-subtle))
    (set-face 'ivy-cursor                             'aurora-face-strong)
    (set-face 'ivy-grep-info                          'aurora-face-strong)
    (set-face 'ivy-grep-line-number                    'aurora-face-faded)
    (set-face 'ivy-highlight-face                     'aurora-face-strong)
    (set-face 'ivy-match-required-face                 'aurora-face-faded)
    (set-face 'ivy-minibuffer-match-face-1             'aurora-face-faded)
    (set-face 'ivy-minibuffer-match-face-2             'aurora-face-faded)
    (set-face 'ivy-minibuffer-match-face-3             'aurora-face-faded)
    (set-face 'ivy-minibuffer-match-face-4             'aurora-face-faded)
    (set-face 'ivy-minibuffer-match-highlight         'aurora-face-strong)
    (set-face 'ivy-modified-buffer                    'aurora-face-popout)
    (set-face 'ivy-modified-outside-buffer            'aurora-face-strong)
    (set-face 'ivy-org                                 'aurora-face-faded)
    (set-face 'ivy-prompt-match                        'aurora-face-faded)
    (set-face 'ivy-remote                            'aurora-face-default)
    (set-face 'ivy-separator                           'aurora-face-faded)
    (set-face 'ivy-subdir                              'aurora-face-faded)
    (set-face 'ivy-virtual                             'aurora-face-faded)
    (set-face 'ivy-yanked-word                         'aurora-face-faded)))

(defun aurora-theme--helm ()
  "Derive helm faces from aurora faces."
  (with-eval-after-load 'helm
    (set-face 'helm-selection                '(aurora-face-strong aurora-face-subtle))
    (set-face 'helm-match                                       'aurora-face-strong)
    (set-face 'helm-source-header                              'aurora-face-salient)
    (set-face 'helm-visible-mark                                'aurora-face-strong)))

(defun aurora-theme--helm-swoop ()
  "Derive helm faces from aurora faces."
  (with-eval-after-load 'helm-swoop
    (set-face 'helm-swoop-target-line-face   '(aurora-face-strong aurora-face-subtle))))

(defun aurora-theme--helm-occur ()
  "Derive helm faces from aurora faces."
  (with-eval-after-load 'helm-occur
    (set-face 'helm-moccur-buffer                               'aurora-face-strong)))

(defun aurora-theme--helm-ff ()
  "Derive helm faces from aurora faces."
  (with-eval-after-load 'helm-ff
    (set-face 'helm-ff-file                                      'aurora-face-faded)
    (set-face 'helm-ff-prefix                                   'aurora-face-strong)
    (set-face 'helm-ff-dotted-directory                          'aurora-face-faded)
    (set-face 'helm-ff-directory                                'aurora-face-strong)
    (set-face 'helm-ff-executable                               'aurora-face-popout)))

(defun aurora-theme--helm-grep ()
  "Derive helm faces from aurora faces."
  (with-eval-after-load 'helm-grep
    (set-face 'helm-grep-match                                  'aurora-face-strong)
    (set-face 'helm-grep-file                                    'aurora-face-faded)
    (set-face 'helm-grep-lineno                                  'aurora-face-faded)
    (set-face 'helm-grep-finish                                'aurora-face-default)))

(defun aurora-theme--company ()
  "Derive company tooltip window from aurora faces."
  (with-eval-after-load 'company
    (set-face 'company-tooltip-selection                   '(aurora-face-strong aurora-face-subtle))
    (set-face-attribute 'company-tooltip-selection nil :background aurora-color-popout)
    
    (set-face 'company-tooltip                                               'aurora-face-subtle)

    (set-face 'company-scrollbar-fg                                          'aurora-face-faded)
    (set-face-attribute 'company-scrollbar-fg nil :background aurora-color-foreground)
    
    (set-face 'company-scrollbar-bg                                          'aurora-face-default)
    (set-face-attribute 'company-scrollbar-bg nil :background aurora-color-faded)

    (set-face 'company-tooltip-common                                        'aurora-face-faded)
    (set-face 'company-tooltip-common-selection            '(aurora-face-strong aurora-face-subtle))
    (set-face-attribute 'company-tooltip-common-selection nil :background aurora-color-popout)
    
    (set-face 'company-tooltip-annotation                                    'aurora-face-default)
    (set-face 'company-tooltip-annotation-selection        '(aurora-face-strong aurora-face-subtle))))

(defun aurora-theme ()
  "Derive many, many faces from the core aurora faces."
  (aurora-theme--basics)
  (aurora-theme--font-lock)
  (aurora-theme--mode-line)
  (aurora-theme--minibuffer)
  (aurora-theme--buttons)
  (aurora-theme--info)
  (aurora-theme--bookmark)
  (aurora-theme--speedbar)
  (aurora-theme--message)
  (aurora-theme--outline)
  (aurora-theme--customize)
  (aurora-theme--package)
  (aurora-theme--flyspell)
  (aurora-theme--ido)
  (aurora-theme--diff)
  (aurora-theme--term)
  (aurora-theme--calendar)
  (aurora-theme--agenda)
  (aurora-theme--org)
  (aurora-theme--mu4e)
  (aurora-theme--elfeed)
  (aurora-theme--deft)
  (aurora-theme--rst)
  (aurora-theme--markdown)
  (aurora-theme--ivy)
  (aurora-theme--helm)
  (aurora-theme--helm-swoop)
  (aurora-theme--helm-occur)
  (aurora-theme--helm-ff)
  (aurora-theme--helm-grep)
  (aurora-theme--hl-line)
  (aurora-theme--company))

(defun aurora-refresh-theme ()
  "Convenience function which refreshes the aurora-theme.
Calls \(aurora-faces\) and \(aurora-theme\) sequentially."
  (interactive)
  (progn
    (aurora-faces)
    (aurora-theme)))


(defun aurora-toggle-theme ()
  "Function to interactively toggle between light and dark aurora themes.
Requires both to be loaded in order to work."
  (interactive)
  (cond ((string= aurora-theme-var "light")
         (aurora-theme-set-dark))
         ((string= aurora-theme-var "dark")
          (aurora-theme-set-light))
         (t nil))
  (aurora-refresh-theme))

(provide 'aurora-theme)
