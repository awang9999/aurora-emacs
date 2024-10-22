(require 'aurora-base-colors)
(require 'aurora-fonts)

;; A theme is fully defined by these seven faces

(defface aurora-face-default nil
  "Default face is used for regular information."
  :group 'aurora)

(defface aurora-face-variable-pitch nil
  "Default variable-pitch face is used for variable pitch mode."
  :group 'aurora)

(defface aurora-face-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group 'aurora)

(defface aurora-face-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group 'aurora)

(defface aurora-face-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group 'aurora)

(defface aurora-face-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group 'aurora)

(defface aurora-face-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group 'aurora)

(defface aurora-face-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group 'aurora)

(defface aurora-face-highlight nil
  "Highlight face is used for the marked region and highlighted
text such as links. It can be made very light but still perceptible"
  :group 'aurora)

(defface aurora-face-modeline-default nil
  "Default face for ther modeline line."
  :group 'aurora)

(defface aurora-face-modeline-critical nil
  "Critical face for ther modeline line."
  :group 'aurora)

(defface aurora-face-modeline-popout nil
  "Popout face for ther modeline line."
  :group 'aurora)

(defface aurora-face-modeline-strong nil
  "Strong face for ther modeline line."
  :group 'aurora)

(defface aurora-face-modeline-salient nil
  "Salient face for ther modeline line."
  :group 'aurora)

(defface aurora-face-modeline-faded nil
  "Faded face for ther modeline line."
  :group 'aurora)

(defface aurora-face-modeline-subtle nil
  "Subtle face for ther modeline line."
  :group 'aurora)

(defface aurora-face-modeline-highlight nil
  "Highlight face for ther modeline line."
  :group 'aurora)

(defface aurora-face-modeline-separator nil
  "Face for separating item in the modeline line (internal use)"
  :group 'aurora)

(defface aurora-face-modeline-filler nil
  "Face compsenting spaces in the modeline line (internal use) "
  :group 'aurora)

(defface aurora-face-tag-default nil
  "Default face for tags"
  :group 'aurora)

(defface aurora-face-tag-faded nil
  "Faded face for tags"
  :group 'aurora)

(defface aurora-face-tag-strong nil
  "Strong face for tags"
  :group 'aurora)

(defface aurora-face-tag-salient nil
  "Salient face for tags"
  :group 'aurora)

(defface aurora-face-tag-popout nil
  "Popout face for tags"
  :group 'aurora)

(defface aurora-face-tag-critical nil
  "Critical face for tags"
  :group 'aurora)

(defun aurora-what-faces (pos)
  "Get the font faces at POS."
  (interactive "d")
  (let ((faces (remq nil
                     (list
                      (get-char-property pos 'read-face-name)
                      (get-char-property pos 'face)
                      (plist-get (text-properties-at pos) 'face)))))
    (message "Faces: %s" faces)))

(defun aurora-faces ()
  "Derive face attributes for aurora-faces using aurora-theme values."
  (set-face-attribute 'aurora-face-default nil
                      :foreground aurora-color-foreground
                      :background aurora-color-background
                      :family     aurora-font-family-monospaced
                      :height       (* aurora-font-size 10))
  (set-face-attribute 'aurora-face-critical nil
                      :foreground aurora-color-foreground
                      :background aurora-color-critical)
  (set-face-attribute 'aurora-face-popout nil
                      :foreground aurora-color-popout)

  (set-face-attribute 'aurora-face-variable-pitch nil
                          :foreground (face-foreground 'aurora-face-default)
                          :background (face-background 'aurora-face-default)
                          :family aurora-font-family-proportional
                          :height (* aurora-font-size 10))
  (if (display-graphic-p)
      (set-face-attribute 'aurora-face-strong nil
                          :foreground aurora-color-strong
                          :weight 'medium)
    (set-face-attribute 'aurora-face-strong nil
                        :foreground aurora-color-strong
                        :weight 'bold))

  (set-face-attribute 'aurora-face-salient nil
                      :foreground aurora-color-salient
                      :weight 'light)

  (set-face-attribute 'aurora-face-faded nil
                      :foreground aurora-color-faded
                      :weight 'light)

  (set-face-attribute 'aurora-face-subtle nil
                      :background aurora-color-subtle)
  
  (set-face-attribute 'aurora-face-highlight nil
                      :background aurora-color-highlight)
  
  (set-face-attribute 'aurora-face-modeline-default nil
                      :foreground aurora-color-foreground
                      :background aurora-color-subtle
                      :box `(:line-width 1
                                         :color ,aurora-color-background
                                         :style nil))

  (set-face-attribute 'aurora-face-tag-default nil
                      :foreground aurora-color-foreground
                      :background aurora-color-background
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 aurora-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,aurora-color-foreground
                                         :style nil))

  (set-face-attribute 'aurora-face-modeline-strong nil
                      :foreground aurora-color-strong
                      :background aurora-color-subtle
                      :inherit 'aurora-face-strong
                      :box `(:line-width 1
                                         :color ,aurora-color-background
                                         :style nil))

  (set-face-attribute 'aurora-face-tag-strong nil
                      :foreground aurora-color-strong
                      :background aurora-color-subtle
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 aurora-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,aurora-color-strong
                                         :style nil))

  (set-face-attribute 'aurora-face-modeline-salient nil
                      :foreground aurora-color-background
                      :background aurora-color-salient
                      :box `(:line-width 1
                                         :color ,aurora-color-background
                                         :style nil))

  (set-face-attribute 'aurora-face-tag-salient nil
                      :foreground aurora-color-background
                      :background aurora-color-salient
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 aurora-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,aurora-color-salient
                                         :style nil))

  (set-face-attribute 'aurora-face-modeline-popout nil
                      :foreground aurora-color-background
                      :background aurora-color-popout
                      :box `(:line-width 1
                                         :color ,aurora-color-background
                                         :style nil))

  (set-face-attribute 'aurora-face-tag-popout nil
                      :foreground aurora-color-background
                      :background aurora-color-popout
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 aurora-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,aurora-color-popout
                                         :style nil))

  (set-face-attribute 'aurora-face-modeline-faded nil
                      :foreground aurora-color-background
                      :background aurora-color-faded
                      :box `(:line-width 1
                                         :color ,aurora-color-background
                                         :style nil))

  (set-face-attribute 'aurora-face-tag-faded nil
                      :foreground aurora-color-background
                      :background aurora-color-faded
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 aurora-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,aurora-color-faded
                                         :style nil))

  (set-face-attribute 'aurora-face-modeline-subtle nil)

  (set-face-attribute 'aurora-face-modeline-critical nil
                      :foreground aurora-color-background
                      :background aurora-color-critical
                      :box `(:line-width 1
                                         :color ,aurora-color-background
                                         :style nil))
  (set-face-attribute 'aurora-face-tag-critical nil
                      :foreground aurora-color-background
                      :background aurora-color-critical
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 aurora-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,aurora-color-critical
                                         :style nil))

  (set-face-attribute 'aurora-face-modeline-separator nil
                      :inherit 'aurora-face-default
                      :height 0.1)
  (set-face-attribute 'aurora-face-modeline-filler nil
                      :inherit 'aurora-face-modeline-default
                      :height 0.1)
  (set-face-attribute 'aurora-face-modeline-highlight nil
                      :inherit 'aurora-face-modeline-faded
                      :box nil))

(provide 'aurora-faces)
