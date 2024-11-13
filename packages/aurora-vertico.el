(use-package vertico
  :init
  (vertico-mode))

(defun posframe-poshandler-frame-top-center-offset (info)
  "Aurora Custom Posframe position handler.

This poshandler function let top edge center of posframe align
to top edge center of frame and offset down by 48.

The structure of INFO can be found in docstring of
`posframe-show'."
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        48))

(use-package vertico-posframe
  :init
  (vertico-posframe-mode 1)
  :config
  (setq vertico-posframe-parameters
      '((left-fringe . 4)
        (right-fringe . 4)))
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-top-center-offset))

(provide 'aurora-vertico)
