(use-package org)

;; Improve org mode looks
(setq-default org-startup-indented t
              org-pretty-entities t
              org-use-sub-superscripts "{}"
              org-hide-emphasis-markers t
              org-startup-with-inline-images t
              org-image-actual-width '(300))

;; Modernized org symbols
(use-package org-modern
    :hook
    (org-mode . global-org-modern-mode)
    :custom
    (org-modern-keyword nil)
    (org-modern-checkbox nil)
    (org-modern-table nil))

;; Install AUCTeX
(use-package tex
  :ensure auctex)

;; LaTeX previews
;; If having problems, check this: https://emacs.stackexchange.com/questions/57898/getting-latex-preview-to-work-with-org-mode-dvi-not-found
 (use-package org-fragtog
  :after org
  :custom
  (org-startup-with-latex-preview t)
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto))
  (let ((png (cdr (assoc 'dvipng org-preview-latex-process-alist))))
    (plist-put png :latex-compiler '("latex -interaction nonstopmode -output-directory %o %F"))
    (plist-put png :image-converter '("dvipng -D %D -T tight -o %O %F"))
    (plist-put png :transparent-image-converter '("dvipng -D %D -T tight -bg Transparent -o %O %F"))))

;; Distraction-free writing
(defun ews-distraction-free ()
  "Distraction-free writing environment using Olivetti package."
  (interactive)
  (if (equal major-mode 'org-mode)
      (if (equal olivetti-mode nil)
          (progn
            (window-configuration-to-register 1)
            (delete-other-frames)
            (text-scale-set 1)
            (olivetti-mode t))
        (progn
          (if (eq (length (window-list)) 1)
              (jump-to-register 1))
          (set-register 1 nil)
          (olivetti-mode 0)
          (text-scale-set 0)))
    (message "Cannot enable distraction-free mode in non-org mode buffers")))

(use-package olivetti
  :demand t
  :bind
  (("<f9>" . ews-distraction-free)))

(provide 'aurora-org)
