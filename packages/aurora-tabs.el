(use-package centaur-tabs
  :config
  (setq centaur-tabs-style "slant"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-show-new-tab-button t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-bar 'under
        centaur-tabs-show-count nil
        ;; centaur-tabs-label-fixed-length 15
        ;; centaur-tabs-gray-out-icons 'buffer
        ;; centaur-tabs-plain-icons t
        x-underline-at-descent-line t
        centaur-tabs-left-edge-margin nil)
  (centaur-tabs-change-fonts (face-attribute 'default :family) aurora-font-size)
  (centaur-tabs-headline-match)
  ;; (centaur-tabs-enable-buffer-alphabetical-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (magit-mode . centaur-tabs-local-mode)
  :bind
  ("C-S-<iso-lefttab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  ("C-x t f" . centaur-tabs-move-current-tab-to-left)
  ("C-x t n" . centaur-tabs-move-current-tab-to-right))

;; display tab-bar-mode and unbinding "C-x t" to start from scratch
(tab-bar-mode -1)
(global-unset-key (kbd "C-x t"))

;; Centaur Tab Keybindings
(define-prefix-command 'my-centaur-tabs-map)
(global-set-key (kbd "C-x t") 'my-centaur-tabs-map)
(define-key my-centaur-tabs-map (kbd "2") 'centaur-tabs--create-new-tab)
(define-key my-centaur-tabs-map (kbd "0") 'centaur-tabs--kill-this-buffer-dont-ask)
(define-key my-centaur-tabs-map (kbd "n") 'centaur-tabs-forward)
(define-key my-centaur-tabs-map (kbd "p") 'centaur-tabs-backward)

;; Enable centaur tab mode
(centaur-tabs-mode t)

(provide 'aurora-tabs)
