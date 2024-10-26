;; -*- lexical-binding: t -*-
;; -------------------------------------------------------------------
;;
;; Aurora mode line format:
;;
;; [ status | name (primary)               secondary | item1 | item2 ]
;;
;; -------------------------------------------------------------------
(require 'subr-x)


;; -------------------------------------------------------------------
(defun vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (substring-no-properties vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))  nil))

(defun aurora-mode-name ()
  (if (listp mode-name) (car mode-name) mode-name))


;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; ---------------------------------------------------------------------
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;; -------------------------------------------------------------------
(defun aurora-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'mode-line))
         (window        (get-buffer-window (current-buffer)))
         (space-up       +0.15)
         (space-down     -0.20)
	 (prefix (cond ((string= status "RO")
			        (propertize (if (window-dedicated-p)" -- " " RO ")
                                'face 'aurora-face-modeline-popout))
                   ((string= status "**")
			        (propertize (if (window-dedicated-p)" -- " " ** ")
                                'face 'aurora-face-modeline-critical))
                   ((string= status "RW")
			        (propertize (if (window-dedicated-p)" -- " " RW ")
                                'face 'aurora-face-modeline-faded))
                   (t (propertize status 'face 'aurora-face-modeline-popout))))
         (left (concat
                (propertize " "  'face 'aurora-face-modeline-default
			    'display `(raise ,space-up))
                (propertize name 'face 'aurora-face-modeline-strong)
                (propertize " "  'face 'aurora-face-modeline-default
			    'display `(raise ,space-down))
		(propertize primary 'face 'aurora-face-modeline-default)))
         (right (concat secondary " "))
         (available-width (- (window-total-width) 
			     (length prefix) (length left) (length right)
			     (/ (window-right-divider-width) char-width)))
	 (available-width (max 1 available-width)))
    (concat prefix
	    left
	    (propertize (make-string available-width ?\ )
                        'face 'aurora-face-modeline-default)
	    (propertize right 'face `(:inherit aurora-face-modeline-default
                                      :foreground ,aurora-color-faded)))))

;; ---------------------------------------------------------------------
(defun aurora-modeline-aurora-help-mode-p ()
  (derived-mode-p 'aurora-help-mode))

(defun aurora-modeline-aurora-help-mode ()
  (aurora-modeline-compose (aurora-modeline-status)
                         "GNU Emacs / AURORA"
                         "(help)"
                         ""))

;; ---------------------------------------------------------------------
(defun aurora-modeline-message-mode-p ()
  (derived-mode-p 'message-mode))

(defun aurora-modeline-message-mode ()
  (aurora-modeline-compose (aurora-modeline-status)
                         "Message" "(draft)" ""))

;; ---------------------------------------------------------------------
(defun aurora-modeline-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun aurora-modeline-text-mode-p ()
  (derived-mode-p 'text-mode))

(defun aurora-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (aurora-mode-name))
          (branch      (vc-branch))
          (position    (format-mode-line "%l:%c")))
      (aurora-modeline-compose (aurora-modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                            (propertize branch 'face 'italic)))
                                     ")" )
                             position)))

;; ---------------------------------------------------------------------
(defun aurora-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)"
  
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "**") (read-only "RO") (t "RW"))))
  
;; ---------------------------------------------------------------------
(defun aurora-modeline ()
  "Install a modeline line whose content is dependent on the major mode"
  (interactive)
  (setq-default mode-line-format
  '((:eval
     (cond ((aurora-modeline-prog-mode-p)            (aurora-modeline-default-mode))
            ((aurora-modeline-message-mode-p)         (aurora-modeline-message-mode))
            ((aurora-modeline-text-mode-p)            (aurora-modeline-default-mode))
            ((aurora-modeline-aurora-help-mode-p)       (aurora-modeline-aurora-help-mode))
            (t                                      (aurora-modeline-default-mode)))))))

;; ---------------------------------------------------------------------

(setq eshell-status-in-modeline nil)
(aurora-modeline)

(provide 'aurora-modeline)
