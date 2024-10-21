;; Emacs initialization file
;; Author: Alexander Wang
;; Email: alexander.wang2001@gmail.com
;; Last updated: Oct 2024
;; Credits: Nicholas Rougier (NANO EMACS)

;; Set Emacs dir
(setq EMACS_DIR "~/.emacs.d/")

;; Path to Aurora emacs modules (mandatory)
(add-to-list 'load-path "/home/zander/projects/aurora-emacs")
(add-to-list 'load-path ".")

;; Aurora theme
(require 'aurora-faces)
(require 'aurora-theme)
(require 'aurora-theme-dark)
(require 'aurora-theme-light)

(aurora-theme-set-light)
(call-interactively 'aurora-refresh-theme)
(call-interactively 'aurora-refresh-theme)


;; Aurora package settings (mandatory)
(require 'aurora-packages)

;; Aurora cache settings (optional)
(require 'aurora-cache)

;; Aurora default settings (optional)
(require 'aurora-defaults)

;; Aurora header & mode lines (optional)
(require 'aurora-modeline)

;; Welcome message (optional)
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / Aurora edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

(provide 'aurora)
