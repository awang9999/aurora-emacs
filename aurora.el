;; Emacs initialization file
;; Author: Alexander Wang
;; Email: alexander.wang2001@gmail.com
;; Last updated: Oct 2024
;; Credits: Nicholas Rougier (NANO EMACS), Henrik Lissner (DOOM THEMES)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Set Emacs dir
(setq EMACS_DIR "~/.emacs.d/")

;; Path to Aurora emacs modules (mandatory)
(defun add-to-load-path-recursively (dir)
  "Add DIR and all its subdirectories to 'load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (normal-top-level-add-subdirs-to-load-path)))
(add-to-load-path-recursively aurora-path)

;; Aurora theme (mandatory)
(require 'aurora-fonts)
(require 'aurora-theme)

;; Aurora package settings (mandatory)
(require 'aurora-packages)

;; Aurora cache settings (optional)
(require 'aurora-cache)

;; Aurora default settings (optional)
(require 'aurora-defaults)

;; Aurora configuration (optional)
(require 'aurora-custom)

;; Welcome message (optional)
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / Aurora edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

(provide 'aurora)
