;; Save miscellaneous history
(setq savehist-additional-variables
      '(kill-ring
        command-history
	    set-variable-value-history
	    custom-variable-history
	    query-replace-history
	    read-expression-history
	    minibuffer-history
	    read-char-history
	    face-name-history
	    bookmark-history
        ivy-history
	    counsel-M-x-history
	    file-name-history
        counsel-minibuffer-history))
(setq history-length 250)
(setq kill-ring-max 25)
(put 'minibuffer-history         'history-length 50)
(put 'file-name-history          'history-length 50)
(put 'set-variable-value-history 'history-length 25)
(put 'custom-variable-history    'history-length 25)
(put 'query-replace-history      'history-length 25)
(put 'read-expression-history    'history-length 25)
(put 'read-char-history          'history-length 25)
(put 'face-name-history          'history-length 25)
(put 'bookmark-history           'history-length 25)
(put 'ivy-history                'history-length 25)
(put 'counsel-M-x-history        'history-length 25)
(put 'counsel-minibuffer-history 'history-length 25)
(setq savehist-file (concat EMACS_DIR "savehist"))
(savehist-mode 1)

;; Move all the backup files to specific cache directory
;; This way you won't have annoying temporary files starting with ~(tilde) in each directory
;; Following setting will move temporary files to specific folders inside cache directory in EMACS_DIR
;; Credit: https://github.com/neppramod/java_emacs/blob/master/emacs-configuration.org
(setq user-cache-directory (concat EMACS_DIR "cache"))
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-cache-directory)))
    url-history-file (expand-file-name "url/history" user-cache-directory)
    auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-cache-directory)
    projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-cache-directory)
    )

(provide 'aurora-cache)
