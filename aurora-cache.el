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
