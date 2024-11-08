;; Language major modes

;; Markdown
(use-package markdown-mode
  :ensure t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

;; Web mode
(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))


;; Kotlin
(use-package kotlin-mode
  :ensure t
  :mode
  (("\\.kt\\'" . kotlin-mode)))

;; LSP
(eglot-ensure)
(use-package eglot
  :ensure t
  :defer t
  :hook ((java-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (kotlin-mode . eglot-ensure)))

;; Tree-sitter
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'aurora-languages)
