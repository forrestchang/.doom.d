;;; private/my-vue/config.el -*- lexical-binding: t; -*-

(use-package! vue-mode
  :config
  (setq mmm-submode-decoration-level 0)
  )

(use-package! lsp-vue
  :hook (vue-mode . lsp)
  )
