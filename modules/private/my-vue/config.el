;;; private/my-vue/config.el -*- lexical-binding: t; -*-

(def-package! vue-mode
  :config
  (setq mmm-submode-decoration-level 0)
  )

(def-package! lsp-vue
  :hook (vue-mode . lsp)
  )
