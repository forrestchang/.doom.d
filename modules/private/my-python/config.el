;;; private/my-python/config.el -*- lexical-binding: t; -*-

(def-package! lsp-python-ms
  :hook (python-mode . lsp)
  :config
  (setq lsp-python-ms-dir
        (expand-file-name "~/Github/python-language-server/output/bin/Release/"))
  )
