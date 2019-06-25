;;; private/my-python/config.el -*- lexical-binding: t; -*-

(def-package! lsp-python-ms
  :demand
  :hook (python-mode . lsp)
  :config
  (setq lsp-python-ms-executable
        "~/.local/bin/Microsoft.Python.LanguageServer")
  )
