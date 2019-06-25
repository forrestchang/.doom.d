;;; private/my-python/config.el -*- lexical-binding: t; -*-

(def-package! lsp-python-ms
  :demand
  :hook (python-mode . lsp)
  :config
  (setq lsp-python-ms-executable "~/.local/bin/Microsoft.Python.LanguageServer"
        lsp-python-ms-extra-paths '("/Users/jiayuan/Developer/iqiyi/topic_admin/backend"
                                    "/Users/jiayuan/Developer/iqiyi/topic_admin/backend/app/api"
                                    "/Users/jiayuan/Developer/iqiyi/topic_admin/backend/app/job")
        lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-imenu-enable nil
        lsp-ui-flycheck-enable t)
  )
