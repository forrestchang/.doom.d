;;; private/my-python/config.el -*- lexical-binding: t; -*-

(def-package! lsp-python-ms
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp-deferred)))
  :config
  (setq lsp-python-ms-extra-paths '("/Users/jiayuan/Developer/iqiyi/topic_admin/backend"
                                    "/Users/jiayuan/Developer/iqiyi/topic_admin/backend/app/api"
                                    "/Users/jiayuan/Developer/iqiyi/topic_admin/backend/app/job"))
  )
