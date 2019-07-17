;;; private/my-python/config.el -*- lexical-binding: t; -*-


(def-package! flycheck-pycheckers
  :after flycheck
  :config
  (setq flycheck-pycheckers-checkers '(flake8 pylint mypy3)
        flycheck-pycheckers-max-line-length 120
        flycheck-pycheckers-venv-root "/Users/jiayuan/venv/")
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))


(def-package! lsp-python-ms
  :hook (python-mode . (lambda ()
                         (progn
                           (require 'lsp-python-ms)
                           (lsp-deferred)
                           )))

  :init
  (setq lsp-python-executable-cmd "python3")
  :config
  (setq lsp-python-ms-extra-paths '("/Users/jiayuan/venv/iqiyi_qixiu/lib/python3.6/site-packages"
                                    "/Users/jiayuan/Developer/iqiyi/topic_admin/backend"
                                    "/Users/jiayuan/Developer/iqiyi/topic_admin/backend/app/api"
                                    "/Users/jiayuan/Developer/iqiyi/topic_admin/backend/app/job"
                                    "/Users/jiayuan/Developer/iqiyi/xiu_main_flask/src"
                                    "/Users/jiayuan/Developer/iqiyi/xiu_main_flask/src/app/api"
                                    "/Users/jiayuan/Developer/iqiyi/xiu_main_flask/src/app/job"
                                    "/Users/jiayuan/Developer/iqiyi/xiu_main_flask/src/app/wap"
                                    ))
  )
