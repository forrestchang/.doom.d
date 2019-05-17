;;; ~/.doom.d/+python.el -*- lexical-binding: t; -*-

(setq python-shell-interpreter "~/venv/python3.7/bin/ipython"
      python-shell-interpreter-args "--simple-prompt -i"
      flycheck-python-pycompile-executable "~/venv/python3.7/bin/python"
      flycheck-python-pylint-executable "~/venv/python3.7/bin/pylint"
      flycheck-python-mypy-executable "~/venv/python3.7/bin/mypy")

(setq anaconda-mode-localhost-address "localhost")

(setenv "WORKON_HOME" "/Users/jiayuan/venv")
(setq python-shell-extra-pythonpaths '("/Users/jiayuan/Developer/iqiyi/xiu_xianzhi/backend"))
(add-to-list 'python-shell-extra-pythonpaths "/Users/jiayuan/Developer/iqiyi/xiu_main_flask/src")
(add-to-list 'python-shell-extra-pythonpaths "/Users/jiayuan/Developer/iqiyi/xiu_main_flask/src/app/api")
(add-to-list 'python-shell-extra-pythonpaths "/Users/jiayuan/Developer/iqiyi/xiu_main_flask/src/app/wap")
(add-to-list 'python-shell-extra-pythonpaths "/Users/jiayuan/Developer/iqiyi/xiu_main_flask/src/app/job")
(add-to-list 'python-shell-extra-pythonpaths "/Users/jiayuan/Developer/iqiyi/xiu_main_flask/src/test")

;; pylint check
(add-hook 'python-mode-hook
          (lambda ()
            (flycheck-add-next-checker 'python-pylint 'python-mypy)))

