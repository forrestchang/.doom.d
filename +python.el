;;; ~/.doom.d/+python.el -*- lexical-binding: t; -*-

(setq python-shell-interpreter "~/venv/python3.7/bin/python"
      flycheck-python-pycompile-executable "~/venv/python3.7/bin/python"
      flycheck-python-pylint-executable "~/venv/python3.7/bin/pylint"
      flycheck-python-mypy-executable "~/venv/python3.7/bin/mypy")

(setq anaconda-mode-localhost-address "localhost")

;; pylint check
(add-hook 'python-mode-hook
          (lambda ()
            (flycheck-add-next-checker 'python-pylint 'python-mypy)))
