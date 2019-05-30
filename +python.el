;;; ~/.doom.d/+python.el -*- lexical-binding: t; -*-

;; Formating hack
;; Use format-all to format python code
;;
;; In ~format-all.el~
;;
;; (define-format-all-formatter black
;;   (:executable "~/venv/python3.7/bin/black")  ; Use spefic black executable
;;   (:install "pip install black")
;;   (:modes python-mode)
;;   (:format (format-all-buffer-easy executable "-q" "-")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key bindings
;;;
;;; SPC m i    Optimize import order
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map! :map python-mode-map
      :localleader
      :desc "Optimize import order"               "i" #'py-isort-buffer
      )


;; (setq python-shell-interpreter "~/venv/python3.7/bin/ipython"
;;       python-shell-interpreter-args "--simple-prompt -i")

(setq flycheck-python-pycompile-executable "~/venv/python3.7/bin/python"
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

;; Set indent for python
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent-offset 4)))
