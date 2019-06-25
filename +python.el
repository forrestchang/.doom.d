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
      :desc "Optimize import order"               "o" #'py-isort-buffer
      )


(setq python-shell-interpreter "/usr/local/bin/python3")

;; (setq flycheck-python-pylint-executable "/usr/local/bin/pylint"
;;       flycheck-pylintrc "~/.pylintrc"
;;       flycheck-python-mypy-executable "/usr/local/bin/mypy")

(setq anaconda-mode-localhost-address "localhost")

(setenv "WORKON_HOME" "/Users/jiayuan/venv")

;; Set indent for python
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent-offset 4)))


(setq python-shell-extra-pythonpaths '("/Users/jiayuan/Developer/iqiyi/topic_admin/backend"
                                       "/Users/jiayuan/Developer/iqiyi/topic_admin/backend/api"
                                       "/Users/jiayuan/Developer/iqiyi/topic_admin/backend/job"))
