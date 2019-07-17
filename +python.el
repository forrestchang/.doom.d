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
      :desc "Sort import"               "o" #'py-isort-buffer
      :desc "Import missing package"    "i" #'pyimport-insert-missing
      )

(setq indent-tabs-mode nil
      tab-width 4)

;; Setup default python executable
(setq python-shell-interpreter "python3")

;; Setup PYTHONPATH
(add-hook! python-mode
  (setenv "PYTHONPATH" (concat "/Users/jiayuan/Developer/iqiyi/topic_admin/backend:"
                               "/Users/jiayuan/Developer/iqiyi/topic_admin/backend/app/api:"
                               "/Users/jiayuan/Developer/iqiyi/topic_admin/backend/app/job:"
                               (getenv "PYTHONPATH")
                               )))

(add-hook! python-mode
  (setenv "MYPYPATH" (concat "/Users/jiayuan/Developer/iqiyi/topic_admin/backend:"
                             "/Users/jiayuan/Developer/iqiyi/topic_admin/backend/app/api:"
                             "/Users/jiayuan/Developer/iqiyi/topic_admin/backend/app/job")))

(setq anaconda-mode-localhost-address "localhost")

(setenv "WORKON_HOME" "/Users/jiayuan/venv")

(defun lsp-python-flycheck-setup ()
  (run-with-timer "5sec" nil (lambda ()
                               (message "!!! Set up flycheck for lsp-python")
                               (after! lsp-ui
                                 (setq flycheck-disabled-checkers '(lsp-ui python-pylint python-mypy
                                                                           python-pycompile python-flake8))
                                 )
                               (setq-default flycheck-checker 'python-pycheckers)))
  )

(add-hook 'python-mode-hook 'lsp-python-flycheck-setup)
