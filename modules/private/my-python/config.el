;;; private/my-python/config.el -*- lexical-binding: t; -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Definition
;;;
;;; - flycheck-pychecker
;;; - lsp-python-ms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! flycheck-pycheckers
  :after flycheck
  :config
  (setq flycheck-pycheckers-checkers '(flake8 pylint mypy3)
        flycheck-pycheckers-max-line-length 120
        flycheck-pycheckers-venv-root (expand-file-name "~/venv/"))
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))


(def-package! lsp-python-ms
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp-deferred)))
  :init
  (setq lsp-python-executable-cmd "python3")
  (setq lsp-python-ms-extra-paths '("/Users/jiayuan/Developer/iqiyi/topic_admin/backend"
                                    "/Users/jiayuan/Developer/iqiyi/topic_admin/backend/app/api"
                                    "/Users/jiayuan/Developer/iqiyi/topic_admin/backend/app/job"
                                    "/Users/jiayuan/Developer/iqiyi/xiu_main_flask/src"
                                    "/Users/jiayuan/Developer/iqiyi/xiu_main_flask/src/app/api"
                                    "/Users/jiayuan/Developer/iqiyi/xiu_main_flask/src/app/job"
                                    "/Users/jiayuan/Developer/iqiyi/xiu_main_flask/src/app/wap"
                                    ))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Init python setup
(defun init-python-setup ()
  ;; Indent setup
  (setq python-indent-offset 4)

  ;; Set default interpreter to python3
  (setq python-shell-interpreter "python3")

  ;; Disable readline based native completion, don't know what it is
  (setq python-shell-completion-native-enable nil)

  ;; Init MYPYPATH
  (setenv "MYPYPATH" (concat (expand-file-name "~/Developer/iqiyi/topic_admin/backend:")
                             (expand-file-name "~/Developer/iqiyi/topic_admin/backend/app/api:")
                             (expand-file-name "~/Developer/iqiyi/topic_admin/backend/app/job")))

  ;; Fix anaconda mode can't read localhost bug
  (setq anaconda-mode-localhost-address "localhost")

  ;; Set virtualenv WORKON_HOME
  (setenv "WORKON_HOME" "/Users/jiayuan/venv")

  )

(add-hook! python-mode 'init-python-setup)

;; Disable some flycheck checkers
(after! flycheck
  (setq flycheck-disabled-checkers '(python-flake8))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use format-all to format python code
;;
;; In ~format-all.el~
;;
;; (define-format-all-formatter black
;;   (:executable "~/venv/python3.7/bin/black")  ; Use spefic black executable
;;   (:install "pip install black")
;;   (:modes python-mode)
;;   (:format (format-all-buffer-easy executable "-q" "-")))
