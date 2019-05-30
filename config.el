;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(load! "+ui")
(load! "+bindings")
(load! "+editor")


(after! python (load! "+python"))
(after! org (load! "+org"))

(setq user-full-name "Jiayuan Zhang"
      user-mail-address "forrestchang7@gmail.com")

(setq confirm-kill-emacs nil)

;; Wakatime
(setq +wakatime-hid-filenames t)

;; Auto-generate custom variable definitions.
(setq custom-file (expand-file-name "custom.el" "~/.doom.d/"))
(load custom-file 'no-error 'no-message)

(after! text-mode
  (set-company-backend! 'text-mode 'company-yasnippet))

(after! org
  (set-company-backend! 'org-mode 'company-yasnippet))

;; Swiper search
(defun eh-ivy-cregexp (str)
    (concat
     (ivy--regex-plus str)
     "\\|"
     (pyim-cregexp-build str)))

  (setq ivy-re-builders-alist
        '((t . eh-ivy-cregexp)))

;; calfw
(require 'calfw)
;; Beancount
(def-package! beancount
  :load-path "~/.doom.d/lib/"
  :config
  (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
  )
;; Applescript mode
(def-package! applescript-mode)
