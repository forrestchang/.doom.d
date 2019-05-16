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

;; Pyim
(setq company-idle-delay 0)
(after! pyim
  (defun eh-company-dabbrev--prefix (orig-fun)
    "取消中文补全"
    (let ((string (pyim-char-before-to-string 0)))
      (if (pyim-string-match-p "\\cc" string)
          nil
        (funcall orig-fun))))
  (advice-add 'company-dabbrev--prefix :around #'eh-company-dabbrev--prefix)
  )

;; cnfonts
(require 'cnfonts)
(cnfonts-enable)
(cnfonts-set-spacemacs-fallback-fonts)
