;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! writeroom-mode)
(package! py-isort)
(package! protobuf-mode)
(package! py-yapf)
(package! calfw)
(package! rfc-mode)
(package! rainbow-identifiers)
(package! keyfreq)
(package! exec-path-from-shell)
(package! anki-editor)
(package! leetcode)
(package! pyim)
(package! ace-pinyin)
(package! org-analyzer)
(package! evil-pinyin)
(package! cnfonts)
(package! org-roam-server)
(package! org-download)
(package! wakatime-mode)

;; beancount
(package! beancount
  :recipe (:host github :repo "cnsunyour/beancount.el"))

;; org-mode
(package! org-pomodoro)
(package! org-gcal)
(package! org-super-agenda)
(package! ox-hugo)
(package! org-journal)
(package! org-attach-screenshot)
(package! org-mru-clock)
(package! org-ql)

(unpin! org-roam company-org-roam company-go)
