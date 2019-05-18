;;; ~/.doom.d/+editor.el -*- lexical-binding: t; -*-

(setq doom-font
      (font-spec :family "Fira Code Retina" :size 17))

;; Use avy jump for all windows
(setq avy-all-windows t)

;; Terminal region color
(when (not window-system)
  (set-face-attribute 'region nil :inherit nil :background "#fff3a3"))
