;;; ~/.doom.d/+editor.el -*- lexical-binding: t; -*-

;; (setq doom-font
;;       (font-spec :family "Sarasa Mono SC" :size 17 :weight 'normal))

(after! doom-big-font-mode
  (setq doom-font
        (font-spec :family "Sarasa Mono SC")))

;; https://blog.csdn.net/xh_acmagic/article/details/78939246
(defun +my/better-font()
  (interactive)
  ;; english font
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font (format   "%s:pixelsize=%d" "Sarasa Mono SC" 18)) ;; 11 13 17 19 23
        ;; chinese font
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Sarasa Mono SC")))) ;; 14 16 20 22 28
    ))

(defun +my|init-font(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (+my/better-font))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'+my|init-font)
  (+my/better-font))

;; Use avy jump for all windows
(setq avy-all-windows t)

;; Terminal region color
(when (not window-system)
  (set-face-attribute 'region nil :inherit nil :background "#fff3a3"))
