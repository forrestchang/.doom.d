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

;; Pyim
(after! pyim
  ;; 激活 basedict 拼音词库
  (pyim-basedict-enable)
  (setq pyim-isearch-mode 1
        pyim-default-scheme 'quanpin
        pyim-page-tooltip 'posframe
        pyim-page-length 6)

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-isearch-mode
                  pyim-probe-program-mode))

  ;; 使用其他字符来进行翻页
  (define-key map "." 'pyim-page-next-page)
  (define-key map "," 'pyim-page-previous-page)
  )

;; Fix input blinking ?
;; (setq redisplay-dont-pause nil)

;; Auto save
(auto-save-visited-mode 1)


;; Smart inference of indentation style
;; from https://www.emacswiki.org/emacs/NoTabs
(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

;; Use space instead of tabs
(setq indent-tabs-mode nil)

;; Use smart indents
;; (infer-indentation-style)

;; Set default indent to 4 space
(setq tab-width 4)

;; Indent setup
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq css-indent-offset 2)

;; Set evil escape key
(after! evil
  (setq evil-escape-key-sequence "fd"))

;; Ligatures
(mac-auto-operator-composition-mode)
