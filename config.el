;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; References
;; - https://tecosaur.github.io/emacs-config/config.html


;;;; Better defaults

;; Simple settings

(setq user-full-name "tisoga"
      user-mail-address "forrestchang7@gmail.com")

(setq-default delete-by-moving-to-trash t
              window-combination-resize t
              x-stretch-cursor t)

(setq undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      truncate-string-ellipsis "..."
      password-cache-expiry nil
      scroll-margin 2)

(display-time-mode 1)

;; Better key bindings

(map! :leader

      :desc "M-x" "SPC"           #'execute-extended-command
      :desc "Buffer Switch" "TAB" #'evil-switch-to-windows-last-buffer

      (:desc "Code" :prefix "c"
       :desc "Comment or uncomment lines" :gnv "l" #'evilnc-comment-or-uncomment-lines)

      (:desc "Git" :prefix "g"
       :desc "Magit status" "s" #'magit-status))

(map!
 (:when (featurep! :tools lookup)
  :nv "gb" #'better-jumper-jump-backward
  :nv "gf" #'better-jumper-jump-forward))

;; Some evil stuff
(use-package! evil-little-word
  :config
  (map! :nov "w" #'evil-forward-little-word-begin
        :nov "b" #'evil-backward-little-word-begin
        :v "i w" #'evil-inner-little-word))

;; Frame sizing

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(weight . 80))

;; Auto-customisations

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Windows

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)

(map! :map evil-window-map
      "SPC"       #'rotate-layout
      ;; Split
      "/"         #'split-window-right
      "-"         #'split-window-below
      ;; Navigation
      "<left>"    #'evil-window-left
      "<down>"    #'evil-window-down
      "<up>"      #'evil-window-up
      "<right>"   #'evil-window-right
      ;; Swapping windows
      "C-<left>"  #'+evil/window-move-left
      "C-<down>"  #'+evil/window-move-down
      "C-<up>"    #'+evil/window-move-up
      "C-<right>" #'+evil/window-move-right
      )

;; Buffer defaults

(setq-default major-mode 'org-mode)

;;;; Doom configuration

;; Font

(setq doom-font (font-spec :family "JetBrains Mono" :size 20)
      doom-big-font (font-spec :family "JetBrains Mono" :size 36))

;; Theme and modeline

(setq doom-theme 'doom-plain)
(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
(delq! t custom-theme-load-path)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq doom-modeline-icon nil)

;; Misc

(setq display-line-numbers-type 'relative)

;; Company

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; Evil

(after! evil
  (setq evil-kill-on-visual-paste nil))

;; Info colours

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

;; Which key
(setq which-key-idle-delay 0.5)

;;;; Language configuration

;; Plaintext

(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max)))))
