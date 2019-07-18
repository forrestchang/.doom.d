;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Load system environment variables
(when (memq window-system '(mac ns x))
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "PYTHONPATH" "MANPATH" "GOPATH" "MYPYPATH" "RUST_SRC_PATH"))
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)
  )
(after! racer
  (setq racer-rust-src-path "/Users/jiayuan/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
  )

(load! "+ui")
(load! "+bindings")
(load! "+editor")

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
  (set-company-backend! 'org-mode 'company-yasnippet)
  )

;; Company
(after! company
  (setq company-idle-delay 0
        company-box-doc-enable t
        company-box-enable-icon nil))

;; Swiper search
(defun eh-ivy-cregexp (str)
  (concat
   (ivy--regex-plus str)
   "\\|"
   (pyim-cregexp-build str)))

(setq ivy-re-builders-alist
      '((t . eh-ivy-cregexp)))

;; calfw
(def-package! calfw)

;; Beancount
(def-package! beancount
  :load-path "~/.doom.d/lib/"
  :config
  (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
  )

;; Web mode
(setq flycheck-javascript-eslint-executable "/usr/local/bin/eslint")

(add-hook 'web-mode-hook
          (lambda ()
            (flycheck-add-next-checker 'javascript-eslint)))


;; Applescript mode
(def-package! applescript-mode)

;; Rainbow identifiers
(def-package! rainbow-identifiers
  :config (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

;; Key frequences
(def-package! keyfreq
  :config (progn (keyfreq-mode 1)
                 (keyfreq-autosave-mode 1)))


;; Hack for ox-hugo
(after! ox-hugo
  (defadvice org-hugo-paragraph (before org-hugo-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to hugo markdown."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents)))
  )

;; git-gutter
(after! git-gutter
  (setq git-gutter:disabled-modes '(org-mode image-mode)))

;; Deft
(after! deft
  (setq deft-directory "~/Dropbox/org/notes")
  )

;; Rust
(after! flycheck
  (setq flycheck-rust-cargo-executable "~/.cargo/bin/cargo")
  )

;; Turn off line numbers in org-mode
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Alternate-buffer
(defun jiayuan/alternate-buffer ()
  (interactive)
  (if (evil-alternate-buffer)
      (switch-to-buffer (car (evil-alternate-buffer)))
    (switch-to-buffer (other-buffer (current-buffer) t))))

;; Instant Rename Web-mode tag
(def-package! instant-rename-tag
  :load-path "~/.doom.d/lib/")

;; lsp-ui
(after! lsp-ui
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-use-webkit nil
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-border "#fdf5b1"
        lsp-ui-doc-max-width 65
        lsp-ui-doc-max-height 70
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-peek-enable t
        lsp-ui-flycheck-enable -1)

  (add-to-list 'lsp-ui-doc-frame-parameters '(left-fringe . 0))

  ;; `C-g' to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

(after! flycheck
  (defun my-python-flycheck-setup ()
    (setq-default flycheck-disabled-checkers '(
                                               python-pylint python-mypy
                                               python-flake8 python-pycompile
                                               ))
    (setq-default flycheck-checker 'python-pycheckers))

  (add-hook 'python-mode-hook 'my-python-flycheck-setup)
  )

;; company-lsp
(after! company-lsp
  (setq company-lsp-cache-candidates 'auto))


(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))
(global-set-key (kbd "C-c +") 'my-increment-number-decimal)


;; Anki
(def-package! anki-editor)

;; Copy file path to clipboard
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))


;; Smart align
(def-package! smart-align)
