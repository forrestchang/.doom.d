;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(load! "+bindings")
(load! "+editor")

;; Set org directory before loading org
(setq org-directory "~/org/")

(after! org (load! "+org"))

(setq user-full-name "Jiayuan Zhang"
      user-mail-address "forrestchang7@gmail.com")

(setq confirm-kill-emacs nil)

;; Theme
(setq doom-theme 'doom-one)

(setq evil-normal-state-cursor '(box "#FF6F65")
      evil-insert-state-cursor '(bar "#FF6F65")
      evil-visual-state-cursor '(hollow "#FF6F65"))

;; Auto-generate custom variable definitions.
(setq custom-file (expand-file-name "custom.el" "~/.doom.d/"))
(load custom-file 'no-error 'no-message)

;; (after! text-mode
;;   (set-company-backend! 'text-mode 'company-yasnippet))

(after! org
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet))
  )

;; Company
(after! company
  (setq company-idle-delay 0
        company-box-doc-enable nil
        company-box-enable-icon nil))

;; Swiper search
;; (defun eh-ivy-cregexp (str)
;;   (concat
;;    (ivy--regex-plus str)
;;    "\\|"
;;    (pyim-cregexp-build-1 str)))
(defun eh-ivy-cregexp (str)
  (let ((x (ivy--regex-plus str))
        (case-fold-search nil))
    (if (listp x)
        (mapcar (lambda (y)
                  (if (cdr y)
                      (list (if (equal (car y) "")
                                ""
                              (pyim-cregexp-build-1 (car y)))
                            (cdr y))
                    (list (pyim-cregexp-build-1 (car y)))))
                x)
      (pyim-cregexp-build-1 x))))
(setq ivy-re-builders-alist
      '((t . eh-ivy-cregexp)))



;; calfw
(use-package! calfw)

;; Web mode
(setq flycheck-javascript-eslint-executable "/usr/local/bin/eslint")

(add-hook 'web-mode-hook
          (lambda ()
            (flycheck-add-next-checker 'javascript-eslint)))

;; Rainbow identifiers
(use-package! rainbow-identifiers
  :config (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

;; Key frequences
(use-package! keyfreq
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
  (setq deft-directory "~/org/roam/")
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

;; lsp-ui
(after! lsp-ui
  (setq lsp-ui-doc-enable nil
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
(use-package! anki-editor)

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


;; Magit assum unchanged
(after! magit
  (defun magit-skip-assume-unchanged-files ()
    (--keep (and (and (= (aref it 0) ?h)
                      (substring it 2)))
            (magit-git-items "ls-files"
                             (string-trim
                              (car (magit-git-items "rev-parse" "--show-toplevel")))
                             "-v" "--full-name" "-z")))

  (defun magit-insert-assume-unchanged-files ()
    "Insert a tree of assume unchanged files.

    If the first element of `magit-buffer-diff-files' is a
    directory, then limit the list to files below that.  The value
    of that variable can be set using \"D -- DIRECTORY RET g\"."
    (when-let ((files (magit-skip-assume-unchanged-files)))
      (let* ((base (car magit-buffer-diff-files))
             (base (and base (file-directory-p base) base)))
        (magit-insert-section (assume-unchanged nil t)
          (magit-insert-heading "Assume-unchanged files:")
          (magit-insert-files files base)
          (insert ?\n)))))

  (defun magit-list-all-files ()
    (magit-with-toplevel
      (with-temp-buffer
        (apply #'magit-git-insert '("ls-files" "-z" "--full-name"))
        (split-string (buffer-string) "\0" t))))

  (defun magit-assume-unchanged (file)
    "Call \"git update-index --assume-unchanged FILE\"."
    (interactive (list (magit-read-file-choice "Assume unchanged for: "
                                               (cl-set-difference
                                                (magit-list-all-files)
                                                (magit-skip-assume-unchanged-files)))))
    (magit-with-toplevel
      (magit-run-git "update-index" "--assume-unchanged" "--" file)))

  (defun magit-no-assume-unchanged (file)
    "Call \"git update-index --no-assume-unchanged FILE\"."
    (interactive (list (magit-read-file-choice "Do not assume unchanged for: "
                                               (magit-skip-assume-unchanged-files))))
    (magit-with-toplevel
      (magit-run-git "update-index" "--no-assume-unchanged" "--" file)))

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-assume-unchanged-files nil t)
  ;; define jump
  (magit-define-section-jumper magit-jump-to-assume-unchanged "Assume-unchanged files" assume-unchanged)
  (define-key magit-status-mode-map "ga" 'magit-jump-to-assume-unchanged)
  )


;; Leetcode
(use-package! leetcode
  :init
  (setq leetcode-prefer-language "python3"
        leetcode-prefer-sql "mysql")
  )

;; Smart align
;; (use-package! smart-align
;;   :load-path "~/.doom.d/site-lisp/smart-align"
;;   )

;; (use-package! snails
;;   :load-path "~/.doom.d/site-lisp/snails"
;;   )

;; Beancount
;; (use-package! beancount
;;   :load-path "~/.doom.d/site-lisp/beancount"
;;   )

;; Reload file from disk without confirm
(defun revert-buffer-no-confirm ()
  "Revert buffer without confimation"
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;; Devonthink support
(org-add-link-type "x-devonthink-item" 'org-devonthink-open)

(defun org-devonthink-open (link)
  "Open the devonthink link"
  (start-process (concat "open " link) nil "open"
                 (concat "x-devonthink-item:" link))
  )

;; Bear link support
(org-add-link-type "bear" 'org-bear-open)

(defun org-bear-open (link)
  "Open the bear link"
  (start-process (concat "open " link) nil "open"
                 (concat "bear:" link)))

(use-package! org-analyzer)

(use-package! evil-pinyin
  :init
  (setq evil-pinyin-with-search-rule 'always)
  :config
  (global-evil-pinyin-mode))

(use-package! lsp-pyright
  :config
  (setq lsp-pyright-python-executable-cmd "~/.pyenv/versions/3.8.5/envs/default-3.8.5/bin/python"))


;; Current time and date
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%H:%M"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert (format-time-string current-date-time-format (current-time)))
       )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       )

(global-set-key (kbd "C-c d") 'insert-current-date-time)
(global-set-key (kbd "C-c t") 'insert-current-time)
