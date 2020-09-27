;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;; Unbind keys
(map! :leader
      "A" nil
      "X" nil
      )

;; Unbind functions
(map! [remap swiper] #'swiper-isearch)
(map! [remap org-capture] nil)

;; Leader key
(map! :leader
      :desc "M-x"                                    "SPC" #'execute-extended-command
      :desc "Find file in project"                     "." #'snails
      :desc "Org agenda"                               "a" #'org-agenda
      :desc "Org agenda today"                         "A" (lambda () (interactive) (org-agenda nil "a"))
      :desc "Capture Inbox"                            "I" (lambda () (interactive) (org-capture nil "t"))
      :desc "Org Journal"                              "J" (lambda () (interactive) (org-capture nil "j"))
      :desc "Org clock goto"                           "G" #'org-clock-goto
      :desc "Org capture"                              "C" #'org-capture
      :desc "Expand region"                            "<" #'er/expand-region
      :desc "Toggle popup window"                      "`" #'+popup/toggle
      :desc "Reload buffer from disc"                  "R" #'revert-buffer-no-confirm
      :desc "Switch last buffer"                       "TAB" #'jiayuan/alternate-buffer
      :desc "Custom org-ql: Most Important Tasks"      "1" #'custom-ql-most-important-thins
      :desc "Custom org-ql: Work Related Tasks"        "2" #'custom-ql-work-related-tasks
      :desc "Custom org-ql: Non-Work Related Tasks"    "3" #'custom-ql-non-work-related-tasks
      :desc "Custom org-ql: Today Log"                 "4" #'custom-ql-today-log
      :desc "Custom org-ql: Weekly Review"             "5" #'custom-ql-weekly-review
      :desc "Jump to today's daily note"               "T" #'org-roam-dailies-today
      :desc "Jump to yesterday's daily note"           "Y" #'org-roam-dailies-yesterday
      )

(map!
 (:when (featurep! :tools lookup)
  :nv "gb" #'better-jumper-jump-backward
  :nv "gf" #'better-jumper-jump-forward
  :nv "B"  #'jiayuan/alternate-buffer
  )
 )

;; Mode specific key map

;; web-mode
(map! (:map web-mode-map
       :localleader
       :desc "Instant rename tag"                        "r" #'instant-rename-tag
       ))

;; Prefix map
(map! :leader
      (:prefix ("b" . "buffer")
       :desc "Previous buffer"           "TAB" #'previous-buffer
       :desc "Switch buffer"               "b" #'switch-to-buffer
       :desc "Delete buffer"               "d" #'kill-this-buffer
       :desc "Recent files"                "r" #'recentf-open-files
       :desc "Swith to home"               "h" #'+doom-dashboard/open
       )

      (:prefix ("f" . "file")
       :desc "Save all files"              "S" #'evil-write-all
       :desc "Dired jump"                  "j" #'dired-jump
       )

      (:prefix ("w" . "window")
       :desc "Delete window or workspace"  "d" #'delete-window
       :desc "Maximize window"             "m" #'doom/window-maximize-buffer
       :desc "Other window"                "w" #'other-window
       :desc "Split window right"          "|" #'split-window-right
       :desc "Split window below"          "-" #'split-window-below
       :desc "Move window up"              "K" #'evil-window-up
       :desc "Move window down"            "J" #'evil-window-down
       :desc "Move window left"            "H" #'evil-window-left
       :desc "Move window right"           "L" #'evil-window-right
       )

      (:prefix ("c" . "code")
       :desc "Comment line or region"      :gn "l" #'evilnc-comment-or-uncomment-lines
       :desc "Comment line or region"      :v  "l" #'evilnc-comment-operator
       )

      (:prefix ("i" . "insert")
       :desc "Insert org property"         "p" #'org-set-property
       :desc "Insert last org stored link" "l" #'org-insert-last-stored-link
       )

      (:prefix ("e" . "error")
       :desc "Flycheck list errors"        "l" #'flycheck-list-errors
       :desc "Disable flycheck"            "d" #'flycheck-disable-checker
       :desc "Enable flycheck"             "C" #'flycheck-buffer
       :desc "Flycheck next error"         "n" #'flycheck-next-error
       :desc "Flycheck previous error"     "p" #'flycheck-previous-error
       :desc "Flycheck clear errors"       "c" #'flycheck-clear
       :desc "Flycheck which checker"      "w" #'flycheck-select-checker
       )

      (:prefix ("t" . "toggle")
       :desc "Toggle truncate lines"       "t" #'toggle-truncate-lines
       :desc "Toggle flycheck mode"        "f" #'global-flycheck-mode
       :desc "Toggle company mode"         "c" #'global-company-mode
       :desc "Toggle visual line mode"     "v" #'visual-line-mode
       )

      (:prefix ("j" . "jump")
       :desc "Jump to symbol"              "i" #'imenu
       :desc "Jump to symbol across buffers" "I" #'imenu-anywhere
       :desc "Jump to link"                "l" #'ace-link
       :desc "Avy jump work"               "j" #'ace-pinyin-jump-word
       :desc "Recently clock items"        "c" #'org-mru-clock-in
       )

      (:prefix ("s" . "search")
       :desc "Search buffer"               "s" #'swiper
       :desc "Search current directory"    "d" #'+default/search-from-cwd
       :desc "Search project"              "p" #'+default/search-project
       :desc "Look up online"              "o" #'+lookup/online-select
       )

      (:prefix ("p" . "project")
       :desc "Find file in project"        "f" #'projectile-find-file
       )

      (:prefix ("g" . "git")
       (:when (featurep! :tools magit)
        :desc "Magit status"              "s" #'magit-status
        (:prefix "f"
         "l"                                 #'magit-log-buffer-file
         "p"                                 #'my-put-file-name-on-clipboard
         )
        )
       )
      )

;; org-roam
(global-set-key (kbd "C-c r i") 'org-roam-insert)
(global-set-key (kbd "C-c r I") 'org-roam-insert-immediate)

(after! org
  (global-set-key (kbd "C-c r r") 'org-download-clipboard)
  )
