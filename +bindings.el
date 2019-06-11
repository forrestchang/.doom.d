;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;; Unbind keys
(map! :leader
      "A" nil
      "X" nil
      )

;; Leader key
(map! :leader
      :desc "M-x"                                    "SPC" #'execute-extended-command
      :desc "Find file in project"                     "." #'projectile-find-file
      :desc "Org agenda view"                          "A" (lambda () (interactive) (org-agenda nil "a"))
      :desc "Capture Inbox"                            "I" (lambda () (interactive) (org-capture nil "t"))
      :desc "Org Journal"                              "J" (lambda () (interactive) (org-capture nil "j"))
      :desc "Org clock goto"                           "G" #'org-clock-goto
      :desc "Org capture"                              "C" #'org-capture
      :desc "Expand region"                            "<" #'er/expand-region
      )

(map!
 (:when (featurep! :tools lookup)
   :nv "gb" #'better-jumper-jump-backward
   :nv "gf" #'better-jumper-jump-forward
   :nv "B"  #'switch-to-buffer
   ))

;; Prefix map
(map! :leader
      (:prefix ("b" . "buffer")
        :desc "Previous buffer"           "TAB" #'previous-buffer
        :desc "Switch buffer"               "b" #'switch-to-buffer
        :desc "Delete buffer"               "d" #'kill-this-buffer
        :desc "Recent files"                "r" #'recentf-open-files
        )

      (:prefix ("w" . "window")
        :desc "Delete window or workspace"  "d" #'+workspace/close-window-or-workspace
        :desc "Maximize window"             "m" #'doom/window-maximize-buffer
        :desc "Other window"                "w" #'other-window
        :desc "Split window right"          "|" #'split-window-right
        :desc "Split window below"          "-" #'split-window-below
        )

      (:prefix ("c" . "code")
        :desc "Comment line or region"      "l" #'evil-commentary-line
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
        :desc "Avy jump work"               "j" #'avy-goto-char-timer
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
          )
        )
      )
