;;; private/my-org/config.el -*- lexical-binding: t; -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Definition
;;;
;;; - org-pomodoro
;;; - org-super-agendsa
;;; - ox-hugo
;;; - org-hugo-auto-export-mode
;;; - org-journal
;;; - org-attach-screenshot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Org pomodoro
(def-package! org-pomodoro
  :init (setq org-pomodoro-length '50
              org-pomodoro-short-break-length '10)
  :config
  (defvar terminal-notifier-command
    (executable-find "terminal-notifier") "/usr/local/bin/terminal-notifier")
  ;; Org notification
  (defun notify-osx (title message)
    (start-process "terminal-notifier"
                   "*terminal-notifier*"
                   terminal-notifier-command
                   "-title" title
                   "-message" message
                   "-sender" "org.gnu.Emacs"
                   "-activate" "org.gnu.Emacs"))

  (add-hook 'org-pomodoro-finished-hook
            (lambda ()
              (notify-osx "Pomodoro completed!" "Time for a break.")))

  (add-hook 'org-pomodoro-break-finished-hook
            (lambda ()
              (notify-osx "Pomodoro Short Break Finished" "Ready for Another?")))

  (add-hook 'org-pomodoro-long-break-finished-hook
            (lambda ()
              (notify-osx "Pomodoro Long Break Finished" "Ready for Another?")))

  (add-hook 'org-pomodoro-killed-hook
            (lambda ()
              (notify-osx "Pomodoro Killed" "One does not simply kill a pomodoro!")))
  )

;; Org Super Agenda
(def-package! org-super-agenda
  :config
  (org-super-agenda-mode)

  ;;; Org Agenda Views

  ;; Inbox
  (add-to-list 'org-agenda-custom-commands
               '("i" "inbox" tags-todo "+CATEGORY=\"inbox\""
                 ((org-super-agenda-groups
                   '((:name "TODO"
                            :todo "TODO")
                     (:discard (:anything)))))))

  ;; Dashboard
  (add-to-list 'org-agenda-custom-commands
               '("a" "Dashboard" agenda ""
                 ((org-super-agenda-groups
                   '(
                     (:name "Most Important"
                            :priority "A")
                     (:name "Daily View"
                            :time-grid t)
                     (:name "Scheduled Today"
                            :scheduled today)
                     (:name "Over Schedule"
                            :scheduled past)
                     (:name "Due Today"
                            :deadline today)
                     (:name "Overdue"
                            :deadline past)
                     (:name "Due Future"
                            :deadline future)
                     (:discard (:anything))))
                  (org-agenda-span 1))) t)

  ;; Weekly Overview
  (add-to-list 'org-agenda-custom-commands
               '("w" "Weekly Overview" todo ""
                 ((org-super-agenda-groups
                   '((:name "This Week's Tasks"
                            :todo "NEXT")
                     (:name "Delayed Tasks"
                            :todo "DELAYED")
                     (:name "In Progress"
                            :todo "STARTED")
                     (:discard (:anything)))))))

  ;; OKRs
  (add-to-list 'org-agenda-custom-commands
               '("o" "OKRs" todo ""
                 ((org-super-agenda-groups
                   '((:name "Objectives"
                            :todo "OBJECTIVE")
                     (:name "Key Results"
                            :todo "KEYR")
                     (:discard (:anything)))))))

  ;; Goals
  ;;
  ;; All tasks include scheduled items.
  (add-to-list 'org-agenda-custom-commands
               '("g" "Goals" todo ""
                 ((org-super-agenda-groups
                   '((:name "Focus This Week"
                            :tag "this_week")
                     (:name "Focus This Month"
                            :tag "this_month")
                     (:name "Focus This Quarter"
                            :tag "this_quarter")
                     (:name "Focus This Year"
                            :tag "this_year")
                     (:discard (:anything)))))) t)

  ;; Planning
  ;;
  ;; Same to Goals filter, but not include scheduled items, just for daily planning.
  (add-to-list 'org-agenda-custom-commands
               '("p" "Planning"
                 ((todo ""
                        ((org-super-agenda-groups
                          '((:name "Not Scheduled This Week"
                                   :tag "this_week")
                            (:discard (:anything))))
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled)))))) t)

  ;; Review
  (add-to-list 'org-agenda-custom-commands
               '("r" . "Review...") t)

  ;; Review by Deadline
  (add-to-list 'org-agenda-custom-commands
               '("rd" "Review by Deadline"
                 ((tags-todo "DEADLINE<\"<+0d>\""
                             ((org-agenda-overriding-header "Overdue")
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if 'notdeadline))))
                  (tags-todo "DEADLINE=\"<+0d>\""
                             ((org-agenda-overriding-header "Due Today")
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if 'notdeadline))))
                  (tags-todo "DEADLINE=\"<+1d>\""
                             ((org-agenda-overriding-header "Due Tomorrow")
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if 'notdeadline))))
                  (tags-todo "DEADLINE>\"<+1d>\"+DEADLINE<=\"<+7d>\""
                             ((org-agenda-overriding-header "Due Within A Week")
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if 'notdeadline))))
                  (tags-todo "DEADLINE>\"<+7d>\"+DEADLINE<=\"<+28d>\""
                             ((org-agenda-overriding-header "Due Within A Month")
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if 'notdeadline))))
                  (tags-todo "DEADLINE>\"<+28d>\""
                             ((org-agenda-overriding-header "Due Later")
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if 'notdeadline))))

                  (tags-todo "TODO={STARTED}"
                             ((org-agenda-overriding-header "No Due Date & STARTED")
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if 'deadline))))
                  (tags-todo "TODO={HOLD}"
                             ((org-agenda-overriding-header "No Due Date & HOLD")
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if 'deadline))))
                  (tags-todo "TODO={WAITING}"
                             ((org-agenda-overriding-header "No Due Date & WAITING")
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if 'deadline))))
                  )) t)

  ;; Review by Priority
  (add-to-list 'org-agenda-custom-commands
               '("rp" "Review by Priority (Unscheduled)"
                 ((tags-todo "PRIORITY={A}"
                             ((org-agenda-overriding-header "High PRIORITY")
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if 'deadline 'schedule))))
                  (tags-todo "PRIORITY={B}"
                             ((org-agenda-overriding-header "Medium PRIORITY")
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if 'deadline 'schedule))))
                  (tags-todo "PRIORITY=\"\""
                             ((org-agenda-overriding-header "None PRIORITY")
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if 'deadline 'schedule))))
                  (tags-todo "PRIORITY={C}"
                             ((org-agenda-overriding-header "Low PRIORITY")
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if 'deadline 'schedule))))
                  )) t)

  ;; Easy Stuff
  (add-to-list 'org-agenda-custom-commands
               '("e" "Easy Stuff" todo ""
                 ((org-super-agenda-groups
                   '((:name "When You Are Tired"
                            :tag "easy")
                     (:name "Small Stuff, Less Than 15 min"
                            :effort< "00:15")
                     (:discard (:anything)))))) t)

  )

;; Hugo blog
(def-package! ox-hugo
  :after ox
  )

;; Org hugo auto export mode
(def-package! org-hugo-auto-export-mode)

;; Org journal
(def-package! org-journal
  :custom
  (org-journal-dir "~/Dropbox/org/journal/")
  (org-journal-file-format "%Y-%m-%d.org")
  )

;; Org attach screenshot
(def-package! org-attach-screenshot
  :config
  (setq org-attach-screenshot-command-line "screencapture -i %f")
  )
