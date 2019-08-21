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


;;; Agenda filters

  ;; Inbox
  (add-to-list 'org-agenda-custom-commands
               '("i" "inbox"
                 ((tags-todo "+CATEGORY=\"inbox\"" ((org-super-agenda-groups
                                                     '((:name "Inbox items"
                                                              :todo "TODO")
                                                       (:discard (:anything)))))))))

  ;; Today Dashboard
  (add-to-list 'org-agenda-custom-commands
               '("a" "Today Dashboard"
                 ((agenda "" ((org-super-agenda-groups
                               '((:name "Done Today"
                                        :todo "DONE"
                                        :order 99)
                                 (:name "Overdue"
                                        :deadline past)
                                 (:name "Over Scheduled"
                                        :scheduled past)
                                 (:name "Deadline Today"
                                        :deadline today)
                                 (:name "Scheduled Today"
                                        :time-grid t
                                        :scheduled today)
                                 (:name "Due Future"
                                        :deadline future)
                                 (:discard (:anything))))))
                  )
                 ))

  ;; Weekly Dashboard
  (add-to-list 'org-agenda-custom-commands
               '("w" "Weekly Dashboard"
                 ((todo "STARTED" ((org-agenda-overriding-header "In Progress")))
                  (tags-todo "this_week+SCHEDULED<\"<+0d>\"" ((org-agenda-overriding-header "Delayed")))
                  (tags-todo "this_week" ((org-agenda-overriding-header "Planned to do, but unscheduled")
                                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
                  (tags-todo "this_week" ((org-agenda-overriding-header "Already Scheduled")
                                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notscheduled))
                                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("STARTED")))
                                          (org-agenda-sorting-strategy '(todo-state-up time-up category-keep priority-down))))
                  )))


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
  (org-journal-dir "~/Dropbox/X_GTD/journal/")
  (org-journal-file-format "%Y-%m-%d.org")
  )

;; Org attach screenshot
(def-package! org-attach-screenshot
  :config
  (setq org-attach-screenshot-command-line "screencapture -i %f")
  )

;; org-mru-clock
(def-package! org-mru-clock
  :init
  (setq org-mru-clock-how-many 100
        org-mru-clock-completing-read #'ivy-completing-read)
  )

;; org-effectiveness
(def-package! org-effectiveness)
