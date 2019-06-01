;;; private/my-org/config.el -*- lexical-binding: t; -*-

;; Org pomodoro
(def-package! org-pomodoro
  :init (setq org-pomodoro-length '50
              org-pomodoro-short-break-length '10)
  :config
  ;; Org notification
  (defun notify-osx (title message)
    (call-process "terminal-notifier"
                  nil 0 nil
                  "-group" "Emacs"
                  "-title" title
                  "-sender" "org.gnu.Emacs"
                  "-message" message
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


;; Org Google Calendar
(def-package! org-gcal
  :init (setq org-gcal-client-id "93021018604-omjsj3g7ci2pgvgnj22epu5u0ka9p33h.apps.googleusercontent.com"
              org-gcal-client-secret "TuYRHD5T5DkmypdevKsh7enz"
              org-gcal-file-alist '(("forrestchang7@gmail.com" . "~/Dropbox/org/calendars/gcal_default.org")
                                    ("addressbook#contacts@group.v.calendar.google.com" . "~/Dropbox/org/calendars/gcal_holidays.org")
                                    ("o2lcgjdtg3h441i6m1ddugiee4@group.calendar.google.com" . "~/Dropbox/org/calendars/gcal_hobby.org")
                                    ("b7tj496ae8b76pbfn1utoen58o@group.calendar.google.com" . "~/Dropbox/org/calendars/gcal_learning.org")
                                    ("8eahnd4d5p3avqfbnhn0cgcqq0@group.calendar.google.com" . "~/Dropbox/org/calendars/gcal_life.org")
                                    ("3j9ok2brthfbpdojuj4vtr0hq0@group.calendar.google.com" . "~/Dropbox/org/calendars/gcal_system.org")
                                    ("f39mckhr7gdtp2evivta86gvbk@group.calendar.google.com" . "~/Dropbox/org/calendars/gcal_work.org")
                                    ))
  )


;; Org Super Agenda
(def-package! org-super-agenda
  :config
  (org-super-agenda-mode)

  (setq org-agenda-custom-commands
        '(("a" "Dashboard\n"
           ((agenda ""
                    ((org-super-agenda-groups
                      '((:name "For Today"
                               :time-grid t
                               :scheduled today
                               :deadline today)
                        (:name "Overdue"
                               :deadline past)
                        (:name "Past Schedule"
                               :scheduled past)
                        (:discard (:anything t))))))))))
  )
