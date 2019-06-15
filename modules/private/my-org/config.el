;;; private/my-org/config.el -*- lexical-binding: t; -*-

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

  (setq org-agenda-custom-commands
        '(("a" "Dashboard\n"
           ((agenda ""
                    ((org-super-agenda-groups
                      '(
                        (:name "GOAL"
                               :tag ("goal"))
                        (:name "Most Important Tasks"
                               :priority "A")
                        (:name "For Today"
                               :time-grid t
                               :scheduled today
                               :deadline today)
                        (:name "Overdue"
                               :deadline past)
                        (:name "Past Schedule"
                               :scheduled past)
                        (:discard (:anything t))))))))))
  )

;; Hugo blog
(def-package! ox-hugo
  :after ox
  )

(def-package! org-hugo-auto-export-mode)
