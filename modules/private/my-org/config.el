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

;; (defmacro target-date ()
;;   (let (((sec minute hour day month year dow dst utcoff) (decode-time))
;;         (last-day-of-month (calendar-last-day-of-month month year)))
;;     (format "%d-%02d-%02d" year month (1+ last-day-of-month))))

;; (defconst jiayuan/org-completed-date-regexp
;;   (concat " \\("
;;           "CLOSED: \\[%Y-%m-%d"
;;           ;; "\\|"
;;           ;; "- State \"\\(DONE\\)\" * from .* \\[%Y-%m-%d"
;;           ;; "\\|"
;;           ;; "- State .* ->  *\"\\(DONE\\)\" * \\[%Y-%m-%d"
;;           "\\) ")
;;   "Matches any completion time stamp.")

;; Org Super Agenda
(def-package! org-super-agenda
  :config
  (org-super-agenda-mode)

  (setq org-agenda-custom-commands
        '(
          ;; Dashboard
          ("a" "Dashboard" agenda ""
           ((org-super-agenda-groups
             '(
               (:name "MIT"
                      :tag "mit")
               (:name "Day Plan"
                      :time-grid t)
               (:name "Important"
                      :priority "A"
                      :deadline today)
               (:name "For Today"
                      :scheduled today)
               (:name "Deadline Past"
                      :deadline past)
               (:name "Scheduled past"
                      :scheduled past)))
            (org-agenda-span 1)))

          ;; Goals
          ("g" "Goals"
           ((tags-todo "DEADLINE<\"<+0d>\"+TODO={PROJ}"
                       ((org-agenda-overriding-header "Past Deadline")))
            (tags-todo "DEADLINE>=\"<+0d>\"+DEADLINE<=\"<+7d>\"+TODO={PROJ}"
                       ((org-agenda-overriding-header "Focus This Week")))
            (tags-todo "DEADLINE>\"<+7d>\"+DEADLINE<=\"<+1m>\"+TODO={PROJ}"
                       ((org-agenda-overriding-header "Focus This Month")))
            (tags-todo "DEADLINE>\"<+1m>\"+DEADLINE<=\"<+3m>\"+TODO={PROJ}"
                       ((org-agenda-overriding-header "Focus This Quarter")))
            (tags-todo "DEADLINE>\"<+3m>\"+DEADLINE<=\"<+1m>\"+TODO={PROJ}"
                       ((org-agenda-overriding-header "Focus This Year")))
            ))

          ;; Quarter OKR
          ("o" "Quarter OKR"
           ((tags-todo "DEADLINE>=\"[2019-07-01]\"+DEADLINE<=\"[2019-09-30]\"+TODO={PROJ}"
                       ((org-agenda-overriding-header "2019 Quarter 3 OKRs")))))

          ;; Stuck
          ("s" "Stuck"
           ((tags-todo "TODO={WAITING}"
                       ((org-agenda-overriding-header "WAITING Tasks")))
            (tags-todo "TODO={HOLD}"
                       ((org-agenda-overriding-header "HOLD Tasks")))))

          ;; Planning
          ("p" "Planning"
           ((tags-todo "SCHEDULED<\"<+0d>\""
                       ((org-agenda-overriding-header "Past Schedule")))
            (tags-todo "SCHEDULED>=\"<+0d>\"+SCHEDULED<=\"<+7d>\""
                       ((org-agenda-overriding-header "Scheduled This Week")))
            (tags-todo "SCHEDULED>\"<+7d>\"+SCHEDULED<=\"<+1m>\""
                       ((org-agenda-overriding-header "Scheduled This Month")))
            (tags-todo "SCHEDULED>\"<+1m>\"+SCHEDULED<=\"<+3m>\""
                       ((org-agenda-overriding-header "Scheduled This Quarter")))
            (tags-todo "SCHEDULED>\"<+3m>\"+SCHEDULED<=\"<+1m>\""
                       ((org-agenda-overriding-header "Scheduled This Year")))
            ))

          ;; Easy Stuff
          ("e" "Easy Stuff" todo ""
           ((org-super-agenda-groups
             '((:name "When Your Are Tired"
                      :tag "easy")
               (:name "Small Things, Less Than 15 min"
                      :effort< "00:15")
               (:discard (:anything))))))

          ;; Forecast
          ;; ("f" "Forecast" todo ""
          ;;  ((org-super-agenda-groups
          ;;    `((:deadline (before ,target-date))
          ;;      (:discard (:anything t))))))

          ))
  )

;; Hugo blog
(def-package! ox-hugo
  :after ox
  )

(def-package! org-hugo-auto-export-mode)


;; org-journal
(def-package! org-journal
  :custom
  (org-journal-dir "~/Dropbox/org/journal/")
  (org-journal-file-format "%Y-%m-%d.org")
  )

(def-package! org-attach-screenshot
  :config
  (setq org-attach-screenshot-command-line "screencapture -i %f")
  )
