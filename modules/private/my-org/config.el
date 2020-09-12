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
(use-package! org-pomodoro
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
(use-package! org-super-agenda
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
  )

;; Org journal
(use-package! org-journal
  :custom
  (org-journal-dir "~/org/journal/")
  (org-journal-file-format "%Y-%m-%d.org")
  )

;; Org attach screenshot
(use-package! org-attach-screenshot
  :config
  (setq org-attach-screenshot-command-line "screencapture -i %f")
  )

;; org-mru-clock
(use-package! org-mru-clock
  :init
  (setq org-mru-clock-how-many 100
        org-mru-clock-completing-read #'ivy-completing-read)
  )

;; org-effectiveness
(use-package! org-effectiveness)

;; org-ql
(use-package! org-ql
  :config

  (setq org-ql-views
        (list
         (cons "Recent entries" #'org-ql-view-recent-items)
         ))

  ;; Most Importants Tasks
  ;; - Show priority A tasks in 7 days
  (defun custom-ql-most-important-thins ()
    (interactive)
    (org-ql-search (org-agenda-files)
      '(and (todo "TODO" "STARTED" "BLOCKED")
            (priority >= "A")
            (or (deadline auto)
                (scheduled :to today)))
      :title "Most Important Tasks"
      :sort '(deadline priority)
      :super-groups '((:name "Deadline"
                             :deadline past
                             :deadline today)
                      (:name "Scheduled"
                             :scheduled past
                             :scheduled today))))

  ;; Work Related Tasks Scheduled or Deadline today
  (defun custom-ql-work-related-tasks ()
    (interactive)
    (org-ql-search (org-agenda-files)
      '(and (or (scheduled :to today)
                (deadline :to today))
            (todo "TODO" "STARTED" "BLOCKED")
            (not (tags "PROJ"))
            (category "iqiyi" "pingduck" "workflow" "talk" "blog" "open-source" "programming"))
      :title "Work Related Tasks"
      :sort '(priority deadline scheduled)
      :super-groups '((:name "STARTED"
                             :todo "STARTED")
                      (:name "Deadline"
                             :deadline past
                             :deadline today)
                      (:name "Scheduled"
                             :scheduled past
                             :scheduled today)))
    )

  ;; Non-Work Related Tasks Scheduled or Deadline today
  (defun custom-ql-non-work-related-tasks ()
    (interactive)
    (org-ql-search (org-agenda-files)
      '(and (or (scheduled :to today)
                (deadline :to today))
            (todo "TODO" "STARTED" "BLOCKED")
            (not (tags "PROJ"))
            (not (category "iqiyi" "pingduck" "workflow" "talk" "blog" "open-source" "programming")))
      :title "Non-Work Related Tasks"
      :sort '(priority deadline scheduled)
      :super-groups '((:name "STARTED"
                             :todo "STARTED")
                      (:name "Deadline"
                             :deadline past
                             :deadline today)
                      (:name "Scheduled"
                             :scheduled past
                             :scheduled today))))

  ;; Today log
  (defun custom-ql-today-log ()
    (interactive)
    (org-ql-search (org-agenda-files)
      '(and (todo "DONE")
            (closed :on today))
      :title "Today log"
      :sort '(date)
      :super-groups '((:name "Projects"
                             :tag "PROJ")))
    )

  ;; Weekly Review
  (defun custom-ql-weekly-review ()
    (interactive)
    (let* ((ts-default-format "%Y-%m-%d")
           (beg (ts-format (car (current-week-range))))
           (end (ts-format (cdr (current-week-range)))))
      (org-ql-search (org-agenda-files)
        `(and (todo "TODO" "STARTED" "BLOCKED")
              (tags "PROJ")
              (or (scheduled :from ,beg :to ,end)
                  (deadline :from ,beg :to ,end)
                  (scheduled :to ,beg)
                  (deadline :to ,beg)))
        :title "Weekly Review"
        :super-groups '((:name "Overdue"
                               :deadline past)
                        (:name "Over Scheduled"
                               :scheduled past)
                        (:name "STARTED"
                               :todo "STARTED")
                        (:name "TODO"
                               :todo "TODO")
                        (:name "BLOCKED"
                               :todo "BLOCKED")
                        (:name "DONE"
                               :todo "DONE"))
        )))
  )

(defun current-week-range ()
  "Return timestamps (BEG . END) spanning current calendar week."
  (let* (;; Bind `now' to the current timestamp to ensure all calculations
         ;; begin from the same timestamp.  (In the unlikely event that
         ;; the execution of this code spanned from one day into the next,
         ;; that would cause a wrong result.)
         (now (ts-now))
         ;; We start by calculating the offsets for the beginning and
         ;; ending timestamps using the current day of the week.  Note
         ;; that the `ts-dow' slot uses the "%w" format specifier, which
         ;; counts from Sunday to Saturday as a number from 0 to 6.
         (adjust-beg-day (- 1 (ts-dow now)))
         (adjust-end-day (- 7 (ts-dow now)))
         ;; Make beginning/end timestamps based on `now', with adjusted
         ;; day and hour/minute/second values.  These functions return
         ;; new timestamps, so `now' is unchanged.
         (beg (thread-last now
                ;; `ts-adjust' makes relative adjustments to timestamps.
                (ts-adjust 'day adjust-beg-day)
                ;; `ts-apply' applies absolute values to timestamps.
                (ts-apply :hour 0 :minute 0 :second 0)))
         (end (thread-last now
                (ts-adjust 'day adjust-end-day)
                (ts-apply :hour 23 :minute 59 :second 59))))
    (cons beg end)))
