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


;; org-mru-clock
(use-package! org-mru-clock
  :init
  (setq org-mru-clock-how-many 100
        org-mru-clock-completing-read #'ivy-completing-read)
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

(setq org-agenda-custom-commands
      '(("a" "Cumstom Agenda View"
         ((org-ql-block '(and (todo "TODO" "STARTED")
                              (scheduled :on today)
                              (category "Routine"))
                        ((org-ql-block-header "Routines")))
          (org-ql-block '(and (todo "TODO" "STARTED")
                              (priority "A")
                              (or (scheduled :on today) (deadline :on today)))
                        ((org-ql-block-header "Today MITs")))
          (org-ql-block '(and (todo "TODO" "STARTED")
                              (or (scheduled :on today) (deadline :on today))
                              (not (priority "A"))
                              (not (category "Routine")))
                        ((org-ql-block-header "Today Tasks")))
          (org-ql-block '(and (todo "TODO" "STARTED")
                              (or (scheduled :to today) (deadline :to today))
                              (not (ts :on today)))
                        ((org-ql-block-header "Overscheduled")))
          (org-ql-block '(and (clocked :on today)
                              (not (category "Routine")))
                        ((org-ql-block-header "Today Clocked")))
          (org-ql-block '(and (closed :on today))
                        ((org-ql-block-header "Today Done")))
          (org-ql-block '(and (todo "PROJ"))
                        ((org-ql-block-header "Projects")))
          (agenda)))))
