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
  ;;
  ;; 这个 agenda view 主要用来查看今天要做的事情，包括：
  ;; 1. deadline today: 今天截止的任务，优先级 P1
  ;; 2. over deadline：已经过期的任务，优先级 P0，一般来说，deadline 只设定
  ;;    真正有具体截止日期的任务
  ;; 3. scheduled today：安排到今天做的任务
  ;; 4. over scheduled：过了安排的日期，这部分任务可以 reschedule
  ;; 5. most important: 优先级高(A)的任务，优先做这部分内容
  ;; 6. done: 已经完成的任务
  ;;
  ;; 每天的 workflow：
  ;; 1. 查看 Today 视图，是否有 overdue 的任务，如果有，优先做 overdue 的任务
  ;; 2. reschedule over scheduled 的任务
  ;; 3. 从这周安排的任务中挑选出今天要做的任务，添加 scheduled time
  ;; 4. 对于 schedule 的任务，添加优先级
  (add-to-list 'org-agenda-custom-commands
               '("a" "Today Dashboard"
                 ((agenda "" ((org-super-agenda-groups
                               '(
                                 (:name "Completed Today"
                                        :todo ("DONE" "CANCELLED")
                                        :order 99)
                                 (:name "Overdue"
                                        :deadline past)
                                 (:name "Most Important Tasks"
                                        :priority "A")
                                 (:name "Deadline Today"
                                        :deadline today)
                                 (:name "Over Scheduled"
                                        :scheduled past)
                                 (:name "Agenda View"
                                        :time-grid t)
                                 (:name "Scheduled Today"
                                        :scheduled today)
                                 (:name "Due Future"
                                        :deadline future)
                                 (:discard (:anything))))
                              ;; (org-agenda-tag-filter-preset '("-PROJ"))
                              ))
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

;; org-ql
(def-package! org-ql
  :config

  (setq org-ql-views
        (list
         (cons "Recent entries" #'org-ql-view-recent-items)
         ))

  )
