;;; ~/.doom.d/_org.el -*- lexical-binding: t; -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cI" 'jiayuan/org-clock-in)
(global-set-key "\C-cO" 'org-clock-out)
(global-set-key "\C-cG" 'org-clock-goto)

(map! :map org-mode-map
      :localleader
      :desc "Org clock in"                        "I" #'org-clock-in
      :desc "Org clock out"                       "O" #'org-clock-out
      :desc "Org clock goto"                      "g" #'org-clock-goto
      :desc "Org pomodoro"                        "p" #'org-pomodoro
      :desc "Org focus subtree"                   "f" #'org-narrow-to-subtree
      :desc "Org unfocus subtree"                 "F" #'widen
      :desc "Org clock report"                    "R" #'org-clock-report
      :desc "Org set effort"                      "e" #'org-set-effort
      )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Org headlines face
(dolist (face '(org-level-1
                org-level-2 org-level-3
                org-level-4 org-level-5
                org-level-6 org-level-7
                org-level-8))
  (set-face-attribute face nil :weight 'normal))

;; Org todo keywords face
(setq org-todo-keyword-faces
      (quote (
              ("TODO" :foreground "#d65a31" :weight bold)
              ("STARTED" :foreground "#4285F4" :weight bold)
              ("PROJ" :foreground "#5cc9f5" :weight bold)
              ("WAITING" :foreground "#7b88ff" :weight bold)
              ("HOLD" :foreground "#ffe837" :weight bold)
              )))

(custom-set-faces
 '(org-checkbox-statistics-todo ((t (:inherit org-todo :foreground "DeepPink2"))))
 '(org-scheduled-today ((t (:inherit org-todo :foreground "#b0e0a8")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set org directory
(setq org-directory "~/Dropbox/personal-site/workspace/org/")
(setq org-default-notes-file "~/Dropbox/personal-site/workspace/org/inbox.org")

;; Org resize image
(setq org-image-actual-width '(650))

;; Auto trancate lines
(setq-hook! 'org-mode-hook truncate-lines t)

;; Enable org-checklist
(require 'org-checklist)

;; Block tasks when have not done subtasks
(setq org-enforce-todo-dependencies t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Agenda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-agenda-files '("~/Dropbox/personal-site/workspace/org/inbox.org"
                         "~/Dropbox/personal-site/workspace/org/todo.org"
                         ))

(setq org-agenda-span '1)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-start-day "0d")

(setq org-log-into-drawer t)

(setq org-agenda-archives-mode t)

(setq org-agenda-time-grid
      (quote
       ((daily today remove-match)
        (0800 1000 1200 1400 1600 1800 2000 2200)
        "......" "----------------")))

;; Set todo Keyworkds
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "PROJ(p)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

;; Log done date
(setq org-log-done t)

;; Non-nil means switching TODO states with S-cursor counts as state change.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; Warning 30 days before deadline
(setq org-deadline-warning-days 30)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Refile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-refile-use-outline-path 'file)

(setq org-outline-path-complete-in-steps nil)

(setq org-refile-targets
      '((org-agenda-files . (:maxlevel . 4))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline "~/Dropbox/personal-site/workspace/org/inbox.org" "Todos")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:")
        ("P" "Protocol" entry (file+headline "~/Dropbox/personal-site/workspace/org/inbox.org" "Captures")
         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?" :kill-buffer t)
        ("L" "Protocol Link" entry (file+headline "~/Dropbox/personal-site/workspace/org/inbox.org" "Links")
         "* %? [[%:link][%:description]] \nCaptured On: %U" :kill-buffer t)
        ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reusme clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Show lot of clocking history so it's easy to pick items off
(setq org-clock-history-length 25)

;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))

;; Save clock data and state changes to LOGBOOK drawer
(setq org-clock-into-drawer t)

;; Remove zero clock records
(setq org-clock-out-remove-zero-time-clocks t)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume t)

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;; Use pretty things for the clocktable
(setq org-pretty-entities t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

;; Change tasks to whatever when clocking in
(setq org-clock-in-switch-to-state "STARTED")

;; Global effort estimate values
(setq org-global-properties
      '(("Effort_ALL" .
         "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00")))

(defun jiayuan/org-clock-in ()
  (interactive)
  (org-clock-in '(4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Column
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Pomodoro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-pomodoro-length '1)
(setq org-pomodoro-short-break-length '10)

;; Org notification
(defun notify-osx (title message)
  (call-process "terminal-notifier"
                nil 0 nil
                "-group" "Emacs"
                "-title" title
                "-sender" "org.gnu.Emacs"
                "-message" message
                "-activate" "org.gnu.Emacs"))

(after! org-pomodoro
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
