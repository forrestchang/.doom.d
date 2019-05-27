;;; ~/.doom.d/_org.el -*- lexical-binding: t; -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cI" 'jiayuan/org-clock-in)
(global-set-key "\C-cO" 'org-clock-out)
(global-set-key "\C-cG" 'org-clock-goto)

(map! (:map org-mode-map
        :localleader
        :desc "Org clock in"                        "I" #'org-clock-in
        :desc "Org clock out"                       "O" #'org-clock-out
        :desc "Org clock goto"                      "g" #'org-clock-goto
        :desc "Org pomodoro"                        "p" #'org-pomodoro
        :desc "Org focus subtree"                   "f" #'org-narrow-to-subtree
        :desc "Org unfocus subtree"                 "F" #'widen
        :desc "Org clock report"                    "R" #'org-clock-report
        :desc "Org set effort"                      "e" #'org-set-effort
        :desc "Sync Google Calendar"                "S" #'org-gcal-sync
        )
      (:map org-super-agenda-header-map
        "j" #'evil-next-line
        "k" #'evil-previous-line
        )
      )

;; Org agenda key bindings, see:
;; (find-file "~/.emacs.d/.local/packages/elpa/evil-org-20180323.2306/evil-org-agenda.el")

(evil-define-key 'motion org-agenda-mode-map
  ;; views
  "vl" 'org-agenda-log-mode
  "vc" 'org-agenda-clockreport-mode
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
              ("PROJ" :foreground "#0ffd19a" :weight bold)
              ("WAITING" :foreground "#7b88ff" :weight bold)
              ("HOLD" :foreground "#71a0a5" :weight bold)
              )))

(custom-set-faces
 '(org-checkbox-statistics-todo ((t (:inherit org-todo :foreground "DeepPink2"))))
 '(org-scheduled-today ((t (:foreground "#b0e0a8"))))
 '(org-warning ((t (:foreground "#ee5a5a")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set org directory
(setq org-directory "~/org/")
(setq org-default-notes-file "~/org/inbox.org")

;; Org resize image
(setq org-image-actual-width '(650))

;; Auto trancate lines
(setq-hook! 'org-mode-hook truncate-lines t)

;; Enable org-checklist
(require 'org-checklist)

;; Block tasks when have not done subtasks
(setq org-enforce-todo-dependencies t)

;; Turn off auto fill
(add-hook 'org-mode-hook 'turn-off-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Agenda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-agenda-files '("~/org/inbox.org"
                         "~/org/gtd.org"
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

;; Org super agenda
(org-super-agenda-mode)
(setq org-agenda-custom-commands
      '(("a" "Dashboard\n"
         ((agenda ""
                  ((org-super-agenda-groups
                    '((:name "Due Today"
                             :deadline today)
                      (:name "For Today"
                             :time-grid t
                             :scheduled today)
                      (:name "Overdue"
                             :deadline past)
                      (:name "Past Schedule"
                             :scheduled past)
                      (:discard (:anything t))))))))))

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
        ("t" "Todo" entry (file+headline "~/org/inbox.org" "Todos")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:")
        ("m" "Morning Journal" entry (file+olp+datetree "~/org/journal.org")
         "* Morning Journal\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?")
        ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
         "* %<%F %H:%M:%S>\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?")
        ("P" "Protocol" entry (file+headline "~/org/inbox.org" "Captures")
         "* [[%:link][%:description]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n" :immediate-finish t)
        ("L" "Protocol Link" entry (file+headline "~/org/inbox.org" "Links")
         "* [[%:link][%:description]]\nCaptured On: %U" :immediate-finish t)
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

(setq org-pomodoro-length '50)
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
