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
        :desc "Org clock in"                        "i" #'org-clock-in
        :desc "Org clock out"                       "o" #'org-clock-out
        :desc "Org clock goto"                      "g" #'org-clock-goto
        :desc "Org pomodoro"                        "p" #'org-pomodoro
        :desc "Org focus subtree"                   "f" #'org-narrow-to-subtree
        :desc "Org unfocus subtree"                 "F" #'widen
        :desc "Org clock report"                    "R" #'org-clock-report
        :desc "Org set effort"                      "e" #'org-set-effort
        :desc "Generage Hugo blog post"             "h" #'org-hugo-export-wim-to-md
        :desc "Org attach image"                    "c" #'org-attach-screenshot
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
  "ds" 'org-agenda-schedule
  "dd" 'org-agenda-deadline
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
              ("NEXT" :foreground "#f9b95c" :weight bold)
              ("STARTED" :foreground "#67c23a" :weight bold)
              ("PROJ" :foreground "#f36886" :weight bold)
              ("WAITING" :foreground "#7b88ff" :weight bold)
              ("HOLD" :foreground "#71a0a5" :weight bold)
              ("MAYBE" :foreground "#f78ae0" :weight bold)
              ("DELAYED" :foreground "#ff0000" :weight bold)
              )))

(custom-set-faces
 '(org-checkbox-statistics-todo ((t (:inherit org-todo :foreground "DeepPink2"))))
 '(org-scheduled-today ((t (:foreground "#b0e0a8"))))
 '(org-warning ((t (:foreground "#ee5a5a")))))

;; Popup rules
(set-popup-rules!
  '(
    ("^\\*Org Agenda"    :size 0.5 :select t :ttl nil)
    ("^\\*Org Src"       :size 0.4 :quit nil :select t :autosave t :ttl nil)
    ("^CAPTURE.*\\.org$" :size 0.4 :quit nil :select t :autosave t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set org directory
(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file "~/Dropbox/org/inbox.org")

;; Org resize image
(setq org-image-actual-width '(650))

;; Auto wrap line
;; (remove-hook 'org-mode-hook 'auto-fill-mode)
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (setq truncate-lines nil)
;;             (visual-line-mode 1)))

;; Enable org-checklist
(def-package! org-checklist)

;; Block tasks when have not done subtasks
(setq org-enforce-todo-dependencies t)

;; Block tasks when have not done checkobx
(setq org-enforce-todo-checkbox-dependencies t)

;; Remove hack in chinese layer: remove blanks when export
(advice-remove! 'org-html-paragraph '+chinese*org-html-paragraph)

;; org-checklist
(add-to-list 'org-modules 'org-checklist)

;; org-buttlets
(setq org-bullets-bullet-list '("◉" "◉" "◉" "◉"))

;; The notes will be ordered according to time
(setq org-log-states-order-reversed nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Agenda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-agenda-files '("~/Dropbox/org/phone.org"
                         "~/Dropbox/org/gtd.org"
                         "~/Dropbox/org/someday.org"
                         "~/Dropbox/org/planning.org"
                         "~/Dropbox/org/calendar.org"
                         "~/Dropbox/personal-site/blog/content-org/post.org"
                         "~/Dropbox/org/journal/"
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
      (quote ((sequence "TODO(t)" "NEXT(n)" "DELAYED(x@/!)" "STARTED(s)" "MAYBE(m)" "WAIT(w@/!)" "HOLD(h@/!)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "PROJ(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              )))

;; Log done date
(setq org-log-done t)

;; Non-nil means switching todo states with S-cursor counts as state change.
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
        ("t" "Todo" entry (file+headline "~/Dropbox/org/gtd.org" "Inbox")
         "* TODO %?\nCREATED: %U\n")
        ("j" "Journal" entry (file+olp+datetree "~/Dropbox/org/journal.org")
         "* %?\nEntered on %U\n%i\n")
        ("r" "Reading" entry (file+olp+datetree "~/Dropbox/personal-site/blog/content-org/readings.org")
         "* %?\nEntered on %U\n%i\n" :tree-type week)
        ("n" "Quick Notes" entry (file+olp+datetree "~/Dropbox/org/journal.org")
         "* %?\nCREATED: %U\n")
        ("P" "Protocol" entry (file+headline "~/Dropbox/org/gtd.org" "Inbox")
         "* [[%:link][%:description]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n" :immediate-finish t)
        ("L" "Protocol Link" entry (file+headline "~/Dropbox/org/gtd.org" "Inbox")
         "* TODO [[%:link][%:description]]\nCaptured On: %U" :immediate-finish t)
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

;; Set org clock report time format
(setq org-duration-format (quote h:mm))

;; Clock mode line
(setq org-clock-mode-line-total 'auto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Column
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Hacks
;;;
;;; https://orgmode.org/worg/org-hacks.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 让中文也可以不加空格就使用行内格式
(setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
(setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
(org-element-update-syntax)
;; 规定上下标必须加 {}，否则中文使用下划线时它会以为是两个连着的下标
(setq org-use-sub-superscripts "{}")
