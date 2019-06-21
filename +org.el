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
        :desc "Sync Google Calendar"                "S" #'org-gcal-sync
        :desc "Generage Hugo blog post"             "h" #'org-hugo-export-wim-to-md
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
              ("STARTED" :foreground "#67c23a" :weight bold)
              ("PROJ" :foreground "#0ffd19a" :weight bold)
              ("WAITING" :foreground "#7b88ff" :weight bold)
              ("HOLD" :foreground "#71a0a5" :weight bold)
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

;; Remove hack in chinese layer: remove blanks when export
(advice-remove! 'org-html-paragraph '+chinese*org-html-paragraph)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Agenda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-agenda-files '("~/Dropbox/org/inbox.org"
                         "~/Dropbox/org/gtd.org"
                         "~/Dropbox/org/someday.org"
                         "~/Dropbox/org/planning.org"
                         "~/Dropbox/org/calendar.org"
                         "~/Dropbox/org/personal-site/blog/content-org/post.org"
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
        ("t" "Todo" entry (file+headline "~/Dropbox/org/inbox.org" "Todos")
         "* TODO %?\nCREATED: %U\n")
        ("j" "Journal" entry (file+olp+datetree "~/Dropbox/org/journal.org")
         "* %?\nEntered on %U\n%i\n")
        ("r" "Reading" entry (file+olp+datetree "~/Dropbox/personal-site/blog/content-org/readings.org")
         "* %?\nEntered on %U\n%i\n" :tree-type week)
        ("q" "Quick Notes" entry (file+olp+datetree "~/Dropbox/org/journal.org")
         "* %?\nCREATED: %U\n")
        ("P" "Protocol" entry (file+headline "~/Dropbox/org/inbox.org" "Captures")
         "* [[%:link][%:description]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n" :immediate-finish t)
        ("L" "Protocol Link" entry (file+headline "~/Dropbox/org/inbox.org" "Links")
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

;; Set org clock report time format
(setq org-duration-format (quote h:mm))


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

;; Pick random org entry
;; https://gist.github.com/tonyday567/4343164
(defun org-random-entry (&optional arg)
  "Select and goto a random todo item from the global agenda"
  (interactive "P")
  (if org-agenda-overriding-arguments
      (setq arg org-agenda-overriding-arguments))
  (if (and (stringp arg) (not (string-match "\\S-" arg))) (setq arg nil))
  (let* ((today (org-today))
         (date (calendar-gregorian-from-absolute today))
         (kwds org-todo-keywords-for-agenda)
         (lucky-entry nil)
         (completion-ignore-case t)
         (org-agenda-buffer (when (buffer-live-p org-agenda-buffer)
                              org-agenda-buffer))
         (org-select-this-todo-keyword
          (if (stringp arg) arg
            (and arg (integerp arg) (> arg 0)
                 (nth (1- arg) kwds))))
         rtn rtnall files file pos marker buffer)
    (when (equal arg '(4))
      (setq org-select-this-todo-keyword
            (org-icompleting-read "Keyword (or KWD1|K2D2|...): "
                                  (mapcar 'list kwds) nil nil)))
    (and (equal 0 arg) (setq org-select-this-todo-keyword nil))
    (catch 'exit
      (org-compile-prefix-format 'todo)
      (org-set-sorting-strategy 'todo)
      (setq files (org-agenda-files nil 'ifmode)
            rtnall nil)
      (while (setq file (pop files))
        (catch 'nextfile
          (org-check-agenda-file file)
          (setq rtn (org-agenda-get-day-entries file date :todo))
          (setq rtnall (append rtnall rtn))))

      (when rtnall
        (setq lucky-entry
              (nth (random
                    (safe-length
                     (setq entries rtnall)))
                   entries))

        (setq marker (or (get-text-property 0 'org-marker lucky-entry)
                         (org-agenda-error)))
        (setq buffer (marker-buffer marker))
        (setq pos (marker-position marker))
        (org-pop-to-buffer-same-window buffer)
        (widen)
        (goto-char pos)
        (when (derived-mode-p 'org-mode)
          (org-show-context 'agenda)
          (save-excursion
            (and (outline-next-heading)
                 (org-flag-heading nil))) ; show the next heading
          (when (outline-invisible-p)
            (show-entry))                 ; display invisible text
          (run-hooks 'org-agenda-after-show-hook))))))
