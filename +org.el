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
       :desc "Org add note"                        "n" #'org-add-note
       :desc "Org archive default"                 "a" #'org-archive-subtree-default
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


;; Set todo Keyworkds
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "MAYBE(m)" "BLOCKED(b@/!)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "PROJ(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              )))

(setq org-todo-keyword-faces
      '(("TODO" . "green")
        ("STARTED" . "yellow")
        ("MAYBE" . "gray")
        ("BLOCKED" . "pink")
        ))

;; Org tags face
(setq org-tag-faces
      '(
        ("PROJ"
         (:weight bold :slant italic
          :foreground "#E6AC00"))
        ))

;; Popup rules
(set-popup-rules!
  '(
    ("^\\*Org Agenda"    :size 0.5 :select t :ttl nil :modeline t)
    ("^\\*Org Src"       :size 0.4 :quit nil :select t :autosave t :ttl nil)
    ("^CAPTURE.*\\.org$" :size 0.4 :quit nil :select t :autosave t)
    ("^\\*Org*" :size 0.5 :select t)
    ))

;; Show modeline in popup windows
;; (remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-default-notes-file
(setq org-default-notes-file "~/org/roam/gtd/inbox.org")

;; Export ignore `_`
(setq org-export-with-sub-superscripts nil)

;; Org resize image
(setq org-image-actual-width '(650))

;; Auto wrap line
;; (remove-hook 'org-mode-hook 'auto-fill-mode)
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (setq truncate-lines nil)
;;             (visual-line-mode 1)))

;; Enable org-checklist
(use-package! org-checklist)

;; Block tasks when have not done subtasks
(setq org-enforce-todo-dependencies t)

;; Block tasks when have not done checkobx
(setq org-enforce-todo-checkbox-dependencies t)

;; Remove hack in chinese layer: remove blanks when export
;; (advice-remove! 'org-html-paragraph '+chinese*org-html-paragraph)

;; org-checklist
(add-to-list 'org-modules 'org-checklist)

;; org-buttlets
(setq org-bullets-bullet-list '("◉" "⋆" "○" "‣"))

;; The notes will be ordered according to time
(setq org-log-states-order-reversed nil)

;; Using unique ID's for links in Org-mode
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
      org-clone-delete-id t)

;; Exclude some tags inheritance
(setq org-tags-exclude-from-inheritance '("PROJ"))

;; Copy org item link in org agenda mode
;; Source: https://koenig-haunstetten.de/2018/02/17/improving-my-orgmode-workflow/
(defun my/copy-idlink-to-clipboard()
  "Copy an ID link with the headline to killring, if no ID is there then create
a new unique ID. This function works only in org-mode or org-agenda buffers.

The purpose of this function is to easily construct id:-links to org-mode items.
If its assigned to a key it saves you marking the text and copying to the
killring."
  (interactive)
  (when (eq major-mode 'org-agenda-mode) ;switch to orgmode
    (org-agenda-show)
    (org-agenda-goto))
  (when (eq major-mode 'org-mode)       ; do this only in org-mode buffers
    (setq mytmphead (nth 4 (org-heading-components)))
    (setq mytmpid (funcall 'org-id-get-create))
    (setq mytmplink (format "[[id:%s][%s]]" mytmpid mytmphead))
    (kill-new mytmplink)
    (message "Copied %s to killring (clipboard)" mytmplink)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Agenda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-agenda-files '(
                         "~/org/roam/gtd/inbox.org"
                         "~/org/roam/gtd/todo.org"
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

;; Log done date
(setq org-log-done t)

;; Non-nil means switching todo states with S-cursor counts as state change.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; Warning 30 days before deadline
(setq org-deadline-warning-days 7)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Refile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-refile-use-outline-path 'file)

(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-outline-path-complete-in-steps nil)

(setq org-refile-targets
      '((org-agenda-files . (:maxlevel . 4))))

;; Exclude completed tasks from refile targets
;; https://michael.englehorn.com/config.html
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-capture-templates nil)

;; Quick capture todo item to inbox
(add-to-list 'org-capture-templates
             '("t" "Todo" entry (file "~/org/roam/gtd/inbox.org")
               (file "~/.doom.d/templates/new-todo-template.org")))

;; Protocal
(add-to-list 'org-capture-templates '("!" "Protocal"))
(add-to-list 'org-capture-templates
             '("!h" "Highlight" entry (file "~/org/roam/gtd/inbox.org")
               "* 摘录：%:description\n%:link\n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n" :immediate-finish t))
(add-to-list 'org-capture-templates
             '("!l" "Link" entry (file "~/org/roam/gtd/inbox.org")
               "* TODO %:description\nCaptured On: %U\n\n%:link" :immediate-finish t))
(add-to-list 'org-capture-templates
             '("!t" "Quick Capture" entry (file "~/org/roam/gtd/inbox.org")
               (file "~/.doom.d/templates/new-quick-capture-template.org") :immediate-finish t))

;; Quick note for clocking item
(add-to-list 'org-capture-templates
             '("q" "Quick note for clocking item" item (clock)
               "Note taken on %U \\\ \n%?" :prepend t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Include current clocking item
(setq org-clock-report-include-clocking-task t)

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

;; Remove empty logbook drawers
;; https://michael.englehorn.com/config.html
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;; Add an effort estimate on the fly when clocking in
(add-hook 'org-clock-in-prepare-hook
          'my-org-mode-ask-effort)

(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Column
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%64ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %16TIMESTAMP_IA")


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
