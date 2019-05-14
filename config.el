;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Org mode settings

(setq org-agenda-files '("~/Dropbox/personal-site/workspace/org/"))

(setq org-agenda-span 'day)

(setq org-log-into-drawer t)

;; Org refile
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
      '((org-agenda-files . (:maxlevel . 4))))

;; Org headlines style
(after! org
  (dolist (face '(org-level-1
                  org-level-2 org-level-3
                  org-level-4 org-level-5
                  org-level-6 org-level-7
                  org-level-8))
    (set-face-attribute face nil :weight 'normal))
  )
