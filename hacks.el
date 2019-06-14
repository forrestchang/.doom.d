;;; ~/.doom.d/hacks.el -*- lexical-binding: t; -*-


;; Don't update other todo items headlines when refresh checkbox state
;; org-list.el
;; TODO: fix it in org-mode source code
(defun org-reset-checkbox-state-subtree ()
  "Reset all checkboxes in an entry subtree."
  (interactive "*")
  (if (org-before-first-heading-p)
      (error "Not inside a tree")
    (save-restriction
      (save-excursion
	(org-narrow-to-subtree)
	(org-show-subtree)
	(goto-char (point-min))
	(let ((end (point-max)))
	  (while (< (point) end)
	    (when (org-at-item-checkbox-p)
	      (replace-match "[ ]" t t nil 1))
	    (beginning-of-line 2)))
	(org-update-checkbox-count-maybe)))))
