;;; my/tasks/plan.el -*- lexical-binding: t; -*-
;; PLAN.md: what the agent's interview settled, approved by you before any code is written.

(defconst +tasks-plan-file-name "PLAN.md")

(defconst +tasks--plan-approved-regexp "^Approved: ")

(defun +tasks--plan-file (worktree)
  (and worktree (expand-file-name +tasks-plan-file-name worktree)))

(defun +tasks--plan-text (worktree)
  (when-let* ((file (+tasks--plan-file worktree))
              ((file-exists-p file)))
    (+tasks--read-file file)))

(defun +tasks--plan-approved-p (text)
  (and text (string-match-p +tasks--plan-approved-regexp text) t))

(defun +tasks--plan-mark (text)
  "What a PLAN task waits for: the interview to write the plan, or you to approve it."
  (cond ((null text) "plan?")
        ((not (+tasks--plan-approved-p text)) "approve!")))

(defun +tasks--plan-mark-at-point ()
  (when (equal (org-get-todo-state) "PLAN")
    (+tasks--plan-mark
     (+tasks--plan-text (+tasks--branch-worktree-in default-directory (org-entry-get nil "BRANCH"))))))
