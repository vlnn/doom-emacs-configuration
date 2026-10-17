;;; my/tasks/plan.el -*- lexical-binding: t; -*-
;; PLAN.md: what the agent's interview settled, approved by you before any code is written.

(defconst +tasks-plan-file-name "PLAN.md")

(defconst +tasks--plan-headings
  '("## Decisions" "## Tests, in order" "## Functions" "## Touches guarded files" "## Open")
  "The sections a PLAN.md must carry, in order; the parser and the prompt both read this.")

(defconst +tasks--plan-required-lines (cons "ACCEPT:" +tasks--plan-headings))

(defconst +tasks--plan-approved-regexp "^Approved: ")

(defconst +tasks--plan-go-line
  "Plan approved: PLAN.md carries the Approved line. Start with its first test; any decision it does not settle goes through the grilling interview first.")

(defconst +tasks--plan-reread-line
  "PLAN.md edited; read it again and tell me if anything no longer holds, then wait.")

(defconst +tasks--plan-refusal-draft "Plan not approved: ")

;;; reading the plan

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
    (+tasks--plan-mark (+tasks--plan-text (+tasks--worktree-at-point)))))

(defun +tasks--worktree-at-point ()
  (+tasks--branch-worktree-in default-directory (org-entry-get nil "BRANCH")))

(defun +tasks--has-line-p (prefix text)
  (string-match-p (concat "^" (regexp-quote prefix)) text))

(defun +tasks--plan-missing-heading (text)
  "The first required line PLAN.md TEXT lacks, else nil."
  (seq-find (lambda (line) (not (+tasks--has-line-p line text))) +tasks--plan-required-lines))

;;; approving

(defun +tasks--plan-with-approval (text date)
  (replace-regexp-in-string "^\\(ACCEPT: .*\n\\)" (concat "\\1Approved: " date "\n") text t))

(defun +tasks--write-plan (worktree text)
  (with-temp-file (+tasks--plan-file worktree) (insert text))
  (when-let* ((buffer (get-file-buffer (+tasks--plan-file worktree))))
    (with-current-buffer buffer (revert-buffer t t))))

(defun +tasks--send-to-session (task line)
  "Send LINE to TASK's pi session, run from its worktree so pilish finds the right one."
  (when (fboundp '+ai--send-pilish-input)
    (let ((default-directory (+tasks--task-worktree-of task)))
      (+ai--send-pilish-input line (plist-get task :branch)))))

(defun +tasks--prefill-session (task draft)
  (when (fboundp '+ai--prefill-pilish-input)
    (let ((default-directory (+tasks--task-worktree-of task)))
      (+ai--prefill-pilish-input draft (plist-get task :branch)))))

(defun +tasks--approve (task worktree)
  "Date the plan, move TASK to RED and tell pi to start on the first test."
  (+tasks--write-plan worktree (+tasks--plan-with-approval (+tasks--plan-text worktree)
                                                           (format-time-string "%Y-%m-%d")))
  (+tasks--set-state task "RED")
  (+tasks--send-to-session task +tasks--plan-go-line))

(defun +tasks--plan-buffer (worktree)
  (find-file-noselect (+tasks--plan-file worktree)))

(defun +tasks--show-plan (worktree)
  (let ((buffer (+tasks--plan-buffer worktree)))
    (with-current-buffer buffer (read-only-mode 1))
    (display-buffer buffer '(display-buffer-in-side-window (side . right)))))

(defun +tasks--read-approval ()
  (read-char-choice "Approve this plan? (y, n, e to edit) " '(?y ?n ?e)))

(defun +tasks--send-reread-on-save (task)
  (lambda () (+tasks--send-to-session task +tasks--plan-reread-line)))

(defun +tasks--edit-plan (task worktree)
  "Open PLAN.md writable; each save asks pi to read it again."
  (let ((buffer (+tasks--plan-buffer worktree)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (add-hook 'after-save-hook (+tasks--send-reread-on-save task) nil t))
    (pop-to-buffer buffer)))

(defun +tasks--refuse-plan (task)
  (+tasks/resume)
  (+tasks--prefill-session task +tasks--plan-refusal-draft))

(defun +tasks--decide-plan (task worktree answer)
  (pcase answer
    (?y (+tasks--approve task worktree))
    (?n (+tasks--refuse-plan task))
    (?e (+tasks--edit-plan task worktree))))

(defun +tasks--check-plan (text)
  (when-let* ((missing (+tasks--plan-missing-heading text)))
    (user-error "%s has no \"%s\"" +tasks-plan-file-name missing)))

(defun +tasks/approve-plan ()
  "Read the task's PLAN.md and approve it (y), refuse it (n) or edit it (e).
Without one, the interview is still running: go back to it."
  (interactive)
  (let* ((task (+tasks--work-task))
         (worktree (+tasks--task-worktree-of task))
         (text (+tasks--plan-text worktree)))
    (if (null text)
        (progn (+tasks/resume)
               (message "No %s yet; the interview is still running" +tasks-plan-file-name))
      (+tasks--check-plan text)
      (+tasks--show-plan worktree)
      (+tasks--decide-plan task worktree (+tasks--read-approval)))))

(defun +tasks--unparked-state (task)
  "RED when TASK's plan is approved, else PLAN: the interview, or its approval, is still due."
  (if (+tasks--plan-approved-p (+tasks--plan-text (+tasks--task-worktree-of task))) "RED" "PLAN"))
