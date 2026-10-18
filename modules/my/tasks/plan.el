;;; my/tasks/plan.el -*- lexical-binding: t; -*-
;; PLAN.md: what the agent's interview settled, approved by you before any code is written.

(defconst +tasks-plan-file-name "PLAN.md")

(defconst +tasks--plan-headings
  '("## Decisions" "## Tests, in order" "## Functions" "## Touches guarded files" "## Open")
  "The sections a PLAN.md must carry, in order; the parser and the prompt both read this.")

(defconst +tasks--plan-required-lines (append '("# Plan:" "ACCEPT:") +tasks--plan-headings))

(defconst +tasks--plan-accept-regexp "^ACCEPT: .*\n")

(defconst +tasks--plan-approved-regexp
  (concat +tasks--plan-accept-regexp "Approved: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[ \t]*$")
  "The approval as Emacs writes it, right under ACCEPT; a stray Approved elsewhere is the agent's, not yours.")

(defconst +tasks--plan-go-line
  "Plan approved: PLAN.md carries the Approved line. Start with its first test; any decision it does not settle goes through the grilling interview first.")

(defconst +tasks--plan-reread-line
  "PLAN.md edited; read it again and tell me if anything no longer holds, then wait.")

(defconst +tasks--plan-refusal-draft "Plan not approved: ")

;;; the interview s starts, and the plan it asks for

(defconst +tasks--plan-section-bodies
  '(("## Decisions" . "- Q: <question as asked> → A: <the user's answer>")
    ("## Tests, in order" . "- [ ] `<test name>`: <X> should <Y>")
    ("## Functions" . "- `<name>` in `<file>`: <one line>")
    ("## Touches guarded files" . "- none | <paths>")
    ("## Open" . "")))

(defun +tasks--plan-section-format (heading)
  (string-trim-right (concat heading "\n" (alist-get heading +tasks--plan-section-bodies nil nil #'equal))))

(defun +tasks--plan-format ()
  "The PLAN.md shape the interview must write, built from the headings the parser checks."
  (string-join (append '("# Plan: <heading>" "ACCEPT: <the acceptance line>")
                       (mapcar #'+tasks--plan-section-format +tasks--plan-headings))
               "\n\n"))

(defun +tasks--interview-prompt (heading &optional accept parents)
  "What s puts in pi's input: the grilling skill on HEADING, then the plan to write when done."
  (concat (format "/skill:grilling %s" heading)
          (if parents (format "\n\nPART OF: %s" parents) "")
          (if accept (format "\n\nACCEPT: %s" accept) "")
          (format "\n\nWhen the frontier is empty, write %s at the repo root:\n\n%s\n\n"
                  +tasks-plan-file-name (+tasks--plan-format))
          (format "Leave \"%s\" bare when nothing is unresolved. Then say \"%s written; waiting for approval.\" and stop."
                  (car (last +tasks--plan-headings)) +tasks-plan-file-name)))

;;; what init offers a new project

(defconst +tasks--agents-md-text
  "# Working with agents here

## The plan gate

While `PLAN.md` exists at the repo root without an `Approved:` line, edit no
file other than `PLAN.md`. The human approves the plan from Emacs; the
`Approved:` line is the record.

## Decisions

Any decision this task forces that `PLAN.md` does not settle, during planning
or later, goes through the grilling interview
(`.agents/skills/grilling/SKILL.md`), never a guess. Append each answer to
`## Decisions` in `PLAN.md` before continuing, in the same `Q → A` form. Say
so when a round changes the test list.

There are no sub-agents here. Look facts up yourself with read, grep and
bash.

## PLAN.md

Once approved, work the tests under `## Tests, in order` one at a time: write
the next unticked one, run the tests, show the failure, make the smallest
change that passes, tick it. Finish with a summary of the diff and how
`ACCEPT` is met.
"
  "The AGENTS.md init offers: the gate and the standing rule only; style and the test command are the project's own.")

(defconst +tasks--skill-dir ".agents/skills/grilling/")

(defun +tasks--own-skill-dir ()
  (expand-file-name +tasks--skill-dir doom-user-dir))

(defun +tasks--offer-skill (root)
  (let ((target (expand-file-name +tasks--skill-dir root)))
    (when (and (not (file-exists-p target))
               (file-directory-p (+tasks--own-skill-dir))
               (y-or-n-p "Copy the grilling skill into this project, so s can interview you? "))
      (copy-directory (+tasks--own-skill-dir) target nil t t))))

(defun +tasks--offer-agents-md (root)
  (let ((target (expand-file-name "AGENTS.md" root)))
    (when (and (not (file-exists-p target))
               (y-or-n-p "Write an AGENTS.md with the plan gate and the interview rule? "))
      (with-temp-file target (insert +tasks--agents-md-text)))))

(defun +tasks--skill-missing-p (root worktree)
  "Whether WORKTREE, cut before the skill reached ROOT, lacks the grilling skill ROOT has."
  (and (file-directory-p (expand-file-name +tasks--skill-dir root))
       (not (file-directory-p (expand-file-name +tasks--skill-dir worktree)))))

(defun +tasks--ensure-skill (root worktree)
  "Offer to copy ROOT's grilling skill into WORKTREE when it has none; /skill:grilling resolves there."
  (when (and (+tasks--skill-missing-p root worktree)
             (y-or-n-p "This worktree predates the grilling skill; copy it in from the main checkout? "))
    (copy-directory (expand-file-name +tasks--skill-dir root)
                    (expand-file-name +tasks--skill-dir worktree) nil t t)))

(defun +tasks--offer-agent-inputs (root)
  "Ask, never silently, before giving a new project the skill and the AGENTS.md the flow expects."
  (+tasks--offer-skill root)
  (+tasks--offer-agents-md root))

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

(defun +tasks--line-regexp (line)
  "LINE on its own, trailing blanks aside; a title line (ending in a colon) may carry anything after it."
  (concat "^" (regexp-quote line) (if (string-suffix-p ":" line) "" "[ \t]*$")))

(defun +tasks--has-line-p (line text)
  (string-match-p (+tasks--line-regexp line) text))

(defun +tasks--plan-missing-heading (text)
  "The first required line PLAN.md TEXT lacks, else nil."
  (seq-find (lambda (line) (not (+tasks--has-line-p line text))) +tasks--plan-required-lines))

;;; which model runs which stage

(defun +tasks--work-model (task)
  (or (plist-get task :model) "local"))

(defun +tasks--interview-model (task)
  "The model that runs the interview: :PLAN_MODEL:, else the work model."
  (or (plist-get task :plan-model) (+tasks--work-model task)))

(defun +tasks--session-model (task plan-text)
  "The model a session of TASK runs on now: the interview's until PLAN-TEXT is approved, the work model after."
  (if (+tasks--plan-approved-p plan-text) (+tasks--work-model task) (+tasks--interview-model task)))

(defun +tasks--model-names ()
  (mapcar #'car (bound-and-true-p +ai-pi-model-args)))

(defun +tasks--read-plan-model ()
  (completing-read "Model for the interview: " (+tasks--model-names) nil t))

(defun +tasks--check-models (&rest models)
  "Refuse a model `+ai-pi-model-args' has no flags for, before any review or worktree."
  (when (fboundp '+ai--check-pi-model)
    (mapc #'+ai--check-pi-model models)))

;;; approving

(defun +tasks--plan-with-approval (text date)
  "TEXT with Approved: DATE under its first ACCEPT line; unchanged when already approved or without one."
  (if (or (+tasks--plan-approved-p text) (not (string-match +tasks--plan-accept-regexp text)))
      text
    (concat (substring text 0 (match-end 0)) "Approved: " date "\n" (substring text (match-end 0)))))

(defun +tasks--approved-plan (text date)
  (let ((approved (+tasks--plan-with-approval text date)))
    (unless (+tasks--plan-approved-p approved)
      (user-error "%s has no \"ACCEPT: <line>\" to write the approval under" +tasks-plan-file-name))
    approved))

(defun +tasks--write-plan (worktree text)
  (with-temp-file (+tasks--plan-file worktree) (insert text))
  (when-let* ((buffer (get-file-buffer (+tasks--plan-file worktree))))
    (with-current-buffer buffer (revert-buffer t t))))

(defun +tasks--refuse-unsaved-plan (worktree)
  (when-let* ((buffer (get-file-buffer (+tasks--plan-file worktree)))
              ((buffer-modified-p buffer)))
    (user-error "%s has unsaved edits; save them first (they would be lost)" +tasks-plan-file-name)))

(defun +tasks--send-to-session (task line)
  "Send LINE to TASK's pi session, run from its worktree so pilish finds the right one."
  (let ((default-directory (+tasks--task-worktree-of task)))
    (unless (and (fboundp '+ai--send-pilish-input)
                 (+ai--send-pilish-input line (plist-get task :branch)))
      (message "No pi session took this; resume the task and send it yourself: %s" line))))

(defun +tasks--prefill-session (task draft)
  (when (fboundp '+ai--prefill-pilish-input)
    (let ((default-directory (+tasks--task-worktree-of task)))
      (+ai--prefill-pilish-input draft (plist-get task :branch)))))

(defun +tasks--restart-on-work-model (task worktree)
  "Trade the interview's session for one on the work model, which sends the go line once it is up."
  (when (fboundp '+ai--stop-pilish-session)
    (+ai--stop-pilish-session worktree (plist-get task :branch)))
  (+tasks--hand-off worktree
                    :review-root (+tasks--task-root task)
                    :draft +tasks--plan-go-line
                    :send t
                    :session (plist-get task :branch)
                    :model (+tasks--work-model task)))

(defun +tasks--go (task worktree)
  (if (equal (+tasks--work-model task) (+tasks--interview-model task))
      (+tasks--send-to-session task +tasks--plan-go-line)
    (+tasks--restart-on-work-model task worktree)))

(defun +tasks--approve (task worktree)
  "Date the plan, move TASK to RED and tell pi to start on the first test."
  (+tasks--refuse-unsaved-plan worktree)
  (+tasks--write-plan worktree (+tasks--approved-plan (+tasks--plan-text worktree)
                                                      (format-time-string "%Y-%m-%d")))
  (+tasks--set-state task "RED")
  (+tasks--go task worktree))

(defun +tasks--plan-buffer (worktree)
  "PLAN.md visited as text: the agent wrote it, so none of its local variables run."
  (let ((enable-local-variables nil)
        (enable-dir-local-variables nil))
    (find-file-noselect (+tasks--plan-file worktree))))

(defun +tasks--show-plan (worktree)
  (let ((buffer (+tasks--plan-buffer worktree)))
    (with-current-buffer buffer (read-only-mode 1))
    (display-buffer buffer '(display-buffer-in-side-window (side . right)))))

(defun +tasks--read-approval ()
  (read-char-choice "Approve this plan? (y, n, e to edit) " '(?y ?n ?e)))

(defvar-local +tasks--plan-task nil
  "The task whose PLAN.md this buffer edits.")

(defun +tasks--send-reread-h ()
  (+tasks--send-to-session +tasks--plan-task +tasks--plan-reread-line))

(defun +tasks--edit-plan (task worktree)
  "Open PLAN.md writable; each save asks pi to read it again."
  (let ((buffer (+tasks--plan-buffer worktree)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (setq +tasks--plan-task task)
      (add-hook 'after-save-hook #'+tasks--send-reread-h nil t))
    (pop-to-buffer buffer)))

(defun +tasks--refuse-plan (task)
  (+tasks--resume-task task)
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
        (progn (+tasks--resume-task task)
               (message "No %s yet; the interview is still running" +tasks-plan-file-name))
      (+tasks--check-plan text)
      (+tasks--show-plan worktree)
      (+tasks--decide-plan task worktree (+tasks--read-approval)))))

;;; what review and done read from the plan

(defun +tasks--plan-section (heading text)
  "The lines of TEXT under HEADING, up to the next heading."
  (when text
    (let ((lines (split-string text "\n"))
          in-section result)
      (dolist (line lines (nreverse result))
        (cond ((string-match-p (+tasks--line-regexp heading) line) (setq in-section t))
              ((string-prefix-p "#" line) (setq in-section nil))
              (in-section (push line result)))))))

(defun +tasks--plan-test-lines (text)
  (seq-filter (apply-partially #'string-match-p "\\`- \\[[ xX]\\]")
              (+tasks--plan-section "## Tests, in order" text)))

(defun +tasks--plan-tick-count (text)
  "(TICKED . TOTAL) over the test boxes of the plan."
  (let ((tests (+tasks--plan-test-lines text)))
    (cons (seq-count (apply-partially #'string-match-p "\\`- \\[[xX]\\]") tests)
          (length tests))))

(defun +tasks--plan-open-items (text)
  (seq-remove #'string-blank-p (+tasks--plan-section "## Open" text)))

(defun +tasks--plan-tick-line (text)
  (when text
    (pcase-let ((`(,ticked . ,total) (+tasks--plan-tick-count text)))
      (format "plan: %d/%d tests ticked" ticked total))))

(defun +tasks--refuse-open-items (text)
  "Stop done while the plan's Open section still has a bullet."
  (when-let* ((items (+tasks--plan-open-items text)))
    (user-error "%s still has open items; settle them, or C-u to merge anyway:\n%s"
                +tasks-plan-file-name (string-join items "\n"))))

(defun +tasks--unparked-state (task)
  "RED when TASK's plan is approved, else PLAN: the interview, or its approval, is still due."
  (if (+tasks--plan-approved-p (+tasks--plan-text (+tasks--task-worktree-of task))) "RED" "PLAN"))
