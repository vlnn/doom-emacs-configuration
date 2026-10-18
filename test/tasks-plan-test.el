;;; test/tasks-plan-test.el -*- lexical-binding: t; -*-

(defconst +tasks-plan-test--written
  "# Plan: Fix it

ACCEPT: it works

## Decisions
- Q: where? → A: here

## Tests, in order
- [x] `a/one`: a should one
- [ ] `a/two`: a should two

## Functions
- `+a--one` in `a.el`: does one

## Touches guarded files
- none

## Open
")

(defconst +tasks-plan-test--approved
  (replace-regexp-in-string "^ACCEPT: it works\n" "ACCEPT: it works\nApproved: 2026-10-17\n" +tasks-plan-test--written))

(defmacro +tasks-plan-test--with-worktree (plan-text &rest body)
  "Run BODY with `tree' bound to a temp dir holding PLAN.md with PLAN-TEXT, or nothing when nil."
  (declare (indent 1))
  `(let ((tree (file-name-as-directory (file-truename (make-temp-file "tasks-plan-test" t)))))
     (unwind-protect
         (progn
           (when ,plan-text
             (with-temp-file (expand-file-name "PLAN.md" tree) (insert ,plan-text)))
           ,@body)
       (delete-directory tree t))))

(ert-deftest +tasks--excluded-files/keeps-the-plan-out-of-git ()
  (ert-info ("the plan should be excluded from git, since it lives in the worktree and is not part of the change")
    (should (member "/PLAN.md" (+tasks--excluded-files)))))

(ert-deftest +tasks--plan-file/sits-at-the-worktree-root ()
  (ert-info ("plan-file should be PLAN.md at the root of the given worktree")
    (should (equal "/r/.worktrees/feat-x/PLAN.md" (+tasks--plan-file "/r/.worktrees/feat-x/"))))
  (ert-info ("plan-file should be nil for a task without a worktree")
    (should-not (+tasks--plan-file nil))))

(ert-deftest +tasks--plan-text/reads-the-plan-when-there-is-one ()
  (+tasks-plan-test--with-worktree +tasks-plan-test--written
    (ert-info ("plan-text should read PLAN.md from the worktree")
      (should (equal +tasks-plan-test--written (+tasks--plan-text tree)))))
  (+tasks-plan-test--with-worktree nil
    (ert-info ("plan-text should be nil while the interview has not written the file")
      (should-not (+tasks--plan-text tree))))
  (ert-info ("plan-text should be nil without a worktree")
    (should-not (+tasks--plan-text nil))))

(ert-deftest +tasks--plan-approved-p/reads-the-approved-line ()
  (pcase-dolist (`(,text ,expected ,why) `((,+tasks-plan-test--approved t "accept a plan carrying the Approved line")
                                           (,+tasks-plan-test--written nil "refuse a plan without it")
                                           ("# Plan\n\nApproved by nobody\n" nil "want the exact Approved: line, not the word")
                                           (nil nil "be nil without a plan")))
    (ert-info ((format "plan-approved-p should %s" why))
      (should (eq expected (+tasks--plan-approved-p text))))))

(ert-deftest +tasks--plan-mark/says-what-the-plan-waits-for ()
  (pcase-dolist (`(,text . ,expected) `((nil . "plan?")
                                        (,+tasks-plan-test--written . "approve!")
                                        (,+tasks-plan-test--approved . nil)))
    (ert-info ((format "plan-mark for %s should be %S" (if text "a written plan" "no plan") expected))
      (should (equal expected (+tasks--plan-mark text))))))

(ert-deftest +tasks--agenda-prefix/shows-plan-marks-instead-of-the-branch ()
  (+tasks-plan-test--with-worktree +tasks-plan-test--written
    (+tasks-test--in-tasks-buffer "** PLAN Written\n:PROPERTIES:\n:BRANCH: feat/written\n:END:\n** PLAN Unwritten\n:PROPERTIES:\n:BRANCH: feat/unwritten\n:END:\n** RED Working\n:PROPERTIES:\n:BRANCH: feat/working\n:END:\n"
      (cl-letf (((symbol-function '+tasks--branch-worktree-in)
                 (lambda (_root branch) (and (equal branch "feat/written") tree))))
        (pcase-dolist (`(,heading . ,expected) '(("Written" . "approve!") ("Unwritten" . "plan?") ("Working" . "feat/working")))
          (+tasks-test--goto-heading heading)
          (ert-info ((format "the board prefix of %s should read %S" heading expected))
            (should (equal expected (+tasks--agenda-prefix)))))))))

;;; approving a plan

(ert-deftest +tasks--next-step/plan-asks-for-approval-not-tests ()
  (ert-info ("next on a PLAN task should offer the plan for approval; there are no tests to run yet")
    (should (eq '+tasks/approve-plan (+tasks--next-step "PLAN"))))
  (ert-info ("next on a RED task should still run the tests")
    (should (eq '+tasks/test (+tasks--next-step "RED")))))

(ert-deftest +tasks--plan-missing-heading/names-the-first-absent-section ()
  (pcase-dolist (`(,text . ,expected)
                 `((,+tasks-plan-test--written . nil)
                   (,(replace-regexp-in-string "^## Open\n" "" +tasks-plan-test--written) . "## Open")
                   (,(replace-regexp-in-string "^## Tests, in order\n\\|^## Functions\n" "" +tasks-plan-test--written) . "## Tests, in order")
                   (,(replace-regexp-in-string "^ACCEPT: .*\n" "" +tasks-plan-test--written) . "ACCEPT:")
                   ("# Plan\n" . "# Plan:")))
    (ert-info ((format "plan-missing-heading should be %S" expected))
      (should (equal expected (+tasks--plan-missing-heading text))))))

(ert-deftest +tasks--plan-with-approval/dates-the-plan-under-accept ()
  (ert-info ("approving should add the Approved line right after the ACCEPT line, once")
    (should (equal +tasks-plan-test--approved
                   (+tasks--plan-with-approval +tasks-plan-test--written "2026-10-17")))))

(defmacro +tasks-plan-test--approving (plan-text answer &rest body)
  "Run +tasks/approve-plan on a PLAN task whose worktree holds PLAN-TEXT, answering ANSWER.
BODY sees `tree', `tasks' (the task buffer), `sent' (lines sent to pi), `prefilled' and `resumed'."
  (declare (indent 2))
  `(+tasks-plan-test--with-worktree ,plan-text
     (+tasks-test--in-tasks-buffer "** PLAN Fix it\n:PROPERTIES:\n:BRANCH: feat/fix-it\n:MODEL: local\n:END:\n"
       (+tasks-test--goto-heading "Fix it")
       (let ((task (+tasks--task-plist))
             (tasks (current-buffer))
             sent prefilled resumed)
         (cl-letf (((symbol-function '+tasks--work-task) (lambda () task))
                   ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) tree))
                   ((symbol-function '+tasks--read-approval) (lambda () ,answer))
                   ((symbol-function '+tasks--show-plan) #'ignore)
                   ((symbol-function '+tasks--resume-task) (lambda (_) (setq resumed t)))
                   ((symbol-function '+ai--send-pilish-input) (lambda (line &optional _) (push line sent)))
                   ((symbol-function '+ai--prefill-pilish-input) (lambda (line &optional _) (setq prefilled line)))
                   ((symbol-function 'message) #'ignore))
           (+tasks/approve-plan)
           (with-current-buffer tasks
             ,@body))))))

(ert-deftest +tasks/approve-plan/resumes-when-the-plan-is-not-written-yet ()
  (+tasks-plan-test--approving nil ?y
    (ert-info ("approve-plan without PLAN.md should take you back to the running interview")
      (should resumed))
    (ert-info ("approve-plan without PLAN.md should neither approve nor send anything")
      (should (equal '("PLAN" nil) (+tasks-test--state-of (current-buffer) "Fix it")))
      (should-not sent))))

(ert-deftest +tasks/approve-plan/refuses-a-plan-without-the-fixed-headings ()
  (+tasks-plan-test--with-worktree (replace-regexp-in-string "^## Open\n" "" +tasks-plan-test--written)
    (+tasks-test--in-tasks-buffer "** PLAN Fix it\n:PROPERTIES:\n:BRANCH: feat/fix-it\n:END:\n"
      (+tasks-test--goto-heading "Fix it")
      (let ((task (+tasks--task-plist)))
        (cl-letf (((symbol-function '+tasks--work-task) (lambda () task))
                  ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) tree))
                  ((symbol-function '+tasks--read-approval) (lambda () (error "Should not ask"))))
          (ert-info ("approve-plan should stop, naming the missing section, before asking anything")
            (should (string-match-p "## Open" (cadr (should-error (+tasks/approve-plan) :type 'user-error))))))))))

(ert-deftest +tasks/approve-plan/approves-into-red-and-sends-the-go-line ()
  (+tasks-plan-test--approving +tasks-plan-test--written ?y
    (ert-info ("y should write the Approved line into PLAN.md")
      (should (+tasks--plan-approved-p (+tasks--plan-text tree))))
    (ert-info ("y should move the task to RED")
      (should (equal '("RED" nil) (+tasks-test--state-of (current-buffer) "Fix it"))))
    (ert-info ("y should tell pi to start with the first test, naming the file that carries the approval")
      (should (= 1 (length sent)))
      (should (string-match-p "Plan approved: PLAN.md carries the Approved line" (car sent)))
      (should (string-match-p "grilling interview" (car sent))))))

(ert-deftest +tasks/approve-plan/prefills-a-refusal ()
  (+tasks-plan-test--approving +tasks-plan-test--written ?n
    (ert-info ("n should leave the task in PLAN and the plan unapproved")
      (should (equal '("PLAN" nil) (+tasks-test--state-of (current-buffer) "Fix it")))
      (should-not (+tasks--plan-approved-p (+tasks--plan-text tree))))
    (ert-info ("n should prefill the refusal for you to finish, sending nothing")
      (should (equal "Plan not approved: " prefilled))
      (should-not sent))))

(ert-deftest +tasks/approve-plan/sends-the-reread-line-after-an-edit ()
  (+tasks-plan-test--approving +tasks-plan-test--written ?e
    (let ((plan (get-file-buffer (+tasks--plan-file tree))))
      (unwind-protect
          (progn
            (ert-info ("e should open PLAN.md for editing")
              (should plan)
              (should-not (buffer-local-value 'buffer-read-only plan)))
            (with-current-buffer plan
              (goto-char (point-max))
              (insert "- a change\n")
              (save-buffer))
            (ert-info ("saving the edited plan should ask pi to read it again and wait")
              (should (equal 1 (length sent)))
              (should (string-match-p "PLAN.md edited; read it again" (car sent))))
            (ert-info ("e should leave the task in PLAN, unapproved")
              (should (equal '("PLAN" nil) (+tasks-test--state-of tasks "Fix it")))))
        (when plan (kill-buffer plan))))))

(ert-deftest +tasks/unpark/returns-to-red-when-the-plan-is-approved ()
  (pcase-dolist (`(,plan-text . ,expected) `((,+tasks-plan-test--approved . "RED")
                                             (,+tasks-plan-test--written . "PLAN")
                                             (nil . "PLAN")))
    (+tasks-plan-test--with-worktree plan-text
      (+tasks-test--in-tasks-buffer "** WAIT Thing\n:PROPERTIES:\n:BRANCH: feat/thing\n:REASON: API key\n:END:\n"
        (+tasks-test--goto-heading "Thing")
        (cl-letf (((symbol-function '+tasks--work-task) (let ((task (+tasks--task-plist))) (lambda () task)))
                  ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) tree))
                  ((symbol-function '+tasks/resume) #'ignore))
          (+tasks/unpark))
        (ert-info ((format "unpark should return to %s when the plan is %s" expected
                           (cond ((null plan-text) "not written") ((equal expected "RED") "approved") (t "unapproved"))))
          (should (equal (list expected nil) (+tasks-test--state-of (current-buffer) "Thing"))))))))

;;; review and done read the plan

(ert-deftest +tasks--plan-tick-count/counts-checked-tests ()
  (pcase-dolist (`(,text . ,expected) `((,+tasks-plan-test--written . (1 . 2))
                                        (,(concat +tasks-plan-test--written "- [ ] an open item\n") . (1 . 2))
                                        ("# Plan\n\n## Tests, in order\n\n## Open\n" . (0 . 0))))
    (ert-info ((format "tick-count should be %S, counting only the boxes under Tests" expected))
      (should (equal expected (+tasks--plan-tick-count text))))))

(ert-deftest +tasks--plan-open-items/lists-the-bullets-under-open ()
  (ert-info ("open-items should be empty for a plan whose Open section has no bullet")
    (should-not (+tasks--plan-open-items +tasks-plan-test--written)))
  (ert-info ("open-items should list each bullet under Open, and nothing from other sections")
    (should (equal '("- which backend?" "- naming")
                   (+tasks--plan-open-items (concat +tasks-plan-test--written "- which backend?\n- naming\n")))))
  (ert-info ("open-items should be nil without a plan")
    (should-not (+tasks--plan-open-items nil))))

(ert-deftest +tasks--review-prompt/carries-the-plan-when-there-is-one ()
  (let ((with-plan (+tasks--review-prompt "it works" "diff --git a/x b/x" +tasks-plan-test--written))
        (without (+tasks--review-prompt "it works" "diff --git a/x b/x" nil)))
    (ert-info ("review-prompt should put the plan the agent committed to before the diff")
      (should (< (string-search "Plan the agent committed to:" with-plan)
                 (string-search "## Decisions" with-plan)
                 (string-search "diff --git" with-plan))))
    (ert-info ("review-prompt with a plan should ask about departures from it")
      (should (string-search "departs from the plan" with-plan)))
    (ert-info ("review-prompt without a plan should read as before")
      (should-not (string-search "Plan the agent committed to:" without))
      (should (string-search "diff --git a/x b/x" without)))))

(ert-deftest +tasks--review-message/adds-the-tick-count-after-the-guard-note ()
  (ert-info ("review-message should end with the tick count when there is a plan")
    (should (equal "ACCEPT: it works\nTouches agent guards: AGENTS.md\nplan: 1/2 tests ticked"
                   (+tasks--review-message "it works" "Touches agent guards: AGENTS.md" +tasks-plan-test--written))))
  (ert-info ("review-message should say nothing about a plan when there is none")
    (should (equal "ACCEPT: it works" (+tasks--review-message "it works" nil nil)))))

(defmacro +tasks-plan-test--done-with-plan (plan-text force &rest body)
  "Run (+tasks/done FORCE) on feat/x, whose worktree holds PLAN-TEXT; BODY sees `repo', `tree', `buffer'."
  (declare (indent 2))
  `(+tasks-test--in-git-repo
    (let* ((repo default-directory)
           (tree (+tasks--add-worktree repo "feat/x"))
           (buffer (find-file-noselect (expand-file-name "tasks.org" repo))))
      (unwind-protect
          (progn
            (let ((default-directory tree))
              (+tasks-test--commit-file "feature.txt" "done"))
            (with-temp-file (expand-file-name "PLAN.md" tree) (insert ,plan-text))
            (+tasks-test--write-active-task buffer "Ship it" "feat/x")
            (cl-letf (((symbol-function '+tasks--work-task) (lambda () (+tasks-test--task-in buffer "Ship it")))
                      ((symbol-function '+tasks--project-test-command) (lambda (_) "true"))
                      ((symbol-function 'message) #'ignore))
              ,@body))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest +tasks/done/refuses-while-the-plan-has-open-items ()
  (+tasks-plan-test--done-with-plan (concat +tasks-plan-test--written "- which backend?\n") nil
    (ert-info ("done should stop, listing the open items, while the plan's Open section has a bullet")
      (should (string-search "which backend?" (cadr (should-error (+tasks/done) :type 'user-error)))))
    (ert-info ("a refused done should merge nothing")
      (should-not (file-exists-p (expand-file-name "feature.txt" repo))))))

(ert-deftest +tasks/done/merges-with-a-prefix-arg-despite-open-items ()
  (+tasks-plan-test--done-with-plan (concat +tasks-plan-test--written "- which backend?\n") t
    (+tasks/done t)
    (ert-info ("done with a prefix argument should merge even with open items")
      (should (file-exists-p (expand-file-name "feature.txt" repo))))))

(ert-deftest +tasks/done/merges-a-planned-branch-with-nothing-open ()
  (+tasks-plan-test--done-with-plan +tasks-plan-test--approved nil
    (+tasks/done)
    (ert-info ("done should merge a planned branch whose Open section is bare, and take PLAN.md away with the worktree")
      (should (file-exists-p (expand-file-name "feature.txt" repo)))
      (should-not (file-exists-p tree)))))

;;; the interview prompt and what init offers

(ert-deftest +tasks--plan-format/renders-every-required-section-in-order ()
  (let ((format (+tasks--plan-format)))
    (ert-info ("plan-format should start with the title line")
      (should (string-prefix-p "# Plan: <heading>\n\nACCEPT: <the acceptance line>" format)))
    (ert-info ("plan-format should list the sections the parser requires, in the parser's order")
      (should (equal +tasks--plan-headings
                     (seq-filter (apply-partially #'string-prefix-p "## ") (split-string format "\n")))))
    (ert-info ("a plan written to the format should pass the heading check")
      (should-not (+tasks--plan-missing-heading format)))))

(ert-deftest +tasks--interview-prompt/starts-the-skill-and-asks-for-the-plan ()
  (let ((prompt (+tasks--interview-prompt "Deep" "works" "Parent / Middle")))
    (ert-info ("interview-prompt should invoke the grilling skill on the heading, the one route pi honours")
      (should (string-prefix-p "/skill:grilling Deep\n\nPART OF: Parent / Middle\n\nACCEPT: works\n\n" prompt)))
    (ert-info ("interview-prompt should ask for PLAN.md in the fixed format once the frontier is empty")
      (should (string-search "When the frontier is empty, write PLAN.md at the repo root" prompt))
      (should (string-search (+tasks--plan-format) prompt)))
    (ert-info ("interview-prompt should end by telling pi to stop and wait")
      (should (string-suffix-p "Then say \"PLAN.md written; waiting for approval.\" and stop." prompt)))))

(ert-deftest +tasks--interview-prompt/leaves-out-what-a-task-does-not-have ()
  (let ((prompt (+tasks--interview-prompt "Deep" nil nil)))
    (ert-info ("interview-prompt should say nothing about parents or ACCEPT when there are none")
      (should-not (string-search "PART OF" prompt))
      (should-not (string-search "ACCEPT:" (car (split-string prompt "When the frontier")))))))

(ert-deftest +tasks/start/drafts-the-interview ()
  (let (handed)
    (+tasks-test--in-tasks-buffer "** IDEA Fix it\n"
      (setq default-directory (file-name-as-directory (file-truename temporary-file-directory)))
      (+tasks-test--goto-heading "Fix it")
      (cl-letf (((symbol-function '+tasks--task-to-start) (lambda () (cons (current-buffer) (line-beginning-position))))
                ((symbol-function 'read-string) (lambda (_prompt &optional initial &rest _) (or initial "it works")))
                ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) nil))
                ((symbol-function '+tasks--hand-off) (lambda (root &rest args) (setq handed (cons root args)))))
        (+tasks/start)))
    (ert-info ("start should hand pi the interview, not the retired /feature template")
      (should (string-prefix-p "/skill:grilling Fix it\n\nACCEPT: it works" (plist-get (cdr handed) :draft))))))

(defmacro +tasks-plan-test--init-in-repo (answers &rest body)
  "Run +tasks/init in a fresh repo, answering y-or-n-p from ANSWERS by prompt substring; BODY sees `repo' and `asked'."
  (declare (indent 1))
  `(+tasks-test--in-git-repo
    (let ((repo default-directory)
          asked)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (push prompt asked)
                   (cdr (seq-find (lambda (a) (string-search (car a) prompt)) ,answers))))
                ((symbol-function 'find-file) #'ignore)
                ((symbol-function '+tasks--project-root) (lambda () repo)))
        (+tasks/init)
        ,@body))))

(ert-deftest +tasks/init/offers-the-grilling-skill-and-agents-md ()
  (+tasks-plan-test--init-in-repo '(("grilling" . t) ("AGENTS.md" . t))
    (ert-info ("init should ask before copying the skill and before writing AGENTS.md")
      (should (= 2 (length asked))))
    (ert-info ("a yes should copy the grilling skill into the new project")
      (should (file-exists-p (expand-file-name ".agents/skills/grilling/SKILL.md" repo))))
    (ert-info ("a yes should write an AGENTS.md holding the gate and the standing rule only")
      (let ((text (+tasks--read-file (expand-file-name "AGENTS.md" repo))))
        (should (string-search "without an `Approved:` line" text))
        (should (string-search "grilling interview" text))
        (should (string-search "no sub-agents" text))
        (should-not (string-search "bin/test" text))))))

(ert-deftest +tasks/init/writes-nothing-you-decline ()
  (+tasks-plan-test--init-in-repo '(("grilling" . nil) ("AGENTS.md" . nil))
    (ert-info ("init should still write tasks.org when the offers are declined")
      (should (file-exists-p (expand-file-name "tasks.org" repo))))
    (ert-info ("a no should leave the project without the skill and without AGENTS.md")
      (should-not (file-exists-p (expand-file-name ".agents" repo)))
      (should-not (file-exists-p (expand-file-name "AGENTS.md" repo))))))

(ert-deftest +tasks/init/does-not-offer-what-the-project-already-has ()
  (+tasks-test--in-git-repo
   (let ((repo default-directory) asked)
     (make-directory (expand-file-name ".agents/skills/grilling" repo) t)
     (with-temp-file (expand-file-name ".agents/skills/grilling/SKILL.md" repo) (insert "theirs"))
     (with-temp-file (expand-file-name "AGENTS.md" repo) (insert "theirs"))
     (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt asked) t))
               ((symbol-function 'find-file) #'ignore)
               ((symbol-function '+tasks--project-root) (lambda () repo)))
       (+tasks/init))
     (ert-info ("init should not ask about, or overwrite, a skill or AGENTS.md the project already has")
       (should-not asked)
       (should (equal "theirs" (+tasks--read-file (expand-file-name "AGENTS.md" repo))))))))

;;; a model per stage

(ert-deftest +tasks--skeleton/offers-a-plan-model-per-task ()
  (ert-info ("the skeleton should offer PLAN_MODEL values beside MODEL's")
    (should (string-search "#+PROPERTY: PLAN_MODEL_ALL local claude" (+tasks--skeleton)))))

(ert-deftest +tasks--task-plist/carries-the-plan-model ()
  (+tasks-test--in-tasks-buffer "** PLAN Thought out\n:PROPERTIES:\n:BRANCH: feat/t\n:MODEL: local\n:PLAN_MODEL: claude\n:END:\n"
    (+tasks-test--goto-heading "Thought out")
    (ert-info ("task-plist should carry :PLAN_MODEL: so the interview can run on a better model")
      (should (equal "claude" (plist-get (+tasks--task-plist) :plan-model))))))

(ert-deftest +tasks--interview-model/falls-back-to-the-work-model ()
  (pcase-dolist (`(,task . ,expected) '(((:model "local" :plan-model "claude") . "claude")
                                        ((:model "claude") . "claude")
                                        (() . "local")))
    (ert-info ((format "interview-model of %S should be %s" task expected))
      (should (equal expected (+tasks--interview-model task))))))

(ert-deftest +tasks--session-model/follows-the-approval ()
  (let ((task '(:model "local" :plan-model "claude")))
    (ert-info ("the session should run on the plan model while the plan is not approved")
      (should (equal "claude" (+tasks--session-model task +tasks-plan-test--written)))
      (should (equal "claude" (+tasks--session-model task nil))))
    (ert-info ("the session should run on the work model once the plan is approved")
      (should (equal "local" (+tasks--session-model task +tasks-plan-test--approved))))))

(defun +tasks-plan-test--start (prefix-arg)
  "Run (+tasks/start PREFIX-ARG) on a fresh IDEA with every prompt stubbed; return what was handed to pi."
  (let (handed)
    (+tasks-test--in-tasks-buffer "** IDEA Fix it\n"
      (setq default-directory (file-name-as-directory (file-truename temporary-file-directory)))
      (+tasks-test--goto-heading "Fix it")
      (cl-letf (((symbol-function '+tasks--task-to-start) (lambda () (cons (current-buffer) (line-beginning-position))))
                ((symbol-function 'read-string) (lambda (_prompt &optional initial &rest _) (or initial "it works")))
                ((symbol-function 'completing-read) (lambda (&rest _) "claude"))
                ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) nil))
                ((symbol-function '+tasks--add-worktree) #'ignore)
                ((symbol-function '+tasks--ensure-skill) #'ignore)
                ((symbol-function '+tasks/refresh-branches) #'ignore)
                ((symbol-function 'save-buffer) #'ignore)
                ((symbol-function '+tasks--hand-off)
                 (lambda (root &rest args) (funcall (plist-get args :prepare)) (setq handed (cons root args)))))
        (+tasks/start prefix-arg))
      (+tasks-test--goto-heading "Fix it")
      (list handed (org-entry-get nil "PLAN_MODEL") (org-entry-get nil "MODEL")))))

(ert-deftest +tasks/start/asks-for-the-plan-model-with-a-prefix-arg ()
  (pcase-let ((`(,handed ,plan-model ,model) (+tasks-plan-test--start t)))
    (ert-info ("C-u s should store the plan model you pick next to the work model")
      (should (equal '("claude" "local") (list plan-model model))))
    (ert-info ("C-u s should open the interview on the plan model you picked")
      (should (equal "claude" (plist-get (cdr handed) :model))))))

(ert-deftest +tasks/start/opens-the-interview-on-the-plan-model ()
  (pcase-let ((`(,handed ,plan-model ,_) (+tasks-plan-test--start nil)))
    (ert-info ("plain s should ask nothing new and store no plan model")
      (should-not plan-model))
    (ert-info ("plain s should open the interview on the work model, the plan model's fallback")
      (should (equal "local" (plist-get (cdr handed) :model))))))

(ert-deftest +tasks/resume/uses-the-plan-model-until-the-plan-is-approved ()
  (pcase-dolist (`(,plan-text . ,expected) `((nil . "claude") (,+tasks-plan-test--approved . "local")))
    (+tasks-plan-test--with-worktree plan-text
      (let (launched)
        (cl-letf (((symbol-function '+tasks--work-task)
                   (lambda () (list :buffer (current-buffer) :heading "X" :branch "feat/x" :model "local" :plan-model "claude")))
                  ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) tree))
                  ((symbol-function '+tasks--hand-off) (lambda (_root &rest args) (setq launched args))))
          (with-temp-buffer
            (setq default-directory "/r/")
            (+tasks/resume)))
        (ert-info ((format "resume should open the session on %s when the plan is %s" expected (if plan-text "approved" "not written")))
          (should (equal expected (plist-get launched :model))))))))

(ert-deftest +tasks/approve-plan/restarts-on-the-work-model-when-it-differs ()
  (+tasks-plan-test--with-worktree +tasks-plan-test--written
    (+tasks-test--in-tasks-buffer "** PLAN Fix it\n:PROPERTIES:\n:BRANCH: feat/fix-it\n:MODEL: local\n:PLAN_MODEL: claude\n:END:\n"
      (+tasks-test--goto-heading "Fix it")
      (let ((task (+tasks--task-plist))
            stopped handed sent)
        (cl-letf (((symbol-function '+tasks--work-task) (lambda () task))
                  ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) tree))
                  ((symbol-function '+tasks--read-approval) (lambda () ?y))
                  ((symbol-function '+tasks--show-plan) #'ignore)
                  ((symbol-function '+ai--stop-pilish-session) (lambda (root session) (setq stopped (list root session))))
                  ((symbol-function '+ai--send-pilish-input) (lambda (line &optional _) (push line sent)))
                  ((symbol-function '+tasks--hand-off) (lambda (root &rest args) (setq handed (cons root args))))
                  ((symbol-function 'message) #'ignore))
          (+tasks/approve-plan))
        (ert-info ("y should stop the interview's session when the work model differs from the plan model")
          (should (equal (list tree "feat/fix-it") stopped)))
        (ert-info ("y should start the session again on the work model, in the worktree, with the go line to send")
          (should (equal (list tree "local" "feat/fix-it" t) (list (car handed) (plist-get (cdr handed) :model)
                                                                   (plist-get (cdr handed) :session) (plist-get (cdr handed) :send))))
          (should (string-prefix-p "Plan approved:" (plist-get (cdr handed) :draft))))
        (ert-info ("y should not also send the line into the session it just stopped")
          (should-not sent))))))

(ert-deftest +tasks--edit-plan/sends-one-reread-per-save-however-often-e-is-pressed ()
  (+tasks-plan-test--with-worktree +tasks-plan-test--written
    (let ((task (list :branch "feat/x"))
          sent)
      (cl-letf (((symbol-function '+tasks--task-worktree-of) (lambda (_) tree))
                ((symbol-function '+ai--send-pilish-input) (lambda (line &optional _) (push line sent)))
                ((symbol-function 'pop-to-buffer) #'ignore))
        (+tasks--edit-plan task tree)
        (+tasks--edit-plan task tree)
        (let ((plan (get-file-buffer (+tasks--plan-file tree))))
          (unwind-protect
              (progn
                (with-current-buffer plan
                  (goto-char (point-max))
                  (insert "- x\n")
                  (save-buffer))
                (ert-info ("a second e should not double the reread line sent on save")
                  (should (= 1 (length sent)))))
            (kill-buffer plan)))))))

;;; hardening after review

(ert-deftest +tasks--plan-missing-heading/wants-exact-heading-lines ()
  (pcase-dolist (`(,text . ,expected)
                 `((,(replace-regexp-in-string "^## Open\n" "## Open questions\n" +tasks-plan-test--written) . "## Open")
                   (,(replace-regexp-in-string "^## Open\n" "## Open  \n" +tasks-plan-test--written) . nil)
                   (,(replace-regexp-in-string "^# Plan: .*\n" "" +tasks-plan-test--written) . "# Plan:")))
    (ert-info ((format "a heading that only starts like a required one should count as missing: %S" expected))
      (should (equal expected (+tasks--plan-missing-heading text))))))

(ert-deftest +tasks--plan-approved-p/wants-emacs-own-line-under-accept ()
  (pcase-dolist (`(,text ,expected ,why)
                 `((,(concat +tasks-plan-test--written "Approved: 2026-10-17\n") nil "ignore an Approved line anywhere but under ACCEPT")
                   (,(replace-regexp-in-string "^ACCEPT: it works\n" "ACCEPT: it works\nApproved: yes\n" +tasks-plan-test--written) nil "want the date Emacs writes")
                   (,+tasks-plan-test--approved t "accept the line Emacs writes")))
    (ert-info ((format "plan-approved-p should %s" why))
      (should (eq expected (+tasks--plan-approved-p text))))))

(ert-deftest +tasks--plan-with-approval/dates-once-after-the-first-accept-line ()
  (ert-info ("approving an approved plan should change nothing")
    (should (equal +tasks-plan-test--approved (+tasks--plan-with-approval +tasks-plan-test--approved "2026-10-18"))))
  (ert-info ("approving should leave a second ACCEPT line, quoted later in the plan, alone")
    (let ((text (concat +tasks-plan-test--written "ACCEPT: quoted again\n")))
      (should (equal (concat +tasks-plan-test--approved "ACCEPT: quoted again\n")
                     (+tasks--plan-with-approval text "2026-10-17"))))))

(ert-deftest +tasks/approve-plan/refuses-when-no-approval-can-be-written ()
  (+tasks-plan-test--with-worktree (replace-regexp-in-string "^ACCEPT: it works\n" "ACCEPT:it works\n" +tasks-plan-test--written)
    (+tasks-test--in-tasks-buffer "** PLAN Fix it\n:PROPERTIES:\n:BRANCH: feat/fix-it\n:END:\n"
      (+tasks-test--goto-heading "Fix it")
      (let ((task (+tasks--task-plist))
            (tasks (current-buffer))
            sent)
        (cl-letf (((symbol-function '+tasks--work-task) (lambda () task))
                  ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) tree))
                  ((symbol-function '+tasks--read-approval) (lambda () ?y))
                  ((symbol-function '+tasks--show-plan) #'ignore)
                  ((symbol-function '+ai--send-pilish-input) (lambda (line &optional _) (push line sent))))
          (ert-info ("y on a plan whose ACCEPT line the approval cannot be written under should stop")
            (should-error (+tasks/approve-plan) :type 'user-error)))
        (ert-info ("a refused approval should leave the task in PLAN, the plan unapproved and pi untold")
          (should (equal '("PLAN" nil) (+tasks-test--state-of tasks "Fix it")))
          (should-not sent)
          (should-not (+tasks--plan-approved-p (+tasks--plan-text tree))))))))

(ert-deftest +tasks/approve-plan/refuses-over-unsaved-edits ()
  (+tasks-plan-test--with-worktree +tasks-plan-test--written
    (+tasks-test--in-tasks-buffer "** PLAN Fix it\n:PROPERTIES:\n:BRANCH: feat/fix-it\n:END:\n"
      (+tasks-test--goto-heading "Fix it")
      (let ((task (+tasks--task-plist))
            (plan (find-file-noselect (expand-file-name "PLAN.md" tree))))
        (unwind-protect
            (progn
              (with-current-buffer plan (goto-char (point-max)) (insert "- edit\n"))
              (cl-letf (((symbol-function '+tasks--work-task) (lambda () task))
                        ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) tree))
                        ((symbol-function '+tasks--read-approval) (lambda () ?y))
                        ((symbol-function '+tasks--show-plan) #'ignore))
                (ert-info ("approve-plan should stop while PLAN.md has unsaved edits, rather than approve the stale file and drop them")
                  (should-error (+tasks/approve-plan) :type 'user-error)
                  (should (buffer-modified-p plan)))))
          (with-current-buffer plan (set-buffer-modified-p nil))
          (kill-buffer plan))))))

(ert-deftest +tasks--plan-buffer/never-applies-the-agents-file-local-variables ()
  (+tasks-plan-test--with-worktree (concat "<!-- -*- mode: emacs-lisp; eval: (setq +tasks-plan-test--pwned t) -*- -->\n" +tasks-plan-test--written)
    (let ((enable-local-variables :all)
          (+tasks-plan-test--pwned nil)
          buffer)
      (unwind-protect
          (progn
            (setq buffer (+tasks--plan-buffer tree))
            (ert-info ("plan-buffer should visit the agent's file without evaluating its local variables")
              (should-not +tasks-plan-test--pwned)
              (should-not (eq 'emacs-lisp-mode (buffer-local-value 'major-mode buffer)))))
        (when buffer (kill-buffer buffer))))))

(ert-deftest +tasks--plan-open-items/counts-any-line-under-open ()
  (ert-info ("any non-blank line under Open should count as an open item, whatever its bullet")
    (should (equal '("* which backend?" "1. naming" "  - nested")
                   (+tasks--plan-open-items (concat +tasks-plan-test--written "* which backend?\n1. naming\n\n  - nested\n"))))))

(ert-deftest +tasks--excluded-files/anchors-the-plan-to-the-root ()
  (ert-info ("only a PLAN.md at the worktree root should be hidden from git")
    (should (member "/PLAN.md" (+tasks--excluded-files)))
    (should-not (member "PLAN.md" (+tasks--excluded-files)))))

(ert-deftest +tasks--start-entry/replaces-the-plan-model-you-pick ()
  (+tasks-test--in-tasks-buffer "** TODO Again\n:PROPERTIES:\n:PLAN_MODEL: local\n:END:\n"
    (+tasks-test--goto-heading "Again")
    (+tasks--start-entry "feat/again" nil "claude")
    (+tasks-test--goto-heading "Again")
    (ert-info ("a plan model picked with C-u s should replace an older one, so c resumes on it")
      (should (equal "claude" (org-entry-get nil "PLAN_MODEL"))))))

(ert-deftest +tasks--send-to-session/says-when-nobody-is-listening ()
  (let (said)
    (cl-letf (((symbol-function '+tasks--task-worktree-of) (lambda (_) "/r/.worktrees/feat-x/"))
              ((symbol-function '+ai--send-pilish-input) #'ignore)
              ((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
      (+tasks--send-to-session '(:branch "feat/x") "Plan approved"))
    (ert-info ("a line that reached no session should be reported, not lost in silence")
      (should (string-search "Plan approved" said)))))

(ert-deftest +tasks/approve-plan/asks-for-the-task-once ()
  (+tasks-plan-test--with-worktree +tasks-plan-test--written
    (+tasks-test--in-tasks-buffer "** PLAN Fix it\n:PROPERTIES:\n:BRANCH: feat/fix-it\n:END:\n"
      (+tasks-test--goto-heading "Fix it")
      (let ((task (+tasks--task-plist))
            (asked 0))
        (cl-letf (((symbol-function '+tasks--work-task) (lambda () (cl-incf asked) task))
                  ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) tree))
                  ((symbol-function '+tasks--read-approval) (lambda () ?n))
                  ((symbol-function '+tasks--show-plan) #'ignore)
                  ((symbol-function '+tasks--hand-off) #'ignore)
                  ((symbol-function '+ai--prefill-pilish-input) #'ignore))
          (+tasks/approve-plan))
        (ert-info ("n should resume the task it already has, not pick a task a second time")
          (should (= 1 asked)))))))

;;; tasks started before the skill existed

(ert-deftest +tasks--skill-missing-p/sees-a-worktree-cut-before-the-skill ()
  (+tasks-test--in-git-repo
   (let* ((repo default-directory)
          (tree (+tasks--add-worktree repo "feat/old")))
     (make-directory (expand-file-name ".agents/skills/grilling" repo) t)
     (with-temp-file (expand-file-name ".agents/skills/grilling/SKILL.md" repo) (insert "skill"))
     (ert-info ("a worktree without the skill the main checkout has should count as missing it")
       (should (+tasks--skill-missing-p repo tree)))
     (ert-info ("with no copy anywhere there is nothing to offer, so nothing is missing")
       (delete-directory (expand-file-name ".agents" repo) t)
       (cl-letf (((symbol-function '+tasks--own-skill-dir) (lambda () "/nowhere/")))
         (should-not (+tasks--skill-missing-p repo tree)))))))

(ert-deftest +tasks--ensure-skill/copies-the-skill-into-an-older-worktree-when-told-to ()
  (+tasks-test--in-git-repo
   (let* ((repo default-directory)
          (tree (+tasks--add-worktree repo "feat/old")))
     (make-directory (expand-file-name ".agents/skills/grilling" repo) t)
     (with-temp-file (expand-file-name ".agents/skills/grilling/SKILL.md" repo) (insert "skill"))
     (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
       (+tasks--ensure-skill repo tree)
       (ert-info ("a no should leave the worktree as it is")
         (should-not (file-exists-p (expand-file-name ".agents/skills/grilling/SKILL.md" tree)))))
     (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
       (+tasks--ensure-skill repo tree)
       (ert-info ("a yes should copy the main checkout's skill into the worktree, so /skill:grilling resolves there")
         (should (equal "skill" (+tasks--read-file (expand-file-name ".agents/skills/grilling/SKILL.md" tree))))))
     (ert-info ("the copied skill should be untracked work the branch carries, visible to the dirty check")
       (should (+tasks--worktree-dirty-p tree))))))

(ert-deftest +tasks/resume/offers-the-skill-to-a-worktree-that-lacks-it ()
  (let (ensured)
    (cl-letf (((symbol-function '+tasks--work-task)
               (lambda () (list :buffer (current-buffer) :heading "X" :branch "feat/x" :model "local")))
              ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) "/r/.worktrees/feat-x/"))
              ((symbol-function '+tasks--add-worktree) (lambda (_root _branch) "/r/.worktrees/feat-x/"))
              ((symbol-function '+tasks--ensure-skill) (lambda (root tree) (setq ensured (list root tree))))
              ((symbol-function '+tasks--hand-off)
               (lambda (_root &rest args) (funcall (plist-get args :prepare)))))
      (with-temp-buffer
        (setq default-directory "/r/")
        (+tasks/resume)))
    (ert-info ("resume should check the worktree for the skill once the review allows it")
      (should (equal '("/r/" "/r/.worktrees/feat-x/") ensured)))))

;;; projects that never had the skill

(ert-deftest +tasks--skill-source/prefers-the-projects-copy-and-falls-back-to-doom ()
  (+tasks-test--in-git-repo
   (let ((repo default-directory))
     (cl-letf (((symbol-function '+tasks--own-skill-dir) (lambda () "/doom/.agents/skills/grilling/"))
               ((symbol-function 'file-directory-p)
                (lambda (dir) (or (equal dir "/doom/.agents/skills/grilling/")
                                  (equal dir (expand-file-name ".agents/skills/grilling/" repo))))))
       (ert-info ("skill-source should be the project's own copy when it has one")
         (should (equal (expand-file-name ".agents/skills/grilling/" repo) (+tasks--skill-source repo)))))
     (cl-letf (((symbol-function '+tasks--own-skill-dir) (lambda () "/doom/.agents/skills/grilling/"))
               ((symbol-function 'file-directory-p) (lambda (dir) (equal dir "/doom/.agents/skills/grilling/"))))
       (ert-info ("skill-source should fall back to the copy this config ships, for a project that never had one")
         (should (equal "/doom/.agents/skills/grilling/" (+tasks--skill-source repo)))))
     (cl-letf (((symbol-function '+tasks--own-skill-dir) (lambda () "/doom/.agents/skills/grilling/"))
               ((symbol-function 'file-directory-p) #'ignore))
       (ert-info ("skill-source should be nil when there is no copy anywhere")
         (should-not (+tasks--skill-source repo)))))))

(ert-deftest +tasks--ensure-skill/offers-dooms-copy-to-a-project-without-one ()
  (+tasks-test--in-git-repo
   (let* ((repo default-directory)
          (tree (+tasks--add-worktree repo "feat/x"))
          (doom (file-name-as-directory (make-temp-file "doom-skill" t)))
          asked)
     (unwind-protect
         (progn
           (make-directory (expand-file-name ".agents/skills/grilling" doom) t)
           (with-temp-file (expand-file-name ".agents/skills/grilling/SKILL.md" doom) (insert "dooms"))
           (cl-letf (((symbol-function '+tasks--own-skill-dir)
                      (lambda () (expand-file-name ".agents/skills/grilling/" doom)))
                     ((symbol-function 'y-or-n-p) (lambda (p) (setq asked p) t)))
             (+tasks--ensure-skill repo tree))
           (ert-info ("a project that never had the skill should be offered this config's copy, into the worktree")
             (should asked)
             (should (equal "dooms" (+tasks--read-file (expand-file-name ".agents/skills/grilling/SKILL.md" tree))))))
       (delete-directory doom t)))))

(ert-deftest +tasks--start-at/offers-the-skill-to-the-new-worktree ()
  (+tasks-test--in-git-repo
   (let ((repo default-directory)
         (buffer (find-file-noselect (expand-file-name "tasks.org" default-directory)))
         ensured)
     (unwind-protect
         (progn
           (with-current-buffer buffer
             (insert "#+TODO: IDEA TODO PLAN | DONE\n* Ideas\n** IDEA Fix it\n* Active\n")
             (save-buffer)
             (org-mode)
             (+tasks-test--goto-heading "Fix it")
             (cl-letf (((symbol-function '+tasks--ensure-skill) (lambda (root tree) (setq ensured (list root tree)))))
               (+tasks--start-at (point-marker) repo "feat/fix-it" "it works")))
           (ert-info ("start should offer the skill to the worktree it just added, so the interview can run there")
             (should (equal (list repo (+tasks--worktree-path repo "feat/fix-it")) ensured))))
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer)))))

;;; c on a PLAN task without a plan drafts the interview

(ert-deftest +tasks--task-plist/carries-the-parents ()
  (+tasks-test--in-tasks-buffer "** IDEA Parent\n*** PLAN Child\n:PROPERTIES:\n:BRANCH: feat/c\n:END:\n"
    (+tasks-test--goto-heading "Child")
    (ert-info ("task-plist should carry the parents so a resumed interview can name them")
      (should (equal "Parent" (plist-get (+tasks--task-plist) :parents))))))

(ert-deftest +tasks--resume-draft/interviews-a-plan-task-that-has-no-plan-yet ()
  (pcase-dolist (`(,state ,plan-text ,live ,expected ,why)
                 `(("PLAN" nil nil t "draft the interview for a fresh session on a PLAN task without a plan")
                   ("PLAN" nil t nil "leave a running session alone, its chat already holds the interview")
                   ("PLAN" ,+tasks-plan-test--written nil nil "draft nothing once the plan is written")
                   ("RED" nil nil nil "draft nothing for a task past planning")))
    (let ((task (list :state state :heading "Fix it" :accept "it works" :parents nil)))
      (cl-letf (((symbol-function '+ai--live-pilish-session-p) (lambda (&rest _) live)))
        (ert-info ((format "resume-draft should %s" why))
          (should (eq expected (and (+tasks--resume-draft task plan-text "/r/.worktrees/feat-x/" "feat/x") t)))))
      (when expected
        (ert-info ("the resume draft should be the same interview s starts")
          (should (equal (+tasks--interview-prompt "Fix it" "it works" nil)
                         (+tasks--resume-draft task nil "/r/.worktrees/feat-x/" "feat/x"))))))))

(ert-deftest +tasks/resume/hands-pi-the-interview-when-the-plan-is-still-due ()
  (+tasks-test--in-tasks-buffer "** PLAN Fix it\n:PROPERTIES:\n:BRANCH: feat/fix-it\n:ACCEPT: it works\n:END:\n"
    (+tasks-test--goto-heading "Fix it")
    (let ((task (+tasks--task-plist)) launched)
      (cl-letf (((symbol-function '+tasks--work-task) (lambda () task))
                ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) nil))
                ((symbol-function '+ai--live-pilish-session-p) #'ignore)
                ((symbol-function '+tasks--hand-off) (lambda (_root &rest args) (setq launched args))))
        (+tasks/resume))
      (ert-info ("c on a PLAN task with no PLAN.md and no session should open pi with the interview in its input")
        (should (string-prefix-p "/skill:grilling Fix it\n\nACCEPT: it works" (plist-get launched :draft)))))))

(ert-deftest +tasks/interview/puts-the-interview-into-the-tasks-session ()
  (+tasks-test--in-tasks-buffer "** PLAN Fix it\n:PROPERTIES:\n:BRANCH: feat/fix-it\n:ACCEPT: it works\n:END:\n"
    (+tasks-test--goto-heading "Fix it")
    (let ((task (+tasks--task-plist)) launched)
      (cl-letf (((symbol-function '+tasks--work-task) (lambda () task))
                ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) "/r/.worktrees/feat-fix-it/"))
                ((symbol-function '+ai--live-pilish-session-p) (lambda (&rest _) t))
                ((symbol-function '+tasks--hand-off) (lambda (root &rest args) (setq launched (cons root args)))))
        (+tasks/interview))
      (ert-info ("interview should go through the same hand-off as c, in the worktree, on the branch's session, even when it is running")
        (should (equal '("/r/.worktrees/feat-fix-it/" "feat/fix-it")
                       (list (car launched) (plist-get (cdr launched) :session))))
        (should (string-prefix-p "/skill:grilling Fix it\n\nACCEPT: it works" (plist-get (cdr launched) :draft)))))))

(ert-deftest +tasks/interview/refuses-a-task-past-planning ()
  (+tasks-test--in-tasks-buffer "** RED Fix it\n:PROPERTIES:\n:BRANCH: feat/fix-it\n:END:\n"
    (+tasks-test--goto-heading "Fix it")
    (let ((task (+tasks--task-plist)))
      (cl-letf (((symbol-function '+tasks--work-task) (lambda () task)))
        (ert-info ("interview on a task whose plan is approved should stop; its decisions go through the agent's own rounds")
          (should-error (+tasks/interview) :type 'user-error))))))

(ert-deftest +tasks/menu-file/binds-interview ()
  (require 'transient)
  (load (expand-file-name "modules/my/tasks/menu.el" default-directory) nil t)
  (ert-info ("menu key p should put the interview into the task's session")
    (should (eq '+tasks/interview (plist-get (nth 2 (transient-get-suffix '+tasks-transient "p")) :command)))))

(ert-deftest +tasks--clear-session-input/moves-stale-text-to-the-kill-ring ()
  (with-temp-buffer
    (insert "/feature old draft")
    (let ((input (current-buffer))
          (kill-ring nil))
      (cl-letf (((symbol-function '+tasks--task-worktree-of) (lambda (_) "/r/.worktrees/feat-x/"))
                ((symbol-function '+ai--pilish-input-buffer) (lambda (&optional _) input)))
        (+tasks--clear-session-input '(:branch "feat/x")))
      (ert-info ("clearing should empty the input so the interview can take its place")
        (should (string-empty-p (buffer-string))))
      (ert-info ("clearing should keep what was there on the kill ring")
        (should (equal "/feature old draft" (car kill-ring)))))))

(ert-deftest +tasks/interview/replaces-a-stale-draft-in-the-input ()
  (+tasks-test--in-tasks-buffer "** PLAN Fix it\n:PROPERTIES:\n:BRANCH: feat/fix-it\n:ACCEPT: it works\n:END:\n"
    (+tasks-test--goto-heading "Fix it")
    (let ((task (+tasks--task-plist)) cleared)
      (cl-letf (((symbol-function '+tasks--work-task) (lambda () task))
                ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) "/r/.worktrees/feat-fix-it/"))
                ((symbol-function '+tasks--clear-session-input) (lambda (_) (setq cleared t)))
                ((symbol-function '+tasks--hand-off) #'ignore))
        (+tasks/interview))
      (ert-info ("p should clear whatever the input holds before drafting, since it was asked for the interview")
        (should cleared)))))

;;; next from anywhere in the worktree

(ert-deftest +tasks/next/finds-the-task-from-a-non-org-buffer ()
  (+tasks-test--in-tasks-buffer "** PLAN Thing\n:PROPERTIES:\n:BRANCH: feat/thing\n:END:\n"
    (setq buffer-file-name (expand-file-name "tasks.org" temporary-file-directory))
    (+tasks-test--goto-heading "Thing")
    (let ((task (+tasks--task-plist)) ran)
      (cl-letf (((symbol-function '+tasks--work-task) (lambda () task))
                ((symbol-function '+tasks/approve-plan) (lambda () (setq ran (list major-mode (org-get-heading t t t t))))))
        (with-temp-buffer
          (fundamental-mode)
          (+tasks/next)))
      (ert-info ("next from a pi chat or source buffer should act on the task on the current branch, at its heading")
        (should (equal (list 'org-mode "Thing") ran))))
    (setq buffer-file-name nil)))

(ert-deftest +tasks/next/still-prefers-the-heading-at-point-in-tasks-org ()
  (+tasks-test--in-tasks-buffer "** GREEN Here\n:PROPERTIES:\n:BRANCH: feat/here\n:END:\n"
    (setq buffer-file-name (expand-file-name "tasks.org" temporary-file-directory))
    (+tasks-test--goto-heading "Here")
    (let (ran)
      (cl-letf (((symbol-function '+tasks--work-task) (lambda () (error "Should not look elsewhere")))
                ((symbol-function '+tasks/review) (lambda () (setq ran (org-get-heading t t t t)))))
        (+tasks/next))
      (ert-info ("next in tasks.org should keep acting on the heading at point")
        (should (equal "Here" ran))))
    (setq buffer-file-name nil)))

;;; a green run with unticked plan tests is called out

(ert-deftest +tasks--verdict-note/warns-of-a-green-run-the-plan-does-not-vouch-for ()
  (pcase-dolist (`(,verdict ,plan ,expected ,why)
                 `(("GREEN" ,+tasks-plan-test--written "GREEN, but plan: 1/2 tests ticked" "flag a pass while plan tests are unticked")
                   ("GREEN" ,(replace-regexp-in-string "- \\[ \\]" "- [x]" +tasks-plan-test--written) "GREEN; plan: 2/2 tests ticked" "report a pass the plan vouches for")
                   ("RED" ,+tasks-plan-test--written "RED; plan: 1/2 tests ticked" "report a failure with the count")
                   ("GREEN" nil "GREEN" "say only the verdict without a plan")))
    (ert-info ((format "verdict-note should %s" why))
      (should (equal expected (+tasks--verdict-note verdict plan))))))

(ert-deftest +tasks--mark-compiled-tests/says-when-green-is-not-vouched-for ()
  (+tasks-plan-test--with-worktree +tasks-plan-test--written
    (+tasks-test--in-tasks-buffer "** RED Thing\n:PROPERTIES:\n:BRANCH: feat/thing\n:END:\n"
      (+tasks-test--goto-heading "Thing")
      (let ((task (+tasks--task-plist)) said)
        (cl-letf (((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) tree))
                  ((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
          (with-temp-buffer
            (setq-local +tasks--compiled-task task)
            (+tasks--mark-compiled-tests (current-buffer) "finished\n")))
        (ert-info ("a passing run should still move the task to GREEN")
          (should (equal "GREEN" (car (+tasks-test--state-of (current-buffer) "Thing")))))
        (ert-info ("a passing run with unticked plan tests should say so, since GREEN alone would mislead")
          (should (string-search "GREEN, but plan: 1/2 tests ticked" said)))))))
